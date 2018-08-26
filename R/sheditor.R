sheditor <- R6::R6Class(
  "sheditor",
  public = list(
    initialize =
      function(
        input  = NULL,
        file   = if (is.data.frame(input)) tempfile() else input,
        format = shed_format_csv,
        locale = readr::locale(),
        theme  = "default"
      ){
        if (is.data.frame(input))
          self$data <- input
        else
          data  <- tryCatch(
            format$read_fun(input, locale = locale),
            error = function(e) empty_df(1, 1)
          )

        self$theme  <- load_theme(theme)
        self$fname  <- file
        self$format <- format
        self$locale <- locale

      },
    edit =
      function(
        x,
        ...
      ){
        res <- print(private$app(
          .data   = self$data,
          .fname  = self$fname,
          .format = self$format,
          .theme  = self$theme,
          .locale = self$locale
        ))
        self$data  <- res$data
        self$fname <- res$fname

        res$data
      },
    fname = NULL,
    data = NULL,
    format = NULL,
    theme = NULL,
    locale = NULL
  ),

  private = list(
    app = function(
      .data,
      .fname,
      .format,
      .locale,
      .theme,
      options = list()  # passed on to shinyApp()
    ){
      shiny::shinyApp(
        options = options,
        ui = fluidPage(
          width = "100%",
          shinyjs::useShinyjs(),
          tags$head(
            tags$style(HTML(.theme)),
            tags$script(HTML(
              js_add_ctrl_hotkey("$('#btnSave').click()", key = c(83))
            ))
          ),

          fixedPanel(
            id = "panelTop",
            top = 0,
            left = 0,
            right = 0,
            div(
              class = "shedFnameContainer",
              div(textInput("fname", NULL, .fname, width = "100%"), class = "fnameSaved", id = "fnameDiv")
            ),
            div(
              class = "shedCtrl",
              actionButton("btnLoad", "load", class = "shedButton shedCtrlElement"),
              div(class = "shedCtrlSpacing"),
              actionButton("btnSave", "save", class = "shedButton shedCtrlElement")
            )
          ),

          absolutePanel(
            rHandsontableOutput("hot"),
            top = 160,
            left = 0,
            right = 0
          )
        ),

      server = function(
        input,
        output,
        session
        ){
          # helpers ------------------------------------------------------------
          is_bool <- function(x) identical(x, TRUE) || identical(x, FALSE)

          # int -----------------------------------------------------------
          values    <- reactiveValues()
          read_fun  <- .format$read_fun
          write_fun <- .format$write_fun


          # startup -----------------------------------------------------------
          observeEvent(TRUE, once = TRUE, {
            flog.debug("Trigger App Startup")

            values[["overwrite"]] <- FALSE
            values[["modified"]]  <- FALSE
            values[["output"]]    <- prep_input_df(.data)

            stopifnot(
              is.function(read_fun),
              is.function(write_fun),
              is.data.frame(values[["output"]]),
              is_bool(values[["modified"]]),
              is_bool(values[["overwrite"]])
            )
          })


          # infile ui -----------------------------------------------------------
          observe({
            flog.trace("Trigger input file color change")

            if(!file.exists(input$fname)){
              values[["modified"]] <- TRUE
            }

            if (isTRUE(values[["modified"]])){
              flog.trace("Input file color changed to NotSaved")
              shinyjs::runjs('document.getElementById("fnameDiv").className  = "fnameNotSaved";')
            } else {
              flog.trace("Input file color changed to Saved")
              shinyjs::runjs('document.getElementById("fnameDiv").className  = "fnameSaved";')
            }
          })


          # render hot ---------------------------------------------------------
          output$hot <- renderRHandsontable({
            if (is.data.frame(values[["output"]])){
              flog.trace("Trigger HOT render")
              rhandsontable_shed(values[["output"]])
            } else {
              flog.trace(
                "'output' is not a data.frame but %s",
                fmt_class(values[["output"]])
              )
              NULL
            }
          })


          # i/o -----------------------------------------------------------------

          # . edit hot ----------------------------------------------------------
          observeEvent(input$hot, {
            flog.trace("Trigger user input HOT update")

            if (!is.null(input$hot)) {
              values[["output"]]   <- prep_input_df(hot_to_r_safely(input$hot))

              if (
                identical(nrow(values[["output"]]), 0L) ||
                identical(ncol(values[["output"]]), 0L)
              ){
                flog.trace(
                  paste("data.frame has illegal dimensions: %sx%s; returning",
                    "empty 1x1 data.frame instead."),
                  nrow(values[["output"]]),
                  ncol(values[["output"]])
                )
                values[["output"]] <- empty_df(1, 1)
                output$hot <- renderRHandsontable(
                  rhandsontable_shed(values[["output"]])
                )
              }

              values[["modified"]] <- !isTRUE(all.equal(
                try(unname(as.matrix(values[["output_saved"]])), silent = TRUE),
                unname(as.matrix(values[["output"]]))
              ))
            }
          })


          # . save --------------------------------------------------------------
          save_file <- function(){
            flog.trace("Trigger save file")
            assert_only_char_cols(values[["output"]])

            write_ok <- tryCatch(
              expr = {
                write_fun(values[["output"]], path = input$fname)
                TRUE
              },
              error = function(e){
                flog.error("Write function aborted with error: %s", e)
                FALSE
              }
            )

            is_saved <- write_ok && file.exists(input$fname)

            if (is_saved){
              values[["output_saved"]] <- values[["output"]]
              values[["modified"]] <- FALSE
              flog.info("Saved to %s", input$fname)

            } else {
              flog.error("Could not save file to '%s'", input$fname)
            }
          }


          observeEvent(input$btnSave, {
            flog.trace("Trigger Save Button")
            fname  <- input$fname
            overwrite <- values[["overwrite"]]

            flog.trace("Target file %s", fname)
            flog.trace("Overwrite is set to %s", overwrite)

            if (!file.exists(fname) || isTRUE(overwrite)){
              save_file()

            } else {
              flog.trace("Trigger Overwrite Modal")
              showModal(shiny::modalDialog(
                size = "s",
                div("Overwrite existing file?", style = "height: 40px; " ),
                shiny::actionButton("modalOverwriteYes", "Yes", class = "modal-button"),
                shiny::actionButton("modalOverwriteNo", "No", class = "modal-button"),
                footer = NULL
              ))
            }

            rm(overwrite)
            rm(fname)
          })


          # overwrite modal
          observeEvent(input$modalOverwriteYes, {
            flog.trace("Trigger modalOverwriteYes")
            values[["overwrite"]] <- TRUE
            save_file()
            removeModal()
          })


          observeEvent(input$modalOverwriteNo, {
            flog.trace("Trigger modalOverwriteNo")
            flog.info("Not saved")
            removeModal()
          })


          # . load --------------------------------------------------------------------
          observeEvent(input$btnLoad, {
            flog.trace("Trigger Load Button")

            if (file.exists(input$fname)){
              tryCatch(
                {
                  flog.info("Loading data from file system: %s", input$fname)
                  output <- read_fun(input$fname, locale = .locale)
                  output <- prep_input_df(output)

                  values[["output"]] <- output
                  values[["output_saved"]] <- output
                  values[["modified"]] <- FALSE
                  values[["overwrite"]] <- FALSE
                  rm(output)
                },
                error = function(e) {
                  flog.error("Input file exists but cannot be read %s", input$fname)
                  flog.error("Reason: %s", e)
                }
              )

            } else {
              flog.error("Input file does not exist: %s", input$fname)
            }

            assert_only_char_cols(values[["output"]])
          })


          # session end -------------------------------------------------------------
          session$onSessionEnded(function() {
            flog.trace("Trigger Session End")
            stopApp({
              sheditor_retval(
                parse_output_df(isolate(values[["output"]])),
                isolate(input$fname)
              )
            })
          })
        }
      )
    }
  )
)




has_only_char_cols <- function(x){
  is.data.frame(x) && all(vapply(x, is.character, logical(1)))
}




assert_only_char_cols <- function(x){
  if (!is.data.frame(x))
    stop(flog.fatal("'x' is not a data.frame but %s", fmt_class(x)))

  if (length(x) == 0)
    stop(flog.fatal("'x' is a zero length data.frame."))

  if (!has_only_char_cols(x)){
    stop(flog.fatal(
      "All columns of 'x' must be character but x are: %s",
      paste(vapply(x, fmt_class, character(1)), collapse = ", ")
    ))
  }

  TRUE
}




prep_input_df <- function(x){

  if (!is.data.frame(x)){
    stop(flog.fatal("'x' must be a data.frame"))
  }

  if (nrow(x) > 1000){
    flog.warn(paste(
      "Shed is designed for datasets with less than 1000 rows and",
      "performs badly for larger ones. Input has %s rows."),
      nrow(x) - 1
    )

  } else if (nrow(x) > 10000){
    stop(flog.fatal(paste(
      "Loading data > 10000 rows is disabled as shed is unusably slow",
      "for such large datasets. Input has %s rows."), nrow(x) - 1
    ))
  }

  res <- x

  if (!has_only_char_cols(x)){
    flog.debug(paste(
      "Autoconverting all columns to character. 'shed' can only handle",
      "data.frames with all-character columns properly. Please ensure that",
      "the 'read_fun' in your 'shed_format' returns such data.frames."
    ))
    res[] <- lapply(res, as.character)
  }

  autonames <- paste0("X", seq_along(res))
  if (is.null(names(res)))  names(res) <- autonames

  if (!identical(names(res), autonames)){
    flog.trace(
      "Converting colnames to first data.frame row: %s",
      paste(colnames(res), collapse = ", ")
    )

    header <- as.data.frame(as.list(names(res)))
    names(header) <- paste0("X", seq_along(header))
    names(res)    <- paste0("X", seq_along(header))

    res <- rbind(
      header,
      res
    )
  }

  res
}




fmt_class <- function(x) paste0("<", paste(class(x), collapse = "/"), ">")
