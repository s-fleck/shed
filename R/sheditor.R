sheditor <- R6::R6Class(
  "sheditor",
  public = list(
    initialize =
      function(
        input  = NULL,
        file   = if (is_scalar_character(input)) input else tempfile(),
        format = shed_format_csv,
        locale = readr::locale(),
        theme  = "default"
      ){

        self$data   <- handle_input(input, file, format, locale)
        self$theme  <- load_theme(theme)
        self$fname  <- file
        self$format <- format
        self$locale <- locale

      },
    edit =
      function(
        x = NULL
      ){
        if (!is.null(x))
          self$fname <- x

        res <- print(private$app(
          .data   = self$data,
          .fname  = self$fname,
          .format = self$format,
          .theme  = self$theme,
          .locale = self$locale
        ))
        self$data  <- res$data
        self$fname <- res$fname

        invisible(res$data)
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
          if (!has_colnames_row(.data)) .data <- colnames_to_row(.data)


          # startup -----------------------------------------------------------
          observeEvent(TRUE, once = TRUE, {
            lg$debug("Trigger App Startup")

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
            lg$trace("Trigger input file color change")

            if(!file.exists(input$fname)){
              values[["modified"]] <- TRUE
            }

            if (isTRUE(values[["modified"]])){
              lg$trace("Input file color changed to NotSaved")
              shinyjs::runjs('document.getElementById("fnameDiv").className  = "fnameNotSaved";')
            } else {
              lg$trace("Input file color changed to Saved")
              shinyjs::runjs('document.getElementById("fnameDiv").className  = "fnameSaved";')
            }
          })


          # render hot ---------------------------------------------------------
          output$hot <- renderRHandsontable({
            if (is.data.frame(values[["output"]])){
              lg$trace("Trigger HOT render")
              rhandsontable_shed(values[["output"]])
            } else {
              lg$trace(
                "'output' is not a data.frame but %s",
                fmt_class(values[["output"]])
              )
              NULL
            }
          })


          # i/o -----------------------------------------------------------------

          # . edit hot ----------------------------------------------------------
          observeEvent(input$hot, {
            lg$trace("Trigger user input HOT update")

            if (!is.null(input$hot)) {
              values[["output"]]   <- prep_input_df(hot_to_r_safely(input$hot))

              if (
                identical(nrow(values[["output"]]), 0L) ||
                identical(ncol(values[["output"]]), 0L)
              ){
                lg$trace(
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
            lg$trace("Trigger save file")
            assert_only_char_cols(values[["output"]])

            write_ok <- tryCatch(
              expr = {
                write_fun(values[["output"]], path = input$fname)
                TRUE
              },
              error = function(e){
                lg$error("Write function aborted with error: %s", e)
                FALSE
              }
            )

            is_saved <- write_ok && file.exists(input$fname)

            if (is_saved){
              values[["output_saved"]] <- values[["output"]]
              values[["modified"]] <- FALSE
              lg$info("Saved to %s", input$fname)

            } else {
              lg$error("Could not save file to '%s'", input$fname)
            }
          }


          observeEvent(input$btnSave, {
            lg$trace("Trigger Save Button")
            fname  <- input$fname
            overwrite <- values[["overwrite"]]

            lg$trace("Target file %s", fname)
            lg$trace("Overwrite is set to %s", overwrite)

            if (!file.exists(fname) || isTRUE(overwrite)){
              save_file()

            } else {
              lg$trace("Trigger Overwrite Modal")
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
            lg$trace("Trigger modalOverwriteYes")
            values[["overwrite"]] <- TRUE
            save_file()
            removeModal()
          })


          observeEvent(input$modalOverwriteNo, {
            lg$trace("Trigger modalOverwriteNo")
            lg$info("Not saved")
            removeModal()
          })


          # . load --------------------------------------------------------------------
          observeEvent(input$btnLoad, {
            lg$trace("Trigger Load Button")

            if (file.exists(input$fname)){
              tryCatch(
                {
                  lg$info("Loading data from file system: %s", input$fname)
                  output <- read_fun(input$fname, locale = .locale)
                  output <- prep_input_df(output)

                  values[["output"]] <- output
                  values[["output_saved"]] <- output
                  values[["modified"]] <- FALSE
                  values[["overwrite"]] <- FALSE
                  rm(output)
                },
                error = function(e) {
                  lg$error("Input file exists but cannot be read %s", input$fname)
                  lg$error("Reason: %s", e)
                }
              )

            } else {
              lg$error("Input file does not exist: %s", input$fname)
            }

            assert_only_char_cols(values[["output"]])
          })


          # session end -------------------------------------------------------------
          session$onSessionEnded(function() {
            lg$trace("Trigger Session End")
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
    stop(lg$fatal("'x' is not a data.frame but %s", fmt_class(x)))

  if (length(x) == 0)
    stop(lg$fatal("'x' is a zero length data.frame."))

  if (!has_only_char_cols(x)){
    stop(lg$fatal(
      "All columns of 'x' must be character but x are: %s",
      paste(vapply(x, fmt_class, character(1)), collapse = ", ")
    ))
  }

  TRUE
}




handle_input <- function(
  input,
  file,
  format,
  locale
){
  if (is.data.frame(input))
    return(input)


  if (is_scalar_character(input))
    return(tryCatch(
      format$read_fun(input, locale = locale),
      error = function(e) empty_df(1, 1)
    ))


  if (is_integerish(input)) {
    if (length(input) == 1)  return(empty_df(1, input))
    if (length(input) == 2)  return(empty_df(input[[1]], input[[2]]))

    lg$error("If 'x' is an integer it must be of length `1` (cols) or `2` (rows, cols)")
  }

  return(empty_df(1, 1))
}




prep_input_df <- function(
  x,
  recover = function() stop("Preparing data.frame failed")
){
  # preconditions
    ok <- TRUE

    if (!is.data.frame(x)){
      lg$fatal("'x' must be a data.frame")
      ok <- FALSE
    }

    if (nrow(x) > 10000){
      lg$fatal(paste(
        "Loading data > 10000 rows is disabled as shed is unusably slow",
        "for such large datasets. Input has %s rows."), nrow(x) - 1
      )
      ok <- FALSE
    }

    if (!ok) return(recover())


  # init
    res <- data.table::copy(x)

    if (nrow(x) > 1000){
      lg$warn(paste(
        "Shed is designed for datasets with less than 1000 rows and",
        "performs badly for larger ones. Input has %s rows."),
        nrow(x) - 1
      )
    }

    if (!has_only_char_cols(x)){
      lg$debug(paste(
        "Autoconverting all columns to character. 'shed' can only handle",
        "data.frames with all-character columns properly. Please ensure that",
        "the 'read_fun' in your 'shed_format' returns such data.frames."
      ))
      res[] <- lapply(res, as.character)
    }

  # return
  attr(res, "spec") <- NULL
  res
}




fmt_class <- function(x){
  paste0("<", paste(class(x), collapse = "/"), ">")
}




make_default_names <- function(x){
  paste0("X", seq.int(1, x))
}




colnames_to_row <- function(x){
  stopifnot(
    is.data.frame(x),
    !is.null(names(x)),
    is.null(attr(x, "has_colnames_row"))
  )

  coldf        <- as.data.frame(as.list(names(x)), stringsAsFactors = FALSE)
  names(x)     <- make_default_names(length(x))
  names(coldf) <- make_default_names(length(x))

  res <- rbind(coldf, x)
  attr(res, "has_colnames_row") <- TRUE
  res
}




has_colnames_row <- function(x){
  isTRUE(attr(x, "has_colnames_row"))
}




`has_colnames_row<-` <- function(x, value){
  attr(x, "has_colnames_row") <- value
  x
}
