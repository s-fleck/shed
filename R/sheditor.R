
#' @include ui.R server.R
sheditor <- R6::R6Class(
  "sheditor",
  public = list(
    initialize =
      function(
        x,
        fname,
        format,
        theme = read_css_theme(getOption("shed.css", system.file("css", "shed_dark.css", package = "shed")))
      ){
        self$theme  <- theme
        self$fname  <- fname
        self$format <- format
        self$data   <- x
      },
    edit =
      function(
        x,
        ...
      ){
        private$app(
          .data   = self$data,
          .fname  = self$fname,
          .format = self$format,
          .theme  = self$theme
        )
      },
    fname = NULL,
    data = NULL,
    format = NULL,
    theme = NULL
  ),

  private = list(
    app = function(
      .data,
      .fname,
      .format,
      .theme
    ){
      shiny::shinyApp(
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
          # reactives -----------------------------------------------------------
          values <- reactiveValues()
          values[["overwrite"]] <- FALSE
          values[["modified"]] <- FALSE
          values[["output"]] <- .data

          # . startup -----------------------------------------------------------
          # observeEvent(TRUE, once = TRUE, {
          #   flog.debug("Trigger App Startup")
          #
          #   print(.fname)
          #   print(.data)
          #
          # })

          # local funs ----------------------------------------------------------
          save_file <- function(){
            flog.trace("Trigger save file")
            output   <- values[["output"]]
            fname    <- input$fname
            stopifnot( all(vapply(output, is.character, logical(1))) )

            is_saved <- try(.format$write_fun(output, path = fname))
            is_saved <- !inherits(is_saved, "try-error") && file.exists(fname)

            if (is_saved){
              values[["output_saved"]] <- output
              values[["modified"]] <- FALSE
              flog.info("Saved to %s", fname)

            } else {
              flog.error("Could not save file to '%s'", fname)
            }
          }


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


          # render hot
          output$hot <- renderRHandsontable({
            if (is.data.frame(values[["output"]])){
              flog.trace("Trigger HOT render")
              rhandsontable_shed(values[["output"]])
            } else {
              flog.trace("'output' is not a data.frame")
              NULL
            }
          })


          # i/o -----------------------------------------------------------------



          # . edit hot ----------------------------------------------------------
          observeEvent(input$hot, {
            flog.trace("Trigger user input HOT update")

            if (!is.null(input$hot)) {

              values[["output"]]   <- hot_to_r_safely(input$hot)

              if (
                identical(nrow(values[["output"]]), 0L) ||
                identical(ncol(values[["output"]]), 0L)
              ){
                flog.trace("Trigger HOT render")
                flog.trace(
                  "data.frame has illegal dimensions: %sx%s; returning empty 1x1 data.frame instead.",
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
              tryCatch({
                flog.info("Loading data from file system: %s", input$fname)
                output <- .format$read_fun(input$fname, encoding = input[["readEncoding"]])

                validate_input_df(output)

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

            if (!all(vapply(values[["output"]], is.character, logical(1)))) {
              flog.warn("All columns should be read as character")
            }
          })



          # session end -------------------------------------------------------------
          session$onSessionEnded(function() {
            flog.trace("Trigger Session End")
            stopApp(parse_output_df(isolate(values[["output"]])))
          })
      }
      )
    }
  )
)
