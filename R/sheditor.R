#' Shedditor
#'
#' A `Shedditor` manages a single on-disc plaintext file (usually .csv). It is
#' the engine that powers function [shed()].
#'
#' @eval r6_usage(Sheditor)
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`file`}{The file to read/write}
#'
#'   \item{`data`}{a `data.frame`: the parsed contents of `file`}
#'
#'   \item{`format`}{A [ShedFormat]}
#'
#'   \item{`theme`}{see [load_theme()]}
#'
#'   \item{`locale`}{A [readr::locale()]}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`edit`}{Open a shiny app for editing `file`}
#' }
#'
#'
#' @name Sheditor
#' @aliases sheditor
#'
NULL




Sheditor <- R6::R6Class(
  "Sheditor",
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
        self$file  <- file
        self$format <- format
        self$locale <- locale

      },
    edit =
      function(
        x = NULL
      ){
        if (!is.null(x))
          self$file <- x

        res <- print(private$app(
          .data   = self$data,
          .file  = self$file,
          .format = self$format,
          .theme  = self$theme,
          .locale = self$locale
        ))

        invisible(self)
      },
    file = NULL,
    data = NULL,
    format = NULL,
    theme = NULL,
    locale = NULL
  ),

  private = list(
    app = function(
      .data,
      .file,
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
              class = "shedFileContainer",
              div(textInput("file", NULL, .file, width = "100%"), class = "fileSaved", id = "fileDiv")
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
          if (!has_colnames_row(.data)) .data <- colnames_to_row(.data)


          # startup -----------------------------------------------------------
          observeEvent(TRUE, once = TRUE, {
            lg$trace("App startup", event = "SessionStarted")

            values[["overwrite"]] <- FALSE
            values[["modified"]]  <- FALSE
            values[["output"]]    <- prep_input_df(.data)

            stopifnot(
              is_ShedFormat(.format),
              is.data.frame(values[["output"]]),
              is_bool(values[["modified"]]),
              is_bool(values[["overwrite"]])
            )
          })


          # infile ui -----------------------------------------------------
          observe({
            lg$trace("File path was modified", event = "InputFilePathModified")

            if (!file.exists(input$file)){
              values[["modified"]] <- TRUE
            }

            if (isTRUE(values[["modified"]])){
              lg$trace("Input file status changed",  status = "NotSaved")
              shinyjs::runjs('document.getElementById("fileDiv").className  = "fileNotSaved";')
            } else {
              lg$trace("Input file status changed",  status = "Saved")
              shinyjs::runjs('document.getElementById("fileDiv").className  = "fileSaved";')
            }
          })


          # render hot ----------------------------------------------------
          output$hot <- renderRHandsontable({
            if (is.data.frame(values[["output"]])){
              lg$trace("Rendering HOT", event = "renderHOT", data = values[["output"]])
              rhandsontable_shed(values[["output"]])
            } else {
              lg$trace(
                "`output` is not a data.frame but %s",
                fmt_class(values[["output"]])
              )
              NULL
            }
          })


          # i/o -----------------------------------------------------------------

          # +- edit hot ----------------------------------------------------------
          observeEvent(input$hot, {
            lg$trace("HOT as modified by user", event = "userModifiedHot")

            if (!is.null(input$hot)) {
              values[["output"]]   <- prep_input_df(hot_to_r_safely(input$hot))

              if (
                identical(nrow(values[["output"]]), 0L) ||
                identical(ncol(values[["output"]]), 0L)
              ){
                lg$debug(
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


          # +- save --------------------------------------------------------------
          save_file <- function(){
            lg$trace("Saving file", event = "saveFile")
            assert_only_char_cols(values[["output"]])

            if (file.exists(input$file) && isTRUE(values[["overwrite"]]))
              lg$debug("Overwriting existing file", overwrite = TRUE, input$file)

            write_ok <- tryCatch(
              expr = {
                self$format$write(values[["output"]], path = input$file)
                TRUE
              },
              error = function(e){
                lg$error("Write function aborted with error: %s", e)
                FALSE
              }
            )

            is_saved <- write_ok && file.exists(input$file)

            if (is_saved){
              values[["output_saved"]] <- values[["output"]]
              values[["modified"]] <- FALSE
              lg$info("Saving file", file = input$file)

            } else {
              lg$error("Could not save file", file = input$file)
            }
          }


          observeEvent(input$btnSave, {
            lg$trace("User triggered btnSave", event = "btnSave")

            if (!file.exists(input$file) || isTRUE(values[["overwrite"]])){
              save_file()

            } else {
              lg$trace("Showing overwrite modal", event = "modalOverwriteShow")
              showModal(shiny::modalDialog(
                size = "s",
                div("Overwrite existing file?", style = "height: 40px; " ),
                shiny::actionButton("modalOverwriteYes", "Yes", class = "modal-button"),
                shiny::actionButton("modalOverwriteNo", "No", class = "modal-button"),
                footer = NULL
              ))
            }
          })


          # overwrite modal
          observeEvent(input$modalOverwriteYes, {
            lg$trace("User set overwrite to `TRUE`", event = "modalOverwriteYes")
            values[["overwrite"]] <- TRUE
            save_file()
            removeModal()
          })


          observeEvent(input$modalOverwriteNo, {
            lg$trace("Overwrite stays `FALSE`", event = "modalOverwriteNo")
            lg$info("File not saved")
            removeModal()
          })


          # +- load -------------------------------------------------------
          observeEvent(input$btnLoad, {
            lg$trace("User triggered btnLoad", event = "btnLoad")

            if (file.exists(input$file)){
              tryCatch(
                {
                  lg$info("Loading file", file = input$file)
                  output <- self$format$read(input$file, locale = .locale)
                  output <- prep_input_df(output)

                  values[["output"]] <- output
                  values[["output_saved"]] <- output
                  values[["modified"]] <- FALSE
                  values[["overwrite"]] <- FALSE
                  rm(output)
                },
                error = function(e) {
                  lg$error(
                    "Input file exists but cannot be read",
                    file = input$file,
                    error = e
                  )
                }
              )

            } else {
              lg$error("Input file does not exist", file = input$file)
            }

            assert_only_char_cols(values[["output"]])
          })


          # session end ---------------------------------------------------
          session$onSessionEnded(function() {
            lg$trace("App shutdown", event = "SessionEnded" )
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
    stop(lg$fatal("`x` is not a data.frame but %s", fmt_class(x)))

  if (length(x) == 0)
    stop(lg$fatal("`x` is a data.frame without columns"))

  if (!has_only_char_cols(x)){
    stop(lg$fatal(
      "All columns of `x` must be of type `character`.",
      column_types = paste(vapply(x, fmt_class, character(1)), collapse = ", ")
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
      format$read(input, locale = locale),
      error = function(e) empty_df(1, 1)
    ))


  if (is_integerish(input)) {
    if (length(input) == 1)  return(empty_df(1, input))
    if (length(input) == 2)  return(empty_df(input[[1]], input[[2]]))

    lg$error(
      "If `input` is an integer it must be of length 1 (cols) or 2 (rows, cols)",
      input = input
    )
  }

  return(empty_df(1, 1))
}




prep_input_df <- function(
  x,
  recover = function() stop("Preparing data.frame failed")
){
  # preconditions
    if (!is.data.frame(x)){
      lg$error("input must be a data.frame")
      return(recover())
    }

    ok <- tryCatch(
      assert_cell_limit(nrow(x), ncol(x)),
      error = function(e) { lg$fatal(e); FALSE }
    )
    if (!ok) return(recover())

  # init
    if (!has_only_char_cols(x)){
      lg$debug(paste(
        "Autoconverting all columns to character. 'shed' can only handle",
        "data.frames with all-character columns properly. Please ensure that",
        "the `read()` in your `ShedFormat` returns such data.frames."
      ))
      x[] <- lapply(x, as.character)
    }

  # return
  attr(x, "spec") <- NULL  # clean up readr 'spec'
  x
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
