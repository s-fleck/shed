#' Edit csv Files With Shiny
#'
#' @param infile Input file
#' @param outfile Output file path
#' @param write_funs write functions
#' @param read_funs read functions
#' @param opts Options to configure behaviour and appearence of the shed
#'   app (see below)
#'
#'
#' @section Options:
#'
#'  Options can be set with `options(option = value)`
#'
#'  \describe{
#'    \item{shed.font_size}{Font Size of the Table Cells}
#'  }
#'
#'
#' @return The edited csv file as a `data.frame` (invisibly)
#' @export
#'
#' @examples
#'
#' \dontrun{
#' shed(iris)
#' }
#'
shed <- function(
  infile,
  outfile = make_outfile_name(infile),
  write_funs = list(
    csv  = shed_write_csv,
    csv2 = shed_write_csv2
  ),
  read_funs = list(
    csv  = shed_read_csv,
    csv2 = shed_read_csv2
  ),
  opts = list(
    css = system.file("css", "shed_dark.css", package = "shed"),
    font_size = getOption("shed.font_size", 14)
  )
){
  # preconditions
  stopifnot(is_scalar_integerish(opts$font_size))
  stopifnot(is_css_file(opts$css))
  stopifnot(is_scalar_character(infile) || is.data.frame(infile))
  stopifnot(is_scalar_character(outfile))

  # init
  theme <- paste(
    paste(readLines(opts$css), collapse = "\n"),
    sprintf("#hot tr td { font-size: %spx;  }", opts$font_size)
  )


  shed_app <- shiny::shinyApp(
    ui = fluidPage(
      width = "100%",
      tags$head(tags$style(HTML(theme)) ),

      fixedPanel(
        id = "panelTop",
        top = 0,
        left = 0,
        right = 0,
        textInput("outputFile", NULL, outfile, width = "100%"),

        div(
          style = "display: inline-block;",
          actionButton("btnLoad", "load", class = "shedButton shedCtrlElement"),

          div(
            class = "shedDropdownContainer",
            selectInput("readFun", NULL, names(read_funs))
          ),

          actionButton("btnSave", "save", class = "shedButton shedCtrlElement"),

          div(
            class = "shedDropdownContainer",
            selectInput("writeFun", NULL, names(write_funs))
          )
        )),

      absolutePanel(
        rHandsontableOutput("hot"),
        top = 130,
        left = 0,
        right = 0
      )
    ),


    server = function(input, output, session) {

      values <- reactiveValues()
      read_fun  <- reactive({ read_funs[[input$readFun]] })   #nolint
      write_fun <- reactive({ write_funs[[input$writeFun]] })   #nolint

      observe({
        if (!is.null(input$hot)) {
          values[["previous"]] <- isolate(values[["output"]])
          output <- hot_to_r(input$hot)
        } else if (!is.null(values[["output"]])) {
          output <- values[["output"]]
        } else if (is.data.frame(infile)) {
          output <- as.data.frame(rbind(
            colnames(infile),
            as.matrix(infile)
          ),
            stringsAsFactors = FALSE)

          if (!all(vapply(output, is.character, logical(1)))) {
            flog.warn("All columns should be read as character")
          }

        } else {
          output <- read_fun()(infile)
        }

        values[["output"]] <- output
      })

      output$hot <- renderRHandsontable({
        if (!is.null(values[["output"]])){
          rhandsontable(
            values[["output"]],
            readOnly = FALSE,
            useTypes = FALSE,
            colHeaders = NULL,
            rowHeights = opts$font_size + 20
          )
        }
      })


      observeEvent(input$btnSave, {
        .output <- isolate(values[["output"]] )

        if (!all(vapply(.output, is.character, logical(1)))) {
          flog.warn("All columns should be read as character")
        }

        write_fun <- write_funs[[input$writeFun]]
        write_fun(.output, path = input$outputFile)
        flog.info("Saved to %s", input$outputFile)
      })


      observeEvent(input$btnLoad, {

        read_fun <- isolate(read_fun())

        if (file.exists(input$outputFile)){
          tryCatch({
            .output <- read_fun(input$outputFile)

            flog.debug(
              "Loaded data.frame with structure \n\n %s",
              paste(capture.output(str(.output )), collapse = "\n")
            )

            flog.info("Loaded %s", input$outputFile)
            values[["output"]] <- .output
          },
            error = function(e) {
              flog.error("Input file exists but cannot be read %s", input$outputFile)
              flog.error("Reason: %s", e)
            }
          )

        } else {
          flog.error("Input file does not exist: %s", input$outputFile)
        }

        if (!all(vapply(values[["output"]], is.character, logical(1)))) {
          flog.warn("All columns should be read as character")
        }
      })


      session$onSessionEnded(function() {
        stopApp(isolate(values[["output"]]))
      })
    }
  )

  invisible(runApp(shed_app))
}




#' @rdname shed
#' @export
shed2 <- function(
  infile,
  outfile = make_outfile_name(infile)
){
  shed(
    infile = infile,
    outfile = outfile,
    write_funs = list(
      csv2 = shed_write_csv2,
      csv  = shed_write_csv
    ),
    read_funs = list(
      csv2 = shed_read_csv2,
      csv  = shed_read_csv
    )
  )
}




# helpers -----------------------------------------------------------------

shed_read_csv   <- function(path){
  suppressMessages(as.data.frame(
    readr::read_csv(
      path,
      col_names = FALSE,
      col_types = readr::cols(.default = "c"))
    )
  )

}


shed_read_csv2  <- function(path){
  res <- suppressMessages(as.data.frame(
      readr::read_csv2(
        path,
        col_names = FALSE,
        col_types = readr::cols(.default = "c")
      )
  ))
}


shed_write_csv  <- function(x, path)
  readr::write_excel_csv(x, path, col_names = FALSE)

shed_write_csv2 <- function(x, path)
  readr::write_excel_csv2(x, path, col_names = FALSE)




make_outfile_name <- function(x){
  ifelse(
    is.character(x) && (length(x) == 1),
    x,
    tempfile(fileext = ".csv")
  )
}
