#' Title
#'
#' @param infile
#' @param outfile
#' @param ...
#'
#' @return The edited csv file a `data.frame` (invisibly)
#' @export
#'
#' @examples
#'
run <- function(
  infile  = system.file("iris.csv", package = "csvshine"),
  outfile = ifelse (rlang::is_scalar_character(infile), infile, tempfile(fileext = ".csv")),
  informat = "csv",
  outformat = informat
){
  tf <- tempfile()
  on.exit(try(unlink(tf)))

  shinycsv_app <- shiny::shinyApp(
    ui = fluidPage(
      theme = shinythemes::shinytheme("superhero"),
      width = "100%",
      tags$head(
        tags$style(HTML(shinycsv_css))
      ),

      fixedPanel(
        id = "panelTop",
        top = 0,
        left = 0,
        right = 0,
        textInput("outputFile", NULL, outfile, width = "100%"),

        div(
          style="display: inline-block;vertical-align:top;",
          actionButton("btnLoad", "load", style="padding:6px; font-size:90%", width = 80)),

        div(
          style="display: inline-block;vertical-align:top;width:130px",
          selectInput(
            "writeFun",
            NULL,
            c("csv", "csv2"),
            selected = informat,
            width = 80
          )
        ),

        div(
          style="display: inline-block;vertical-align:top;height:30px",
          actionButton("btnSave", "save", style="padding:6px; font-size:90%", width = 80)),
        div(
          style="display: inline-block;vertical-align:top;width:130px",
          selectInput(
            "readFun",
            NULL,
            c("csv", "csv2"),
            selected = outformat,
            width = 80
          )
        )
      ),

      absolutePanel(
        rHandsontableOutput("hot"),
        top = 130,
        left = 0,
        right = 0
      )
    ),


    server = function(input, output, session) {

      values <- reactiveValues()

      write_funs <- list(
        csv  = purrr::partial(readr::write_excel_csv, col_names = FALSE),
        csv2 = purrr::partial(readr::write_excel_csv2, col_names = FALSE)
      )

      read_funs <- list(
        csv  = purrr::partial(readr::read_csv, col_names = FALSE),
        csv2 = purrr::partial(readr::read_csv2, col_names = FALSE)
      )

      write_fun <- reactive({ write_funs[[input$writeFun]] })
      read_fun  <- reactive({ read_funs[[input$readFun]] })

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
          ))
        } else {
          output <- read_fun()(infile)
        }

        values[["output"]] <- output
      })

      output$hot <- renderRHandsontable({
        if(!is.null(values[["output"]])){
          rhandsontable(values[["output"]], readOnly = FALSE, useTypes = FALSE, colHeaders = NULL)
        }
      })

      observeEvent(input$btnSave, {
        .output <- isolate(values[["output"]] )
        .output[] <- lapply(.output, readr::parse_guess)
        write_fun()(.output, path = input$outputFile)
        saveRDS(.output, file = tf)
      })

      observeEvent(input$btnLoad, {
        try(values[["output"]] <- read_fun()(outfile))
      })

      session$onSessionEnded(function() {
        stopApp(isolate(values[["output"]]))
      })

    }
  )

  invisible(runApp(shinycsv_app))
}



run2 <- function(
  infile  = system.file("iris.csv", package = "csvshine"),
  outfile = infile
){
  run(
    infile = infile,
    outfile = outfile,
    informat = "csv2",
    outformat = "csv2"
  )
}




shinycsv_css <-
"
  #panelTop {
    background: #bbbbbb;
    height = 250px;
    margin-top = 35px;
    margin-bottom = 35px;
    z-index: 10000;
  }

  .handsontable .currentRow {
    background-color: #E7E8EF;
  }

  .handsontable .currentCol {
    background-color: #F9F9FB;
  }

  .handsontable {
    overflow: auto;
    color: black;
  }
"
