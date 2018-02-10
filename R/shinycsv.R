# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


run <- function(
  infile  = system.file("iris.csv", package = "shinycsv"),
  outfile = infile,
  ...
){

  shinycsv_output <- new.env()

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
        actionButton("btnSave", "save"),
        actionButton("btnLoad", "load")
      ),

      absolutePanel(
        rHandsontableOutput("hot"),
        top = 150,
        left = 0,
        right = 0
      )
    ),


    server = function(input, output, session) {
      values <- reactiveValues()
      observe({
        if (!is.null(input$hot)) {
          values[["previous"]] <- isolate(values[["output"]])
          output <- hot_to_r(input$hot)
        } else if (!is.null(values[["output"]])) {
          output <- values[["output"]]
        } else {
          output <- data.table::fread(infile)
        }

        values[["output"]] <- output
        assign("output", output, envir = shinycsv_output)
      })


      output$hot <- renderRHandsontable({
        if(!is.null(values[["output"]])){
          rhandsontable(values[["output"]], readOnly = FALSE, useTypes = FALSE)
        }
      })

      observeEvent(input$btnSave, {
        .output <- isolate(values[["output"]] )
        .output[] <- lapply(.output, readr::parse_guess)
        data.table::fwrite(.output, file = input$outputFile)
      })

      session$onSessionEnded(
        function(...) isolate(values[["output"]])
      )
    }
  )

  runApp(shinycsv_app)
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
