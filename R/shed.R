#' Title
#'
#' @param infile
#' @param outfile
#' @param ...
#'
#' @return The edited csv file as a `data.frame` (invisibly)
#' @export
#'
#' @examples
#'
shed <- function(
  infile,
  outfile = make_outfile_name(infile),
  write_funs = list(
    csv  = purrr::partial(readr::write_excel_csv, col_names = FALSE),
    csv2 = purrr::partial(readr::write_excel_csv2, col_names = FALSE)
  ),
  read_funs = list(
    csv  = purrr::compose(
      as.data.frame,
      purrr::partial(readr::read_csv, col_names = FALSE)
    ),
    csv2 = purrr::compose(
      as.data.frame,
      purrr::partial(readr::read_csv2, col_names = FALSE)
    )
  )
){
  shed_app <- shiny::shinyApp(
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
            "readFun",
            NULL,
            names(read_funs),
            width = 80
          )
        ),

        div(
          style="display: inline-block;vertical-align:top;height:30px",
          actionButton("btnSave", "save", style="padding:6px; font-size:90%", width = 80)),
        div(
          style="display: inline-block;vertical-align:top;width:130px",
          selectInput(
            "writeFun",
            NULL,
            names(write_funs),
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
        write_fun <- write_funs[[input$writeFun]]
        write_fun(.output, path = input$outputFile)
      })

      observeEvent(input$btnLoad, {
        read_fun <- read_funs[[input$readFun]]
        try(values[["output"]] <- read_fun(file = input$outputFile))
      })

      session$onSessionEnded(function() {
        stopApp(isolate(values[["output"]]))
      })

    }
  )

  invisible(runApp(shed_app))
}



shed2 <- function(
  infile,
  outfile = make_outfile_name(infile)
){
  shed(
    infile = infile,
    outfile = outfile,
    write_funs = list(
      csv2 = purrr::partial(readr::write_excel_csv2, col_names = FALSE),
      csv  = purrr::partial(readr::write_excel_csv, col_names = FALSE)
    ),
    read_funs = list(
      csv2 = purrr::compose(
        as.data.frame,
        purrr::partial(readr::read_csv2, col_names = FALSE)
      ),
      csv  = purrr::compose(
        as.data.frame,
        purrr::partial(readr::read_csv, col_names = FALSE)
      )
    )
  )
}



shed_split <- function(
  infile,
  outfile = make_outfile_name(infile),
  write_funs = list(
    csv  = purrr::partial(readr::write_excel_csv, col_names = FALSE),
    csv2 = purrr::partial(readr::write_excel_csv2, col_names = FALSE)
  ),
  read_funs = list(
    csv  = purrr::compose(
      as.data.frame,
      purrr::partial(readr::read_csv, col_names = FALSE)
    ),
    csv2 = purrr::compose(
      as.data.frame,
      purrr::partial(readr::read_csv2, col_names = FALSE)
    )
  )
){
  shed_app <- shiny::shinyApp(
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
            "readFun",
            NULL,
            names(read_funs),
            width = 80
          )
        ),

        div(
          style="display: inline-block;vertical-align:top;height:30px",
          actionButton("btnSave", "save", style="padding:6px; font-size:90%", width = 80)),
        div(
          style="display: inline-block;vertical-align:top;width:130px",
          selectInput(
            "writeFun",
            NULL,
            names(write_funs),
            width = 80
          )
        )
      ),

      absolutePanel(
        top = 200,
        left = 0,
        right = 0,
        splitLayout(
          rHandsontableOutput("hot"),
          verbatimTextOutput("text")
        )
      )
    ),


    server = function(input, output, session) {

      values    <- reactiveValues()
      read_fun  <- reactive({ read_funs[[input$readFun]] })
      write_fun <- reactive({ write_funs[[input$writeFun]] })

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
          output_text <- ""
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


      output_text <- reactive({
        try(paste(values[["output_text"]], collapse = "\n"))
      })

      output$text <- renderText({ output_text() })


      observeEvent(input$btnSave, {
        .output <- isolate(values[["output"]] )
        .output[] <- lapply(.output, readr::parse_guess)
        write_fun()(.output, path = input$outputFile)
        values[["output_text"]] <- readLines(input$outputFile)
      })

      observeEvent(input$btnLoad, {
        try(values[["output"]] <- read_fun()(file = input$outputFile))
        try(values[["output_text"]] <- readLines(input$outputFile))
      })

      session$onSessionEnded(function() {
        stopApp(isolate(values[["output"]]))
      })

    }
  )

  invisible(runApp(shed_app))
}






make_outfile_name <- function(x){
  ifelse (rlang::is_scalar_character(x), x, tempfile(fileext = ".csv"))
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


shinycsv_css <-
  "
  #panelTop {
    background: #bbbbbb;
    height = 250px;
    margin-top = 35px;
    margin-bottom = 35px;
    z-index: 10000;
  }

  handsontable .currentRow {
    background-color: #E7E8EF;
  }

  handsontable .currentCol {
    background-color: #F9F9FB;
  }

  .handsontable {
    overflow: auto;
  }

  /* Master */
  #hot tr td {
    background-color: #181712;
    color: #f0f0f0
  }

  /* All headers */
  #hot.handsontable th {
    background-color: #2b3e50;
    color: #757571;
  }

  /* Context Menue */
  .htMenu tr td {
    color: #2b3e50;
  }

  /* Borders (data) */
  #hot .ht_master tr > td {
      border-bottom: 1px solid #2b3e50!important;
      border-right: 1px solid #2b3e50!important;
      border-top: 1px solid #2b3e50!important;
      border-left: 1px solid #2b3e50!important;
   }

  /* Borders (row numbers) */
  #hot .ht_clone_left th {
      border-bottom: 1px solid #2b3e50!important;
      border-right: 1px solid #2b3e50!important;
      border-top: 1px solid #2b3e50!important;
      border-left: 1px solid #2b3e50!important;
  }
"

