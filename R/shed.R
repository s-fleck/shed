#' Edit csv Files With Shiny
#'
#' @param infile Input file
#' @param outfile Output file path
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
  opts = list(
    css = system.file("css", "shed_dark.css", package = "shed"),
    font_size = getOption("shed.font_size", 14),
    write_funs = list(
      csv  = shed_write_csv,
      csv2 = shed_write_csv2
    ),
    read_funs = list(
      csv  = shed_read_csv,
      csv2 = shed_read_csv2
    ),
    read_encoding  = union(c("guess", "UTF-8"), iconvlist()),
    write_encoding = union("UTF-8", iconvlist())
  )
){
  # preconditions
  stopifnot(is_scalar_integerish(opts$font_size))
  stopifnot(is_css_file(opts$css))
  stopifnot(
    (is_scalar_character(infile) && file.exists(infile)) ||
    (is.data.frame(infile))
  )
  stopifnot(is_scalar_character(outfile))

  # init
  theme <- paste(
    paste(readLines(opts$css), collapse = "\n"),
    sprintf("#hot tr td { font-size: %spx;  }", opts$font_size)
  )


  shed_app <- shiny::shinyApp(
    ui = fluidPage(
      width = "100%",
      tags$head(
        tags$style(HTML(theme)),
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
          class = "shedInfileContainer",
          uiOutput("uiInfile")
        ),


        div(
          class = "shedCtrl",

          actionButton("btnLoad", "load", class = "shedButton shedCtrlElement"),

          div(
            class = "shedDropdownContainer",
            selectInput("readFun", NULL, names(opts$read_funs))
          ),

          div(
            class = "shedDropdownContainer",
            selectInput("readEncoding", NULL, opts$read_encoding)
          ),

          div(class = "shedCtrlSpacing"),

          actionButton("btnSave", "save", class = "shedButton shedCtrlElement"),

          div(
            class = "shedDropdownContainer",
            selectInput("writeFun", NULL, names(opts$write_funs))
          ),

          div(
            class = "shedDropdownContainer",
            shiny::checkboxInput("chkOverwrite", "overwrite", value = FALSE)
          )
        )
      ),

      absolutePanel(
        rHandsontableOutput("hot"),
        top = 160,
        left = 0,
        right = 0
      )
    ),


    server = function(input, output, session) {

      values <- reactiveValues()
      read_fun  <- reactive({ opts$read_funs[[input$readFun]] })   #nolint
      write_fun <- reactive({ opts$write_funs[[input$writeFun]] })   #nolint


    # I/O ---------------------------------------------------------------------
      output$uiInfile <- renderUI({
        if (isTRUE(values[["modified"]])){
          div(textInput("outputFile", NULL, outfile, width = "100%"), class = "infileNotSaved")
        } else {
          div(textInput("outputFile", NULL, outfile, width = "100%"), class = "infileSaved")
        }
      })


      observe({

        if (!is.null(input$hot)) {
          flog.trace("Loading data.frame from HOT")
          .output   <- hot_to_r(input$hot)
        } else if (!is.null(values[["output"]])) {
          flog.trace("Loading data.frame from output")
          .output <- values[["output"]]
        } else if (is.data.frame(infile)) {
          flog.trace("Loading data.frame from input data.frame")
          .output <- as.data.frame(rbind(
            colnames(infile),
            as.matrix(infile)
          ),
            stringsAsFactors = FALSE
          )

          if (!all(vapply(.output, is.character, logical(1)))) {
            flog.warn("All columns should be read as character")
          }

        } else {
            flog.trace("Loading data.frame from input file")
            .output <- read_fun()(infile, input[["readEncoding"]])
        }

        values[["output"]]   <- .output

        values[["modified"]] <- !isTRUE(all.equal(
          try(unname(as.matrix(values[["output_saved"]])), silent = TRUE),
          unname(as.matrix(.output))
        ))
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



      # save --------------------------------------------------------------------
      observeEvent(input$btnSave, {
        .output   <- isolate(values[["output"]] )


        if (file.exists(outfile)){
          showModal(shiny::modalDialog(
            title = "Overwrite?",
            size = "s",
            shiny::actionButton("modalOverwriteYes", "Yes"),
            shiny::actionButton("modalOverwriteNo", "No"),
            footer = NULL
          ))
        }

        if (!all(vapply(.output, is.character, logical(1)))) {
          flog.warn("All columns should be read as character")
        }

        write_fun <- opts$write_funs[[input$writeFun]]
        write_fun(.output, path = input$outputFile)
        values[["output_saved"]] <- .output
        values[["modified"]] <- FALSE
        flog.info("Saved to %s", input$outputFile)
      })



      # Overwrite Modal ---------------------------------------------------------
      observeEvent(input$modalOverwriteYes,{
        print("overwrite yes")
        removeModal()
      })

      observeEvent(input$modalOverwriteNo,{
        print("overwrite no")
        removeModal()
      })





      # load --------------------------------------------------------------------
      observeEvent(input$btnLoad, {

        read_fun <- isolate(read_fun())

        if (file.exists(input$outputFile)){
          tryCatch({
            .output <- read_fun(input$outputFile, encoding = input[["readEncoding"]])
            flog.info("Loaded %s", input$outputFile)
            values[["output"]] <- .output
            values[["output_saved"]] <- .output
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
    opts = list(
      css = system.file("css", "shed_dark.css", package = "shed"),
      font_size = getOption("shed.font_size", 14),
      write_funs = list(
        csv2 = shed_write_csv2,
        csv  = shed_write_csv
      ),
      read_funs = list(
        csv2 = shed_read_csv2,
        csv  = shed_read_csv
      )
    )

  )
}




# helpers -----------------------------------------------------------------

shed_read_csv   <- function(path, encoding){

  flog.debug("Reading file %s with encoding %s", path, encoding)

  if (encoding == "guess"){
    encoding <- guess_encoding2(path)
  }

  loc <- readr::locale(encoding = encoding)

  res <- as.data.frame(
    readr::read_csv(
      path,
      col_names = FALSE,
      col_types = readr::cols(.default = "c")),
      locale = loc
    )

  mostattributes(res) <- NULL
  flog.trace("Loaded data.frame: \n%s", to_string(res))
  res
}




shed_read_csv2  <- function(path, encoding){

  flog.debug("Reading file %s with encoding %s", path, encoding)

  if (encoding == "guess"){
    encoding <- guess_encoding2(path)
  }

  loc <- readr::locale(encoding = encoding)

  res <- suppressMessages(as.data.frame(
      readr::read_csv2(
        path,
        col_names = FALSE,
        col_types = readr::cols(.default = "c"),
      locale = loc
    )
  ))

  mostattributes(res) <- NULL

  flog.trace("Loaded data.frame: \n%s", to_string(res))
  res
}


shed_write_csv  <- function(x, path)
  readr::write_excel_csv(x, path, col_names = FALSE, na = "")

shed_write_csv2 <- function(x, path)
  readr::write_excel_csv2(x, path, col_names = FALSE, na = "")




make_outfile_name <- function(x){
  ifelse(
    is.character(x) && (length(x) == 1),
    x,
    tempfile(fileext = ".csv")
  )
}



guess_encoding2 <- function(path, default = "UTF-8"){
  dd <- readr::guess_encoding(path)

  if (nrow(dd) > 0){
    res <- dd[[1, 1]]
    flog.debug("Guessed Encoding: %s", res)
  } else {
    res <- default
    flog.debug("Could not determine encoding. Falling back to %s", default)
  }

  res
}



to_string <- function(x){
  paste(capture.output(print(x)), collapse = "\n")
}


#' Title
#'
#' @param command
#' @param keys `integer` vector of Keycode numbers, see
#'   http://keycode.info
#'
#' @return `character` java script code
js_add_ctrl_hotkey <- function(command = 'console.log("pressed")', key){
  stopifnot(length(key) == 1)

  sprintf(
  "
  $(document).keydown(function(e) {
    if (e.keyCode == %s && e.ctrlKey) {
      e.preventDefault();
      %s;
    }
  });
  ",
  key,
  command
  )

}
