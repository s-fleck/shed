#' Edit csv Files With Shiny
#'
#' @param fname Input file
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
  file,
  informat = "csv",
  outformat = "csv",
  opts = list(
    css = system.file("css", "shed_dark.css", package = "shed"),
    font_size = getOption("shed.font_size", 14),
    write_funs = list(
      csv  = shed_write_csv,
      csv2 = shed_write_csv2,
      tsv  = shed_write_tsv
    ),
    read_funs = list(
      csv  = shed_read_csv,
      csv2 = shed_read_csv2,
      tsv  = shed_read_tsv
    ),
    read_encoding  = union(c("guess", "UTF-8"), iconvlist()),
    write_encoding = union("UTF-8", iconvlist())
  )
){
  # preconditions
  stopifnot(is_scalar_integerish(opts$font_size))
  stopifnot(is_css_file(opts$css))
  stopifnot(
    (is_scalar_character(file) && file.exists(file)) ||
    (is.data.frame(file))
  )

  # init
  theme <- paste(
    paste(readLines(opts$css), collapse = "\n"),
    sprintf("#hot tr td { font-size: %spx;  }", opts$font_size)
  )

  fname <- make_outfile_name(file)

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
          class = "shedFnameContainer",
          uiOutput("uiFname")
        ),
        div(
          class = "shedCtrl",
          actionButton("btnLoad", "load", class = "shedButton shedCtrlElement"),

          div(
              class = "shedDropdownContainer",
              selectInput("readFun", NULL, names(opts$read_funs), selected = informat)
            ),
          div(
            class = "shedDropdownContainer",
            selectInput("readEncoding", NULL, opts$read_encoding)
          ),
          div(class = "shedCtrlSpacing"),
          actionButton("btnSave", "save", class = "shedButton shedCtrlElement"),
          div(
            class = "shedDropdownContainer",
            selectInput("writeFun", NULL, names(opts$write_funs), selected = outformat)
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

    # reactives -----------------------------------------------------------
      values <- reactiveValues()
      read_fun  <- reactive({ opts$read_funs[[input$readFun]] })   #nolint
      write_fun <- reactive({ opts$write_funs[[input$writeFun]] })   #nolint


    # local funs ----------------------------------------------------------
      save_file <- function(){
        .output   <- values[["output"]]
        .fname    <- input$fname
        stopifnot( all(vapply(.output, is.character, logical(1))) )

        write_fun <- opts$write_funs[[input$writeFun]]
        write_fun(.output, path = .fname)
        values[["output_saved"]] <- .output
        values[["modified"]] <- FALSE
        flog.info("Saved to %s", .fname)
      }


    # I/O -----------------------------------------------------------------
      output$uiFname <- renderUI({
        flog.trace("Trigger input file color change")

        if (isTRUE(values[["modified"]])){
          div(textInput("fname", NULL, input$fname, width = "100%"), class = "fnameNotSaved")
        } else {
          div(textInput("fname", NULL, input$fname, width = "100%"), class = "fnameSaved")
        }
      })


    # Startup -----------------------------------------------------------------
      observe({
        flog.debug("Trigger App Startup")

        updateTextInput(session, inputId = "fname", value = fname)

        if (is.data.frame(file)) {
          flog.trace("Loading data from input data.frame")
          .output <- as.data.frame(rbind(
            colnames(file),
            as.matrix(file)
          ),
            stringsAsFactors = FALSE
          )

          if (!all(vapply(.output, is.character, logical(1)))) {
            flog.warn("All columns should be read as character")
          }

          values[["modified"]] <- TRUE

        } else {
          read    <- isolate(read_fun())
          .output <- read(fname, encoding = input[["readEncoding"]])
          values[["modified"]] <- FALSE
        }

        values[["output"]] <- .output
        rm(.output)
      })



      observeEvent(input$hot, {
        flog.trace("Trigger user input HOT update")

        if (!is.null(input$hot)) {
          values[["output"]]   <- hot_to_r(input$hot)
          values[["modified"]] <- !isTRUE(all.equal(
            try(unname(as.matrix(values[["output_saved"]])), silent = TRUE),
            unname(as.matrix(values[["output"]]))
          ))
        }
      })


      output$hot <- renderRHandsontable({
        flog.trace("Trigger HOT display update")
        flog.trace(to_string(head(values[["output"]])))

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
        flog.trace("Trigger Save Button")
        .fname  <- isolate(input$fname)
        .overwrite <- isolate(input$chkOverwrite)

        flog.trace("Target file %s", .fname)
        flog.trace("Overwrite is set to %s", .overwrite)

        if (!file.exists(.fname) || isTRUE(.overwrite)){
          save_file()

        } else {
          flog.trace("Trigger Overwrite Modal")
          showModal(shiny::modalDialog(
            title = "Overwrite?",
            size = "s",
            shiny::actionButton("modalOverwriteYes", "Yes"),
            shiny::actionButton("modalOverwriteNo", "No"),
            footer = NULL
          ))
        }

        rm(.overwrite)
        rm(.fname)
      })



      # Overwrite Modal ---------------------------------------------------------
      observeEvent(input$modalOverwriteYes, {
        updateCheckboxInput(session, "chkOverwrite", value = TRUE)
        save_file()
        removeModal()
      })

      observeEvent(input$modalOverwriteNo, {
        flog.info("Not saved")
        removeModal()
      })



      # load --------------------------------------------------------------------
      observeEvent(input$btnLoad, {
        flog.trace("Trigger Load Button")

        read <- isolate(read_fun())

        if (file.exists(input$fname)){
          tryCatch({
            flog.info("Loading data from file system: %s", input$fname)
            .output <- read(input$fname, encoding = input[["readEncoding"]])
            values[["output"]] <- .output
            values[["output_saved"]] <- .output
            values[["modified"]] <- FALSE
            rm(.output)
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


      session$onSessionEnded(function() {
        flog.trace("Trigger Session End")
        stopApp(isolate(values[["output"]]))
      })
    }
  )

  invisible(runApp(shed_app))
}




#' @rdname shed
#' @export
shed2 <- function(
  file
){
  shed(
    file = file,
    informat = "csv2",
    outformat = "csv2"
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
  flog.trace("Loaded data.frame: \n%s", to_string(head(res)))
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

  flog.trace("Loaded data.frame: \n%s", to_string(head(res)))
  res
}




shed_read_tsv  <- function(path, encoding){

  flog.debug("Reading file %s with encoding %s", path, encoding)

  if (encoding == "guess"){
    encoding <- guess_encoding2(path)
  }

  loc <- readr::locale(encoding = encoding)

  res <- suppressMessages(as.data.frame(
      readr::read_tsv(
        path,
        col_names = FALSE,
        col_types = readr::cols(.default = "c"),
      locale = loc
    )
  ))

  mostattributes(res) <- NULL

  flog.trace("Loaded data.frame: \n%s", to_string(head(res)))
  res
}


shed_write_csv  <- function(x, path){
  readr::write_excel_csv(x, path, col_names = FALSE, na = "")
}


shed_write_csv2 <- function(x, path){
  readr::write_excel_csv2(x, path, col_names = FALSE, na = "")
}


shed_write_tsv <- function(x, path) {
  readr::write_tsv(x, path, col_names = FALSE, na = "")
}




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
