#' Edit csv Files With Shiny
#'
#' @param fname either:
#'   * A scalar `character`: Path to the input file
#'   * A `data.frame`
#'   * An `integer` scalar: number of columns of desired empty table
#'   * An `integer` vector of length 2: desired `rows, columns` of target table
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
#' shed(4)  # Empty table with 4 columns
#' shed(c(2, 4))  # Empty table with 2 rows and 4 columns
#' }
#'
#'
shed <- function(
  file = NULL,
  informat = "csv",
  outformat = "csv",
  opts = shed_opts()
){
  # preconditions
  stopifnot(
    is_scalar_integerish(opts$font_size),
    is_css_file(opts$css),

    is.null(file) ||
    (is_scalar_character(file)) ||
    (is.data.frame(file)) ||
    (is_integerish(file) && length(file) %in% 1:2)
  )

  # init

  if (is.null(file)) file <- 1L

  if (is_integerish(file)){

    stopifnot(all(file > 0))

    if (identical(length(file), 1L)){
      file <- c(1, file)
    }

    file <- empty_df(file[[1]], file[[2]])
  }


  theme <- paste(
    paste(readLines(opts$css), collapse = "\n"),
    sprintf("#hot tr td { font-size: %spx;  }", opts$font_size)
  )

  fname <- make_outfile_name(file)

  if (is_scalar_character(file) && !file.exists(file)){
    file <- empty_df(1, 1)
  }



  invisible(runApp(shed_app))
}




#' @rdname shed
#' @export
shed2 <- function(
  file = NULL
){
  shed(
    file = file,
    informat = "csv2",
    outformat = "csv2"
  )
}




#' @rdname shed
#' @export
shedx <- function(
  file = NULL
){
  shed(
    file = file,
    informat = "csv2",
    outformat = "excel_csv2"
  )
}




#' @rdname shed
#' @export
shedx2 <- function(
  file = NULL
){
  shed(
    file = file,
    informat = "csv2",
    outformat = "excel_csv2"
  )
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




#' Add hotkey that contains CTRL to a shiny app
#'
#' @param command scalar character. a java script command to execute
#' @param keys `integer` vector of Keycode numbers, see
#'   http://keycode.info
#'
#' @return `character` java script code
#' @noRd
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




parse_output_df <- function(x){
  res <- x[-1, , drop = FALSE]
  colnames(res) <- as.character(x[1, ])
  res[] <- lapply(res, readr::parse_guess)
  rownames(res) <- NULL
  res
}



empty_df <- function(rows, cols){
  res <- as.list(rep("", cols))
  res[[1]] <- rep("", rows)
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  names(res) <- paste0("X", seq_len(cols))
  res
}



hot_to_r_safely <- function(...){

  res <- tryCatch(
    hot_to_r(...),
    error = function(e) {
      flog.error(e)
      flog.debug("Cannot convert Handsontable, returning empty 0x0 data.frame instead.")
      empty_df(0, 0)
    }
  )

  stopifnot(is.data.frame(res))

  if (identical(nrow(res), 0L) || identical(ncol(res), 0L)){
    flog.debug("Cannot handle zero-row data.frame, returning empty 0x0 data.frame instead.")
    empty_df(0, 0)
  } else {
    res
  }
}



rhandsontable_shed <- function(data, opts){
  rhandsontable(
    data,
    readOnly = FALSE,
    useTypes = FALSE,
    colHeaders = NULL,
    rowHeights = getOption("shed.font_size", 14L) + 20
  )
}
