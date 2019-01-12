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
#' \donttest{
#' shed(iris)
#' }
#' \dontrun{
#' shed(4)  # Empty table with 4 columns
#' shed(c(2, 4))  # Empty table with 2 rows and 4 columns
#' }
#'
#'
shed <- function(
  input,
  file   = if (is_scalar_character(input)) input else tempfile(),
  format = shed_format_csv,
  locale = readr::locale(),
  theme = "default"
){
  editor <- Sheditor$new(
    input = input,
    file = file,
    format = format,
    locale = locale,
    theme = theme
  )

  editor$edit()
  invisible(editor$data)
}




#' @rdname shed
#' @export
shed2 <- function(
  input,
  file   = if (is_scalar_character(input)) input else tempfile(),
  locale = readr::locale(),
  theme = "default"
){
  editor <- Sheditor$new(
    input = input,
    file = file,
    format = shed_format_csv2,
    locale = locale,
    theme = theme
  )

  editor$edit()
  invisible(editor$data)
}




#' @rdname shed
#' @export
shedx <- function(
  input,
  file   = if (is_scalar_character(input)) input else tempfile(),
  locale = readr::locale(),
  theme = "default"
){
  editor <- Sheditor$new(
    input = input,
    file = file,
    format = shed_format_csvx,
    locale = locale,
    theme = theme
  )

  editor$edit()
  invisible(editor$data)
}




#' @rdname shed
#' @export
shed2x <- function(
  input,
  file   = if (is_scalar_character(input)) input else tempfile(),
  locale = readr::locale(),
  theme = "default"
){
  editor <- Sheditor$new(
    input = input,
    file = file,
    format = shed_format_csv2x,
    locale = locale,
    theme = theme
  )

  editor$edit()
  invisible(editor$data)
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
    lg$debug("Guessed Encoding: %s", res)
  } else {
    res <- default
    lg$debug("Could not determine encoding. Falling back to %s", default)
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




empty_df <- function(
  rows = 1,
  cols = 1
){
  assert_cell_limit(rows, cols)
  lg$trace("Generating empty data.frame", dimensions = c(rows = rows, cols = cols))
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
      lg$error(e)
      lg$debug("Cannot convert Handsontable, returning empty 0x0 data.frame instead.")
      empty_df(0, 0)
    }
  )

  stopifnot(is.data.frame(res))

  if (identical(nrow(res), 0L) || identical(ncol(res), 0L)){
    lg$debug("Cannot handle zero-row data.frame, returning empty 0x0 data.frame instead.")
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




assert_cell_limit <- function(
  rows,
  cols
){
  rows <- max(rows, 1)
  cols <- max(cols, 1)

  if (
    rows >= getOption("shed.row_limit") ||
    cols >= getOption("shed.col_limit")
  ){
    stop(lg$fatal(
      "Input dataset to big",
      dimensions = c(rows = rows, cols = cols),
      shed.row_limit = getOption("shed.row_limit"),
      shed.col_limit = getOption("shed.col_limit")
    ))
  } else if (
    rows >= getOption("shed.row_warn") ||
    cols >= getOption("shed.col_warn")
  ){
    lg$warn(
      "Input dataset is big and editing the table might be slow",
      dimensions = c(rows = rows, cols = cols),
      shed.row_limit = getOption("shed.row_limit"),
      shed.col_limit = getOption("shed.col_limit"),
      shed.row_warn = getOption("shed.row_warn"),
      shed.col_warn = getOption("shed.col_warn")
    )
  }

  TRUE
}
