#' @keywords internal
#' @import shiny rhandsontable
#' @noRd
"_PACKAGE"


.onLoad <- function(...) {
  op <- options()

  op.default <- list(
    shed.theme     = system.file("css", "shed_dark.css", package = "shed"),
    shed.row_limit = 1e4,
    shed.row_warn  = 1e3,
    shed.col_limit = 2e3,
    shed.col_warn  = 1e2
  )

  toset <- !(names(op.default) %in% names(op))
  if(any(toset)) options(op.default[toset])

  assign(
    "lg",
    lgr::get_logger("shed"),
    envir = parent.env(environment())
  )

  invisible()
}
