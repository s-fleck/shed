#' @keywords internal
#' @import shiny rhandsontable futile.logger
#' @noRd
"_PACKAGE"


.onLoad <- function(...) {
  op <- options()

  op.default <- list(
    shed.theme = system.file("css", "shed_dark.css", package = "shed")
  )

  toset <- !(names(op.default) %in% names(op))
  if(any(toset)) options(op.default[toset])

  assign(
    "lg",
    lgr::Logger$new(name = "shed", threshold = 400L),
    envir = parent.env(environment())
  )

  invisible()
}
