shed_opts <- function(
  css = getOption(
    "shed.css",
    system.file("css", "shed_dark.css", package = "shed")
  ),
  font_size = getOption("shed.font_size", 14L),
  write_funs = getOption(
    "shed.write_funs",
    list(
      csv  = shed:::shed_write_csv,
      csv2 = shed:::shed_write_csv2,
      excel_csv  = shed:::shed_write_excel_csv,
      excel_csv2 = shed:::shed_write_excel_csv2,
      tsv  = shed:::shed_write_tsv
    )
  ),
  read_funs = getOption(
    "shed.write_funs",
    list(
      csv  = shed:::shed_read_csv,
      csv2 = shed:::shed_read_csv2,
      tsv  = shed:::shed_read_tsv
    )
  ),
  read_encoding  =
    getOption("shed.read_encoding", union(c("guess", "UTF-8"), iconvlist()) ),
  write_encoding =
    getOption("shed.write_encoding", union(c("UTF-8"), iconvlist()) )
){
  list(
    css = css,
    font_size = font_size,
    write_funs = write_funs,
    read_funs = read_funs,
    read_encoding = read_encoding,
    write_encoding = write_encoding
  )
}
