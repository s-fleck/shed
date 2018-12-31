tdf <- data.frame(
  x = "äöüß"
)

outdir <- system.file("tests", "testthat", "testdata", package = "shed")


write.csv(
  tdf,
  file = file.path(outdir, "tdf_utf8.csv"),
  fileEncoding = "UTF-8"
)

cat(readr::read_lines(file.path(outdir, "tdf_utf8.csv")))

write.csv(
  tdf,
  file = file.path(outdir, "tdf_latin1.csv"),
  fileEncoding = "LATIN1"
)

cat(readr::read_lines(file.path(outdir, "tdf_latin1.csv")))

