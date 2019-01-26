tdf <- data.frame(
  x = c("äöüß", "blah"),
  y = c("blubb", "#$^@&#^*")
)

outdir <- system.file("tests", "testthat", "testdata", package = "shed")


write.csv(
  tdf,
  file = file.path(outdir, "tdf_utf8.csv"),
  fileEncoding = "UTF-8"
)

write.csv(
  tdf,
  file = file.path(outdir, "tdf_latin1.csv"),
  fileEncoding = "LATIN1"
)



lg$threshold(TRACE)

shed(file.path(outdir, "tdf_utf8.csv"))
shed(file.path(outdir, "tdf_latin1.csv"))


file.exists(file.path(outdir, "tdf_utf8.csv"))
