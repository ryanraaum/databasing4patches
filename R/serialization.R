
cerealize <- function(object) {
  paste0(memCompress(serialize(object, NULL), "bzip2"), collapse="")
}

decerealize <- function(cereal) {
  flakes_bz <- substring(cereal, seq(1,nchar(cereal),2), seq(2,nchar(cereal),2))
  flakes_raw <- flakes_bz |> strtoi(16L) |> as.raw()
  flakes <- memDecompress(flakes_raw, "bzip2")
  unserialize(as.raw(as.integer(paste0('0x', flakes))))
}
