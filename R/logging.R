.onLoad <- function(libname, pkgname) {
  logger::log_formatter(logger::formatter_pander, namespace = pkgname)
}

type_signature <- function(tbl) {
  types <- as.character(vctrs::vec_ptype(as_tibble(tbl)))
  paste(names(tbl), types, sep = " = ", collapse = "; ")
}
