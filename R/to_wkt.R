to_wkt_pt <- function(x, fmt, prefix = "POINT ") {
  sprintf(
    "%s(%s)", prefix,
    paste0(format(x, nsmall = fmt, trim = TRUE), collapse = " ")
  )
}

to_wkt_poly <- function(x, fmt, prefix = "POLYGON ") {
  sprintf(
    "%s((%s))", prefix,
    paste0(vapply(x, function(z) paste0(format(z, nsmall = fmt, trim = TRUE),
                                        collapse = " "), ""), collapse = ", ")
  )
}

to_wkt_linestring <- function(x, fmt, prefix = "LINESTRING ") {
  sprintf(
    "%s(%s)", prefix,
    paste0(vapply(x, function(z) paste0(format(z, nsmall = fmt, trim = TRUE),
                                        collapse = " "), ""), collapse = ", ")
  )
}

to_wkt_multi <- function(x, fmt, prefix, fn_to_wkt) {
  sprintf(
    "%s(%s)", prefix,
    paste0(vapply(x, function(z) fn_to_wkt(z, fmt, prefix = ""), ""),
           collapse = ", ")
  )
}

to_wkt_multipoint <- function(x, fmt) {
  to_wkt_multi(x, fmt, "MULTIPOINT ", to_wkt_pt)
}

to_wkt_multilinestring <- function(x, fmt) {
  to_wkt_multi(x, fmt, "MULTILINESTRING ", to_wkt_linestring)
}

to_wkt_multipoly <- function(x, fmt) {
  to_wkt_multi(x, fmt, "MULTIPOLYGON ", to_wkt_poly)
}
