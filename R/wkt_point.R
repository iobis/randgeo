#' Random WKT point
#'
#' @export
#' @param count (integer/numeric) number of points. Default: 1
#' @param bbox (integer/numeric) lat/long bounding box from which to generate
#'   positions; numeric vector of the form \code{west (long), south (lat), east
#'   (long), north (lat)}. optional
#' @param fmt (integer/numeric) number of digits. Default: 7
#' @param as_multi (logical) if \code{TRUE} then a MULTIPOINT is returned, else
#'   a character vector of POINT strings is returned. Default: FALSE.
#' @return WKT; a character vector with one ore more POINT strings or a
#'   MULTIPOINT string if as_multi is \code{TRUE}
#' @examples
#' wkt_point()
#' wkt_point(10)
#' wkt_point(100)
#'
#' wkt_point(fmt = 5)
#' wkt_point(fmt = 6)
#' wkt_point(fmt = 7)
#'
#' wkt_point(bbox = c(50, 50, 60, 60))
#' wkt_point(3, as_multi = TRUE)
wkt_point <- function(count = 1, bbox = NULL, fmt = 7, as_multi = FALSE) {
  assert(fmt, c('numeric', 'integer'))

  res <- geo_point(count, bbox)
  if (as_multi) {
    to_wkt_multipoint(
      lapply(res$features, function(z) z$geometry$coordinates),
      fmt)
  } else {
    unlist(
      lapply(res$features, function(z) to_wkt_pt(z$geometry$coordinates, fmt))
    )
  }
}
