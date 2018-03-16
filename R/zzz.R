
#' @import BBmisc
#' @import data.table
#' @import checkmate

#' @title Test title
#'
#' @export
harbinger = TRUE

#' Test Function
#'
#' The identity function.
#'
#' @param x \[`any`]\cr
#'   Anything, really.
#' @param y \[\code{\link{harbinger}}]\cr
#'   Ignored.
#' @return \[`any`] The `x` input.
#' @export
test = function(x, y) {
  x
}

#' Select the first or second row of a given `data.frame`
#'
#' Test function.
#'
#' @param a \[`logical(1)`]\cr
#'   Whether to select the first or second row of `b`. If this is `TRUE`, the
#'   first row is selected, otherwise the second row.
#' @param b \[\code{\link{data.frame}}]\cr
#'   `data.frame` to extract the row from. Must have two rows.
selectFirst = function(a = .: logical(1), b = .: data.frame [[nrow(b) == 2]]) {
  b[2 - as.numeric(a), ]
}


compileTypes()  # compile functions.
