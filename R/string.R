#' Last Token of Character Vector
#'
#' Calls `strsplit` and then returns the last token.
#'
#' @param x a character vector to extract last token from
#' @param split string or regex on which to do the split
#' @param ... further arguments to strsplit
#'
#' @return last token of the character vector
#'
#' @examples
#' last_token("http://tb.plazi.org/GgServer/xml/uno", "/")
#' last_token(c("http://tb.plazi.org/GgServer/xml/dos", "http://tb.plazi.org/GgServer/xml/tres"), "/")
#'
#' @export
last_token = function(x, split, ...) {
  intermediate = strsplit(x, split)
  sapply(intermediate, last)
}

#' Last Element of Vector
#'
#' @x a vector (not a list)
#'
#' @return the last element of the vector
#'
#' @examples
#' x = c(1, 2, 3)
#' y = c("a", "b", "c")
#' last(x)
#' last(y)
#'
#' @export
last = function(x) {
  return(x[length(x)])
}
