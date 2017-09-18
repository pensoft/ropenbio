#' Constructor for Treatment
#'
#' @param node XML2 node containing a node.
#'
#' @return a Treatment object
#' @export
Treatment = function(node) {
  this = DocumentComponent(node, obkms$xpath$taxpub$Treatment, "Treatment")
  return(this)
}
