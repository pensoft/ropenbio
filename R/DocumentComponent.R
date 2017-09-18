##################################################
# FUNCTIONS THAT WORK ON CLASS DocumentComponent #
##################################################

#' DocumentComponent Constructor
#'
#'
#' This constructor creates a document component from an XML node.
#'
#' TODO Note: a document component is made up of atoms. The atoms of a component
#' may be literals as well as URI's. Currently, only literals are implemented.
#'
#' @param node the XML node that will be converted to a `DocumentComponent`
#' @param atom_location a vector of XPATH where the objects will be sought after
#' @param component_class the class that will be assigned to the object
#'
#' @return an object of class \code{component_class}
#'
#' @export
#'
DocumentComponent = function( node, atom_location, component_class )
{
  object = as.environment( as.list(find_literals( node, atom_location, quoted = TRUE ) ))

  object$id = get_or_set_obkms_id( node, fullname = TRUE)
  object$parent_id = parent_id( node, fullname = TRUE)

  object$type = strip_angle( paste0(strip_angle(obkms$prefixes$`_base`), component_class), reverse = TRUE)

  class( object ) = c(component_class, "DocumentComponent")

  return( object )
}








#' Print Method for DocumentComponent
#'
#' @param obj DocumentComponent to print
#'
#' @return
print.DocumentComponent =
  function( obj ) {
    for( n in names( obj ) ) {
      print( obj[[n]] )
    }
  }
