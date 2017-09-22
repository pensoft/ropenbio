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
  # sanity check
  if (is.null(node)) {
    return (NULL)
  }

  # Initialization
  object = as.environment(as.list(find_literals(node, atom_location, quoted = FALSE)))
  class( object ) = c(component_class, "DocumentComponent")

  # private fields
  object$node = node
  object$id = get_or_set_obkms_id( node, fullname = TRUE)
  object$parent_id = parent_id( node, fullname = TRUE)
  object$root_id = root_id(node, fullname = TRUE)
  object$language = list(semantic_code = xml2::xml_text( xml2::xml_find_first( x = node, "@xml:lang")))

  # property fields
  object$type = obkms$classes[[component_class]]$uri

  return(object)
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
