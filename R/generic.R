#' A Generic Object Constructor
#'
#'
#'
#'
#'
#' A wrapper to \code{find_literals} that can be used to create all kinds
#' of objects. It also sets the contents of node. It's better to use
#' environments for objects than lists for objects because objects are
#' key-value pairs (probably there are cases where not true).
#'
#' @seealso find_literals
#'
#' @param xml the XML node that will be converted to an object
#' @param atoms a vector of XPATH where the objects will be sought after
#' @param id you can pass an ID for the object; default is NA
#' @param parent_id the id of the parent object (if present)
#' @param obj_class the class that will be assigned to the object
#'
#' @return an object of class \code{obj_class}
#'
#' @export
#'

generic_xml_constructor = function( xml, atoms, id, parent_id, obj_class )
{
  if ( !missing( atoms ) ) {
    object = as.environment ( find_literals( xml, atoms ) )
  }
  else {
    object = new.env()
  }

  if ( !missing( id) && !is.na( id ) && is.character( id ) && length ( id ) > 0 ) {
    object$id = id
  }
  else {
    object$id = NA
  }

  if ( !missing( parent_id ) && !is.na( parent_id ) && is.character( parent_id ) && length ( parent_id ) > 0 ) {
    object$parent_id = parent_id
  }
  else {
    object$parent_id = NA
  }

  object$language = list( semantic_code = xml2::xml_text( xml2::xml_find_first( x = xml, "@xml:lang") ) )
  object$content = xml2::xml_text( xml )

  class( object ) = obj_class

  return( object )
}
