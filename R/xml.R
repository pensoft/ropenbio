



#' Finds the atoms in an xml node
#'
#'
#'
#'
#'
#' @param xml the XML node
#' @param x xpath expressions
#'
#' @return a vector of atoms
#'
#' @export
#'
find_literals = function( xml, x  ) {
  r =
    sapply( names(x), function( l ) {
      ns = xml2::xml_find_all( xml, x[[l]] )
      # ns is a list
      if ( length( ns ) == 0) return ( NA )
      else return ( trim_label ( xml2::xml_text( ns , trim = TRUE) ) )
    })
}








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














#' Generic Document Component Constructor
#'
#'
#' Creates a document component of specified class given a list of XPATH
#' atom locations. Id and parent ID are extracted from the XML.
#'
#'
#' @param node the XML node that will be converted to an object
#' @param atom_location a vector of XPATH where the objects will be sought after
#' @param component_class the class that will be assigned to the object
#'
#' @return an object of class \code{component_class}
#'
#' @export
#'
generic_document_component = function( node, atom_location, component_class )
{
  atom = find_literals( node, atom_location )
  object = vector( mode = "list", length = length( atom_location ) + 2 )
  object[ 1:length( atom_location) ] = as.list ( atom )
  names( object ) = c(names( atom ), "id", "parent_id" )

  object$id = get_or_set_obkms_id( node, fullname = TRUE)
  object$parent_id = parent_id( node, fullname = TRUE)

  class( object ) = component_class

  return( object )
}














#' Get the OBKMS Id of an XML node, if not set, set it
#'
#' Does not do any database lookups. Is exported.
#'
#' @param node the XML node from which to take the ID; cannot be missing!
#' @param fullname if TRUE, returns a URI with the OBKMS base prefix
#'
#' @export
get_or_set_obkms_id = function ( node, fullname = FALSE )
{

  if ( is.na( xml2::xml_attr( node, "obkms_id" ) ) )
  {
    xml2::xml_attr( node, "obkms_id" ) = uuid::UUIDgenerate()
  }
  obkms_id = xml2::xml_attr( node, "obkms_id" )


  if ( fullname )
  {
    return (  paste0( strip_angle( obkms$prefixes$`_base`) , obkms_id ) )
  }

  else return ( obkms_id )
}


#' Get the Parent OBKS Id for an XML Node
#'
#' Does not do any database lookups. Recursive looks for the parent node until
#' it finds one with ID.
#'
#' @param node the XML node from which to take the ID; cannot be missing!
#' @param fullname if TRUE, returns a URI with the OBKMS base prefix
#'
#' @export
parent_id = function ( node, document, fullname = FALSE )
{
  obkms_id = NA
  path = xml2::xml_path( node )
  # will repeat while we don't have an id and we aren't at the top
  while ( path != "/" && is.na( obkms_id ) ) {
    node = xml2::xml_parent( node )
    obkms_id = xml2::xml_attr( node, "obkms_id")
    path = xml2::xml_path( node )
  }

  if ( fullname )
  {
    return (  paste0( strip_angle( obkms$prefixes$`_base`) , obkms_id ) )
  }

  else return ( obkms_id )
}




#' Break Down a XML object into its Components
#'
#' Takes a top-level XML object (e.g. a paper) and a vector of XPATH locations,
#' indicating where its subcomponents are.
#' This function returns a list of XML objects that are found at the XPATH
#' locations (that can later be processed by lower-level extractors) with their
#' IDs.
#'
#' @param xml the xml resource
#' @param document_component_xpath datapase of xpaths where the document components
#' are found in the corresponding XML schema
#' @return a list of XML nodes and ID's corresponding to each of the elements
#'   in the `document_component_xpath` vector
#'
document_components = function ( xml, document_component_xpath ) {
  doco = list()
  for ( name in names( document_component_xpath ) ) {
    xml_nodeset = xml2::xml_find_all( xml, document_component_xpath[[ name ]] )
    doco[[name]] = list()
    j = 1
    for ( xml_node in xml_nodeset ) {
      obkms_id = get_or_set_obkms_id( xml_node, fullname = TRUE )
      doco[[name]][[j]] = list( id = obkms_id, xml = xml_node )
      j = j + 1
    }
  }
  # after the first pass is done and id's have been assigned, need to look for
  # parent id's
  for ( name in names( document_component_xpath ) ) {
    xml_nodeset = xml2::xml_find_all( xml, document_component_xpath[[ name ]] )
    j = 1
    for ( xml_node in xml_nodeset ) {
      parent_id = parent_id( xml_node, xml )
      doco[[name]][[j]]$parent_id = parent_id
      j = j + 1
    }
  }
  return ( doco )
}

