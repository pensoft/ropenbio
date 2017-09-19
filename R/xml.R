#' Convert an XML Document to RDF
#'
#' The function ought to work on different types of XML documents. It tackes
#' care of error checking and calling the correct top-level extractor.
#'
#' This function is exported and can be called from the user.
#'
#' @author Viktor Senderov
#' @param resource_locator
#'        locator, e.g. file path or URL, of the XML resource
#' @param locator_type
#'        type of the locator; one of c("FILE", "URL"); default is "FILE"
#' @param resource_format
#'        XML schema of the XML resource; one of c( "TAXPUB", "PLAZI" );
#'        default is "TAXPUB"
#' @param serialization_format
#'        output serialization format for the RDF; one of c( "TURTLE" );
#'        default is "TURTLE"
#'
#' @export
xml2rdf = function( resource_locator,
                    resource_type = "FILE",
                    resource_format = "TAXPUB",
                    serialization_format = "Turtle")
{
  # checks, if not FILE, cannot write back
  stopifnot ( is.character(resource_type), resource_type == "FILE" )
  stopifnot ( is.character(resource_format), resource_format %in%
                c( "TAXPUB", "REFBANK_XML" ) )
  stopifnot (
    is.character(serialization_format), serialization_format == "Turtle" )

  # load the XML document
  xml = xml2::read_xml( resource_locator , options = c()) # both a document and a node
  # fetch or set the id for the XML document
  context = qname(  get_or_set_obkms_id( xml , fullname = TRUE ) )

  # Call the top-level extractors depending on the type
  if ( resource_format == "TAXPUB" ) {
    triples = TaxonomicArticle_extractor( xml ) # will return a triples object (TODO S3 or S4??)
    # serialize
    serialization = c()
    if (serialization_format == "Turtle") {
      serialization = turtle_prepend_prefixes()
      serialization = c ( serialization, triples2turtle2 ( context, triples ), "\n\n" )
      #serialization = c ( serialization, triples2turtle2 ( context, triples$article ), "\n\n" )
      # serialization = c( serialization, triples2turtle2( "pensoft:Nomenclature", triples$nomenclature))
    }
    # context need to be cleared
    clear_context( context )
    # return the return string as a string
    xml2::write_xml( xml, resource_locator )
  }
  return ( do.call(paste, as.list( serialization )))
}






#' Finds the atoms in an xml node
#'
#'
#'
#'
#'
#' @param xml the XML node
#' @param x xpath expressions
#' @param quoted if TRUE, will run squote with needed params on the arguments
#'
#' @return a vector of atoms
#'
#' @export
#'
find_literals = function( xml, x, quoted = FALSE  ) {
  r =
    sapply( names(x), function( l ) {
      ns = xml2::xml_find_all( xml, x[[l]] )
      # ns is a list
      if ( length( ns ) == 0) return ( NA )
      else {
        if (quoted) {
          return( paste0("\"", trim_label ( xml2::xml_text( ns , trim = TRUE) ) , "\"") )
        }
        else {
          return ( trim_label ( xml2::xml_text( ns , trim = TRUE) ) )
        }

      }
    })
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





#' Get the Root (Article) OBKMS Id for an XML Node
#'
#' Does not do any database lookups - only looks at XML
#'
#' @param node
#' @param fullname
#'
#' @export
root_id = function(node, fullname = FALSE)
{
  obkms_id = xml2::xml_text( xml2::xml_find_all(node, "/article/@obkms_id"))

  if (fullname) {
    return (paste0( strip_angle( obkms$prefixes$`_base`) , obkms_id))
  }

  else return (obkms_id)
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









