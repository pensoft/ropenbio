#' RDF-ization of a Single XML File
#'
#' @param filename locator, e.g. file path or URL, of the XML resource
#' @param xml_schema
#'        XML schema of the XML resource; one of
#'        taxpub taxonx or plazi_int
#' @param access_options endpoint to database to which to submit
#' @param serialization_dir directory where to dump intermediate serializations
#' @param serialization_format default is "turtle"
#' @param reprocess a character vector of which extractors should be reprocessed
#'
#' @return logical. TRUE if everything went OK. FALSE if there was a problem
#'
#'
#' @export
xml2rdf = function(
  filename,
  xml_schema = taxonx,
  access_options,
  serialization_dir,
  reporcess = c("root_extractor")
  )
{
  tryCatch(
    {
      xml = xml2::read_xml(filename)

      triples = ResourceDescriptionFramework$new()

      root_extractor(
        xml,
        xml_schema = xml_schema,
        reprocess = reprocess,
        rdf_object = triples,
        access_options = access_options
      )

      serialization = triples$serialize()

      add_data(serialization, access_options = access_options)

      xml2::write_xml(xml, filenames)

      writeLines(
        serialization,
        con = paste0(
          serialization_dir,
          last_token(filename, split = "/")
        )
      )

      return(TRUE)
    },
    error = function(e)
    {
      warning(e)
      return(FALSE)
    })
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


#' Finds the Atoms in a XML Node
#'
#' @param xml the XML node
#' @param xpath the atom locations as a named character vector
#'
#' @return list
#'
#' @export
find_atoms =
function(xml, xpath) {
  lapply(xpath, function(p)
  {
    xml2::xml_text(xml2::xml_find_all(xml, p))
  })
}




#' Get the OBKMS Id of an XML node, if not set, set it.
#'
#' Does not do any database lookups.
#'
#' @param node the XML node from which to take the ID; cannot be missing!
#'
#' @return the local id (not an identifier object)
#'
#' @export
get_or_set_obkms_id = function (node)
{
  if (is.na(xml2::xml_attr(node, "obkms_id")))
  {
    xml2::xml_attr(node, "obkms_id") = uuid::UUIDgenerate()
  }

  xml2::xml_attr(node, "obkms_id")
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







#' Strips all XML tags
#'
#' @param xmldoc
#'
#' @return a vector of the document as plain text
#' @export
remove_all_tags = function(xmldoc) {
  sapply(xml_find_all(xmldoc, xpath = paste0("//", "p")), function(p) {
    xml_text(p) = paste(xml_text(p), "\n")
  })
  unescape_html(do.call(pasteconstr(" "), xml_find_all(xmldoc, "//text()")))
}

#' Unescpae HTML Characters
#'
#' @param str input string
#' @return string without the escape characters
#' @export
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}
