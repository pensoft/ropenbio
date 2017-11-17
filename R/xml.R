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
    # will return a triples object (TODO S3 or S4??)
    triples = tryCatch(TaxonomicArticle_extractor(xml) ,
                       error = function(e) {
                         log_event(eventName = "Extractor Failure", callingFunction = "TaxonomicArticle_extractor")
                         NULL
                       })

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






#' Convert an XML File to Plain Text NaCTeM Format
#'
#' Possible side effect: file gets modified on disk!
#'
#' @param xml_filename the filename (one file) to convert to NaCTeM plain text
#' @param xml_schema a list with mark-up information for taxonomic names in the XML schema
#' @param modify_xml if TRUE, the function has a side-effect and modifies the XML on disk
#'
#' @return plain text to write as NaCTeM format
#' @export
xml2nactem = function(xml_filename, xml_schema, modify_xml = FALSE) {
  xml_doc = xml2::read_xml(xml_filename)
  regnames = regularize_taxonomic_names(xml_doc, xml_schema)
  underscored_names = underscore_taxonomic_names(xml_doc, xml_schema)
  plain_text = remove_all_tags(xml_doc)
  if (modify_xml == TRUE) {
    xml2::write_xml(xml_doc, xml_filename)
  }
  return(list(names = underscored_names, text = plain_text))
}


#' Regularize Taxonomic Names in an XML Document
#'
#' Side-effect: modifies the XML object by regularizing taxonimic names!
#'
#' @param xmldoc the taxonomic article as a XML2 document
#' @param schema a list with mark-up information for taxonomic names in the XML schema
#'
#' @return a vector of regularized name parts
#' @export
regularize_taxonomic_names = function(xmldoc, schema) {
  sapply(xml_find_all(xmldoc, xpath = paste0("//", schema$PartialTaxonomicNameUsage$markup)), function(ptnu) {
    regtext = xml_text(xml_find_first(ptnu, xpath = paste0("./", schema$RegularizedPartialTaxonomicNameUsage$markup)))
    if (!is.na(regtext)) {
      xml2::xml_text(ptnu) = regtext
    }
    return(xml2::xml_text(ptnu))
  })
}


#' Underscore Taxonimic Names in an XML Document Object
#'
#' Side-effect: modifies the XML object by underscoring taxonimic names
#'
#' @param xmldoc an XML2 object in which to underscore the names
#' @param schema the properties of the XML schema as a list
#'
#' @return a vector of underscored names
#' @export
underscore_taxonomic_names = function(xmldoc, schema) {
  sapply(xml_find_all(xmldoc, xpath = paste0("//", schema$TaxonomicNameUsage$markup)), function(tnu) {
    # text nodes including partent in children, in their order
    uname = do.call(pasteconstr("_"), xml_find_all(tnu, xpath = ".//text()[normalize-space()]"))
    # do it again if some node contained a space in it
    uname = do.call(pasteconstr("_"), as.list(unlist(strsplit(uname, " "))))
    xml_remove(xml_children(tnu))
    xml_text(tnu) = uname
    return(uname)
  })
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
