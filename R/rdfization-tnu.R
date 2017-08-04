

#' Taxonomic Name Usage Extractor
#'
#' Extracts knowledge from a TNU comp
#'
#' @param tnu_comp a component (list of an XML2 node and an id
#'  vector) containing the taxonomic name usage
#' @param metadata A journal article object. You need this because a TNU is part
#'   the paper as well as of the journal
#'
#' @return triples
#'
tnu_extractor = function ( tnu_comp,
                                    metadata )
{
  tnu =  tnu_constructor(node =  tnu_comp$xml,
                          id = tnu_comp$id,
                          parent_id = tnu_comp$parent_id )



  rdf = list(
    triple2( qname ( metadata$article_id ), qname( obkms$properties$contains$uri ), qname( front_matter$id ) ),

    triple2( qname( front_matter$id ), qname( obkms$properties$type$uri ), qname ( obkms$classes$FrontMatter$uri ) ),
    triple2( qname( front_matter$id ), qname( obkms$properties$contains$uri ), qname ( title$id ) ),
    triple2( qname( front_matter$id ), qname( obkms$properties$contains$uri ), qname ( abstract$id ) ),

    triple2( qname( title$id ), qname( obkms$properties$type$uri ), qname ( obkms$classes$Title$uri ) ),
    triple2( qname( title$id ), qname( obkms$properties$character_content$uri ), squote( title$content, language = title$language ) ),

    triple2( qname( abstract$id ), qname( obkms$properties$type$uri ), qname( obkms$classes$Abstract$uri ) ),
    triple2( qname( abstract$id ), qname( obkms$properties$character_content$uri ), squote( abstract$content, language = abstract$language ) )
  )
  #triple( metadata$article_id, qname( obkms$properties$contains$uri ), front_matter$id ),
  #triple( front_matter$id,     qname( obkms$properties$type$uri ),  qname ( obkms$classes$FrontMatter$uri ) ) ,
  #triple( front_matter$id,     qname( entities$contains, doco$title[[1]]$id),

  # 	triple( local$article,           entities$contains, doco$body[[1]]$id),
  # 	triple( local$article,           entities$contains, doco$back_matter[[1]]$id),
  # 	# front matter


  #     triple( doco$front_matter[[1]]$id,      entities$contains, doco$abstract[1][[1]]$id),
  #     triple( doco$front_matter[[1]]$id,      entities$first_item,
  #                                       list( triple("", entities$item_content, doco$title[[1]]$id),
  #                                             triple("", entities$next_item,
  #                                                 list( triple( "", entities$item_content, doco$front_matter[[1]]$id))))),
  #     triple( doco$title[[1]]$id,         entities$a,                      entities$doco_title),
  #     triple( doco$title[[1]]$id,         entities$has_content,            squote(literals$article_title, "@en-us"   )),
  #     triple( doco$abstract[1][[1]]$id,    entities$a,               entities$sro_abstract),
  #   ##
}












#' Specialized Constructor for Taxonomic Name Usage
#'
#'
#'
#' As TNU's have info in more than one XML node they need a specialized
#' constructor
#'
#' @seealso generic_xml_constructor
#'
#' @param node the XML node that will be converted to an object
#' @param id you can pass an ID for the object; default is NA
#' @param parent_id the id of the parent object (if present)
#'
#' @return an object of class \code{obj_class}
#'
#' @export
#'

tnu_constructor = function( node, id, parent_id )
{
  if ( !missing( atoms ) ) {
    object = as.environment ( find_literals( xml, obkms$xpath$taxpub$taxonomic_name_usage ) )
  }  else {
    object = new.env()
  }

  class( object ) = "taxonomic_name_usage"

  if ( !missing( id ) && !is.na( id ) && is.character( id ) && length ( id ) > 0 ) {
    object$id = id
  } else {
    object$id = NA
  }

  if ( !missing( parent_id ) && !is.na( parent_id ) && is.character( parent_id ) && length ( parent_id ) > 0 ) {
    object$parent_id = parent_id
  } else {
    object$parent_id = NA
  }

  object$language = NA
  object$content  = xml2::xml_text( xml )



  return( object )
}
