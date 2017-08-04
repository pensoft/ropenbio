

#' Front Matter Extractor
#'
#' Extracts the front matter of an article.
#'
#' @param front_matter_comp a component (list of an XML2 node and an id
#'  vector) containing the front matter of the article
#' @param title_comp a component containing the title of the article
#' @param abstract_comp a component containing the abstract of the article
#'
#' @param metadata a journal article object
#'
#' @return triples
#'
front_matter_extractor = function ( front_matter_comp,
                                    title_comp,
                                    abstract_comp,
                                    metadata )
{
  front_matter =  generic_xml_constructor( xml = front_matter_comp$xml,
                                           id = front_matter_comp$id,
                                           obj_class = "front_matter")

  title        = generic_xml_constructor( xml = title_comp$xml ,
                                          id = title_comp$id,
                                          obj_class = "title" )

  abstract     = generic_xml_constructor( xml = abstract_comp$xml,
                                          id = abstract_comp$id,
                                          obj_class = "abstract")
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
