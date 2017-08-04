

#' Body Extractor
#'
#' Extracts knowledge from the body of an article
#'
#' @param body_comp a component (list of an XML2 node and an id
#'  vector) containing the body of the article
#' @param metadata a journal article object
#'
#' @return triples
#'
body_extractor = function ( body_comp,
                            metadata )
{
  body =  generic_xml_constructor( xml = body_comp$xml,
                                           id = body_comp$id,
                                           obj_class = "article_body")


}
