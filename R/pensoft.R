#' Pensoft Links for Images of a Given Pensoft Paper
#'
#' Should work with any Taxpub XML!
#'
#' @param articles character(n) of links to article XML's
#'
#' @return character(n) of links to images contained in the article
#'
#' @examples
#' pensoft_images("/media/obkms/pensoft-corpus.xml/10.3897_BDJ.2.e4168.xml")
#'
#' @export
pensoft_images = function(articles) {
  unique(unlist(lapply(articles, function(t)
    {
      lapply(xml2::xml_find_all(tryCatch(xml2::read_xml(t), error = function(e)
      {
        warning(t)
        return(xml2::read_xml("<xml>Error</xml>"))
      }), "//fig/@id"), xml2::xml_text)
    }),
    recursive = TRUE))
}


#' Pensoft Article E-Location
#'
#' Should work with any Taxpub XML!
#'
#' @param articles character(n) of links to article XML's
#'
#' @return character(n) eloction
#'
#' @examples
#' pensoft_article_eloc("/media/obkms/pensoft-corpus.xml/10.3897_BDJ.2.e4168.xml")
#'
#' @export
pensoft_article_eloc = function(articles) {
  unique(unlist(lapply(articles, function(t)
  {
    lapply(xml2::xml_find_all(tryCatch(xml2::read_xml(t), error = function(e)
    {
      warning(t)
      return(xml2::read_xml("<xml>Error</xml>"))
    }), "//elocation-id"), xml2::xml_text)
  }),
  recursive = TRUE))
}
