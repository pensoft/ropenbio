#' Keyword Extractor
#'
#' @param XML an XML object containing a keyword group
#'
#' @param metadata a Taxonomic Article object containing the keyword group
#'
#' @param XPath atom locations of a keyword group
#'
#' @param keyword_scheme keyword group classifiaction scheme
#'
#' @return triples pertaining to the keyword
#'
#' @export
#'
keyword_extractor = function( XML,
                             metadata,
                             XPath = yaml::yaml.load_file( obkms$config$keywords_db_xpath ),
                             keyword_scheme = obkms$parameters$Vocabularies$Subject_Classification_Terms)
{
  keyword_group = as.list ( find_literals( XML$node, XPath ) )
  class(keyword_group) = "Keyword"

  language = list( semantic_code = keyword_group$language )
  if ( is.na ( keyword_group$language ) ) { language =  obkms$parameters$Language$English }

  triples = list()

  return( unlist(lapply(  keyword_group$keyword, function( keyword  )  {

    dbpedia_id = qname ( dbpedia_lookup( keyword, language = language ) )
    subject_term_id = qname( lookup_id ( keyword, resource_type = obkms$classes$Subject_Term,
                                         language = language, in_scheme = keyword_scheme) )

    triples = list (
      triple2( subject_term_id, qname( obkms$properties$type$uri ), qname( obkms$classes$Subject_Term$uri  ) ),
      triple2( subject_term_id, qname( obkms$properties$label$uri ), squote( keyword, language = language ) ) ,
      triple2( subject_term_id, qname( obkms$properties$belongs_to_scheme$uri ), qname ( keyword_scheme$uri ) ),
      triple2( subject_term_id, qname ( obkms$properties$exact_match$uri), dbpedia_id ) ,

      triple2( metadata$paper_id, qname ( obkms$properties$keywords$uri ), squote ( keyword, language = language ) ),
      triple2( metadata$paper_id, qname ( obkms$properties$subject_term$uri ), subject_term_id  ) )

    return ( triples )
  }), recursive = FALSE) )
}

