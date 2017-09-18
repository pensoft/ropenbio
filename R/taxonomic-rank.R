#' Maps Verbatim Rank String to a Vocabulary of Taxonomic Ranks
#'
#' Constructor for taxonomic rank
#'
#' @param verbatim_rank (character) the rank as a free-form string
#'
#' @return a vocabulary item (`TaxonomicRank`) corresponding to the rank encoded
#' @export

taxonomic_rank = function ( verbatim_rank ) {
  # normalize the verbatim_rank

  verbatim_rank = gsub("[[:punct:]]", " ", verbatim_rank)
  verbatim_rank = trimws( tolower( verbatim_rank ) )

  ix = integer(0)
  i = 1
  while( length(ix) == 0 && i <= 4) {
    ix = grep( paste0("^" ,verbatim_rank, "$"), obkms$vocabulary$rank[[i]])
    i = i + 1
  }

  if ( length(ix) > 0 ) {
    rank = strip_angle( add_prefix( obkms$vocabulary$rank[ix, 1] ), reverse = TRUE)

    class(rank) = c( class(rank), "TaxonomicRank", "VocabularyItem" )
  }
  else rank = verbatim_rank

  return(rank)
}
