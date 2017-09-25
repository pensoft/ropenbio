#' Maps Verbatim Status String to a Vocabulary of Taxonomic Ranks
#'
#' Constructor for taxonomic status
#'
#' @param verbatim_status (character) the status as a free-form string
#'
#' @return a vocabulary item (taxonomic status) corresponding to the rank encoded
#' @export

taxonomic_status = function ( verbatim_status ) {
  # normalize the verbatim_rank
  verbatim_status = trimws( tolower( verbatim_status ) )

  status_check = as.logical(  lapply( obkms$vocabulary$status, function (status ) {

  sum (unlist(lapply( status, function(s) {
      grepl(s, verbatim_status)
    })))
  }) )

  if ( sum( status_check ) > 0 ) {
    ix = which(  status_check )

    status = names( obkms$vocabulary$status)[ix]

    status = strip_angle( add_prefix( status ), reverse = TRUE)

    class(status) = c( class(status), "TaxonomicStatus", "VocabularyItem" )
  }
  else status = NA

  return(status)
}
