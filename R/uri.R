#' Minimizes a URI to a Qname.
#'
#' E.g. qname("http://openbiodiv.net/leis-papuensis") minimizes to
#' ":leis-papuensis". The special case here is the `_base` prefixes, as
#' it minimizes to nothing.
#'
#' if the prefix is _base, it is shortened to :
#'
#' @param uri        the full URI to minimize
#' @export
qname = function( uri )
{
  if ( missing (uri) || is.null( uri ) ) return ( NA )

  unlist ( sapply( uri, function( uri)  {

    if ( is.na( uri ) ) return( NA )

    if ( grepl( "^#" , uri )  ) {
      log_event( "URI will be forced to null", "qname", uri )
      return ( NULL ) # clean-up dbpedia shit
    }
    if ( grepl( ",", uri )) {
      # if the thing has a comma, it cannot be shortened, as it will
      # break turtle, so just put angle brackets
      return ( strip_angle( uri, reverse = TRUE ) )
    }
    if ( missing (uri) || is.null( uri ) || uri == "" ) return (NULL)
    stopifnot( exists( 'obkms', mode = 'environment' ))
    # strip brackets from the uri and from the OBKMS databases of prefixes
    uri = strip_angle ( uri )
    stripped_prefixes = sapply ( obkms$prefixes, strip_angle )

    # try each of the prefixes, r is logical vector of where the beginning of the
    # uri matches any of the prefix
    r = sapply( stripped_prefixes , function ( p ) {
      grepl( paste0( "^",  p  ) , uri )
    } )
    # if found, replace the beginning of the uri with the prefix
    if ( sum( r ) > 0) {
      p = stripped_prefixes[r]

      n = names(stripped_prefixes)[r]
      if ( names(p) == "_base")  # special case of the "_base"
      {
        uri = gsub( paste0("^", p), ":" , uri )
      }
      else {
        uri = gsub( paste0("^", p), paste0( n, ":" ), uri )
      }
    }
    return (uri)
  }) )

}





#' Adds the openBiodiv prefix to a string to create a proper URI
#'
#' @param string (character) to be pasted to
#'
#' @return URI
#' @export

add_prefix = function( string ) {
  uri = paste0( strip_angle( obkms$prefixes$'_base' ) , string )
  class(uri) = "URI"
  return( uri )
}





#' @export
has_meaningful_value = function ( literal ) {
  if ( !is.null( literal ) ) {
    if ( !is.na( literal ) ) {
      if ( literal != "" ) {
        return( TRUE )
      }
    }
  }
  return( FALSE )
}
