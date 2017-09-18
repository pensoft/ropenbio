#' Convert an Entity to RDF
#'
#' @param x
#'
#' Converts the atoms to appropriate triples. and connects to parent ID via
#' po:contains.
#'
#' @param x an entity to convert to RDF
#'
#' @return RDF
#' @export
as.rdf = function( x ) {
  properties = setdiff( names( x ), c( "this", "id", "parent_id" ) )

  rdf = lapply( properties, function( p ) {
    lapply( x[[p]], function( value ) {
      triple2( qname (x$id), qname( obkms$properties[[p]]$uri ), value )
    })
  })

  #rdf$containment = list(
  #  triple2( qname( document_component$parent_id), qname( obkms$properties$contains$uri ), qname( document_component$id) )
  #)

  return( unlist( rdf, recursive = FALSE) )
}



