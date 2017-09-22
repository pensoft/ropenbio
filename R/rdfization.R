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
  properties = setdiff( names( x ), c( "this", "id", "parent_id", "node" ) )

  rdf = lapply( properties, function( p ) {
    lapply( x[[p]], function( value ) {

      if (has_meaningful_value(obkms$properties[[p]]$type) && obkms$properties[[p]]$type == "object") {
        triple2(qname(x$id), qname(obkms$properties[[p]]$uri), value)
      }
      else if(has_meaningful_value(obkms$properties[[p]]$type)) {
        triple2(qname(x$id), qname(obkms$properties[[p]]$uri), squote(value, literal_type = obkms$properties[[p]]$type))
      }
      else {
        # just a string
        # TODO add language processing
        triple2(qname(x$id), qname(obkms$properties[[p]]$uri), squote(value))
      }
    })
  })

  #rdf$containment = list(
  #  triple2( qname( document_component$parent_id), qname( obkms$properties$contains$uri ), qname( document_component$id) )
  #)

  return( unlist( rdf, recursive = FALSE) )
}



