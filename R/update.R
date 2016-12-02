#' Perform update described in a SPARQL 1.1 Update string
#' Based on http://docs.rdf4j.org/rest-api/#_perform_update_described_in_a_sparql_1_1_update_string
#' @param server_access_options \emph{list} of rdf4j server access options
#' @param repository \emph{character} id of the repository to be updated
#' @param update \emph{character} the update string
#' @return success status of the operation
#' @export
update_repository = function( server_access_options, repository, update ) {
#  curl -u obkms:1obkms -X POST --header "Accept:application/x-www-form-urlencoded" --data   "update=DROP GRAPH <http://id.pensoft.net/0787216b-e737-4260-9258-a1e9f659c3e8>" http://213.191.204.69:7777/graphdb/repositories/OBKMS/statements
  endpoint = paste(
    server_access_options$server_url, "/repositories/", repository, "/statements",
                                                                      sep = "")

  content_type = "application/x-www-form-urlencoded"
  if ( server_access_options$authentication == "basic_http") {
    user = strsplit( server_access_options$userpwd, ":" )[[1]][1]
    password = strsplit( server_access_options$userpwd, ":" )[[1]][2]
    r = httr::POST(
      url = endpoint,httr::content_type(content_type),
                httr::authenticate(user, password, type = "basic"), body = paste0("update=", update ) )
  }
  else {
    # API authentication: do nothing, header already contains what is needed
    # hopefully
    # TODO test
    r= httr::POST(
      url = endpoint, httr::content_type(content_type), body = paste0("update=", update ) )
  }
  return ( r )
}
