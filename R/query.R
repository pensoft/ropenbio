#     ____    _    _   ______   _____   __     __
#    / __ \  | |  | | |  ____| |  __ \  \ \   / /
#   | |  | | | |  | | | |__    | |__) |  \ \_/ /
#   | |  | | | |  | | |  __|   |  _  /    \   /
#   | |__| | | |__| | | |____  | | \ \     | |
#    \___\_\  \____/  |______| |_|  \_\    |_|
#

#' Use the GET protocol to execute a graph database query
#'
#' This function submits a query to a previously configured graph database
#' (configuration is speciefied in the \code{options} parameter) using the GET
#' protocol. The query string (in SPARQL - tested, or SERQL - not yet tested) as
#' well as the return format of the results are passed as parameters.
#'
#' @note This function has currently only been tested with the S4 semantic suite of
#'   Ontotext, as there seems to be some bug with the standalone GraphDB
#'   installation (in GraphDB, not in the package!).
#'
#' @param options           a list returned by \code{create_server_options}.
#' @param repo_id           a string holding the repository id as returned by \code{get_repositories}.
#' @param results_format    a string, indicating the type of results required. Currently supported: "XML", "CSV".
#'                            If "XML" is specified, the return value is an XML string, ready to be parsed as a document.
#'                            If "CSV" is specified, the return variable is already parsed as a data.frame.
#' @param infer             a boolean, indicating whether we want to use inference.
#' @param query_language    a string: "SPARQL" or "SERQL" (untested)
#' @param varbindings       currently unused
#' @param timeout           currently unused
#'
#' @examples
#' \dontrun{r = GET_query( options, "plazi", query, "CSV" )}
#' \dontrun{r = GET_query( options, "plazi", query, "XML" )}
#' @export

GET_query = function ( options, repo_id, query, results_format = "CSV",  query_ln = "sparql", infer = TRUE, varbindings = "", timeout = "" ) {
  if ( results_format == "CSV" ) {
    header = c(Accept = "text/csv, */*;q=0.5")
  }
  else { #XML case
    header = c(Accept = "application/sparql-results+xml, */*;q=0.5")
  }
  endpoint = paste( options$server_url, "/repositories/", repo_id, sep = "")
  if ( options$authentication == "basic_http") {
    r = RCurl::getURL( paste( endpoint, "?query=", URLencode(query), "&queryLn=", query_ln, "&infer=", as.character( infer ), sep = "") , verbose = TRUE, httpheader = header, httpauth = 1L, userpwd = options$userpwd)
  }
  else { #API case
    r = RCurl::getURL( paste( endpoint, "?query=", URLencode(query), "&queryLn=", query_ln, "&infer=", as.character( infer ), sep = "") , verbose = FALSE, httpheader = header)
  }
  if ( results_format == "CSV") {
    data = read.csv( textConnection( r ) )
    return( data )
  }
  else
    { #XML case
    return ( r )
    }
}

#' Use the POST protocol to execute a graph database query
#'
#' This function is very similar to \code{GET_query}; the difference is that
#' while \code{GET_query} submits the SPARQL/SERQL query to the database endpoint
#' via the GET protocol, \code{POST_query} submits it via the POST protocol.
#'
#' @note This function was tested both on GraphDB standalone and on Ontotext S4.
#'
#' @param options           a list returned by \code{create_server_options}.
#' @param repo_id           a string holding the repository id as returned by \code{get_repositories}.
#' @param results_format    a string, indicating the type of results required. Currently supported: "XML", "CSV".

#' @param infer             a boolean, indicating whether we want to use inference.
#' @param query_language    a string: "SPARQL" or "SERQL" (untested)
#' @param varbindings       currently unused
#' @param timeout           currently unused
#'
#' @return                  If \code{results_format} is "XML", the return value is an XML string containing the output of the , ready to be parsed as a document.
#'                            If "CSV" is specified, the return variable is already parsed as a data.frame.
#'
#' @examples
#' \dontrun{r = POST_query( options, "plazi", query, "CSV" )}
#' \dontrun{r = POST_query( options, "plazi", query, "XML" )}
#' @export

POST_query = function ( options , repo_id, query, results_format = "CSV", query_ln = "SPARQL", infer = TRUE, varbindings, timeout = "" ) {
  # need to construct a POST query, here is how to do it with curl
  # curl -u obkms:1obkms -X POST --header "Accept:application/sparql-results+xml" --data   "query=SELECT%20(COUNT(*)%20as%20?count)%0AFROM%20%3Chttp://www.ontotext.com/implicit%3E%0AWHERE%20%7B%0A%20%20%20?s%20?p%20?o%20.%0A%7D" http://213.191.204.69:7777/graphdb/repositories/OBKMS
  # many issues here, see http://stackoverflow.com/questions/5797688/post-request-using-rcurl ,  more specifically comments by Duncan

  endpoint = paste( options$server_url, "/repositories/", repo_id, sep = "")
  if ( results_format == "CSV" ) {
    header = c(Accept = "text/csv, */*;q=0.5")
  }
  else { #XML case
    header = c(Accept = "application/sparql-results+xml, */*;q=0.5")
  }

  curl = RCurl::getCurlHandle()
  if ( options$authentication == "basic_http") {
    RCurl::curlSetOpt( .opts = list(httpheader = header, verbose = FALSE), userpwd = options$userpwd, httpauth = 1L, curl = curl)
  }
  else { # API authentication
    RCurl::curlSetOpt( .opts = list(httpheader = header, verbose = FALSE), curl = curl)
  }

  r = RCurl::postForm( endpoint, query = query, curl = curl,  style = "POST")
  if ( results_format == "CSV") {
    data = read.csv( textConnection( r ) )
    return ( data )
  }
  else { #XML case
    return ( r )
    }
}

