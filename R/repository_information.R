                      #' REPOSITORY INFORMATION

#' Test connectivity to the graph database and get the communication protocol
#' version.
#'
#' This function tests the connectivity to the graph database. If there is
#' connectivity it will return the protocol version as a numeric.
#' If there is no connectivity, an error condition will be set.
#'
#' @param options   a list, containing the graph database connectivity options,
#'                returned by the helper function \code{create_server_options}.
#'
#' @return    a numeric, containing the protocol version if connectivity is OK.
#'
#' @examples
#'
#' \dontrun{ get_protocol_version( options ) }
#'
#' @export

get_protocol_version = function( options  ) {

  if ( options$authentication == "basic_http" ) {

    request_url = paste( options$server_url, "/protocol", sep = "" )
    username = strsplit( options$userpwd, ":" )[[1]][1]
    password = strsplit( options$userpwd, ":" )[[1]][2]
    r = httr::GET( request_url , httr::authenticate( username, password, "basic"))
    response = httr::content( r, as = "text" )
    return ( httr::content (r ) )
    #RCurl::getURL( paste( options$server_url, "/protocol", sep = "" ), verbose = FALSE, userpwd = options$userpwd, httpauth = 1L)

  } else { # API-KEY Authentication or No authentication

    r = httr::GET( paste( options$server_url, "/protocol", sep = "" ) )
    #RCurl::getURL( paste( options$server_url, "/protocol", sep = "" ), verbose = FALSE )
  }

  # match if the response is constructed of a number (more than one digit and
  # nothing else ):
  response = httr::content( r, as = "text" )
  stopifnot( grepl( "^\\d+^", response ) )
  return ( as.numeric( response ) )
}

# Gets the list of repositories

# Parameters:
# options        should be an object returned by create_server_options
#' @export
#'
get_repositories = function( options ) {
  endpoint = "/repositories"
  header = c(Accept = "application/sparql-results+xml, */*;q=0.5")
  if ( options$authentication == "basic_http" ) {
    r = RCurl::getURL( paste( options$server_url, endpoint, sep = ""), verbose = FALSE, userpwd = options$userpwd, httpauth = 1L, httpheader = header)

  }
  else { #API-case
    r = RCurl::getURL( paste( options$server_url, endpoint, sep = ""), verbose = FALSE, httpheader = header)
  }
  cols = c("uri", "id", "title", "readable", "writable")
  d = XML::xmlTreeParse(r, asText = TRUE, useInternalNodes = TRUE)

  ns = XML::getNodeSet(d, "//x:results/x:result/x:binding[@name = 'uri']/x:uri", "x")
  uri = sapply (ns, XML::xmlValue)

  ns = XML::getNodeSet(d, "//x:results/x:result/x:binding[@name = 'id']/x:literal", "x")
  id = sapply (ns, XML::xmlValue)

  ns = XML::getNodeSet(d, "//x:results/x:result/x:binding[@name = 'title']/x:literal", "x")
  title = sapply (ns, XML::xmlValue)

  ns = XML::getNodeSet(d, "//x:results/x:result/x:binding[@name = 'title']/x:literal", "x")
  title = sapply (ns, XML::xmlValue)

  ns = XML::getNodeSet(d, "//x:results/x:result/x:binding[@name = 'readable']/x:literal", "x")
  readable = sapply (ns, XML::xmlValue)

  ns = XML::getNodeSet(d, "//x:results/x:result/x:binding[@name = 'writable']/x:literal", "x")
  writable = sapply (ns, XML::xmlValue)

  data.frame(uri, id, title, readable, writable)
}
