#   _____  ______ _____   ____   _____ _____ _______ ____  _______     __  _____ _   _ ______  ____
#  |  __ \|  ____|  __ \ / __ \ / ____|_   _|__   __/ __ \|  __ \ \   / / |_   _| \ | |  ____ / __ \
#  | |__) | |__  | |__) | |  | | (___   | |    | | | |  | | |__) \ \_/ /    | | |  \| | |__  | |  | |
#  |  _  /|  __| |  ___/| |  | |\___ \  | |    | | | |  | |  _  / \   /     | | | . ` |  __| | |  | |
#  | | \ \| |____| |    | |__| |____) |_| |_   | | | |__| | | \ \  | |     _| |_| |\  | |    | |__| |
#  |_|  \_\______|_|     \____/|_____/|_____|  |_|  \____/|_|  \_\ |_|    |_____|_| \_|_|     \____/
#


#' Get the communication protocol version
#'
#' This function queries the RDF endpoint for information about the protocol version.
#'
#' @param options           a list returned by \code{create_server_options}.
#'
#' @return a string, the protocol version.
#'
#' @examples
#' \dontrun{get_protocol_version(options)}
#'
get_protocol_version = function( options  ) {
  if ( options$authentication == "basic_http" ) {
    RCurl::getURL( paste( options$server_url, "/protocol", sep = "" ), verbose = FALSE, userpwd = options$userpwd, httpauth = 1L)
  }
  else { #API-KEY Authentication
    RCurl::getURL( paste( options$server_url, "/protocol", sep = "" ), verbose = FALSE )
  }
}

# Gets the list of repositories
# Parameters:
# options        should be an object returned by create_server_options
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
  d = xmlTreeParse(r, asText = TRUE, useInternalNodes = TRUE)

  ns = getNodeSet(d, "//x:results/x:result/x:binding[@name = 'uri']/x:uri", "x")
  uri = sapply (ns, xmlValue)

  ns = getNodeSet(d, "//x:results/x:result/x:binding[@name = 'id']/x:literal", "x")
  id = sapply (ns, xmlValue)

  ns = getNodeSet(d, "//x:results/x:result/x:binding[@name = 'title']/x:literal", "x")
  title = sapply (ns, xmlValue)

  ns = getNodeSet(d, "//x:results/x:result/x:binding[@name = 'title']/x:literal", "x")
  title = sapply (ns, xmlValue)

  ns = getNodeSet(d, "//x:results/x:result/x:binding[@name = 'readable']/x:literal", "x")
  readable = sapply (ns, xmlValue)

  ns = getNodeSet(d, "//x:results/x:result/x:binding[@name = 'writable']/x:literal", "x")
  writable = sapply (ns, xmlValue)

  data.frame(uri, id, title, readable, writable)
}
