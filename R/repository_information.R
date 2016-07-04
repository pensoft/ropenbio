# Functions needed to query repository information from the server

# Gets the communication protocol version
# Parameters:
# options         should be an object returned by create_server_options
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
    r = RCurl::getURL( paste( options$server_url, endpoint, sep = ""), verbose = TRUE, httpheader = header)
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
