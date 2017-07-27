


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
#' @param options           a list returned by \code{server_access_options}.
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
#' @param options           a list returned by \code{server_access_options}.
#' @param repo_id           a string holding the repository id as returned by \code{get_repositories}.
#' @param results_format    a string, indicating the type of results required. Currently supported: "XML", "CSV".

#' @param infer             a boolean, indicating whether we want to use inference.
#' @param query_language    a string: "SPARQL" or "SERQL" (untested)
#' @param varbindings       currently unused
#' @param timeout           currently unused
#' @param update            if this is TRUE, the query is submitted instead to the
#'   statements endpoint
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
  else if ( options$authentication == "api" ) { # API authentication
    RCurl::curlSetOpt( .opts = list(httpheader = header, verbose = FALSE), curl = curl)
  }
  else {
    RCurl::curlSetOpt( .opts = list(httpheader = header, verbose = FALSE), curl = curl)
  }

  r = RCurl::postForm( endpoint, query = query, curl = curl,  style = "POST")
  if ( results_format == "CSV") {
    data = read.csv( textConnection( r ) , stringsAsFactors = FALSE)
    return ( data )
  }
  else { #XML case
    return ( r )
    }
}

#' Add data to a repository
#' @param server_access_options the access details to an RDF4J store, created by
#' \code{server_access_options}
#' @param repository the name of the repository, where to add data to
#' @param data the RDF data containing the triples to be submitted
#' @param data_format data format, one of {"application/x-trig"}
#' @export

add_data = function( server_access_options, repository, data,
             data_format = "application/x-trig", update = FALSE)
{

  endpoint = paste( server_access_options$server_url, "/repositories/",
                    repository, "/statements", sep = "")

  if ( update ) {
    endpoint = paste( server_access_options$server_url, "/repositories/",
                      repository, "/statements?update=", URLencode(data, reserved = TRUE), sep = "")
    data = ""
    data_format = "application/rdf+xml"
  }
  if ( server_access_options$authentication == "basic_http") {

    user = strsplit( server_access_options$userpwd, ":" )[[1]][1]
    password = strsplit( server_access_options$userpwd, ":" )[[1]][2]
    httr::POST( url = endpoint, httr::content_type(data_format),
                httr::authenticate(user, password, type = "basic"), body = data )

  }
  else if ( server_access_options$authentication == "FALSE" ) {
    httr::POST( url = endpoint, httr::content_type(data_format), body = data )
  }
}

#' REPOSITORY INFORMATION

#' Test connectivity to the graph database and get the communication protocol
#' version.
#'
#' This function tests the connectivity to the graph database. If there is
#' connectivity it will return the protocol version as a numeric.
#' If there is no connectivity, an error condition will be set.
#'
#' @param options   a list, containing the graph database connectivity options,
#'                returned by the helper function \code{server_access_options}.
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
# options        should be an object returned by server_access_options
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

#
# FRAMEWORK FOR ESTABLISHING A CONNECTION TO A SEMANTIC GRAPH DATABASE
#


#' Server Access Options Constructor
#'
#' This function is used to create an \code{Server_Access_Options} object,
#' containing all the configuration #' needed to access the RDF4J endpoint:
#' protocol, server address, authentication method, credentials, and so on.
#'
#' TODO: rename to simply server_access_options
#'
#' @param protocol a string, either "http://" or "https://".  Will be prepended
#'   to the endpoint URL.
#' @param server_add a string, the address part (without the protocol)
#'   of the RDF4J server. Please, do not use trailing slash!
#' @param authentication a string, either "basic_http" or "api". In the case
#'   of "basic_http", you are further required to supply a username-password
#'   combination. In the case of "api", you are further required to supply an
#'  API key and the associated secret.
#' @param userpwd a string, a user-password combination, used in case of
#'   basic HTTP authentication, e.g. "user:pass".
#' @param api_key a string, the API key used for API-style authentication.
#' @param secret a string, the secret string corresponding to the API key
#'   needed for API-style authentication.
#' @param repository a string, an optional parameter specifying the repository
#'   in the graph database that we will be accessing
#'
#' @return a \code{Server_Access_Options} object
#'
#' @examples
#'
#' \dontrun{ options = server_access_options( protocol = "http://",
#'   server_add = "213.191.204.69:7777/graphdb",
#'   authentication = "basic_http",
#'   userpwd = "deliberately_changed_username:fake_password" ),
#' r  epository = "OBKMS" }
#'
#' \dontrun{ options = server_access_options( protocol = "https://",
#'   server_add = "rdf.s4.ontotext.com/4937448214/OBKMS",
#'   authentication = "api", api_key = "wrong_api_key", secret = "no_secret" ) }
#'
#' @export

server_access_options = function( protocol = "http://",
                                  server_add,
                                  authentication = FALSE,
                                  userpwd,
                                  api_key,
                                  secret,
                                  repository = "" )
{
  if( authentication == "basic_http" ) {
    server_access_options = list(
      server_url = paste( protocol, server_add, sep = "" ),
      authentication = authentication,
      userpwd = userpwd,
      repository = repository
    )
  }
  else if( authentication == "api" ) {
    server_access_options = list(
      server_url = paste( protocol, api_key, ":", secret, "@", server_add, sep = ""),
      authentication = "api",
      api_key = api_key,
      secret = secret,
      repository = repository
    )
  }
  else if( authentication == FALSE ) {
    server_access_options = list(
      authentication  = "no" ,
      server_url = paste( protocol, server_add, sep = "" ) ,
      repository = repository )
  }
  class( server_access_options ) = "Server_Access_Options"
  return( server_access_options )
}



#' Server Access Options Constructor from File
#'
#' Additionally, it reads the access secret (password) from the environment
#' variable \code{OBKMS_SECRET}
#'
#' TODO: rename to server_access_options.file
#' TODO: class names ought to be a global parameter (like Server_Access_Options)
#'
#' @param filename a YAML file containing the server access options
#' @return a \code{Server_Access_Options} object read from the file
#'
#' @export

server_access_options.file = function( filename ) {
  obkms_options = yaml::yaml.load_file( filename )
  obkms_options$userpwd = Sys.getenv("OBKMS_SECRET")
  class( obkms_options ) = "Server_Access_Options"
  return (obkms_options)
}


