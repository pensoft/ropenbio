# Query Functions

# Uses the GET protocol to execute a query
# Currently, only works on S4?
# Parameters:
# options           an list returned by create_server_options
# repo_id           a string with repository id as returned by get_repositories
# results_format    a variable, indicating the type of results required, currently supported: "XML", "CSV"
#                   in case of CSV, will return a proper data frame
#                   in case of XML, will return a character string to be further handled by the user
# infer             boolean, whether we want to use inference
# query_language    "SPARQL" or "SERQL"
# varbindings       currently unused
# timeout           currently unuused
#
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

# Uses the POST protocol to query an endpoint
# Parameters:
# options           an list returned by create_server_options
# repo_id           a string with repository id as returned by get_repositories
# results_format    a variable, indicating the type of results required, currently supported: "XML", "CSV"
#                   in case of CSV, will return a proper data frame
#                   in case of XML, will return a character string to be further handled by the user
# infer             boolean, whether we want to use inference
# query_language    "SPARQL" or "SERQL"
# varbindings       currently unused
# timeout           currently unuused
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

