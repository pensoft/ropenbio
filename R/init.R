#  _____   _   _   _____   _______
# |_   _| | \ | | |_   _| |__   __|
#   | |   |  \| |   | |      | |
#   | |   | . ` |   | |      | |
#  _| |_  | |\  |  _| |_     | |
# |_____| |_| \_| |_____|    |_|
#

#' Create an object with the settings needed to access the server
#'
#' This function is used to create an \code{options} variable, containing all the configuration
#' needed to access the RDF4J endpoint: protocol, server address, authentication method, and
#' credentials.
#'
#' @param protocol         a string, either "http://" or "https://". Will be prepended to the endpoint URL.
#' @param server_add       a string, the address part (without the protocol) of the RDF4J server. Please, do not use trailing slash!
#' @param authentication   a string, either "basic_http" or "api".
#'                            In the case of "basic_http", you are further required to supply a username-password combination.
#'                            In the case of "api", you are further required to supply an API key and the associated secret.
#'
#' @param userpwd         a string, a user-password combination, used in case of basic HTTP authentication, e.g. "user:pass".
#' @param api_key         a string, the API key used for API-style authentication.
#' @param secret          a string, the secret string corresponding to the API key needed for API-style authentication.
#'
#' @param repository      a string, an optional parameter specifying the repository in the graph database that we will be accessing
#'
#' @examples
#' \dontrun{options = create_server_options(protocol = "http://", server_add = "213.191.204.69:7777/graphdb", authentication = "basic_http", userpwd = "deliberately_changed_username:fake_password"), repository = "OBKMS"}
#' \dontrun{options = create_server_options(protocol = "https://", server_add = "rdf.s4.ontotext.com/4937448214/OBKMS", authentication = "api", api_key = "wrong_api_key", secret = "no_secret"  )}
#' @export

create_server_options = function ( protocol = "http://", server_add, authentication = FALSE, userpwd, api_key, secret, repository = "" ) {
  if ( authentication == "basic_http" ) {
    return ( list(
      server_url = paste( protocol, server_add, sep = "" ),
      authentication = authentication,
      userpwd = userpwd,
      repository = repository
    ) )
  }
  else if (authentication == "api") {
    return ( list (
      server_url = paste( protocol, api_key, ":", secret, "@", server_add, sep = ""),
      authentication = "api",
      api_key = api_key,
      secret = secret,
      repository = repository
    ))
  }
  else if (authentication == FALSE ) {
    return ( list (
    authentication  = "no" ,
    server_url = paste( protocol, server_add, sep = "" ) ,
    repository = repository ) )
  }
}


#' Creates the server options from a YAML file and the environment
#'
#' @export
#'
create_server_options_env = function( filename ) {
  obkms_options = yaml::yaml.load_file( filename )
  obkms_options$userpwd = Sys.getenv("OBKMS_SECRET")
  return (obkms_options)
}
