# Initialization functions

# Creates an object with the settings needed to access the server
# Parameters:
# protocol            "http://" or "https://"
# server_add          the address part (without the protocol) of the RDF4J server (do not use trailing slash)
# authentication      "basic_http" or "api"
# userpwd             a user-password combination, used in case of basic HTTP authentication, e.g. "user:pass"
# api_key             API key used for API-style authentication
# secret              the secret string corresponding to the API key needed for API-style authentication

create_server_options = function ( protocol, server_add, authentication = "basic_http", userpwd, api_key, secret ) {
  if ( authentication == "basic_http" ) {
    return ( list(
      server_url = paste( protocol, server_add, sep = "" ),
      authentication = authentication,
      userpwd = userpwd
    ) )
  }
  else {
    return ( list (
      server_url = paste( protocol, api_key, ":", secret, "@", server_add, sep = ""),
      authentication = "api",
      api_key = api_key,
      secret = secret
    ))
  }
}
