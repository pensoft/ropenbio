library(testthat)
library(rdf4jr)

context( paste( "PROTOCOL ", paste( rep( "=", 80 - nchar( "PROTOCOL " ) ) , collapse = ""), "\n", sep = "" ) )

test_that("Getting protocol version returns a character (basic authentication):",
          {
            # first with basic authentication
            options = create_server_options(protocol = "http://", server_add = "213.191.204.69:7777/graphdb", authentication = "basic_http", userpwd = "obkms:1obkms")
            expect_match(get_protocol_version(options), "[0-9]")

          })

test_that("Getting protocol version returns a character (API authentication):",
          {
            # now with API
            options = create_server_options(protocol = "https://", server_add = "rdf.s4.ontotext.com/4937448214/OBKMS", authentication = "api", api_key = "s4i0k6o7v6l0", secret = "9k03ah49e3da607"  )
            expect_match(get_protocol_version(options), "[0-9]")
          })

my_context = "LIST OF REPOSITORIES "
context( paste( my_context, paste( rep( "=", 80 - nchar( my_context ) ) , collapse = ""), "\n", sep = "" ) )

test_that("Getting list of repositories returns a data frame (basic authentication):",
          {
            library(XML)
            options = create_server_options(protocol = "http://", server_add = "213.191.204.69:7777/graphdb", authentication = "basic_http", userpwd = "obkms:1obkms")
            expect_is( get_repositories( options ), "data.frame" )
          })

test_that("Getting list of repositories returns a data frame (API authentication)::",
          {
            # now with API
            options = create_server_options(protocol = "https://", server_add = "rdf.s4.ontotext.com/4937448214/OBKMS", authentication = "api", api_key = "s4i0k6o7v6l0", secret = "9k03ah49e3da607"  )
            expect_is( get_repositories( options ), "data.frame" )
          })
