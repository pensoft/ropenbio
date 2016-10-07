# Test Package Against EUCases (Basic Authentication)

library(testthat)
library(rdf4jr)
library(XML)

query = "SELECT (COUNT(*) as ?count)
FROM <http://www.ontotext.com/implicit>
WHERE {
   ?s ?p ?o .
}"

context( paste( "EU-CAESES (Basic Auth) ", paste( rep( "=", 80 - nchar( "EU-CAESES (Basic Auth) " ) ) , collapse = ""), "\n", sep = "" ) )

eucases_secret = read.table( file = "../../secrets/eucases.txt", header = TRUE , stringsAsFactors = FALSE)
options = create_server_options(protocol = eucases_secret[1, "protocol"], server_add = eucases_secret[1, "server_add"], authentication = eucases_secret[1, "authentication"], userpwd = eucases_secret[1, "userpwd"])

test_that("getting protocol version returns a number between 0 and 9...",
          {
            expect_match(get_protocol_version(options), "[0-9]")
          })

test_that("getting list of repositories returns a data frame...",
          {
            expect_is( get_repositories( options ), "data.frame" )
          })

test_that("POST-ing a query with CSV result returns a data frame...",
          {
            expect_is( POST_query( options, "OBKMS", query, "CSV" ), "data.frame" )
          })

test_that("POST-ing a query with XML result returns a character vector...",
          {
            expect_is( POST_query( options, "OBKMS", query, "XML" ), "character" )
          })


