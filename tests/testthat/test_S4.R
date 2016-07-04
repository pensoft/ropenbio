library(testthat)
library(rdf4jr)
library(XML)

query = "SELECT (COUNT(*) as ?count)
FROM <http://www.ontotext.com/implicit>
WHERE {
   ?s ?p ?o .
}"

context( paste( "S4 (Api Auth) ", paste( rep( "=", 80 - nchar( "S4 (Api Auth) " ) ) , collapse = ""), "\n", sep = "" ) )

options = create_server_options(protocol = "https://", server_add = "rdf.s4.ontotext.com/4937448214/OBKMS", authentication = "api", api_key = "s4i0k6o7v6l0", secret = "9k03ah49e3da607"  )

test_that("getting protocol version returns a number between 0 and 9...",
          {
            expect_match(get_protocol_version(options), "[0-9]")
          })


test_that("getting list of repositories returns a data frame ...",
          {
            expect_is( get_repositories( options ), "data.frame" )
          })

test_that("GET-ing a query with a CSV-result returns a data frame...",
          {
            expect_is( GET_query( options, "plazi", query, "CSV" ), "data.frame" )

          })

test_that("GET-ing a query with a XML-result returns a character...", {
  expect_is( GET_query( options, "plazi", query, "XML" ), "character" )
})


test_that("POST-ing a query with CSV result returns a data.frame...",
          {you
            expect_is( POST_query( options, "plazi", query, "CSV" ), "data.frame" )
          })

test_that("POST-ing a query with XM<L result returns a character vector...", {
  expect_is( POST_query( options, "plazi", query, "XML" ), "character" )
})
