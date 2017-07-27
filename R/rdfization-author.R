#' Author Matches According to Author Rule 1
#'
#' Looks up an author's ID by his or her email.
#'
#' Idea: first letter of first name AND
#'       exact match of email AND
#'       fuzzy match of last name
#'
#' @param collab, surname, given_name, email, reference, affiliation
#'
#' @return a data.frame with person matches of class SearchResult
#'
#' @export
author_rule.1 = function( collab, surname, given_name, email, reference, affiliation, label )
{
  if ( is.na( given_name) || is.na( email ) || is.na( surname ) ) return ( data.frame())
  query = "SELECT ?entity ?score ?label
  WHERE {

  ?search a <http://www.ontotext.com/connectors/lucene/instance#PhraseSearch> ;
  lucene:query %lucene_query1 ;
  lucene:entities ?entity1 .

  ?search2 a <http://www.ontotext.com/connectors/lucene/instance#WordSearch>;
  lucene:query %lucene_query2 ;
  lucene:entities ?entity2 .

  FILTER ( ?entity1 = ?entity2 )
  BIND ( ?entity1 as ?entity )

  ?entity lucene:score ?score ;
  rdfs:label ?label ;
  foaf:mbox ?email .


  } ORDER BY DESC (?score)"

  lucene_query = character(2)
  lucene_query[1] = lucene_author_query( first_name = given_name,
                                         email  = email)
  lucene_query[2] = lucene_author_query( last_name = surname )


  query = gsub("%lucene_query1", paste0( "\"", lucene_query[1], "\""), query)
  query = gsub("%lucene_query2", paste0( "\"", lucene_query[2], "\""), query)
  query = do.call( paste, as.list( c( turtle_prepend_prefixes( t = "SPARQL" ), query )))

  result = rdf4jr::POST_query(
    obkms$server_access_options,
    obkms$server_access_options$repository,
    query, "CSV" )

  class(result) = c( class( result ), "SearchResult" )

  return( result )
}

#' Person Matches According to Author Rule 2
#'
#' Match against existing affiliations.
#' Note: one author may have multiple affiliations.
#'
#' Idea: first letter of first name AND
#'       fuzzy-and-match of last name AND
#'       fuzzy-and-match of affiliation AND
#'
#' @param collab, surname, given_name, email, reference, affiliation
#'
#' @return a data.frame with person matches of class SearchResult
#'
#' @export
author_rule.2 = function( collab, surname, given_name, email, reference, affiliation, label )
{
  r = lapply( affiliation, function ( a ) {
    if ( is.na( given_name) || is.na( surname ) || is.na( a ) ) return ( data.frame())
    query = "SELECT ?entity ?score ?label
    WHERE {
    ?search a <http://www.ontotext.com/connectors/lucene/instance#PhraseSearch> ;
    lucene:query %lucene_query1 ;
    lucene:entities ?entity1 .

    ?search2 a <http://www.ontotext.com/connectors/lucene/instance#WordSearch> ;
    lucene:query %lucene_query2 ;
    lucene:entities ?entity2 .

    ?search3 a <http://www.ontotext.com/connectors/lucene/instance#WordSearch> ;
    lucene:query %lucene_query3 ;
    lucene:entities ?entity3 .

    FILTER ( ?entity1 = ?entity2 && ?entity2 = ?entity3 )
    BIND ( ?entity1 as ?entity )

    ?entity   lucene:score ?score ;
    rdfs:label ?label .

    } ORDER BY DESC (?score)"

    lucene_query = character(3)
    lucene_query[1] = lucene_author_query( first_name = given_name )
    lucene_query[2] = lucene_author_query( last_name = surname )
    lucene_query[3] = lucene_author_query( affiliation = a )

    query = gsub("%lucene_query1", paste0( "\"", lucene_query[1], "\""), query)
    query = gsub("%lucene_query2", paste0( "\"", lucene_query[2], "\""), query)
    query = gsub("%lucene_query3", paste0( "\"", lucene_query[3], "\""), query)
    query = do.call( paste, as.list( c( turtle_prepend_prefixes( t = "SPARQL" ), query )))

    result = rdf4jr::POST_query(
      obkms$server_access_options,
      obkms$server_access_options$repository,
      query, "CSV" )

    class(result) = c( class( result ), "SearchResult" )

    return( result )
  })
  return ( do.call( rbind, r) )
  }

#' Person Matches According to Author Rule 3
#'
#' For collaborative authors, match the label
#'
#' @param collab
#' @param surname
#' @param given_name
#' @param email
#' @param reference
#' @param affiliation
#'
#' @return a data.frame with person matches of class SearchResult
#'
#' @export
author_rule.3 = function( collab, surname, given_name, email, reference, affiliation, label )
{
  if ( is.na( collab ) ) return (data.frame () )
  query = "SELECT ?entity ?score ?label
  WHERE {
  ?search a <http://www.ontotext.com/connectors/lucene/instance#PhraseSearch> ;
    lucene:query %lucene_query1 ;
    lucene:entities ?entity .

  ?entity a foaf:Agent .

  } ORDER BY DESC (?score)"

  lucene_query = c( lucene_label_query( collab ) )

  query = gsub("%lucene_query1", paste0( "\"", lucene_query[1], "\""), query)
  query = do.call( paste, as.list( c( turtle_prepend_prefixes( t = "SPARQL" ), query )))

  result = rdf4jr::POST_query(
    obkms$server_access_options,
    obkms$server_access_options$repository,
    query, "CSV" )

  class(result) = c( class( result ), "SearchResult" )

  return( result )
}

#' Execute all author rules on a Person object
#'
#' Useful to get all URI's that correspond to a Person.
#'
#' The function works by creating all possible combinations of the Person's
#' fields and running them against all the rules.
#'
#' @param person an object of class Person
#'
#' @return search results data.frame as returned by the rules
#' @export
author_rule.person = function( person )
{
  rule = c( "author_rule.1", "author_rule.2") # not 3 because not a collab author
  person.grid = expand.grid( affiliation = person$affiliation,
                             label = person$label,
                             first_name = person$first_name,
                             last_name = person$last_name,
                             email = person$email )
  result = list()
  for ( i in 1:nrow( person.grid ) ) {
    result[[i]] =  lapply( rule, function ( r ) {
      do.call( r, list( collab = NA,
                        label = person.grid[ i, ]$label ,
                        given_name = person.grid[i, ]$first_name,
                        surname = person.grid[i, ]$last_name,
                        email = person.grid[i, ]$email,
                        affiliation = person.grid[i, ]$affiliation,
                        reference = NA ) )
    })
    result[[ i ]] = do.call( rbind, result[[ i ]])
  }
  do.call( rbind, result )
}
