#' OpenBiodiv Person URI's
#'
#' @return a character vector of all person URI's in OpenBiodiv
#'
#' @export
person_id = function()
{
	query = "SELECT ?person
			  WHERE {
				?person a foaf:Person .
			  }"
	person.data.frame = execute_openbiodiv_query( query )
  return( person.data.frame$person )
}

#' Person Constructor
#'
#' @param id
#'
#' @return an object of class Person
#'
#' @export

person_constructor = function ( id ) {
  query = "SELECT ?affiliation ?label ?first_name ?last_name ?email
           WHERE {
             %id a foaf:Person ;
                 rdfs:label ?label .
             OPTIONAL { %id :affiliation ?affiliation ;
                            foaf:firstName ?first_name ;
                            foaf:surname ?last_name . }
             OPTIONAL { %id foaf:mbox ?email ;
                            foaf:firstName ?first_name ;
                            foaf:surname ?last_name . }
            }
          "
  result = execute_openbiodiv_query( query, c( "%id" = strip_angle( id, reverse = TRUE ) ) )
  person = list(
    affiliation = unique( result$affiliation[ result$affiliation != "" ] ),
    label = unique( result$label [ result$label != "" ] ),
    first_name = unique( result$first_name [ result$first_name != "" ] ),
    last_name = unique( result$last_name[ result$last_name != "" ] ),
    email = unique( result$email [result$email != "" ]) )
  class( person ) = "Person"
  return( person )
}


#' Same Person
#'
#' - construct the person object for the person URI
#' - run all three rules
#'
#' @param id an person URI in OpenBiodiv
#'
#' @return all other URI's that point to the same person
#'
#' @export

same_person = function ( id ) {
  person = person_constructor( id )
  setdiff( unique( author_rule.person( person )$entity ), id)
}


#' owl:sameAs SPARQL query
#'
#' @param id1 the first ID, must a vector long exactly 1
#' @param id2 the second ID, must be a vector long at least 1
#'
#' @return SPARQL query string where id1 is the same as all of the id2's,
#'   unprefixed
#'
#' @export
same_as_query = function( id1, id2 )
{
  if (!( length( id1 ) == 1 && length( id2 ) >= 1)) return ("")
  query_begin = "INSERT DATA {"
  query_middle = sapply ( id2, function ( id ) {
    paste( strip_angle( id1, reverse = TRUE) , "owl:sameAs", strip_angle( id, reverse = TRUE) , "." )
  } )
  query_end ="}"
  return( c ( query_begin, query_middle, query_end ) )
}

#' Inserts all possible owl:sameAs properties for class Person
#' @export
insert_sameas_person = function( person_id ) {

  for ( pid in person_id ) {
    query = same_as_query( pid, same_person( pid ) )
    cat(query)
    pid<<-pid
    query<<-query
    execute_openbiodiv_update_query( query )
  }
}


