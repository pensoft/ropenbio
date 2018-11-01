#' Lookup Paper ID Query
#'
#' Looks for an object of type paper insde the article context.
#'
#' @field article_id
#'
#' @export
qlookup_paper_id =
"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX fabio: <http://purl.org/spar/fabio/>

SELECT DISTINCT ?id WHERE {
 GRAPH %article_id {
  ?id rdf:type fabio:ResearchPaper .
 }
}"



#' Lookup the Taxonomic Concept of a Treatment
#'
#' Looks for an object of type paper insde the article context.
#'
#' @field article_id
#'
#' @export
qlookup_taxonomic_concept_id =
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX fabio: <http://purl.org/spar/fabio/>
PREFIX openbiodiv: <http://openbiodiv.net/>
PREFIX frbr: <http://purl.org/vocab/frbr/core#>

SELECT DISTINCT ?taxonomic_concept WHERE {
 {
  %treatment rdf:type openbiodiv:Treatment ;
            frbr:realizationOf ?taxonomic_concept .
 }
}"


#' Look Up Anything by Label
#'
#' Most general lookup by label query.
#'
#' @field label
#'
#' @export
qlookup_by_label = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT DISTINCT ?id WHERE {
  ?id (rdfs:label|skos:prefLabel|skos:altLabel) %label
}"




#' Look Up Anything by Label In Context
#'
#' Most general lookup by label query.
#'
#' @field label
#'
#' @export
qlookup_by_label_in_context = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT DISTINCT ?id WHERE {
  GRAPH %context {
    ?id (rdfs:label|skos:prefLabel|skos:altLabel) %label
  }
}"







#' Look Up Anything by Label No Context
#'
#' Equivalent to lookup_by_label but with an additional context argument
#' ignored.
#'opebiodiv_id_via_label_lookup
#' Most general lookup by label query.
#'
#' @field label
#'
#' @export
qlookup_by_label_no_context = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT DISTINCT ?id WHERE {
  #GRAPH %context {
    ?id (rdfs:label|skos:prefLabel|skos:altLabel) %label
  #}
}"









#' Look Up by Scientific Name by Label No Context
#'
#' Equivalent to lookup_by_label but with an additional context argument
#' ignored.
#'
#' Most general lookup by label query.
#'
#' @field label
#'
#' @export
qlookup_by_sciname_no_context = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX dwc: <http://rs.tdwg.org/dwc/terms/>

SELECT DISTINCT ?id WHERE {
  #GRAPH %context {
    ?id dwc:scientificName %label
  #}
}"








#' Looks up an author's ID by his or her email.
#'
#' Idea: first letter of first name AND
#'       exact match of email AND
#'       fuzzy match of last name
#'
qlookup_author1 = "PREFIX
SELECT ?author_id
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
