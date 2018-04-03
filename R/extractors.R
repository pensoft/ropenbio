#' Top Level (Metadata) Extractor for a Taxnomic Article
#'
#' First this function finds all of the atoms that are belong to the metadata
#' of a taxonomic paper and creates triples for it. Then, it chops up the paper
#' XML into smaller parts that are sent to lower-level extractors to get
#' triples for them. After all of this everything is concattenated and returned.
#'
#' @inheritParams xml2rdf
#' @param triples the RDF object where the newly created triples will be
#'   stored (note this is a side-effect/pass by reference)
#'
#' @return ResourceDescriptionFormat - returns the \code{rdf_object} but
#'   you don't have to
#'
#' @export
root_extractor = function(xml, xml_schema, reprocess, triples, access_options)
{
  # 1. Find obkms_id of node. Set the context in the rdf_object. Create
  # the obvious triple(s) that id is ...article.
  # The first mentioning of the openbiodiv prefix will make it available
  # to the triples object for subsequent lookups (we will stay DRY)
  root_id = identifier(
    id = get_or_set_obkms_id(xml),
    prefix = c(openbiodiv = get_namespace(access_options$prefix, "_base"))
  )

  triples$set_context(root_id)
  triples$add_triple(subject = root_it, predicate = rdfs_label, object = journal_article)

  browser()

  # 2. Find the processing status of the node.
  # 3. If the processing status is FALSE, or reprocess for root is present
  #   Find literals
  #   call paper and/or article construcs ---> actually these can be subclasses of ResourceDescriptionFramework, initialized with literals and access_options
  #   add the RDF subobjects via big RDF object via add_list or similar
  # 4. Find subcomponets (indicated in the xml_schema)
  # 5. Foreach subcomponent, execute its extractor (also part of the xml_schema) will get the triples object passed on for the side effects
  # 6. return the triples even though it is not necessary

  metadata = find_literals(xml, xml_schema$atoms)
  class(metadata) = "Taxonomic Article"

  # `get_or_set_obkms_id` only looks in the XML, does not do any db lookup
  metadata[['article_id']] =
    qname( get_or_set_obkms_id( xml , fullname = TRUE ) )

  paper_label = paste0( "paper", metadata$doi )
  metadata[['paper_id']] =
    qname ( lookup_id( label = paper_label,
                       article_id = metadata[['article_id']] ) )
  # the paper is the "work" of an article

  metadata[['publisher_id']] =
    qname ( lookup_id( metadata$publisher_name,
                       language = obkms$parameters$Language$English) )
  # looks up the publishers by its label

  metadata[['journal_id']] =
    qname ( lookup_id( metadata$journal_title,
                       language = obkms$parameters$Language$English ))

  triples = list()

  triples[["metadata"]] = list (

    triple2( metadata$journal_id, qname( obkms$properties$type$uri ),                qname ( obkms$classes$Journal$uri ) ),
    triple2( metadata$journal_id, qname( obkms$properties$preferred_label$uri ),     squote ( metadata$journal_title, language = obkms$parameters$Language$English ) ),
    triple2( metadata$journal_id, qname( obkms$properties$alternate_label$uri ),     squote ( metadata$abbrev_journal_title, language = obkms$parameters$Language$English ) ),
    triple2( metadata$journal_id, qname( obkms$properties$issn$uri ),                squote ( metadata$issn ) ),
    triple2( metadata$journal_id, qname( obkms$properties$eissn$uri ),               squote ( metadata$eissn ) ),
    triple2( metadata$journal_id, qname( obkms$properties$sub_endeavour_id$uri ),    metadata$journal_id),

    triple2( metadata$article_id, qname( obkms$properties$type$uri ),                qname( obkms$classes$Article$uri ) ),
    triple2( metadata$article_id, qname( obkms$properties$preferred_label$uri ),     squote( metadata$doi ) ) ,
    triple2( metadata$article_id, qname( obkms$properties$title$uri ),               squote( metadata$article_title, language = obkms$parameters$Language$English ) ) ,
    triple2( metadata$article_id, qname( obkms$properties$doi$uri ),                 squote( metadata$doi ) ) ,
    triple2( metadata$article_id, qname( obkms$properties$publisher$uri ),           squote( metadata$publisher_name , language = obkms$parameters$Language$English ) ), # the publisher of an article is clear
    triple2( metadata$article_id, qname( obkms$properties$publication_year$uri ),    squote( metadata$pub_year, literal_type = obkms$parameters$literal_type$year ) ),
    triple2( metadata$article_id, qname( obkms$properties$publication_date$uri ),    squote( paste( metadata$pub_year, metadata$pub_month, metadata$pub_day, sep = "-") , literal_type = obkms$parameters$literal_type$date ) ),
    triple2( metadata$article_id, qname( obkms$properties$publisher_id$uri ),        metadata$publisher_id ) ,
    triple2( metadata$article_id, qname( obkms$properties$realization_of$uri ),      metadata$paper_id ) , # TODO check whether the inverse is being materialized by the ontology

    triple2(  metadata$paper_id,  qname( obkms$properties$type$uri) ,                qname( obkms$classes$Paper$uri ) ) ,
    triple2(  metadata$paper_id,  qname( obkms$properties$alternate_label$uri ),     squote( paper_label ) ) ,

    triple2( metadata$publisher_id,  qname( obkms$properties$type$uri ),             qname( obkms$classes$Publisher$uri ) ),
    triple2( metadata$publisher_id,  qname( obkms$properties$preferred_label$uri ),  squote( metadata$publisher_name, language = obkms$parameters$Language$English ) ) )

  # 2nd part of the function Document component entities (sub-article level entities )
  article_component = document_components ( xml, xdoco )

  for (cc in article_component$collection_code_usage) {
    triples[["collection_code"]] = c(triples[["collection_code"]], collection_code_extractor(metadata$paper_id, cc$xml, metadata$article_id))
  }

  for ( a in article_component$authors ) {
    triples[["author"]] = c ( triples[["author"]],  author_extractor( metadata$paper_id , metadata$article_id, a$xml, document = xml) )
  }

  # keyword processing
  # TODO : broke extract_keywords
  triples[["subject"]] = unlist( lapply ( c( article_component$subject_classification, article_component$zookeys_keywords), function ( keyword_group_node) {
    keyword_extractor( XML = list( node = keyword_group_node$xml ),
                      metadata = metadata,
                      keyword_scheme = obkms$parameters$Vocabularies$Subject_Classification_Terms )
  }), recursive = FALSE)

  triples[["taxon"]] = unlist( lapply (  article_component$taxon_classification, function ( keyword_group_node) {
    keyword_extractor( XML = list( node = keyword_group_node$xml ),
                      metadata = metadata,
                      keyword_scheme = obkms$parameters$Vocabularies$Taxon_Classification_Terms )
  }), recursive = FALSE)

  triples[["geography"]] = unlist( lapply (  article_component$geographic_classification, function ( keyword_group_node) {
    keyword_extractor( XML = list( node = keyword_group_node$xml ),
                      metadata = metadata,
                      keyword_scheme = obkms$parameters$Vocabularies$Geographic_Classification_Terms )
  }), recursive = FALSE)

  # TODO maybe add publisher role

  triples[["front"]] = FrontMatter_extractor(article_component$front_matter, metadata )

  triples[["TNU"]] = unlist( lapply ( article_component$taxonomic_name_usage,
    function ( TNU ) {
      TaxonomicNameUsage_extractor(TNU, metadata)
    } ), recursive = FALSE)

  triples[["treatment"]] = unlist(lapply(article_component$treatment,
    function (t) {
      Treatment_extractor(t, "taxpub")
    }), recursive = FALSE)

  triples = do.call( c, triples )

  return ( triples )
}




#' Extract Collection Code
#'
#'
#' @param paper_id URI of the `fabio:ResearchPaper` of the paper
#' @param cc_xml XML2 node of the collection code usage

#' @return list of triples
#'
#' @export
collection_code_extractor = function(paper_id, cc_xml, article_id)
{
  cc = as.list( find_literals( cc_xml, list(collection_code = ".") ) )
  id = lookup_id(cc$collection_code)
  triples = list(
      triple2(qname(paper_id),      qname(obkms$properties$mentions_collection_code$uri), qname(id)),
      triple2(qname(id),    qname(obkms$properties$label$uri),            squote(cc$collection_code)),
      triple2(qname(id),    qname(obkms$properties$type$uri),            qname(obkms$classes$Collection$uri)))

  serialization = c(turtle_prepend_prefixes(), triples2turtle2(article_id, triples), "\n\n" )

  add_data(server_access_options = obkms$server_access_options, repository = obkms$server_access_options$repository, data = serialization)

  return ( triples )
}




#' Extract Author Information from XML
#'
#' Note: extracts the information for _one_ author.
#'
#' @param paper_id URI of the `fabio:ResearchPaper`
#' @param article_id URI of the `fabio:JournalArticle` that is the realization
#'                                                                  of the paper
#' @param author_xml XML2 object, where the current author information is to be
#'                                                                         found
#' @param author_xpath (relative) xpaths of where the author literals are to
#'   be found in the XML object
#' @param document the whole XML document of which the current XML object is
#'                                                                       part of
#' @return list of triples
#'
#' TODO: this function should not be exported
#' @export
author_extractor = function ( paper_id ,
                              article_id,
                              author_xml,
                              authors_xpath =  yaml::yaml.load_file( obkms$config$authors_db_xpath ),
                              document )
{
  # construct the author record
  # TODO refactor find_literals to return a list (or maybe even an env!, as we
  # do want to increase it )
  # and set the class parameter as well, i.e. make find_literals to a contructor
  author = as.list( find_literals( author_xml, authors_xpath ) )
  author$affiliation  = sapply( author$reference,
                                retrieve_affiliation_string_from_author,
                                xml_document = document )
  author$label = paste( author$given_name, author$surname )
  class( author ) = "Author"

  # id's TODO we're copying the list here, may need to use a env instead
  identifier = list()
  identifier$article = article_id

  fcall = vector(mode = "list", length = 3)
  fcall[[1]] = list( "lookup_id", list( label = author$label, article_id = identifier$article, generate_on_fail = FALSE) )
  fcall[[2]] = list ( "rule_based_lookup", list( parameter.lst = author, rule = c("author_rule.3" , "author_rule.1", "author_rule.2") ) )
  fcall[[3]] = list( "lookup_id", list( label = author$label, article_id = identifier$article, generate_on_fail = TRUE) )

  identifier$author = NA
  i = 1
  while ( is.na ( identifier$author ) && i <= length( fcall ) ) {
    identifier$author = do.call( fcall[[i]][[1]], fcall[[i]][[2]] )
    i = i + 1
  }
  identifier$author =  qname( identifier$author )

  if ( !is.na( author$collab ) ) {
    triples = list (
      triple2( paper_id,                qname( obkms$properties$creator$uri ),         identifier$author ) ,
      triple2( identifier$author,    qname( obkms$properties$type$uri ),            qname ( obkms$classes$Agent$uri ) ),
      triple2( identifier$author,    qname( obkms$properties$label$uri ), squote ( author$collab ) ),
      triple2( identifier$author,    qname( obkms$properties$email$uri ),           squote ( author$email ) ) )
  }
  else {
    triples = list (
      triple2( paper_id,                qname( obkms$properties$creator$uri ),         identifier$author ) ,
      triple2( identifier$author,    qname( obkms$properties$type$uri ),            qname ( obkms$classes$Person$uri ) ),
      triple2( identifier$author,    qname( obkms$properties$preferred_label$uri ), squote ( paste(author$given_name, author$surname, sep = " " ) ) ),
      triple2( identifier$author,    qname( obkms$properties$first_name$uri ),      squote ( author$given_name ) ),
      triple2( identifier$author,    qname( obkms$properties$surname$uri ),         squote ( author$surname ) ),
      triple2( identifier$author,    qname( obkms$properties$email$uri ),           squote ( author$email ) ) )
  }

  # add the affiliation string
  for ( affiliation in author$affiliation ) {
    triples = c( triples, list (
    triple2( identifier$author,    qname( obkms$properties$has_affiliation_string$uri ),
          squote( affiliation, language = obkms$parameters$Language$English) ) ) )
  }

  # add the institution information
  for ( inst_id in unlist( identifier$institution ) ) {
    triples = c( triples, list (
        triple2( identifier$author, qname( obkms$properties$is_member_of_organization$uri ), inst_id ) ) )
  }
  return ( triples )
}








#' Taxonomic Name Usage Extractor
#'
#'
#' Extracts knowledge from an XML node containing a taxonomic name usage.
#' Extracts also names.
#'
#' @param comp an XML componennt containing a taxonomic name usage
#' @param metadata The metadata of the article. Needed to get the context.
#'
#' @return triples of RDF
TaxonomicNameUsage_extractor = function (comp, metadata)
{

  a_TaxonomicNameUsage = TaxonomicNameUsage( comp$xml )
  a_TaxonomicName = as.TaxonomicName(a_TaxonomicNameUsage)

  rdf = list()

  rdf$TNU = as.rdf(a_TaxonomicNameUsage)
  # potentially may be connected to multiple names
  rdf$TNU_connect =   list(
    triple2(qname(a_TaxonomicNameUsage$parent_id), qname(obkms$properties$contains$uri), qname(a_TaxonomicNameUsage$id))
  )
  rdf$name = as.rdf(a_TaxonomicName)
  rdf$name_connect = list(
    triple2(qname(a_TaxonomicNameUsage$id), qname(obkms$properties$mentions$uri), qname(a_TaxonomicName$id))
  )

  #TODO RDF must be submitted immediately because next stage depends on it



  rdf = unlist(rdf, recursive = FALSE)
  serialization = c(turtle_prepend_prefixes(), triples2turtle2(metadata$article_id, rdf))
  result = add_data(obkms$server_access_options, obkms$server_access_options$repository, serialization)


  return(rdf)
}










#' Extracts RDF from a Treatment Node
#'
#' @param node XML node. Contains a Treatment.
#' @param xml_schema character, contains the XML name of the XML schema to use
#'
#' @return RDF
Treatment_extractor = function(node, xml_schema) {

  a_Treatment = Treatment(node$xml)
  a_TaxonomicNameUsage_node = xml2::xml_find_first(node$xml,
                    obkms$xpath[[xml_schema]]$Treatment$first_taxonomic_name_usage)
  a_TaxonomicNameUsage = TaxonomicNameUsage(a_TaxonomicNameUsage_node)
  a_TaxonomicConceptLabel_id = lookup_TaxonomicName_id(a_TaxonomicNameUsage, a_Treatment$root_id) # don't generate?
  a_TaxonomicConcept_id = lookup_TaxonomicConcept_id(a_TaxonomicConceptLabel_id)

  # need to getthe nodes that are in the nomenclatural section
  b_TaxonomicNameUsage_nodes = xml2::xml_find_all(node$xml, obkms$xpath[[xml_schema]]$Treatment$nomenclatural_TNU)

  # for each of the XML nodes, we need to extract the TNU and get the ID of the name the TNU is pointing at
  b_TaxonomicName_id = sapply(b_TaxonomicNameUsage_nodes, function(node) {
    lookup_TaxonomicName_id(TaxonomicNameUsage(node), a_Treatment$root_id)
  })


  rdf = list()

  #Now we want to say that id of the TCL is related to the IDs of the
  rdf$related_names = lapply(b_TaxonomicName_id, function(x) {
    triple2(qname(a_TaxonomicConceptLabel_id), qname(obkms$properties$related_name$uri), qname(x))
  })

  rdf$treatment = as.rdf(a_Treatment)
  rdf$connect = list(
    triple2(qname(a_Treatment$parent_id), qname(obkms$properties$contains$uri), qname(a_Treatment$id)),
    triple2(qname(a_Treatment$id), qname(obkms$properties$mentions$uri), qname(a_TaxonomicConceptLabel_id)),
    triple2(qname(a_TaxonomicConcept_id), qname(obkms$properties$taxonomicConceptLabek$uri), qname(a_TaxonomicConceptLabel_id))
  )

  rdf = unlist(rdf, recursive = FALSE)
  return(rdf)
}





#' Front Matter Extractor
#'
#' Extracts the front matter as RDF from a list of components
#'
#' @param comp A list of components containing the front matter.
#' @param article a journal article object
#'
#' @return triples
#'
FrontMatter_extractor = function (comp, article)
{
  comp = unlist(comp, recursive = FALSE)

  # process the front matter itself
  front_matter = DocumentComponent(comp$xml, obkms$xpath$taxpub$FrontMatter, c("FrontMatter"))

  # find sub-components
  # this needs to use the absolute paths again
  component_xpath = list(Abstract = obkms$xdoco$Abstract,
                         Title = obkms$xdoco$Title)
  subcomponent = document_components(comp$xml, component_xpath)



  title        = DocumentComponent(unlist(subcomponent$Title, recursive = FALSE)$xml, obkms$xpath$taxpub$Title, "Title" )

  contains = list(triple2(qname(front_matter$id), qname(obkms$properties$contains$uri), qname(title$id)))

  # potentiall multiple abstracts in different languages
  abstract     = lapply(subcomponent$Abstract, function(a) {
    DocumentComponent(a$xml, obkms$xpath$taxpub$Abstract, "Abstract")
  })


  contains = c(
    list(triple2(qname(front_matter$id), qname(obkms$properties$contains$uri), qname(title$id))),
    lapply(abstract, function(a) {
    triple2(qname(front_matter$id), qname(obkms$properties$contains$uri), qname(a$id))
  }))
  #containment

  rdf = c(as.rdf(front_matter), as.rdf(title), unlist(lapply(abstract, as.rdf), recursive = FALSE))

  return(c(rdf, contains))
}

