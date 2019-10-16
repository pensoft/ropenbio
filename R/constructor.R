#' @export
metadata = function (atoms, identifiers, prefix,new_taxons, mongo_key)
{
  pub_date = function(year, month, day) {
    literal(paste0(text_value = unlist(year)["text_value"],
                   "-", unlist(month)["text_value"], "-", unlist(day)["text_value"]),
            xsd_type = rdf4r::xsd_date)
  }
  if (length(unlist(atoms$pensoft_pub)) > 0) {
    stop("Pensoft publication")
  }
  general_collection = mongolite::mongo("new_collection")
  doi = unlist(atoms$doi)["text_value"]

  article_id = identifiers$root_id
  publisher_lit = toString(unlist(atoms$publisher)["text_value"])

  df = set_component_frame(label = publisher_lit, mongo_key = c(publisher = NA), type = "publisher", orcid = NA, parent = NA, key = NA)
  publisher_id = get_or_set_mongoid(df, prefix )
  publisher_id = identifier(publisher_id, prefix)

  journal_lit = toString(unlist(atoms$journal)["text_value"])

  df = set_component_frame(label = journal_lit, mongo_key = c(journal = NA), type = "journal", orcid = NA, parent = NA, key = NA)
  journal_id = get_or_set_mongoid(df, prefix )
  journal_id = identifier(journal_id, prefix)

  paper_label = unlist(atoms$title)["text_value"]

  research_paper_df = set_component_frame(label = paper_label, mongo_key = NA, type = "researchPaper", orcid = NA, parent = article_id$uri, key = NA)

  paper_id = get_or_set_mongoid(research_paper_df, prefix)
  paper_id = identifier(paper_id, prefix)


  tt = ResourceDescriptionFramework$new()


  tt$add_triple(journal_id, rdf_type, Journal)
  sapply(atoms$journal, function(j) {
    tt$add_triple(journal_id, pref_label, j)
  })
  sapply(atoms$journal_abbrev, function(j) {
    tt$add_triple(journal_id, alt_label, j)
  })
  sapply(atoms$issn, function(i) {
    tt$add_triple(journal_id, issn, i)
  })
  sapply(atoms$eIssn, function(i) {
    tt$add_triple(journal_id, eissn, i)
  })

  if(length(unlist(atoms$journal_zoobank))>0){
    for (n in 1:length(atoms$journal_zoobank)){
      if (grepl("zoobank", unlist(atoms$journal_zoobank[[n]])["text_value"]) == TRUE){
        text_value = gsub("^(.*):", "", unlist(atoms$journal_zoobank[n])["text_value"])
        text_value = gsub(" ", "", text_value)

        ll = list(text_value = text_value, xsd_type = xsd_string, lang = "",
                  squote = paste0("\"", text_value, "\"", ""))
        class(ll) = "literal"
        journal_zoobank_literal = ll


    zoobank_id = identifier(text_value, c(zoobank = "http://zoobank.org/"))
    zoobank_url = paste0("http://zoobank.org/",text_value)
    tt$add_triple(journal_id, has_identifier, zoobank_id)
    tt$add_triple(zoobank_id, rdf_type, ResourceIdentifier)
    tt$add_triple(zoobank_id, identifier_scheme, zoobank)
    tt$add_triple(zoobank_id, rdfs_label, journal_zoobank_literal)
    tt$add_triple(zoobank_id, has_url, literal(zoobank_url, xsd_type = xsd_uri))

      }
    }
  }


  tt$add_triple(journal_id, frbr_part, article_id)
  tt$add_triple(article_id, rdf_type, Article)


  articleTitle = atoms$title[[1]]
  articleTitle = escape_special(articleTitle$text_value)

  tt$add_triple(article_id, rdfs_label, literal(articleTitle))

  tt$add_triple(article_id, realization_of, paper_id)

  tt$add_triple(article_id, dc_title, literal(articleTitle))

  sapply(atoms$doi, function(i) {
    tt$add_triple(article_id, has_doi, i)
  })


  #the article zoobank id is the one containing the words "zoobank"
  if(length(unlist(atoms$article_zoobank)) > 0){
    for (n in 1:length(atoms$article_zoobank)){
      if (grepl("zoobank", unlist(atoms$article_zoobank[[n]])["text_value"]) == TRUE){
        text_value = gsub("^(.*):", "", unlist(atoms$article_zoobank[n])["text_value"])
        text_value = gsub(" ", "", text_value)

        ll = list(text_value = text_value, xsd_type = xsd_string, lang = "",
                  squote = paste0("\"", text_value, "\"", ""))
        class(ll) = "literal"
        article_zoobank_literal = ll

    article_zoobank_id = identifier(text_value, c(zoobank = "http://zoobank.org/"))
    tt$add_triple(article_id, has_identifier, article_zoobank_id)
    tt$add_triple(article_zoobank_id, rdf_type, ResourceIdentifier)
    tt$add_triple(article_zoobank_id, identifier_scheme, zoobank)
    tt$add_triple(article_zoobank_id, rdfs_label, article_zoobank_literal)
    tt$add_triple(article_zoobank_id, has_url, literal(strip_angle(article_zoobank_id$uri), xsd_type = xsd_uri))
      }
    }
  }


  if(length(unlist(atoms$plazi_id))> 0){
    for (n in 1:length(atoms$plazi_id)){
      text_value =  unlist(atoms$plazi_id[n])["text_value"]
      text_value = gsub(" ", "", text_value)
      text_value = gsub("http://tb.plazi.org/GgServer/summary/", "", text_value)
      ll = list(text_value = text_value, xsd_type = xsd_string, lang = "",
                squote = paste0("\"", text_value, "\"", ""))
      class(ll) = "literal"
      plazi_article_id_lit = ll
      plazi_article_id = identifier(text_value, c(plazi = "http://tb.plazi.org/GgServer/summary/"))
      plazi_url = paste0("http://tb.plazi.org/GgServer/summary/",text_value)

      tt$add_triple(article_id, has_identifier, plazi_article_id)
      tt$add_triple(plazi_article_id, rdf_type, ResourceIdentifier)
      tt$add_triple(plazi_article_id, identifier_scheme, plazi)
      tt$add_triple(plazi_article_id, rdfs_label, plazi_article_id_lit)
      tt$add_triple(plazi_article_id, has_url, literal(plazi_url, xsd_type = xsd_uri))

    }

  }

  sapply(atoms$publisher, function(i) {
    tt$add_triple(article_id, has_publisher, i)
  })
  sapply(atoms$date, function(i) {
    tt$add_triple(article_id, publication_date, i)
  })
  sapply(list(pub_date(atoms$pub_year, atoms$pub_month, atoms$pub_day)),
         function(i) {
           tt$add_triple(article_id, publication_date, i)
         })
  tt$add_triple(article_id, has_publisher_id, publisher_id)
  sapply(atoms$issue, function(i) {
    tt$add_triple(article_id, has_issue, i)
  })

  tt$add_triple(publisher_id, rdf_type, Publisher)
  sapply(atoms$publisher, function(i) {
    tt$add_triple(publisher_id, rdfs_label, i)
  })

  tt$add_triple(paper_id, rdf_type, Paper)


  sapply(atoms$keyword, function(i) {
    tt$add_triple(identifiers$nid, has_keyword, i)
  })

  return(tt)
}


#' Keyword Group Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
keyword_group = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{

  tt = ResourceDescriptionFramework$new()
  tt$add_triple(identifiers$nid, rdf_type, KeywordGroup)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
  sapply(atoms$keyword, function(i) {
    tt$add_triple(identifiers$nid, has_keyword, i)
  })
  return(tt)
}

#' Title Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
title = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{
  title_content = atoms$text_content[[1]]
  title_content = escape_special(title_content$text_value)


  tt = ResourceDescriptionFramework$new()
  tt$add_triple(identifiers$nid, rdf_type, Title)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)

  tt$add_triple(identifiers$nid, has_content, literal(title_content))

  return(tt)
}

#' Abstract Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
abstract = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{
  abstract_content = atoms$text_content[[1]]
  abstract_content = escape_special(abstract_content$text_value)


  #  trans_abstract = escape_special(atoms$trans_abstract)
  tt = ResourceDescriptionFramework$new()
  tt$add_triple(identifiers$nid, rdf_type, Abstract)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
  tt$add_triple(identifiers$nid, has_content, literal(abstract_content))

  if(length(unlist(atoms$trans_abstract)) > 0)
  {
    trans_content = atoms$trans_abstract[[1]]
    trans_content = escape_special(trans_content$text_value)
    tt$add_triple(identifiers$nid, has_content, literal(trans_content))

  }


  tt = bold_genbank_serializer(tt, atoms, identifiers)
  tt =  institution_serializer(tt, atoms, identifiers)


  return(tt)
}


#' Author constructor
#'
#' TODO rewrite the atoms as individual arguments. I would need a
#' do.call in the extractor for that.
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
author = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{

  full_name = function(lsurname, lgiven_name) {
    if (length(lsurname) == 1 && length(lgiven_name) == 1) {
      paste(lgiven_name[[1]]$text_value, lsurname[[1]]$text_value)
    }
    else if (length(lsurname) == 1) {
      lsurname[[1]]$text_value
    }
    else {
      NA
    }
  }

  atoms$full_name = ifelse(length(unlist(atoms$full_name)) == 0, list(literal(full_name(atoms$surname,
                                                                                        atoms$given_names), xsd_type = rdf4r::xsd_string)), atoms$full_name)
  aid = sapply(atoms$aff_id, function(a) {
    as.integer(gsub("[^0-9.]", "", a$text_value))
  })



  article_id = identifiers$root_id
  author_id = identifiers$nid
  paper_id = check_mongo_key_via_parent(parent = article_id$uri, type = "researchPaper", collection = general_collection)
  cat(paper_id, file="~/paper_id")

  paper_id = gsub("http://openbiodiv.net/", "", paper_id)
  paper_id = identifier(paper_id, prefix)

  tt = ResourceDescriptionFramework$new()


  tt$add_triple(paper_id, creator, author_id)
  tt$add_triple(author_id, rdf_type, Person)
  sapply(atoms$full_name, function(j) {
    tt$add_triple(author_id, rdfs_label, j)
  })

  if (length(aid)>0){
    sapply(atoms$all_affiliations[aid], function(j) {
      tt$add_triple(author_id, has_affiliation, j)
      })
  }


  sapply(atoms$email, function(j) {
    tt$add_triple(author_id, has_email, j)
  })


  if(length(unlist(atoms$orcid))>0){
    orcid_value = atoms$orcid[[1]]$text_value
    orcid_value = gsub(" ", "", orcid_value)
    orcid_value = gsub("^(.*)orcid.org\\/", "", orcid_value)
    orcid_id = identifier(orcid_value, c(orcid = "https://orcid.org/"))
  }else{
    orcid_id = NULL
  }


  #following the datacite ontology: http://www.sparontologies.net/ontologies/datacite
  tt$add_triple(author_id, has_identifier, orcid_id)

  tt$add_triple(orcid_id, rdf_type, PersonalIdentifier)
  tt$add_triple(orcid_id, identifier_scheme, orcid)
  tt$add_triple(orcid_id, rdfs_label, literal(orcid_value))
  tt$add_triple(orcid_id, has_url, literal(strip_angle(orcid$uri), xsd_type = xsd_uri))
  return(tt)
}



#' Introduction Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
introduction_section = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{


  intro_content = atoms$text_content[[1]]
  intro_content = escape_special(intro_content$text_value)

  tt = ResourceDescriptionFramework$new()
  tt$add_triple(identifiers$nid, rdf_type, Introduction)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)

  tt$add_triple(identifiers$nid, has_content, literal(intro_content))

  tt = bold_genbank_serializer(tt, atoms, identifiers)
  tt =  institution_serializer(tt, atoms, identifiers)

  return(tt)
}


#' @export
treatment = function (atoms, identifiers, prefix, new_taxons, mongo_key){

  treatment_id = identifiers$nid
  tt = ResourceDescriptionFramework$new()

  #get or set taxonomic concept id
  tc_df = set_component_frame(label = NA, mongo_key = NA, type = "taxonomicConcept", orcid = NA, parent = treatment_id$uri, key = NA)
  tc_identifier = get_or_set_mongoid(tc_df, prefix)
  tc_identifier = identifier(tc_identifier, prefix)

  tt$add_triple(treatment_id, rdf_type, Treatment)
  tt$add_triple(treatment_id, is_contained_by, identifiers$pid)
  if (length(unlist(atoms$status))>0 ){
    status = atoms$status[[1]]$text_value
    if (!(is.null(status)))
    {
      tt$add_triple(treatment_id, taxonStatus, literal(status))
      if (status %in% new_taxons ==TRUE)
        tt$add_triple(treatment_id, mentions, TaxonomicDiscovery)
    }
  }
  #for (a in 1:length(atoms$text_content)){
  #  atoms$text_content[[a]]$text_value = escape_special(atoms$text_content[[a]]$text_value)
  #}
  treatment_content = atoms$text_content[[1]]
  treatment_id_literal = treatment_id$id
  treatment_content = gsub(treatment_id_literal, "", treatment_content$text_value)
  treatment_content = escape_special(treatment_content)


  tt$add_triple(treatment_id, has_content, literal(treatment_content))

  tt$add_triple(tc_identifier, rdf_type, TaxonomicConcept)
  tt$add_triple(tc_identifier, realization, treatment_id)
  tt = bold_genbank_serializer(tt, atoms, identifiers)

  tt = institution_serializer(tt, atoms, identifiers)

  return(tt)
}


#' @export
nomenclature = function (atoms, identifiers, prefix, new_taxons, mongo_key){

  nomenclature_id = identifiers$nid #remove any ids from the text contents
  nomenclature_parent_id = identifiers$pid$id
  nomenclature_content = atoms$text_content[[1]]

  nomenclature_content = gsub(nomenclature_id, "", nomenclature_content$text_value)
  nomenclature_content = gsub(nomenclature_parent_id, "", nomenclature_content)

  nomenclature_content = escape_special(nomenclature_content)

  tt = ResourceDescriptionFramework$new()
  tt$add_triple(nomenclature_id, rdf_type, Nomenclature)
  tt$add_triple(nomenclature_id, is_contained_by, identifiers$pid)
  tt$add_triple(nomenclature_id, has_content, literal(nomenclature_content))
  tt = institution_serializer(tt, atoms, identifiers)
  return(tt)

}

#' @export
nomenclature_citations = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{

  tt = ResourceDescriptionFramework$new()
  citations = atoms$text_content[[1]]
  citations = escape_special(citations$text_value)

  tt$add_triple(identifiers$nid, rdf_type, NomenclatureCitationsList)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
  tt$add_triple(identifiers$nid, has_content, literal(citations))



  return(tt)
}



#' Diagnosis Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
diagnosis = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{

  diagnosis_id = identifiers$nid
  diagnosis_content = atoms$text_content[[1]]
  diagnosis_content = escape_special(diagnosis_content$text_value)
  tt = ResourceDescriptionFramework$new()
  tt$add_triple(diagnosis_id, rdf_type, Diagnosis)
  tt$add_triple(diagnosis_id, is_contained_by, identifiers$pid)
  tt$add_triple(diagnosis_id, has_content, literal(diagnosis_content))

  tt = bold_genbank_serializer(tt, atoms, identifiers)
  tt =  institution_serializer(tt, atoms, identifiers)

  return(tt)
}

#' Discussion Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
discussion = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{

  tt = ResourceDescriptionFramework$new()
  discussion_content = atoms$text_content[[1]]
  discussion_content = escape_special(discussion_content$text_value)
  tt$add_triple(identifiers$nid, rdf_type, Discussion)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
  tt$add_triple(identifiers$nid, has_content, literal(discussion_content))

  tt = bold_genbank_serializer(tt, atoms, identifiers)
  tt =  institution_serializer(tt, atoms, identifiers)

  return(tt)
}

#' @export
methods = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{

  tt = ResourceDescriptionFramework$new()
  methods_content = atoms$text_content[[1]]
  methods_content = escape_special(methods_content$text_value)
  tt$add_triple(identifiers$nid, rdf_type, Methods)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
  tt$add_triple(identifiers$nid, has_content, literal(methods_content))

  tt = bold_genbank_serializer(tt, atoms, identifiers)
  tt =  institution_serializer(tt, atoms, identifiers)

  return(tt)
}



#' Checklist Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
checklist = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{

  tt = ResourceDescriptionFramework$new()

  checklist_content = atoms$text_content[[1]]
  checklist_content = escape_special(checklist_content$text_value)
  tt$add_triple(identifiers$nid, rdf_type, Checklist)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
  tt$add_triple(identifiers$nid, has_content, literal(checklist_content))
  tt = bold_genbank_serializer(tt, atoms, identifiers)

  tt =  institution_serializer(tt, atoms, identifiers)


  return(tt)
}

#' Distribution Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
distribution = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{
  print(atoms)
  tt = ResourceDescriptionFramework$new()
  distribution_content = atoms$text_content[[1]]
  distribution_content = escape_special(distribution_content$text_value)
  tt$add_triple(identifiers$nid, rdf_type, Distribution)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
  tt$add_triple(identifiers$nid, has_content, literal(distribution_content))

  tt = bold_genbank_serializer(tt, atoms, identifiers)
  tt =  institution_serializer(tt, atoms, identifiers)

  return(tt)
}



#' @export
figure = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{

  tt = ResourceDescriptionFramework$new()



  tt$add_triple(identifiers$nid, rdf_type, Figure)
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
  if (length(unlist(atoms$caption))>0){
    fig_caption =  atoms$caption[[1]]
    fig_caption = escape_special(fig_caption$text_value)
    tt$add_triple(identifiers$nid, has_content, literal(fig_caption))

  }
  sapply(atoms$doi, function(i){
    tt$add_triple(identifiers$nid, has_doi, i)
  })

  sapply(atoms$download_link, function(i){
    tt$add_triple(identifiers$nid, has_link, i)
  })

  tt = bold_genbank_serializer(tt, atoms, identifiers)

  return(tt)
}

#' @export
treatment_en = function (atoms, identifiers, prefix, new_taxons, mongo_key){

  atoml_to_val = function(atoml)
  {
    if (length(atoml) > 0) {
      return(atoml[[1]]$text_value)
    }
    else {
      return (NA)
    }
  }




  tt = ResourceDescriptionFramework$new()


  #get the name:
  #escape special chars
  #atoms$text_content = double_quote_replacer(atoms$text_content)



  scName = get_scientific_name_or_tcl(kingdom = NA, phylum = NA, class = NA, order = NA,
                                      family = NA, subfamily = NA, genus = atoml_to_val(atoms$genus), subgenus = NA, species = atoml_to_val(atoms$species),
                                      subspecies = atoml_to_val(atoms$subspecies), authorship = atoml_to_val(atoms$authorship), secundum_literal = NA)




  scName_gbif = gbif_taxonomy_mapping(scName = scName, collection = checklistCol)
  if (is.null(scName_gbif))
    scNameParent = NA
  else{
    cat(scName_gbif$uri,file = "~/gbif_names.txt",append = TRUE )

    scNameParent = scName_gbif$uri

  }
  df = set_component_frame(label = scName, mongo_key = NA, type = "scientificName", orcid = NA, parent = scNameParent, key = NA)


  scNameID = get_or_set_mongoid(df, prefix)
  scName_ident = identifier(scNameID, prefix)

  #get or set taxonomic concept id
  tc_df = set_component_frame(label = NA, mongo_key = NA, type = "taxonomicConcept", orcid = NA, parent = treatment_id$uri, key = NA)
  tc_identifier = get_or_set_mongoid(tc_df, prefix)
  tc_identifier = identifier(tc_identifier, prefix)


  treatment_id = identifiers$nid
  tt$add_triple(treatment_id, rdf_type, Treatment)
  tt$add_triple(treatment_id, is_contained_by, identifiers$pid)


  tt$add_triple(treatment_id, mentions, scName_ident)
  tt$add_triple(scName_ident, rdf_type, ScientificName)



  tt$add_triple(scName_ident, has_gbifID, scName_gbif)
  tt$add_triple(scName_ident, rdfs_label, literal(scName))

  sapply(atoms$genus, function(i){
    tt$add_triple(scName_ident, dwc_genus, i)
  })

  sapply(atoms$species, function(i){
    tt$add_triple(scName_ident, dwc_species_ep, i)
  })

  sapply(atoms$subspecies, function(i){
    tt$add_triple(scName_ident, dwc_subspecies_ep, i)
  })

  sapply(atoms$authorship, function(i){
    tt$add_triple(scName_ident, dwc_authorship, i)
  })



  if (length(unlist(atoms$status))>0 ){
    status = atoms$status[[1]]$text_value
    tt$add_triple(treatment_id, taxonStatus, literal(status))
    if (status %in% new_taxons ==TRUE)
      tt$add_triple(treatment_id, mentions, TaxonomicDiscovery)
  }
  #for (a in 1:length(atoms$text_content)){
  #  atoms$text_content[[a]]$text_value = escape_special(atoms$text_content[[a]]$text_value)
  #}
  treatment_content = atoms$text_content[[1]]
  treatment_content = escape_special(treatment_content$text_value)
  tt$add_triple(treatment_id, has_content, literal(treatment_content))

  tt$add_triple(tc_identifier, rdf_type, TaxonomicConcept)
  tt$add_triple(tc_identifier, realization, treatment_id)

  tt = bold_genbank_serializer(tt, atoms, identifiers)
  tt = institution_serializer(tt, atoms, identifiers)

  return(tt)
}

#' @export
type_material = function (atoms, identifiers, prefix, new_taxons, mongo_key){
  #first check the status
  tt = ResourceDescriptionFramework$new()
  #get the name:
  #escape special chars

  typeMaterialID = identifiers$nid
  tt$add_triple(typeMaterialID, rdf_type, MaterialsExamined)
  tt$add_triple(typeMaterialID, is_contained_by, identifiers$pid)
  material_content = atoms$text_content[[1]]
  material_content = escape_special(material_content$text_value)
  tt$add_triple(typeMaterialID, has_content, literal(material_content))

  if (length(unlist(atoms$holotype))>0){
    sapply(atoms$holotype, function(n){
      #n$text_value = escape_special(n$text_value)
      #label = escape_special_json(n$text_value)
      df = set_component_frame(label = escape_special_json(n$text_value), mongo_key = NA, type = "holotype", orcid = NA, parent = identifiers$nid$uri, key = NA)
      holotypeID = get_or_set_mongoid(df, prefix)
      tt$add_triple(identifier(holotypeID, prefix), rdf_type, HolotypeDescription)
      tt$add_triple(identifier(holotypeID, prefix), is_contained_by, typeMaterialID)

      holotype_content = escape_special(n$text_value)
      tt$add_triple(identifier(holotypeID, prefix), has_content, literal(holotype_content))
    })
  }

tt = check_dwc_terms(tt, atoms, typeMaterialID)

#tt = check_dwc_occurrence(tt, atoms, typeMaterialID)
#tt = check_dwc_location(tt, atoms, typeMaterialID)

#tt = check_dwc_identification(tt, atoms, typeMaterialID)
#tt = check_dwc_event(tt, atoms, typeMaterialID)

tt = bold_genbank_serializer(tt, atoms, identifiers)
tt = institution_serializer(tt, atoms, identifiers)


}




#' @export
occurrence_list = function (atoms, identifiers, prefix, new_taxons, mongo_key){
  #first check the status
  tt = ResourceDescriptionFramework$new()
  #get the name:
  #escape special chars

  typeMaterialID = identifiers$pid
  #tt$add_triple(occurrenceList, rdf_type, OccurrenceList)

  tt = check_dwc_terms(tt, atoms, typeMaterialID)

  #tt = check_dwc_occurrence(tt, atoms, typeMaterialID)
  #tt = check_dwc_location(tt, atoms, typeMaterialID)
  #tt = check_dwc_identification(tt, atoms, typeMaterialID)
  #tt = check_dwc_event(tt, atoms, typeMaterialID)

  tt = bold_genbank_serializer(tt, atoms, identifiers)
  tt = institution_serializer(tt, atoms, identifiers)


return(tt)
}


#' @export
taxonomic_name_usage = function (atoms, identifiers, prefix, new_taxons, mongo_key){

  tnu_id = identifiers$nid
  tt = ResourceDescriptionFramework$new()


  atoml_to_val = function(atoml)
  {
    if (length(atoml) > 0) {
      return(atoml[[1]]$text_value)
    }
    else {
      return (NA)
    }
  }


  #get the name:
  #escape special chars
  #atoms$text_content = double_quote_replacer(atoms$text_content)



  scName = get_scientific_name_or_tcl(kingdom = atoml_to_val(atoms$kingdom), phylum = atoml_to_val(atoms$phylum), class = atoml_to_val(atoms$class), order = atoml_to_val(atoms$order),
                                      family = atoml_to_val(atoms$family), subfamily = atoml_to_val(atoms$subfamily), genus = atoml_to_val(atoms$genus), subgenus = atoml_to_val(atoms$subgenus), species = atoml_to_val(atoms$species),
                                      subspecies = atoml_to_val(atoms$subspecies), authorship = atoml_to_val(atoms$authorship), secundum_literal = NA)
  #if there is no genus, species, etc. just take the whole node content (e.g. <tp:taxon-name obkms_id="540d2809-0e18-4beb-9bf0-a1118d0a6d37" obkms_process="TRUE">Flueggea suffruticosa</tp:taxon-name>)

  if(scName==""){
    scName = atoms$text_content[[1]]$text_value
  }

  print(scName)

  if( !(is.null(scName))){
    if ( !(scName == "")){


      scName_df = set_component_frame(label = scName, mongo_key = NA, type = "scName", orcid = NA, parent = NA, key = NA)


      scNameID = get_or_set_mongoid(scName_df, prefix)
      scNameID = identifier(scNameID, prefix)



      #get or set scName identifier - mongoDB



      scName_gbif = gbif_taxonomy_mapping(scName = scName, collection = checklistCol)
      if (is.null(scName_gbif)){
        scNameParent = NA
      }
      else
      {
        scNameParent = scName_gbif$uri
        update_parent(key=scNameID$uri, parent = scNameParent, collection = general_collection)

      }
      #add the parent to mongo

      update_parent = function(key, parent,  collection = general_collection){
        query = sprintf("{\"%s\":\"%s\"}", "key", key)
        update = sprintf("{\"$set\":{\"%s\":\"%s\"}}", "parent", parent)

        collection$update(query = query, update = update)
      }




      tt$add_triple(tnu_id, rdf_type, TaxonomicNameUsage)
      tt$add_triple(tnu_id, is_contained_by, identifiers$pid)


      tt$add_triple(tnu_id, mentions, scNameID)
      tt$add_triple(scNameID, rdf_type, ScientificName)

      tt$add_triple(scNameID, has_gbifID, scName_gbif)
      tt$add_triple(scNameID, rdfs_label, literal(scName))

      sapply(atoms$kingdom, function(i) {
        tt$add_triple(scNameID, dwc_kingdom, i)
      })
      sapply(atoms$phylum, function(i) {
        tt$add_triple(scNameID, dwc_phylum, i)
      })
      sapply(atoms$class, function(i) {
        tt$add_triple(scNameID, dwc_class, i)
      })
      sapply(atoms$order, function(i) {
        tt$add_triple(scNameID, dwc_order, i)
      })
      sapply(atoms$family, function(i) {
        tt$add_triple(scNameID, dwc_family, i)
      })
      sapply(atoms$subfamily, function(i) {
        tt$add_triple(scNameID, dwc_family, i)
      })
      sapply(atoms$genus, function(i) {
        tt$add_triple(scNameID, dwc_genus, i)
      })
      sapply(atoms$subgenus, function(i) {
        tt$add_triple(scNameID, dwc_subgenus, i)
      })
      sapply(atoms$species, function(i) {
        tt$add_triple(scNameID, dwc_species_ep, i)
      })
      sapply(atoms$subspecies, function(i) {
        tt$add_triple(scNameID, dwc_subspecies_ep, i)
      })
      sapply(atoms$verbatim_rank, function(i) {
        tt$add_triple(scNameID, has_verbatim_rank, i)
      })
      sapply(atoms$taxonomic_rank, function(i) {
        tt$add_triple(scNameID, has_taxonomic_rank_id, i)
      })
      sapply(atoms$authorship, function(i) {
        tt$add_triple(scNameID, dwc_authorship, i)
      })


      if (length(unlist(atoms$status))>0 ){
        status = atoms$status[[1]]$text_value
        tt$add_triple(scNameID, taxonStatus, literal(status))
      }
    }
  }


  return(tt)
}

#' Taxonomic Key Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param prefix
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
taxonomic_key = function (atoms, identifiers, prefix, new_taxons, mongo_key)
{

  tt = ResourceDescriptionFramework$new()
  tt$add_triple(identifiers$nid, rdf_type, TaxonomicKey)
  title = atoms$title[[1]]
  tt$add_triple(identifiers$nid, dc_title, title)


  if (length(unlist(atoms$text_content))>0){
    table_content = atoms$text_content[[1]]

    table_content = escape_special(table_content$text_value)

    tt$add_triple(identifiers$nid, has_content, literal(table_content))
  }


  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
  tt = bold_genbank_serializer(tt, atoms, identifiers)




  return(tt)
}

#' @export
materials_examined = function(){

}

#' @export
institution_code_usage = function(atoms, identifiers, prefix, new_taxons, mongo_key)
{

  tt = ResourceDescriptionFramework$new()
  tt$add_triple(identifiers$nid, rdf_type, InstitutionalCodeUsage) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment

  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, institutional_code, i) })

  # TODO add Institions as resources
  return(tt)
}

#' @export
metadata_en = function (atoms, identifiers, prefix,new_taxons, mongo_key)
{
  pub_date = function(year, month, day) {
    literal(paste0(text_value = unlist(year)["text_value"],
                   "-", unlist(month)["text_value"], "-", unlist(day)["text_value"]),
            xsd_type = rdf4r::xsd_date)
  }
  if (length(unlist(atoms$pensoft_pub)) > 0) {
    stop("Pensoft publication")
  }
  general_collection = mongolite::mongo("new_collection")
  doi = unlist(atoms$doi)["text_value"]

  article_id = identifiers$root_id
  publisher_lit = toString(unlist(atoms$publisher)["text_value"])

  df = set_component_frame(label = publisher_lit, mongo_key = c(publisher = NA), type = "publisher", orcid = NA, parent = NA, key = NA)
  publisher_id = get_or_set_mongoid(df, prefix )
  publisher_id = identifier(publisher_id, prefix)

  journal_lit = toString(unlist(atoms$journal)["text_value"])

  df = set_component_frame(label = journal_lit, mongo_key = c(journal = NA), type = "journal", orcid = NA, parent = NA, key = NA)
  journal_id = get_or_set_mongoid(df, prefix )
  journal_id = identifier(journal_id, prefix)

  paper_label = unlist(atoms$title)["text_value"]

  research_paper_df = set_component_frame(label = paper_label, mongo_key = NA, type = "researchPaper", orcid = NA, parent = article_id$uri, key = NA)

  paper_id = get_or_set_mongoid(research_paper_df, prefix)
  paper_id = identifier(paper_id, prefix)


  tt = ResourceDescriptionFramework$new()


  tt$add_triple(journal_id, rdf_type, Journal)
  sapply(atoms$journal, function(j) {
    tt$add_triple(journal_id, pref_label, j)
  })
  sapply(atoms$journal_abbrev, function(j) {
    tt$add_triple(journal_id, alt_label, j)
  })
  sapply(atoms$issn, function(i) {
    tt$add_triple(journal_id, issn, i)
  })
  sapply(atoms$eIssn, function(i) {
    tt$add_triple(journal_id, eissn, i)
  })

  if(length(atoms$journal_zoobank)>0){
    for (n in 1:length(atoms$journal_zoobank)){
      if (grepl("zoobank", unlist(atoms$journal_zoobank[[n]])["text_value"]) == TRUE){
        text_value = gsub("^(.*):", "", unlist(atoms$journal_zoobank[n])["text_value"])
        text_value = gsub(" ", "", text_value)

        ll = list(text_value = text_value, xsd_type = xsd_string, lang = "",
                  squote = paste0("\"", text_value, "\"", ""))
        class(ll) = "literal"
        journal_zoobank_literal = ll
      }
    }

    zoobank_id = identifier(text_value, c(zoobank = "http://zoobank.org/"))
    tt$add_triple(journal_id, has_identifier, zoobank_id)
    tt$add_triple(zoobank_id, rdf_type, ResourceIdentifier)
    tt$add_triple(zoobank_id, identifier_scheme, zoobank)
    tt$add_triple(zoobank_id, rdfs_label, journal_zoobank_literal)
    tt$add_triple(zoobank_id, has_url, literal(strip_angle(zoobank_id$uri), xsd_type = xsd_uri))

  }


  tt$add_triple(journal_id, frbr_part, article_id)
  tt$add_triple(article_id, rdf_type, Article)


  articleTitle = atoms$title[[1]]
  articleTitle = escape_special(articleTitle$text_value)

  tt$add_triple(article_id, rdfs_label, literal(articleTitle))

  tt$add_triple(article_id, realization_of, paper_id)


  sapply(atoms$title, function(i) {
    tt$add_triple(article_id, dc_title, i)
  })
  sapply(atoms$doi, function(i) {
    tt$add_triple(article_id, has_doi, i)
  })


  #the article zoobank id is the one containing the words "zoobank"
  if(length(atoms$article_zoobank) > 0){
    for (n in 1:length(atoms$article_zoobank)){
      if (grepl("zoobank", unlist(atoms$article_zoobank[[n]])["text_value"]) == TRUE){
        text_value = gsub("^(.*):", "", unlist(atoms$article_zoobank[n])["text_value"])
        text_value = gsub(" ", "", text_value)

        ll = list(text_value = text_value, xsd_type = xsd_string, lang = "",
                  squote = paste0("\"", text_value, "\"", ""))
        class(ll) = "literal"
        article_zoobank_literal = ll

    article_zoobank_id = identifier(text_value, c(zoobank = "http://zoobank.org/"))
    tt$add_triple(article_id, has_identifier, article_zoobank_id)
    tt$add_triple(article_zoobank_id, rdf_type, ResourceIdentifier)
    tt$add_triple(article_zoobank_id, identifier_scheme, zoobank)
    tt$add_triple(article_zoobank_id, rdfs_label, article_zoobank_literal)
    tt$add_triple(article_zoobank_id, has_url, literal(article_zoobank_id, xsd_type = xsd_uri))
      }
    }

  }




  if(length(atoms$plazi_id) > 0){
    for (n in 1:length(atoms$plazi_id)){
      text_value =  unlist(atoms$plazi_id[n])["text_value"]
      text_value = gsub(" ", "", text_value)
      text_value = gsub("http://tb.plazi.org/GgServer/summary/", "", text_value)
      ll = list(text_value = text_value, xsd_type = xsd_string, lang = "",
                squote = paste0("\"", text_value, "\"", ""))
      class(ll) = "literal"
      plazi_article_id_lit = ll
      plazi_url = paste0("http://tb.plazi.org/GgServer/summary/",text_value)


    plazi_article_id = identifier(text_value, c(plazi = "http://tb.plazi.org/GgServer/summary/"))
    tt$add_triple(article_id, has_identifier, plazi_article_id)
    tt$add_triple(plazi_article_id, rdf_type, ResourceIdentifier)
    tt$add_triple(plazi_article_id, identifier_scheme, plazi)
    tt$add_triple(plazi_article_id, rdfs_label, plazi_article_id_lit)
    tt$add_triple(plazi_article_id, has_url, literal(plazi_url, xsd_type = xsd_uri))
  }
  }

  sapply(atoms$publisher, function(i) {
    tt$add_triple(article_id, has_publisher, i)
  })
  sapply(atoms$date, function(i) {
    tt$add_triple(article_id, publication_date, i)
  })
  sapply(list(pub_date(atoms$pub_year, atoms$pub_month, atoms$pub_day)),
         function(i) {
           tt$add_triple(article_id, publication_date, i)
         })
  tt$add_triple(article_id, has_publisher_id, publisher_id)
  sapply(atoms$issue, function(i) {
    tt$add_triple(article_id, has_issue, i)
  })

  tt$add_triple(publisher_id, rdf_type, Publisher)
  sapply(atoms$publisher, function(i) {
    tt$add_triple(publisher_id, rdfs_label, i)
  })

  tt$add_triple(paper_id, rdf_type, Paper)


  sapply(atoms$keyword, function(i) {
    tt$add_triple(identifiers$nid, has_keyword, i)
  })

  return(tt)
}

#' @export
tnu = function (atoms, identifiers, prefix, new_taxons, mongo_key){

  tnu_id = identifiers$nid
  tt = ResourceDescriptionFramework$new()


  atoml_to_val = function(atoml)
  {
    if (length(atoml) > 0) {
      return(atoml[[1]]$text_value)
    }
    else {
      return (NA)
    }
  }


  #get the name:
  #escape special chars
  #atoms$text_content = double_quote_replacer(atoms$text_content)



  scName = get_scientific_name_or_tcl(kingdom = atoml_to_val(atoms$kingdom), phylum = atoml_to_val(atoms$phylum), class = atoml_to_val(atoms$class), order = atoml_to_val(atoms$order),
                                      family = atoml_to_val(atoms$family), subfamily = atoml_to_val(atoms$subfamily), genus = atoml_to_val(atoms$genus), subgenus = atoml_to_val(atoms$subgenus), species = atoml_to_val(atoms$species),
                                      subspecies = atoml_to_val(atoms$subspecies), authorship = atoml_to_val(atoms$authorship), secundum_literal = NA)
  #if there is no genus, species, etc. just take the whole node content (e.g. <tp:taxon-name obkms_id="540d2809-0e18-4beb-9bf0-a1118d0a6d37" obkms_process="TRUE">Flueggea suffruticosa</tp:taxon-name>)

  if(scName==""){
    scName = atoms$text_content[[1]]$text_value
  }

  print(scName)

  if( !(is.null(scName))){
    if ( !(scName == "")){


      scName_df = set_component_frame(label = scName, mongo_key = NA, type = "scName", orcid = NA, parent = NA, key = NA)


      scNameID = get_or_set_mongoid(scName_df, prefix)
      scNameID = identifier(scNameID, prefix)



      #get or set scName identifier - mongoDB



      scName_gbif = gbif_taxonomy_mapping(scName = scName, collection = checklistCol)
      if (is.null(scName_gbif)){
        scNameParent = NA
      }
      else
      {
        scNameParent = scName_gbif$uri
        update_parent(key=scNameID$uri, parent = scNameParent, collection = general_collection)

      }
      #add the parent to mongo

      update_parent = function(key, parent,  collection = general_collection){
        query = sprintf("{\"%s\":\"%s\"}", "key", key)
        update = sprintf("{\"$set\":{\"%s\":\"%s\"}}", "parent", parent)

        collection$update(query = query, update = update)
      }




      tt$add_triple(tnu_id, rdf_type, TaxonomicNameUsage)
      tt$add_triple(tnu_id, is_contained_by, identifiers$pid)


      tt$add_triple(tnu_id, mentions, scNameID)
      tt$add_triple(scNameID, rdf_type, ScientificName)

      tt$add_triple(scNameID, has_gbifID, scName_gbif)
      tt$add_triple(scNameID, rdfs_label, literal(scName))

      sapply(atoms$kingdom, function(i) {
        tt$add_triple(scNameID, dwc_kingdom, i)
      })
      sapply(atoms$phylum, function(i) {
        tt$add_triple(scNameID, dwc_phylum, i)
      })
      sapply(atoms$class, function(i) {
        tt$add_triple(scNameID, dwc_class, i)
      })
      sapply(atoms$order, function(i) {
        tt$add_triple(scNameID, dwc_order, i)
      })
      sapply(atoms$family, function(i) {
        tt$add_triple(scNameID, dwc_family, i)
      })
      sapply(atoms$subfamily, function(i) {
        tt$add_triple(scNameID, dwc_family, i)
      })
      sapply(atoms$genus, function(i) {
        tt$add_triple(scNameID, dwc_genus, i)
      })
      sapply(atoms$subgenus, function(i) {
        tt$add_triple(scNameID, dwc_subgenus, i)
      })
      sapply(atoms$species, function(i) {
        tt$add_triple(scNameID, dwc_species_ep, i)
      })
      sapply(atoms$subspecies, function(i) {
        tt$add_triple(scNameID, dwc_subspecies_ep, i)
      })
      sapply(atoms$verbatim_rank, function(i) {
        tt$add_triple(scNameID, has_verbatim_rank, i)
      })
      sapply(atoms$taxonomic_rank, function(i) {
        tt$add_triple(scNameID, has_taxonomic_rank_id, i)
      })
      sapply(atoms$authorship, function(i) {
        tt$add_triple(scNameID, dwc_authorship, i)
      })


      if (length(atoms$status)>0 ){
        status = atoms$status[[1]]$text_value
        tt$add_triple(scNameID, taxonStatus, literal(status))
      }
    }
  }


  return(tt)
}

#' @export
treatment_en = function (atoms, identifiers, prefix, new_taxons, mongo_key){

  atoml_to_val = function(atoml)
  {
    if (length(atoml) > 0) {
      return(atoml[[1]]$text_value)
    }
    else {
      return (NA)
    }
  }

  tt = ResourceDescriptionFramework$new()


  #get the name:
  #escape special chars
  #atoms$text_content = double_quote_replacer(atoms$text_content)



  scName = get_scientific_name_or_tcl(kingdom = NA, phylum = NA, class = NA, order = NA,
                                      family = NA, subfamily = NA, genus = atoml_to_val(atoms$genus), subgenus = NA, species = atoml_to_val(atoms$species),
                                      subspecies = atoml_to_val(atoms$subspecies), authorship = atoml_to_val(atoms$authorship), secundum_literal = NA)




  scName_gbif = gbif_taxonomy_mapping(scName = scName, collection = checklistCol)
  if (is.null(scName_gbif))
    scNameParent = NA
  else{
    cat(scName_gbif$uri,file = "~/gbif_names.txt",append = TRUE )

    scNameParent = scName_gbif$uri

  }
  df = set_component_frame(label = scName, mongo_key = NA, type = "scientificName", orcid = NA, parent = scNameParent, key = NA)


  scNameID = get_or_set_mongoid(df, prefix)
  scName_ident = identifier(scNameID, prefix)

  #get or set taxonomic concept id
  tc_df = set_component_frame(label = NA, mongo_key = NA, type = "taxonomicConcept", orcid = NA, parent = treatment_id$uri, key = NA)
  tc_identifier = get_or_set_mongoid(tc_df, prefix)
  tc_identifier = identifier(tc_identifier, prefix)


  treatment_id = identifiers$nid
  tt$add_triple(treatment_id, rdf_type, Treatment)
  tt$add_triple(treatment_id, is_contained_by, identifiers$pid)


  tt$add_triple(treatment_id, mentions, scName_ident)
  tt$add_triple(scName_ident, rdf_type, ScientificName)



  tt$add_triple(scName_ident, has_gbifID, scName_gbif)
  tt$add_triple(scName_ident, rdfs_label, literal(scName))

  sapply(atoms$genus, function(i){
    tt$add_triple(scName_ident, dwc_genus, i)
  })

  sapply(atoms$species, function(i){
    tt$add_triple(scName_ident, dwc_species_ep, i)
  })

  sapply(atoms$subspecies, function(i){
    tt$add_triple(scName_ident, dwc_subspecies_ep, i)
  })

  sapply(atoms$authorship, function(i){
    tt$add_triple(scName_ident, dwc_authorship, i)
  })



  if (length(atoms$status)>0 ){
    status = atoms$status[[1]]$text_value
    tt$add_triple(treatment_id, taxonStatus, literal(status))
    if (status %in% new_taxons ==TRUE)
      tt$add_triple(treatment_id, mentions, TaxonomicDiscovery)
  }
  #for (a in 1:length(atoms$text_content)){
  #  atoms$text_content[[a]]$text_value = escape_special(atoms$text_content[[a]]$text_value)
  #}
  treatment_content = atoms$text_content[[1]]
  treatment_content = escape_special(treatment_content$text_value)
  tt$add_triple(treatment_id, has_content, literal(treatment_content))

  tt$add_triple(tc_identifier, rdf_type, TaxonomicConcept)
  tt$add_triple(tc_identifier, realization, treatment_id)

  tt = bold_genbank_serializer(tt, atoms, identifiers)
  tt = institution_serializer(tt, atoms, identifiers)

  return(tt)
}

###############################3
#dwc checks

#' @export
check_dwc_terms = function(tt, atoms, typeMaterialID){

  #Occurrence
  if (length(atoms$catalog_number)>0 || length(atoms$other_catalog_numbers)>0 ||   length(atoms$record_number)>0  || length(atoms$recorded_by)>0 || length(atoms$individual_count)>0 || length(atoms$sex)>0 || length(atoms$life_stage)>0 ){
    occurrence_content_label = escape_special(atoms$text_content[[1]]$text_value)
    occurrence_df = set_component_frame(label = paste0("Occurrence: ", occurrence_content_label), mongo_key = NA, type = "occurrence", orcid = NA, parent = typeMaterialID$uri, key = NA)
    occurrenceID = identifier(get_or_set_mongoid(occurrence_df, prefix), prefix)
  }

  #Location
  if (length(atoms$coordinates)>0 || length(atoms$verbatim_lat)>0 ||   length(atoms$verbatim_long)>0  || length(atoms$decimal_long)>0 || length(atoms$decimal_lat)>0 || length(atoms$country)>0 || length(atoms$state_province)>0 || length(atoms$decimal_lat)>0 || length(atoms$country)>0 || length(atoms$locality)>0 || length(atoms$elevation)>0 || length(atoms$depth)>0 || length(atoms$water_body)>0){
    occurrence_content_label = escape_special(atoms$text_content[[1]]$text_value)
    location_df = set_component_frame(label = paste0("Location: ", occurrence_content_label), mongo_key = NA, type = "location", orcid = NA, parent = typeMaterialID$uri, key = NA)
    locationID = identifier(get_or_set_mongoid(location_df, prefix), prefix)
  }

  #Event
  if (length(atoms$collection_year)>0 || length(atoms$collection_month)>0 || length(atoms$collection_day)>0 || length(atoms$event_date)>0 || length(atoms$collection_date)>0 || length(atoms$samplingProtocol)>0 || length(atoms$habitat) > 0){
    occurrence_content_label = escape_special(atoms$text_content[[1]]$text_value)
    event_df = set_component_frame(label = paste0("Event: ", occurrence_content_label), mongo_key = NA, type = "event", orcid = NA, parent = typeMaterialID$uri, key = NA)
    eventID = identifier(get_or_set_mongoid(event_df, prefix), prefix)
  }

  #Identification
  if (length(atoms$identified_by)>0){
    occurrence_content_label = escape_special(atoms$text_content[[1]]$text_value)
    identification_df = set_component_frame(label = paste0("Identification: ", occurrence_content_label), mongo_key = NA, type = "identification", orcid = NA, parent = typeMaterialID$uri, key = NA)
    identificationID = identifier(get_or_set_mongoid(identification_df, prefix), prefix)
  }

  tt = process_dwc_occurrence(tt, atoms, typeMaterialID, occurrenceID, eventID, locationID, identificationID)
  tt = process_dwc_event(tt, atoms, typeMaterialID, occurrenceID, eventID, locationID, identificationID)
  tt = process_dwc_location(tt, atoms, typeMaterialID, occurrenceID, eventID, locationID, identificationID)
  tt = process_dwc_identification(tt, atoms, typeMaterialID, occurrenceID, eventID, locationID, identificationID)

return(tt)


}





#' @export
process_dwc_occurrence = function(tt, atoms, typeMaterialID, occurrenceID, eventID, locationID, identificationID){

    #Occurrence
    tt$add_triple(typeMaterialID, dwc_occurrence_id, occurrenceID)
    tt$add_triple(occurrenceID, rdf_type, Occurrence)
    tt$add_triple(occurrenceID, relation, eventID)
    tt$add_triple(occurrenceID, relation, locationID)
    tt$add_triple(occurrenceID, relation, identificationID)

    sapply(atoms$record_number, function(n){
      tt$add_triple(occurrenceID, dwc_record_number, n)
    })

    sapply(atoms$recorded_by, function(n){
      tt$add_triple(occurrenceID, dwc_recorded_by, n)
    })

    sapply(atoms$catalog_number, function(n){
      tt$add_triple(occurrenceID, dwc_catalog_number, n)
    })

    sapply(atoms$other_catalog_numbers, function(n){
      tt$add_triple(occurrenceID, dwc_other_catalog_numbers, n)
    })

    sapply(atoms$individual_count, function(n){
      tt$add_triple(occurrenceID, dwc_individual_count, n)
    })

    sapply(atoms$sex, function(n){
      tt$add_triple(occurrenceID, dwc_sex, n)
    })

    sapply(atoms$life_stage, function(n){
      tt$add_triple(occurrenceID, dwc_life_stage, n)
    })


  return(tt)
}


#' @export
process_dwc_location = function(tt, atoms, typeMaterialID){

    #Location
    tt$add_triple(typeMaterialID, dwc_location_id, locationID)
    tt$add_triple(locationID, rdf_type, Location)
    tt$add_triple(locationID, relation, occurrenceID)
    tt$add_triple(locationID, relation, eventID)
    tt$add_triple(locationID, relation, identificationID)

    verbatim_coord = function(lat, long) {
      if (length(lat) == 1 && length(long) == 1) {
        paste0(lat[[1]]$text_value, ", ", long[[1]]$text_value)
      }
      else {
        NA
      }
    }

    atoms$coordinates  = ifelse(length(unlist(atoms$coordinates)) == 0, list(literal(verbatim_coord(atoms$verbatim_lat,
                                                                                                    atoms$verbatim_long), xsd_type = rdf4r::xsd_string)), atoms$coordinates)

    sapply(atoms$coordinates , function(n){
      tt$add_triple(locationID, dwc_coordinates, n)
    })

    sapply(atoms$decimal_long , function(n){
      tt$add_triple(locationID, dwc_decimal_long, n)
    })

    sapply(atoms$decimal_lat , function(n){
      tt$add_triple(locationID, dwc_decimal_lat, n)
    })


    sapply(atoms$country, function(n){
      tt$add_triple(locationID, dwc_country, n)
    })

    sapply(atoms$state_province, function(n){
      tt$add_triple(locationID, dwc_state_province, n)
    })

    sapply(atoms$locality, function(n){
      tt$add_triple(locationID, dwc_locality, n)
    })


    sapply(atoms$elevation, function(n){
      tt$add_triple(locationID, dwc_elevation, n)
    })

    sapply(atoms$depth, function(n){
      tt$add_triple(locationID, dwc_depth, n)
    })

    sapply(atoms$water_body, function(n){
      tt$add_triple(locationID, dwc_water_body, n)
    })


  return(tt)
}


#' @export
process_dwc_identification = function(tt, atoms, typeMaterialID){
  tt$add_triple(typeMaterialID, dwc_identification_id, identificationID)
  tt$add_triple(identificationID, rdf_type, Identification)
  tt$add_triple(identificationID, relation, occurrenceID)
  tt$add_triple(identificationID, relation, eventID)
  tt$add_triple(identificationID, relation, locationID)


  sapply(atoms$identified_by, function(n){
    tt$add_triple(identificationID, dwc_identified_by, n)
  })

return(tt)

}

#' @export
process_dwc_event = function(tt, atoms, typeMaterialID){
     tt$add_triple(typeMaterialID, dwc_event_id, eventID)
     tt$add_triple(eventID, rdf_type, Event)
     tt$add_triple(eventID, relation, occurrenceID)
     tt$add_triple(eventID, relation, identificationID)
     tt$add_triple(eventID, relation, locationID)

     collection_date = function(year, month, day) {
       if (length(year) == 1 && length(month) == 1 && length(day) == 1) {
         paste0(year[[1]]$text_value, "-", month[[1]]$text_value, "-", day[[1]]$text_value)
       }
       else if (length(year) == 1) {
         year[[1]]$text_value
       }
       else {
         NA
       }
     }

     if (length(atoms$collection_month)>0){
       for (n in 1:length(atoms$collection_month)){
         collection_month_text = atoms$collection_month[[n]]$text_value
         if (nchar(collection_month_text)<2){
           collection_month_text = paste0("0", collection_month_text)
         }
         atoms$collection_month[[n]]=literal(collection_month_text)
       }
     }

     atoms$collection_date = ifelse(length(unlist(atoms$collection_date)) == 0, list(literal(collection_date(atoms$collection_year,
                                                                                                             atoms$collection_month, atoms$collection_day), xsd_type = rdf4r::xsd_date)), atoms$collection_date)


     sapply(atoms$collection_date, function(n){
       tt$add_triple(eventID, dwc_event_date, n)
     })

     sapply(atoms$event_date, function(n){
       tt$add_triple(eventID, dwc_event_date, n)
     })

     sapply(atoms$collection_year, function(n){
       tt$add_triple(eventID, dwc_collection_year, n)
     })

     sapply(atoms$collection_month, function(n){
       tt$add_triple(eventID, dwc_collection_month, n)
     })

     sapply(atoms$collection_day, function(n){
       tt$add_triple(eventID, dwc_collection_day, n)
     })

     sapply(atoms$sampling_protocol, function(n){
       tt$add_triple(eventID, dwc_sampling_protocol, n)
     })

     sapply(atoms$habitat, function(n){
       tt$add_triple(eventID, dwc_habitat, n)
     })

   return(tt)
 }
