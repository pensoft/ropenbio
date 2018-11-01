#' Metadata constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
metadata = function(atoms, identifiers, access_options)
{
  pub_date = function(year, month, day)
    {
    literal(
      paste0(text_value = unlist(year)["text_value"], "-", unlist(month)["text_value"],"-", unlist(day)["text_value"]),
    xsd_type = rdf4r::xsd_date)
  }

  if (length(atoms$pensoft_pub) > 0) {
    stop("Pensoft publication")
  }

  lookup_paper_id = query_factory(
    p_query = qlookup_paper_id,
    access_options = access_options
  )

  openbiodiv_paper_id = identifier_factory(
    fun = list(lookup_paper_id),
    prefixes = access_options$prefix,
    def_prefix = access_options$prefix["openbiodiv"]
  )

  lookup_by_label = query_factory(
    p_query = qlookup_by_label,
    access_options = access_options
  )

  opebiodiv_id_via_label_lookup = identifier_factory(
    fun = list(lookup_by_label),
    prefixes = access_options$prefix,
    def_prefix = access_options$prefix["openbiodiv"]
  )


  # Identifiers
  article_id = identifiers$root_id
  paper_id = openbiodiv_paper_id(list(article_id))
  publisher_id = opebiodiv_id_via_label_lookup(atoms$publisher)
  journal_id = opebiodiv_id_via_label_lookup(atoms$journal)

  ## USE CROSS from PURRR for more complex lookups

  tt = ResourceDescriptionFramework$new()
  # Journal
  tt$add_triple(journal_id, rdf_type, Journal) # type
  sapply(atoms$journal, function(j) { tt$add_triple(journal_id, pref_label, j) }) # journal long
  sapply(atoms$journal_abbrev, function(j) { tt$add_triple(journal_id, alt_label, j) }) #journal short
  sapply(atoms$issn, function(i) { tt$add_triple(journal_id, issn, i) }) # issn
  sapply(atoms$eIssn, function(i) { tt$add_triple(journal_id, eissn, i) }) # eissn
  tt$add_triple(journal_id, frbr_part, article_id) # part
  # Article
  tt$add_triple(article_id, rdf_type, Article)
  sapply(atoms$title, function(i) { tt$add_triple(article_id, rdfs_label, i) }) # title and label
  sapply(atoms$title, function(i) { tt$add_triple(article_id, dc_title, i) })
  sapply(atoms$doi, function(i) { tt$add_triple(article_id, has_doi, i) })
  sapply(atoms$publisher, function(i) { tt$add_triple(article_id, has_publisher, i) })
  sapply(atoms$date, function(i) { tt$add_triple(article_id, publication_date, i) })

  sapply(list(pub_date(atoms$pub_year, atoms$pub_month, atoms$pub_day)), function(i) { tt$add_triple(article_id, publication_date, i) })

  tt$add_triple(article_id, has_publisher_id, publisher_id)
  tt$add_triple(article_id, realization_of, paper_id)

  sapply(atoms$issue, function(i) { tt$add_triple(article_id, has_issue, i) })
  sapply(atoms$keyword, function(i) { tt$add_triple(identifiers$nid, has_keyword, i) })

  # Publisher
  tt$add_triple(publisher_id, rdf_type, Publisher)
  sapply(atoms$publisher, function(i) { tt$add_triple(publisher_id, rdfs_label, i) })

  # Paper
  tt$add_triple(paper_id, rdf_type, Paper)

  return(tt)
}








#' Keyword Group Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
keyword_group = function(atoms, identifiers, access_options)
{
  tt = ResourceDescriptionFramework$new()
  # Title
  tt$add_triple(identifiers$nid, rdf_type, KeywordGroup) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$keyword, function(i) { tt$add_triple(identifiers$nid, has_keyword, i) })

  return(tt)
}








#' Title Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
title = function(atoms, identifiers, access_options)
{
  tt = ResourceDescriptionFramework$new()
  # Title
  tt$add_triple(identifiers$nid, rdf_type, Title) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

  return(tt)
}






#' Abstract Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
abstract = function(atoms, identifiers, access_options)
{
  tt = ResourceDescriptionFramework$new()
  # Abstract
  tt$add_triple(identifiers$nid, rdf_type, Abstract) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })
  sapply(atoms$trans_abstract, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

  return(tt)
}










#' Author constructor
#'
#' TODO rewrite the atoms as individual arguments. I would need a
#' do.call in the extractor for that.
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
author = function(atoms, identifiers, access_options)
{

  full_name = function(lsurname, lgiven_name)
  {
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
  atoms$full_name = ifelse(length(atoms$full_name) == 0, list(literal(full_name(atoms$surname, atoms$given_names), xsd_type = rdf4r::xsd_string)), atoms$full_name)

  aid = sapply(atoms$aff_id, function(a)
    {
    as.integer(gsub("[^0-9.]", "", a$text_value))
  })

  if (is.list(aid)) aid = NULL
  openbiodiv_paper_id = identifier_factory(
    fun = list(
      query_factory(
        p_query = qlookup_paper_id,
        access_options = access_options
      )
    ),
    prefixes = access_options$prefix,
    def_prefix = access_options$prefix["openbiodiv"]
  )

  openbiodiv_author_id = identifier_factory(
    fun = list(
      query_factory(
        p_query = qlookup_by_label_in_context,
        access_options = access_options
      ),
      query_factory(
        p_query = qlookup_by_label_no_context,
        access_options = access_options
      )
    ),
    prefixes = access_options$prefix,
    def_prefix = access_options$prefix["openbiodiv"]
  )

  #  opebiodiv_id_via_label = identifier_factory(
  #    fun = list(
  #      query_factory(
  #        p_query = qlookup_by_label,
  #        access_options = access_options
  #      )
  #    ),
  #    prefixes = access_options$prefix,
  #    def_prefix = access_options$prefix["openbiodiv"]
  #  )

  # Identifiers
  article_id = identifiers$pid
  paper_id = openbiodiv_paper_id(list(article_id))
  author_id = openbiodiv_author_id(purrr::cross(list(list(article_id), atoms$full_name)))

  tt = ResourceDescriptionFramework$new()
  # Paper
  tt$add_triple(paper_id, creator, author_id)

  # Author
  tt$add_triple(author_id, rdf_type, Person)
  sapply(atoms$full_name, function(j) { tt$add_triple(author_id, rdfs_label, j) }) # journal long
  sapply(atoms$all_affiliations[aid], function(j) { tt$add_triple(author_id, has_affiliation, j) }) # journal long
  sapply(atoms$email, function(j) { tt$add_triple(author_id, has_email, j) }) # journal long

  return(tt)
}



#' Introduction Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
introduction_section = function(atoms, identifiers, access_options)
{
  tt = ResourceDescriptionFramework$new()
  # Introduction
  tt$add_triple(identifiers$nid, rdf_type, Introduction) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

  return(tt)
}




#' Treatment Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
treatment = function(atoms, identifiers, access_options)
{
  # identifiers
  lookup_taxonomic_concept = query_factory(
    p_query = qlookup_taxonomic_concept_id,
    access_options = access_options
  )

  openbiodiv_taxonomic_concept_id = identifier_factory(
    fun = list(lookup_taxonomic_concept),
    prefixes = access_options$prefix,
    def_prefix = access_options$prefix["openbiodiv"]
  )

  # Identifiers

  treatment_id = identifiers$nid
  taxonomic_concept_id = openbiodiv_taxonomic_concept_id(list(treatment_id))

  tt = ResourceDescriptionFramework$new()

  # Treatment
  tt$add_triple(treatment_id, rdf_type, Treatment) # type
  tt$add_triple(treatment_id, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(treatment_id, has_content, i ) } )

  # Taxonomic Concept
  tt$add_triple(taxonomic_concept_id, rdf_type, TaxonomicConcept) # type
  tt$add_triple(taxonomic_concept_id, realization, treatment_id) # type

  return(tt)
}














#' Nomenclature Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
nomenclature = function(atoms, identifiers, access_options)
{
  # Identifiers

  nomenclature_id = identifiers$nid

  tt = ResourceDescriptionFramework$new()
  # Treatment
  tt$add_triple(nomenclature_id, rdf_type, Nomenclature) # type
  tt$add_triple(nomenclature_id, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(nomenclature_id, has_content, i) })

  return(tt)
}




#' Nomenclature Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
nomenclature_citations = function(atoms, identifiers, access_options)
{
  # Identifiers


  tt = ResourceDescriptionFramework$new()
  # Treatment
  tt$add_triple(identifiers$nid, rdf_type, NomenclatureCitationsList) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

  return(tt)
}






#' Diagnosis Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
diagnosis = function(atoms, identifiers, access_options)
{
  # Identifiers

  diagnosis_id = identifiers$nid

  tt = ResourceDescriptionFramework$new()
  # Treatment
  tt$add_triple(diagnosis_id, rdf_type, Diagnosis) # type
  tt$add_triple(diagnosis_id, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(diagnosis_id, has_content, i) })

  return(tt)
}






#' Discussion Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
discussion = function(atoms, identifiers, access_options)
{
  # Identifiers

  tt = ResourceDescriptionFramework$new()
  # Treatment
  tt$add_triple(identifiers$nid, rdf_type, Discussion) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

  return(tt)
}







#' Distribution Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
distribution = function(atoms, identifiers, access_options)
{

  tt = ResourceDescriptionFramework$new()
  # Treatment
  tt$add_triple(identifiers$nid, rdf_type, Distribution) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

  return(tt)
}





#' Materials Examined Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
materials_examined = function(atoms, identifiers, access_options)
{

  tt = ResourceDescriptionFramework$new()
  # Treatment
  tt$add_triple(identifiers$nid, rdf_type, MaterialsExamined) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

  return(tt)
}




#' Taxonomic Key Section Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
taxonomic_key = function(atoms, identifiers, access_options)
{
  tt = ResourceDescriptionFramework$new()
  # Treatment
  tt$add_triple(identifiers$nid, rdf_type, TaxonomicKey) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

  return(tt)
}





#' Figure Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
figure = function(atoms, identifiers, access_options)
{

  tt = ResourceDescriptionFramework$new()
  # Treatment
  tt$add_triple(identifiers$nid, rdf_type, Figure) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

  return(tt)
}




#' Taxonomic Name Usage Constructor
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
taxonomic_name_usage = function(atoms, identifiers, access_options)
{


  atoml_to_val = function(atoml)
  {
    if (length(atoml) > 0) {
      return(atoml[[1]]$text_value)
    }
    else {
      return (NA)
    }
  }


  # identifiers
  label = literal(get_scientific_name_or_tcl(kingdom = atoml_to_val(atoms$kingdom),
                                phylum = atoml_to_val(atoms$phylum),
                                class = atoml_to_val(atoms$class),
                                order = atoml_to_val(atoms$order),
                                family = atoml_to_val(atoms$family),
                                subfamily = atoml_to_val(atoms$subfamily),
                                genus = atoml_to_val(atoms$genus),
                                subgenus = atoml_to_val(atoms$subgenus),
                                species = atoml_to_val(atoms$species),
                                subspecies = atoml_to_val(atoms$subspecies),
                                authorship = atoml_to_val(atoms$authorship),
                                secundum_literal = atoml_to_val(atoms$secundum_literal)))

  if (is.null(label) && length(atoms$verbatim) >= 1)
  {
    label = atoms$verbatim[[1]]
  }
  if (is.null(label))
  {

    return(ResourceDescriptionFramework$new())
  }

  if(length(atoms$verbatim_status) >= 1)
  {
    atoms$status = list(verbstat2openbiodiv(atoms$verbatim_status[[1]]$text_value, def_prefix = access_options$prefix))
  }

  rank = function(kingdom = NA, phylum = NA, class = NA, order = NA,
                  family = NA, subfamily = NA, genus = NA, subgenus = NA, species = NA,
                  subspecies = NA)
  {
    # TODO not dry
    arglist = list(Kingdom = kingdom, Phylum = phylum, Class = class, Order = order,
                      Family = family, Subfamily = subfamily, Genus = genus, Subgenus = subgenus, Species = species,
                      Subspecies = subspecies)
    identifier(id = names(arglist)[max(
      sapply(
        1:length(arglist),
        function(index)
        {
          ifelse(has_meaningful_value(arglist[[index]]), index, 0)
        }
      )
    )],
    prefix = access_options$prefix)
  }

  atoms$taxonomic_rank = list(rank(kingdom = atoml_to_val(atoms$kingdom),
                              phylum = atoml_to_val(atoms$phylum),
                              class = atoml_to_val(atoms$class),
                              order = atoml_to_val(atoms$order),
                              family = atoml_to_val(atoms$family),
                              subfamily = atoml_to_val(atoms$subfamily),
                              genus = atoml_to_val(atoms$genus),
                              subgenus = atoml_to_val(atoms$subgenus),
                              species = atoml_to_val(atoms$species),
                              subspecies = atoml_to_val(atoms$subspecies)))

  if (length(atoms$regularzied_genus) > 0) {
    atoms$genus = atoms$regularzied_genus
  }

  openbiodiv_taxonomic_name_id = identifier_factory(
    fun = list(
      query_factory(
        p_query = qlookup_by_label_in_context,
        access_options = access_options
      ),
      query_factory(
        p_query = qlookup_by_label_no_context,
        access_options = access_options
      )
    ),
    prefixes = access_options$prefix,
    def_prefix = access_options$prefix["openbiodiv"]
  )

  todate = function(year, month, day)
  {
    list(literal(paste(year[[1]]$text_value, month[[1]]$text_value, day[[1]]$text_value, sep = "-"),
            xsd_type = rdf4r::xsd_date))
  }

  atoms$date = append(atoms$date, todate(atoms$pub_year, atoms$pub_month, atoms$pub_day))


  # Identifiers
  article_id = identifiers$root_id
  parent_element_id = identifiers$pid
  tnu_id = identifiers$nid
  taxonomic_name_id = openbiodiv_taxonomic_name_id(purrr::cross2(list(article_id), list(label)))
  tt = ResourceDescriptionFramework$new()
  #TNU
  tt$add_triple(tnu_id, rdf_type, TaxonomicNameUsage) # type
  tt$add_triple(tnu_id, mentions, taxonomic_name_id)
  tt$add_triple(tnu_id, is_contained_by, parent_element_id)
  sapply(atoms$date, function(i) { tt$add_triple(tnu_id, publication_date, i ) } )
  sapply(atoms$verbatim_status, function(i) { tt$add_triple(tnu_id, has_taxonomic_status, i ) } )
  sapply(atoms$status, function(i) { tt$add_triple(tnu_id, has_taxonomic_status_id, i ) } )
  # Taxonomic Name
  tt$add_triple(taxonomic_name_id, rdf_type, ScientificName)
  tt$add_triple(taxonomic_name_id, rdfs_label, label)
  tt$add_triple(taxonomic_name_id, has_scientific_name, label)
  sapply(atoms$kingdom, function(i) { tt$add_triple(taxonomic_name_id, dwc_kingdom, i ) } )
  sapply(atoms$phylum, function(i) { tt$add_triple(taxonomic_name_id, dwc_phylum, i ) } )
  sapply(atoms$class, function(i) { tt$add_triple(taxonomic_name_id, dwc_class, i ) } )
  sapply(atoms$order, function(i) { tt$add_triple(taxonomic_name_id, dwc_order, i ) } )
  sapply(atoms$family, function(i) { tt$add_triple(taxonomic_name_id, dwc_family, i ) } )
  sapply(atoms$subfamily, function(i) { tt$add_triple(taxonomic_name_id, dwc_family, i ) } )
  sapply(atoms$genus, function(i) { tt$add_triple(taxonomic_name_id, dwc_genus, i ) } )
  sapply(atoms$subgenus, function(i) { tt$add_triple(taxonomic_name_id, dwc_subgenus, i ) } )
  sapply(atoms$species, function(i) { tt$add_triple(taxonomic_name_id, dwc_species_ep, i ) } )
  sapply(atoms$subspecies, function(i) { tt$add_triple(taxonomic_name_id, dwc_subspecies_ep, i ) } )
  sapply(atoms$verbatim_rank, function(i) { tt$add_triple(taxonomic_name_id, has_verbatim_rank, i ) } )
  sapply(atoms$taxonomic_rank, function(i) { tt$add_triple(taxonomic_name_id, has_taxonomic_rank_id, i ) } )
  sapply(atoms$authorship, function(i) { tt$add_triple(taxonomic_name_id, dwc_authorship, i ) } )
  # TCL
  #sapply(atoms$secundum_literal, function(i) { tt$add_triple(taxonomic_name_id, has_secundum_string, i ) } )

  return(tt)
}




#'  Institution Code Usage
#'
#'  This creates RDF for institutional_code usages in the document.
#'
#' @param atoms a list of literals
#' @param identifiers a list of identifiers
#' @param access_options
#'
#' @return \code{ResourceDescriptionFramework}
#' @export
institution_code_usage = function(atoms, identifiers, access_options)
{
  tt = ResourceDescriptionFramework$new()
  tt$add_triple(identifiers$nid, rdf_type, InstitutionalCodeUsage) # type
  tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment

  sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, institutional_code, i) })

  # TODO add Institions as resources
  return(tt)
}
