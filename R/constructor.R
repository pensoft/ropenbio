    #' Metadata constructor
    #'
    #' @param atoms a list of literals
    #' @param identifiers a list of identifiers
    #' @param prefix
    #'
    #' @return \code{ResourceDescriptionFramework}
    #' @export
    metadata = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      pub_date = function(year, month, day) {
        literal(paste0(text_value = unlist(year)["text_value"],
                       "-", unlist(month)["text_value"], "-", unlist(day)["text_value"]),
                xsd_type = rdf4r::xsd_date)
      }
      if (length(atoms$pensoft_pub) > 0) {
        stop("Pensoft publication")
      }
      general_collection = mongolite::mongo("new_collection")
      doi = unlist(atoms$doi)["text_value"]

      df = set_component_frame(label = doi, mongo_key = c(paper = ""), type = "research_paper", orcid = NA)
      researchPaper_prefix = c(openbiodivResearchPaper = "http://openbiodiv.net/resource/researchPaper/")
      paper_id = get_or_set_mongoid(df, researchPaper_prefix )
      paper_id = identifier(paper_id, researchPaper_prefix)

      article_id = identifiers$root_id
      publisher_lit = unlist(atoms$publisher)["text_value"]

      df = set_component_frame(label = publisher_lit, mongo_key = c(publisher = ""), type = "publisher", orcid = NA)
      publisher_prefix = c(openbiodivPublisher = "http://openbiodiv.net/resource/publisher/")
      publisher_id = get_or_set_mongoid(df, publisher_prefix )
      publisher_id = identifier(publisher_id, publisher_prefix)


      journal_lit = unlist(atoms$journal)["text_value"]

      df = set_component_frame(label = journal_lit, mongo_key = c(journal = ""), type = "journal", orcid = NA)
      journal_prefix = c(openbiodivJournal = "http://openbiodiv.net/resource/journal/")
      journal_id = get_or_set_mongoid(df, journal_prefix )
      journal_id = identifier(journal_id, journal_prefix)



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
            ll = list(text_value = text_value, xsd_type = xsd_string, lang = "",
                      squote = paste0("\"", text_value, "\"", ""))
            class(ll) = "literal"
            journal_zoobank_literal = ll
          }
        }
        zoobank_id = identifier(text_value, c(zoobank = "http://zoobank.org/"))

      }


      tt$add_triple(journal_id, has_identifier, zoobank_id)
      tt$add_triple(zoobank_id, rdf_type, ResourceIdentifier)
      tt$add_triple(zoobank_id, identifier_scheme, zoobank)
      tt$add_triple(zoobank_id, rdfs_label, journal_zoobank_literal)


      tt$add_triple(journal_id, frbr_part, article_id)
      tt$add_triple(article_id, rdf_type, Article)
      sapply(atoms$title, function(i) {
        tt$add_triple(article_id, rdfs_label, i)
      })
      sapply(atoms$title, function(i) {
        tt$add_triple(article_id, dc_title, i)
      })
      sapply(atoms$doi, function(i) {
        tt$add_triple(article_id, has_doi, i)
      })

      #
    #the article zoobank id is the one containing the words "zoobank"
      if(length(atoms$article_zoobank) > 0){
        for (n in 1:length(atoms$article_zoobank)){
          if (grepl("zoobank", unlist(atoms$article_zoobank[[n]])["text_value"]) == TRUE){
            text_value = gsub("^(.*):", "", unlist(atoms$article_zoobank[n])["text_value"])
            ll = list(text_value = text_value, xsd_type = xsd_string, lang = "",
                      squote = paste0("\"", text_value, "\"", ""))
            class(ll) = "literal"
            article_zoobank_literal = ll
          }
        }
      article_zoobank_id = identifier(text_value, c(zoobank = "http://zoobank.org/"))
    }


    tt$add_triple(article_id, has_identifier, article_zoobank_id)
    tt$add_triple(article_zoobank_id, rdf_type, ResourceIdentifier)
    tt$add_triple(article_zoobank_id, identifier_scheme, zoobank)
    tt$add_triple(article_zoobank_id, rdfs_label, article_zoobank_literal)


    if(length(atoms$plazi_id) > 0){
      for (n in 1:length(atoms$plazi_id)){
          text_value = gsub("http://tb.plazi.org/GgServer/summary/", "", unlist(atoms$plazi_id[n])["text_value"])
          ll = list(text_value = text_value, xsd_type = xsd_string, lang = "",
                    squote = paste0("\"", text_value, "\"", ""))
          class(ll) = "literal"
          plazi_article_id_lit = ll

      }
      plazi_article_id = identifier(text_value, c(plazi = "http://tb.plazi.org/GgServer/summary/"))
      tt$add_triple(article_id, has_identifier, plazi_article_id)
      tt$add_triple(plazi_article_id, rdf_type, ResourceIdentifier)
      tt$add_triple(plazi_article_id, identifier_scheme, plazi)
      tt$add_triple(plazi_article_id, rdfs_label, plazi_article_id_lit)
    }



    #  tt$add_triple(article_id, has_zoobank, article_zoobank)

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
      tt$add_triple(article_id, realization_of, paper_id)
      sapply(atoms$issue, function(i) {
        tt$add_triple(article_id, has_issue, i)
      })
      sapply(atoms$keyword, function(i) {
        tt$add_triple(identifiers$nid, has_keyword, i)
      })



      #bold_prefix = "http://openbiodiv.net/property/BOLD"
      #names(bold_prefix) = "openbiodivBOLD"
      #tt$prefix_list$add(bold_prefix)

      bold_values=c()
      bold_identifiers = c()
      if (length(atoms$bold_id)>0){
        for(i in 1:length(atoms$bold_id)){
          bold_values = c(bold_values, atoms$bold_id[[i]]$text_value)
          bold_identifiers = c(bold_identifiers, list(identifier(gsub("BOLD:", "",atoms$bold_id[[i]]$text_value), c(openbiodivBOLD = "http://openbiodiv.net/resource/BOLD/"))))
          }
      }


      bin_values=c()
      bin_identifiers = c()
      if (length(atoms$bin)>0){
        for(i in 1:length(atoms$bin)){
          bin_values = c(bin_values, atoms$bin[[i]]$text_value)
          bin_identifiers = c(bin_identifiers, list(identifier(gsub("BOLD:", "",atoms$bin[[i]]$text_value), c(openbiodivBIN = "http://openbiodiv.net/resource/BIN/"))))
        }
      }

      #BOLD IDS
      sapply(bold_identifiers, function(i){
        tt$add_triple(article_id, mentions_id, i)
      })
      sapply(bold_identifiers, function(i){
        tt$add_triple(bold_identifiers, rdf_type, ResourceIdentifier)

      })
      for (i in 1:length(bold_identifiers)){
        tt$add_triple(bold_identifiers[[i]], rdfs_label, literal(bold_values[i]))
        tt$add_triple(bold_identifiers[[i]], identifier_scheme, bold)
      }

      #BINS
      sapply(bin_identifiers, function(i){
        tt$add_triple(article_id, mentions_id, i)
      })
      sapply(bin_identifiers, function(i){
        tt$add_triple(bin_identifiers, rdf_type, ResourceIdentifier)

      })
      for (i in 1:length(bin_identifiers)){
        tt$add_triple(bin_identifiers[[i]], rdfs_label, literal(bin_values[i]))
        tt$add_triple(bin_identifiers[[i]], identifier_scheme, bold)
      }



      genbank_values=c()
      genbank_identifiers = c()
      if (length(atoms$genbank_id)>0){
        for(i in 1:length(atoms$genbank_id)){
          genbank_values = c(genbank_values, atoms$genbank_id[[i]]$text_value)
          genbank_identifiers = c(genbank_identifiers, list(identifier(atoms$genbank_id[[i]]$text_value, c(openbiodivGenBank = "http://openbiodiv.net/resource/GenBank/"))))
        }
      }

      #GenBank
      sapply(genbank_identifiers, function(i){
        tt$add_triple(article_id, mentions_id, i)
      })
      sapply(genbank_identifiers, function(i){
        tt$add_triple(genbank_identifiers, rdf_type, ResourceIdentifier)

      })
      for (i in 1:length(genbank_identifiers)){
        tt$add_triple(genbank_identifiers[[i]], rdfs_label, literal(genbank_values[i]))
        tt$add_triple(genbank_identifiers[[i]], identifier_scheme, genbank)
      }

      tt$add_triple(publisher_id, rdf_type, Publisher)
      sapply(atoms$publisher, function(i) {
        tt$add_triple(publisher_id, rdfs_label, i)
      })
      tt$add_triple(paper_id, rdf_type, Paper)
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
    keyword_group = function (atoms, identifiers, prefix, schema_name, mongo_key)
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
    title = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      atoms$text_content = double_quote_replacer(atoms$text_content)


      tt = ResourceDescriptionFramework$new()
      tt$add_triple(identifiers$nid, rdf_type, Title)
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(identifiers$nid, has_content, i)
      })
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
    abstract = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      atoms$text_content = double_quote_replacer(atoms$text_content)

    #  trans_abstract = escape_special(atoms$trans_abstract)
      tt = ResourceDescriptionFramework$new()
      tt$add_triple(identifiers$nid, rdf_type, Abstract)
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(identifiers$nid, has_content, i)
      })
      sapply(atoms$trans_abstract, function(i) {
        tt$add_triple(identifiers$nid, has_content, i)
      })
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
    author = function (atoms, identifiers, prefix, schema_name, mongo_key)
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

      atoms$full_name = ifelse(length(atoms$full_name) == 0, list(literal(full_name(atoms$surname,
                                                                                    atoms$given_names), xsd_type = rdf4r::xsd_string)), atoms$full_name)
      aid = sapply(atoms$aff_id, function(a) {
        as.integer(gsub("[^0-9.]", "", a$text_value))
      })

       # for (a in 1:length(atoms$orcid)){
      #    atoms$orcid[[a]]$text_value = gsub("^(.*)orcid.org\\/", "", atoms$orcid[[a]]$text_value)
      #    atoms$orcid[[a]]$squote = gsub("^(.*)orcid.org\\/", "",  atoms$orcid[[a]]$squote)
      #  }


      paper_id = identifiers$root_id
      author_id = identifiers$nid
      #concatenate affiliation institution and city
      all_affs = NULL
      for(r in 1:length(atoms$all_affiliations_institutions)){
        if (length(atoms$all_affiliations_cities)>0 && length(atoms$all_affiliations_cities)==length(atoms$all_affiliations_institutions)){
          a = paste0(atoms$all_affiliations_institutions[[r]]$text_value, ",", atoms$all_affiliations_cities[[r]]$text_value)
        }else{
          a = atoms$all_affiliations_institutions[[r]]$text_value
        }
        a = literal(a, xsd_type = rdf4r::xsd_string)
        all_affs[[r]] = a
      }



      tt = ResourceDescriptionFramework$new()

      affiliation_prefix = "http://openbiodiv.net/property/affiliation"
      names(affiliation_prefix) = "openbiodivAffil"
      tt$prefix_list$add(affiliation_prefix)

      tt$add_triple(paper_id, creator, author_id)
      tt$add_triple(author_id, rdf_type, Person)
      sapply(atoms$full_name, function(j) {
        tt$add_triple(author_id, rdfs_label, j)
      })

      sapply(all_affs[aid], function(j) {
        tt$add_triple(author_id, has_affiliation, j)
      })

      sapply(atoms$email, function(j) {
        tt$add_triple(author_id, has_email, j)
      })


      if(length(atoms$orcid)>0){
        orcid_value = gsub("^(.*)orcid.org\\/", "", atoms$orcid[[1]]$text_value)
        orcid_id = identifier(orcid_value, c(orcid = "https://orcid.org/"))
      }else{
        orcid_id = NULL
      }


      #following the datacite ontology: http://www.sparontologies.net/ontologies/datacite
      tt$add_triple(author_id, has_identifier, orcid_id)

      tt$add_triple(orcid_id, rdf_type, PersonalIdentifier)
      tt$add_triple(orcid_id, identifier_scheme, orcid)
      tt$add_triple(orcid_id, rdfs_label, literal(orcid_value))
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
    introduction_section = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {

      atoms$text_content = double_quote_replacer(atoms$text_content)
      tt = ResourceDescriptionFramework$new()
      tt$add_triple(identifiers$nid, rdf_type, Introduction)
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(identifiers$nid, has_content, i)
      })
      return(tt)
    }







    #' Treatment Constructor
    #'
    #' @param atoms a list of literals
    #' @param identifiers a list of identifiers
    #' @param prefix
    #'
    #' @return \code{ResourceDescriptionFramework}
    #' @export
    treatment = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {

      treatment_id = identifiers$nid

      atoms$text_content = double_quote_replacer(atoms$text_content)
      for (a in 1:length(atoms$text_content)){
        atoms$text_content[[a]]$text_value = escape_special(atoms$text_content[[a]]$text_value)
      }

      #check mongo collection for treatments+tc (treatment frbf:realizationof taxonomicConcept)
     # treatment_collection = mongolite::mongo("treatment_collection")
     #tc_key = check_mongo(value = treatment_id$uri, type  = names(mongo_key), collection = treatment_collection, regex = FALSE)
      #treatment_prefix = c(openbiodivTreatment = "http://openbiodiv.net/resource/treatment")
      #tc_prefix = c(openbiodivTaxonomicConcept = "http://openbiodiv.net/resource/taxonomicConcept")
       # if (is.null(tc_key)){
        #if there is no tc realization of the treatment, create it
        #key = uuid::UUIDgenerate()
        #save it both in the treatment collection and in the
        #general_collection =  mongolite::mongo("new_collection")
        #save_to_mongo(treatment_id$uri, treatment_id$uri, names(mongo_key), treatment_collection)
      #  save_to_mongo(identifier(key, tc_prefix)$uri, treatment_id$uri, "taxonomic_concept", general_collection)
      #}
      #taxonomic_concept_id = identifier(id = key, prefix = tc_prefix)

      #taxonomic_concept_id = openbiodiv_taxonomic_concept_id(list(treatment_id))
      tt = ResourceDescriptionFramework$new()
      tt$add_triple(treatment_id, rdf_type, Treatment)
      tt$add_triple(treatment_id, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(treatment_id, has_content, i)
      })
     # tt$add_triple(taxonomic_concept_id, rdf_type, TaxonomicConcept)
    #  tt$add_triple(taxonomic_concept_id, realization, treatment_id)
      return(tt)
    }



    #' Nomenclature Constructor
    #'
    #' @param atoms a list of literals
    #' @param identifiers a list of identifiers
    #' @param prefix
    #'
    #' @return \code{ResourceDescriptionFramework}
    #' @export
    nomenclature = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      nomenclature_id = identifiers$nid
      atoms$text_content = double_quote_replacer(atoms$text_content)
      tt = ResourceDescriptionFramework$new()
      tt$add_triple(nomenclature_id, rdf_type, Nomenclature)
      tt$add_triple(nomenclature_id, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(nomenclature_id, has_content, i)
      })
      return(tt)
    }




    #' Nomenclature Constructor
    #'
    #' @param atoms a list of literals
    #' @param identifiers a list of identifiers
    #' @param prefix
    #'
    #' @return \code{ResourceDescriptionFramework}
    #' @export
    nomenclature_citations = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      tt = ResourceDescriptionFramework$new()
      atoms$text_content = double_quote_replacer(atoms$text_content)
      tt$add_triple(identifiers$nid, rdf_type, NomenclatureCitationsList)
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(identifiers$nid, has_content, i)
      })
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
    diagnosis = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      diagnosis_id = identifiers$nid
      atoms$text_content = double_quote_replacer(atoms$text_content)
      tt = ResourceDescriptionFramework$new()
      tt$add_triple(diagnosis_id, rdf_type, Diagnosis)
      tt$add_triple(diagnosis_id, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(diagnosis_id, has_content, i)
      })
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
    discussion = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      tt = ResourceDescriptionFramework$new()
      atoms$text_content = double_quote_replacer(atoms$text_content)
      tt$add_triple(identifiers$nid, rdf_type, Discussion)
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(identifiers$nid, has_content, i)
      })
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
    distribution = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      tt = ResourceDescriptionFramework$new()
      atoms$text_content = double_quote_replacer(atoms$text_content)

      tt$add_triple(identifiers$nid, rdf_type, Distribution)
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(identifiers$nid, has_content, i)
      })
      return(tt)
    }





    #' Materials Examined Constructor
    #'
    #' @param atoms a list of literals
    #' @param identifiers a list of identifiers
    #' @param prefix
    #'
    #' @return \code{ResourceDescriptionFramework}
    #' @export
    materials_examined = function(atoms, identifiers, prefix, schema_name, mongo_key)
    {

      tt = ResourceDescriptionFramework$new()
      atoms$text_content = double_quote_replacer(atoms$text_content)


      tt$add_triple(identifiers$nid, rdf_type, MaterialsExamined) # type
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment
      sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, has_content, i) })

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
    taxonomic_key = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      tt = ResourceDescriptionFramework$new()
      atoms$text_content = double_quote_replacer(atoms$text_content)

      tt$add_triple(identifiers$nid, rdf_type, TaxonomicKey)
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(identifiers$nid, has_content, i)
      })
      return(tt)
    }





    #' Figure Constructor
    #'
    #' @param atoms a list of literals
    #' @param identifiers a list of identifiers
    #' @param prefix
    #'
    #' @return \code{ResourceDescriptionFramework}
    #' @export
    figure =  function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      tt = ResourceDescriptionFramework$new()
      atoms$text_content = double_quote_replacer(atoms$text_content)

      tt$add_triple(identifiers$nid, rdf_type, Figure)
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)
      sapply(atoms$text_content, function(i) {
        tt$add_triple(identifiers$nid, has_content, i)
      })
      return(tt)
    }



    #' Taxonomic Name Usage Constructor
    #'
    #' @param atoms a list of literals
    #' @param identifiers a list of identifiers
    #' @param prefix
    #'
    #' @return \code{ResourceDescriptionFramework}
    #' @export
    taxonomic_name_usage = function (atoms, identifiers, prefix, schema_name, mongo_key)
    {
      taxon_id = identifiers$nid
      atoml_to_val = function(atoml) {
        if (length(atoml) > 0) {
          return(atoml[[1]]$text_value)
        }
        else {
          return(NA)
        }
      }
      label = literal(get_scientific_name_or_tcl(kingdom = atoml_to_val(atoms$kingdom),
                                                 phylum = atoml_to_val(atoms$phylum), class = atoml_to_val(atoms$class),
                                                 order = atoml_to_val(atoms$order), family = atoml_to_val(atoms$family),
                                                 subfamily = atoml_to_val(atoms$subfamily), genus = atoml_to_val(atoms$genus),
                                                 subgenus = atoml_to_val(atoms$subgenus), species = atoml_to_val(atoms$species),
                                                 subspecies = atoml_to_val(atoms$subspecies), authorship = atoml_to_val(atoms$authorship),
                                                 secundum_literal = atoml_to_val(atoms$secundum_literal)))
      if (is.null(label) && length(atoms$verbatim) >= 1) {
        label = atoms$verbatim[[1]]
      }
      if (is.null(label)) {
        return(ResourceDescriptionFramework$new())
      }
      if (length(atoms$verbatim_status) >= 1) {
        atoms$status = list(verbstat2openbiodiv(atoms$verbatim_status[[1]]$text_value,
                                                def_prefix = prefix))
      }
      rank = function(kingdom = NA, phylum = NA, class = NA, order = NA,
                      family = NA, subfamily = NA, genus = NA, subgenus = NA,
                      species = NA, subspecies = NA) {
        arglist = list(Kingdom = kingdom, Phylum = phylum, Class = class,
                       Order = order, Family = family, Subfamily = subfamily,
                       Genus = genus, Subgenus = subgenus, Species = species,
                       Subspecies = subspecies)
        identifier(id = names(arglist)[max(sapply(1:length(arglist),
                                                  function(index) {
                                                    ifelse(has_meaningful_value(arglist[[index]]),
                                                           index, 0)
                                                  }))], prefix = prefix)
      }
      atoms$taxonomic_rank = list(rank(kingdom = atoml_to_val(atoms$kingdom),
                                       phylum = atoml_to_val(atoms$phylum), class = atoml_to_val(atoms$class),
                                       order = atoml_to_val(atoms$order), family = atoml_to_val(atoms$family),
                                       subfamily = atoml_to_val(atoms$subfamily), genus = atoml_to_val(atoms$genus),
                                       subgenus = atoml_to_val(atoms$subgenus), species = atoml_to_val(atoms$species),
                                       subspecies = atoml_to_val(atoms$subspecies)))
      if (length(atoms$regularzied_genus) > 0) {
        atoms$genus = atoms$regularzied_genus
      }
      tnu_collection = mongolite::mongo("tnus")
      #label = label[!is.null(label)]
      #paper_id = stringr::str_extract(identifiers$root_id$id, "([^\\/]*)$")
      paper_id = identifiers$root_id$uri
      mongo_tnu = check_mongo(paper_id, label$text_value, tnu_collection,
                              regex = FALSE)
      #print("mongo tnu")
      #print(mongo_tnu)
      tnu_prefix = c(openbiodivTNU = "http://openbiodiv.net/resource/tnu/")
      if (is.null(mongo_tnu)) {
        mongo_tnu = uuid::UUIDgenerate()
        type = as.character(label$text_value)
        save_to_mongo(key = identifier(mongo_tnu, tnu_prefix)$uri, value = paper_id, type = type,
                      collection = tnu_collection)
      }else{
        mongo_tnu = rdf4r::strip_angle(mongo_tnu)
        mongo_tnu = gsub("^(.*)resource\\/(.*)\\/", "", mongo_tnu)
      }
     #
      #print(mongo_tnu)
      tnu_id = identifier(mongo_tnu, prefix = tnu_prefix)
      #print(tnu_id)
      todate = function(year, month, day) {
        list(literal(paste(year[[1]]$text_value, month[[1]]$text_value,
                           day[[1]]$text_value, sep = "-"), xsd_type = rdf4r::xsd_date))
      }
      atoms$date = append(atoms$date, todate(atoms$pub_year, atoms$pub_month,
                                             atoms$pub_day))
      article_id = identifiers$root_id

      parent_element_id = identifiers$pid #only works if the pid is set as identifier(parent_id(node))
      tt = ResourceDescriptionFramework$new()
      tt$add_triple(tnu_id, rdf_type, TaxonomicNameUsage)
      tt$add_triple(tnu_id, mentions, taxon_id)
      tt$add_triple(tnu_id, is_contained_by, parent_element_id)
      sapply(atoms$date, function(i) {
        tt$add_triple(tnu_id, publication_date, i)
      })
      sapply(atoms$verbatim_status, function(i) {
        tt$add_triple(tnu_id, has_taxonomic_status, i)
      })
      sapply(atoms$status, function(i) {
        tt$add_triple(tnu_id, has_taxonomic_status_id, i)
      })
      tt$add_triple(taxon_id, rdf_type, ScientificName)
      ScPrefix = "http://openbiodiv.net/resource/ScientificName/"
      names(ScPrefix) = "openbiodivScName"
      tt$prefix_list$add(ScPrefix)

      tt$add_triple(taxon_id, rdfs_label, label)
      tt$add_triple(taxon_id, has_scientific_name, label)
      sapply(atoms$kingdom, function(i) {
        tt$add_triple(taxon_id, dwc_kingdom, i)
      })
      sapply(atoms$phylum, function(i) {
        tt$add_triple(taxon_id, dwc_phylum, i)
      })
      sapply(atoms$class, function(i) {
        tt$add_triple(taxon_id, dwc_class, i)
      })
      sapply(atoms$order, function(i) {
        tt$add_triple(taxon_id, dwc_order, i)
      })
      sapply(atoms$family, function(i) {
        tt$add_triple(taxon_id, dwc_family, i)
      })
      sapply(atoms$subfamily, function(i) {
        tt$add_triple(taxon_id, dwc_family, i)
      })
      sapply(atoms$genus, function(i) {
        tt$add_triple(taxon_id, dwc_genus, i)
      })
      sapply(atoms$subgenus, function(i) {
        tt$add_triple(taxon_id, dwc_subgenus, i)
      })
      sapply(atoms$species, function(i) {
        tt$add_triple(taxon_id, dwc_species_ep, i)
      })
      sapply(atoms$subspecies, function(i) {
        tt$add_triple(taxon_id, dwc_subspecies_ep, i)
      })
      sapply(atoms$verbatim_rank, function(i) {
        tt$add_triple(taxon_id, has_verbatim_rank, i)
      })
      sapply(atoms$taxonomic_rank, function(i) {
        tt$add_triple(taxon_id, has_taxonomic_rank_id, i)
      })
      sapply(atoms$authorship, function(i) {
        tt$add_triple(taxon_id, dwc_authorship, i)
      })
      return(tt)
    }





    #'  Institution Code Usage
    #'
    #'  This creates RDF for institutional_code usages in the document.
    #'
    #' @param atoms a list of literals
    #' @param identifiers a list of identifiers
    #' @param prefix
    #'
    #' @return \code{ResourceDescriptionFramework}
    #' @export
    institution_code_usage = function(atoms, identifiers, prefix, schema_name, mongo_key)
    {
      tt = ResourceDescriptionFramework$new()
      tt$add_triple(identifiers$nid, rdf_type, InstitutionalCodeUsage) # type
      tt$add_triple(identifiers$nid, is_contained_by, identifiers$pid)   # containtment

      sapply(atoms$text_content, function(i) { tt$add_triple(identifiers$nid, institutional_code, i) })

      # TODO add Institions as resources
      return(tt)
    }
