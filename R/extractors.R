  #' Node Extractor
  #'
  #' Given an XML2 object, and an \code{ResourceDescriptionFramework} object, which
  #' are both mutable, the \code{ResourceDescriptionFramework} object is
  #' mutated into containing the triples extracted from the XML2 object,
  #' while the XML2 object is possibly mutated into containing some annotations
  #' allowing the faster reprocessing.
  #'
  #' @inheritParams xml2rdf
  #' @param triples the RDF object where the newly created triples will be
  #'   stored (note this is a side-effect/pass by reference)
  #'
  #' @return \code{ResourceDescriptionFramework} containing all the triples from
  #'   the article
  #'
  #' @export
  node_extractor = function (node, xml_schema, reprocess, triples, prefix,
            dry = FALSE, filename, root_id)
  {
    if (processing_status(node) == FALSE || reprocess == TRUE) {
      if (!is.null(xml_schema$injector)) {
        xml_schema$injector(obkms_id = rdf4r::last_token(rdf4r::strip_filename_extension(filename),
                                                         split = "/"), node)
      }


      atoms = find_literals(node, xml_schema)
      #TODO fix parent_id prefix

      new_triples = xml_schema$constructor(atoms, identifiers = list(nid = identifier_new(node, xml, mongo_key = xml_schema$mongo_key,prefix = xml_schema$prefix, blank = FALSE),
                                                                     pid = parent_id(node),
                                                                     root_id = root_id),
                                           prefix = xml_schema$prefix,schema_name = xml_schema$schema_name, mongo_key = xml_schema$mongo_key)
      new_triples$set_context(triples$context)


      serialization = new_triples$serialize()
      # if (dry == FALSE) {
      #    add_data(serialization, access_options = access_options)
      #  }
      xml2::xml_attr(node, "obkms_process") = "TRUE"
      triples$add_triples(new_triples)
    }
    for (c in xml_schema$components) {
      nodel = xml2::xml_find_all(node, c$xpath)
      for (n in nodel) {
        node_extractor(n, c, reprocess = reprocess, triples = triples,
                       prefix = c$prefix, dry = dry, filename = filename,
                       root_id = root_id)
      }
    }
    return(triples)
  }


#' @export
  populate_prefix_list = function(triples){
    #prefixes which are not added to prefix list via identifier()
    prefixes = c(openbiodivScName = "http://openbiodiv.net/resource/ScientificName/",
                 openbiodivResearchPaper = "http://openbiodiv.net/resource/ResearchPaper/",
                 openbiodivPublisher = "http://openbiodiv.net/resource/Publisher/",
                 openbiodivJournal = "http://openbiodiv.net/resource/Journal/",
                 openbiodivAffil = "http://openbiodiv.net/property/affiliation/",
                 openbiodivTC = "http://openbiodiv.net/resource/TaxonomicConcept/",
                 openbiodivCoordinates = "http://openbiodiv.net/property/hasCoordinates/",
                 openbiodivTNU = "http://openbiodiv.net/resource/TNU/")



    for (s in 1:length(prefixes)){
      triples$prefix_list$add(prefixes[s])
    }

    return(triples)
  }
