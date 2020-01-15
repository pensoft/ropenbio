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
node_extractor = function (node, xml_schema, reprocess, triples, prefix, new_taxons,
                              dry = FALSE, filename, root_id)
{
  if (processing_status(node) == FALSE || reprocess == TRUE && !(is.null(triples))) {
    if (!is.null(xml_schema$injector)) {
      xml_schema$injector(obkms_id = rdf4r::last_token(rdf4r::strip_filename_extension(filename),
                                                       split = "/"), node)
    }


    atoms = find_literals(node, xml_schema)
    #TODO fix parent_id prefix

    new_triples = xml_schema$constructor(atoms, identifiers = list(nid = identifier_new(node, xml, mongo_key = xml_schema$mongo_key,prefix = prefix, blank = FALSE),
                                                                   pid = identifier(parent_id(node), prefix),
                                                                   root_id = root_id),
                                         prefix = xml_schema$prefix,new_taxons = new_taxons, mongo_key = xml_schema$mongo_key)
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

        node_extractor_en(n, c, reprocess = reprocess, triples = triples,
                          prefix = prefix, new_taxons = new_taxons, dry = dry, filename = filename,
                          root_id = root_id)


    }
  }
  return(triples)
}

#' @export
node_extractor_en = function (node, xml_schema, reprocess, triples, prefix, new_taxons,
          dry = FALSE, filename, root_id)
{
  if (processing_status(node) == FALSE || reprocess == TRUE && !(is.null(triples))) {
    if (!is.null(xml_schema$injector)) {
      xml_schema$injector(obkms_id = rdf4r::last_token(rdf4r::strip_filename_extension(filename),
                                                       split = "/"), node)
    }


    atoms = find_literals(node, xml_schema)
    #TODO fix parent_id prefix

    new_triples = xml_schema$constructor(atoms, identifiers = list(nid = identifier_new(node, xml, mongo_key = xml_schema$mongo_key,prefix = prefix, blank = FALSE),
                                                                   pid = identifier(parent_id(node), prefix),
                                                                   root_id = root_id),
                                         prefix = xml_schema$prefix,new_taxons = new_taxons, mongo_key = xml_schema$mongo_key)
    new_triples$set_context(triples$context)

    serialization = new_triples$serialize()
    # if (dry == FALSE) {
    #    add_data(serialization, access_options = access_options)
    #  }

    triples$add_triples(new_triples)
  }
  for (c in xml_schema$components) {
    nodel = xml2::xml_find_all(node, c$xpath)
    for (n in nodel) {

      node_extractor_en(n, c, reprocess = reprocess, triples = triples,
                        prefix = prefix, new_taxons = new_taxons, dry = dry, filename = filename,
                        root_id = root_id)


    }
  }
  return(triples)
}
