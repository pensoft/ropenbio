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
node_extractor = function (node, xml_schema, reprocess, triples, access_options, 
                             dry = FALSE, filename, root_id=root_ident) 
  {
    if (processing_status(node) == FALSE || reprocess == TRUE) {
      if (!is.null(xml_schema$injector)) {
        xml_schema$injector(obkms_id = rdf4r::last_token(rdf4r::strip_filename_extension(filename), 
                                                         split = "/"), node)
      }
      atoms = find_literals(node, xml_schema)
      new_triples = xml_schema$constructor(atoms, identifiers = list(nid = identifier_new(node, xml, schema_name = xml_schema$schema_name, mongo_key = xml_schema$mongo_key, prefix = access_options$prefix["openbiodiv"], blank = FALSE), 
                                                                     pid = parent_id(node), 
                                                                     root_id = root_ident),
                                                                     access_options = access_options, 
                                                                     schema_name = xml_schema$schema_name,
                                                                     mongo_key = xml_schema$mongo_key)
   
      new_triples$set_context(triples$context)
      serialization = new_triples$serialize()
   
      if (dry == FALSE) {
        add_data(serialization, access_options = access_options)
      }
      xml2::xml_attr(node, "obkms_process") = "TRUE"
      triples$add_triples(new_triples)
    }
    for (c in xml_schema$components) {
      nodel = xml2::xml_find_all(node, c$xpath)
      for (n in nodel) {
        node_extractor(n, c, reprocess = reprocess, triples = triples, 
                       access_options = access_options, dry = dry, 
                       filename = filename, root_id = root_ident)
      }
    }
    return(triples)
  }

