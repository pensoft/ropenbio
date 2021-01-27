#' XML Node Schema
#'
#' Defines the XML schema of a node.
#'
#' @field schema_name the name of the schema
#' @field file_pattern regular expression pattern matching the file-name extension
#' @field extension the file-name extension
#' @field prefix the prefix that these documents have in the URI
#' @field atoms named character vector of xpath locations
#' @field constructor an RDF constructor function that can be called on
#'   a list of atoms extractor from a node in the schema
#' @field injector
#' @field atom_types
#' @field atom_lang
#' @field mongo_key
#' @field components a list of \code{XmlSchema} object containing the nested
#'   components in the node
#'
#'
#' @examples
#' taxonx = XmlSchema$new(schema_name = "taxonx", file_pattern = ".*\\.xml")
#'
#' @export
XmlSchema =
  R6::R6Class("xml_schema",
              public = list(
                schema_name = NULL,
                xpath = NA,
                file_pattern = NULL,
                extension = NULL,
                prefix = NULL,
                atoms = NULL,
                atom_types = NULL,
                atom_lang = NULL,
                constructor = NULL,
                injector = NULL,
                mongo_key = NULL,
                components = NULL,

                initialize =
                  function(schema_name = NA, xpath = NA, file_pattern = NA, extension = NA, prefix = NA, atoms = NA, atom_types = NULL, atom_lang = NA, constructor = NULL, injector = NULL, mongo_key = NULL, components = NULL)
                  {
                    self$schema_name = schema_name
                    self$xpath = xpath
                    self$file_pattern = file_pattern
                    self$extension = extension
                    self$prefix = prefix
                    self$atoms = atoms
                    self$atom_lang = atom_lang
                    self$atom_types = atom_types
                    self$constructor = constructor
                    self$injector = injector
                    self$mongo_key = mongo_key
                    self$components = components
                  }
              )
  )








#' RDF-ization of a Single XML File
#'
#' Converts an XML file to RDF and submits it to a triple store. Writes
#' the serialization in a file in the file system.
#'
#' @param filename locator, e.g. file path or URL, of the XML resource
#' @param xml_schema
#'        XML schema of the XML resource; one of
#'        taxpub taxonx or plazi_int
#' @param access_options endpoint to database to which to submit
#' @param serialization_dir directory where to dump intermediate serializations
#' @param serialization_format default is "turtle"
#' @param reprocess \code{logical}
#'
#' @return \code{logical}. TRUE if everything went OK. FALSE if there was a problem
#'
#'
#' @export
xml2rdf = function(filename, xml, xml_schema, access_options, serialization_dir, reprocess, dry, grbio, taxon_discovery)
{
  # generate lookup functions



  tryCatch(
    {
      #xml = xml2::read_xml(filename)

      if (is.plazi_pensoft_pub(xml) == FALSE){
        xml_string = toString(xml)
        xml_string = strip_xml_newlines(xml_string)
        xml = xml2::as_xml_document(xml_string)

        doi = xml2::xml_text(xml2::xml_find_first(xml, "/article/front/article-meta/article-id[@pub-id-type='doi']"))
        if (is.na(doi)){
          doi = xml2::xml_text(xml2::xml_find_first(xml, "/document/mods:mods/mods:identifier[@type='DOI']"))
        }
        if (is.null(doi)){
          doi = NA
        }
        article_ident = xml2::xml_text(xml2::xml_find_all(xml, "//article/front/article-meta/article-id[@pub-id-type='publisher-id']"))
        if (length(article_ident) == 0){
          article_ident = NA
        }
        prefix = c(openbiodiv = "http://openbiodiv.net/")
        triples = ResourceDescriptionFramework$new()
        if (is.plazi_doc(xml)==TRUE)
        {
          xml_schema = plazi_schema
          processed_xml_dir = "/opt/data/obkms/plazi_corpus/plazi_processed/"
          filename_processed = gsub("/opt/data/obkms/plazi_corpus/", "", filename)
          filename_processed = paste0(processed_xml_dir, filename_processed)
        }else{
          xml_schema = taxpub
          processed_xml_dir = "/opt/data/obkms/pjsupload/pensoft_processed/"
          filename_processed = gsub("/opt/data/obkms/pjsupload/", "", filename)
          filename_processed = paste0(processed_xml_dir, filename_processed)
        }

        processing_xml = xml

        root_setting = root(node=xml, xml_schema = xml_schema, xml=xml, mongo_key = xml_schema$mongo_key, prefix = prefix, blank = FALSE)
        root_ident = root_setting

        xml2::write_xml(xml, filename)
        triples$set_context(root_ident)

        set_journal_publisher_ids = function(xml){
          plazi_doc = is.plazi_doc(xml)
            #if plazi_doc == TRUE, only set journal_id (with different xpath), otherwise do both
          if (plazi_doc == TRUE){
            journal_name = xml2::xml_text(xml2::xml_find_all(xml, "/document/mods:mods/mods:relatedItem[@type='host']/mods:titleInfo/mods:title"))
            publisher_id = NA
          } else{
            journal_name = xml2::xml_text(xml2::xml_find_all(xml, "/article/front/journal-meta/journal-title-group/journal-title"))
            publisher_name = xml2::xml_text(xml2::xml_find_all(processing_xml, "/article/front/journal-meta/publisher/publisher-name"))
            type_publisher= "publisher"

            df = set_component_frame(label = publisher_name, mongo_key = c(publisher = NA), type = type_publisher, orcid = NA, parent = NA, key = NA, publisher_id = NA, journal_id = NA, plazi_doc= plazi_doc, doi = doi, article_id = article_ident)
            publisher_id = get_or_set_mongoid(df, prefix )
            publisher_id = paste0("<http://openbiodiv.net/",publisher_id,">")
          }

          df = set_component_frame(label = journal_name, mongo_key = c(journal = NA), type = "journal", orcid = NA, parent = NA, key = NA, publisher_id = NA, journal_id = NA, plazi_doc = plazi_doc, doi = doi, article_id = article_ident)
          journal_id = get_or_set_mongoid(df, prefix)
          journal_id = paste0("<http://openbiodiv.net/",journal_id,">")

          #return both
          res = c(journal_id, publisher_id)
          names(res) = c("journal_id", "publisher_id")
          return(res)
        }

        ids = set_journal_publisher_ids(processing_xml)
        journal_id = ids["journal_id"]
        publisher_id = ids["publisher_id"]

        if (is.plazi_doc(xml)){
          plazi_treatment_id = xml2::xml_text(xml2::xml_find_all(xml, ".//treatment/@httpUri"))
          pref = "http://treatment.plazi.org/id/"
          plazi_treatment_id = gsub(pref, "", plazi_treatment_id)
          plazi_treatment_id = uuid_dasher(plazi_treatment_id)
          plazi_treatment_id = identifier(plazi_treatment_id, prefix)
        }else{
          plazi_treatment_id =  NA
        }



        #finds all institution codes and names and saves them in mongodb collection
        extract_inst_identifiers(processing_xml, root_id = root_ident, prefix = prefix, collection = inst_collection, grbio = grbio)

        new_taxons = scan(taxon_discovery, character(), quote = "", sep="\n")
        triples = node_extractor(
          node = processing_xml,
          xml_schema = xml_schema,
          xml = processing_xml,
          reprocess = reprocess,
          triples = triples,
          prefix = prefix,
          new_taxons = new_taxons,
          dry = dry,
          filename = filename,
          root_id = root_ident,
          publisher_id = publisher_id,
          journal_id = journal_id,
          plazi_doc = is.plazi_doc(xml),
          plazi_treatment_id = plazi_treatment_id,
          doi = doi,
          article_id = article_ident
        )

        serialization = triples$serialize()
        #changed to a new mode of triple saving. save + upload

        add_data(serialization, access_options = access_options)

       cat(
        serialization,
        file = paste0(
          serialization_dir, "/",
          paste0(strip_filename_extension(last_token(filename, split = "/")), ".trig")
        )
      )
       #save the formatted xml into the directory with processed xmls

        xml2::write_xml(processing_xml, filename_processed)
        #remove the other xml from the directory with unprocessed xmls
        file.remove(filename)


        if (is.plazi_doc(xml)==TRUE){
          collection_name = "plazi_xmls"
        }else{
          collection_name = "xmls"
        }
        xml_collection = mongolite::mongo(collection = collection_name, db = "test")
        xml_str = toString(processing_xml)

        d = data.frame(
          xml = xml_str,
          filename = as.character(filename),
          doi = doi
        )
        xml_collection$insert(d)
      return(TRUE)
    }
      },
    error = function(e)
    {
      warning(e)
      return(FALSE)
    })
}

#' @export
general_collection =  mongolite::mongo(collection = "new_collection", db = "test")

#' @export
inst_collection = mongolite::mongo(collection = "institutions", db = "test")

#' @export
checklistCol = mongolite::mongo(collection = "checklist", db = "openbiodiv")


#' @export
create_new_file = function(serialization_dir){
  time = Sys.time()
  time = gsub(":|\\s", "-", time)
  file = paste0(serialization_dir, "/",time, ".trig")
  file.create(file)
  file
}


#' @export
save_serialization = function(serialization,serialization_dir){
  df <- file.info(list.files(serialization_dir, full.names = T))
  #if there are no files in the serialization dir, create new one and write to it
  if (nrow(df) == 0){
    file = create_new_file(serialization_dir)
    cat(serialization, file = file, append = TRUE)
    ret = TRUE
  }else{
    #if there are files, find the last modified one and write to it if its less than 200 Mb, or otherwise create a new one
    last_modified = rownames(df)[which.max(df$mtime)]
    file_size = file.info(last_modified)$size
    if(file_size < 200000000){
      #keep appending to file
      cat(serialization, file = last_modified, append = TRUE)
      ret =TRUE
    }else{
      file = create_new_file(serialization_dir)
      #open new file and start appending to it
      cat(serialization,file = file,append = TRUE )
      ret =  TRUE
    }
  }
  return(ret)
}





#' NOT USED ANYMORE
#' Get the OBKMS Id of an XML node, if not set, set it.
#'
#' Does not do any database lookups.
#'
#' @param node the XML node from which to take the ID; cannot be missing!
#'
#' @return the local id (not an identifier object)
#'
#' @export
get_or_set_obkms_id = function (node)
{
  if (is.na(xml2::xml_attr(node, "obkms_id")))
  {
    xml2::xml_attr(node, "obkms_id") = uuid::UUIDgenerate()
  }

  xml2::xml_attr(node, "obkms_id")
}


#' NOT USED ANYMORE
#' Set the OBKMS ID of an XML node according to new id format
#'
#' Does not do any database lookups.
#'
#' @param schema_name the name of the xml_schema
#' @param mongo_name the names of mongo_key
#' @return the local id (not an identifier object)
#'
#' @export
set_obkms = function(schema_name, mongo_name)
{
  random = uuid::UUIDgenerate()
  id = paste0(schema_name,"/", mongo_name, "/",random)
}






#' Find (Re-)Processing Status
#'
#' @param node \code{XML2} object
#'
#' @return \code{logical} TRUE if the node has been processed, FALSE otherwise
#' @export
processing_status = function(node)
{
  if (is.na(xml2::xml_attr(node, "obkms_process"))) {
    return (FALSE)
  }
  else {
    return (as.logical(xml2::xml_attr(node, "obkms_process")))
  }
}









#' Finds the Atoms in a XML Node
#'
#' @param xml the XML node
#' @param xpath the atom locations as a named character vector
#' @param atom_types the type (explicitly stated, not as xpath) of the atom
#' @param atom_lang the language (as xpath), if the xpath fails, will set to
#'   en (if string)
#'
#' @return list
#'
#' @export
find_literals = function (xml, xml_schema)
{
  rr = vector(mode = "list", length = length(xml_schema$atoms))
  names(rr) = names(xml_schema$atoms)
  for (nn in names(xml_schema$atoms)) {
    nodes = xml2::xml_find_all(xml, xml_schema$atoms[nn])
    #literals = paste(xml2::xml_text(nodes), collapse = " ")
    literals = xml2::xml_text(nodes)
    literals = strip_starting_whitespace(literals)
    literals = strip_trailing_whitespace(literals)


       # literals = gsub("(?<=[a-z0-9])(?=[A-Z])", " ", literals,
                #    perl = TRUE)
    languages = tryCatch(xml2::xml_text(xml2::xml_find_all(xml,
                                                           xml_schema$atom_lang[nn])), error = function(e) {
                                                             NA
                                                           })
    rr[[nn]] = lapply(seq(along.with = literals), function(i) {
      literal(literals[i], xsd_type = xml_schema$atom_types[[nn]],
              lang = languages[i])
    })
  }
  return(rr)
}


#'
#find_identifiers = function(node, xml_schema)
#{
#  lapply(xml_schmea$, function(p)
#  {
#    xml2::xml_text(xml2::xml_find_all(xml, p))
#  })
#
#}





#' Get the Parent OBKS Id for an XML Node
#'
#' Does not do any database lookups. Recursive looks for the parent node until
#' it finds one with ID.
#'
#' @param node the XML node from which to take the ID; cannot be missing!
#' @param fullname if TRUE, returns a URI with the OBKMS base prefix
#'
#' @export
parent_id = function (node, fullname = FALSE )
{
  obkms_id = NA
  path = xml2::xml_path( node )
  # will repeat while we don't have an id and we aren't at the top
  while ( path != "/" && is.na( obkms_id ) ) {
    node = xml2::xml_parent( node )
    obkms_id = xml2::xml_attr( node, "obkms_id")
    path = xml2::xml_path( node )
  }

  if ( fullname )
  {
    return (  paste0( strip_angle( obkms$prefixes$`_base`) , obkms_id ) )
  }

  else return ( obkms_id )
}



#' Get the Root (Article) OBKMS Id for an XML Node
#'
#' Does not do any database lookups - only looks at XML
#'
#' @param node
#'
#' @export
root = function (node, xml_schema, xml, mongo_key, prefix = NA, blank = FALSE)
{
  pensoft_xpath = "//article/front/article-meta/uri[@content-type='arpha']"
  arpha_id = xml2::xml_text(xml2::xml_find_first(node, pensoft_xpath))
  if (!(is.na(arpha_id))){
    arpha_id = uuid_dasher(arpha_id)
  }

  plazi_xpath = "//document/@masterDocId"
  plazi_id = xml2::xml_text(xml2::xml_find_first(node, plazi_xpath))
  root_node = xml2::xml_find_all(node, xpath = "/*")

  doi = xml2::xml_text(xml2::xml_find_first(xml, "/article/front/article-meta/article-id[@pub-id-type='doi']"))
  if (is.na(doi)){
    doi = xml2::xml_text(xml2::xml_find_first(xml, "/document/mods:mods/mods:identifier[@type='DOI']"))
  }

  article_ident = xml2::xml_text(xml2::xml_find_all(xml, "//article/front/article-meta/article-id[@pub-id-type='publisher-id']"))
  if (length(article_ident) == 0){
    article_ident = NA #plazi docs don't have this article ident
  }

  if (is.na(arpha_id) && is.na(plazi_id)){
    id = identifier_new(node = root_node, xml = xml, mongo_key = mongo_key,
                        prefix = prefix, blank = FALSE, publisher_id = NA,
                        journal_id = NA, doi = doi, article_id = article_ident)
  }else
  {
    if (!(is.na(arpha_id))){
      article_id = arpha_id
    } else if (!(is.na(plazi_id))){
      #add dashes to uuid if the article is from pensoft
      article_id = uuid_dasher(plazi_id)
    }
  }

  xml2::xml_attr(root_node, "obkms_id") = article_id
  id = identifier(id = article_id, prefix = prefix)
  xml2::xml_attr(node, "obkms_process") = "TRUE"

  title = xml2::xml_text(xml2::xml_find_first(xml, "/article/front/article-meta/title-group/article-title"))
  if (is.na(title)){
    title = xml2::xml_text(xml2::xml_find_first(xml, "/document/mods:mods/mods:titleInfo/mods:title"))
  }


  #check whether the id was saved and save it to mongo only if it wasnt
  res = check_mongo_key(value = title, type = "article", collection = general_collection, regex = FALSE)
  # remove_meta = FALSE
  #if there is no such article id in mongo, save it
  if (is.null(res)){
    save_to_mongo(key = toString(id$uri), value = title, type = "article",
                  orcid = NA, parent = doi, publisher_id = NA, journal_id = NA, plazi_doc = is.plazi_doc(xml),
                  doi = doi, article_id = article_ident,
                  collection = general_collection)
  }
  return(id)
}

#add dashes to uuids which do not have them
#uuids have pattern 8-4-4-4-12
#' @export
uuid_dasher = function(uuid){
  if(nchar(uuid)==32 && grepl("-", uuid)==FALSE){
    s8 = substr(uuid, 1, 8)
    s4_1 = substr(uuid, 9, 12)
    s4_2 = substr(uuid, 13, 16)
    s4_3 = substr(uuid, 17, 20)
    s12 = substr(uuid, 21, 32)
    uuid = paste0(s8, "-", s4_1, "-", s4_2, "-", s4_3, "-", s12)
  }
  return(uuid)
}




#' Strips all XML tags
#'
#' @param xmldoc
#'
#' @return a vector of the document as plain text
#' @export
remove_all_tags = function(xmldoc) {
  sapply(xml_find_all(xmldoc, xpath = paste0("//", "p")), function(p) {
    xml_text(p) = paste(xml_text(p), "\n")
  })
  unescape_html(do.call(pasteconstr(" "), xml_find_all(xmldoc, "//text()")))
}

#' Unescpae HTML Characters
#'
#' @param str input string
#' @return string without the escape characters
#' @export
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>"

  )))
}





#' Simpler version of find literals
#'
#' Finds the Atoms in a XML Node
#'
#' @param xml the XML node
#' @param xpath the atom locations as a named character vector
#'
#' @return list
#'
#' @export
find_atoms =
  function(xml, xpath) {
    lapply(xpath, function(p)
    {
      t = xml2::xml_text(xml2::xml_find_all(xml, p))
      t = strip_starting_whitespace(t)
      t = strip_trailing_whitespace(t)
    })

  }




#' Standard injector. Sets the id from the XPATH
#'
#' @param obkms_id
#' @param xml_node
#'
#' @return
#' @export
standard_injector = function(obkms_id, xml_node)
{
  xml2::xml_attr(xml_node, "obkms_id") = obkms_id
}

