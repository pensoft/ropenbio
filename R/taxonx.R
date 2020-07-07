#' @include xml.R

#' TaxonX Schema
#'
#' @export
taxonx = XmlSchema$new(
  schema_name = "taxonx",
  xpath = "/",
  file_pattern = ".*\\.xml",
  extension = ".xml",
  prefix = "http://tb.plazi.org/GgServer/taxonx/",
  atoms = c(
    title = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:titleInfo/mods:title",
    date = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:date",
    doi = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:identifier[@type='DOI']",
    zenodo = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:identifier[@type='Zenodo-Dep']",
    zoobank = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:identifier[@type='ZooBank']",
    publisher = NA,
    journal = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:titleInfo/mods:title",
    journal_abbrev = NA,
    issn = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:identifier[@type='ISSN']",
    eIssn = NA,
    issue = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:detail[@type='issue']/mods:number",
    volume = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:detail[@type='volume']/mods:number",
    starting_page = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:extent[@unit='page']/mods:start",
    ending_page = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:extent[@unit='page']/mods:end",
    pensoft_pub = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:identifier[@type='Pensoft-Pub']"
    ),

  atom_lang = c(
    title = NA,
    date = NA,
    doi = NA,
    zenodo = NA,
    publisher = NA,
    journal = NA,
    journal_abbrev = NA,
    issn = NA,
    eIssn = NA,
    issue = NA,
    volume = NA,
    starting_page = NA,
    ending_page = NA,
    pensoft_pub = NA
  ),

  atom_types = list(
    title = rdf4r::xsd_string,
    date = rdf4r::xsd_date,
    doi = rdf4r::xsd_string,
    zenodo = rdf4r::xsd_string,
    publisher = rdf4r::xsd_string,
    journal = rdf4r::xsd_string,
    journal_abbrev = rdf4r::xsd_string,
    issn = rdf4r::xsd_string,
    eIssn = rdf4r::xsd_string,
    issue = rdf4r::xsd_integer,
    volume = rdf4r::xsd_integer,
    starting_page = rdf4r::xsd_integer,
    ending_page = rdf4r::xsd_integer,
    pensoft_pub = rdf4r::xsd_string
  ),

  constructor = metadata,

  #injector = standard_injector,

  components = list(
    # Author
    XmlSchema$new(
      schema_name = "taxonx_author",
      xpath = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:name[@type='personal']",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = "http://tb.plazi.org/GgServer/taxonx/",
      atoms = c(
        full_name = "./mods:namePart",
        role = "./mods:role/mods:roleTerm"
      ),

      atom_lang = c(
        full_name = NA,
        role = NA
      ),

      atom_types = list(
        full_name = rdf4r::xsd_string,
        role = rdf4r::xsd_string
      ),

      constructor = author,

      components = NULL
    ),
    # Treatment
    XmlSchema$new(
      schema_name = "taxonx_treatment",
      xpath = "/tax:taxonx/tax:taxonxBody/tax:treatment",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = "http://tb.plazi.org/GgServer/taxonx/",
      atoms = c(
        text_content = "."
      ),

      atom_lang = c(
        text_content = NA
      ),

      atom_types = list(
        text_content =  rdf4r::xsd_string
      ),

   #   injector = standard_injector,

      constructor = treatment,

      components = list(
        # Nomenclature
        XmlSchema$new(
          schema_name = "nomenclature_section",
          xpath = ".//tax:nomenclature", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix = "http://tb.plazi.org/GgServer/taxonx/",
          atoms = c(
            text_content = "."
          ),

          atom_lang = c(
            text_content = NA
          ),

          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),

          constructor = nomenclature
        ),

        XmlSchema$new(
          schema_name = "materials_examined",
          xpath = ".//tax:div[@type='materials_examined']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix = "http://tb.plazi.org/GgServer/taxonx/",
          atoms = c(
            text_content = "."
          ),

          atom_lang = c(
            text_content = NA
          ),

          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),

          constructor = materials_examined
        ),

        #taxonx_diagnosis
        XmlSchema$new(
          schema_name = "diagnosis_section",
          xpath = ".//tax:div[@type='diagnosis']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix = "http://tb.plazi.org/GgServer/taxonx/",
          atoms = c(
            text_content = "."
          ),

          atom_lang = c(
            text_content = NA
          ),

          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),

          constructor = diagnosis
        ),



        # distribution
        XmlSchema$new(
          schema_name = "distribution_section",
          xpath = ".//tax:div[@type='distribution']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix = "http://tb.plazi.org/GgServer/taxonx/",
          atoms = c(
            text_content = "."
          ),

          atom_lang = c(
            text_content = NA
          ),

          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),

          constructor = distribution
        )

      )
    ),
    # DISCUSSION
    XmlSchema$new(
      schema_name = "discussion_section",
      xpath = ".//tax:div[@type='discussion']", #rel path from treatment
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = "http://tb.plazi.org/GgServer/taxonx/",
      atoms = c(
        text_content = "."
      ),

      atom_lang = c(
        text_content = NA
      ),

      atom_types = list(
        text_content =  rdf4r::xsd_string
      ),

      constructor = discussion
    ),
    # Taxonomic Key
    XmlSchema$new(
        schema_name = "taxonx_taxonomic_key",
        xpath = "/tax:taxonx/tax:taxonxBody//tax:div[@type='key']",
        file_pattern = ".*\\.xml",
        extension = ".xml",
        prefix = "http://tb.plazi.org/GgServer/taxonx/",
        atoms = c(
          text_content = "."
        ),

      atom_lang = c(
        text_content = NA
      ),

      atom_types = list(
        text_content = rdf4r::xsd_string
      ),

      constructor = taxonomic_key,

      components = NULL
    ),

    # Figure
    XmlSchema$new(
      schema_name = "taxonx_figure",
      xpath = ".//tax:figure",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = "http://tb.plazi.org/GgServer/taxonx/",
      atoms = c(
        text_content = "."
      ),

      atom_lang = c(
        text_content = NA
      ),

      atom_types = list(
        text_content = rdf4r::xsd_string
      ),

      constructor = figure,

      components = NULL
    ),

    # Taxonomic Name Usage
    XmlSchema$new(
      schema_name = "taxonx_taxonomic_name_usage",
      xpath = ".//tax:name",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = "http://tb.plazi.org/GgServer/taxonx/",
      atoms = c(
        date = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:date",
        kingdom = ".//dwc:Kingdom",
        phylum = ".//dwc:Phylum",
        class = ".//dwc:Class",
        order = ".//dwc:Order",
        family = ".//dwc:Family",
        genus = ".//dwc:Genus",
        subgenus = ".//dwc:Subgenus",
        species = ".//dwc:Species",  ## This is an error in TaxonX, not ropenbio!!!
        subspecies = ".//dwc:Subspecies",
        verbatim_rank = ".//dwc:taxonRank",
        verbatim_status = NA,
        authorship = ".//dwc:scientificNameAuthorship",
        secundum_literal = NA
      ),

      atom_lang = c(
        date = NA,
        kingdom = NA,
        phylum = NA,
        class = NA,
        order = NA,
        family = NA,
        genus = NA,
        subgenus = NA,
        species = NA,  ## This is an error in TaxonX, not DwC!
        subspecies = NA,
        verbatim_rank = NA,
        verbatim_status = NA,
        authorship = NA,
        secundum_literal = NA
      ),

      atom_types = list(
        date = rdf4r::xsd_date,
        kingdom = rdf4r::xsd_string,
        class = rdf4r::xsd_string,
        order = rdf4r::xsd_string,
        family = rdf4r::xsd_string,
        genus = rdf4r::xsd_string,
        subgenus = rdf4r::xsd_string,
        species = rdf4r::xsd_string,
        subspecies = rdf4r::xsd_string,
        verbatim_rank = rdf4r::xsd_string,
        taxonomic_rank = rdf4r::xsd_string,
        taxonomic_status = rdf4r::xsd_string,
        authorship = rdf4r::xsd_string,
        secundum_literal = rdf4r::xsd_string
      ),

      constructor = taxonomic_name_usage,

      components = NULL
    )
  )
)




#' @include xml.R

#' Plazi XML schema (not TAXONX)
#'
#' @export
plazi_schema = XmlSchema$new(
  schema_name = "plazi_schema",
  xpath = "/",
  file_pattern = ".*\\.xml",
  extension = ".xml",
  prefix = c(openbiodiv = "http://openbiodiv.net/"),
  atoms = c(
    title = "/document/mods:mods/mods:titleInfo/mods:title",
    date = "/document/mods:mods/mods:relatedItem/mods:part/mods:date",
    doi = "/document/mods:mods/mods:identifier[@type='DOI']",
    zenodo = "/document/mods:mods/mods:identifier[@type='Zenodo-Dep']",
    zoobank = "/document/mods:mods/mods:identifier[@type='ZooBank']",
    gbif_dataset = "/document/mods:mods/mods:identifier[@type='GBIF-Dataset']",
    publisher = NA,
    journal = "/document/mods:mods/mods:relatedItem/mods:titleInfo/mods:title",
    journal_abbrev = NA,
    issn = "/document/mods:mods/mods:identifier[@type='ISSN']",
    eIssn = NA,
    issue = "/document/mods:mods/mods:relatedItem/mods:part/mods:detail[@type='issue']/mods:number",
    volume = "/document/mods:mods/mods:relatedItem/mods:part/mods:detail[@type='volume']/mods:number",
    starting_page = "/document/mods:mods/mods:relatedItem/mods:part/mods:extent[@unit='page']/mods:start",
    ending_page = "/document/mods:mods/mods:relatedItem/mods:part/mods:extent[@unit='page']/mods:end",
    pensoft_pub = "/document/mods:mods/mods:identifier[@type='Pensoft-Pub']",
    pensoft_uuid = "/document/mods:mods/mods:identifier[@type='Pensoft-UUID']"
  ),
  
  atom_lang = c(
    title = NA,
    date = NA,
    doi = NA,
    zenodo = NA,
    gbif_dataset = NA,
    publisher = NA,
    journal = NA,
    journal_abbrev = NA,
    issn = NA,
    eIssn = NA,
    issue = NA,
    volume = NA,
    starting_page = NA,
    ending_page = NA,
    pensoft_pub = NA,
    pensoft_uuid = NA
  ),
  
  atom_types = list(
    title = rdf4r::xsd_string,
    date = rdf4r::xsd_date,
    doi = rdf4r::xsd_string,
    zenodo = rdf4r::xsd_string,
    gbif_dataset = rdf4r::xsd_string,
    publisher = rdf4r::xsd_string,
    journal = rdf4r::xsd_string,
    journal_abbrev = rdf4r::xsd_string,
    issn = rdf4r::xsd_string,
    eIssn = rdf4r::xsd_string,
    issue = rdf4r::xsd_integer,
    volume = rdf4r::xsd_integer,
    starting_page = rdf4r::xsd_integer,
    ending_page = rdf4r::xsd_integer,
    pensoft_pub = rdf4r::xsd_string,
    pensoft_uuid = rdf4r::xsd_string
  ),
  mongo_key = c(plazi_article = "/document/mods:mods/mods:identifier[@type='DOI']"),
  constructor = plazi_metadata,
  
  #injector = standard_injector,
  
  components = list(
    # Author
    XmlSchema$new(
      schema_name = "plazi_author",
      xpath = "/document/mods:mods/mods:name[@type='personal']",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodiv = "http://openbiodiv.net/"),
      atoms = c(
        full_name = "./mods:namePart",
        role = "./mods:role/mods:roleTerm"
      ),
      
      atom_lang = c(
        full_name = NA,
        role = NA
      ),
      
      atom_types = list(
        full_name = rdf4r::xsd_string,
        role = rdf4r::xsd_string
      ),
      # injector = standard_injector,
      mongo_key = c(plazi_author = "/document/mods:mods/mods:name[@type='personal']/mods:namePart"),
      constructor = plazi_author,
      components = NULL
    ),
    # Treatment
    XmlSchema$new(
      schema_name = "plazi_treatment",
      xpath = ".//treatment",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix =  c(openbiodiv = "http://openbiodiv.net/"),
      atoms = c(
        text_content = ".",
        collecting_date = ".//collectingDate/@value",
        collection_code = ".//collectionCode",
        institution_name = ".//collectionCode/@name",
        institution_uri = ".//collectionCode/@httpUri",
        collector_name = ".//collectorName", 
        country = ".//collectingCountry",
        elevation = ".//elevation",
        locality = ".//location",
        type_status = ".//typeStatus",
        specimen_sex =  ".//specimenCount[@type]",
        decimal_lat = ".//geoCoordinate[@orientation='latitude']/@value",
        decimal_long = ".//geoCoordinate[@orientation='longitude']/@value"
      ),
      
      atom_lang = c(
        text_content = NA,
        collecting_date = NA,
        collection_code = NA,
        institution_name = NA,
        institution_uri = NA,
        collector_name = NA,
        country = NA,
        elevation = NA,
        locality = NA,
        type_status = NA,
        specimen_sex =  NA,
        decimal_lat = NA,
        decimal_long = NA
      ),
      
      atom_types = list(
        text_content =  rdf4r::xsd_string,
        collecting_date = rdf4r::xsd_string,
        collection_code = rdf4r::xsd_string,
        institution_name = rdf4r::xsd_string,
        institution_uri = rdf4r::xsd_string,
        collector_name = rdf4r::xsd_string,
        country = rdf4r::xsd_string,
        elevation = rdf4r::xsd_string,
        locality = rdf4r::xsd_string,
        type_status = rdf4r::xsd_string,
        specimen_sex =  rdf4r::xsd_string,
        decimal_lat = rdf4r::xsd_string,
        decimal_long = rdf4r::xsd_string
      ),
      
      #injector = standard_injector,
      mongo_key = c(plazi_treatment = ".//treatment"),
      constructor = plazi_treatment,
      
      components = list(
        # Nomenclature
        XmlSchema$new(
          schema_name = "nomenclature_section",
          xpath = ".//subSubSection[@type='nomenclature']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix =  c(openbiodiv = "http://openbiodiv.net/"),
          atoms = c(
            text_content = "."
          ),
          
          atom_lang = c(
            text_content = NA
          ),
          
          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),
          mongo_key = c(nomenclature = ".//subSubSection[@type='nomenclature']"),
          constructor = nomenclature,
          components = list(
            XmlSchema$new(
              schema_name = "nomenclature_citations-list",
              xpath = "./paragraph", #rel path from nomenclature
              file_pattern = ".*\\.xml",
              extension = ".xml",
              prefix = c(openbiodiv = "http://openbiodiv.net/"),
              atoms = c(
                text_content = "."
              ),
              
              atom_lang = c(
                text_content = NA
              ),
              
              atom_types = list(
                text_content =  rdf4r::xsd_string
              ),
              
              
              mongo_key =  c(nomenclature_citations = "."),
              
              constructor = nomenclature_citations,
              components = list(
                XmlSchema$new(
                  schema_name = "treatment_citation",
                  xpath = "./../../subSubSection[@type='reference_group']/paragraph/treatmentCitationGroup", #rel path from treatment
                  file_pattern = ".*\\.xml",
                  extension = ".xml",
                  prefix = c(openbiodiv = "http://openbiodiv.net/"),
                  atoms = c(
                    text_content = ".", #need to crossreference with the bibliography section,
                    bibr = ".//treatmentCitation/bibRefCitation/@refId"
                  ),
                  
                  atom_lang = c(
                    text_content = NA,
                    bibr = NA
                  ),
                  
                  atom_types = list(
                    text_content =  rdf4r::xsd_string,
                    bibr =  rdf4r::xsd_string
                  ),
                  
                  
                  mongo_key =  c(nomenclature_citation = "."),
                  
                  constructor = plazi_nomenclature_citation,
                  components = NULL
                )
              )
            )
          )
        )
        
        ,
        
        XmlSchema$new(
          schema_name = "materials_examined",
          xpath = ".//subSubSection[@type='materials_examined'] | .//subSubSection[@type='types'] ", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix =  c(openbiodiv = "http://openbiodiv.net/"),
          atoms = c(
            text_content = "."
            #paragraph = ".//paragraph"
          ),
          
          atom_lang = c(
            text_content = NA
          ),
          
          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),
          mongo_key = c(type_material = "subSubSection[@type='materials_examined']"),
          constructor = type_material,
          components = list(  
            XmlSchema$new(
              schema_name = "materials_examined_paragraph",
              xpath = "./paragraph", #rel path from treatment
              file_pattern = ".*\\.xml",
              extension = ".xml",
              prefix =  c(openbiodiv = "http://openbiodiv.net/"),
              atoms = c(
                collecting_date = ".//collectingDate/@value",
                collection_code = ".//collectionCode",
                institution_name = ".//collectionCode/@name",
                institution_uri = ".//collectionCode/@httpUri",
                collector_name = ".//collectorName", 
                country = ".//collectingCountry",
                elevation = ".//elevation",
                locality = ".//location",
                type_status = ".//typeStatus",
                specimen_sex =  ".//specimenCount[@type]",
                decimal_lat = ".//geoCoordinate[@orientation='latitude']/@value",
                decimal_long = ".//geoCoordinate[@orientation='longitude']/@value"
                
              ),
              atom_lang = c(
                collecting_date = NA,
                collection_code = NA,
                institution_name = NA,
                institution_uri = NA,
                collector_name = NA,
                country = NA,
                elevation = NA,
                locality = NA,
                type_status = NA,
                specimen_sex = NA,
                decimal_lat = NA,
                decimal_long = NA
              ),
              
              atom_types = list(
                collecting_date = rdf4r::xsd_string,
                collection_code = rdf4r::xsd_string,
                institution_name = rdf4r::xsd_string,
                institution_uri = rdf4r::xsd_string,
                collector_name = rdf4r::xsd_string,
                country = rdf4r::xsd_string,
                elevation = rdf4r::xsd_string,
                locality = rdf4r::xsd_string,
                type_status = rdf4r::xsd_string,
                specimen_sex =  rdf4r::xsd_string,
                decimal_lat = rdf4r::xsd_string,
                decimal_long = rdf4r::xsd_string
              ),
              
              mongo_key =  c(occurrence_info_list = "."),
              constructor = occurrence_list
            )
          )
        ),
        
        #taxonx_diagnosis
        XmlSchema$new(
          
          schema_name = "diagnosis_section",
          xpath = ".//subSubSection[@type='diagnosis']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix =  c(openbiodiv = "http://openbiodiv.net/"),
          atoms = c(
            text_content = ".",
            collecting_date = ".//collectingDate/@value",
            collection_code = ".//collectionCode",
            institution_name = ".//collectionCode/@name",
            institution_uri = ".//collectionCode/@httpUri",
            collector_name = ".//collectorName", 
            country = ".//collectingCountry",
            elevation = ".//elevation",
            locality = ".//location",
            type_status = ".//typeStatus",
            specimen_sex =  ".//specimenCount[@type]",
            decimal_lat = ".//geoCoordinate[@orientation='latitude']/@value",
            decimal_long = ".//geoCoordinate[@orientation='longitude']/@value"
        ),
        atom_lang = c(
          text_content = NA,
          collecting_date = NA,
          collection_code = NA,
          institution_name = NA,
          institution_uri = NA,
          collector_name = NA,
          country = NA,
          elevation = NA,
          locality = NA,
          type_status = NA,
          specimen_sex = NA,
          decimal_lat = NA,
          decimal_long = NA
        ),
        
        atom_types = list(
          text_content = rdf4r::xsd_string,
          collecting_date = rdf4r::xsd_string,
          collection_code = rdf4r::xsd_string,
          institution_name = rdf4r::xsd_string,
          institution_uri = rdf4r::xsd_string,
          collector_name = rdf4r::xsd_string,
          country = rdf4r::xsd_string,
          elevation = rdf4r::xsd_string,
          locality = rdf4r::xsd_string,
          type_status = rdf4r::xsd_string,
          specimen_sex =  rdf4r::xsd_string,
          decimal_lat = rdf4r::xsd_string,
          decimal_long = rdf4r::xsd_string
        ),
          mongo_key =  c(diagnosis = "."),
          constructor = diagnosis
        ),
        
        
        # distribution
        XmlSchema$new(
          schema_name = "distribution_section",
          xpath = ".//subSubSection[@type='distribution']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix =  c(openbiodiv = "http://openbiodiv.net/"),
          atoms = c(
            text_content = ".",
            collecting_date = ".//collectingDate/@value",
            collection_code = ".//collectionCode",
            institution_name = ".//collectionCode/@name",
            institution_uri = ".//collectionCode/@httpUri",
            collector_name = ".//collectorName", 
            country = ".//collectingCountry",
            elevation = ".//elevation",
            locality = ".//location",
            type_status = ".//typeStatus",
            specimen_sex =  ".//specimenCount[@type]",
            decimal_lat = ".//geoCoordinate[@orientation='latitude']/@value",
            decimal_long = ".//geoCoordinate[@orientation='longitude']/@value"
          ),
          atom_lang = c(
            text_content = NA,
            collecting_date = NA,
            collection_code = NA,
            institution_name = NA,
            institution_uri = NA,
            collector_name = NA,
            country = NA,
            elevation = NA,
            locality = NA,
            type_status = NA,
            specimen_sex = NA,
            decimal_lat = NA,
            decimal_long = NA
          ),
          
          atom_types = list(
            text_content = rdf4r::xsd_string,
            collecting_date = rdf4r::xsd_string,
            collection_code = rdf4r::xsd_string,
            institution_name = rdf4r::xsd_string,
            institution_uri = rdf4r::xsd_string,
            collector_name = rdf4r::xsd_string,
            country = rdf4r::xsd_string,
            elevation = rdf4r::xsd_string,
            locality = rdf4r::xsd_string,
            type_status = rdf4r::xsd_string,
            specimen_sex =  rdf4r::xsd_string,
            decimal_lat = rdf4r::xsd_string,
            decimal_long = rdf4r::xsd_string
          ),
          mongo_key =  c(distribution = "."),
          constructor = distribution
        )
        ,
        # DISCUSSION
        XmlSchema$new(
          schema_name = "discussion_section",
          xpath = ".//subSubSection[@type='discussion']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix =  c(openbiodiv = "http://openbiodiv.net/"),
          atoms = c(
            text_content = ".",
            collecting_date = ".//collectingDate/@value",
            collection_code = ".//collectionCode",
            institution_name = ".//collectionCode/@name",
            institution_uri = ".//collectionCode/@httpUri",
            collector_name = ".//collectorName", 
            country = ".//collectingCountry",
            elevation = ".//elevation",
            locality = ".//location",
            type_status = ".//typeStatus",
            specimen_sex =  ".//specimenCount[@type]",
            decimal_lat = ".//geoCoordinate[@orientation='latitude']/@value",
            decimal_long = ".//geoCoordinate[@orientation='longitude']/@value"
          ),
          atom_lang = c(
            text_content = NA,
            collecting_date = NA,
            collection_code = NA,
            institution_name = NA,
            institution_uri = NA,
            collector_name = NA,
            country = NA,
            elevation = NA,
            locality = NA,
            type_status = NA,
            specimen_sex = NA,
            decimal_lat = NA,
            decimal_long = NA
          ),
          
          atom_types = list(
            text_content = rdf4r::xsd_string,
            collecting_date = rdf4r::xsd_string,
            collection_code = rdf4r::xsd_string,
            institution_name = rdf4r::xsd_string,
            institution_uri = rdf4r::xsd_string,
            collector_name = rdf4r::xsd_string,
            country = rdf4r::xsd_string,
            elevation = rdf4r::xsd_string,
            locality = rdf4r::xsd_string,
            type_status = rdf4r::xsd_string,
            specimen_sex =  rdf4r::xsd_string,
            decimal_lat = rdf4r::xsd_string,
            decimal_long = rdf4r::xsd_string
          ),
          mongo_key =  c(discussion = ".//subSubSection[@type='discussion']"),
          constructor = discussion
        )
        ,
        # Taxonomic Key
        XmlSchema$new(
          schema_name = "taxonomic_key",
          xpath = ".//subSubSection[@type='taxonomicKey']",
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix =  c(openbiodiv = "http://openbiodiv.net/"),
          atoms = c(
            text_content = "."
          ),
          
          atom_lang = c(
            text_content = NA
          ),
          
          atom_types = list(
            text_content = rdf4r::xsd_string
          ),
          mongo_key =  c(taxonomic_key = "."),
          constructor = taxonomic_key,
          components = NULL
        ),
        
        # Figure
        XmlSchema$new(
          schema_name = "figure",
          xpath = ".//figureCitation[@figureDoi]",
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix =  c(openbiodiv = "http://openbiodiv.net/"),
          atoms = c(
            text_content = ".",
            caption = "./@captionText",
            download_link = "./@httpUri",
            doi = "./@figureDoi"
          ),
          
          atom_lang = c(
            text_content = NA,
            caption = NA,
            download_link = NA,
            doi = NA
          ),
          
          atom_types = list(
            text_content = rdf4r::xsd_string,
            caption = rdf4r::xsd_string,
            download_link = rdf4r::xsd_string,
            doi = rdf4r::xsd_string
          ),
          mongo_key =  c(plazi_figure = "."),
          constructor = figure,
          components = NULL
        ),
        
        # Taxonomic Name Usage
        XmlSchema$new(
          schema_name = "taxonomic_name_usage",
          xpath = ".//taxonomicName",
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix =  c(openbiodiv = "http://openbiodiv.net/"),
          atoms = c(
            text_content = ".",
            authorship = "./@authority",
            kingdom = "./@kingdom",
            phylum = "./@phylum",
            class = "./@class",
            order = "./@order",
            family = "./@family",
            genus = "./@genus",
            species = "./@species",  
            rank = "./@rank",
            status = "./@status"
          ),
          
          atom_lang = c(
            text_content = NA,
            authorship = NA,
            kingdom = NA,
            phylum = NA,
            class = NA,
            order = NA,
            family = NA,
            genus = NA,
            species = NA, 
            rank = NA,
            status = NA
          ),
          
          atom_types = list(
            text_content = rdf4r::xsd_string,
            authorship = rdf4r::xsd_string,
            kingdom = rdf4r::xsd_string,
            phylum = rdf4r::xsd_string,
            class = rdf4r::xsd_string,
            order = rdf4r::xsd_string,
            family = rdf4r::xsd_string,
            genus = rdf4r::xsd_string,
            species = rdf4r::xsd_string,
            rank = rdf4r::xsd_string,
            status = rdf4r::xsd_string
          ),
          
          mongo_key = c(
            kingdom = "./@kingdom",
            phylum = "./@phylum",
            class = "./@class",
            order = "./@order",
            family = "./@family",
            subfamily = NA,
            genus = "./@genus",
            subgenus = NA,
            species = "./@species",
            subspecies = NA,
            #rank = "./@rank",
            #status = "./@status",
            authorship = "./@authority",
            secundum_literal = NA
          ),
          constructor = taxonomic_name_usage,
          components = NULL
        ),
        #REFERENCE
        XmlSchema$new(
          schema_name = "reference",
          xpath = ".//bibRefCitation",
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix = c(openbiodiv = "http://openbiodiv.net/"),
          atoms = c(
            reference_id = "./@refId",
            verbatim = ".",
            author = "./@author",
            refString = "./@refString",
            year = "./@year"
          ),
          
          atom_lang = c(
            reference_id = NA,
            verbatim = NA,
            author = NA,
            refString = NA,
            year = NA
            
          ),
          
          atom_types = list(
            reference_id = rdf4r::xsd_string,
            verbatim = rdf4r::xsd_string,
            author = rdf4r::xsd_string,
            refString = rdf4r::xsd_string,
            year = rdf4r::xsd_string
          ),
          mongo_key =  c(reference = "."),
          constructor = plazi_reference,
          
          components = NULL
        )
      )
    )
  )
)




#' Plazi Internal
#'
#' @export
plazi_int =
XmlSchema$new(schema_name = "plazi_int",
              file_pattern = ".*\\.plazixml",
              extension =  ".plazixml",
              prefix = "http://tb.plazi.org/GgServer/xml/",
              atoms = c(lang = "/document/@docLanguage"))

#' Plazi Feed
#'
#' @export
plazi_feed_schema =
XmlSchema$new(schema_name = "plazi_feed",
              atoms = c(link = "/rss/channel/item/link",
                       pub_date = "/rss/channel/item/pubDate"))
