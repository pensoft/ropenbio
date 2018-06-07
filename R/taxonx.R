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
