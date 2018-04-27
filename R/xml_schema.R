#' @include metadata.R
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
    file_pattern = NULL,
    extension = NULL,
    prefix = NULL,
    atoms = NULL,
    atom_types = NULL,
    atom_lang = NULL,
    constructor = NULL,
    components = NULL,

    initialize =
    function(schema_name = NA, file_pattern = NA, extension = NA, prefix = NA, atoms = NA, atom_types = NULL, atom_lang = NA, constructor = NULL)
    {
      self$schema_name = schema_name
      self$file_pattern = file_pattern
      self$extension = extension
      self$prefix = prefix
      self$atoms = atoms
      self$atom_lang = atom_lang
      self$atom_types = atom_types
      self$constructor = constructor
    }
  )
)


#' TaxonX
#'
#' @export
taxonx =
XmlSchema$new(
  schema_name = "taxonx",
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
    issn = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:identifier[@type='ISSN']",
    issue = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:detail[@type='issue']/mods:number",
    volume = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:detail[@type='volume']/mods:number",
    starting_page = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:extent[@unit='page']/mods:start",
    ending_page = "/tax:taxonx/tax:taxonxHeader/mods:mods/mods:relatedItem/mods:part/mods:extent[@unit='page']/mods:end"
    ),

  atom_lang = c(
    title = NA,
    date = NA,
    doi = NA,
    zenodo = NA,
    publisher = NA,
    journal = NA,
    issn = NA,
    issue = NA,
    volume = NA,
    starting_page = NA,
    ending_page = NA
  ),

  atom_types = list(
    title = rdf4r::xsd_string,
    date = rdf4r::xsd_date,
    doi = rdf4r::xsd_string,
    zenodo = rdf4r::xsd_string,
    publisher = rdf4r::xsd_string,
    journal = rdf4r::xsd_string,
    issn = rdf4r::xsd_string,
    issue = rdf4r::xsd_integer,
    volume = rdf4r::xsd_integer,
    starting_page = rdf4r::xsd_integer,
    ending_page = rdf4r::xsd_integer
  ),

  constructor = metadata
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
