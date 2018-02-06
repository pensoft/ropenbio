#' R6 Class for XML Schemas
#'
#' @field schema_name the name of the schema
#' @field file_pattern regular expression pattern matching the file-name extension
#' @field extension the file-name extension
#' @field prefix the prefix that these documents have in the URI
#' @atoms named character vector of xpath locations
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

    initialize =
    function(schema_name = NA, file_pattern = NA, extension = NA, prefix = NA, atoms = NA)
    {
      self$schema_name = schema_name
      self$file_pattern = file_pattern
      self$extension = extension
      self$prefix = prefix
      self$atoms = atoms
    }
  )
)


#' TaxonX
#'
#' @export
taxonx =
XmlSchema$new(schema_name = "taxonx",
              file_pattern = ".*\\.xml",
              extension = ".xml",
              prefix = "http://tb.plazi.org/GgServer/taxonx/")


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
