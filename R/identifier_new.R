#' Identifier new
#'
#' A modification of identifier() from rdf4r (queries mongo, instead of openbiodiv)
#'
#' @export
identifier_new = function (node, xml, mongo_key, prefix = NA, blank = FALSE)
{
  if (blank == TRUE) {
    prefix = c(`_` = "_")
  }
  if (is.na(xml2::xml_attr(node, "obkms_id"))) {
    if (!is.na(mongo_key)) {
      component_df = process_schema_component(node, mongo_key)
      id = get_or_set_mongoid(component_df, prefix)

      xml2::xml_attr(node, "obkms_id") = id
    }
  }
  else {
    id = xml2::xml_attr(node, "obkms_id")
  }
  ident = identifier(id = id, prefix = prefix)
  ident
}
