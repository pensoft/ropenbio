#' Identifier new
#'
#' A modification of identifier() from rdf4r (queries mongo, instead of openbiodiv)
#'
#' @export
identifier_new = function (node, xml, schema_name, mongo_key, prefix = NA, blank = FALSE) 
{
  if (blank == TRUE) {
    prefix = c(`_` = "_")
  }
  #check for obkms in xml - if no obkms_id found
  if (is.na(xml2::xml_attr(node, "obkms_id"))) {
    #safety measure for now
    if (!is.na(mongo_key)){
      component_df =  process_schema_component(node, mongo_key)
      id = get_or_set_mongoid(component_df, schema_name, mongo_key)
      xml2::xml_attr(node, "obkms_id") = id
    }} else{
      #if there is an obkms_id, just extract it
      id = xml2::xml_attr(node, "obkms_id")
    }
  ident = identifier(id = id, prefix = prefix)
  ident
}
