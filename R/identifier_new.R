#' Identifier new
#'
#' A modification of identifier() from rdf4r (queries mongo, instead of openbiodiv)
#'
#' @export
#identifier_new = function (node, xml, mongo_key, prefix = NA, blank = FALSE)
#{
#  if (blank == TRUE) {
#    prefix = c(`_` = "_")
#  }


#  if (is.na(xml2::xml_attr(node, "obkms_id"))) {

#      component_df = process_schema_component(node, mongo_key)
#      id = get_or_set_mongoid(component_df, prefix)
 #     xml2::xml_attr(node, "obkms_id") = id
#    }else {
#    id = xml2::xml_attr(node, "obkms_id")
#  }

#  if(is.null(id))
#    ident = NULL
 # else
#    ident = identifier(id = id, prefix = prefix)
#  ident
#}
#' @export
identifier_new = function (node, xml, mongo_key, prefix = NA, blank = FALSE) 
{
  if (blank == TRUE) {
    prefix = c(`_` = "_")
  }
  
  
 # print(xml2::xml_name(node))
  
  if ((xml2::xml_name(node) == "article") || (xml2::xml_name(node)=="tp:taxon-treatment")){
    id = xml2::xml_attr(node, "obkms_id")
  } else{
  #if (is.na(xml2::xml_attr(node, "obkms_id"))) {
    component_df = process_schema_component(node, mongo_key)
    id = get_or_set_mongoid(component_df, prefix)
    xml2::xml_attr(node, "obkms_id") = id
  }
 # else {
 #   id = xml2::xml_attr(node, "obkms_id")
#  }
  if (is.null(id)) 
    ident = NULL
  else ident = identifier(id = id, prefix = prefix)
  ident
}


#' @export
atoml_to_val = function(atoml)
{
  if (length(atoml) > 0) {
    return(atoml[[1]]$text_value)
  }
  else {
    return (NA)
  }
}
