#' @export
get_author_label = function(node, mongo_key){
  label = paste0(xml2::xml_text(xml2::xml_find_all(node, mongo_key[1])), " ", xml2::xml_text(xml2::xml_find_all(node, mongo_key[2])))
  return(label)
}

#' @export
get_author_orcid = function(node){
  orcid = xml2::xml_text(xml2::xml_find_first(node, "./uri[@content-type='orcid']"))
  orcid = gsub("^(.*)orcid.org\\/", "", orcid)
  #print(orcid)
  return(orcid)
}

#' @export
get_taxon_label = function(node, mongo_key)
{
  b=NULL
  x = sapply(mongo_key, function(i){
    label = xml2::xml_text(xml2::xml_find_all(node, i))
    if(length(label) > 0 && label!="" && label!=" "){
      b = c(b, label)
      b
    }
  })

  tolit = do.call(get_scientific_name_or_tcl, x)
  label = tolit
  label
}

#' @export
get_figure_label = function (node, mongo_key, fig_number)
{
  caption_xpath = paste0("//fig[@id='", fig_number, "']", "|//fig-group[@id='", fig_number, "']")
  label = xml2::xml_text(xml2::xml_find_first(node, caption_xpath))
  label
}


#' @export
set_component_frame = function(label, mongo_key, type, orcid, parent)
{
  df = data.frame(label = label, mongo_key = mongo_key, type = type, orcid = orcid, parent = parent)
  df
}

#' @export
process_author = function (node, mongo_key)
{
  label = get_author_label(node, mongo_key)
  orcid = get_author_orcid(node)
  mongo_key = c(author = "")
  df = set_component_frame(label = label, mongo_key = mongo_key, type = names(mongo_key), orcid = orcid, parent = NA)
  return(df)
}


#' @export
process_tnu = function (node, mongo_key)
{
  label = get_taxon_label(node, mongo_key)
  if (nchar(label)<3 && grepl(".", label)==TRUE){ #if the label is something like "B." then don't save it (too ambiguous)
    df = NULL
    }else{
    mongo_key = c(taxonomic_name = "")
    df = set_component_frame(label = label, mongo_key = mongo_key,type = names(mongo_key), orcid = NA, parent=NA)
    return(df)
  }

}


#' @export
process_figure =  function (node, mongo_key)
{
  fig_number = xml2::xml_attr(node, "id")
  label = get_figure_label(node, mongo_key, fig_number)
  label = escape_special(label)
  type = paste0(names(mongo_key), " ", fig_number)
  df = set_component_frame(label = label, mongo_key = mongo_key, type = type, orcid = NA, parent = NA)
  return(df)
}


#' @export
process_general_component = function (node, mongo_key)
{
  label = xml2::xml_text(xml2::xml_find_first(node, mongo_key))
  label = escape_special(label) #escape special chars

  df = set_component_frame(label = label, mongo_key = mongo_key, type = names(mongo_key), orcid = NA, parent = NA)
  return(df)
}

#' @export
process_schema_component = function(node, mongo_key)
{
  if (is.author(mongo_key) == TRUE){
    df = process_author(node, mongo_key)
  } else if (is.tnu(mongo_key) == TRUE){
    df = process_tnu(node, mongo_key)
  } else if (is.figure(mongo_key) == TRUE){
    df = process_figure(node, mongo_key)
  } else{
    df = process_general_component(node, mongo_key)
  }
  df
}

#' @export
get_or_set_mongoid= function (df, prefix)
{
  general_collection = mongolite::mongo("new_collection")
 # print(df)
  if (is.null(df) == FALSE){
    if (!(is.na(df$orcid))) {
      key = check_mongo_key_via_orcid(df$orcid, general_collection)
      if (is.null(key) == TRUE) {
        key = df$orcid
        save_to_mongo(key = identifier(key, prefix)$uri, value = df$label, type = df$type, parent = NA, collection = general_collection)
        id = key
      }
      id = rdf4r::strip_angle(key)
      # id = stringr::str_extract(id, "(?:.(?!\\/)){36}$")
      id = gsub("^(.*)resource\\/(.*)\\/", "", id) #only get the uuid part of the id
    }
    else {

      key = check_mongo_key(value = df$label, type = df$type, collection = general_collection,
                        regex = FALSE)
      if (is.null(key) == TRUE) {
        key = uuid::UUIDgenerate()
        save_to_mongo(key = identifier(key, prefix)$uri, value = df$label, type = df$type, parent = NA,
                      collection = general_collection)
        id = key
      }else{
        id = rdf4r::strip_angle(key)
        # id = stringr::str_extract(id, "(?:.(?!\\/)){36}$")
        id = gsub("^(.*)resource\\/(.*)\\/", "", id) #only get the uuid part of the id
      }

      #print(id)
    }
  }else{
    id = NULL
  }

  return(id)
}

#' @export
escape_special = function(string){
  string =  gsub("\"", "\\\"", string , fixed = TRUE)
  string =  gsub("\r?\n|\r", " ", string)
  string =  gsub("\\“", "\\\\", string , fixed = TRUE)
  string =  gsub("\\”", "\\\\", string , fixed = TRUE)
  string =  gsub("[-[\\]{}()*+?.,\\^$|#\\s]", "\\\\", string, fixed = TRUE)

}

#' @export
#replace all double quotes with sngle quotes
double_quote_replacer = function(x){
  for (a in 1:length(x)){
    x[[a]]$text_value = gsub("(?<=[\\w\\s(])(\")(?=[\\.\\w\\s)]{3}[^\n])", "'", x[[a]]$text_value  ,perl=TRUE)
    x[[a]]$squote = gsub("(?<=[\\w\\s(])(\")(?=[\\.\\w\\s)]{3}[^\n])", "'", x[[a]]$squote  ,perl=TRUE)

  }
  x
}

#' @export
strip_trailing_whitespace = function(string){
  string = gsub("\\s*$", "", string)
}
