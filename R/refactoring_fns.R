#' @export
get_author_label = function(node, mongo_key){
  label = paste0(xml2::xml_text(xml2::xml_find_all(node, mongo_key[1])), " ", xml2::xml_text(xml2::xml_find_all(node, mongo_key[2])))
  return(label)
}

#' @export
get_author_orcid = function(node){
  orcid = xml2::xml_text(xml2::xml_find_first(node, "./uri[@content-type='orcid']"))
  orcid = gsub("^(.*)orcid.org\\/", "", orcid)
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
  if (is.null(tolit)){
    tolit = xml2::xml_text(xml2::xml_find_first(node, "."))
  }
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
set_component_frame = function(label, mongo_key, type, orcid, parent, key)
{

  df = data.frame(label = label, mongo_key = mongo_key, type = type, orcid = orcid, parent = parent, key = key,  stringsAsFactors = FALSE)
  return(df)
}


#' @export
process_author = function (node, mongo_key)
{
  label = get_author_label(node, mongo_key)
  orcid = get_author_orcid(node)
  mongo_key = c(author = "")
  df = set_component_frame(label = label, mongo_key = mongo_key, type = names(mongo_key), orcid = orcid, parent = NA, key = NA)
  return(df)
}


#' @export
process_tnu = function (node, mongo_key)
{
   label = get_taxon_label(node, mongo_key)
   if (nchar(label)<3 && grepl(".", label)==TRUE){ #if the label is something like "B." then don't save it (too ambiguous)
   label = get_taxon_label(node, mongo_key)
   label = escape_special(label)
    df = NULL
  }else{
    mongo_key = c(taxonomic_name = "")
    label = escape_special(label)
    df = set_component_frame(label = label, mongo_key = mongo_key,type = names(mongo_key), orcid = NA, parent=NA, key = NA)
    return(df)
  }

}


#' @export
process_figure  = function (node, mongo_key)
{
  id = xml2::xml_text(xml2::xml_find_all(node, "..//object-id[@content-type='arpha']"))
  if (length(id)>0){
    fig_id = identifier(id, prefix)
    fig_id = fig_id$uri
    id_literal = id
  }else{
    fig_id = NA
    id_literal = ""
  }
  fig_number = xml2::xml_attr(node, "id")

  label = get_figure_label(node, mongo_key, fig_number)
  label = gsub(id_literal, "", label)
  label = escape_special(label)
  type = paste0(names(mongo_key), " ", fig_number)
  df = set_component_frame(label = label, mongo_key = mongo_key, type = type, orcid = NA, parent = NA, key = fig_id)

  return(df)
}

#' @export
process_treatment = function(node, mongo_key){
  #if the treatment id is in the article xml, get it and save it in mongodb
  id = xml2::xml_text(xml2::xml_find_all(node, "tp:nomenclature/tp:taxon-name/object-id[@content-type='arpha']"))
  if (length(id)>0){
    treat_id = identifier(id, prefix)
    id_literal = id
    label = xml2::xml_text(xml2::xml_find_first(node, mongo_key))
    label = gsub(id_literal, "", label)
    label = escape_special(label) #escape special chars
    df = set_component_frame(label = label, mongo_key = mongo_key, type = names(mongo_key), orcid = NA, parent = NA, key = treat_id$uri)
  }else{
    df = process_general_component(node, mongo_key)
  }
  return(df)
}


#' @export
escape_special_json = function(string){

  string =  gsub("\"", "\\\"", string , fixed = TRUE)
  string =  gsub("\r?\n|\r", " ", string)
  string =  gsub("\\“", "\\\\", string , fixed = TRUE)
  string =  gsub("\\”", "\\\\", string , fixed = TRUE)
  string =  gsub("[[\\]\\{\\}()*+?\\^$|#]", "\\\\", string, perl=TRUE)
  string =  gsub("\\n", " ", string, perl=TRUE)
  string =  gsub("\"N", "\"N", string , fixed = TRUE)
  string =  gsub("\"E", "\"E", string , fixed = TRUE)
  string =  gsub("\"S", "\"S", string , fixed = TRUE)
  string =  gsub("\"W", "\"W", string , fixed = TRUE)
  string = gsub("\\\\(?=[WNSE])(?=[a-zA-Z])", "", string,fixed = TRUE)
  string = gsub("\\s{2,}", " ", string)
  string = strip_trailing_whitespace(string)

  #string = stringr::str_extract(string, ".*\\n")
    write(string, "~/unknown_json.txt", append = TRUE)

 # string = strip_trailing_whitespace(string)

  return(string)
}

#' @export
escape_special = function(string){
  string =  gsub("\r?\n|\r", " ", string)
  string =  gsub("\\“", "\\\\", string , fixed = TRUE)
  string =  gsub("\\”", "\\\\", string , fixed = TRUE)
  #  string =  gsub("[-[\\]{}()*+?.,\\^$|#\\s]", "\\\\", string, fixed = TRUE)
  string =  gsub("\"N", "\\\"N", string , fixed = TRUE)
  string =  gsub("\"E", "\\\"E", string , fixed = TRUE)
  string =  gsub("\"S", "\\\"S", string , fixed = TRUE)
  string =  gsub("\"W", "\\\"W", string , fixed = TRUE)
  string =  gsub("\"", "\\\"", string , fixed = TRUE)
  string =  gsub("\\\\", "\\", string , fixed = TRUE)
  #string = gsub("\\", " ", string, fixed = TRUE)
  string = gsub("\\]", "\\\\\\]", string)
  string = gsub("\\[", "\\\\\\[", string)
  string = gsub("–", "-", string)
  string = gsub("\\r\\n", " ", string)
  string = gsub("\\\\(?=[WNSE])(?=[a-zA-Z])", "", string,fixed = TRUE)
  string = gsub("\\s{2,}", " ", string)  
  string = strip_trailing_whitespace(string)

  
  #string = stringr::str_extract(string, ".*\\n")
  write(string, "~/unknown.txt", append = TRUE)

 # string = trimws(string)
  #string = gsub("[^\x00-\x7F]+", string)
  # string =  gsub("(?<=[^\\])\"", "\\\"", string , fixed = TRUE)

  return(string)
}

#' @export
process_general_component = function (node, mongo_key)
{
  label = xml2::xml_text(xml2::xml_find_first(node, mongo_key))
  
  id = xml2::xml_text(xml2::xml_find_all(node, ".//object-id[@content-type='arpha']"))
  if (length(id)>0){
    print("this is the id")
    print(id)
    print(toString(node))
    label = gsub(id, "", label)
  }

  
  label = escape_special(label) #escape special chars
  #label = escape_special_json(label)
  print(label)
  df = set_component_frame(label = label, mongo_key = mongo_key, type = names(mongo_key), orcid = NA, parent = NA, key = NA)
  print(df)
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
  } else if (is.treatment(mongo_key) == TRUE){
    df = process_treatment(node, mongo_key)
  } else{
    df = process_general_component(node, mongo_key)
  }
  return(df)
}

#' @export
get_or_set_mongoid= function (df, prefix)
{
  general_collection = mongolite::mongo("new_collection")
  if (is.null(df) == FALSE){
    #check whether the type is treatment and the id is not na


    if (!(is.na(df$key))){
      key = df$key
      save_to_mongo(key = key, value = df$label, type = df$type, parent = NA, collection = general_collection)
      key = toString(key)
      id = rdf4r::strip_angle(key)
      id = gsub("http:\\/\\/openbiodiv\\.net\\/", "", id)
    } else{
        if (!(is.na(df$orcid))) {
          key = check_mongo_key_via_orcid(df$orcid, general_collection)
          id = get_or_set(key, df)
        }
        else {
          if (df$type == "taxonomicConcept"){
            key = check_mongo_key_via_parent(parent = df$parent, type = df$type, collection = general_collection)
            id = get_or_set(key, df)
          } else if (df$type == "tnu"){
            id = get_or_set(NULL, df)
          }
          else
          {

          key = check_mongo_key(value = df$label, type = df$type, collection = general_collection, regex = FALSE)
          id = get_or_set(key, df)
          print(id)
          }
        }
      }
   }else{
    id = NULL
  }

  return(id)
}

#' @export
get_or_set = function(key, df){
  if (is.null(key) == TRUE) {
    key = uuid::UUIDgenerate()
    save_to_mongo(key = identifier(key, prefix)$uri, value = df$label, type = df$type, parent = df$parent, collection = general_collection)
    id = key
  }else{
    id = rdf4r::strip_angle(key)
    id = gsub("http:\\/\\/openbiodiv\\.net\\/", "", id)
  }
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
  return(string)
}
