#' @export
is.plazi_doc = function(xml){
  if (xml2::xml_name(xml)=="document"){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' @export
is.plazi_pensoft_pub = function(xml){
  res = xml2::xml_find_all(xml, "/document")
  if (length(res)>0){
    res = xml2::xml_find_all(xml, "/document/mods:mods/mods:identifier[@type='Pensoft-Pub']")
    if (length(res) > 0) {
      check = TRUE
    } else{
      check = FALSE
    }
  }else{
    check = FALSE
  }
  
  return(check)
}


#' @export
is.tnu = function(mongo_key){
  if ("genus" %in% names(mongo_key) || "kingdom" %in% names(mongo_key) || "phylum" %in% names(mongo_key) || "order" %in% names(mongo_key) || "subfamily" %in% names(mongo_key) || "subgenus" %in% names(mongo_key) || "subspecies" %in% names(mongo_key) || "secundum_literal" %in% names(mongo_key))
  {check = TRUE
  }
  else{
    check=FALSE
  }
  return(check)
}

#' @export
is.author = function(mongo_key){
  check = "author_name" %in% names(mongo_key)
  return(check)
}

#' @export
is.plazi_author = function(mongo_key){
  check = "plazi_author" %in% names(mongo_key)
  return(check)
}

#' @export
is.figure = function(mongo_key){
  check = "figure" %in% names(mongo_key)
  return(check)
}

#' @export
is.treatment = function(mongo_key){
  check = "treatment" %in% names(mongo_key)
  return(check)
}

#' @export
is.plazi_treatment = function (mongo_key) 
{
  check = "plazi_treatment" %in% names(mongo_key)
  return(check)
}

#' @export
is.plazi_figure = function (mongo_key)
{
  check = "plazi_figure" %in% names(mongo_key)
  return(check)
}