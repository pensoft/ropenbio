
#' @export
check_mongo_key = function(value, type, collection, regex)
{
  if (regex == TRUE){
    query = sprintf("{\"%s\":{\"%s\":\"%s\",\"%s\":\"%s\"}}", "value", "$regex", value, "$options", "i")
  }else{
    query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\"}", "value", value, "type", type)
  }
  key = collection$find(query)$key
  return(key)
}

#' @export
check_mongo_parent = function(key, value, type, collection)
{
  if (is.null(key)){
    query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\"}", "value", value, "type", type)
  }else{
    query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\"}", "key", key, "type", type)
  }
  parent = collection$find(query)$parent
  return(parent)
}

 #' @export
  check_mongo_instCode = function(code, collection){
    query = sprintf("{\"%s\":\"%s\"}", "code", code)
    res = collection$find(query)
    return(res)
  }
  
  #' @export
  check_mongo_instName = function(name, collection){
    query = sprintf("{\"%s\":\"%s\"}", "name", name)
    res = collection$find(query)
    return(res)
  }
  

  
  #' @export
  check_mongo_key_via_parent = function(parent,type, collection){
    query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\"}", "parent", parent, "type", type)
    key = collection$find(query)$key
    return(key)
  }
  

#' @export
check_mongo_inst = function(code, parent, collection){
  #search by: code, parent
  #get key

  #query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\",\"%s\":\"%s\"}", "code", code, "parent", parent, "type", "institution")
  query = sprintf("{\"%s\":{\"%s\":\"%s\",\"%s\":\"%s\"}, \"%s\":\"%s\",\"%s\":\"%s\"}", "parent", "$regex", parent, "$options", "i", "code", code, "type", "institution")
  res = collection$find(query)

}


#' @export
check_mongo_key_via_orcid = function(orcid, collection)
{
  query = sprintf("{\"%s\":{\"%s\":\"%s\",\"%s\":\"%s\"}}", "key", "$regex", orcid, "$options", "i")
  collection$find(query)
  key = collection$find(query)$key
  return(key)
}




#' @export
save_to_mongo = function(key, value, type, parent, collection)
{
  d = data.frame(
    key = as.character(key),
    value = as.character(value),
    type = as.character(type),
    parent = as.character(parent)
  )
  collection$insert(d)
}


#' @export
save_orcid_to_mongo = function(key, value, type, parent, orcid, collection)
{
  d = data.frame(
    key = as.character(key),
    value = as.character(value),
    type = as.character(type),
    parent = as.character(parent),
    orcid = as.character(orcid)
  )
  collection$insert(d)
}

#' @export
check_code_name_combo = function(code, name, collection){
    query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\"}", "code", code, "name", name)
    key = collection$find(query)
    return(key)
  }
