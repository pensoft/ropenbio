
#' @export
check_mongo = function(value, type, collection, regex)
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
check_mongo_via_orcid = function(orcid, collection)
{
  query = sprintf("{\"%s\":{\"%s\":\"%s\",\"%s\":\"%s\"}}", "key", "$regex", orcid, "$options", "i")
  collection$find(query)
  key = collection$find(query)$key
  return(key)    
}

#' @export
save_to_mongo = function(key, value, type, collection)
{
  d = data.frame(
    key = as.character(key),
    value = as.character(value),
    type = as.character(type)
  )
  collection$insert(d)
}

