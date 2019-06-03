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
is.figure = function(mongo_key){
  check = "figure" %in% names(mongo_key)
  return(check)
}