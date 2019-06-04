#' Identifier factory
#'
#' A modified fn from rdf4r
#'
#'@export
identifier_factory = function (fun, prefixes, def_prefix, schema_name, mongo_key) 
{
  function(label, generate = TRUE) {
    stopifnot(is.list(label))
    if (length(label) == 0) {
      return(NA)
    }
    else {
      for (l in label) {
        if ((is.literal(l) || is.identifier(l))) {
          ii = do.call(fidentifier, list(fun = fun, l, 
                                         prefixes = prefixes))
        }
        else {
          l$fun = fun
          l$prefixes = prefixes
          ii = do.call(fidentifier, l)
        }
        if (!is.na(ii)) {
          return(ii)
        }
      }
    }
    if (is.na(ii) && generate == TRUE) {
      print(xml_schema['schema_name'])
      id = paste0(schema_name$schema_name,"/",names(mongo_key),"/",uuid::UUIDgenerate())
      
      identifier(id = id, prefix = def_prefix)
    return(identifier)
}
    return(NA)
  }
}
