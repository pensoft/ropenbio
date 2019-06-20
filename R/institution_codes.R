#' @export
institutionalizer = function(xml, root_id, collection){
  df1 = dwc_institutions_extractor(xml)
  df2 = abbrev_institutions_extractor(xml)
  institutions_to_mongo(df1, df2, root_id, collection)
}

#' @export
dwc_institutions_extractor = function(xml){
  #extract institution names
  nodes = xml2::xml_find_all(xml, "//named-content[@xlink:type='simple'][@content-type='institution']")
  codes = c()
  names = c()
  for (d in 1:length(nodes)){
    siblings = xml2::xml_siblings(nodes[d])
    for (s in 1:length(siblings)){
      if (xml2::xml_attrs(siblings[s]) == "dwc:institutional_code" || grepl("dwc:institutional_code", toString(siblings[s]))){
        codes = c(codes, xml2::xml_text(siblings[s]))
        names = c(names, xml2::xml_text(nodes[d]))
        break
      }
    }
  }

institutions_df = data.frame(
  key = codes,
  value = names,
  type = "institution"
)
return(institutions_df)
}


#' @export
abbrev_institutions_extractor = function(xml){
  nodes = xml2::xml_find_all(xml, "//abbrev[@content-type = 'institution']")
  attribs = sapply(nodes, function(x){
    xml2::xml_attrs(x)
  })
  names = attribs[2,]
  codes = sapply(nodes, function(x){
    xml2::xml_text(x)
  })

  institutions_df = data.frame(
    key = unique(codes),
    value = unique(names),
    type = "institution"
  )
  return(institutions_df)
}


#' @export
institutions_to_mongo = function(df1, df2, root_id, collection){
  df = rbind(df1, df2)
  #parent saves the context (article id) in which an institution is used
  df$parent = root_id$uri
  collection$insert(df)
  return(TRUE)
}


