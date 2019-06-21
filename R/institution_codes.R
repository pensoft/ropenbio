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

identif = sapply(codes, function(x){
  id = identifier(uuid::UUIDgenerate(), prefix = c(openbiodivInstitution = "http://openbiodiv.net/resource/institution/"))
  id$uri
})
institutions_df = data.frame(
  key = identif,
  code = codes,
  name = names,
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

  codes = unique(codes)
  names = unique(names)
  identif = sapply(codes, function(x){
    id = identifier(uuid::UUIDgenerate(), prefix = c(openbiodivInstitution = "http://openbiodiv.net/resource/institution/"))
    id$uri
  })

  institutions_df = data.frame(
    key = identif,
    code =  codes,
    name = names,
    type = "institution"
  )
  return(institutions_df)
}


#' @export
institutions_to_mongo = function(df1, df2, root_id, collection){
  df = rbind(df1, df2)
  #parent saves the context (article id) in which an institution is used
  df$parent = root_id$uri
  rownames(df) = NULL
  collection$insert(df)
  return(TRUE)
}

#' @export
add_inst_triples = function(atoms, triples)
{
  triples$prefix_list$add(c(openbiodivHasInst = "http://openbiodiv.net/property/hasInstitution"))
  triples$prefix_list$add(c(openbiodivHasInstName = "http://openbiodiv.net/property/hasInstitutionName"))
  triples$prefix_list$add(c(openbiodivHasInstCode = "http://openbiodiv.net/property/hasInstitutionCode"))

  atoms$inst_code = unique(atoms$inst_code)
  #the inst codes within the abstract, also check mongo
  sapply(atoms$inst_code, function(i){
    res = check_mongo_inst(code = i$text_value, parent = identifiers$root_id$id, collection = inst_collection)
    if (length(res) > 0){
      inst_id = strip_angle(res$key)
      inst_id = gsub("^(.*)resource\\/(.*)\\/", "", inst_id)
      inst_name = res$name
      institut = identifier(inst_id, prefix = c(openbiodivInstitution = "http://openbiodiv.net/resource/Institution/"))
      triples$add_triple(identifiers$nid, has_inst, institut)
      triples$add_triple(institut, rdf_type, Institution)
      triples$add_triple(institut, has_instName, literal(inst_name))
      triples$add_triple(institut, has_instCode, i)

    }
  })

  return(triples)
}

