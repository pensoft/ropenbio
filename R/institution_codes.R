
#' @export
get_or_set_inst_id = function(name, url, root_id, prefix, collection, grbio = grbio){
  mongo_res = check_mongo_inst(tpKey = url, collection = collection)
  if (nrow(mongo_res)>0){
    coolURI = mongo_res$coolURI
    code = mongo_res$code
    id = grbio_uri_parser(coolURI)
  }else{
    grbio_res = check_grbio(grbio = grbio, url = url, name = name)
    grbio_uri = grbio_res$coolURI
    code = grbio_res$instCode
    if (length(grbio_uri)==0){
      #set it
      grbio_uri = uuid::UUIDgenerate()
      id = identifier(grbio_uri, prefix = prefix)
      code = NA
    }else{
      id = grbio_uri_parser(grbio_uri)
    }

    #save to mongo
    #key #value  #type #parent
    mongo_df = data.frame(
      tpKey = url,
      coolURI = strip_angle(id$uri),
      name = name,
      code = code,
      parent = strip_angle(root_id$uri)
    )


    collection$insert(mongo_df)
  }

  return(id)
}


#' @export
check_grbio = function(grbio, url, name){
  grbio_df  = read.csv(grbio, header = TRUE)
  #first look for the url if present
  if (is.na(url) == FALSE){
    #res = grbio_tibble %>% filter(url == url)
    res = grbio_df[which(grbio_df$url == url),]

  }else if (is.na(url) || nrow(res)==0){
    res = grbio_df[which(grbio_df$instName == name),]
  }
}

#' @export
grbio_uri_parser = function(grbio_uri){
  #extract grbio uri and turn into id
  #need to detect whether its lsid or a cool uri
  lsid_regex = c(biocol =  "http:\\/\\/biocol\\.org\\/urn:lsid:biocol\\.org:")
  grbio_cool_regex = c(grbioCool = "http:\\/\\/grbio\\.org\\/cool\\/")
  biocol_cool_regex = c(biocolCool = "http:\\/\\/biocol\\.org\\/cool\\/")
  gsrscicol_regex = c(gsrscicoll = "http:\\/\\/grscicoll\\.org\\/cool\\/")
  usfc_regex = c(usfc = "http:\\/\\/usfsc\\.grscicoll\\.org\\/cool\\/")
  default_regex = c(openbiodiv = "http:\\/\\/openbiodiv\\.net\\/")

  patterns = list(lsid_regex, grbio_cool_regex, biocol_cool_regex, gsrscicol_regex, usfc_regex, default_regex)

  grep_and_id = function(pattern, grbio_uri){
    if (grepl(pattern, grbio_uri)){
      grbio_uri = gsub(pattern, "", grbio_uri)
      prefix = stringi::stri_unescape_unicode(pattern)
      names(prefix) = names(pattern)
      id = identifier(grbio_uri, prefix)
      return(id)
    }
  }
 inst_id =  sapply(patterns, grep_and_id, grbio_uri=grbio_uri)
 inst_id=inst_id[lapply(inst_id,length)>0][[1]]
 return(inst_id)
}


#' @export
set_institution_id = function(uri){
  lsid_regex = "http:\\/\\/biocol\\.org\\/urn:lsid:biocol\\.org:"
  grbio_cool_regex = "http:\\/\\/grbio\\.org\\/cool\\/"
  biocol_cool_regex = "http:\\/\\/biocol\\.org\\/cool\\/"
  gsrscicol_regex = "http:\\/\\/grscicoll\\.org\\/cool\\/"
  usfc_regex = "http:\\/\\/usfsc\\.grscicoll\\.org\\/cool\\/"

  if (grepl(lsid_regex, uri)){
    grbio_uri = gsub(lsid_regex, "", uri)
    id = identifier(grbio_uri, c(biocol = "http://biocol.org/urn:lsid:biocol.org:"))
  }else if (grepl(grbio_cool_regex, uri))
  {
    grbio_uri = gsub(grbio_cool_regex, "", uri)
    id = identifier(grbio_uri, c(grbioCool = "http://grbio.org/cool/"))
  } else if (grepl(biocol_cool_regex, uri)){
    grbio_uri = gsub(biocol_cool_regex, "", uri)
    id = identifier(grbio_uri, c(biocolCool = "http://biocol.org/cool"))
  } else if (grepl(gsrscicol_regex, uri)){
    grbio_uri = gsub(gsrscicol_regex, "", uri)
    id = identifier(grbio_uri, c(gsrscicoll = "http://grscicoll.org/cool/"))
  } else if (grepl(usfc_regex, uri)){
    grbio_uri = gsub(usfc_regex, "", uri)
    id = identifier(grbio_uri, c(usfc = "http://usfsc.grscicoll.org/cool/"))
  }else{
    grbio_uri = gsub("http:\\/\\/openbiodiv\\.net\\/", "", uri)
    id = identifier(grbio_uri, c(openbiodiv = "http://openbiodiv.net/"))
  }

  return(id)
}

#' @export
extract_inst_identifiers = function(xml, root_id, prefix, collection, grbio){


  #inst_codes = c()
  inst_names = c()
  inst_urls = c()

  xpath_institutions = "//named-content[@xlink:type='simple'][@content-type='institution']"
  inst =xml2::xml_find_all(xml, xpath_institutions)
  #xpath_codes = "//named-content[@content-type='dwc:institutional_code']"
  #code =xml2::xml_find_all(xml, xpath_codes)

  #inst_lit = xml2::xml_text(inst)
  #code_lit = xml2::xml_text(code)
  #length(unique(inst_lit))
  #length(unique(code_lit))


  #xpath_brace = "//named-content[@xlink:type='simple'][@content-type='institution']/following-sibling::text()[1]"
  #braces = xml2::xml_find_all(xml, xpath_brace)
  #if (length(inst) == length(braces)){
  #  for (r in 1:length(braces)){
  #    if (gsub("\\s+","",braces[r]) == "(" | gsub("\\s+","",braces[r]) == ","){
  #      inst_code = xml2::xml_find_first(braces[r], "following-sibling::named-content[@content-type='dwc:institutional_code']")
  #      inst_codes = c(inst_codes, xml2::xml_text(inst_code))
  #      inst_names = c(inst_names, xml2::xml_text(inst[r]))
  if (length(inst)>0){
    for (r in 1:length(inst)){
      inst_names = c(inst_names, xml2::xml_text(inst[r]))
      inst_urls = c(inst_urls, xml2::xml_attr(inst[r], "href"))
    }


    name_url_df = data.frame(
      names = inst_names,
      urls = inst_urls,
      stringsAsFactors = FALSE
    )

    name_url_df = unique(name_url_df)

    #get or set institution id (from grbio) if name + code combination is the same

    for (n in 1:nrow(name_url_df)){
      inst_id = get_or_set_inst_id(name = name_url_df$names[n], url = name_url_df$urls[n], root_id = root_id, prefix = prefix, collection = collection, grbio = grbio )
    }
  }


}

#' @export
institution_serializer = function (tt, atoms, identifiers)
{
  rdfized_codes = c()
  nid = identifiers$nid

    if (!(is.null(unlist(atoms$institution_name)))) {
      sapply(atoms$institution_name, function(n) {
        tt$add_triple(nid, inst_names, n)
        res = check_mongo_instName(name = n$text_value,
                                   collection = inst_collection)
        if (nrow(res) > 0) {
          for (i in 1:nrow(res)) {
            inst_identifier = grbio_uri_parser(res$coolURI[i])
            tt$add_triple(nid, dwc_inst_id, inst_identifier)
            tt$add_triple(inst_identifier, rdf_type, Institution)
            if (names(inst_identifier$prefix) == "grbioCool" ||
                names(inst_identifier$prefix) == "biocol" ||
                names(inst_identifier$prefix) == "biocolCool" ||
                names(inst_identifier$prefix) == "gsrscicoll" ||
                names(inst_identifier$prefix) == "usfc") {
              tt$add_triple(inst_identifier, rdf_type,
                            GrbioInst)
            }
            tt$add_triple(inst_identifier, has_instName,
                          literal(res$name[i]))
            tt$add_triple(inst_identifier, dwc_inst_code,
                          literal(res$code[i]))
            rdfized_codes = c(rdfized_codes, res$code[i])
          }
        }
      })
    }
    if (!(is.null(unlist(atoms$institution_code)))) {
      sapply(atoms$institution_code, function(n) {
        if (!(n$text_value %in% rdfized_codes)) {
          tt$add_triple(nid, inst_codes, n)
          res = check_mongo_instCode(code = n$text_value,
                                     collection = inst_collection)
          if (nrow(res) > 0) {
            for (i in 1:nrow(res)) {
              inst_identifier = grbio_uri_parser(res$coolURI[i])
              tt$add_triple(nid, dwc_inst_id, inst_identifier)
              tt$add_triple(inst_identifier, rdf_type,
                            Institution)
              if (names(inst_identifier$prefix) == "grbio" ||
                  names(inst_identifier$prefix) == "biocol") {
                tt$add_triple(inst_identifier, rdf_type,
                              GrbioInst)
              }
              tt$add_triple(inst_identifier, dwc_inst_code,
                            literal(res$code[i]))
              tt$add_triple(inst_identifier, has_instName,
                            literal(res$name[i]))
            }
          }
        }
      })
    }


  return(tt)
}


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
  if(length(nodes)>0){
    codes = c()
    names = c()
    for (d in 1:length(nodes)){
      siblings = xml2::xml_siblings(nodes[d])
        for (s in 1:length(siblings)){
            if(xml2::xml_attrs(siblings[s]) == "dwc:institutional_code" || grepl("dwc:institutional_code", toString(siblings[s])) || grepl("abbrev", toString(siblings[s])) || grepl("content-type = \"institution\"", toString(siblings[s]))){
              codes = c(codes, strip_trailing_whitespace(xml2::xml_text(siblings[s])))
              names = c(names, strip_trailing_whitespace(xml2::xml_text(nodes[d])))
              break
            }
          }

    }
    if(!(length(unique(codes))==length(unique(names)))){
      institutions_df = NULL
    }else{
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
    }
  }else{
    institutions_df = NULL
  }

return(institutions_df)
}


#' @export
abbrev_institutions_extractor = function(xml){
  nodes = xml2::xml_find_all(xml, "//abbrev[@content-type = 'institution']")
  if(length(nodes)>0){
  attribs = sapply(nodes, function(x){
    xml2::xml_attrs(x)
  })

  if(!(is.null(nrow(attribs)))){
    names = strip_trailing_whitespace(attribs[2,])
    codes = sapply(nodes, function(x){
      strip_trailing_whitespace(xml2::xml_text(x))
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
  }else{
    institutions_df = NULL
    }
  }else{
    institutions_df = NULL
  }
  return(institutions_df)
}


#' @export
institutions_to_mongo = function(df1, df2, root_id, collection){
  df = rbind(df1, df2)
  if(is.null(df) == FALSE){
    rownames(df) = NULL
    #parent saves the context (article id) in which an institution is used
    toRemove = c()
    for (n in 1:nrow(df)){
      res = check_code_name_combo(code = df[n,]$code, name = df[n,]$name, collection = collection)
      if (length(res)==0){
        df$parent = root_id$uri #only add root_id as parent if the code+name combo is not in mongo (not every time)
      }else{
        toRemove = c(toRemove,n)
      }
    }
    removal <- which(rownames(df) %in% toRemove)
    if (length(removal)>0){
      df = df[-removal,]
    }
    collection$insert(df)
  }

  return(TRUE)
}

#' @export
add_inst_triples = function(atoms, triples, identifiers)
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

