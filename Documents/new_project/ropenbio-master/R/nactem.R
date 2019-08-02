#' Convert an XML File to Plain Text NaCTeM Format
#'
#' @param xml_filename the filename (one XML file) to convert to NaCTeM plain text
#' @param TNU_markup
#' @param PTNU_markup
#' @param regPTNU_markup
#' @param xmlschema the schema of the XML document; will be assigned as class to the XML object, once the file is read
#'
#' @return a list of the names that have been extracted and the plain text or an error
#' @export
xml2nactem = function(xml_filename, TNU_markup, PTNU_markup, regPTNU_markup, xmlschema)
{
  xml_doc = tryCatch({
      xml2::read_xml(xml_filename)
    },
    error = function(e) {
      return(e)
  })

  if (!is.error(xml_doc)) {
    class(xml_doc) = c(class(xml_doc), xmlschema)
  }
  else {
    return(xml_doc)
  }

  regularize_taxonomic_names(xml_doc, TNU_markup = TNU_markup, PTNU_markup = PTNU_markup, regPTNU_markup = regPTNU_markup)
  remove_taxpub_markup(xml_doc)

  return(list(names = underscore_taxonomic_names(xml_doc, TNU_markup), text = remove_all_tags(xml_doc)))
}


#' Did you return an error?
#'
#' @param e
#'
#' @return T/F
#'
#' @examples
#' is.error(simpleError("my bad"))
#'
#' @export
is.error = function(e) {
  if ("error" %in% class(e)) {
    TRUE
  }
  else {
    FALSE
  }
}



#' Remove TaxPub markup (Generic)
#'
#' Modifies the XML object by removing superflous Taxpub markup
#'
#' @param xmldoc
#' @param ...
#'
#'
#' @export
#'
remove_taxpub_markup = function(xmldoc, ...) {
  UseMethod("remove_taxpub_markup", xmldoc)
}

remove_taxpub_markup.taxonx = function(xmldoc, ...) {
}

remove_taxpub_markup.taxpub = function(xmldoc, ...) {
  sapply(xml_find_all(xml_doc, "//tp:taxon-authority", ns = c(tp = "http://www.plazi.org/taxpub")), xml_remove) # remove taxpub authorities
  sapply(xml_find_all(xml_doc, "//tp:taxon-name-part[@taxon-name-part-type='infraspecific-rank']", ns = c(tp = "http://www.plazi.org/taxpub")), xml_remove)
  sapply(xml_find_all(xml_doc, "//tp:taxon-name-part[@taxon-name-part-type='subsection']", ns = c(tp = "http://www.plazi.org/taxpub")), xml_remove)
  sapply(xml_find_all(xml_doc, "//tp:taxon-name-part[@taxon-name-part-type='subgenus']", ns = c(tp = "http://www.plazi.org/taxpub")), xml_remove)
  # remove subsect.
  sapply(xml_find_all(xml_doc, "//object-id[@xlink:type='simple']"), xml_remove) # remove links (may appear in names)
}

#' Regularize Taxonomic Names in an XML Document (Generic)
#'
#' @param xmldoc
#' @param ...
#'
#' @return a vector of regularized name parts
#' @export
#'
regularize_taxonomic_names = function(xmldoc, ...) {
  UseMethod("regularize_taxonomic_names", xmldoc)
}

#' Regularize Taxonomic Names in an XML Document (TaxPub)
#'
#' Side-effect: modifies the XML object by regularizing taxonimic names!
#'
#' @param xmldoc the taxonomic article as a XML2 document
#' @param PTNU_markup how taxonomic name parts/ partial taxonomic name usages are marked up
#' @param regPTNU_markup how regularized parts are marked up
#'
#' @return a vector of regularized name parts
#' @export
regularize_taxonomic_names.taxpub = function(xmldoc, TNU_markup, PTNU_markup, regPTNU_markup) {
  sapply(xml_find_all(xmldoc, xpath = paste0("//", PTNU_markup)), function(ptnu) {
    regtext = xml_text(xml_find_first(ptnu, xpath = paste0("./", regPTNU_markup)))
    if (!is.na(regtext)) {
      xml2::xml_text(ptnu) = regtext
    }
    return(xml2::xml_text(ptnu))
  })
}

#' Regularize Plazi XML
#'
#' TODO: generic function
#'
#' @param xmldoc
#'
#' @return a vector of regularized name parts
#' @export
regularize_taxonomic_names.taxonx = function(xmldoc, TNU_markup, PTNU_markup, regPTNU_markup) {
  sapply(xml_find_all(xmldoc, paste0("//", TNU_markup)), function(TNU)
    {
      genus = xml_text(xml_find_first(TNU, "./tax:xmldata/dwc:Genus"))
      species = xml_text(xml_find_first(TNU, "./tax:xmldata/dwc:Species"))
      remove_plazi_metadata(TNU)
      if(!is.na(genus)) {
        if (!is.na(species)) scname = paste(genus, species)
        else scname = genus
        xml_text(TNU) = scname
      }

      return(xml_text(TNU))
  })
}

#' Remove Plazi Metadata from Taxonomic Names
#'
#'
#' Side-effect: modifies the XML doc
#'
#' @param xmldoc
#' @param metadata_markup
#' @param TNU_markup
#'
#' @return the new text of the taxonomic name usages
#' @export
remove_plazi_metadata = function(node, metadata_markup = "tax:xmldata") {
    xml_remove(xml_find_all(node, xpath = paste0("./", metadata_markup)))
}




#' Underscore Taxonimic Names in an XML Document Object
#'
#' Underscores taxonimic names (only keeps the binomial part).
#'
#' Side-effect: modifies the XML object.
#'
#' @param xmldoc an XML2 object in which to underscore the names
#' @param TNU_markup a string with the markup of taxonomic names/ taxonomic name usages
#'
#' @return a character vector of underscored names (only binomial part)
#' @export
underscore_taxonomic_names = function(xmldoc, TNU_markup) {
  as.character(sapply(xml_find_all(xmldoc, xpath = paste0("//", TNU_markup)), function(tnu) {
      uname = do.call(pasteconstr(" "), xml_find_all(tnu, xpath = ".//text()[normalize-space()]")) # text nodes including partent in children, in their order
      uname = do.call(pasteconstr("_"), as.list(binomialize(uname))) # do it again if some node contained a space in it, also only keep the first two
      xml_remove(xml_children(tnu))
      if (!(is.character(uname) & length(uname) == 1)) {
        return("")
      }
      else {
        xml_text(tnu) = uname
        return(uname)
      }
  }))
}

#' Binomialize Taxonomic Name
#'
#' @param taxname character vector with name parts with name to binomialize
#' @return just the binomial name
#'
#' @examples
#'
#' binomialize("Agapostemon (Agapostemon) sericeus (Förster, 1771)")
#' @export
binomialize = function(taxname) {
  nopar = gsub("(?=\\().*?(?<=\\))", "", perl = TRUE, x = taxname)
  nodoublesp = gsub(' +',' ',nopar)
  taxname_vec = unlist(strsplit(nodoublesp, " "))
  if(length(taxname_vec) > 1 && !is.na(taxname_vec[2])) {
    return(taxname_vec[1:2])
  }
  else {
    return(taxname_vec[1])
  }
}
