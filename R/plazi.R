#' Get Treatment Information from Plazi
#'
#' @param plazi_feed the URL of the Plazi feed
#' @param paths a list of types mapped to paths that will be used to form the columns of the data.frame
#' @return a dataframe of Plazi treatment information
#' @examples
#' plazi_info = plazi_treatment_info()
#' @export
plazi_treatment_info = function(plazi_feed = "http://tb.plazi.org/GgServer/xml.rss.xml", paths = c(link = "/rss/channel/item/link", pub_date = "/rss/channel/item/pubDate")) {
  as.data.frame(lapply(paths, function(p) {
    xml2::xml_text(xml2::xml_find_all(httr::content(httr::GET(plazi_feed)), p))
  }))
}


#' Retrieve Links for Images of a Given Treatment
#'
#' @param treatment_link character(n) of links to treatment XML's
#'
#' @return character(n) of links to images
#'
#' @examples
#' plazi_images("http://tb.plazi.org/GgServer/xml/F58CB27430C0CCBD272770124C31BFF8")
#'
#' @export
plazi_images = function(treatments) {
  unique(unlist(lapply(treatments, function(t) {
    lapply(xml2::xml_find_all(tryCatch({
      httr::content(httr::GET(t))
      }, error = function(e) {
        warning(t)
        return(xml2::read_xml("<xml>Error</xml>"))
      }), "//figureCitation") , function(f) {
      n = names(xml_attrs(f))
      lapply(n[grep("httpUri", n)], function(a) {
        xml2::xml_text(xml2::xml_find_first(f, paste0("./@", a)))
      })
    })
  })), recursive = TRUE)
}



#' Retrieve and Write to Disk Plazi Treatments
#'
#' Side-effect: writes files to disk
#'
#' @param links a character vector containing the links to the XML of treatments
#' @param plazi_pref Plazi endpoint prefix to use for XML type. Defaults to TaxonX
#' @param dir string holding the directory where the files should be written to
#' @return a character vector of the written filenames
#'
#' @examples
#' new_files = get_plazi_treatments(c("http://tb.plazi.org/GgServer/xml/945D879F9E0B1705FF6BF83AFED6FFA4", "http://tb.plazi.org/GgServer/xml/945D879F9E0B1705FE85F856FA26FFA4"), dir = "/media/obkms/plazi-corpus.xml")
#'
#' @export
get_plazi_treatments = function(links, plazi_pref = "http://tb.plazi.org/GgServer/taxonx/", dir) {
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, type="FORK")
  new_files = sapply( links, function(l) {
    id = last_token(l, "/")
    fname = paste0(dir, "/", id, ".xml")
    tryCatch({
      browser()
      #xml2::write_xml(xml2::read_xml(paste0(plazi_pref, id)), fname) -- too slow
      writeLines(httr::content(httr::GET(URLencode(paste0(plazi_pref, id))), as = "text"), con = fname)
      writeBin(httr::content(httr::GET(URLencode(paste0(plazi_pref, id))), as = "raw"), con = fname)
    },
    error = function(e) {
      warning(e)
    })
    return(fname)
  })
  stopCluster(cl)
  return(newFiles)
}
