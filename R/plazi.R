#' Constructs a PlaziFeed Object
#'
#' Obtains information about all published Plazi treatments from the Plazi feed
#' URL.
#'
#' @param feed_url string, the URL of the Plazi feed. A default is present.
#'
#' @return PlaziFeed, xml_document, the Plazi Feed.
#'
#' @examples
#' a_PlaziFeed <- PlaziFeed()
#'
#' @export
PlaziFeed =
function(feed_url = "http://tb.plazi.org/GgServer/xml.rss.xml")
{
  cat("This function takes a while to complete.")
  feed = httr::content(httr::GET(feed_url))
  class(feed) = c(class(feed), "PlaziFeed")
  return(feed)
}









#' Convert a PlaziFeed to a Dataframe
#'
#' Converts the PlaziFeed XML to a dataframe with interesting columns.
#'
#' @param feed PlaziFeed, xml_document, the Plazi feed.
#'
#' @return data.frame
#'
#' @examples
#' a_PlaziFeed <- PlaziFeed()
#' plazi_infotable <- as.data.frame(a_PlaziFeed)
#'
#' @export
as.data.frame.PlaziFeed =
function(a_PlaziFeed)
{
  uri <- xml2::xml_text(xml2::xml_find_all(a_PlaziFeed, "/rss/channel/item/link"))
  id <- last_token(uri, "/")
  pub_date <- xml2::xml_text(xml2::xml_find_all(a_PlaziFeed, "/rss/channel/item/pubDate"))
  taxonx <- paste0("http://tb.plazi.org/GgServer/taxonx/", id)
  return(data.frame(taxonx, id, pub_date, stringsAsFactors = FALSE))
}









#' Plazi Treatment Downloader
#'
#' Retrieves and writes to disk Plazi treatments.
#' Requires `aria2c` command line utility.
#'
#' @param uri character, URI's of the XML of treatments to download.
#' @param id character, the URI's of treatments corresponding to the URI's.
#' @param directory string, directory to store the downloaded files.
#' @param logdir string, directory to store the logs.
#'
#' @return doesn't return anything. pure procedure.
#'
#' @examples
#' a_PlaziFeed = PlaziFeed()
#' plazi_infotable = as.data.frame(a_PlaziFeed)
#' download_plazi_treatments(uri = plazi_infotable$taxonx[1:10], id = plazi_infotable$id[1:10], dir = "/home/viktor2/tmp", logdir = "/home/viktor2/tmp")
#'
#' @export
download_plazi_treatments =
function(uri, id, directory, logdir)
{
  input_file_constructor = function(uri, id) {
    stopifnot(length(uri) == length(id))
    input_file_text = character(length = 2*length(uri))
    j = 1
    for (i in 1:length(uri)) {
      input_file_text[j] = uri[i]
      input_file_text[j + 1] = paste0("  out=", id[i], ".xml")
      j = j + 2
    }
    return(input_file_text)
  }

  ctime = format(Sys.time(), "%y%m%d%H%M")
  input_file = paste0(logdir, "/download-", ctime, ".input")
  writeLines(input_file_constructor(uri, id), input_file)
  log_file = paste0(logdir, "/download-", ctime, ".log")
  command = paste0('aria2c ', '--input-file=', input_file, " --log=", log_file, ' --split=16 ', '--dir=', directory)
  system(command)
}









#' Get Plazi Treatment Language
#'
#' @param id character. treatment id's.
#' @param directory string. where the treatments are stored.
#'
#' @return character, languages. NA if file cannot be read.
#'
#' @examples
#' a_PlaziFeed = PlaziFeed()
#' plazi_infotable = as.data.frame(a_PlaziFeed)
#' download_plazi_treatments(uri = plazi_infotable$taxonx[1:100], id = plazi_infotable$id[1:100], dir = "/home/viktor2/tmp", logdir = "/home/viktor2/tmp")
#' get_plazi_treatment_language(id = plazi_infotable$id[1:100], directory = "/home/viktor2/tmp")
#'
#' @export
get_plazi_treatment_language=
function(id, directory) {
  sapply(id, function(a) {
    tryCatch(
      xml_text(xml2::xml_find_first(xml2::read_xml(paste0(directory, "/", a, ".xml")), "/document/@docLanguage")),
      error = function(e) {
        return (NA)
      }
    )
  })
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




