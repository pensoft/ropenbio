#' Plazi Feed Constructor
#'
#' Constructs a Plazi Feed Object from the Plazi Feed URL
#'
#' @param feed_url string, the URL of the Plazi feed. A default is present.
#'
#' @return plazi_feed, xml_document, the Plazi feed, as a local object.
#'
#' @examples
#' todays_plazi_feed = plazi_feed()
#' xml2::write_xml(todays_plazi_feed, file = "~/tmp/plazi_feed.xml")
#' todays_plazi_feed = plazi_feed("~/tmp/plazi_feed.xml")
#'
#' @export
plazi_feed =
function(feed_url = "http://tb.plazi.org/GgServer/xml.rss.xml")
{
  start_time = Sys.time()
  feed = xml2::read_xml(feed_url)
  class(feed) = append(class(feed), "plazi_feed")
  print("Plazi feed constructor running time:")
  print(Sys.time() - start_time)
  return(feed)
}









#' Plazi Feed to Dataframe Converter
#'
#' Converts the Plazi feed XML to a data frame with interesting columns.
#'
#' @param feed plazi_feed, xml_document, the Plazi feed.
#'
#' @return a `data.frame` and a `plazi_infotable` object containing tabular
#'   information about Plazi treatments
#'
#' @examples
#' a_plazi_feed = plazi_feed()
#' xml2::write_xml(a_plazi_feed, file = "~/tmp/plazi_feed.xml")
#' b_plazi_feed = plazi_feed("/home/viktor2/tmp/plazi_feed.xml")
#' plazi_info = as.data.frame(b_plazi_feed)
#'
#' @export
as.data.frame.plazi_feed =
function(feed)
{
  atoms = find_atoms(feed, plazi_feed_schema$atoms)
  atoms$id = last_token(atoms$link, "/")
  return(data.frame(atoms, stringsAsFactors = FALSE))
}




#' Check Download Status of Plazi Treatment
#'
#' Looks in a Plazi corpus directory and updates a Plazi information frame
#' with it. Tells you which treatments have been downloaded.
#'
#' In description below, "string" refers to a character vector of length 1.
#'
#' @param plazi_id character vector, with Plazi ID's whose status ought to be checked
#' @param corpus_dir string, where the Plazi treatment files have been downloaded.
#' @param xml_schema an object of class `xml_schema`, containing information
#'   about the xml_schema
#'
#' @return logical vector
#'
#' @examples
#' download_status = check_download_status(plazi_info$id,
#'   corpus_dir = "/media/obkms/plazi-corpus-xml", xml_schema = taxonx)
#'
#'
#' @export
check_download_status =
function(plazi_id, corpus_dir, xml_schema )
{
  is.element(plazi_id, strip_filename_extension(list.files(corpus_dir, pattern = xml_schema$file_pattern)))
}









#' Generate `aria2c` Input
#'
#' `aria2c` is a download tool. Its advantage over 'wget' is that it is fast
#' thanks to using multiple connections.
#'
#' This function generates an input file for `aria2c` for Plazi treatments.
#'
#' WILL DOWNLOAD THE ONES THAT ARE FALSE!
#'
#' @param id
#' @param corpus_dir
#'
#' @return character vector, containing the `aria2c` input file
#'
#' @examples
#'
#' @export
generate_aria2c_input =
function(id, corpus_dir)
{
  aria2c_line =
    function(id, prefix, extension)
    {
      c(paste0(prefix, id), paste0("  out=", id, extension))
    }

  aria2c_bunch_lines =
    function(p) # p is the xml schema (i.e. "taxonx" or "plazi_int")
    {
      unlist(lapply(id[!check_download_status(id, corpus_dir, p)], aria2c_line, prefix = p$prefix, extension = p$extension))
    }

  return(as.character(unlist(lapply(list(taxonx, plazi_int), aria2c_bunch_lines))))
}









#' Get Plazi Treatment Language
#'
#' This is a parallelized funciton.
#'
#' @param id a character vector of Plazi treatment ID's.
#' @param directory a string, location of treatments.
#'
#' @return a named character vector where each entry corresponds to an
#'   individual treatment's language.
#'
#' @examples
#'
#' lang = treatment_language(sort(treatments$id)[1:40], directory = "/home/viktor2/tmp")
#'
#' @export
treatment_language =
function(id, directory) {
  no_cores = parallel::detectCores() - 1
  cl = parallel::makeCluster(no_cores, type = "FORK")
  r = parallel::parSapply(cl, id, function(i) {
    tryCatch(
      xml2::xml_text(xml2::xml_find_first(xml2::read_xml(paste0(directory, "/", i, plazi_int$extension)), plazi_int$atoms["lang"])),
      error = function(e) {as.character(NA)}
    )
  })
  parallel::stopCluster(cl)
  return(r)
}









#' Retrieve Links for Images of a Given Treatment
#'
#' @param id
#' @param directory
#'
#' @return character(n) of links to images
#'
#' @examples
#' plazi_images("http://tb.plazi.org/GgServer/xml/F58CB27430C0CCBD272770124C31BFF8")
#'
#' @export
plazi_images = function(id, directory) {
  #unique(unlist(lapply(treatments, function(t) {
  sapply(id, function(t) {
    unlist(lapply(xml2::xml_find_all(tryCatch({
      xml2::read_xml(paste0(directory, "/", t, plazi_int$extension))
      }, error = function(e) {
        warning(t)
        return(xml2::read_xml("<xml>Error</xml>"))
      }), "//figureCitation") , function(f) {
      n = names(xml2::xml_attrs(f))
      lapply(n[grep("httpUri", n)], function(a) {
        xml2::xml_text(xml2::xml_find_first(f, paste0("./@", a)))
      })
    }))
  })
  #})), recursive = TRUE)
}








#' Generate Aria Input For Images
#'
#' @param image_list named list of links to images
#'
#' @return character vector; `aria2c` input file
#'
#' @examples
#' generate_aria2c_input_images(plazi_images(treatments$id, directory =
#'
#'
#'
#'
#'
#'
#'
