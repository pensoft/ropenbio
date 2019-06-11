#' @export
process_bold <- function(file)
{
  suppressWarnings({
    xml <- xml2::read_xml(file)
    #xpath to find all bold systems links within article!
    results <- xml2::xml_find_all(xml, "//*[starts-with(@xlink:href, 'http://www.boldsystems.org/')] | //*[starts-with(@xlink:href, 'http://boldsystems.org/')]")
    if(!(length(results)==0))
    {
      doc <- XML::xmlParse(xml, isHTML = FALSE)
      nodeset <- XML::getNodeSet(doc, "//article-meta")
      #creates parent nodes for the bold-ids or bins section
      parent_id <- XML::newXMLNode("bold-ids", parent = nodeset)
      parent_bin <- XML::newXMLNode("bins", parent = nodeset)
      empty_bin <- TRUE
      empty_bold_id <- TRUE
      for (r in results)
      {
        node_string <- sub('.*http', '', r)
        #handles cases where the node value is either a link or just an id
        #if we have 2 closing tags => the value is just an id
        #1 closing tag means the value contained a link and sub('.*http','', r) matched and replaced the second 'http' (within the value)
        if (stringi::stri_count_regex(node_string, '>') > 1){
          string <- sub('\">?.*', '', node_string)
        }
        else {
          string <- sub('<.*', '', node_string)
        }

        #adds the http part to url_parse the string => break it down into parts and only take the query part
        string <- paste0("http", string)
        result <- xml2::url_parse(string)
        query <- result$query

        #if the query part contains "bin" or "clusteruri" the uri contains a bin
        #else - bold-id (which can be anything, like record-id or process-id)
        if((sub('=.*', '', query) == "bin") || (sub('=.*', '', query) == "clusteruri" ))
        {
          empty_bin = FALSE
          tag = "bin"
          new_node <- XML::newXMLNode(tag, parent = parent_bin)

        } else
        {
          empty_bold_id = FALSE
          tag = "bold-id"
          new_node <- XML::newXMLNode(tag, parent = parent_id)
        }
        value <- sub('.*=', '', query)
        XML::xmlValue(new_node) <- value
      }
      #remove the empty nodesets
      if (empty_bin == TRUE)
      {
        XML::removeNodes(parent_bin)
      }
      if (empty_bold_id == TRUE)
      {
        XML::removeNodes(parent_id)
      }

      #saving_file_name <- paste0(dirname(file), "/modified/",basename(file), "_mod.xml" )
      #saveXML(doc, saving_file_name)
      new_xml = XML::saveXML(doc)
    }
    else{
      new_xml = NULL
    }
    return(new_xml)
  })
}
