#' Fetches BLR Record Id's From Zenodo via Zenodeo
#'
#' Access the Zenodo API via a Zenodeo instance stored at endpoint.
#'
#' @param start integer(1) where to start from
#' @param offset integer(1) how many records to fetch
#' @param all logical(1) if true will keep iterating through the API until the end is reached/ fetches all id's; default behaviour is to stop after fetching one page
#' @param endpoint character(1) endpoint address with "/" at the end
#' @param ... further paramters to be passed to the Zenodeo "records" API
#'
#' @return vector('list', n) of BLR id's.
#'
#' @examples
#' zenodeo_fetch_ids(1, 10)
#'
#' @export
zenodeo_fetch_ids = function(start = 1, offset = 10, all = FALSE, endpoint = "http://zenodeo.punkish.org/v1/", ...) {
  browser()
  # curl -X GET --header 'Accept: application/json' 'http://zenodeo.punkish.org/v1/records?page=1&size=10&communities=biosyslit&summary=true'
  fullp = unlist(httr::content(httr::GET(URLencode(paste0(endpoint, "records?", params2string(page = start, size = offset, communities = "biosyslit", summary= TRUE, ...))))))

  regmatches(fullp, regexpr("/[0-9]+$", fullp) + 1)
}



#' Converts a List of Parameters to a URL GET String
#'
#' @param ... named list of character(1)
#'
#' @return character(1) the parameters converted to a string useful for GET
#'
#' @examples
#' params2string(page = 1, size = 10)
#'
#' @export
params2string = function(...)
{
  params = unlist(list(...))
  gsub("&$", "", do.call(paste0, as.list(paste0(names(params), rep("=", n = length(params)), params, rep("&", n = length(params))))))
}

