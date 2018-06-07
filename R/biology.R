




#' Converts a Verbatim Status to a Controlled Vocabulary
#' @param verbatim_status string
#'
#' @return identifier from OpenBiodiv
#' @export
verbstat2openbiodiv = function(verbatim_status, vocab_file = paste0(path.package('ropenbio') , "/vocabulary/", "taxonomic-status.yml"), def_prefix)
{
  vocab = yaml::yaml.load_file(vocab_file)
  verbatim_status = trimws(tolower(verbatim_status))

  status_check = as.logical(lapply(vocab, function (status)
    {
    sum (unlist(lapply( status, function(s) {
      grepl(s, verbatim_status)
    })))
  }) )

  if (sum(status_check) > 0) {
    ix = which(status_check)
    return (identifier(names(vocab)[ix], prefix = def_prefix))
  }
  else {
    return(NULL)
  }
}







#' Get the Label (Monimial, Bionomial, or Trinomial Name, or Taxonomic Concept Label) from Taxonomic Name Atoms
#'
#' @param kingdom
#' @param phylum
#' @param class
#' @param order
#' @param family
#' @param genus
#' @param subgenus
#' @param species
#' @param subspecies
#' @param authorship
#' @param secundum_literal
#'
#' @return the correct way to use the name
#'
#' @export
get_scientific_name_or_tcl = function(kingdom = NA, phylum = NA, class = NA, order = NA,
                                   family = NA, subfamily = NA, genus = NA, subgenus = NA, species = NA,
                                   subspecies = NA, authorship = NA, secundum_literal = NA) {
  label = "%1 %15 %2 %3 %4 %5 "
  # do we have authorship?
  if (has_meaningful_value(authorship)) {
    label = gsub(pattern = "%4", replacement = authorship, label)
  }
  else {
    label = gsub(pattern = "%4 ", replacement = "", label)
  }

  # 4 - taxonomic concept label
  if (has_meaningful_value(secundum_literal)) {
    label = gsub(pattern = "%5", replacement = paste("sec.", secundum_literal), label)
  }
  else{
    label = gsub(pattern = "%5 ", replacement = "", label)
  }

  # replace 2 and 3 with epithets or empty
  if (has_meaningful_value(subspecies)) {
    label = gsub("%3", subspecies, label)
  }
  else {
    label = gsub("%3 ", "", label )
  }

  if (has_meaningful_value(species)) {
    label = gsub("%2", species, label)
  }
  else {
    label = gsub("%2 ", "", label)
  }

  if (has_meaningful_value(subgenus)) {
    label = gsub("%15", paste0("(", subgenus, ")"), label)
  }
  else {
    label = gsub("%15 ", "", label)
  }

  # replace 1 with the most senior taxon
  senior_ranks = c("kingdom", "phylum", "class", "order", "family", "subfamily", "genus", "subgenus")

  i  = 7
  while(grepl("%1", label) && i > 0 ) {
    if (has_meaningful_value(get(senior_ranks[i]))) {
      label = gsub("%1", get(senior_ranks[i]), label)
    }
    i = i - 1
  }

  #just kill %1 if we didn't fid anything (could be the case if only infraspecific
  #epithet is provided in the XML)
  if (grepl("%1 ", label)) {
    label = gsub("%1 ", "", label)
  }
  label = trimws(label)
  return(label)
}
