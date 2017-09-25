#' Constructor for Latin name
#'
#' LatinName is a subclass of TaxonomicName.
#'
#' @param id URI, identifier of the name
#'
#' @param kingdom character
#' @param phylum character
#' @param order character
#' @param family character
#' @param genus character
#' @param subgenus character
#' @param species character
#' @param subspecies character
#' @param verbatim_rank character
#' @param rank vocabulary item
#' @param taxonomic_status vocabulary item
#' @param authorship character
#'
#' @return LatinName
#'
#' @export
LatinName = function(id, kingdom, phylum, class, order, family, genus, subgenus,
           species, subspecies, taxonomic_rank, taxonomic_status, verbatim_rank, verbatim_status, authorship)
{
  # get this environment
  this = environment()

  class(this) = c("LatinName", "TaxonomicName")
  this$type = obkms$classes$LatinName$uri
  this$label = get_label(this)

  return (this)
}









#' Constructor for TaxonomicConceptLabel
#'
#' TaxonomicConceptLabel is a subclass of LatinName containing s sec./sensu
#' part.
#'
#' @param id URI, identifier of the name
#'
#' @param kingdom character
#' @param phylum character
#' @param order character
#' @param family character
#' @param genus character
#' @param subgenus character
#' @param species character
#' @param subspecies character
#' @param verbatim_rank character
#' @param rank vocabulary item
#' @param status vocabulary item
#' @param authorship character
#'
#' @param secundum_literal character
#' @param secundum URI
#'
#' @return TaxonomicConceptLabel
#'
#' @export
TaxonomicConceptLabel = function(id, kingdom, phylum, class, order, family,
                                 genus, subgenus, species, subspecies,
                                 taxonomic_rank, taxonomic_status, verbatim_rank, verbatim_status, authorship,
                                 secundum_literal, secundum)
{
  # TODO : bad call based on positions!!!
  this = LatinName(id, kingdom, phylum, class, order, family, genus,
                   subgenus, species, subspecies,  taxonomic_rank, taxonomic_status, verbatim_rank, verbatim_status,  authorship)

  this$secundum_literal = secundum_literal
  this$secundum = secundum

  class(this) = c("TaxonomicConceptLabel", class(this))
  this$type = obkms$classes$TaxonomicConceptLabel$uri
  this$label = get_label(this)

  return(this)
}









#' Transform an Object to TaxonomicName
#'
#' Generic function.
#'
#' @param x the object to be transformed to a taxonomic name
#' @param ... additional parameters passed to the appropriate consturctor
#'
#' @return TaxonomicName
#' @export
as.TaxonomicName = function (x, ...) {
  UseMethod("as.TaxonomicName", x)
}















#' Print Method for TaxonomicName
#'
#' @param obj TaxonomicName
#'
#' @export
print.TaxonomicName = function(obj) {
  properties = setdiff(names(obj), c("this"))
  for(n in properties)
  {
    print(paste(n, "=", obj[[n]]))
  }
}







#' Gets the Label of a Latinized Name
#'
#' @param x Latininzed name (LatinName).
#'
#' @return Character. The label of the Latinized name.
#'
#' @export
get_label.TaxonomicName = function(x) {
  return(get_label_TaxonomicName( x$kingdom, x$phylum, x$class, x$order,
            x$family, x$genus, x$subgenus, x$species, x$subspecies, x$authorship,
            x$secundum_literal))
}




#' Get the Label (Monimial, Bionomial, or Trinomial) from Taxonomic Name Atoms
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
get_label_TaxonomicName = function(kingdom, phylum, class, order,
                         family, genus, subgenus, species, subspecies, authorship,
                         secundum_literal) {
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
    label = gsub("%15", paste0("(", species, ")"), label)
  }
  else {
    label = gsub("%15 ", "", label)
  }

  # replace 1 with the most senior taxon
  senior_ranks = c("kingdom", "phylum", "class", "order", "family", "genus", "subgenus")

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
