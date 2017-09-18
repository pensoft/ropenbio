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
           species, subspecies, verbatim_rank, rank, taxonomic_status, authorship)
{
  # get this environment
  this = environment()

  class(this) = c("LatinName", "TaxonomicName")
  this$type = obkms$classes$LatinName$uri

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
                                 verbatim_rank, rank, taxonomic_status, authorship,
                                 secundum_literal, secundum)
{

  this = LatinName(id, kingdom, phylum, class, order, family, genus,
                   subgenus, species, subspecies, verbatim_rank,
                   rank, taxonomic_status, authorship)

  this$secundum_literal = secundum_literal
  this$secundum = secundum

  class(this) = c("TaxonomicConceptLabel", class(this))
  this$type = obkms$classes$TaxonomicConceptLabel$uri

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









#' Transform a TaxonomicNameUsage to a TaxonomicName
#'
#' @param TNU TaxonomicNameUsage
#'
#' @return TaxonomicName of the appropriate sub-class
#'
#' @export
as.TaxonomicName.TaxonomicNameUsage = function(TNU) {
  argument = list(id = lookup_TaxonomicName_id(TNU),
                  kingdom = TNU$kingdom,
                  phylum = TNU$phylum,
                  class = TNU$class,
                  order = TNU$order,
                  family = TNU$family,
                  genus = TNU$genus,
                  subgenus = TNU$subgenus,
                  species = TNU$species,
                  subspecies = TNU$subspecies,
                  verbatim_rank = TNU$verbatim_rank,
                  rank = TNU$rank,
                  taxonomic_status = TNU$taxonomic_status,
                  authorship = TNU$authorship,
                  secundum_literal = TNU$name_according_to_id,
                  secundum = TNU$name_according_to)

  if(TNU$TaxonomicName_type() == "TaxonomicConceptLabel") {
    this = do.call(TaxonomicConceptLabel, argument)
  }
  else if(TNU$TaxonomicName_type() == "LatinName") {
    # don't want the last two arguments as only applicable to TCL
    this = do.call(LatinName, argument[1:14])
  }
  return(this)
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
