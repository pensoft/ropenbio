#' Construct TaxonomicNameUsage
#'
#' TODO: should be parameterized with the XML schema (i.e. TaxOub) that the
#' constructor is using.
#'
#' @param node XML node, contains the taxonomic name usage
#'
#' @return TaxonomicNameUsaage
#'
#' @export
TaxonomicNameUsage = function(node) {
  # first call generic constructor (initializes the objects, set id and parent_id)
  this = DocumentComponent(node, obkms$xpath$taxpub$TaxonomicNameUsage,
                           "TaxonomicNameUsage")

  # normalize rank and status (verbatim rank is kept)
  this$taxonomic_rank = taxonomic_rank(this$verbatim_rank)
  this$taxonomic_status = taxonomic_status(this$verbatim_status)

  # add date, same as the metadata
  this$publication_year = this$pub_year
  this$publication_date = paste(this$pub_year, this$pub_month, this$pub_day, sep = "-")

  if ( in_nomenclature_heading ( node ) ) {
    # TODO really bad usage of parent.frame, needs fix
    # this links the two together
    this$name_according_to = parent.frame( n = 4)$metadata$article_id
    this$name_according_to_id = squote(parent.frame( n = 4)$metadata$doi)
  }

  rm("pub_day", envir = this)
  rm("pub_month", envir = this)
  rm("pub_year", envir = this)

  # Methods

  # Does the TNU mention a simple Latin name or a taxonomic concept label?
  this$TaxonomicName_type = function() {
    if ( !is.null ( this$name_according_to  ) )
      return( "TaxonomicConceptLabel" )
    else {
      return( "LatinName" )
    }
  }





  this$normalize_name_part = function( label )
  {
    regular_label = paste0( label, ".reg")
    if (has_meaningful_value(this[[regular_label]])) {
      this[[label]] = this[[regular_label]]
    }
  }

  # normalize genus
  this$normalize_name_part("genus")


  return( this )
}




#' Is the current XML node a child of nomenclature heading?
#'
#' TODO This fucntion should probably get encapusalted inside a TNU
#'
#' @param node XML

in_nomenclature_heading =
  function( node ) {
    location = xml2::xml_path( node )

    # obtains the word after the last / in the location xpath
    last_part = function( location ) {
      x = unlist( strsplit( location, "/" ) )
      x[ length( x) ]
    }
    # TODO following check should come from the scheme (taxpub)
    # checke here is wrong "/article/body/sec[3]/tp:taxon-treatment/tp:nomenclature"
    # just need to compare to the last component
    # TODO if it is nomenclature-citation-list, then it is not in the heading done

    while ( last_part( location ) != "tp:nomenclature" && location != "/" ) {
      if (last_part(location)=="tp:nomenclature-citation-list") {
        return(FALSE)
      }
      node = xml2::xml_parent(  node  )
      location = xml2::xml_path( node )
    }

    if ( last_part( location ) == "tp:nomenclature" ) {

      return( TRUE )
    }
    else return ( FALSE )
  }





#' Transform a TaxonomicNameUsage to a TaxonomicName
#'
#' @param TNU TaxonomicNameUsage
#'
#' @return TaxonomicName of the appropriate sub-class
#'
#' @export
as.TaxonomicName.TaxonomicNameUsage = function(TNU) {
  argument = list(id = lookup_TaxonomicName_id(TNU, root_id(TNU$node, TRUE), TRUE),
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
                  verbatim_status = TNU$verbatim_status,
                  taxonomic_rank = TNU$taxonomic_rank,
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



#' Gets the Label of the Taxonomic Name Mentioned by the Taxonomic Name Usage
#'
#' @param x Taxonomic name usage.
#'
#' @return Character. The label of the mentioned name
#' @export
get_label.TaxonomicNameUsage = function(x) {
  return(get_label_TaxonomicName( x$kingdom, x$phylum, x$class, x$order,
                                  x$family, x$genus, x$subgenus, x$species, x$subspecies, x$authorship,
                                  x$secundum_literal))
}

