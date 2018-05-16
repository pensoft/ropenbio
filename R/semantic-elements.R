Collection = rdf4r::identifier(
  id = "Collection",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)

Thing = rdf4r::identifier(
  id = "Thing",
  prefix = c(owl = "http://www.w3.org/2002/07/owl#")
)

Agent = rdf4r::identifier(
  id = "Agent",
  prefix = c(foaf = "http://xmlns.com/foaf/0.1/")
)

#' Person
#' @export
Person = rdf4r::identifier(
  id = "Person",
  prefix = c(foaf = "http://xmlns.com/foaf/0.1/")
)

#' Publisher
#' @export
Publisher = rdf4r::identifier(
  id = "Publisher",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)

#' Organization
#' @export
Organization = rdf4r::identifier(
  id = "Organization",
  prefix = c(org = "http://www.w3.org/ns/org#")
)

#' To specify the components of the delivery address for the object
#' @export
Address = rdf4r::identifier(
  id = "Address",
  prefix = c(vcard = "http://www.w3.org/2006/vcard/ns#")
)

#' a role of agent in time
#' @export
Role = rdf4r::identifier(
  id = "RoleInTime",
  prefix = c(pro = "http://purl.org/spar/pro/")
)

#' a creative or scientific work
#' @export
Work = rdf4r::identifier(
  id = "Work",
  prefix = c(fabio = "http://purl.org/spar/fabio/")
)

#' a science journal
#' @export
Journal = rdf4r::identifier(
  id = "Journal",
  prefix = c(fabio = "http://purl.org/spar/fabio/")
)

#' an article in a science journal
#' @export
Article = rdf4r::identifier(
  id = "JournalArticle",
  prefix = c(fabio = "http://purl.org/spar/fabio/")
)

#' Treatment Section
#' @export
Treatment = rdf4r::identifier(
  id = "Treatment",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)


#' Paper
#' the informational content of an article
#' @export
Paper = rdf4r::identifier(
  id = "ResearchPaper",
  prefix = c(fabio = "http://purl.org/spar/fabio/")
)


#' Taxonomic Concept
#'  the informational content of an article
#' @export
TaxonomicConcept = rdf4r::identifier(
  id = "TaxonomicConcept",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)


#' Nomenclature
#'  the informational content of an article
#' @export
Nomenclature = rdf4r::identifier(
  id = "NomenclatureSection",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)


#' Nom Citations
#' the informational content of an article
#' @export
NomenclatureCitationsList = rdf4r::identifier(
  id = "NomenclatureCitationsList",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)


#' TNU
#' @export
TaxonomicNameUsage = rdf4r::identifier(
  id = "TaxonomicNameUsage",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)


#' Scientific Name
#' @export
ScientificName= rdf4r::identifier(
  id = "ScientificName",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)



#' Diagnosis
#' @export
Diagnosis = rdf4r::identifier(
  id = "DiagnosisSection",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)



#' Discussion
#' @export
Discussion = rdf4r::identifier(
  id = "Discussion",
  prefix = c(orb = "http://purl.org/orb/1.0/")
)


#' Distribution
#' @export
Distribution = rdf4r::identifier(
  id = "DistributionSection",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)




#' Distribution
#' @export
TaxonomicKey = rdf4r::identifier(
  id = "TaxonomicKey",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)




#' Distribution
#' @export
MaterialsExamined = rdf4r::identifier(
  id = "MaterialsExamined",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)




#' Figure:
#'  uri: <http://purl.org/spar/doco/Figure>
#'   comment: a figure in a science article
Figure = rdf4r::identifier(
  id = "Figure",
  prefix = c(doco = "http://purl.org/spar/doco/")
)


#' Abstract:
#'   uri: <http://salt.semanticauthoring.org/ontologies/sro#Abstract>
#' comment: an abstract of a science article
#
Abstract = rdf4r::identifier(
  id = "Abstract",
  prefix = c(sro = "http://salt.semanticauthoring.org/ontologies/sro#")
)



#' Introduction:
#'   uri: http://purl.org/spar/deo/Introduction>
#' comment: an abstract of a science article
#
Introduction = rdf4r::identifier(
  id = "Introduction",
  prefix = c(deo = "http://purl.org/spar/deo/")
)



# Title:
#   uri: <http://purl.org/spar/doco/Title>
#   comment: the title of an article
#
Title = rdf4r::identifier(
  id = "Title",
  prefix = c(doco = "http://purl.org/spar/doco/")
)


#' Issn property
#' @export
issn = rdf4r::identifier(
  id = "issn",
  prefix = c(fabio = "http://purl.org/spar/fabio/")
)

#' e-Issn property
#' @export
eissn = rdf4r::identifier(
  id = "eIssn",
  prefix = c(fabio = "http://purl.org/spar/fabio/")
)

#' e-Issn property
#' @export
frbr_part = rdf4r::identifier(
  id = "part",
  prefix = c(frbr = "http://purl.org/vocab/frbr/core#")
)

#' SKOS Preferred Label
#' @export
pref_label = rdf4r::identifier(
  id = "prefLabel",
  prefix = c(skos = "http://www.w3.org/2004/02/skos/core#")
)

#' SKOS Alternative Label
#' @export
alt_label = rdf4r::identifier(
  id = "altLabel",
  prefix = c(skos = "http://www.w3.org/2004/02/skos/core#")
)

#' DC Elements Title
#' @export
dc_title = rdf4r::identifier(
  id = "title",
  prefix = c(dc = "http://purl.org/dc/elements/1.1/")
)

#' Has a DOI
#' @export
has_doi = rdf4r::identifier(
  id = "doi",
  prefix = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")
)

#' Has a Publisher
#' @export
has_publisher = rdf4r::identifier(
  id = "publisher",
  prefix = c(dc = "http://purl.org/dc/elements/1.1/")
)

#' Has a Publisher
#' @export
has_publisher_id = rdf4r::identifier(
  id = "publisher",
  prefix = c(dcterms = "http://purl.org/dc/terms/")
)

#' Realization Of
#' @export
realization_of = rdf4r::identifier(
  id = "realizationOf",
  prefix = c(frbr = "http://purl.org/vocab/frbr/core#")
)

#' Realization Of
#' @export
realization = rdf4r::identifier(
  id = "realization",
  prefix = c(frbr = "http://purl.org/vocab/frbr/core#")
)

#' Creator (Author)
#' @export
creator = rdf4r::identifier(
  id = "creator",
  prefix = c(dcterms = "http://purl.org/dc/terms/")
)


#' Creator (Author)
#' @export
is_contained_by = rdf4r::identifier(
  id = "isContainedBy",
  prefix = c(po = "http://www.essepuntato.it/2008/12/pattern#")
)


#' Creator (Author)
#' @export
has_content = rdf4r::identifier(
  id = "hasContent",
  prefix = c(c4o = "http://purl.org/spar/c4o/")
)




#' mentions
#' @export
mentions = rdf4r::identifier(
  id = "mentions",
  prefix = c(pkm = "http://proton.semanticweb.org/protonkm#")
)


#' publication_date
#' @export
publication_date = rdf4r::identifier(
  id = "publicationDate",
  prefix = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")
)


#' Dwc Kingdom
#' @export
dwc_kingdom = rdf4r::identifier(
  id = "kingdom",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)



#' Dwc Phylum
#' @export
dwc_phylum = rdf4r::identifier(
  id = "phylum",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)



#' Dwc class
#' @export
dwc_class = rdf4r::identifier(
  id = "class",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)



#' Dwc order
#' @export
dwc_order = rdf4r::identifier(
  id = "order",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)



#' Dwc order
#' @export
dwc_family = rdf4r::identifier(
  id = "family",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)




#' Dwc genus
#' @export
dwc_genus = rdf4r::identifier(
  id = "genus",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)


#' Dwc genus
#' @export
dwc_subgenus = rdf4r::identifier(
  id = "subgenus",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)


#' Dwc genus
#' @export
dwc_species_ep = rdf4r::identifier(
  id = "specificEpithet",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)

#' Dwc genus
#' @export
dwc_subspecies_ep = rdf4r::identifier(
  id = "infraspecificEpithet",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)



#' Dwc genus
#' @export
dwc_subspecies_ep = rdf4r::identifier(
  id = "infraspecificEpithet",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)


#' Dwc Verbatim Rank
#' @export
has_verbatim_rank = rdf4r::identifier(
  id = "verbatimTaxonRank",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)



#' Dwc  Rank
#' @export
has_taxonomic_rank = rdf4r::identifier(
  id = "taxonRank",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)


#' Dwc Status
#' @export
has_taxonomic_status = rdf4r::identifier(
  id = "taxonomicStatus",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)


#' Dwc Authorshiop
#' @export
dwc_authorship = rdf4r::identifier(
  id = "scientificNameAuthorship",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)


#' Secundum string
#' @export
has_secundum_string = rdf4r::identifier(
  id = "nameAccordingTo",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)


#' Has Scientific Name
#' @export
has_scientific_name = rdf4r::identifier(
  id = "scientificName",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
)


#
# Table:
#   uri: <http://purl.org/spar/doco/Table>
#   comment: a table in a science article
#
# References:
#   uri: <http://purl.org/spar/doco/BibliographicReferenceList>
#   comment: a list of references at the end of a science article
#
# Acknowledgement:
#   uri: <http://purl.org/spar/doco/Afterword>
#   comment: an acknowledgement or afterword
#
# BackMatter:
#   uri: <http://purl.org/spar/doco/BackMatter>
#   comment: backmatter at the end of a science article
#
# Body:
#   uri: <http://purl.org/spar/doco/BodyMatter>
#   comment: body of an article
#

# FrontMatter:
#   uri: <http://purl.org/spar/doco/FrontMatter>
#   comment: the front-matter of an article
#
# Caption:
#   uri: <http://purl.org/spar/deo/Caption>
#   comment: a caption, e.g. of a figure
#
# #Reference: doco:Reference #
# #tnu: trt:TaxonNameUsage
# #plate: doco:Plate
# #scientific_name: trt:ScientificName
# #nomenclature: trt:Nomenclature
# #act: trt:NomenclaturalAct
# #valid_name: trt:ValidName
# #nomenclatural_citation: trt:NomenclaturalCitation
#
# DbpediaBiologicalThing :
#   uri: <http://umbel.org/umbel/rc/BiologicalLivingObject>
#   comment: the super-class for all species and higher-level taxa in dbpedia
#
# Subject_Term:
#   uri: <http://purl.org/spar/fabio/SubjectTerm>
#   comment: every keyword is in instance of subject term in some of the vocabularies
#
# LatinName:
#   uri: <http://openbiodiv.net/LatinName>
#
#   TaxonomicConceptLabel:
#   uri: <http://openbiodiv.net/TaxonomicConceptLabel>
#
#   TaxonomicNameUsage:
#   uri: <http://openbiodiv.net/TaxonomicNameUsage>
#
#   Treatment:
#   uri: <http://openbiodiv.net/Treatment>
