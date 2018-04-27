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

#' the informational content of an article
#' @export
Paper = rdf4r::identifier(
  id = "ResearchPaper",
  prefix = c(fabio = "http://purl.org/spar/fabio/")
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



# Abstract:
#   uri: <http://salt.semanticauthoring.org/ontologies/sro#Abstract>
# comment: an abstract of a science article
#
# Figure:
#   uri: <http://purl.org/spar/doco/Figure>
#   comment: a figure in a science article
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
# Title:
#   uri: <http://purl.org/spar/doco/Title>
#   comment: the title of an article
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
