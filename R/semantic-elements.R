#' @export
Collection = rdf4r::identifier(
  id = "Collection",
  prefix = c(openbiodivCollection = "http://openbiodiv.net/resource/")
)

#' @export
Thing = rdf4r::identifier(
  id = "Thing",
  prefix = c(owl = "http://www.w3.org/2002/07/owl#")
)

#' @export
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
  prefix = c(openbiodivPublisher = "http://openbiodiv.net/resource/")
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


#' Keyword Group
#' @export
KeywordGroup = rdf4r::identifier(
  id = "KeywordGroup",
  prefix = c(openbiodivKeywords = "http://openbiodiv.net/resource/")
)

#' Checklist
#' @export
Checklist =  rdf4r::identifier(
  id = "Checklist",
  prefix = c(openbiodivChecklist = "http://openbiodiv.net/resource/")
)

#' Table
#' @export
Table =  rdf4r::identifier(
  id = "Table",
  prefix = c(openbiodivTable = "http://openbiodiv.net/resource/")
)

#' datacite orcid
#' @export
orcid = rdf4r::identifier(
  id = "orcid",
  prefix = c(datacite = "http://purl.org/spar/datacite/")
)

#' datacite zoobank
#' @export
zoobank = rdf4r::identifier(
  id = "zoobank",
  prefix = c(datacite = "http://purl.org/spar/datacite/")
)

#' datacite bold
#' @export
bold = rdf4r::identifier(
  id = "bold",
  prefix = c(datacite = "http://purl.org/spar/datacite/")
)

#' datacite genbank
#' @export
genbank = rdf4r::identifier(
  id = "genbank",
  prefix = c(datacite = "http://purl.org/spar/datacite/")
)



#' datacite plazi
#' @export
plazi = rdf4r::identifier(
  id = "plazi",
  prefix = c(datacite = "http://purl.org/spar/datacite/")
)

#' Personal Identifier
#' @export
PersonalIdentifier = rdf4r::identifier(
  id = "PersonalIdentifier",
  prefix = c(datacite = "http://purl.org/spar/datacite/")
)


#' Resource Identifier
#' @export
ResourceIdentifier = rdf4r::identifier(
  id = "ResourceIdentifier",
  prefix = c(datacite = "http://purl.org/spar/datacite/")
)


#' Treatment Section
#' @export
Treatment = rdf4r::identifier(
  id = "Treatment",
  prefix = c(openbiodivTreatment = "http://openbiodiv.net/resource/")
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
  prefix = c(openbiodivTC = "http://openbiodiv.net/resource/")
)


#' Nomenclature
#'  the informational content of an article
#' @export
Nomenclature = rdf4r::identifier(
  id = "NomenclatureSection",
  prefix = c(openbiodivNomenclature = "http://openbiodiv.net/resource/")
)


#' Nom Citations
#' the informational content of an article
#' @export
NomenclatureCitationsList = rdf4r::identifier(
  id = "NomenclatureCitationsList",
  prefix = c(openbiodivNomenclatureCit = "http://openbiodiv.net/resource/")
)


#' TNU
#' @export
TaxonomicNameUsage = rdf4r::identifier(
  id = "TaxonomicNameUsage",
  prefix = c(openbiodivTNU = "http://openbiodiv.net/resource/")
)


#' TNU
#' @export
InstitutionalCodeUsage = rdf4r::identifier(
  id = "InstitutionalCodeUsage",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)


#' Scientific Name
#' @export
ScientificName= rdf4r::identifier(
  id = "ScientificName",
  prefix = c(openbiodivScName = "http://openbiodiv.net/resource/")
)



#' Diagnosis
#' @export
Diagnosis = rdf4r::identifier(
  id = "DiagnosisSection",
  prefix = c(openbiodivDiagnosis = "http://openbiodiv.net/resource/")
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
  prefix = c(openbiodivDistribution = "http://openbiodiv.net/resource/")
)




#' Distribution
#' @export
TaxonomicKey = rdf4r::identifier(
  id = "TaxonomicKey",
  prefix = c(openbiodivKey = "http://openbiodiv.net/resource/")
)




#' Distribution
#' @export
MaterialsExamined = rdf4r::identifier(
  id = "MaterialsExamined",
  prefix =  c(openbiodivMaterials = "http://openbiodiv.net/resource/")
)




#' Figure:
#'  uri: <http://purl.org/spar/doco/Figure>
#'   comment: a figure in a science article
#' @export
Figure = rdf4r::identifier(
  id = "Figure",
  prefix = c(doco = "http://purl.org/spar/doco/")
)


#' Abstract:
#'   uri: <http://salt.semanticauthoring.org/ontologies/sro#Abstract>
#' comment: an abstract of a science article
#' @export
Abstract = rdf4r::identifier(
  id = "Abstract",
  prefix = c(sro = "http://salt.semanticauthoring.org/ontologies/sro#")
)



#' Introduction:
#'   uri: http://purl.org/spar/deo/Introduction>
#' comment: an abstract of a science article
#' @export
Introduction = rdf4r::identifier(
  id = "Introduction",
  prefix = c(deo = "http://purl.org/spar/deo/")
)



#' Title:
#' uri: <http://purl.org/spar/doco/Title>
#'   comment: the title of an article
#' @export
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

#' @export
has_link = rdf4r::identifier(
  id = "hasDownloadLink",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
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


#' Institutional Code
#' @export
institutional_code = rdf4r::identifier(
  id = "institutional_code",
  prefix = c(dwc = "http://rs.tdwg.org/dwc/terms/")
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

#' Dwc IRI Status
#' @export
has_taxonomic_status_id = rdf4r::identifier(
  id = "taxonomicStatus",
  prefix = c(dwciri = "http://rs.tdwg.org/dwc/iri/")
)

#' Dwc IRI Rank
#' @export
has_taxonomic_rank_id = rdf4r::identifier(
  id = "taxonRank",
  prefix = c(dwciri = "http://rs.tdwg.org/dwc/iri/")
)

#' @export
has_gbifID = rdf4r::identifier(
  id = "hasGbifTaxon",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
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


#' Has Keyword
#' @export
has_keyword = rdf4r::identifier(
  id = "keyword",
  prefix = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")
)


#' Has Issue
#' @export
has_issue = rdf4r::identifier(
  id = "number",
  prefix = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")
)


#' Has Affiliation
#' @export
has_affiliation = rdf4r::identifier(
  id = "affiliation",
  prefix = c(openbiodivAffil = "http://openbiodiv.net/property/")
)

#' @export
has_inst = rdf4r::identifier(
  id = "hasInstitution",
  prefix = c(openbiodivHasInst = "http://openbiodiv.net/property/")
)

#' @export
has_instName = rdf4r::identifier(
  id = "hasInstitutionName",
  prefix = c(openbiodivHasInstName = "http://openbiodiv.net/property/")
)

#' @export
has_instCode = rdf4r::identifier(
  id = "hasInstitutionCode",
  prefix = c(openbiodivHasInstCode = "http://openbiodiv.net/property/")
)

#' @export
Institution =  rdf4r::identifier(
  id = "Institution",
  prefix = c(openbiodivInstitution = "http://openbiodiv.net/resource/")
)

#' Has email
#' @export
has_email = rdf4r::identifier(
  id = "mbox",
  prefix = c(foaf = "http://xmlns.com/foaf/0.1/")
)

#' Has Zoobank
#' @export
has_zoobank = rdf4r::identifier(
  id = "zoobank",
  prefix = c(openbiodivZoobank = "http://openbiodiv.net/property/")
)

#' @export
has_bin = rdf4r::identifier(
  id = "BIN",
  prefix = c(openbiodivBIN = "http://openbiodiv.net/property/")
)

#' @export
has_bold = rdf4r::identifier(
  id = "BOLD",
  prefix = c(openbiodivBOLD = "http://openbiodiv.net/property/")
)

#' @export
has_identifier = rdf4r::identifier(
  id="hasIdentifier",
  prefix = c(datacite = "http://purl.org/spar/datacite/")
)

#' @export
mentions_id = rdf4r::identifier(
  id="mentionsIdentifier",
  prefix = c(openbiodivMentionsID = "http://openbiodiv.net/property/")
)

#' @export
has_coordinates = rdf4r::identifier(
  id="hasCoordinates",
  prefix = c(openbiodivCoordinates = "http://openbiodiv.net/property/")
)


#' @export
identifier_scheme =  rdf4r::identifier(
  id="usesIdentifierScheme",
  prefix = c(datacite = "http://purl.org/spar/datacite/")
)

  #' @export
  has_grbioCool = rdf4r::identifier(
    id = "hasCoolID",
    prefix = c(openbiodiv = "http://openbiodiv.net/")
  )
  
  #' @export
  has_instName = rdf4r::identifier(
    id = "hasInstitutionName",
    prefix = c(openbiodiv = "http://openbiodiv.net/")
  )
  
  #' @export
  inst_names =  rdf4r::identifier(
    id = "institutionNames",
    prefix = c(openbiodiv = "http://openbiodiv.net/")
  )
  
  #' @export
  has_instCode = rdf4r::identifier(
    id = "hasInstitutionCode",
    prefix = c(openbiodiv = "http://openbiodiv.net/")
  )
  
  #' @export
  inst_codes = rdf4r::identifier(
  id = "institutionCodes",
  prefix = c(openbiodiv = "http://openbiodiv.net/")
)
  
  #' @export
   has_grbioInstCode = rdf4r::identifier(
    id = "hasGrbioInstitutionCode",
    prefix = c(openbiodiv = "http://openbiodiv.net/")
  )
  
   #' @export
  has_inst = rdf4r::identifier(
    id = "hasInstitutionRecord",
    prefix = c(openbiodiv = "http://openbiodiv.net/")
  )

  #' @export
  GrbioInst = rdf4r::identifier(
    id = "GRSciCollInstitution",
    prefix = c(openbiodiv = "http://openbiodiv.net/")
  )
  
  #' @export
  taxonStatus =  rdf4r::identifier(
      id = "taxonomicStatus",
      prefix = c(openbiodiv = "http://openbiodiv.net/")
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
