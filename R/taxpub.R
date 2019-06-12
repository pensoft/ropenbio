#' @include xml.R

#' TaxPub Schema
#'
#' @export
taxpub = XmlSchema$new(
  schema_name = "taxpub",
  xpath = "/",
  file_pattern = ".*\\.xml",
  extension = ".xml",
  prefix = c(openbiodivArticle = "http://openbiodiv.net/resource/Article/"),
  atoms = c(
    title = "/article/front/article-meta/title-group/article-title",
    date = NA,
    pub_year = "/article/front/article-meta/pub-date[@pub-type='epub']/year",
    pub_month = "/article/front/article-meta/pub-date[@pub-type='epub']/month",
    pub_day = "/article/front/article-meta/pub-date[@pub-type='epub']/day",
    doi = "/article/front/article-meta/article-id[@pub-id-type='doi']",
    zenodo = NA,
    article_zoobank = "/article/front/article-meta/article-id[@pub-id-type='other']",
    publisher = "/article/front/journal-meta/publisher/publisher-name",
    journal = "/article/front/journal-meta/journal-title-group/journal-title",
    journal_abbrev = "/article/front/journal-meta/journal-title-group/abbrev-journal-title",
    journal_zoobank = "/article/front/journal-meta/journal-id[@journal-id-type='aggregator']",
    issn = "/article/front/journal-meta/issn[@pub-type='ppub']",
    eIssn = "/article/front/journal-meta/issn[@pub-type='epub']",
    issue = "/article/front/article-meta/issue",
    volume = NA,
    starting_page = NA,
    ending_page = NA,
    keyword = "/article/front/article-meta/kwd-group/kwd",
    bold_id="/article/front/article-meta/bold-ids/bold-id",
    bin="/article/front/article-meta/bins/bin"
    #pensoft_pub = NA # becasue we skip only Pensoft pubs from Plazi
  ),

  atom_lang = c(
    title = NA,
    date = NA,
    pub_year = NA,
    pub_month = NA,
    pub_day = NA,
    doi = NA,
    zenodo = NA,
    article_zoobank = NA,
    publisher = NA,
    journal = "/article/front/journal-meta/journal-title-group/journal-title/@xml:lang",
    journal_abbrev = "/article/front/journal-meta/journal-title-group/abbrev-journal-title/@xml:lang",
    journal_zoobank = NA,
    issn = NA,
    eIssn = NA,
    issue = NA,
    volume = NA,
    starting_page = NA,
    ending_page = NA,
    #pensoft_pub = NA
    keyword = NA,
    bold_id=NA,
    bin=NA
  ),

  atom_types = list(
    title = rdf4r::xsd_string,
    date = rdf4r::xsd_date,
    pub_year = rdf4r::xsd_integer,
    pub_month = rdf4r::xsd_integer,
    pub_day = rdf4r::xsd_integer,
    doi = rdf4r::xsd_string,
    zenodo = rdf4r::xsd_string,
    article_zoobank = rdf4r::xsd_string,
    publisher = rdf4r::xsd_string,
    journal = rdf4r::xsd_string,
    journal_abbrev = rdf4r::xsd_string,
    journal_zoobank = rdf4r::xsd_string,
    issn = rdf4r::xsd_string,
    eIssn = rdf4r::xsd_string,
    issue = rdf4r::xsd_integer,
    volume = rdf4r::xsd_integer,
    starting_page = rdf4r::xsd_integer,
    ending_page = rdf4r::xsd_integer,
    #pensoft_pub = rdf4r::xsd_string
    keyword = rdf4r::xsd_string,
    bold_id=rdf4r::xsd_string,
    bin=rdf4r::xsd_string
  ),
  mongo_key = c(article = "/article/front/article-meta/article-id[@pub-id-type='doi']"),
  constructor = metadata,

  components = list(
    # Keyword
    XmlSchema$new(
      schema_name = "taxpub_keyword_group",
      xpath = "/article/front/article-meta/kwd-group",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivKeywords = "http://openbiodiv.net/resource/KeywordGroup/"),
      atoms = c(
        keyword = "./kwd"
      ),

      atom_lang = c(
        keyword = NA
      ),

      atom_types = list(
        keyword =  rdf4r::xsd_string
      ),
      mongo_key = c(keywords = "/article/front/article-meta/kwd-group"),
      constructor = keyword_group,

      components = NULL
    ),
    # Abstract
    XmlSchema$new(
      schema_name = "taxpub_abstract",
      xpath = "/article/front/article-meta/abstract",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivAbstract = "http://openbiodiv.net/resource/Abstract/"),
      atoms = c(
        text_content = ".",
        trans_abstract = "../trans-abstract"
      ),

      atom_lang = c(
        text_content = NA,
        trans_abstract = "../trans-abstract/@xml:lang"
      ),

      atom_types = list(
        text_content =  rdf4r::xsd_string,
        trans_abstract = rdf4r::xsd_string
      ),
      mongo_key = c(abstract = "/article/front/article-meta/article-id[@pub-id-type='doi']"),
      constructor = abstract,

      components = NULL
    ),

    # Title
    XmlSchema$new(
      schema_name = "taxpub_title",
      xpath = "/article/front/article-meta/title-group/article-title",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivTitle = "http://openbiodiv.net/resource/ArticleTitle/"),
      atoms = c(
        text_content = "."
      ),

      atom_lang = c(
        text_content = NA
      ),

      atom_types = list(
        text_content =  rdf4r::xsd_string
      ),
      mongo_key = c(article_title = "/article/front/article-meta/title-group/article-title"),
      constructor = title,

      components = NULL
    ),

    # Author
    XmlSchema$new(
      schema_name = "taxpub_author",
      xpath = "/article/front/article-meta/contrib-group/contrib",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivAuthor = "http://openbiodiv.net/resource/Author/"),
      atoms = c(
        full_name = NA,
        surname = "./name/surname",
        given_names = "./name/given-names",
        email = "./email",
        aff_id = "./xref[@rid]",
        all_affiliations_institutions = "//article/front/article-meta/aff/institution | //article/front/article-meta/aff/addr-line",
        all_affiliations_cities = "//article/front/article-meta/aff/addr-line[@content-type='city']",
        all_affiliations = NA
        # role = "./mods:role/mods:roleTerm"
      ),

      atom_lang = c(
        full_name = NA,
        surname = NA,
        given_names = NA,
        email = NA,
        aff_id = NA,
        all_affiliations_institutions = NA,
        all_affiliations_cities = NA,
        all_affiliations = NA
        #role = NA
      ),

      atom_types = list(
        full_name = rdf4r::xsd_string,
        surname = rdf4r::xsd_string,
        given_names = rdf4r::xsd_string,
        email = rdf4r::xsd_string,
        aff_id = rdf4r::xsd_integer,
        all_affiliations_institutions = rdf4r::xsd_string,
        all_affiliations_cities =  rdf4r::xsd_string,
        all_affiliations = rdf4r::xsd_string
      ),
      #change mongokey
      mongo_key = c(author_name = "./name/given-names", surname = "./name/surname"),
      constructor = author,

      components = NULL
    ),
    # Introduction
    XmlSchema$new(
      schema_name = "taxpub_introduction_section",
      xpath = "/article/body/sec[@sec-type='Introduction']",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivIntroduction = "http://openbiodiv.net/resource/Introduction/"),
      atoms = c(
        text_content = "."
      ),

      atom_lang = c(
        text_content = NA
      ),

      atom_types = list(
        text_content =  rdf4r::xsd_string
      ),
      mongo_key = c(introduction = "/article/front/article-meta/article-id[@pub-id-type='doi']"),
      constructor = introduction_section
    ),
    # DISCUSSION
    XmlSchema$new(
      schema_name = "discussion_section",
      xpath = "//sec[@sec-type='Discussion']", #rel path from treatment
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivDiscussion = "http://openbiodiv.net/resource/Discussion/"),
      atoms = c(
        text_content = "."
      ),

      atom_lang = c(
        text_content = NA
      ),

      atom_types = list(
        text_content =  rdf4r::xsd_string
      ),
      mongo_key = c(discussion = "/article/front/article-meta/article-id[@pub-id-type='doi']"),
      constructor = discussion
    ),
    # Treatment
    XmlSchema$new(
      schema_name = "taxpub_treatment",
      xpath = "//tp:taxon-treatment",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivTreatment = "http://openbiodiv.net/resource/Treatment/"),
      atoms = c(
        text_content = "."
      ),

      atom_lang = c(
        text_content = NA
      ),

      atom_types = list(
        text_content =  rdf4r::xsd_string
      ),
      mongo_key =  c(treatment = "."),
      constructor = treatment,

      components = list(
        # Nomenclature
        XmlSchema$new(
          schema_name = "taxpub_nomenclature_section",
          xpath = "./tp:nomenclature", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix = c(openbiodivNomenclature = "http://openbiodiv.net/resource/NomenclatureSection/"),
          atoms = c(
            text_content = "."
          ),

          atom_lang = c(
            text_content = NA
          ),

          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),
          # nomenclature citations
          components = list(
            XmlSchema$new(
              schema_name = "taxpub_nomenclature_citations",
              xpath = "./tp:nomenclature-citation-list", #rel path from treatment
              file_pattern = ".*\\.xml",
              extension = ".xml",
              prefix = c(openbiodivNomenclatureCit = "http://openbiodiv.net/resource/NomenclatureCitationsList/"),
              atoms = c(
                text_content = "."
              ),

              atom_lang = c(
                text_content = NA
              ),

              atom_types = list(
                text_content =  rdf4r::xsd_string
              ),

              components = NULL,
              mongo_key =  c(nomenclature_citations = "."),

              constructor = nomenclature_citations
            )
          ),
          mongo_key =  c(nomenclature = "."),

          constructor = nomenclature
        ),
        # Materials Examined
        XmlSchema$new(
          schema_name = "taxpub_materials_examined",
          xpath = "./tp:treatment-sec[@sec-type='materials']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix = c(openbiodivMaterials = "http://openbiodiv.net/resource/treatment/MaterialsExamined/"),
          atoms = c(
            text_content = "."
          ),

          atom_lang = c(
            text_content = NA
          ),

          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),
          mongo_key =  c(materials_examined = "."),

          constructor = materials_examined
        ),

        #Diagnosis
        XmlSchema$new(
          schema_name = "taxpub_diagnosis_section",
          xpath = "./tp:treatment-sec[@sec-type='Diagnosis']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix = c(openbiodivDiagnosis = "http://openbiodiv.net/resource/treatment/Diagnosis/"),
          atoms = c(
            text_content = "."
          ),

          atom_lang = c(
            text_content = NA
          ),

          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),
          mongo_key =  c(diagnosis = "."),

          constructor = diagnosis
        ),



        # distribution
        XmlSchema$new(
          schema_name = "taxpub_distribution_section",
          xpath = "./tp:treatment-sec[@sec-type='Distribution']", #rel path from treatment
          file_pattern = ".*\\.xml",
          extension = ".xml",
          prefix = c(openbiodivDistribution = "http://openbiodiv.net/resource/treatment/Distribution/"),
          atoms = c(
            text_content = "."
          ),

          atom_lang = c(
            text_content = NA
          ),

          atom_types = list(
            text_content =  rdf4r::xsd_string
          ),
          mongo_key =  c(distribution = "."),
          constructor = distribution
        )

      )
    ),

    # Taxonomic Key
    XmlSchema$new(
      schema_name = "taxpub_taxonomic_key",
      xpath = "/sec[@sec-type='key']",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivKey = "http://openbiodiv.net/resource/TaxonomicKey/"),
      atoms = c(
        text_content = "."
      ),

      atom_lang = c(
        text_content = NA
      ),

      atom_types = list(
        text_content = rdf4r::xsd_string
      ),
      mongo_key =  c(taxonomic_key = "."),
      constructor = taxonomic_key,

      components = NULL
    ),

    # Figure
    XmlSchema$new(
      schema_name = "taxpub_figure",
      xpath = "//fig|//fig-group",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivFigure = "http://openbiodiv.net/resource/Figure/"),
      atoms = c(
        text_content = ".",
        caption = "./caption"
      ),

      atom_lang = c(
        text_content = NA,
        caption = NA
      ),

      atom_types = list(
        text_content = rdf4r::xsd_string,
        caption = rdf4r::xsd_string
      ),
      mongo_key = c(figure = "//fig|//fig-group"),
      constructor = figure,

      components = NULL
    ),
    # Taxonomic Name Usage
    XmlSchema$new(
      schema_name = "taxpub_taxonomic_name_usage",
      xpath = "//tp:taxon-name",
      file_pattern = ".*\\.xml",
      extension = ".xml",
      prefix = c(openbiodivTaxonName = "http://openbiodiv.net/resource/TaxonName/"),
      atoms = c(
        date = NA,
        pub_year = "/article/front/article-meta/pub-date/year",
        pub_month = "/article/front/article-meta/pub-date/month",
        pub_day = "/article/front/article-meta/pub-date/day",
        kingdom = "./tp:taxon-name-part[@taxon-name-part-type='kingdom' or @taxon-name-part-type='Kingdom' or @taxon-name-part-type='divisio' or @taxon-name-part-type='Divisio' or @taxon-name-part-type='division' or @taxon-name-part-type='Division']",
        phylum = "./tp:taxon-name-part[@taxon-name-part-type='phylum' or @taxon-name-part-type='Phylum' or @taxon-name-part-type='regnum' or @taxon-name-part-type='Regnum']",
        class = "./tp:taxon-name-part[@taxon-name-part-type='class' or @taxon-name-part-type='Class' or @taxon-name-part-type='classis' or @taxon-name-part-type='Classis']",
        order = "./tp:taxon-name-part[@taxon-name-part-type='order' or @taxon-name-part-type='Order' or @taxon-name-part-type='ordo' or @taxon-name-part-type='Ordo']",
        family = "./tp:taxon-name-part[@taxon-name-part-type='family' or @taxon-name-part-type='Family' or @taxon-name-part-type='familia' or @taxon-name-part-type='Familia' or @taxon-name-part-type='famil' or @taxon-name-part-type='Famil']",
        subfamily = "./tp:taxon-name-part[@taxon-name-part-type='subfamily' or @taxon-name-part-type='Subfamily' or @taxon-name-part-type='subfamilia' or @taxon-name-part-type='Subfamilia' or @taxon-name-part-type='subfamil' or @taxon-name-part-type='Subfamil' or @taxon-name-part-type='tribe' or @taxon-name-part-type='Tribe' or @taxon-name-part-type='tribus' or @taxon-name-part-type='Tribus' or @taxon-name-part-type='subtribe' or @taxon-name-part-type='Subtribe' or @taxon-name-part-type='subtribus' or @taxon-name-part-type='Subtribus']",
        genus = "./tp:taxon-name-part[@taxon-name-part-type='genus' or @taxon-name-part-type='Genus' or @taxon-name-part-type='genera' or @taxon-name-part-type='Genera']",
        regularzied_genus = "./tp:taxon-name-part[@taxon-name-part-type='genus' or @taxon-name-part-type='Genus' or @taxon-name-part-type='genera' or @taxon-name-part-type='Genera']/@reg",
        subgenus = "./tp:taxon-name-part[@taxon-name-part-type='subgenus' or @taxon-name-part-type='Subgenus' or @taxon-name-part-type='subgenera' or @taxon-name-part-type='Subgenera' or @taxon-name-part-type='section' or @taxon-name-part-type='Section' or @taxon-name-part-type='sectio' or @taxon-name-part-type='Sectio']",
        species = "./tp:taxon-name-part[@taxon-name-part-type='Species' or @taxon-name-part-type='species']",
        subspecies = "/tp:taxon-name-part[@taxon-name-part-type='Subspecies' or @taxon-name-part-type='subspecies' or @taxon-name-part-type='Variety' or @taxon-name-part-type='variety' or @taxon-name-part-type='varietas' or @taxon-name-part-type='Varietas' or @taxon-name-part-type='variation' or @taxon-name-part-type='Variation' or @taxon-name-part-type='subvariety' or @taxon-name-part-type='Subvariety' or @taxon-name-part-type='subvarietas' or @taxon-name-part-type='Subvarietas' or @taxon-name-part-type='subvariation' or @taxon-name-part-type='Subvariation' or @taxon-name-part-type='Form' or @taxon-name-part-type='form' or @taxon-name-part-type='forma' or @taxon-name-part-type='Forma' or @taxon-name-part-type='aberration' or @taxon-name-part-type='Aberration' or @taxon-name-part-type='race' or @taxon-name-part-type='Race' or @taxon-name-part-type='Subform' or @taxon-name-part-type='subform' or @taxon-name-part-type='subforma' or @taxon-name-part-type='Subforma' or @taxon-name-part-type='subaberation' or @taxon-name-part-type='Subaberation' or @taxon-name-part-type='subrace' or @taxon-name-part-type='Subrace' ]",
        verbatim = ".",
        verbatim_rank = "./tp:taxon-name-part[last()]/@taxon-name-part-type",
        verbatim_status = "following-sibling::tp:taxon-status",
        status = NA,
        authorship = "following-sibling::tp:taxon-authority | ./tp:taxon-name-part[@taxon-name-part-type='authority']",
        external_taxonomic_name_id = "./object-id",
        secundum_literal = NA
      ),

      atom_lang = c(
        date = NA,
        pub_year = NA,
        pub_month = NA,
        pub_day = NA,
        kingdom = NA,
        phylum = NA,
        class = NA,
        order = NA,
        family = NA,
        subfamily = NA,
        genus = NA,
        regularzied_genus = NA,
        subgenus = NA,
        species = NA,  ## This is an error in TaxonX, not DwC!
        subspecies = NA,
        verbatim_rank = NA,
        verbatim_status = NA,
        status = NA,
        authorship = NA,
        external_taxonomic_name_id = NA,
        secundum_literal = NA
      ),

      atom_types = list(
        date = rdf4r::xsd_date,
        pub_year = rdf4r::xsd_integer,
        pub_month = rdf4r::xsd_integer,
        pub_day = rdf4r::xsd_integer,
        kingdom = rdf4r::xsd_string,
        class = rdf4r::xsd_string,
        order = rdf4r::xsd_string,
        family = rdf4r::xsd_string,
        subfamily = rdf4r::xsd_string,
        genus = rdf4r::xsd_string,
        regularzied_genus = rdf4r::xsd_string,
        subgenus = rdf4r::xsd_string,
        species = rdf4r::xsd_string,
        subspecies = rdf4r::xsd_string,
        verbatim_rank = rdf4r::xsd_string,
        taxonomic_rank = rdf4r::xsd_string,
        taxonomic_status = rdf4r::xsd_string,
        status = rdf4r::xsd_string,
        authorship = rdf4r::xsd_string,
        external_taxonomic_name_id = rdf4r::xsd_string,
        secundum_literal = rdf4r::xsd_string
      ),
      mongo_key = c(
        kingdom = "./tp:taxon-name-part[@taxon-name-part-type='kingdom' or @taxon-name-part-type='Kingdom' or @taxon-name-part-type='divisio' or @taxon-name-part-type='Divisio' or @taxon-name-part-type='division' or @taxon-name-part-type='Division']",
        phylum = "./tp:taxon-name-part[@taxon-name-part-type='phylum' or @taxon-name-part-type='Phylum' or @taxon-name-part-type='regnum' or @taxon-name-part-type='Regnum']",
        class = "./tp:taxon-name-part[@taxon-name-part-type='class' or @taxon-name-part-type='Class' or @taxon-name-part-type='classis' or @taxon-name-part-type='Classis']",
        order = "./tp:taxon-name-part[@taxon-name-part-type='order' or @taxon-name-part-type='Order' or @taxon-name-part-type='ordo' or @taxon-name-part-type='Ordo']",
        family = "./tp:taxon-name-part[@taxon-name-part-type='family' or @taxon-name-part-type='Family' or @taxon-name-part-type='familia' or @taxon-name-part-type='Familia' or @taxon-name-part-type='famil' or @taxon-name-part-type='Famil']",
        subfamily = "./tp:taxon-name-part[@taxon-name-part-type='subfamily' or @taxon-name-part-type='Subfamily' or @taxon-name-part-type='subfamilia' or @taxon-name-part-type='Subfamilia' or @taxon-name-part-type='subfamil' or @taxon-name-part-type='Subfamil' or @taxon-name-part-type='tribe' or @taxon-name-part-type='Tribe' or @taxon-name-part-type='tribus' or @taxon-name-part-type='Tribus' or @taxon-name-part-type='subtribe' or @taxon-name-part-type='Subtribe' or @taxon-name-part-type='subtribus' or @taxon-name-part-type='Subtribus']",
        genus = "./tp:taxon-name-part[@taxon-name-part-type='genus' or @taxon-name-part-type='Genus' or @taxon-name-part-type='genera' or @taxon-name-part-type='Genera']",
        subgenus = "./tp:taxon-name-part[@taxon-name-part-type='subgenus' or @taxon-name-part-type='Subgenus' or @taxon-name-part-type='subgenera' or @taxon-name-part-type='Subgenera' or @taxon-name-part-type='section' or @taxon-name-part-type='Section' or @taxon-name-part-type='sectio' or @taxon-name-part-type='Sectio']",
        species = "./tp:taxon-name-part[@taxon-name-part-type='Species' or @taxon-name-part-type='species']",
        subspecies = "/tp:taxon-name-part[@taxon-name-part-type='Subspecies' or @taxon-name-part-type='subspecies' or @taxon-name-part-type='Variety' or @taxon-name-part-type='variety' or @taxon-name-part-type='varietas' or @taxon-name-part-type='Varietas' or @taxon-name-part-type='variation' or @taxon-name-part-type='Variation' or @taxon-name-part-type='subvariety' or @taxon-name-part-type='Subvariety' or @taxon-name-part-type='subvarietas' or @taxon-name-part-type='Subvarietas' or @taxon-name-part-type='subvariation' or @taxon-name-part-type='Subvariation' or @taxon-name-part-type='Form' or @taxon-name-part-type='form' or @taxon-name-part-type='forma' or @taxon-name-part-type='Forma' or @taxon-name-part-type='aberration' or @taxon-name-part-type='Aberration' or @taxon-name-part-type='race' or @taxon-name-part-type='Race' or @taxon-name-part-type='Subform' or @taxon-name-part-type='subform' or @taxon-name-part-type='subforma' or @taxon-name-part-type='Subforma' or @taxon-name-part-type='subaberation' or @taxon-name-part-type='Subaberation' or @taxon-name-part-type='subrace' or @taxon-name-part-type='Subrace' ]",
        authorship = "following-sibling::tp:taxon-authority | ./tp:taxon-name-part[@taxon-name-part-type='authority']",
        secundum_literal = NA),
      constructor = taxonomic_name_usage,

      components = NULL
    )
  )
)


#' Plazi Internal
#'
#' @export
plazi_int =
  XmlSchema$new(schema_name = "plazi_int",
                file_pattern = ".*\\.plazixml",
                extension =  ".plazixml",
                prefix = "http://tb.plazi.org/GgServer/xml/",
                atoms = c(lang = "/document/@docLanguage"))

#' Plazi Feed
#'
#' @export
plazi_feed_schema =
  XmlSchema$new(schema_name = "plazi_feed",
                atoms = c(link = "/rss/channel/item/link",
                          pub_date = "/rss/channel/item/pubDate"))
