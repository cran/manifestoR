#' Access the Manifesto Project's Main Dataset
#' 
#' Gets the Manifesto Project's Main Dataset from the project's web API or
#' the local cache, if it was already downloaded before.
#' 
#' \code{mp_southamerica_dataset} is a shorthand for getting the Manifesto
#' Project's South America Dataset (it is equivalent to 
#' \code{mp_maindataset(..., south_america = TRUE)}). It is nowadays deprecated, for 
#' details see explanation in `south_america` parameter documentation.
#'
#' @param version Specify the version of the dataset you want to access. Use
#'                "current" to obtain the most recent, or use
#'                \code{\link{mp_coreversions}} for a list of available
#'                versions.
#' @param south_america flag whether to download corresponding South America dataset
#' instead of Main Dataset. This parameter deprecated as the previously separated South
#' America Dataset has been integrated into the Main Dataset from version 2023a onwards.
#' To allow for backward compatibilty old South American Datasets can still be accessed
#' but querying for the most recent South American Dataset will result in an empty dataset.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @param download_format Download format. If not NULL, instead of the dataset
#' being returned as an R data.frame, a file path to a temporary file in the specified
#' binary format is returned. Can be one of \code{c("dta", "xlsx", "sav")}. With
#' the "dta" option, labeled columns can be obtained.
#'              
#' @return The Manifesto Project Main Dataset with classes \code{data.frame} and
#' \code{\link[dplyr]{tbl_df}}
#'
#' @examples
#' \dontrun{
#' mpds <- mp_maindataset()
#' head(mpds)
#' median(subset(mpds, countryname == "Switzerland")$rile, na.rm = TRUE)
#' }
#' \dontrun{
#' mp_maindataset(download_format = "dta") %>% read_dta() ## requires package haven
#' }
#' @export
#' @import base64enc
mp_maindataset <- function(version="current", south_america = FALSE, download_format = NULL, apikey=NULL, cache=TRUE) {
  
  if (version == "current") {
    if (south_america) {
      warning(paste("The South American Dataset has been integrated into the Main Dataset from version 2023a onwards.",
                    "Requesting the most recent version of the South American Dataset will result in an empty dataset."))
    }
    version <- current_dataset_version(south_america = south_america, apikey = apikey, cache = cache)
  } else if (!grepl("MPDS", version)) {
    version <- paste0(ifelse(south_america, "MPDSSA", "MPDS"), version)
  }
  
  if (south_america & !grepl("MPDSSA", version)) {
    warning(paste("The specified version string", version,
                  "does not identify a South America dataset, although south_america = TRUE.",
                  "The returned dataset might be incorrect!"))
  }
  if (!south_america & grepl("MPDSSA", version)) {
    warning(paste("The specified version string", version,
                  "does identify a South America dataset, but south_america = FALSE.",
                  "The returned dataset might be incorrect!"))
  }
  
  # if (south_america) {
    # if (as.numeric(gsub(".*?(\\d+).*", "\\1", version)) < 2015) {
      # warning("No south america dataset available before 2015!")
      # return(as_tibble(data.frame()))
    # }
    # version <- gsub("MPDS", "MPDSSA", version)
  # }
  
  parameters <- c(key=version, kind=download_format) %>% as.list()

  mpds <- get_viacache(kmtype.main, ids = parameters,
                       cache = cache, apikey = apikey)
  
  if (!is.null(download_format)) {
    tmp <- tempfile(fileext = paste0(".", download_format))
    mpds %>%
      base64enc::base64decode() %>%
      writeBin(tmp)
    return(tmp)
  } else {
    return(as_tibble(mpds))
  }
  
}

#' `mp_southamerica_dataset` is deprecated as the previously separated South America Dataset
#' has been integrated into the Main Dataset from version 2023a onwards. To allow for backward
#' compatibilty old South American Datasets can still be accessed but querying for the most
#' recent South American Dataset will result in an empty dataset.
#'
#' @rdname mp_maindataset
#' @param ... all arguments of \code{mp_southamerica_data} are passed on to \code{mp_maindataset}
#' @export
mp_southamerica_dataset <- function(...) {
  .Deprecated("mp_maindataset", package = "manifestoR")
  functional::Curry(mp_maindataset, south_america = TRUE)(...)
}


#' List the available versions of the Manifesto Project's Main Dataset
#' 
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @param kind one of "main" (default) or "south_america" to discriminate the Main Dataset
#' and the South America Dataset. "south_america" is nowadays deprecated as the South
#' American Dataset has been integrated into the Main Dataset from version 2023a onwards.
#' Using "south_america" will still return past South American Dataset versions but also 
#' as the most recent version a reference to an empty dataset.
#'
#' @details
#' For the available versions of the corpus, see \code{\link{mp_corpusversions}}
#'
#' @examples
#' \dontrun{mp_coreversions()}
#' @export
mp_coreversions <- function(apikey=NULL, cache=TRUE, kind = "main") {
  
  versions <- get_viacache(kmtype.versions, apikey=apikey, cache=cache, kind=kind, versionid = "CONST")
  
  return(versions)
}

current_dataset_version <- function(south_america = FALSE, apikey = NULL, cache = TRUE) {
  versions <- mp_coreversions(apikey=apikey, cache=cache, kind = ifelse(south_america, "south_america", "main"))
  as.character(versions[nrow(versions), "datasets.id"]) # TODO date in dataset
}


#' Format ids for web API queries
#' 
#' Formats a data.frame of ids such that it can be used for querying
#' the Manifesto Project Database. That is, it must have non-NA-fields
#' party and date.
#'
#' @param ids ids data.frame, information used: party, date, edate
formatids <- function(ids) {
  
  
  names(ids) <- tolower(names(ids))
  ids <- ids[,intersect(c("party", "date", "edate"), names(ids))]
  
  if ("date" %in% names(ids) & "edate" %in% names(ids)) {
    ids <- mutate(ids, date = ifelse(is.na(date),
                                     as.numeric(format(edate, format="%Y%m")),
                                     date))
  }

  n.before <- nrow(ids)
  suppressWarnings(ids <- ids[which(!is.na(ids$party) & !is.na(ids$date)),])
  n.after <- nrow(ids)
  if (n.after < n.before) {
    warning(paste(n.before - n.after, "rows were ommitted from querying the database,",
                  "because they are NULL or NA."))
  }

  return(ids)
}

#' Get meta data for election programmes
#' 
#' @details
#' Meta data contain information on the available documents for a given party
#' and election date. This information comprises links to the text as well as
#' original documents if available, language, versions checksums and more.
#' 
#' @param ids list of partys (as ids) and dates of elections, paired. Dates must
#'            be given either in the \code{date} or the \code{edate} variable,
#'            formatted in the way they are in the main data set in this package
#'            (date: as.numeric, YYYYMM, edate: as.Date()), see \code{\link{mp_maindataset}}
#'        Alternatively, ids can be a logical expression specifying a subset of
#'        the Manifesto Project's main dataset. It will be evaluated within the
#'        data.frame returned by \code{\link{mp_maindataset}} such that all its
#'        variables and functions thereof can be used in the expression.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#'              
#' @return an object of class \code{ManifestoMetadata}, subclassing \code{data.frame}
#'         as well as \code{\link[dplyr]{tbl_df}} and containing the requested
#'         metadata in rows per election programme
#' @examples
#' \dontrun{
#' mp_metadata(party == 21221)
#' 
#' wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' mp_metadata(wanted)
#' }
#' @export
mp_metadata <- function(ids, apikey=NULL, cache=TRUE) {

  ## non standard evaluation handling
  ## one frame up is where the user was if we did not get a data.frame
  ids <- as.metaids(substitute(ids), apikey = apikey, cache = cache, envir = parent.frame(), attach_meta = FALSE)

  # convert ids to parameter list for the api call
  ids <- formatids(ids)  
  
  metadata <- get_viacache(kmtype.meta,
                             ids=ids,
                             cache=cache,
                             apikey=apikey)

  ## type conversion for certain metadata entries
  metadata <- within(metadata, {
    if (exists("manifesto_id", inherits = FALSE)) {
      manifesto_id <- as.character(manifesto_id)
    } else {
      manifesto_id <- as.character(rep(NA_character_, times = nrow(metadata)))
    }
    if (exists("is_primary_doc", inherits = FALSE)) {
      is_primary_doc <- as.logical(is_primary_doc)
    }
    if (exists("may_contradict_core_dataset", inherits = FALSE)) {
      may_contradict_core_dataset <- as.logical(may_contradict_core_dataset)
    }
    if (exists("has_eu_code", inherits = FALSE)) {
      has_eu_code <- as.logical(has_eu_code)
      has_eu_code[is.na(has_eu_code)] <- FALSE
    }
    translation_columns = ls()[grep("^translation_[a-z]{2,3}", ls())]
    if (length(translation_columns > 0)) {
      for (translation_column in translation_columns) {
        if (exists(translation_column, inherits = FALSE)) assign(translation_column, as.logical(get(translation_column)))
      }
    }
    if (exists("translation_columns", inherits = FALSE)) rm(translation_columns)
    if (exists("translation_column", inherits = FALSE)) rm(translation_column)
  })
  
  metadata <- as_tibble(metadata)

  class(metadata) <- c("ManifestoMetadata", class(metadata))
  
  return(metadata)
  
}



## ids must be quoted for this function
as.metaids <- function(ids, apikey=NULL, cache=TRUE, envir = parent.frame(n = 2),
                       attach_meta = TRUE) {

  ## non standard evaluation handling
  ## two frames up is where the user was, as.metaids is not exported
  
  id_is_df <- tryCatch(is.data.frame(eval(ids, envir = envir)), error = function(e) { FALSE } )

  if (id_is_df) {
    ids <- eval(ids, envir = envir)
  } else {
    
    search_data <- mp_maindataset(apikey = apikey, cache = cache) %>%
      attach_year()
    
    ids_to_be_used <- eval(ids, envir = search_data, enclos = envir)
    ids <- search_data[replace(ids_to_be_used, is.na(ids_to_be_used), FALSE), ]
  } 

  if (attach_meta && !("ManifestoMetadata" %in% class(ids))) {
    ## TODO fix south america disappearance here
    ids <- mp_metadata(ids, apikey=apikey, cache=cache)
  }

  if ("is_primary_doc" %in% names(ids)) {
    ids <- subset(ids, is.na(is_primary_doc) | is_primary_doc)
  }

  return(ids)
}

is.naorstringna <- function(v) {
  return(is.na(v) | v=="NA")
}

#' Availability information for election programmes
#' 
#' @param ids Information on which documents to get. This can either be a
#'            list of partys (as ids) and dates of elections as given to
#'            \code{\link{mp_metadata}} or a \code{ManifestoMetadata} object
#'            (\code{data.frame}) as returned by \code{\link{mp_metadata}}.
#'        Alternatively, ids can be a logical expression specifying a subset of
#'        the Manifesto Project's main dataset. It will be evaluated within the
#'        data.frame returned by \code{\link{mp_maindataset}} such that all its
#'        variables and functions thereof can be used in the expression.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @return an object of class \code{\link{ManifestoAvailability}}
#'         containing availability information. Can be treated as a
#'         \code{data.frame} and contains detailed availability information
#'         per document
#' @examples
#' \dontrun{
#' mp_availability(countryname == "New Zealand")
#' 
#' wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' mp_availability(wanted)
#' }
#' @export
mp_availability <- function(ids, apikey=NULL, cache=TRUE) {
  
  columns <- c("party", "date", "language", "annotations", "translation_en")
  
  metadata <- suppressWarnings(as.metaids(substitute(ids),
                                          apikey=apikey,
                                          cache=cache,
                                          envir = parent.frame()))

  if (!("language" %in% names(metadata))) {
    metadata <- mutate(metadata, language = NA_character_)
  }
  if (!("annotations" %in% names(metadata))) {
    metadata <- mutate(metadata, annotations = FALSE)
  }
  if (!("url_original" %in% names(metadata))) {
    metadata <- mutate(metadata, url_original = NA_character_)
  }
  if (!("translation_en" %in% names(metadata))) {
    metadata <- mutate(metadata, translation_en = FALSE)
  }
  availability <- select(metadata, one_of(columns))

  availability$manifestos <- !is.naorstringna(metadata$manifesto_id)
  availability$originals <- !is.naorstringna(metadata$url_original)

  availability <-
      metadata %>%
        as_tibble %>%
        select(one_of("party", "date")) %>%
        anti_join(availability, by = c("party", "date")) %>%
        mutate(manifestos = FALSE,
               originals = FALSE,
               annotations  = FALSE,
               translation_en = FALSE,
               language = NA_character_) %>%
        bind_rows(availability)

  attr(availability, "query") <- metadata
  attr(availability, "date") <- date()
  attr(availability, "corpus_version") <- mp_which_corpus_version()
  
  class(availability) <- c("ManifestoAvailability", class(availability))
  return(availability)
}


#' Manifesto Availability Information class
#' 
#' Objects returned by \code{\link{mp_availability}}.
#' 
#' @details
#' ManifestoAvailability objects are data.frames with variables \code{party}
#' and \code{date} identifying the requested manifestos as in the Manifesto
#' Project's Main Datasets. The additional variables specify whether a machine 
#' readable document is available (\code{manifestos}, "Documents found"),
#' whether digital CMP coding annotations are available (\code{annotations},
#' "Coded Documents found"), whether an orignal PDF is available
#' (\code{originals}, "Originals found"), or whether an english translation
#' for the (digitally annotated) machine-readable document is available
#' (\code{translation_en}, "English Translations found").
#' 
#' Additional a ManifestoAvailability object has attributes \code{query}, containing
#' the original id set which was queried, \code{corpus_version}, specifying the
#' Corpus version ID used for the query, and \code{date} with the timestamp of the query. 
#' 
#' @name ManifestoAvailability
#' @docType class
#' @examples
#' \dontrun{
#' wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' mp_availability(wanted)
#' }
NULL

#' @method print ManifestoAvailability
#' @export
print.ManifestoAvailability <- function(x, ...) {
  
  avl <- x ## for better readability but S3 consistency of parameters
  decs <- 3
  
  nqueried <- avl %>%
    attr("query") %>%
    select(party, date) %>%
    unique() %>%
    nrow()
    
  ncoveredtexts <- avl %>%
    subset(manifestos) %>%
    unique() %>%
    nrow()
    
  ncovereddocs <- avl %>%
    subset(annotations) %>%
    unique() %>%
    nrow()
  
  ncoveredorigs <- avl %>%
    subset(originals) %>%
    unique() %>%
    nrow()
  
  ncoveredtranslen <- avl %>%
    subset(translation_en) %>%
    unique() %>%
    nrow()

  languages <- stats::na.omit(unique(avl$language))
  
  summary <- list('Queried for'=nqueried,
                  'Corpus Version'=attr(avl, "corpus_version"),
                  'Documents found'=paste(sum(avl$manifestos, na.rm = TRUE),
                                          " (", round(100*ncoveredtexts/nqueried, decs), "%)",
                                          sep=""),
                  'Coded Documents found'=paste(sum(avl$annotations, na.rm = TRUE),
                                      " (", round(100*ncovereddocs/nqueried, decs), "%)",
                                      sep=""),
                  'Originals found'=paste(sum(avl$originals, na.rm = TRUE),
                                        " (", round(100*ncoveredorigs/nqueried, decs), "%)",
                                        sep=""),
                  'English Translations found'=paste(sum(avl$translation_en, na.rm = TRUE),
                                        " (", round(100*ncoveredtranslen/nqueried, decs), "%)",
                                        sep=""),
                  Languages=paste(length(languages),
                                  " (", Reduce(paste, languages), ")", sep=""))
  
  class(summary) <- c("summaryDefault", "table")
  print(summary)
  
}

language_codes = function(iso2 = NULL) {
  languages <- c(ar = "arabic", bn = "bengali", zh = "chinese", en = "english", fr = "french",
                 de = "german", hi = "hindi", id = "indonesian", ja = "japanese",
                 pt = "portuguese", ru = "russian", es = "spanish", ur = "urdu")
  languages[iso2]
}

#' Get documents from the Manifesto Corpus Database
#' 
#' Documents are downloaded from the Manifesto Project Corpus Database. If 
#' CMP coding annotations are available, they are attached to the documents,
#' otherwise raw texts are provided. The documents are cached in the working
#' memory to ensure internal consistency, enable offline use and
#' reduce online traffic.
#' 
#' `mp_corpus_df` is a shorthand for getting the documents of the Manifesto Corpus
#' as a tibble/data.frame object instead of a ManifestoCorpus object. It takes
#' the same parameters as `mp_corpus`
#' (it is equivalent to \code{mp_corpus(..., as_tibble = TRUE)}).
#' See \code{\link{mp_save_cache}} for ensuring reproducibility by
#' saving cache and version identifier to the hard drive.
#' See \code{\link{mp_update_cache}} for updating the locally saved content with
#' the most recent version from the Manifesto Project Database API.
#'
#' `mp_corpus_df_bilingual` is a shorthand for getting the original text and the english
#' translations (or in case further translation languages become available also other
#' translation languages than english) from the Manifesto Corpus as a tibble/data.frame
#' object. The original text ends up in the "text" column and the english translation in
#' "text_en" (or more abstract in case of further translation languages in a column
#' named "text_<two digit ISO language code>"). It accepts the same additional parameters
#' as `mp_corpus_df`.
#'
#' @param ids Information on which documents to get. This can either be a
#'            list of partys (as ids) and dates of elections as given to
#'            \code{\link{mp_metadata}} or a \code{ManifestoMetadata} object
#'            (\code{data.frame}) as returned by \code{\link{mp_metadata}}.
#'        Alternatively, ids can be a logical expression specifying a subset of
#'        the Manifesto Project's main dataset. It will be evaluated within the
#'        data.frame returned by \code{\link{mp_maindataset}} such that all its
#'        variables and functions thereof can be used in the expression.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @param codefilter A vector of CMP codes to filter the documents: only quasi-sentences
#'        with the codes specified in \code{codefilter} are returned. If \code{NULL},
#'        no filtering is applied
#' @param codefilter_layer layer to which the codefilter should apply, defaults to cmp_code
#' @param translation A string containing the two digit ISO code of a translation language
#'        that should be used for the text instead of the original document language.
#'        Defaults to \code{NULL}, resulting in the original language of a document. For
#'        documents that are already originally in the requested translation language, it
#'        returns the original text. English would be "en".
#' @param as_tibble Boolean flag indicating whether to return a tibble/data.frame object
#'        instead of a ManifestoCorpus object, for backward compatibility defaults to FALSE
#' @param tibble_metadata A string specifing the handling of document-level metadata when
#'        using `as_tibble` = TRUE. It can be one of the following values:
#'        "none" = no metadata,
#'        "simplified" = basic metadata ("manifesto_id", "party", "date", "language", "annotations", "translation_en"),
#'        "all" = all metadata,
#'        defaults to "simplified"
#'              
#' @return an object of \code{\link[tm]{Corpus}}'s subclass
#' \code{\link{ManifestoCorpus}} holding the available of the requested documents
#' @export
#' @examples
#' \dontrun{
#' corpus <- mp_corpus(party == 61620 & rile > 10)
#' 
#' wanted <- data.frame(party=c(41320, 41320), date=c(200909, 201309))
#' mp_corpus(wanted)
#' 
#' mp_corpus(subset(mp_maindataset(), countryname == "France"))
#' 
#' partially_available <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' mp_corpus(partially_available)
#'
#' corpus_df <- mp_corpus(party == 61620 & rile > 10, as_tibble = TRUE)
#' corpus_df <- mp_corpus_df(party == 61620 & rile > 10)
#' corpus_df <- mp_corpus_df(party == 61620 & rile > 10, tibble_metadata = "all")
#'
#' mp_corpus(wanted, translation = "en")
#' mp_corpus_df(wanted, translation = "en")
#'
#' mp_corpus_df_bilingual(wanted, translation = "en")
#' }
mp_corpus <- function(ids,
                      apikey=NULL,
                      cache=TRUE,
                      codefilter = NULL,
                      codefilter_layer = "cmp_code",
                      translation = NULL,
                      as_tibble = FALSE,
                      tibble_metadata = "simplified") {

  if (!tibble_metadata %in% c("none", "simplified", "all"))
    stop("please provide a valid value for tibble_metadata parameter")

  ids <- as.metaids(substitute(ids), apikey=apikey, cache=cache, envir = parent.frame())

  n_ids <- nrow(ids)
  missings <- list()

  if ("title" %in% colnames(ids)) {
    ids <- base::subset(ids, !(!is.null(title) & !is.na(title) & tolower(title) %in% c("no document coded", "estimate")))
    missings$`no real documents coded (see progtype 3 and 99)` <- (n_ids - sum(unlist(missings), na.rm = TRUE)) - nrow(ids)
  }

  if (nrow(ids) > 0) {

    ids <- base::subset(ids, !is.naorstringna(manifesto_id))
    missings$`no machine-readable texts` <- (n_ids - sum(unlist(missings), na.rm = TRUE)) - nrow(ids)
    ids <- base::subset(ids, (is.null(codefilter) | annotations))
    missings$`no machine-readable annotations` <- (n_ids - sum(unlist(missings), na.rm = TRUE)) - nrow(ids)

    if (!is.null(translation)) {
      translation_column <- paste0("translation_", translation)
      ids <- dplyr::filter(ids, (!is.na(.data[[translation_column]]) & .data[[translation_column]] == TRUE) |
                                (!is.na(.data[["language"]]) & .data[["language"]] == language_codes(iso2 = translation)))
      missings$`no translations` <- (n_ids - sum(unlist(missings), na.rm = TRUE)) - nrow(ids)
    }
  }

  if (nrow(ids) > 0) {

    parameters <- list(ids = ids)
    if (!is.null(translation)) parameters$translation = translation
    corpus <- get_viacache(kmtype.text, parameters, apikey=apikey, cache=cache) %>%
      ManifestoJSONSource(query_meta = ids) %>%
      ManifestoCorpus()

  } else {
    
    corpus <- ManifestoCorpus()
    
  }
  
  missings$`no corpus response` <- nrow(ids) - length(corpus)

  ## codefilter
  if (!is.null(codefilter)) {
    corpus <- tm_map(corpus, function(doc) {
          return(subset(doc, codes(doc, codefilter_layer) %in% codefilter))
      })
  }

  if (any(!is.na(missings) & missings > 0)) {
    missings_stats <- missings[!is.na(missings) & missings > 0] %>%
      { paste0(., " having ", names(.)) } %>%
      paste0(collapse = ", ")
    message(paste0("Your query resulted in ", n_ids, " requested document items containing corpus metadata. Of these items ",
                   sum(unlist(missings), na.rm = TRUE), " could not be retrieved",
                   " (reasons: ", missings_stats, ")."))
  }
  
  if (as_tibble == TRUE) {
    corpus_df = corpus %>% as_tibble(with.meta = TRUE)
    if (nrow(corpus_df) == 0) return(corpus_df)
    metadata_columns = names(meta(corpus[[1]]))
    corpus_df = switch(tibble_metadata,
           "none" = corpus_df %>%
             select(-any_of(metadata_columns)),
           "simplified" = corpus_df %>%
             select(-any_of(setdiff(metadata_columns,
                                    c("manifesto_id", "party", "date", "language", "annotations", "translation_en")))),
           "all" = corpus_df)
    return(corpus_df)
  } else {
    return(corpus)
  }

}

#' `mp_corpus_df` is a shorthand for getting the documents of the Manifesto Corpus
#' as a tibble/data.frame object instead of a ManifestoCorpus object.
#' (it is equivalent to \code{mp_corpus(..., as_tibble = TRUE)}).
#'
#' @rdname mp_corpus
#' @export
mp_corpus_df <- function(ids,
                         apikey = NULL,
                         cache = TRUE,
                         codefilter = NULL,
                         codefilter_layer = "cmp_code",
                         translation = NULL,
                         tibble_metadata = "simplified") {
  if (!tibble_metadata %in% c("none", "simplified", "all"))
    stop("please provide a valid value for tibble_metadata parameter")

  ids <- as.metaids(substitute(ids), apikey=apikey, cache=cache, envir = parent.frame())
  
  mp_corpus(
    ids = ids,
    apikey = apikey,
    cache = cache,
    codefilter = codefilter,
    codefilter_layer = codefilter_layer,
    translation = translation,
    tibble_metadata = tibble_metadata,
    as_tibble = TRUE
  )
}

#' `mp_corpus_df_bilingual` is a shorthand for getting the original text and the english
#' translations (or in case further translation languages become available also other
#' translation languages than english) from the Manifesto Corpus as a tibble/data.frame
#' object. The original text ends up in the "text" column and the english translation in
#' "text_en" (or more abstract in case of further translation languages in a column
#' named "text_<two digit ISO language code>"). It accepts the same additional parameters
#' as `mp_corpus_df`.
#'
#' @rdname mp_corpus
#' @export
mp_corpus_df_bilingual <- function(ids,
                                   apikey = NULL,
                                   cache = TRUE,
                                   codefilter = NULL,
                                   codefilter_layer = "cmp_code",
                                   translation = "en",
                                   tibble_metadata = "simplified") {
  if (!tibble_metadata %in% c("none", "simplified", "all"))
    stop("please provide a valid value for tibble_metadata parameter")

  ids <- as.metaids(substitute(ids), apikey=apikey, cache=cache, envir = parent.frame())

  data_original <- mp_corpus_df(
    ids = ids,
    apikey = apikey,
    cache = cache,
    codefilter = codefilter,
    codefilter_layer = codefilter_layer,
    tibble_metadata = tibble_metadata
  )

  if (nrow(data_original) > 0) {
    data_translation <- mp_corpus_df(
      ids = ids,
      apikey = apikey,
      cache = cache,
      codefilter = codefilter,
      codefilter_layer = codefilter_layer,
      tibble_metadata = tibble_metadata,
      translation = "en"
    )

    column_translation = paste0("text_", translation)

    if (nrow(data_translation) > 0) {
      data_original %>%
        left_join(data_translation %>%
                    rename(all_of(setNames("text", column_translation))) %>%
                    select(manifesto_id, pos, all_of(column_translation)),
                  by = c("manifesto_id", "pos")) %>%
        select(text, all_of(column_translation), everything())
    } else {
      data_original %>%
        mutate(text_en = NA_character_) %>%
        select(text, all_of(column_translation), everything())
    }
  } else {
    tibble()
  }
}

is.nacode <- function(x) {
  return(is.na(x) | as.character(x) %in% c("NA", "n.a."))
}

#' View original documents from the Manifesto Corpus Database
#' 
#' Original documents are opened in the system's browser window. All original
#' documents are stored on the Manifesto Project Website and the URLs opened
#' are all from this site.
#' 
#'
#' @param ids Information on which originals to view This can either be a
#'            list of partys (as ids) and dates of elections as given to
#'            \code{\link{mp_metadata}} or a \code{ManifestoMetadata} object
#'            (\code{data.frame}) as returned by \code{\link{mp_metadata}}.
#'        Alternatively, ids can be a logical expression specifying a subset of
#'        the Manifesto Project's main dataset. It will be evaluated within the
#'        data.frame returned by \code{\link{mp_maindataset}} such that all its
#'        variables and functions thereof can be used in the expression.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available. The original documents themselves are not cached locally,
#'              but the metadata required to find them is.
#' @param maxn maximum number of documents to open simultaneously in browser,
#'       defaults to 5.
#' @examples
#' \dontrun{
#' mp_view_originals(party == 41320 & date == 200909)
#' }
#' @export
mp_view_originals <- function(ids, maxn = 5, apikey = NULL, cache = TRUE) {
  
  ids <- as.metaids(substitute(ids), apikey=apikey, cache=cache, envir = parent.frame())
  ids <- subset(ids, !is.na(url_original))
  
  if (nrow(ids) > maxn) {
    warning(paste("Attempt to open more than", maxn,
                  "URLs in browser prevented. If you are sure that this",
                  "is what you want, please increase the maxn parameter",
                  "of mp_view_originals"))
  } else {
    for (url in ids$url_original) {
      utils::browseURL(paste0(get(kurloriginalsroot, envir = mp_globalenv), url))
    }
  }

}

#' Print Manifesto Corpus citation information
#'
#' @param corpus_version corpus version for which citation should be printed
#' @param core_versions core version for which citation should be printed
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @export
mp_cite <- function(corpus_version = mp_which_corpus_version(),
                    core_versions = mp_which_dataset_versions(),
                    apikey = NULL) {
  
  cite_message <- kcitemessage
  cite_data <- tibble()
  
  if (is.null(apikey) && is.na(getn("apikey", envir = mp_globalenv))) {
    cite_message <- paste0(cite_message, "\n\n",
        "No API key specified. For generation as well as citation information ",
        "please go to https://manifesto-project.wzb.eu.")
  } else {
    
    if (!is.null(corpus_version) && !is.na(corpus_version)) {
      cite_string <- get_citation(corpus_version, kmtype.corpuscitation, apikey = apikey)
      cite_message <- paste0(cite_message, "\n\n",
                             "You're currently using corpus version ", corpus_version, ", ",
                             "please cite as\n\n",
                             cite_string)
      cite_data <- cite_data %>%
        bind_rows(tibble(data = "corpus",
                         source = "MARPOR",
                         version = corpus_version,
                         citation = cite_string))
      
      corpus_cache <- manifestos_in_cache() %>%
                        select(party, date) %>%
                        mp_metadata(apikey = apikey)
      if (!is.null(corpus_cache) && 
          "source" %in% names(corpus_cache)) {
        if(any(corpus_cache$source == "CEMP")) {
          cite_string <- get_citation("CEMP", kmtype.corpuscitation, apikey = apikey)
          cite_message <- paste0(cite_message, "\n\n",
                                 "You have downloaded uncoded machine-readable manifesto texts, ",
                                 "which have been originally created in the Comparative ",
                                 "Electronic Manifestos Project. ",
                                 "Please cite additionally", "\n\n",
                                 cite_string)
          cite_data <- cite_data %>%
            bind_rows(tibble(data = "corpus-additional",
                             source = "CEMP",
                             version = corpus_version,
                             citation = cite_string))
        }
        if(any(corpus_cache$source == "MZES")) {
          cite_string <- get_citation("MZES", kmtype.corpuscitation, apikey = apikey)
          cite_message <- paste0(cite_message, "\n\n",
                                 "You have downloaded uncoded machine-readable manifesto texts, ",
                                 "which have been originally created in cooperation with the ",
                                 "Mannheimer Zentrum fuer Europaeische Sozialforschung.",
                                 "Please cite additionally", "\n\n",
                                 cite_string)
          cite_data <- cite_data %>%
            bind_rows(tibble(data = "corpus-additional",
                             source = "MZES",
                             version = corpus_version,
                             citation = cite_string))
        }
      }
    } else {
      cite_message <- paste0(cite_message, "\n\n",
                             "You're manifestoR cache does not contain any corpus version identification. ",
                             "Please load a cache, download data or specify the corpus version ",
                             "manually in mp_cite() to obtain citation information.")
    }
    
    if (length(core_versions) > 0) {
      if (length(core_versions) > 1 && "MPDSSA9999a" %in% core_versions) {
        core_versions <- setdiff(core_versions, c("MPDSSA9999a"))
      }

      cite_strings <- core_versions %>%
        sapply(get_citation, type = kmtype.corecitation, apikey = apikey)
      cite_message <- paste0(cite_message, "\n\n",
                             "You are using Manifesto Project Dataset version(s) ",
                             paste(core_versions, collapse = ", "), ", please cite as \n\n", 
                             paste(cite_strings, collapse = "\n\n"))
      cite_data <- cite_data %>%
        bind_rows(tibble(data = rep("dataset", length(core_versions)),
                             source = rep("MARPOR", length(core_versions)),
                             version = core_versions,
                             citation = cite_strings))
    }
  }

  message(cite_message)
  
  return(cite_data)

}
