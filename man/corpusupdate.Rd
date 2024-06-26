% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache.R
\name{mp_check_for_corpus_update}
\alias{mp_check_for_corpus_update}
\alias{mp_which_corpus_version}
\alias{mp_which_dataset_versions}
\alias{mp_update_cache}
\title{Check for Updates of Corpus in Manifesto Project DB}
\usage{
mp_check_for_corpus_update(apikey = NULL, only_stable = TRUE)

mp_which_corpus_version(cache_env = mp_cache())

mp_which_dataset_versions(cache_env = mp_cache())

mp_update_cache(apikey = NULL, only_stable = TRUE)
}
\arguments{
\item{apikey}{API key to use. Defaults to \code{NULL}, resulting in using
the API key set via \code{\link{mp_setapikey}}.}

\item{only_stable}{Consider only for versions marked as stable by the Manifesto
Projec Team, defaults to TRUE}

\item{cache_env}{Cache environment}
}
\value{
\code{mp_update_cache} returns a list with a boolean
        \code{update_available} and \code{versionid},
        a character string identifying the most recent online version available

\code{mp_which_corpus_version} returns the current version id of the
corpus and metadata stored in the cache

\code{mp_which_dataset_versions} returns the names of the main dataset
versions which are in the cache, i.e. have been downloaded

\code{mp_update_cache} returns the character identifier of the version updated to
}
\description{
\code{mp_check_for_copus_update} checks if the currently cached version of corpus text and metadata
is older than the most recent version available via the Manifesto Project
DB API.
}
\details{
\code{mp_update_cache} checks if a new corpus version is available and loads
the new version via: \code{\link{mp_use_corpus_version}}. That is, 
the internal cache of manifestoR will automatically be updated to newer version
and all future calls to the API will request for the newer version.
Note that updating/downgrading the corpus version after having already
downloaded translated manifestos is not yet implemented and will result in
an error message.


Note that this versioning applies to the corpus' texts and metadata, and not the
versions of the core dataset. For this see \code{\link{mp_coreversions}}
}
