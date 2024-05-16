#' Access to the Parties Lists for the Manifesto Project Data
#' 
#' @description These functions provide access to machine-readable versions
#' of the List of Parties of the Manifesto Project Data as can be found in CSV form 
#' under https://manifesto-project.wzb.eu/datasets .
#' Note: the list of parties is not available for all of the past datasets. You can check the 
#' availability by going to the datasets page and check for specific datasets whether they have 
#' party lists in the following formats "List – Short (CSV)" or "List – Long (CSV)" available. 
#' There you can also find the codebooks with details for these list of parties.
#' 
#' @details 
#' \code{mp_parties} returns the list of parties as a \code{tibble}, ideal for further automatic processing.
#' 
#' @param version version of the Manifesto Project Main Dataset for which the
#' list of parties is requested. Note: the list of parties is not available for all of the past datasets. 
#' Defaults to "current", which fetches the most recent version. Must be formatted as e.g. "MPDS2023a".
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#' the API key set via \code{\link{mp_setapikey}}.
#' @param cache Whether result of API call should be cached locally (defaults to TRUE)
#' @param list_form Whether the result should be the short or the long version (defaults to "short")
#' 
#' @export
mp_parties <- function(version = "current", apikey = NULL, cache = TRUE, list_form = "short") {
  
  version_year <- suppressWarnings(as.integer(gsub("(MPDS)(\\d{4})(a|b)", "\\2", version)))
  version_sub <- gsub("(MPDS)(\\d{4})(a|b)", "\\3", version)

  if (version == "current") {
    version <- current_dataset_version(south_america = FALSE)
  }
  
  get_viacache(kmtype.parties,
               ids = list(key = version, list_form = list_form),
               cache = cache,
               apikey = apikey,
               versionid = "CONST")
  
}
