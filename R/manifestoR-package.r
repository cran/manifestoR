#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom purrr map map_if map_at map_chr map_dbl map_chr map_df map_dfc map_lgl map_int walk
#' @importFrom NLP content `content<-` meta `meta<-`
#' @import tibble
#' @import dplyr
#' @import tidyselect
#' @import functional
#' @importFrom tm Corpus VCorpus SimpleSource PlainTextDocument getElem tm_map
#' @import jsonlite
## usethis namespace: end
NULL

## A fix to let CRAN check NOTEs diasappear for non-standard-evaluation used
## cf. http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(c("one_of", "all_of", ".", "matches", "starts_with", "contains", # dplyr
                  "manifesto_id", "country", "party", "edate", "per0", # general dataset
                  "md5sum_text.x", "md5sum_text.y", "code", "manifestos", "title", "translation_en", # & metadata & api download
                  "download", "url_original", "is_primary_doc", "originals", "annotations",
                  "name", "tag", # metadata versions
                  "idx", "text", "pos", # general
                  "the_score", "leadedate", "leglength", "countryname", "w", "p", # scaling
                  "cols", "col_integer", "col_character", "col_date", "col_double", # readr
                  "p_lead", "p_lag", "lrcorescores", # median_voter_single
                  "position", "voteshare", "cumvoteshare",
                  "above50", "contains_median", "leftbound", "rightbound",
                  "min_divers", "max_divers", "nicheness", "min_nic", ## nicheness
                  "max_nic", "specialization", "min_spec", "max_spec",
                  "specialization_stand", "specialization_stand_two", "nicheness_stand",
                  "tmp_mp_clarity_sum", "data_2", "score" # clarity measure
))
