#' Process CMP codings
#' 
#' Several functions to process the CMP codings
#' 
#' @param x Vector of codes, ManifestoDocument or ManifestoCorpus
#' 
#' @details
#' \code{recode_cee_codes} recode the sub-categories used
#' in coding several manifestos in Central and Eastern Europe (4 digits) to
#' the main categories in the coding scheme (3 digits).
#' 
#' @rdname cmp_codes
#' @export
recode_cee_codes <- function(x) {
  UseMethod("recode_cee_codes", x)
}

#' @method recode_cee_codes default
#' @export
recode_cee_codes.default <- function(x) {
  for (i in names(cee_aggregation_relations())) { 
    x = gsub(paste0("^(", paste0(gsub("per", "", cee_aggregation_relations()[[i]]), collapse = "|"), ")$"), gsub("per", "", i), x)
  }
  x
}

#' @method recode_cee_codes ManifestoDocument
#' @export
recode_cee_codes.ManifestoDocument <- function(x) {
  doc <- x
  codes(doc) <- recode_cee_codes(codes(doc))
  return(doc)
}

#' @method recode_cee_codes ManifestoCorpus
#' @export
recode_cee_codes.ManifestoCorpus <- function(x) {
  tm_map(x, recode_cee_codes)
}

#' `aggregate_cee_codes` is deprecated and will be removed in a future version of
#' manifestoR. Please use only `recode_cee_codes`, which provides the exact same 
#' functionality, but is more consistent in its name.
#' 
#' @rdname cmp_codes
#' @export
aggregate_cee_codes <- function(x) {
  .Deprecated("recode_cee_codes", package = "manifestoR")
  recode_cee_codes(x)
}


#' @rdname cmp_codes
#' 
#' @details
#' \code{recode_v5_to_v4} recode the CMP codings according to
#' the more specialized Coding Handbook Version 5 to the more general
#' categories of Handbook Version 4. Codes 202.2, 605.2 and 703.2 are
#' converted to a 000, while all other subcategory codes with an appended
#' dot and fourth digit are aggregated to the corresponding three-digit 
#' main category.
#' 
#' @export
recode_v5_to_v4 <- function(x) {
  UseMethod("recode_v5_to_v4", x)
}

#' @method recode_v5_to_v4 default
#' @export
recode_v5_to_v4.default <- function(x) {
  x <- as.character(x)
  x[x %in% c("202.2", "605.2", "703.2")] <- "0"
  return(gsub("^(\\d{3})\\.\\d$", "\\1", x))
}

#' @method recode_v5_to_v4 ManifestoDocument
#' @export
recode_v5_to_v4.ManifestoDocument <- function(x) {
  doc <- x
  codes(doc) <- recode_v5_to_v4(codes(doc))
  return(doc)
}

#' @method recode_v5_to_v4 ManifestoCorpus
#' @export
recode_v5_to_v4.ManifestoCorpus <- function(x) {
  tm_map(x, recode_v5_to_v4)
}

#' Lists of categories and category relations
#' 
#' Code numbers of the Manifesto Project's category scheme. For documentation
#' see \url{https://manifesto-project.wzb.eu/datasets}.
#' 
#' @rdname categories
#' @export 
v4_categories <- function() {
  c("uncod",  101, 102, 103, 104, 105,
    106, 107, 108, 109, 110, 201,
    202, 203, 204, 301, 302, 303,
    304, 305, 401, 402, 403, 404,
    405, 406, 407, 408, 409, 410,
    411, 412, 413, 414, 415, 416,
    501, 502, 503, 504, 505, 506,
    507, 601, 602, 603, 604, 605,
    606, 607, 608, 701, 702, 703,
    704, 705, 706)
}

#' @param include_parents include v5-categories that have subcategories
#' 
#' @rdname categories
#' @export 
v5_categories <- function(include_parents = TRUE) {
  v5_v4_aggregation_relations() %>%
    unlist() %>%
    { gsub("per", "", .) } %>%
    { gsub("_", ".", .) } %>%
    c(v4_categories()) %>%
    unique() %>%
    { if (!include_parents) purrr::discard(., ~ { .x %in% gsub("per", "", setdiff(names(v5_v4_aggregation_relations()), "peruncod")) }) else . } %>%
    sort()
}

#' @rdname categories
#' @export 
cee_categories <- function() {
  c("1011", "1012", "1013", "1014", "1015", "1016", "1021", "1022", 
    "1023", "1024", "1025", "1026", "1031", "1032", "1033", 
    "2021", "2022", "2023", "2031", "2032", "2033", "2041", 
    "3011", "3051", "3052", "3053", "3054", "3055", 
    "4011", "4012", "4013", "4014", "4121", "4122", "4123", "4124", 
    "4131", "4132", 
    "5021", "5031", "5041", "5061", 
    "6011", "6012", "6013", "6014", "6061", "6071", "6072", "6081", 
    "7051", "7052", "7061", "7062")
}

#' @rdname categories
#' @export 
v5_v4_aggregation_relations <- function() {
  list(peruncod = c("peruncod", "per202_2", "per605_2", "per703_2"),
       per103 = c("per103", "per103_1", "per103_2"),
       per201 = c("per201", "per201_1", "per201_2"),
       per202 = c("per202", "per202_1", "per202_3", "per202_4"),
       per305 = c("per305", "per305_1", "per305_2", "per305_3", "per305_4", "per305_5", "per305_6"),
       per416 = c("per416", "per416_1", "per416_2"),
       per601 = c("per601", "per601_1", "per601_2"),
       per602 = c("per602", "per602_1", "per602_2"),
       per605 = c("per605", "per605_1"),
       per606 = c("per606", "per606_1", "per606_2"),
       per607 = c("per607", "per607_1", "per607_2", "per607_3"),
       per608 = c("per608", "per608_1", "per608_2", "per608_3"),
       per703 = c("per703", "per703_1")
  )
}

#' @rdname categories
#' @export 
cee_aggregation_relations <- function() {
  list(
    per101 = c("per101","per1011","per1012","per1013","per1014","per1015","per1016"),
    per102 = c("per102","per1021","per1022","per1023","per1024", "per1025","per1026"),
    per103 = c("per103","per1031","per1032","per1033"),
    per202 = c("per202","per2021","per2022","per2023"),
    per203 = c("per203","per2031","per2032", "per2033"),
    per204 = c("per204","per2041"),
    per301 = c("per301","per3011"),
    per305 = c("per305","per3051","per3052","per3053","per3054","per3055"),
    per401 = c("per401","per4011","per4012","per4013","per4014"),
    per412 = c("per412","per4121","per4122","per4123","per4124"),
    per413 = c("per413","per4131","per4132"),
    per502 = c("per502","per5021"),
    per503 = c("per503","per5031"),
    per504 = c("per504","per5041"),
    per506 = c("per506","per5061"),
    per601 = c("per601","per6011", "per6012","per6013","per6014"),
    per606 = c("per606","per6061"),
    per607 = c("per607","per6071","per6072"),
    per608 = c("per608","per6081"),
    per705 = c("per705","per7051","per7052"),
    per706 = c("per706","per7061","per7062")
  )
}


baeck_policy_dimensions <- function() {
  list(foreign = c(101, 102, 103, 106, 107, 108, 109, 110),
       defence = c(104, 105),
       interior = c(201, 202, 203, 204, 301, 302,
                    303, 304, 605, 607, 608),
       justice = c(201, 202, 203, 204, 303, 304, 605),
       finance = c(402, 414),
       economy = c(401, 403, 404, 405, 406, 407, 408, 409, 410, 412, 413, 415),
       labour = c(504, 505, 701, 702),
       education = c(506, 507),
       health = c(504, 505, 706),
       agriculture = c(703),
       industry = c(401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 412, 413, 414),
       environment = c(416, 501),
       social_affairs = c(503, 603, 604, 606, 705, 706)
       ) %>%
    lapply(prefix, "per")
}

bischof_issue_groups <- function() {
  list(
    ecology = c(416, 410, 501, 106),
    nationalist = c(601, 602, 605, 607, 608),
    agrarian = c(703),
    regional = c(301, 302, 706),
    europe = c(406, 407, 108, 110)
    ) %>%
    lapply(prefix, "per")
}

meyer_miller_2013_policy_dimensions <- function() {
  baeck_policy_dimensions() %>% { .$industry <- NULL; . }
}

#' Default programmatic clarity dimensions from 
#' Giebler/Lacewell/Regel/Werner 2015.
#'
#' @references Giebler/Lacewell/Regel/Werner (2015). Mass, Catch-all, or 
#' Programmatic? Toward an Empirical Classification of Party Types. Manuscript.
#'
#' @export
clarity_dimensions <- function() {
  list(
    "fsp" = list(pole_1 = c(101), pole_2 = c(102)),
    "mil" = list(pole_1 = c(104), pole_2 = c(105)),
    "int" = list(pole_1 = c(107), pole_2 = c(109)),
    "eui" = list(pole_1 = c(108), pole_2 = c(110)),
    "con" = list(pole_1 = c(203), pole_2 = c(204)),
    "cen" = list(pole_1 = c(301), pole_2 = c(302)),
    "mre" = list(pole_1 = c(401), pole_2 = c(403)),
    "pro" = list(pole_1 = c(406), pole_2 = c(407)),
    "fis" = list(pole_1 = c(409), pole_2 = c(414)),
    "wel" = list(pole_1 = c(504), pole_2 = c(505)),
    "edu" = list(pole_1 = c(506), pole_2 = c(507)),
    "nwl" = list(pole_1 = c(601), pole_2 = c(602)),
    "tmo" = list(pole_1 = c(603), pole_2 = c(604)),
    "mul" = list(pole_1 = c(607), pole_2 = c(608)),
    "lab" = list(pole_1 = c(701), pole_2 = c(702))
  ) %>%
    lapply(lapply, prefix, "per")
}


#' Aggregate category percentages in groups
#' 
#' \code{aggregate_pers} is a general function to aggregate percentage variables by creating a new
#' variable holding the sum. If a variable with the name for the aggregate
#' already exists and if it is part of the `overwrite` parameter, it is
#' overwritten.
#' 
#' @param data dataset to use in aggregation
#' @param groups (named) list of variable name vectors to aggregate to a new one
#' (as given in the name); see default value for an example of the format
#' @param na.rm passed on to \code{\link{sum}}
#' @param keep keep variables that were aggregated in result?
#' @param overwrite Names of the variables that are allowed to be overwritten by
#' aggregate. Defaults to all aggregate variable names.
#' @param verbose show messages in case of possibly problematic side effects
#' @seealso \code{\link{aggregate_pers_cee}}
#' 
#' @rdname aggregate_pers
#' @export
aggregate_pers <- function(data,
                           groups = v5_v4_aggregation_relations(),
                           na.rm = FALSE,
                           keep = FALSE,
                           overwrite = names(groups),
                           verbose = TRUE) {
  
  data <- 
    Reduce(function(data, aggregate) {
        aggregated <- data %>%
          select(one_of(intersect(groups[[aggregate]], names(data))))
        if (ncol(aggregated) != 0L) {
          aggregated <- rowSums(aggregated, na.rm = na.rm)
          if (aggregate %in% names(data)) {
            if (aggregate %in% overwrite) {
              if (verbose) {
                if (any(!is.na(data[,aggregate]) &
                        data[,aggregate] != 0.0 &
                        !is.na(aggregate) &
                        na_replace(data[,aggregate] != aggregated, TRUE))) {
                  message(paste0("Changing non-zero supercategory per value ", aggregate,
                                 " when aggregating subcategory percentages"))
                }
                if (any(!is.na(data[,aggregate]) &
                        is.na(aggregated))) {
                  message(paste0("Changing non-NA supercategory per value ", aggregate, " to NA",
                                 " when aggregating subcategory percentages"))
                }
              }
              data[,aggregate] <- aggregated
            }
          } else {
            data[,aggregate] <- aggregated
          }
        }
        data
      },
      names(groups),
      init = data)
  
  if (!keep) {
    data %>%
      select(-one_of(setdiff(intersect(names(data), unlist(groups)), names(groups))))
  } else {
    data
  }
    
}

#' Aggregate cee-categories to main categories
#' 
#' Adds the code frequencies in a dataset of the 4 digit per-variables (per1011 to per7062 - mostly used in codings
#' of Central and Eastern European countries) to the main categories in the coding scheme (3 digits).
#' 
#' A wrapper of \code{\link{aggregate_pers}} using \code{cee_aggregation_relations}. 
#' 
#' @param data dataset to use in aggregation
#' @seealso \code{\link{aggregate_pers}}
#' @export
aggregate_pers_cee <- function(data) {
  aggregate_pers(data, 
                 groups = cee_aggregation_relations(),
                 keep = TRUE,
                 na.rm = TRUE)
}

#' Count the codings from a ManifestoDocument
#'
#' @param doc ManifestoDocument, ManifestoCorpus or vector of codes
#' @param code_layers vector of names of code layers to use, defaults to cmp_code; Caution:
#' The layer eu_code is handled separately in the parameter with_eu_codes due to its
#' different logic
#' @param with_eu_codes Whether to include special EU code layer; by default ("auto") taken
#' from the document's metadata
#' @param prefix prefix for naming the count/percentage columns in the resulting data.frame
#' @param relative If true, percentages are returned, absolute counts else; the percentages
#' are calculated after the exclusion of the categories provided in `drop_codes`
#' @param include_codes Vector of categories that should be included even if they are not
#' present in the data; the value of the created variables then defaults to 0.0 (or NA if
#' no codes are present at all); in contrast to `drop_codes` this only adds variables
#' for possibly missing categories but does not remove any; Defaults to `v4_categories()` if
#' `code_layers` parameter contains `cmp_code`
#' @param aggregate_v5_subcategories if TRUE, for handbook version 5 subcategories, the aggregate
#' category's count/percentage is computed as well
#' @param drop_codes Vector of categories that should be excluded even if they are
#' present in the data; Defaults to `c("H")` if `code_layers` parameter contains `cmp_code`
#' @return A data.frame with onw row and the counts/percentages as columns
#' @export
count_codes <- function(doc,
                        code_layers = c("cmp_code"),
                        with_eu_codes = "auto",
                        prefix = "per",
                        relative = TRUE,
                        include_codes = if("cmp_code" %in% code_layers)
                                          { v4_categories() } else { c() },
                        aggregate_v5_subcategories = TRUE,
                        drop_codes = if("cmp_code" %in% code_layers)
                                       { c("H") } else { c() }) {
  UseMethod("count_codes", doc)
}

fix_names_code_table <- function(df, prefix, include_codes) {
  
  if (length(include_codes) > 0) {
    
    ensure_names <- paste0(prefix, include_codes) %>%
    { gsub(".", "_", ., fixed = TRUE)} %>%
    { subset(., !grepl(paste0("$(", prefix, ").+^"), .)) } %>%
      as.character() %>%
      paste(collapse = ")|(") %>%
      { paste0("^(", . , ")$") }
    
    the_order <- order(names(df))
    df %>%
      select(all_of(the_order)) %>%
      select(dplyr::matches("party"),
             dplyr::matches("date"),
             dplyr::starts_with(prefix),
             dplyr::matches(ensure_names),
             dplyr::matches("total"))
    
    
  } else {
    
    the_order <- order(names(df))
    df %>%
      select(all_of(the_order)) %>%
      select(dplyr::matches("party"),
             dplyr::matches("date"),
             dplyr::starts_with(prefix),
             dplyr::matches("total"))
    
  }
  
}

fix_missing_counted_codes <- function(df, relative = TRUE) {
  
  m <- is.na(df)
  m[which(df$total <= 0),] <- FALSE
  df[m] <- if (relative) 0.0 else 0L
  
  return(df)
  
}

#' @export
count_codes.ManifestoCorpus <- function(doc,
                                        code_layers = c("cmp_code"),
                                        with_eu_codes = "auto",
                                        prefix = "per",
                                        relative = TRUE,
                                        include_codes = if("cmp_code" %in% code_layers)
                                                          { v4_categories() } else { c() },
                                        aggregate_v5_subcategories = TRUE,
                                        drop_codes = if("cmp_code" %in% code_layers)
                                                       { c("H") } else { c() }) {
  
  lapply(content(doc),
         count_codes,
         code_layers, with_eu_codes, prefix, relative, include_codes, aggregate_v5_subcategories, drop_codes) %>%
    bind_rows() %>%
    fix_missing_counted_codes(relative = relative) %>%
    fix_names_code_table(prefix,
                         include_codes = include_codes)
  
}

#' @export
count_codes.ManifestoDocument <- function(doc,
                        code_layers = c("cmp_code"),
                        with_eu_codes = "auto",
                        prefix = "per",
                        relative = TRUE,
                        include_codes = if("cmp_code" %in% code_layers)
                                          { v4_categories() } else { c() },
                        aggregate_v5_subcategories = TRUE,
                        drop_codes = if("cmp_code" %in% code_layers)
                                       { c("H") } else { c() }) {
  
  if (with_eu_codes == "auto") {
    with_eu_codes <- meta(doc, "has_eu_code")
    if (is.null(with_eu_codes)) {
      with_eu_codes <- FALSE
    }
  }
  the_codes <- c()
  if ("eu_code" %in% code_layers) {
    warning("eu_code is included in code_layers, but should be included via the with_eu_codes parameter to respect its logic!")
  }
  for (layer in code_layers) {
    the_codes <- c(the_codes, as.character(codes(doc, layer)))
  }
  if (length(with_eu_codes) > 0 && with_eu_codes) {
    eu_codes <- codes(doc, "eu_code")
    the_codes <- c(the_codes, eu_codes[!is.na(eu_codes) & eu_codes != 0L])
  }
  data.frame(party = null_to_na(meta(doc, "party")),
             date = null_to_na(meta(doc, "date"))) %>%
    bind_cols(count_codes(the_codes, code_layers, with_eu_codes, prefix, relative, include_codes, aggregate_v5_subcategories, drop_codes))
  
  
}

#' @export
count_codes.default <- function(doc,
                                code_layers = c("cmp_code"),
                                with_eu_codes = "auto",
                                prefix = "per",
                                relative = TRUE,
                                include_codes = if("cmp_code" %in% code_layers)
                                                  { v4_categories() } else { c() },
                                aggregate_v5_subcategories = TRUE,
                                drop_codes = if("cmp_code" %in% code_layers)
                                               { c("H") } else { c() }) {
  
  tt <- table(doc)
  
  if(length(drop_codes) > 0){
    tt = tt[!names(tt) %in% drop_codes]
  }

  df <- as.data.frame(t(as.matrix(tt)))
  if (ncol(df) > 0) {
    names(df) <- paste0(prefix, names(df))
    if (relative) {
      n <- sum(df[1,])
      df[1,] <- df[1,]/n * 100
      df$total <- n
    }
  } else {
    df <- data.frame(total = 0L)
  }
  
  df <- aggregate_pers(df,
                       groups = list(peruncod = c("per0", "per000")),
                       keep = FALSE,
                       na.rm = TRUE)

  names(df) <- gsub(".", "_", names(df), fixed = TRUE)
    
  if (length(include_codes) > 0) {
    for (the_name in setdiff(paste0(prefix, gsub(".", "_", include_codes, fixed = TRUE)), names(df))) {
      df[,the_name] <- ifelse(df$total == 0L, NA_real_, 0.0)
    }
  }
  
  if (aggregate_v5_subcategories) {
    df <- aggregate_pers(df,
                         groups = v5_v4_aggregation_relations(),
                         keep = TRUE)
  }

  return(df)    
  
}
