#' Left-Right Scores based on Franzmann & Kaiser Method
#' 
#' Computes left-right scores based on the Franzmann & Kaiser Method (see
#' reference below). The issue structures are not calculated from scratch but
#' taken as given from Franzmann 2009 (or later updates). Note that they 
#' are not available for the entire Manifesto Project Dataset, but only for a 
#' subset of countries and elections.
#' 
#'
#' @param data A data.frame with cases to be scaled, variables named "per..."
#' @param basevalues flag for transforming data to be relative to the minimum
#' @param smoothing flag for using smoothing
#' @param vars Variables/Categories to use for computation of score. Defaults to all
#' available handbook version 4 categories.
#' @param issue_structure issue structure to use for Franzmann & Kaiser method, default 
#' to most recent bundled version (for details see \code{read_fk_issue_structure})
#' @param party_system_split function to recode the country variable to re-partition
#' party systems. Defaults to splitting Belgium into two halfs as done in Franzmann 2009
#' @param mean_presplit if TRUE, for Belgium as a whole (before the split into two
#' party systems) the mean of the issue weights is used (which is equal to taking
#' the mean of the output values, since all subsequent transformations are linear). This step
#' is required to replicate the Franzmann 2009 dataset.
#' @param ... passed on to \code{fk_smoothing} and \code{party_system_split}
#' @references Franzmann, Simon/Kaiser, Andre (2006): Locating Political Parties in Policy Space. A Reanalysis of Party Manifesto Data, Party Politics, 12:2, 163-188
#' @references Franzmann, Simon (2009): The Change of Ideology: How the Left-Right Cleavage transforms into Issue Competition. An Analysis of Party Systems using Party Manifesto Data. PhD Thesis. Cologne.
#' @export
franzmann_kaiser <- function(data,
                             basevalues = TRUE,
                             smoothing = TRUE,
                             vars = grep("per\\d{3}$", names(data), value = TRUE),
                             issue_structure = read_fk_issue_structure(mean_presplit = mean_presplit),
                             party_system_split = split_belgium,
                             mean_presplit = TRUE,
                             ...) {

  if (!("country" %in% names(data)) & ("party" %in% names(data))) {
    data <- mutate(data, country = as.integer(substr(party, 1, 2)))
  }

  if (!is.null(party_system_split) & is.function(party_system_split)) {
    data <- party_system_split(data, ...)
  }
  
  if (basevalues) {
    ## calculates positional scores = saliency scores - base value // pos scores = (x - min(x)) where x is saliency score
    data <- data %>%
      group_by(country, edate) %>%
      mutate_at(vars, .funs = ~{ .-min(., na.rm=TRUE) }) %>%
      ungroup()
  }
  
  data %>%
    select(all_of(c("country", "edate"))) %>%
    left_join(issue_structure, by = c("country", "edate")) %>%
    select(all_of(vars)) %>%
    { scale_weighted(data, vars = vars, weights = .) /
        scale_weighted(data, vars = vars, weights = 1) } -> fkscores
   
  if (smoothing) {
    combined <- cbind(data, fkscores)
    fkscores <- fk_smoothing(data = combined, score_name = "fkscores", ...)
  }
   
  return((fkscores + 1) * 5)

}

#' @importFrom magrittr set_names
#' @importFrom readr cols col_integer col_character col_date col_double
#' @param path path from were to read issue structures (as csv data file). Defaults
#' to the most recent file bundled in the manifestoR package.
#' @param mean_presplit if TRUE, for Belgium as a whole (before the split into two
#' party systems) the mean of the issue weights is used (which is equal to taking
#' the mean of the output values, since all subsequent transformations are linear). This step
#' is required to replicate the Franzmann 2009 dataset. If the issue structures already
#' contain values for Belgium as a whole they are overwritten by the newly generated ones.
#' @param format_version can be 1 or 2 to switch between different structural versions 
#' of the issue structures file (1 for files containing "structure"-columns, 
#' 2 for files containing "per"-columns)
#' @export
#' @rdname franzmann_kaiser
read_fk_issue_structure <- function(path = system.file("extdata", "fk_issue_structure_2019.csv", package = "manifestoR"),
                                    mean_presplit = TRUE,
                                    format_version = 2) {

  if (!format_version %in% c(1, 2)) { stop("please provide a valid format_version.") }
  
  data = switch(format_version,
    `1` = {
      path %>%
        readr::read_csv(col_types = cols(country = col_integer(), countryname = col_character(), edate = col_date(format = ""), .default = col_double())) %>%
        { set_names(., gsub("e(\\d+)_structure", "per\\1", names(.))) } %>%
        select(-countryname)
    },
    `2` = {
      path %>%
        readr::read_csv(col_types = cols(country = col_integer(), edate = col_date(format = ""), .default = col_character())) %>%
        select(country, edate, starts_with("per")) %>%
        mutate_at(vars(starts_with("per")), ~dplyr::recode(.x, "valence" = 0, "left" = -1, "right" = 1))
    })
  
  data %>%
    iff(mean_presplit, function(data) {
      data %>%
        { (select(subset(., country == 218), starts_with("per")) + 
             select(subset(., country == 219), starts_with("per")))/2 } %>%
        set_names(subset(names(data), grepl("^per", names(data)))) %>%
        mutate(country = 21) %>%
        bind_cols(data %>% subset(country == 218) %>% select(edate)) %>%
        bind_rows(data %>% filter(country != 21)) %>%
        arrange(country, edate)
    })

}


#' Split Belgium party system into separate groups
#' 
#' Recodes the country variable of a dataset to 218 (Flanders parties)
#' and 219 (Wallonia parties) from 21 for Belgium
#' 
#' @param data data.frame in format of the Manifesto Project's Main Dataset
#' @param wallonia_parties Party codes for the Wallonia half
#' @param flanders_parties Party codes for the Flanders half
#' @param brussels_parties Party codes for Brussel specific parties, are recoded to NA
#' @param belgium_parties Party codes for complete system, coded as presplit_countrycode
#' @param presplit_countrycode Country code for the belgium_parties
#' @param ... ignored
#' @export
split_belgium <- function(data,
                          wallonia_parties = c(21111, 21322, 21422, 21423, 21425, 21426, 21522, 21911),
                          brussels_parties = c(21424, 21912),
                          belgium_parties = c(21320, 21420, 21520),
                          flanders_parties = c(21112, 21221, 21321, 21330, 21421, 21430, 21521, 21913, 21914, 21915, 21916, 21917),
                          presplit_countrycode = 21,
                          ...) {
  data %>%
    mutate(country = ifelse(country == 21,
                            ifelse(party %in% wallonia_parties, 219,
                            ifelse(party %in% flanders_parties, 218,
                            ifelse(party %in% brussels_parties, NA_integer_,
                            presplit_countrycode))),
                            country))
}

#' @export
#' @rdname franzmann_kaiser
#' @param score_name name of variable with LR Score values to be smoothed
#' @param use_period_length whether to use electoral period length in weighting
fk_smoothing <- function(data, score_name, use_period_length = TRUE, ...) {
  
   # how to deal with "holes" (if party drops out of parliament and then enters later, sorting by party date, does not reflect this...)

   ## check if score is in data
   if (!score_name %in% names(data)) {
      stop("score name not found in data")   
   }
   
   if (!"party" %in% names(data)) {
      stop("no party variable found in data")
   }
   if (!"edate" %in% names(data)) {
      stop("no date variable found in data")
   }
   if (!("Date" %in% class(data$edate))) {
      stop("variable date is not a date")
   }
  
  lagl <- function(x) { dplyr::lag(x, default = first(x)) }
  leadl <- function(x) { dplyr::lead(x, default = last(x)) }
  
   data[,"the_score"] <- data[,score_name]
   data$n <- c(1:nrow(data))
   smoothed <- data %>% 
      group_by(party) %>% 
        select(one_of("party","country","edate","the_score","n")) %>% 
        arrange(edate) %>%
        mutate(
           leadedate = leadl(edate),
           leglength = as.numeric(difftime(leadedate, edate, units="days")),
           w = leglength/(lagl(leglength) + leglength + leadl(leglength)),
           p_lag = lagl(the_score),
           p = the_score ,
           p_lead = leadl(the_score),
           smooth = if_else(rep(use_period_length, n()),
                            (lagl(w)*p_lag + w*p + leadl(w)*p_lead),
                            (leadl(the_score) + the_score + lagl(the_score))/3)) %>%
      ungroup() %>%
      arrange(n)

   return(smoothed$smooth)
}

#' Vanilla Scaling by Gabel & Huber
#' 
#' Computes scores based on the Vanilla method suggested by Gabel & Huber. 
#' A factor analysis identifies the dominant dimension in the data. 
#' Factor scores using the regression method are then considered as party positions on this dominant dimension. 
#'
#' @references Gabel, M. J., & Huber, J. D. (2000). Putting Parties in Their Place: Inferring Party Left-Right Ideological Positions from Party Manifestos Data. American Journal of Political Science, 44(1), 94-103.
#'
#' @param data A data.frame with cases to be scaled, variables named "per..."
#' @param vars variable names that should be used for the scaling (usually the variables per101,per102,...)
#' @param invert invert scores (to change the direction of the dimension to facilitate comparison with other indices) (default is FALSE)
#' @export
vanilla <- function(data,
                    vars = grep("per\\d{3}$", names(data), value=TRUE),
                    invert=FALSE) {
  fa.results <- psych::fa(data[,vars],1,scores="regression")
  vanilla.scores <- fa.results$scores[,1] 
  if (invert==TRUE) vanilla.scores <- vanilla.scores*-1
  return(vanilla.scores)
}
