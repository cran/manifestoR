% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scaling_general.R
\name{mp_scale}
\alias{mp_scale}
\alias{document_scaling}
\alias{corpus_scaling}
\title{Scaling annotated manifesto documents}
\usage{
mp_scale(
  data,
  scalingfun = rile,
  scalingname = as.character(substitute(scalingfun)),
  recode_v5_to_v4 = (scalingname \%in\% c("rile", "logit_rile")),
  ...
)

document_scaling(
  scalingfun,
  returndf = FALSE,
  scalingname = "scaling",
  recode_v5_to_v4 = FALSE,
  ...
)

corpus_scaling(
  scalingfun,
  scalingname = "scaling",
  recode_v5_to_v4 = FALSE,
  ...
)
}
\arguments{
\item{data}{\code{ManifestoDocument} or \code{ManifestoCorpus} with coding
annotations or a data.frame with category percentages}

\item{scalingfun}{a scaling function, i.e. a function that takes a data.frame with
category percentages and returns scaled positions, e.g. \code{\link{scale_weighted}}.}

\item{scalingname}{the name of the scale which will be used as a column name when a data.frame is produced}

\item{recode_v5_to_v4}{recode handbook version 5 scheme to version 4 before scaling; this
parameter is only relevant if data is a ManifestoDocument or ManifestoCorpus, but not for 
data.frames with code percentages}

\item{...}{further arguments passed on to the scaling function \code{scalingfun},
or \code{\link{count_codes}}}

\item{returndf}{if this flag is TRUE, a data.frame with category percentage values,
scaling result and, if available party and date is returned by the returned function}
}
\description{
Since scaling functions such as \code{\link{scale_weighted}} only apply to
data.frames with code percentages, the function \code{mp_scale} makes them
applies them to a \code{ManifestoCorpus} or \code{ManifestoDocument}.
}
\seealso{
\code{\link{scale}}
}
