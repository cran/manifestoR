% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{mp_interpolate}
\alias{mp_interpolate}
\title{Interpolate values within election periods}
\usage{
mp_interpolate(
  df,
  vars = "(^rile$)|(^per((\\\\d{3}(_\\\\d)?)|\\\\d{4})$)",
  by = "year",
  approx = zoo::na.approx,
  ...
)
}
\arguments{
\item{df}{a data.frame with observations to be interpolated}

\item{vars}{a regular expression matching the names of the variables to be interpolated}

\item{by}{increment of the interpolation sequence, passed to \code{\link{seq.Date}}}

\item{approx}{Interpolation function, defaults to zoo's \code{\link{na.approx}}}

\item{...}{Further arguments, passed on to approx}
}
\description{
As the Manifesto Project's variables are collected election-wise, values
for the time/years in between elections are not naturally available.
\code{mp_interpolate} allows to approximate them by several methods from
the abjacent observations.
}
\examples{
\dontrun{
mp_interpolate(mp_maindataset(), method = "constant")
mp_interpolate(mp_maindataset(), approx = na.spline, maxgap = 3)
}
}
