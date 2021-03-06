% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manifesto.R
\name{formatids}
\alias{formatids}
\title{Format ids for web API queries}
\usage{
formatids(ids)
}
\arguments{
\item{ids}{ids data.frame, information used: party, date, edate}
}
\description{
Formats a data.frame of ids such that it can be used for querying
the Manifesto Project Database. That is, it must have non-NA-fields
party and date.
}
