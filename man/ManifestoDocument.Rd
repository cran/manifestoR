% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corpus.R
\docType{class}
\name{ManifestoDocument}
\alias{ManifestoDocument}
\title{Manifesto Document}
\usage{
ManifestoDocument(
  content = data.frame(),
  id = character(0),
  meta = ManifestoDocumentMeta()
)
}
\arguments{
\item{content}{data.frame of text and codes for the ManifestoDocument to be constructed.
There can be multiple columns of codes, but by default the accessor method \code{\link{codes}}
searches for the column named "cmp_code".}

\item{id}{an id to identify the Document}

\item{meta}{an object of class \code{\link{ManifestoDocumentMeta}} containing the metadata for this document}
}
\description{
A ManifestoDocument represents a document from the Manifesto Corpus and contains
text, coding and meta information.
ManifestoDocument objects need not be constructed manually but are the content
of the \code{\link{ManifestoCorpus}} objects downloaded from the Manifesto
Corpus Database API via \code{\link{mp_corpus}}. 

\code{ManifestoDocument}s subclass the \code{\link[tm]{TextDocument}} class
from the package \code{tm}. Hence they can be and usually are collected in a \code{tm}
\code{\link[tm]{Corpus}} to interface easily with text mining and other linguistic
analysis functions. \code{manifestoR} uses the subclass
\code{\link{ManifestoCorpus}} of \code{tm}s \code{\link[tm]{Corpus}}, but
\code{ManifestoDocument}s can be stored in any kind of \code{Corpus}.

As in \code{tm} any ManifestoDocument has metadata which can be accessed and
modified via the \code{meta} function, as well as content, accessible via
\code{content}. Additionally, via \code{\link{codes}()}, the coding of the
(quasi-)sentence ccording to the CMP category scheme can be accessed 
(and modified). The CMP category scheme can be found online at
\url{https://manifesto-project.wzb.eu/coding_schemes/mp_v4} (version 4)
or \url{https://manifesto-project.wzb.eu/coding_schemes/mp_v5} (version 5).
}
\details{
Internally, a ManifestoDocument is a \code{data.frame} with a row for
every quasi-sentence and the columns \code{text} and \code{code}.
}
\examples{
\dontrun{
corpus <- mp_corpus(subset(mp_maindataset(), countryname == "New Zealand"))
doc <- corpus[[1]]
print(doc)
}
}
