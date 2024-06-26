mp_globalenv <- new.env()
assign("mp_cache", new.env(parent = emptyenv()), envir = mp_globalenv)
mp_cache <- function() {
  get("mp_cache", envir = mp_globalenv)
}

kcachelocation <- "cachelocation"
kapikey <- "apikey"
kurlapiroot <- "urlapiroot"
kurloriginalsroot <- "urloriginalsroot"

kversions <- "versions"
kmetaversion <- "metaversionid"
kmanifestorversion <- "manifestoR-version"
kdefaultcachename <- "manifestofiles"
ktextname <- "manifesto_doc"
kmetadata <- "docmetadata"
koriginals <- "originals"
kdatasetname <- "MPDataset_"
kcodebookname <- "MPCodebook_"
kpartiesname <- "MPParties_"

kmtype.meta <- "meta"
kmtype.text <- "text"
kmtype.original <- "original"
kmtype.main <- "main"
kmtype.versions <- "versions"
kmtype.metaversions <- "metaversions"
kmtype.corecitation <- "corecitation"
kmtype.corpuscitation <- "corpuscitation"
kmtype.codebook <- "codebook"
kmtype.parties <- "parties"

kcitemessage <- paste0("When publishing work using the Manifesto Corpus, please ",
                       "make sure to cite it correctly and to give the identification ",
                       "number of the corpus version used for your analysis.")

assign(kmanifestorversion, packageVersion("manifestoR"), envir = mp_cache())

.onAttach = function(libname, pkgname) {
  packageStartupMessage(paste(kcitemessage,
                "You can print citation and version information with the function mp_cite().",
                paste0(c("Note that some of the scaling/analysis algorithms provided with this package were conceptually",
                         "developed by authors referenced in the respective function documentation. Please also",
                         "reference them when using these algorithms."), collapse = " "),
                sep = "\n\n"))
}
