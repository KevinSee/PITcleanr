% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queryInterrogationMeta.R
\name{queryInterrogationMeta}
\alias{queryInterrogationMeta}
\title{PTAGIS Interagation sites metadata}
\source{
\url{http://www.ptagis.org}
}
\usage{
queryInterrogationMeta(site_code = NULL)
}
\arguments{
\item{site_code}{PTAGIS site code. Default is \code{NULL} which will query all sites}
}
\description{
Query and download metadata for PTAGIS Interrogation sites
}
\examples{
queryInterrogationMeta(site_code = 'ZEN')
}
\author{
Kevin See
}
