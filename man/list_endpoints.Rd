% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_endpoints.R
\name{list_endpoints}
\alias{list_endpoints}
\title{List Endpoints}
\usage{
list_endpoints(force = FALSE)
}
\arguments{
\item{force}{force the function to re-scrape the API docs. boolean}
}
\description{
Returns a tibble containing names and brief descriptions of the TOAR REST API
endpoints. Built from the documentation at toar-data.fz-juelich.de/api/v2/.
}
\details{
On first run the API docs are scraped and stored in an internal environment.
This can be overridden by setting \code{force = TRUE}.

Details on the endpoint arguments can be obtained via \code{list_endpoint_args()}

The online version should be checked for up to date information
}
