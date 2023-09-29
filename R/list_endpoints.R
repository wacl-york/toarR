# Environment for storing the various tables that are scraped from the API docs.
# On first run of list_endpoints() or list_endpoint_args() (or when force = TRUE)
# the API docs are scraped for this information. For all other subsequent calls
# in this session the cached value is used. This substantially speeds up build_query()
# as it uses the endpoint argument tables to check for valid arguments.

the = new.env(parent = emptyenv())

the$endpointList = NULL
the$endpointStationmetaArgs = NULL
the$endpointTimeseriesArgs = NULL
the$endpointSearchArgs = NULL
the$endpointDataDesc = NULL
the$endpointVariablesDesc = NULL
the$endpointControlledVocabularyDesc = NULL

#' List Endpoints
#'
#' Returns a tibble containing names and brief descriptions of the TOAR REST API
#' endpoints. Built from the documentation at toar-data.fz-juelich.de/api/v2/.
#'
#' On first run the API docs are scraped and stored in an internal environment.
#' This can be overridden by setting \code{force = TRUE}.
#'
#' Details on the endpoint arguments can be obtained via \code{list_endpoint_args()}
#'
#' The online version should be checked for up to date information
#'
#' @param force force the function to re-scrape the API docs. boolean
#'
#' @export

list_endpoints = function(force = FALSE){

  if(is.null(the$endpointList) | force){
    sess = rvest::session(get_base_url())

    divs = rvest::html_elements(sess,"div")

    services = divs[which(rvest::html_attr(divs,"id") == "services")]

    endpoints = tibble::tibble(name = services |>
                                 rvest::html_element("ul") |>
                                 rvest::html_text() |>
                                 stringr::str_split("\n") |>
                                 unlist()) |>
      dplyr::filter(name != "") |>
      tidyr::separate(name, c("name", "description"), ": ")

    #
    the$endpointList = endpoints
    endpoints

  }else{

    the$endpointList

  }
}

#' List Endpoint Arguments
#'
#' Returns a tibble containing the given endpoints arguments and their descriptions.
#'
#' On first run the API docs are scraped and stored in an internal environment.
#' This can be overridden by setting \code{force = TRUE}.
#'
#' @param endpoint name of the API endpoint to return descriptions for. string
#' @param force force the function to re-scrape the API docs. boolean
#'
#' @export
#'

list_endpoint_args = function(endpoint, force = FALSE){

  if(endpoint == "controlled_vocabulary"){
    endpoint = "controlled-vocabulary"
  }

  argumentEndpoints = c("stationmeta","timeseries","search")
  descriptionEndpoints = c("data","variables","controlled-vocabulary")

  if(!endpoint %in% c(argumentEndpoints,descriptionEndpoints)){
    stop(paste0("Arguments/description for ",endpoint," could not be obtained.\n",
                "Argument lists are avaliable for ",
                paste0(argumentEndpoints, collapse = ", "),".\n",
                "Descriptions are avalible for ",
                paste0(descriptionEndpoints,collapse = ", "),".\n",
                "Check the API docs at ",get_base_url(),"for more details."))
  }

  envEndpointName = switch(endpoint,
                           stationmeta = "endpointStationmetaArgs",
                           timeseries = "endpointTimeseriesArgs",
                           search = "endpointSearchArgs",
                           data = "endpointDataDesc",
                           variables = "endpointVariablesDesc",
                           `controlled-vocabulary` = "endpointControlledVocabularyDesc"



  )

  if(is.null(the[[envEndpointName]]) | force){

    sess = rvest::session(get_base_url())

    divs = rvest::html_elements(sess, "div")

    if(endpoint %in% argumentEndpoints){

      if(endpoint == "search"){

        stn = list_endpoint_args("stationmeta")
        ts = list_endpoint_args("timeseries")

        endpointArgs = bind_rows(stn,ts) |>
          dplyr::distinct() |>
          dplyr::filter(Name != "id") |>
          janitor::clean_names()
        # id does not apply to the search endpoint as it is a cross reference of # stationmeta and timeseries

      }else{
        endpointArgs = divs[which(rvest::html_attr(divs,"id") == endpoint)] |>
          rvest::html_element("table") |>
          rvest::html_table()|>
          purrr::pluck(1) |>
          janitor::clean_names()
      }
      the[[envEndpointName]] = endpointArgs
      return(endpointArgs)
    }

    if(endpoint %in% descriptionEndpoints){
      endpointDesc = tibble::tibble(description = divs[which(rvest::html_attr(divs,"id") == endpoint)] |>
                                      rvest::html_text2() |>
                                      stringr::str_split("\n") |>
                                      purrr::pluck(1)) |>
        dplyr::filter(description != "")

      the[[envEndpointName]] = endpointDesc
      return(endpointDesc)

    }

  }else{

    return(the[[envEndpointName]])

  }

}
