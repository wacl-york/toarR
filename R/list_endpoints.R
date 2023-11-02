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

  name = NULL # bind unbound variable used in NSE

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

  name = description = NULL # bind unbound variables used in NSE

  if(is.null(the[[envEndpointName]]) | force){

    sess = rvest::session(get_base_url())

    divs = rvest::html_elements(sess, "div")

    if(endpoint %in% argumentEndpoints){

      if(endpoint == "search"){

        stn = list_endpoint_args("stationmeta")
        ts = list_endpoint_args("timeseries")

        endpointArgs = dplyr::bind_rows(stn,ts) |>
          dplyr::distinct() |>
          dplyr::filter(name != "id") |>
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

#' List Controlled Controlled Vocabulary
#'
#' Provides the data table associated with the defined controlled Vocabulary.
#' A description of the controlled vocabulary can be generated with \code{list_endpoint_args("controlled_vocabulary")}
#' The response will be stored in the internal environment, which can be overridden with \code{force = TRUE}.
#'
#' @param controlledVocabulary the name of the controlled vocabulary required.
#' Note these are formatted with begining with each word starting with a capital and
#' separated with a space. Arguments will be formatted as such using \code{stringr::str_to_title()}.
#' They will then be formatted for the url via \code{utils:URLencode()}.
#' If no controlled vocabulary is specified, a vector of valid options is instead returned. string.
#'
#' @param force force the function to re-scrape the API docs. boolean.
#'
#' @export
#'

list_controlled_vocabulary = function(controlledVocabulary = NULL, force = FALSE){

  # This is precarious.
  validControlledVocabulary = list_endpoint_args("controlled_vocabulary")
  CVStart = which(stringr::str_detect(validControlledVocabulary$description, "Valid names"))+1
  CVEnd = which(stringr::str_detect(validControlledVocabulary$description, "Example"))-1
  validControlledVocabulary = validControlledVocabulary$description[CVStart:CVEnd]

  controlled_vocabulary = stringr::str_to_title(controlledVocabulary)

  if(is.null(controlledVocabulary)){
    message("No controlled vocabulary specified. Returning vector of valid options")
    return(validControlledVocabulary)
  }

  if(!controlledVocabulary %in% validControlledVocabulary){
    stop(paste0("controlled_vocabulary must be one of: ",
                paste(validControlledVocabulary, collapse = ", ")))
  }

  if(is.null(the$controlledVocabulary[[controlledVocabulary]]) | force){
    response = paste0(get_endpoint_url("controlled_vocabulary"),controlledVocabulary) |>
      utils::URLencode() |>
      query_toar_database() |>
      purrr::pluck(1) |>
      as.data.frame() |>
      dplyr::tibble()

    the$controlledVocabulary[[controlledVocabulary]] = response

    #
    return(response)
  }else{
    return(the$controlledVocabulary[[controlledVocabulary]])
  }
}

