#' List Endpoint Arguments
#'
#' Returns a tibble containing the given endpoints arguments and their descriptions.
#'
#' @param endpoint name of the API endpoint to return descriptions for. string
#'
#' @export
#'

list_endpoint_args = function(endpoint){

  if(endpoint == "controlled_vocabulary"){
    endpoint = "controlled-vocabulary"
  }

  avalibleEndpoints = c("stationmeta","timeseries","search")
  descriptionEndpoints = c("data","variables","controlled-vocabulary")

  if(!endpoint %in% c(avalibleEndpoints,descriptionEndpoints)){
    stop(paste0("Arguments/description for ",endpoint," could not be obtained.\n",
               "Argument lists are avaliable for ",
               paste0(avalibleEndpoints, collapse = ", "),".\n",
               "Descriptions are avalible for ",
               paste0(descriptionEndpoints,collapse = ", "),".\n",
               "Check the API docs at ",get_base_url(),"for more details."))
  }

  sess = rvest::session(get_base_url())

  divs = rvest::html_elements(sess, "div")

  if(endpoint %in% avalibleEndpoints){

    if(endpoint == "search"){

      stn = list_endpoint_args("stationmeta")
      ts = list_endpoint_args("timeseries")

      endpointArgs = bind_rows(stn,ts) |>
        distinct() |>
        filter(Name != "id") |>
        janitor::clean_names()
      # id does not apply to the search endpoint as it is a cross reference of # stationmeta and timeseries

    }else{
      endpointArgs = divs[which(rvest::html_attr(divs,"id") == endpoint)] |>
        rvest::html_element("table") |>
        rvest::html_table()|>
        purrr::pluck(1) |>
        janitor::clean_names()
    }

    return(endpointArgs)
  }

  if(endpoint %in% descriptionEndpoints){
    endpointDesc = tibble(description = divs[which(rvest::html_attr(divs,"id") == endpoint)] |>
                            rvest::html_text2() |>
                            stringr::str_split("\n") |>
                            purrr::pluck(1)) |>
      filter(description != "")

    return(endpointDesc)

  }

}

