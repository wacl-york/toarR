#' List Endpoints
#'
#' Returns a tibble containing names and brief descriptions of the TOAR REST API
#' endpoints. Built from the documentation at toar-data.fz-juelich.de/api/v2/.
#'
#' Details on the endpoint arguments can be obtained via \code{list_endpoint_args()}
#'
#' The online version should be checked for up to date information
#'
#' @export

list_endpoints = function(){

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
  endpoints
}
