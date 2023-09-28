#' Get Endpoint URL
#'
#' Returns the URL for a named endpoint of the TOAR REST API
#'
#' @param endpoint name of endpoint. use \code{list_endpoints()} for details. string
#'
#' @export
#'

get_endpoint_url = function(endpoint){

  if(stringr::str_sub(endpoint,-1,-1) != "/"){
    endpoint = paste0(endpoint,"/")
  }

  paste0(get_base_url(),endpoint)

}

