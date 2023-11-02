#' List Variables
#'
#' Returns a tibble containing the variables available in the TOAR API
#'
#' @param force force the function to re-scrape the API docs. boolean
#'
#' @export

list_variables = function(force = FALSE){

  if(is.null(the$variablesList) | force){
    the$variablesList = paste0(get_endpoint_url("variables"),"?limit=None") |>
      query_toar_database()
  }
  #
  the$variablesList

}
