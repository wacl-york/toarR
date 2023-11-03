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
      dplyr::tibble() |>
      dplyr::rename(flag = V1,
                    value = V2,
                    name = V3)

    the$controlledVocabulary[[controlledVocabulary]] = response

    #
    return(response)
  }else{
    return(the$controlledVocabulary[[controlledVocabulary]])
  }
}
