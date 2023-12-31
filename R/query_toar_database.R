#' Query TOAR database
#'
#' Submits the Query to the TOAR base and handles the response
#'
#' @param queryUrl a query url generated by \code{build_query}. string
#'
#' @export

query_toar_database = function(queryUrl){

  endpoint = stringr::str_remove(queryUrl,get_base_url()) |>
    stringr::word(1, sep = "/")

  response = httr::GET(queryUrl)

  if(httr::status_code(response) == 200){ #

    if(endpoint == "data"){ # data endpoint returns a header with metadata, and the data itself so have to handle differently

      contentData = httr::content(response)$data |>
        data.table::rbindlist() |>
        tibble::tibble()

    }else{

      contentData = response$content |>
        rawToChar() |>
        jsonlite::fromJSON() |>
        tibble::tibble() |>
        tidyr::unnest(tidyselect::any_of(c("codes","globalmeta","coordinates","additional_metadata")))

    }

    return(contentData)

  }else{
    response

  }


}
