formatDate = function(date){

  date = date |>
    as.character() |>
    stringr::str_replace(" ","T")

  #
  date
}
