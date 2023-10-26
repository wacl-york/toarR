formatDate = function(date){

  arg = deparse(substitute(date))

  date = date |>
    as.character() |>
    stringr::str_replace(" ","T")

  # there is 100% a better way to do this
  # but for now this works-ish
  if(nchar(date) == 10){
    date = paste0(date,"T00:00:00")
  }

  if(nchar(date) == 13){
    date = paste0(date,":00:00")
  }

  if(nchar(date) == 16){
    date = paste0(date,":00")
  }

  date = paste0(date,"+00:00")

  #
  date
}
