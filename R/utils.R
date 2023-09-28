formatDate = function(date){

  arg = deparse(substitute(date))

  date = lubridate::ymd_hms(date)

  if(is.na(date)){
    stop(paste0(arg," must be of format ymd_hms"))
  }

  date = date |>
    as.character() |>
    stringr::str_replace(" ","T")

  if(nchar(date) == 10){
    date = paste0(date,"T00:00:00")
  }

  date = paste0(date,"+00:00")

  #
  date
}
