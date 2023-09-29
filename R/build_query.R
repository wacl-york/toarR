#' Build Query
#'
#' Constructs a query from the base url, endpoint and relevant arguments
#'
#' @param endpoint name of API endpoint. see \code{list_endpoints()}. string
#' @param ... Arguments for the given endpoint. Use \code{list_endpoint_args(endpoint)} for details. string
#' @param base_url the base url of the TOAR REST API. Defaults to the output of \code{get_base_url()}. string
#'
#' @export

build_query = function(endpoint, ..., base_url = get_base_url()){

  args = list(...)
  # try convert all arguments to strings

  if(length(args) > 0){ # if optional arguments have been supplied
    for(i in 1:length(args)){

      if("list" %in% class(args[[i]])){ # error with lists - request args as vectors instead
        stop(paste0("Error in ",names(args)[i],".\n",
                    "API arguments with multiple values should be supplied as a vector of strings"))
      }

      if("POSIXt" %in% class(args[[i]])){ # check for timestamps and format
        if("POSIXct" %in% class(args[[i]])){ # format POSIXct types
          args[[i]] = formatDate(args[[i]])
        }else{
          stop("Dates should be supplied as POSIXct not ",class(args[[i]])[1])
        }
      }

      args[[i]] = as.character(args[[i]])

      if(length(args[[i]]) > 1){
        args[[i]] = paste0(args[[i]], collapse = ",")
      }

    }
  }

  validEndpoints = c("stationmeta","timeseries","search","data")
  argumentEndpoints = c("stationmeta","timeseries","search")

  if(!endpoint %in% validEndpoints){
    stop(paste0("build_query() only supports ",
                paste0(validEndpoints,collapse = ", ")," endpoints."))
  }

  if(endpoint %in% argumentEndpoints){
    endpointArguments = list_endpoint_args(endpoint)
  }

  if(endpoint == "data"){
    endpoint = "data/timeseries"

    # extra args are only format or limit, so just hard code a search for them?

    url = paste0(
      get_endpoint_url(endpoint),
      args$id)

    if(!is.null(args$format) | !is.null(args$limit)){ # if either optional argument exists
      url = paste0(url, "/?")

      if(!is.null(args$format) & !is.null(args$limit)){ # if both optional arguments exist

        url = paste0(url, "format=",args$format,"&limit=",args$limit)

      }else{ # if only one is present

        if(!is.null(args$format)){

          if(!args$format %in% c("csv","json")){
            stop("format must either be 'csv' or 'json'")
          }

          url = paste0(url, "format=",args$format)
        }

        if(!is.null(args$limit)){
          url = paste0(url, "limit=",args$limit)
        }

      }
    }

  }else{

    url = get_endpoint_url(endpoint)

    if(length(args) > 0){
      apiArgs = list_endpoint_args(endpoint)

      funcArgNames = tibble::tibble(name = names(args))

      validArgs = dplyr::semi_join(apiArgs,funcArgNames, "name")
      invalidArgs = dplyr::anti_join(funcArgNames,validArgs, "name")

      if(nrow(invalidArgs) > 0){
        warning(paste0("Some arguments were not recognised, ignoring: ",paste0(invalidArgs$name,collapse = ", ")))
      }

      for(i in 1:length(args)){

        argExpress = paste0(names(args)[i],"=",args[[names(args)[i]]])
        if(i == 1)  {
          urlArgs = paste0("?",argExpress)
        }else{
          urlArgs = paste0(urlArgs,"&",argExpress)
        }

      }

      url = paste0(url, urlArgs)

    }

  }

  url

}
