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
  for(i in 1:length(args)){

    if("list" %in% class(args[[i]])){ # error with lists - requrest args as vectors instead
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

# -------------------------------------------------------------------------
 # return(args) # using to test above only
# -------------------------------------------------------------------------

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
      args$id
    )

    # code to do a data request

  }else{

    # code to do other requests
  }

  url

}
