# toarR

An R package for interfacing the with the [TOAR-II Database](https://toar-data.fz-juelich.de/).

Note: this is not an official TOAR project. It comes with absolutely no guarantees.

## Installation

``` r
devtools::install_github("wacl-york/toarR")
```

## Usage

`toarR` helps build the queries for the TOAR-II database, based upon the v2 API documented [here](https://toar-data.fz-juelich.de/api/v2/). It allows the users to access endpoint documentation within R and issue queries to the API. Currently it supports the basic API interface, and does not work with the `analysis` endpoint.

### Examples

#### List the available endpoints:

``` r
list_endpoints()
```

#### Explore the arguments or documentation for an endpoint:

-   When an endpoint has clearly defined arguments (i.e. appears as table on the API documentation), a tibble is created with this information:

``` r
list_endpoint_args("stationmeta")
```

-   When this is not the case, the description from the API docs is returned as a tibble of the lines:

``` r
list_endpoint_args("variables")
```

#### Controlled Vocabulary

Some of the fields that the TOAR database can be filtered by have more detailed requirements. These are described as controlled vocabulary and the information on these can be accessed as follows:

``` r
list_controlled_vocabulary() # returns a vector of the avalible controlled variables

list_controlled_vocabulary("Station Type") # returns a tibble of the requested vocabulary
```

Note: these requests are stored in a local environment after the first query, so there is less overhead if they are being used in code after the initial scraping.

### Making a Query

Using these functions, one may discover the required arguments, and build a query as follows:

``` r
library(toarR)
library(purrr)
library(stringr)

# discover the arguments required to search
# this endpoint will return the timeseries that match by station and variable criteria
list_endpoint_args("search")

# discover the ids for the variables of interest
variable = list_variables() |> 
  filter(str_detect(name, "o3"))

# discover the country code of interest:
countryCode = list_controlled_vocabulary("Country Code") |> 
  filter(str_detect(V3, "Britain"))

# discover how to filter for sampling frequency
sampFreq = list_controlled_vocabulary("Sampling Frequency")

timeseries = build_query("search",
                         variable_id = 5, 
                         country = "GB",
                         sampling_frequency = "Hourly",
                         limit = "None"
                         ) |> 
  query_toar_database() |> 
  mutate(station_name = pluck(station, "name"))

# Filter for what we are interested in, in this case a specific site by its name
timeseriesOfInterest = timeseries |> 
  filter(str_detect(station_name, "Marylebone"))

# Download the actual data
toarDat = build_query("data",
                      id = timeseriesOfInterest$id) |> 
  query_toar_database()
```
