# Environment for storing the various tables that are scraped from the API docs.
# On first run of list_endpoints() or list_endpoint_args() (or when force = TRUE)
# the API docs are scraped for this information. For all other subsequent calls
# in this session the cached value is used. This substantially speeds up build_query()
# as it uses the endpoint argument tables to check for valid arguments.

the = new.env(parent = emptyenv())

# list_endpoints(), list_endpoint_args() and list_controlled_vocabulary()
the$endpointList = NULL
the$endpointStationmetaArgs = NULL
the$endpointTimeseriesArgs = NULL
the$endpointSearchArgs = NULL
the$endpointDataDesc = NULL
the$endpointVariablesDesc = NULL
the$endpointControlledVocabularyDesc = NULL
the$controlledVocabulary = list()

# list_variables()
the$variablesList = NULL
