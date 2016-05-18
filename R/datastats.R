#' A Reference Class to return information on data types, data sources and counts from the REST utils/stats end-point.
#'
#' @field utilsObj An instance of class RestUtils
#' @field allStatsAsList A list that is set in the constructor that contains all the information from the utils/stats end-point.
#' @field dataTypeNames A vector containing all the names of all the data types currently in the Open Targets platform.
#' @field dataSourceNamesForDataTypesList A list mapping data source names to their data type names.
#' @export DataStats
DataStats <- setRefClass(
  "DataStats",
  fields = list(
    utilsObj = "ANY",
    allStatsAsList = "list",
    dataTypeNames = "vector",
    dataSourceNamesForDataTypesList = "list"
  ),
  methods = list(
    initialize = function() {
      "Initialize with no arguments.
      Calls self methods to set field values."
      utilsObj <<- RestUtils$new()
      allStatsAsList <<-
        .self$setAllStatsAsList()
      dataTypeNames <<-
        .self$setDataTypeNames()
      dataSourceNamesForDataTypesList <<-
        .self$setDataSourceNamesForDataTypesList()
    },
    setAllStatsAsList = function() {
      "Called in initialize() to set field allStatsAsList."
      subdomain <- '/utils/stats'
      RESTResponse <-
        utilsObj$getRESTResponse(subdomain)
      utilsObj$checkRESTResponse(RESTResponse)
      return(RESTResponse$responseContent)
    },
    getAllStatsAsList = function() {
      "Return list field value set in in initialize() for all stats for the current version of Open Targets."
      return(allStatsAsList)
    },
    getDataTypeNames = function() {
      "Return vector field value set in in initialize() for data type names for the current version of Open Targets."
      return(dataTypeNames)
    },
    setDataTypeNames = function() {
      "Return data type names as vector.
      Called in initialize() to set field dataSourceNames.
      Uses the allStatsAsList field set in initialize()."
      evidenceStringStats <-
        allStatsAsList$evidencestrings$datatypes
      return(names(allStatsAsList$evidencestrings$datatypes))
    },
    getDataTypeListByName = function(dataTypeName) {
      "Return a list with statistics for a given data type name."
      return(allStatsAsList$evidencestrings$datatypes[[dataTypeName]])
    },
    # Used to get either counts for evidence strings or associations.
    # Expects a string argument with value of "evidencestrings" or "assocations".
    # Returns a list mapping data type names to counts for the given argument.
    getDataTypeCounts = function(countTypeName) {
      dataTypeCountList <- list()
      for (dataTypeName in dataTypeNames) {
        typeCount <-
          allStatsAsList[[countTypeName]]$datatypes[[dataTypeName]]$total
        dataTypeCountList[[dataTypeName]] <-
          typeCount
      }
      return(dataTypeCountList)
    },
    # Return a list mapping data type names to their evidence string counts.
    getDataTypeEvStrCountList = function() {
      dataTypeEvStrCountList <- .self$getDataTypeCounts('evidencestrings')
      return(dataTypeEvStrCountList)
    },
    getDataTypeAssocCountList = function() {
      dataTypeAssocCountList <- .self$getDataTypeCounts('associations')
      return(dataTypeAssocCountList)
    },
    # Return a list of vectors mapping data type names to their associated data sources.
    # Set return value to a field name by calling in "initialize()".
    setDataSourceNamesForDataTypesList = function() {
      dataSourceNamesForDataTypes = list()
      for (dataTypeName in dataTypeNames) {
        dataSourceNamesForDataType <-
          names(allStatsAsList$evidencestrings$datatypes[[dataTypeName]]$datasources)
        dataSourceNamesForDataTypes[[dataTypeName]] <-
          dataSourceNamesForDataType
      }
      return(dataSourceNamesForDataTypes)
    },
    # Return an instance field list where data type names are mapped to
    # vectors of associated data source names.
    getDataSourceNamesForDataTypesList = function() {
      return(dataSourceNamesForDataTypesList)
    },
    getDataSourceCountsList = function(countTypeName) {
      "Return a list mapping data source names to either their association or
      evidence string counts. The count returned depends on the argument value
      which is expected to be either 'associations' or 'evidencestrings'.
      Used internally by methods that specify the type of count to return"
      dataSourceCountsList <- list()
      for (dataTypeName in dataTypeNames) {
        for (dataSourceName in dataSourceNamesForDataTypesList[[dataTypeName]]) {
          dataSourceCount <-
            allStatsAsList[[countTypeName]]$datatypes[[dataTypeName]]$datasources[[dataSourceName]]$total
          dataSourceCountsList[[dataSourceName]] <-
            dataSourceCount
        }
      }
      return(dataSourceCountsList)
    },
    getDataSourceEvStrCountsList = function() {
      "Return a list mapping data source names to their associated evidence string counts."
      dataSourceEvStrCountList <-
        .self$getDataSourceCountsList("evidencestrings")
      return(dataSourceEvStrCountList)
    },
    getDataSourceAssocCountsList = function() {
      "Return a list mapping data source names to their associated association counts."
      dataSourceAssocCountList <-
        .self$getDataSourceCountsList("associations")
      return(dataSourceAssocCountList)
    }
  )
)
