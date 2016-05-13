#' A Reference Class to implement GET and POST methods to return individual evidence entities as JSON or as R lists.
#'
#' @field utilsObj An instance of class RestUtils
#'
Evidence<-setRefClass("Evidence",
  fields = list(
    utilsObj = "ANY"
  ),
  methods = list(
    initialize = function() {
      "Initialize with no arguments."
      utilsObj <<- RestUtils$new()
    },
    getSingleEvidenceAsList = function(evidenceID) {
      "Use GET to return a list representing the evidence string for a given evidence string ID - a 32 character hashed value."
      subdomain <- sprintf('/evidence?id=%s', evidenceID)
      RESTResponse <- utilsObj$getRESTResponse(subdomain)
      utilsObj$checkRESTResponse(RESTResponse)
      evidenceList <- RESTResponse$responseContent
      return(evidenceList)
    },
    getSingleEvidenceAsJson = function(evidenceID) {
      "Convert evidence string for a given ID (32 character hashed value)) to JSON and return the JSON."
      evidenceList <- .self$getSingleEvidenceAsList(evidenceID)
      return(toJSON(evidenceList))
    },
    getMultipleEvidencesAsList = function(evidenceIDList) {
      "Given a list of evidence string IDs (32 character hashed values),
      return a list of lists where the inner lists are evidence strings."
      subdomain <- '/evidence'
      RESTPOSTResponse <- utilsObj$getRESTPOSTResponse(subdomain, evidenceIDList)
      utilsObj$checkRESTResponse(RESTPOSTResponse)
      multipleEvidenceList <- RESTPOSTResponse$responseContent
      return(multipleEvidenceList)
    },
    writeEvStrAsJsonToFile = function(evidenceIDList, fileFullPath) {
      "Write JSONs returned for a given list of one or more evidence string IDs to a given file, one JSON per line."
      multipleEvidencesAsList <- .self$getMultipleEvidencesAsList(evidenceIDList)
      # Extract the list of evidence string lists with name "data".
      evidenceStringList <- multipleEvidencesAsList[['data']]
      # Ignore lapply return value.
      for (evidenceString in evidenceStringList) {
        cat(toJSON(evidenceString),file = fileFullPath, sep="\n",append=TRUE)
      }
      return(NULL)
    },
    getMultipleEvidencesAsJson = function(evidenceIDList) {
      "Convert list returned by method 'getMultipleEvidencesAsList()' to JSON and return the JSON."
      multipleEvidencesAsJson <- toJSON(.self$getMultipleEvidencesAsList(evidenceIDList))
      return(multipleEvidencesAsJson)
    }
  )
)
