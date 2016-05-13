#' A Reference Class to implement GET and POST methods to return individual evidence entities as JSON or as R lists.
#'
#' @field utilsObj An instance of class RestUtils
#' @field dataTypeName The name of a data type in the Open Targets platform.
#'
EvidenceForDataType<-setRefClass("EvidenceForDataType",
  fields = list(
    dataTypeName = "character",
    utilsObj = "ANY"
  ),
  # Provide a data type, e.g. "genetic_association", the other two
  # arguments are the same as their namesakes described for other classes above.
  methods = list(
    initialize = function(dataTypeName) {
      "Initialize with a data type name, e.g. 'genetic_association'."
      dataTypeName <<- dataTypeName
      utilsObj <<- RestUtils$new()
    },
    getEvStrChunk = function(chunkSize, startFrom) {
      "Return a list of evidence objects of  given chunk size and a given start point."
      subdomain <- sprintf('/evidence/filter?datatype=%s&direct=true&size=%d&from=%d', dataTypeName, chunkSize, startFrom)
      RESTResponse <- utilsObj$getRESTResponse(subdomain)
      utilsObj$checkRESTResponse(RESTResponse)
      evStrList <- RESTResponse$responseContent
      return(evStrList)
    },
    getEvStrAll =  function() {
      "Return a list of lists of all the evidence data for the given data type. Warning: This can be very large."
      chunkSize <- 500
      sleepTimeSec <- 10
      startFrom <- 0
      chunkNumber <- 0
      evStrLists <- list()
      repeat {
        evStrList <- .self$getEvStrChunk(chunkSize, startFrom)
        startFrom <- startFrom + chunkSize
        Sys.sleep(sleepTimeSec)
        if(evStrList$size == 0) {
          break
        }
        chunkName <- paste0('Chunk_', chunkNumber)
        evStrLists[[chunkName]] <- evStrList
        chunkNumber <- chunkNumber + 1
      }
      return(evStrLists)
    }
  )
)
