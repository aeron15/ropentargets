#' A Reference Class to manage GET and POST calls to the Open Targets REST API
#'
#' @field BASE_URL A character string for the REST API URL
#'
RestUtils <- setRefClass("RestUtils",
  fields = list(
    BASE_URL = "character"
  ),
  methods = list(
    initialize = function() {
      "Initialize with no arguments. Sets the base URL field."
      BASE_URL <<- 'https://www.targetvalidation.org/api/latest/public'
    },
    processResponse = function(response) {
      "Used for GET responses.
      Returns a three-element list with the status code, the headers and the list returned by the REST call."
      responseHeaders <- httr::headers(response)
      statusCode <- httr::status_code(response)
      responseContent <- httr::content(response)
      return(list("statusCode" = statusCode,
                  "responseContent" = responseContent,
                  "responseHeaders" = responseHeaders))
    },
    getRESTResponse = function(subdomain) {
      "Run a GET request, process the response and return the resulting list.
       Process the response using the method 'processResponse()' and return a list.
       Printing the URL used deliberately as a sort of progress indicator and for de-bugging."
      url <- paste0(.self$BASE_URL, subdomain)
      # print(url)
      response <- httr::GET(url)
      processedResponse <- .self$processResponse(response)
      return(processedResponse)
    },
    getRESTPOSTResponse = function(subdomain, idList) {
      "Run a POST request. The identifiers are provided as a standard R list.
       Process the response using the method 'processResponse()' and return a list.
       Printing the URL used deliberately as a sort of progress indicator and for de-bugging."
      url <- paste0(.self$BASE_URL, subdomain)
      # print(url)
      body <- list(id = idList)
      response <- httr::POST(url, body = body, encode = "json")
      processedResponse <- .self$processResponse(response)
      return(processedResponse)
    },
    checkRESTResponse = function(response) {
      "Check the return code and throw an error if it is not 200."
      if(response$statusCode != 200) {
        stop(sprintf("Status code error '%s' returned!", response$statusCode))
      }
    },
    getRESTAPIVersion = function() {
      "Return the version of the API being used."
      subdomain <- '/utils/version'
      response <- .self$getRESTResponse(.self$BASE_URL, subdomain)
      return(response$responseContent)
    }
  )
)
