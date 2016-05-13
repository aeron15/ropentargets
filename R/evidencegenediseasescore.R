#' A Reference Class to produce a data frame with all evidence for a given gene, disease and score cut-off.
#'
#' @field utilsObj An instance of class RestUtils
#' @field ensemblGeneID The ensembl gene identifer for the gene of interest (ENSG..).
#' @field efoID An EFO disease name identifier (EFO_...) for the disease of interest.
#'
EvidenceGeneDiseaseScore <- setRefClass("EvidenceGeneDiseaseScore",
  fields = list(
    ensemblGeneID = "character",
    efoID = "character",
    utilsObj = "ANY"
  ),
  methods = list(
    initialize = function(ensemblGeneID, efoID) {
      "Initialize with an ensembl gene ID, an EFO ID."
      efoID <<- efoID
      ensemblGeneID <<- ensemblGeneID
      utilsObj <<- RestUtils$new()
    },
    getDataFramePartial = function(directFlag, startFrom, chunkSize, cutoffScore = 0) {
      "Used internally to return a sub-set of the evidence for the disease and gene field values. Used internally."
      subdomain <- sprintf('/evidence/filter?target=%s&disease=%s&datastructure=simple&fields=id&fields=disease.id&fields=scores.association_score&fields=sourceID&fields=target.id&fields=type&direct=%s&scorevalue_min=%0.2f&size=%d&from=%d&format=tab',
                           ensemblGeneID, efoID, directFlag, cutoffScore, chunkSize, startFrom)
      RESTResponse <- utilsObj$getRESTResponse(subdomain)
      utilsObj$checkRESTResponse(RESTResponse)
      targetDiseaseList <- RESTResponse$responseContent
      dataFrame <- do.call(rbind, lapply(targetDiseaseList$data, data.frame, stringsAsFactors=FALSE))
      if(is.null(dataFrame)) {
        return(NULL)
      }
      return(dataFrame)
    },
    getAllEvidenceAsDataFrame = function(directFlag = "false", cutoffScore = 0) {
      "Call method 'getDataFramePartial()' repeatedly to generate a complete data frame and return this."
      startFrom <- 0
      chunkSize <- 500
      sleepSeconds <- 10
      allEvidenceDataFrame <- data.frame()
      repeat {
        dataFrame <- .self$getDataFramePartial(directFlag, startFrom, chunkSize, cutoffScore)
        if(is.null(dataFrame)) {
          break
        }
        allEvidenceDataFrame <- rbind(allEvidenceDataFrame, dataFrame)
        startFrom = startFrom + chunkSize
        Sys.sleep(sleepSeconds)
      }
      return(allEvidenceDataFrame)
    }
  )
)
