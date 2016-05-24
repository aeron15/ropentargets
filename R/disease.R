#' A Reference Class to return information on a given disease.
#'
#' @field utilsObj An instance of class RestUtils
#' @field diseaseName The disease name as string.
#' @field diseaseNameList A list containing platform information for a disease.
#' @export Disease
Disease <- setRefClass("Disease",
  fields = list(
    diseaseName = "character",
    utilsObj = "ANY",
    diseaseList = "list"
  ),
  methods = list(
    initialize = function(diseaseName) {
      "Provide a disease name as the single initialization argument.
       The underlying search engine can deal with approximate spellings, synonyms and
       text case differences."
      diseaseName <<- diseaseName
      utilsObj <<- RestUtils$new()
      .self$setDiseaseList()
    },
    setDiseaseList = function() {
      "Set the field 'diseaseList'. Called in initialize()."
      subdomain <- sprintf('/search?size=10&from=0&q=%s', diseaseName)
      RESTResponse <- utilsObj$getRESTResponse(subdomain)
      utilsObj$checkRESTResponse(RESTResponse)
      diseaseList <<-  RESTResponse$responseContent
    },
    getDiseaseList = function() {
      "Return 'diseaseList' field."
      return(diseaseList)
    },
    getFirstDiseaseEFOID = function() {
      "Return the EFO for what the search engine regards as the 'best match'.
      Check the value returned by the method 'getFirstDiseaseName()'
      defined below to ensure that the description matches what is expected."
      return(diseaseList$data[[1]]$id)
    },
    getFirstDiseaseName = function() {
      "Return the name for the first disease, assumed to be est match, returned by the search engine."
      return(diseaseList$data[[1]]$data$name)
    },
    getDiseaseIDNameMap = function() {
      "Return a list mapping EFO IDs to disease names for all diseases returned by the search
       engine for the disease term set in 'initialize()'."
      diseaseIDNameMap <- list()
      for (diseaseData in diseaseList$data) {
        ID <- diseaseData$data$id
        name <- diseaseData$data$name
        diseaseIDNameMap[[ID]] <- name
      }
      return(diseaseIDNameMap)
    },
    getGenesForDiseaseAsDataFrameChunk = function(filterType, diseaseEFO, startFrom, cutoffScore, direct) {
      "This method is used internally by the class. It uses the 'filterType' argument to determine whether 'associations' or 'evidence' is returned'.
       Return a data frame of gene records for a given EFO ID for 'filterType' =  'evidence/filter?disease' || 'association/filter?disease."
      subdomain <- sprintf('/%s=%s&datastructure=simple&direct=%s&scorevalue_min=%0.2f&size=500&from=%d&format=tab',
                           filterType, diseaseEFO, direct, cutoffScore, startFrom)
      RESTResponse <- utilsObj$getRESTResponse(subdomain)
      utilsObj$checkRESTResponse(RESTResponse)
      genesForDiseaseList <- RESTResponse$responseContent
      dataFrame <- do.call(rbind, lapply(genesForDiseaseList$data, data.frame, stringsAsFactors=FALSE))
      if(is.null(dataFrame)) {
        return(NULL)
      }
      return(dataFrame)
    },
    getGenesForDiseaseAsDataFrame = function(filterType, diseaseEFO, startFrom, cutoffScore, direct) {
      "Used to return both assocoations and evidence genes for a given disease as a data frame.
       The filterType argument determines whether evidences or associations are returned.
       Calls self method 'getGenesForDiseaseAsDataFrameChunk' in a 'repeat' loop to build a data frame
       500 rows at a time. When complete the full data frame is returned.
      "
      allRecords <- data.frame()
      chunkSize <- 500
      sleepTimeSeconds <- 10
      repeat {
        dataFrame <- .self$getGenesForDiseaseAsDataFrameChunk(filterType, diseaseEFO, startFrom, cutoffScore, direct)
        if(is.null(dataFrame)) {
          break
        }
        Sys.sleep(sleepTimeSeconds)
        allRecords <- rbind(allRecords, dataFrame)
        startFrom <- startFrom + chunkSize
      }
      return(allRecords)
    },
    getEvidenceGenesForDiseaseAsDataFrame = function(diseaseEFO, cutoffScore, startFrom = 0 , direct = 'true') {
      "Return a data frame with all evidence genes for a given disease EFO and set of filters."
      filterType <- 'evidence/filter?disease'
      evidenceGenesForDisease <- .self$getGenesForDiseaseAsDataFrame(filterType, diseaseEFO, startFrom, cutoffScore, direct)
      return(evidenceGenesForDisease)
    },
    getAssociationGenesForDiseaseAsDataFrame = function(diseaseEFO, cutoffScore, direct, startFrom = 0) {
      "Return a data frame with all association genes for a given disease EFO and set of filters."
      filterType <- 'association/filter?disease'
      associationGenesForDisease <- .self$getGenesForDiseaseAsDataFrame(filterType, diseaseEFO, startFrom, cutoffScore, direct)
      associationGenesForDisease$DirectEvidenceOnly <- direct
      return(associationGenesForDisease)
    },
    getFirstDiseaseSummaryList= function() {
      "Return a list with summary information for the first disease returned by the search."
      firstDiseaseSummaryList <- list()
      firstDisease <- diseaseList$data[[1]]
      firstDiseaseSummaryList$efoID <- firstDisease$id
      firstDiseaseSummaryList$description <- firstDisease$data$description
      firstDiseaseSummaryList$association_count_total <- firstDisease$data$association_counts$total
      firstDiseaseSummaryList$association_count_direct <- firstDisease$data$association_counts$direct
      firstDiseaseSummaryList$efo_label <- firstDisease$data$efo_label
      firstDiseaseSummaryList$efo_url <- firstDisease$data$efo_url
      return(firstDiseaseSummaryList)
    }
  )
)
