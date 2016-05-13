#' A Reference Class to return information on a given gene.
#'
#' @field utilsObj An instance of class RestUtils
#' @field geneSymbol The gene name or gene symbol as string.
#' @field geneList A list containing platform information for a gene.
#'
Gene <- setRefClass("Gene",
  fields = list(
    geneSymbol = "character",
    geneList = "list",
    utilsObj = "ANY"
  ),
  methods = list(
    initialize = function(geneSymbol) {
      "Provide a gene symbol or gene name as single argument.
      Calls a self method to populate 'geneList'."
      geneSymbol <<- geneSymbol
      utilsObj <<- RestUtils$new()
      .self$setGeneList()
    },
    setGeneList = function() {
      "Set field 'geneList'. Called in 'initialize()'."
      subdomain <- sprintf('/search?q=%s', geneSymbol)
      RESTResponse <- utilsObj$getRESTResponse(subdomain)
      utilsObj$checkRESTResponse(RESTResponse)
      geneList <<- RESTResponse$responseContent
    },
    getGeneList = function() {
      "Return field 'geneList'."
      return(geneList)
    },
    getFirstEnsemblGeneID = function() {
      "Returns the first Ensembl gene ID in 'geneList'.
      The first one is assumed to be the best match but needs to be verified."
      return(geneList$data[[1]]$id)
    },
    getGeneIDNameMap = function() {
      "Return a list mapping Ensembl gene IDs to gene symbols
       for all genes returned by the search engine for the gene name
       set in 'initialize()'."
      geneIDNameMap <- list()
      for (geneData in geneList$data) {
        ID <- geneData$data$id
        name <- geneData$data$name
        geneIDNameMap[[ID]] <- name
      }
      return(geneIDNameMap)
    },
    getDiseasesForGeneAsDataFrameChunk = function(filterType, ensemblGeneID, startFrom, cutoffScore, direct) {
      "Return a data frame of disease records for a given Ensembl gene ID for 'filterType' =  'evidence' || 'association'.
      Used internally."
      subdomain <- sprintf('/%s/filter?target=%s&datastructure=simple&direct=%s&scorevalue_min=%0.2f&size=500&from=%d&format=tab',
                           filterType, ensemblGeneID, direct, cutoffScore, startFrom)
      RESTResponse <- utilsObj$getRESTResponse(subdomain)
      utilsObj$checkRESTResponse(RESTResponse)
      diseasesForGeneList <- RESTResponse$responseContent
      dataFrame <- do.call(rbind, lapply(diseasesForGeneList$data, data.frame, stringsAsFactors=FALSE))
      if(is.null(dataFrame)) {
        return(NULL)
      }
      return(dataFrame)
    },
    getDiseasesForGeneAsDataFrame = function(filterType, ensemblGeneID, startFrom, cutoffScore, direct) {
      "Used to return both associations and evidence genes for a given disease as a data frame.
       The 'filterType' argument determines whether evidences or associations are returned.
      Used internally."
      allRecords <- data.frame()
      chunkSize <- 500
      sleepTimeSeconds <- 10
      repeat {
        dataFrame <- .self$getDiseasesForGeneAsDataFrameChunk(filterType, ensemblGeneID, startFrom, cutoffScore, direct)
        if(is.null(dataFrame)) {
          break
        }
        Sys.sleep(sleepTimeSeconds)
        allRecords <- rbind(allRecords, dataFrame)
        startFrom <- startFrom + chunkSize
      }
      return(allRecords)
    },
    getEvidenceDiseasesForGeneAsDataFrame = function(ensemblGeneID, cutoffScore, startFrom = 0, direct = 'true') {
      "Return a data frame with all evidence diseases for a given ensembl gene ID and set of filters.
       The 'direct'' argument is irrelevant to 'evidence' and only applies to 'associations' so setting it
       to a default of 'true'."
      filterType <- 'evidence'
      evidenceDiseasesForGene <- .self$getDiseasesForGeneAsDataFrame(filterType, ensemblGeneID, startFrom, cutoffScore, direct)
      return(evidenceDiseasesForGene)
    },
    getAssociationDiseasesForGeneAsDataFrame = function(ensemblGeneID, cutoffScore, direct, startFrom = 0) {
      "Return a data frame with all disease association' for a given ensembl gene ID and set of filters."
      filterType <- 'association'
      associationDiseasesForGene <- .self$getDiseasesForGeneAsDataFrame(filterType, ensemblGeneID, startFrom, cutoffScore, direct)
      associationDiseasesForGene$DirectEvidenceOnly <- direct
      return(associationDiseasesForGene)
    }
  )
)
