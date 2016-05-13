#' A Reference Class to return the association object for a gene-disease pair (Ensembl gene ID - EFO ID).
#'
#' @field utilsObj An instance of class RestUtils
#' @field ensemblGeneID The ensembl gene identifer for the gene of interest (ENSG..).
#' @field efoID An EFO disease name identifier (EFO_...) for the disease of interest.
#' @field associationObjectAsList A list representing the gene-disease association object.
#' @export Association
Association <- setRefClass("Association",
  fields = list(
    utilsObj = "ANY",
    ensemblGeneID = "character",
    efoID = "character",
    associationObjectAsList = "list"
  ),
  methods = list(
    initialize = function(ensemblGeneID, efoID) {
      "Injitialize with an Ensembl gene ID and an EFO disease identifier.
      Calls an internal method to set the disease-gene list."
      utilsObj <<- RestUtils$new()
      ensemblGeneID <<- ensemblGeneID
      efoID <<- efoID
      .self$setAssociationObjectAsList()
    },
    setAssociationObjectAsList = function() {
      "Use the Ensembl gene and EFO disease identifer fields set on initialization to
      retrieve a disease-gene association object from the REST API that is used to set the
      'associationObjectAsList' field."
      geneDiseaseCombination <- paste(c(ensemblGeneID, efoID), sep = '', collapse = '-')
      subdomain <- sprintf('/association?id=%s', geneDiseaseCombination)
      RESTResponse <- utilsObj$getRESTResponse(subdomain)
      utilsObj$checkRESTResponse(RESTResponse)
      associationObjectAsList <<- RESTResponse$responseContent
    },
    getAssociationObjectAsList = function() {
      "Return the field 'associationObjectAsList'."
      return(associationObjectAsList)
    },

    getDataTypesMaxScores = function() {
      "Return a vector with the data types as names for the maximum score values
      in the field 'associationObjectAsList'."
      dataTypesMaxScores <- unlist(associationObjectAsList$data[[1]]$max$datatypes)
      return(dataTypesMaxScores)
    }
  )
)
