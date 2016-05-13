#' A Reference Class to generate evidence data frames by extracting certain field values from evidence objects.
#'
#' @field evStrChunks A list of lists of evidence objects as returned by method 'getEvStrAll' in class 'EvidenceForDataType'.
#'
DataTypeEvidenceDataFrame<-setRefClass("DataTypeEvidenceDataFrame",
  fields = list(
    evStrChunks = "list"
  ),
  methods = list(
    initialize = function(evStrChunks) {
      "Initialize with a list of lists of ecidence objects."
      evStrChunks <<- evStrChunks
    },
    getCommonFieldsAsDataFrame = function() {
      "Process the field list 'evStrChunks' and return all the data fields common to all data types as a data frame."
      infoLists = list()
      for (name in names(evStrChunks)) {
        chunk <- evStrChunks[[name]]
        evStrs <- chunk$data
        counter <- 0
        for (evStr in evStrs) {
          infoList <- list(
            evStrId = evStr$id,
            assocScore = evStr$scores$association_score,
            dataSourceName = evStr$sourceID,
            targetId = evStr$target$id,
            geneSymbol = evStr$target$gene_info$symbol,
            geneName = evStr$target$gene_info$name,
            geneID = geneSymbol <- evStr$target$gene_info$geneid,
            diseaseEFOID = evStr$disease$efo_info$efo_id,
            diseaseLabel = evStr$disease$efo_info$label
          )
          infoLists[[length(infoLists)+1]] <- infoList
        }
      }
      dataFrame <- do.call(rbind, lapply(infoLists, data.frame, stringsAsFactors=FALSE))
      return(dataFrame)
    }
  )
)


