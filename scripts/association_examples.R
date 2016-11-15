library(methods)
# Interested in Inflammatory Bowel Disease and all the associations for this disease currently held in CTTV.
# Firstly, we need to find the EFO identifier for "Inflammatory Bowel Disease".
ensemblGeneID <- 'ENSG00000073756'
efoID <- 'EFO_0003767'
assocObj <- ropentargets::Association$new(ensemblGeneID, efoID)
assocDetails <- assocObj$getAssociationObjectAsList()
str(assocDetails)
print(assocObj$getDataTypesMaxScores())
