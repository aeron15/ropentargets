library(ropentargets)
library(xlsx)

ensemblGeneID <- 'ENSG00000171105'
geneObjNoName <- ropentargets::Gene()
cutoffScore <- 0.2
associationDiseaseForGene <- geneObj$getAssociationDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore)
xlFile <- 'ENSG00000171105_DiseaseAssocs.xlsx'
# File will be written to the directory returned by the R function "getwd()".
write.xlsx(associationDiseaseForGene, xlFile, sheetName="GeneAssocs")
