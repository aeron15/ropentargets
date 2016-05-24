library(ropentargets)
geneName = "il13" # Search engine is tolerant of text case differences and can find gene synonyms.
# Create an "OpenTargetsGene" instance.
geneObj <- ropentargets::Gene$new(geneName)
# Print the Ensembl gene ID for the first gene in the returned list.
# This is assumed to be the best match but needs to be checked.
ensemblGeneID <- geneObj$getFirstEnsemblGeneID()
# Print a list mapping the Ensembl gene IDs to gene symbols for all genes returned by the REST API call to the search engine.
print(geneObj$getGeneIDNameMap())
print(ensemblGeneID)
# set the cut-off
cutoffScore <- 0.2
diseasesEvidenceForGeneAsDataFrame <- geneObj$getEvidenceDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore)
direct <- 'true'
diseaseAssocsForGeneDirectTrue <- geneObj$getAssociationDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore, direct)
direct <- 'false'
diseaseAssocsForGeneDirectFalse <- geneObj$getAssociationDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore, direct)
# Print out the row counts for the three data frames
print(nrow(diseasesEvidenceForGeneAsDataFrame))
print(nrow(diseaseAssocsForGeneDirectTrue))
print(nrow(diseaseAssocsForGeneDirectFalse))
