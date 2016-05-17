geneName = "il13" # Search engine is tolerant of text case differences and can find gene synonyms.
# Create an "OpenTargetsGene" instance.
geneObj <- ropentargets::Gene$new(geneName)
# Print the entire gene object as a list (large).
# print(geneObj$getGeneList())
# Print the Ensembl gene ID for the first gene in the returned list.
# This is assumed to be the best match but check it first.
ensemblGeneID <- geneObj$getFirstEnsemblGeneID()
# Print a list mapping the Ensembl gene IDs to gene symbols for all
# genes returned by the REST API call to the search engine.
# print(geneObj$getGeneIDNameMap())
print(ensemblGeneID)
cutoffScore <- 0.2
diseasesEvidenceForGeneAsDataFrame <- geneObj$getEvidenceDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore)
direct <- 'true'
diseaseAssocsForGeneDirectTrue <- geneObj$getAssociationDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore, direct)
direct <- 'false'
diseaseAssocsForGeneDirectFalse <- geneObj$getAssociationDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore, direct)
