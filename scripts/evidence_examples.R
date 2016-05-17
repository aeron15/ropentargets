library(ropentargets)
evidenceObj <- ropentargets::Evidence$new()
evidenceID <- '79f94e736879906aa48d4284a0dc22c8'
print(evidenceObj$getSingleEvidenceAsList(evidenceID))
print(evidenceObj$getSingleEvidenceAsJson(evidenceID))
evidenceIDList <- list('a36fb3bf8b6a4bc1202c347f4dc4fbbb', '79f94e736879906aa48d4284a0dc22c8')
print(evidenceObj$getMultipleEvidencesAsList(evidenceIDList))
print(evidenceObj$getMultipleEvidencesAsJson(evidenceIDList))
filePath <- '~/test_out.json'
evidenceObj$writeEvStrAsJsonToFile(evidenceIDList, filePath)
# Clean up.

