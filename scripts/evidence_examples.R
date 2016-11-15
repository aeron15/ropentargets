library(methods)
library(ropentargets)
evidenceObj <- ropentargets::Evidence$new()
evidenceID <- '79f94e736879906aa48d4284a0dc22c8'
print(evidenceObj$getSingleEvidenceAsList(evidenceID))
print(evidenceObj$getSingleEvidenceAsJson(evidenceID))
evidenceIDList <- list("9f465fc141749e1d224e0d04888a67a2", "442adb2d1b13c4f2ccf7389988aefde5",
                       "fe3a819dfef936bd6cf6495bb8b50872", "083225ea209e2eb03f8aa7e5a318a675",
                       "975d452c23e3d83a333e16ee1ad80467", "dc0904ca379a693eb473bcc80f9b1823")
print(evidenceObj$getMultipleEvidencesAsList(evidenceIDList))
print(evidenceObj$getMultipleEvidencesAsJson(evidenceIDList))
filePath <- '~/test_out.json'
evidenceObj$writeEvStrAsJsonToFile(evidenceIDList, filePath)
# Clean up.

