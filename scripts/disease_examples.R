library(methods)
library(ropentargets)
diseaseName = "inflammatory bowel disease"
# Create an "openTargetsDisease" object
diseaseObj <- ropentargets::Disease$new(diseaseName)
efoID <- diseaseObj$getFirstDiseaseEFOID()
# Print the disease name for the first match returned by the search engine.
fetchedDiseaseName <- diseaseObj$getFirstDiseaseName()
# Manually check the returned disease label to ensure it matches the expected value.
print(fetchedDiseaseName)
# 0.2 is considered a sensible default for the score cut-off.
cutoffScore <- 0.2
# Get the evidence for the disease.
evidenceGenesForDiseaseAsDataFrame <- diseaseObj$getEvidenceGenesForDiseaseAsDataFrame(efoID, cutoffScore)
# Association
associationGenesForDiseaseAsDataFrame <- diseaseObj$getAssociationGenesForDiseaseAsDataFrame(efoID, cutoffScore)
