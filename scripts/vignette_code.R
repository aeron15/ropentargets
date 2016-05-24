# Import the package.
library(ropentargets)
diseaseName = "inflammatory bowel disease"
# Create a Disease object.
diseaseObj <- ropentargets::Disease$new(diseaseName)
# Print all matched diseases as an EFO-Name list.
diseaseIDNameMap <- diseaseObj$getDiseaseIDNameMap()
print(diseaseIDNameMap)
# Print the disease name for the first match returned by the search engine.
firstDiseaseName <- diseaseObj$getFirstDiseaseName()
# Manually check the returned disease label to ensure it matches the expected value.
print(firstDiseaseName)
# Get the EFO ID for the first gene.
efoID <- diseaseObj$getFirstDiseaseEFOID()
print(efoID)
# 0.2 is considered a sensible default for the score cut-off.
cutoffScore <- 0.2
# Get the evidence for the disease as a data frame.
ibdEvidenceRecords <- diseaseObj$getEvidenceGenesForDiseaseAsDataFrame(efoID, cutoffScore)
# Association: There are "direct" and "indirect" associations.
direct <- 'true'
# get direct associations.
ibdAssociationRecordsDirectTrue <- diseaseObj$getAssociationGenesForDiseaseAsDataFrame(efoID, cutoffScore, direct)
direct <- 'false'
# Get indirect associations.
ibdAssociationRecordsDirectFalse <- diseaseObj$getAssociationGenesForDiseaseAsDataFrame(efoID, cutoffScore, direct)
# How many evidences were returned?
print(nrow(ibdEvidenceRecords))
# How many direct associations were returned?
print(nrow(ibdAssociationRecordsDirectTrue))
# How many indirect associations were returned?
print(nrow(ibdAssociationRecordsDirectFalse))
