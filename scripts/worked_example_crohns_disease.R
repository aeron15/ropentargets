library(ropentargets)
diseaseName <- 'Crohns Disease'
diseaseObj <- ropentargets::Disease$new(diseaseName)
efoID <- diseaseObj$getFirstDiseaseEFOID()
print(diseaseObj$getFirstDiseaseName())
cutOffScore <- 0.2
diseaseEvidence <- diseaseObj$getEvidenceGenesForDiseaseAsDataFrame(efoID, cutOffScore)
diseaseAssocDirect <- diseaseObj$getAssociationGenesForDiseaseAsDataFrame(efoID, cutOffScore, 'true')
diseaseAssocIndirect <- diseaseObj$getAssociationGenesForDiseaseAsDataFrame(efoID, cutOffScore, 'false')
