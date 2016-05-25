context("Return values from 'Disease' methods")

# Global variables to be used in multiple tests.
diseaseName <- 'crohns disease'
diseaseObj <- ropentargets::Disease$new(diseaseName)

test_that("The type of the disease object returned by 'new' is 'Disease", {
  expect_is(diseaseObj, "Disease")
})

test_that("The first disease returned by the search engine has the expected EFO ID", {
  efoID <- diseaseObj$getFirstDiseaseEFOID()
  expectedEFOID <- 'EFO_0000384'
  expect_equal(efoID, expectedEFOID)
})

test_that("The method 'getFirstDiseaseName' returns the expected disease name", {
  returnedDiseaseName <- diseaseObj$getFirstDiseaseName()
  expectedDiseaseName <- "Crohn's disease"
  expect_equal(returnedDiseaseName, expectedDiseaseName)
})

test_that("Method 'getDiseaseIDNameMap()' returns a list of length > 0", {
  diseaseIDNameMap <- diseaseObj$getDiseaseIDNameMap()
  expect_is(diseaseIDNameMap, 'list')
  expect_gt(length(diseaseIDNameMap), 0)
})

test_that("Method 'getEvidenceGenesForDiseaseAsDataFrame()' returns a data frame", {
  efoID <- diseaseObj$getFirstDiseaseEFOID()
  cutoffScore <- 0.2
  genesEvidenceForDiseaseAsDataFrame <- diseaseObj$getEvidenceGenesForDiseaseAsDataFrame(efoID, cutoffScore)
  expect_is(genesEvidenceForDiseaseAsDataFrame, 'data.frame')
})

test_that("Method 'getAssociationGenesForDiseaseAsDataFrame' returns a data frame.", {
  efoID <- diseaseObj$getFirstDiseaseEFOID()
  cutoffScore <- 0.2
  direct <- 'true'
  geneAssocsForDiseaseDirectTrue <- diseaseObj$getAssociationGenesForDiseaseAsDataFrame(efoID, cutoffScore, direct)
  expect_is(geneAssocsForDiseaseDirectTrue, 'data.frame')
})

test_that("Method 'getFirstDiseaseSummaryList' returns a list with expected fields.", {
  firstDiseaseSummaryList <- diseaseObj$getFirstDiseaseSummaryList()
  expect_equal(firstDiseaseSummaryList$efoID, 'EFO_0000384')
  expect_true(is.numeric(firstDiseaseSummaryList$association_count_total))
  expect_equal(firstDiseaseSummaryList$efo_label, "Crohn's disease")
  expect_equal(firstDiseaseSummaryList$efo_url, 'http://www.ebi.ac.uk/efo/EFO_0000384')
})
