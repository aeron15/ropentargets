context("Test methods in Evidence class used to retrieve full evidence strings as either JSON or R lists")

# Create a global Evidence object
evidenceObj <- ropentargets::Evidence$new()
# Get some evidence string identifiers by create a disease object and get a data frame containing evidence strings from this.
diseaseObj <- ropentargets::Disease('chronic myelogenous leukemia')
efoID <- diseaseObj$getFirstDiseaseEFOID()
cutoffScore <- 0.5
diseaseEvidence <- diseaseObj$getEvidenceGenesForDiseaseAsDataFrame(efoID, cutoffScore)
# Extract the 32 character evidence unique identifiers into a vector.
evidenceIDs <- diseaseEvidence$id
# Extract the 32 character IDs into a vector.
ids <- diseaseEvidence$id

test_that("The type of the object returned by 'new' is 'Evidence", {
  expect_is(evidenceObj, "Evidence")
})

test_that("The data frame with evidence for the disease at least five rows", {
  expect_gt(nrow(diseaseEvidence), 4)
})

test_that("The method 'getSingleEvidenceAsList' returns a list for a given 32 character unique identifier", {
  # Take the first identifier
  testID <- evidenceIDs[1]
  singleEvidenceAsList <- evidenceObj$getSingleEvidenceAsList(testID)
  expect_is(singleEvidenceAsList, 'list')
})

test_that("The method 'getMultipleEvidencesAsList' returns a list for a given input list of 32 character unique identifiers", {
  testIDs <- as.list(evidenceIDs[1:5])
  multipleEvidenceAsList <- evidenceObj$getMultipleEvidencesAsList(testIDs)
  expect_is(multipleEvidenceAsList, 'list')
})

test_that("The method 'writeEvStrAsJsonToFile' can write the 32 character identifier and their evidence strings as JSON to a tab-delimited file and then load that file into a data frame", {
  testOutputTmpFile <- tempfile()
  testIDs <- as.list(evidenceIDs[1:5])
  retVal <- evidenceObj$writeEvStrAsJsonToFile(testIDs, testOutputTmpFile)
  expect_true(file.exists(testOutputTmpFile))
  expect_true(retVal)
  fileAsDataFrame <- read.csv(testOutputTmpFile, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  expect_equal(nrow(fileAsDataFrame), 5)
})
