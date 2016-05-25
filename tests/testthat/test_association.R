context("Tests for the 'Association' class")

# Global variables for the tests that follow
ensemblGeneID <- 'ENSG00000073756'
efoID <- 'EFO_0003767'
assocObj <- ropentargets::Association$new(ensemblGeneID, efoID)

test_that("The method 'getAssociationObjectAsList' returns a list", {
  geneDiseaseAssoc <-  assocObj$getAssociationObjectAsList()
  expect_is(geneDiseaseAssoc, 'list')
})

test_that("The method 'getDataTypesMaxScores' returns a vector", {
  dataTypesMaxScores <- assocObj$getDataTypesMaxScores()
  expect_true(is.vector(dataTypesMaxScores))
})
