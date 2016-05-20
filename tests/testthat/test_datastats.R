context("Return values from 'DataStats' methods")

# Global variables to be used in multiple tests.
dataStats <- ropentargets::DataStats$new()
expectedDataTypeNames = c("literature", "rna_expression", "genetic_association", "somatic_mutation",
                          "known_drug", "animal_model", "affected_pathway")

test_that("The 'new' method returns an instance of 'DataStats'.", {
  expect_is(dataStats, "DataStats")
})

test_that("The 'getAllStatsAsList' method returns a list containing all the metrics for data in the Open Targets platform.", {
  allStatsAsList <- dataStats$getAllStatsAsList()
  expect_is(allStatsAsList, 'list')
})

test_that("The 'getDataTypeNames' method returns a vector containing the names of all data types.", {
  dataTypeNames <- dataStats$getDataTypeNames()
  expect_equal(is.vector(dataTypeNames), TRUE)
})

test_that("The vector returned by 'getDataTypeNames' contains all expected values.", {
  dataTypeNames <- dataStats$getDataTypeNames()
  expect_equal(unique(expectedDataTypeNames %in% dataTypeNames), TRUE)
})

test_that("The method 'getDataTypeListByName' returns a list for a given data type name.", {
  dataTypeName = 'genetic_association'
  dataTypeList <- dataStats$getDataTypeListByName(dataTypeName)
  expect_is(dataTypeList, 'list')
})

test_that("The method 'getDataSourceNamesForDataTypesList' returns a list of vectors.", {
  dataSourceNamesForDataTypesList <- dataStats$getDataSourceNamesForDataTypesList()
  expect_is(dataSourceNamesForDataTypesList, 'list')
  testDataType <- 'somatic_mutation'
  dataSources <- dataSourceNamesForDataTypesList[[testDataType]]
  expect_equal(is.vector(dataSources), TRUE)
})

test_that("There is at least one data source for each data type.", {
  dataSourceNamesForDataTypesList <- dataStats$getDataSourceNamesForDataTypesList()
  dataSourceCounts <- c()
  for (dataTypeName in expectedDataTypeNames) {
    dataSourceCount = length(dataSourceNamesForDataTypesList[[dataTypeName]])
    dataSourceCounts <- c(dataSourceCounts, dataSourceCount)
  }
  expect_equal(all(dataSourceCounts) > 0, TRUE)
})

test_that("The method 'getDataSourceEvStrCountsList' returns a list mapping data source names to evidence string counts for all expected data types.", {
  dataSourceEvStrCountsList <- dataStats$getDataSourceEvStrCountsList()
  expect_is(dataSourceEvStrCountsList, 'list')
})

test_that("The given data source name exists", {
  dataTypeName <- 'somatic_mutation'
  dataSourceName <- 'cancer_gene_census'
  dataSourceNamesForDataTypesList <- dataStats$getDataSourceNamesForDataTypesList()
  expect_equal(dataSourceName %in% dataSourceNamesForDataTypesList[[dataTypeName]], TRUE)
})

test_that("Evidence string count for a given data source is an integer.", {
  dataSourceEvStrCountsList <- dataStats$getDataSourceEvStrCountsList()
  dataSourceName <- 'cancer_gene_census'
  expect_equal(is.integer(dataSourceEvStrCountsList[[dataSourceName]]), TRUE)
})

test_that("Association count for a given data source is an integer.", {
  dataSourceAssocCountsList <- dataStats$getDataSourceAssocCountsList()
  dataSourceName <- 'cancer_gene_census'
  expect_equal(is.integer(dataSourceAssocCountsList[[dataSourceName]]), TRUE)
})


