context("Return values from 'Gene' methods")

# Global variables to be used in multiple tests.
geneName <- 'nod2'
geneObj <- ropentargets::Gene$new(geneName)

test_that("The type of the gene object returned by 'new' is 'Gene", {
  expect_is(geneObj, "Gene")
})

test_that("The first gene returned by the search engine has the expected Ensembl gene ID", {
  ensemblGeneID <- geneObj$getFirstEnsemblGeneID()
  expectedEnsemblGeneID <- 'ENSG00000167207'
  expect_equal(ensemblGeneID, expectedEnsemblGeneID)
})

test_that("Method 'getGeneIDNameMap()' returns a list of length > 0", {
  geneIDNameMap <- geneObj$getGeneIDNameMap()
  expect_is(geneIDNameMap, 'list')
  expect_gt(length(geneIDNameMap), 0)
})

test_that("Method 'getEvidenceDiseasesForGeneAsDataFrame()' returns a data frame", {
  ensemblGeneID <- geneObj$getFirstEnsemblGeneID()
  cutoffScore <- 0.2
  diseasesEvidenceForGeneAsDataFrame <- geneObj$getEvidenceDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore)
  expect_is(diseasesEvidenceForGeneAsDataFrame, 'data.frame')
})

test_that("Method 'getAssociationDiseasesForGeneAsDataFrame' returns a data frame", {
  ensemblGeneID <- geneObj$getFirstEnsemblGeneID()
  cutoffScore <- 0.2
  diseaseAssocsForGeneDirectTrue <- geneObj$getAssociationDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore)
  expect_is(diseaseAssocsForGeneDirectTrue, 'data.frame')
})

test_that("Method 'getFirstGeneSummaryList' returns a list with expected fields", {
  firstGeneSummaryList <- geneObj$getFirstGeneSummaryList()
  expect_is(firstGeneSummaryList, 'list')
  expectedEnsemblGeneID <- 'ENSG00000167207'
  expectedUniprotAccession <- 'Q9HC29'
  expectedGeneSymbol <- 'NOD2'
  geneID <- firstGeneSummaryList$ensembl_gene_id
  uniprotAccession <- firstGeneSummaryList$uniprot_accession
  assocTotalCount <- firstGeneSummaryList$association_count_total
  approvedSymbol <- firstGeneSummaryList$approved_symbol
  expect_equal(geneID, expectedEnsemblGeneID)
  expect_equal(uniprotAccession, expectedUniprotAccession)
  expect_true(is.numeric(assocTotalCount))
  expect_equal(expectedGeneSymbol, approvedSymbol)
})

test_that("When constructor argument 'geneName' is empty that the Gene object is created", {
  geneObjNoName <- ropentargets::Gene()
  expect_is(geneObjNoName, 'Gene')
})

test_that("When constructor argument 'geneName' is empty that evidence data frame is returned for given Ensembl gene.", {
  ensemblGeneID <- 'ENSG00000167207' # "NOD2"
  geneObjNoName <- ropentargets::Gene()
  cutoffScore <- 0.2
  diseasesEvidenceForGeneAsDataFrame <- geneObjNoName$getEvidenceDiseasesForGeneAsDataFrame(ensemblGeneID, cutoffScore)
  expect_is(diseasesEvidenceForGeneAsDataFrame, 'data.frame')
})

