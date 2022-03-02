context("ANCOVA -- Verification project")

# does not test
# - descriptives table/plot & Q-Q plot & raincloud plot (uses same code as ANOVA)
# - if analysis handles too few observations

## Testing standard ----

options <- jaspTools::analysisOptions("Ancova")

options$dependent <- "Happiness"
options$fixedFactors <- "Dose"
options$covariates <- "Puppy_love"
options$modelTerms <- list(
  list(components = "Dose"),
  list(components = "Puppy_love")
)

options$contrasts <- list(
  list(contrast = "simple", variable = "Dose")
)

results <- jaspTools::runAnalysis("Ancova", "Puppy Love.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/anova.html#ancova
test_that("Main table results match R, SPSS, SAS and MiniTab 1", {
  # main table
  resultTable <- results$result$anovaContainer$collection$anovaContainer_anovaTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", 2, 4.14192880386377, 12.5925971041911, 0.0274465428639958,
               25.1851942083821, "Dose", "TRUE", 1, 4.9586811332752, 15.0757477099169,
               0.0348333806474408, 15.0757477099169, "Puppy_love", "TRUE",
               26, "", 3.0402736745364, "", 79.0471155379464, "Residuals"))
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#ancova
test_that("Contrast table results match R, SPSS, SAS and MiniTab 1", {
  contrastContainer <- results$results$anovaContainer$collection$anovaContainer_contrastContainer$collection
  contrastContainer2 <- contrastContainer$anovaContainer_contrastContainer_simpleContrast_Dose$collection
  resultTable <- contrastContainer2$anovaContainer_contrastContainer_simpleContrast_Dose_contrastTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", "2 - 1", 0.849355306996752, 26, 1.78568011481422, 0.0453535580857103,
               2.1023947223315, "FALSE", "3 - 1", 0.802810905470399, 26, 2.22488132183556,
               0.0101750138508479, 2.77136409916095))
})

# Ordinal restrictions ----
## see https://restriktor.org/tutorial/example2.html for comparison
options <- analysisOptions("Ancova")
options$.meta <- list(contrasts = list(shouldEncode = TRUE), covariates = list(
  shouldEncode = TRUE), customContrasts = list(shouldEncode = TRUE),
  dependent = list(shouldEncode = TRUE), fixedFactors = list(
    shouldEncode = TRUE), kruskalVariablesAssigned = list(
      shouldEncode = TRUE), marginalMeansTerms = list(shouldEncode = TRUE),
  modelTerms = list(shouldEncode = TRUE), moderatorFactorOne = list(
    shouldEncode = TRUE), moderatorFactorTwo = list(shouldEncode = TRUE),
  plotHorizontalAxis = list(shouldEncode = TRUE), plotSeparateLines = list(
    shouldEncode = TRUE), plotSeparatePlots = list(shouldEncode = TRUE),
  postHocTestsVariables = list(shouldEncode = TRUE), rainCloudPlotsHorizontalAxis = list(
    shouldEncode = TRUE), rainCloudPlotsSeparatePlots = list(
      shouldEncode = TRUE), restrictedModelMarginalMeansTerms = list(
        shouldEncode = TRUE), simpleFactor = list(shouldEncode = TRUE),
  wlsWeights = list(shouldEncode = TRUE))
options$contrasts <- list(list(contrast = "none", variable = "Group"))
options$covariates <- "Age"
options$customContrasts <- list()
options$dependent <- "Anger"
options$fixedFactors <- "Group"
options$modelTerms <- list(list(components = "Group"), list(components = "Age"))
options$rainCloudPlotsHorizontalAxis <- ""
options$rainCloudPlotsHorizontalDisplay <- FALSE
options$rainCloudPlotsLabelYAxis <- ""
options$rainCloudPlotsSeparatePlots <- ""
options$restrictedBootstrapping <- FALSE
options$restrictedBootstrappingConfidenceIntervalLevel <- 0.95
options$restrictedBootstrappingReplicates <- 1000
options$restrictedIncludeIntercept <- FALSE
options$restrictedInformedHypothesisTestByDefault <- FALSE
options$restrictedMarginalMeansByDefault <- FALSE
options$restrictedModelComparison <- "none"
options$restrictedModelComparisonCoefficients <- FALSE
options$restrictedModelComparisonHighlightCoefficients <- TRUE
options$restrictedModelComparisonMatrix <- FALSE
options$restrictedModelComparisonReference <- "Model 1"
options$restrictedModelComparisonWeights <- FALSE
options$restrictedModelMarginalMeansTerms <- list(list(variable = "Group"))
options$restrictedModelShowAvailableCoefficients <- FALSE
options$restrictedModelSummaryByDefault <- FALSE
options$restrictedModels <- list(list(informedHypothesisTest = FALSE, marginalMeans = TRUE,
                                      modelName = "Model 1", modelSummary = FALSE, restrictionSyntax = "GroupNo < GroupPhysical\nGroupPhysical  == GroupBehavioral\nGroupBehavioral < GroupBoth"))
options$restrictedSE <- "standard"
set.seed(1)
results <- runAnalysis("Ancova", "AngerManagement.csv", options)

test_that("Ordinal restrictions: Adjuested marginal means results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer_Group"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Behavioral", 0.467592714510265, 1.95108158220025, "Both", 0.702820099125852,
                                      4.06863411619283, "No", 0.69741742112576, -0.17079728059332,
                                      "Physical", 0.467592714510269, 1.95108158220025))
})
