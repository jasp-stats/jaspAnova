context("ANOVA -- Verification project")

# does not test
# - if analysis handles too few observations

# https://jasp-stats.github.io/jasp-verification-project/anova.html#one-way-independent-anova
test_that("Main table results match R, SPSS, SAS and MiniTab", {
  options <- jaspTools::analysisOptions("Anova")
  options$dependent <- "Score"
  options$fixedFactors <- "Group"

  options$modelTerms <- list(
    list(components="Group")
  )

  result <- jaspTools::runAnalysis("Anova", "ANOVA.csv", options)

  # Main table
  resultTable <- result$results$anovaContainer$collection$anovaContainer_anovaTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", 3, 0.308172852029183, 7.52393902393904, 0.81930549448023,
               22.5718170718171, "Group", "TRUE", 36, "", 24.4146717480051,
               "", 878.928182928183, "Residuals"))
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#factorial-independent-anova
test_that("Main table results match  R, SPSS, SAS and MiniTab 2-factor", {
  options <- jaspTools::analysisOptions("Anova")
  options$dependent <- "Score"
  options$fixedFactors <- c("Norms", "Standing")

  options$modelTerms <- list(
    list(components="Norms"),
    list(components="Standing"),
    list(components=c("Norms", "Standing"))
  )

  result <- jaspTools::runAnalysis("Anova", "FIanova.csv", options)

  #  Main table
  resultTable <- result$results$anovaContainer$collection$anovaContainer_anovaTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", 1, 0.35820895522388, 4.26666666666665, 0.552004973907298,
               4.26666666666665, "Norms", "FALSE", 2, 209.641791044776, 2497.06666666667,
               3.51846028634314e-26, 4994.13333333333, "Standing", "FALSE",
               2, 34.0074626865672, 405.066666666666, 2.76364103266819e-10,
               810.133333333333, "Norms <unicode> Standing", "TRUE", 54, "",
               11.9111111111111, "", 643.199999999999, "Residuals"))
})

## Testing Kruskal-Wallis

options <- jaspTools::analysisOptions("Anova")
options$dependent <- "Score"
options$fixedFactors <- "Treatment"
options$kruskalVariablesAssigned <- "Treatment"

options$modelTerms <- list(
  list(components="Treatment")
)

result <- jaspTools::runAnalysis("Anova", "KWanova.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/anova.html#kruskal-wallis-anova
test_that("Main results match  R, SPSS, SAS and MiniTab 2", {
  #  Main table
  resultTable <- result$results$anovaContainer$collection$anovaContainer_anovaTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", 2, 10.6140474663246, 417.861111111111, 0.000275916880669353,
               835.722222222222, "Treatment", "TRUE", 33, "", 39.3686868686869,
               "", 1299.16666666667, "Residuals"))
})

test_that("Kruskal-Wallis results match  R, SPSS, SAS and MiniTab", {
  #  Kruskal-Wallis
  kruskalContainer <- result$results$anovaContainer$collection$anovaContainer_kruskalContainer$collection
  resultTable <- kruskalContainer$anovaContainer_kruskalContainer_kruskalTable$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("Treatment", 13.844384244926, 2, 0.00098566686805503))
})

# Ordinal restrictions ----
# Tests from https://restriktor.org

## Basic model ----
options <- analysisOptions("Anova")
options$.meta <- list(contrasts = list(shouldEncode = TRUE), customContrasts = list(
  shouldEncode = TRUE), dependent = list(shouldEncode = TRUE),
  fixedFactors = list(shouldEncode = TRUE), kruskalVariablesAssigned = list(
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
options$customContrasts <- list()
options$dependent <- "Age"
options$fixedFactors <- "Group"
options$modelTerms <- list(list(components = "Group"))
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
options$restrictedModelMarginalMeansTerms <- list()
options$restrictedModelShowAvailableCoefficients <- TRUE
options$restrictedModelSummaryByDefault <- FALSE
options$restrictedModels <- list(list(informedHypothesisTest = TRUE, marginalMeans = FALSE,
                                      modelName = "Model 1", modelSummary = TRUE, restrictionSyntax = "GroupActive < GroupPassive\nGroupPassive < GroupNo"))
options$restrictedSE <- "standard"
set.seed(1)
dataset <- read.csv("ZelazoKolb1972.csv")
dataset <- subset(dataset, Group != "Control")
results <- runAnalysis("Anova", dataset, options)


### see https://restriktor.org/tutorial/restriktor.html for comparison ----
test_that("Ordinal restrictions: Likelihood, penalty and GORIC match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison_comparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(64.7533269649054, 1, -29.5348420070337, "Model 1", 2.84182147541904
                                 ))
})


test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("GroupActive", 10.125, 1.59384623700347e-10, 0.618538022852505,
                                      16.3692442920593, "GroupNo", 12.35, 3.77071859965597e-11, 0.677574455581925,
                                      18.2267792096639, "GroupPassive", 11.375, 3.34358875477653e-11,
                                      0.618538022852504, 18.3901386491037))
})

### see https://restriktor.org/tutorial/contest.html for comparison ----
test_that("Ordinal restrictions: Informative Hypothesis Tests table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_ihtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.028341275692807, 5.97803555494542, "F", "Type global", 0.0276478338044095,
                                      6.04479400936707, "LRT", "Type global", 0.044864172884687, 4.78768638768638,
                                      "Score", "Type global", 0.028341275692807, 5.97803555494542,
                                      "F", "Type A", 0.0276478338044095, 6.04479400936707, "LRT",
                                      "Type A", 0.044864172884687, 4.78768638768638, "Score", "Type A",
                                      1, 0, "F", "Type B", 1, -7.105427357601e-15, "LRT", "Type B",
                                      1, 0, "Score", "Type B", 0.152944344084674, 1.0627396241062,
                                      "t", "Type C"))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 0, 1, 0, 0, 1, -1, 0))
})

## Ordered means and with effect sizes ----

options <- analysisOptions("Anova")
options$.meta <- list(contrasts = list(shouldEncode = TRUE), customContrasts = list(
  shouldEncode = TRUE), dependent = list(shouldEncode = TRUE),
  fixedFactors = list(shouldEncode = TRUE), kruskalVariablesAssigned = list(
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
options$customContrasts <- list()
options$dependent <- "Age"
options$fixedFactors <- "Group"
options$modelTerms <- list(list(components = "Group"))
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
options$restrictedModelMarginalMeansTerms <- list()
options$restrictedModelShowAvailableCoefficients <- FALSE
options$restrictedModelSummaryByDefault <- FALSE
options$restrictedModels <- list(list(informedHypothesisTest = TRUE, marginalMeans = FALSE,
                                      modelName = "Model 1", modelSummary = TRUE, restrictionSyntax = "GroupActive  < GroupPassive\nGroupPassive < GroupControl\nGroupControl < GroupNo"),
                                 list(informedHypothesisTest = TRUE, marginalMeans = FALSE,
                                      modelName = "Model 2", modelSummary = TRUE, restrictionSyntax = "(GroupPassive - GroupActive)  / 1.516 > 0.2\n(GroupControl - GroupPassive) / 1.516 > 0.2\n(GroupNo      - GroupControl) / 1.516 > 0.2"))
options$restrictedSE <- "standard"
set.seed(1)
results <- runAnalysis("Anova", "ZelazoKolb1972.csv", options)


### see https://restriktor.org/tutorial/example1.html for comparison ----
test_that("Ordinal restrictions: Informative Hypothesis Tests table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_ihtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0308475934292769, 6.42666580972066, "F", "Type global", 0.0278151519017313,
                                      6.70126802148546, "LRT", "Type global", 0.0431126887497895,
                                      5.56056578050442, "Score", "Type global", 0.0308475934292769,
                                      6.42666580972066, "F", "Type A", 0.0278151519017313, 6.70126802148546,
                                      "LRT", "Type A", 0.0431126887497895, 5.56056578050442, "Score",
                                      "Type A", 1, 0, "F", "Type B", 1, 0, "LRT", "Type B", 1, 0,
                                      "Score", "Type B", 0.35381027878063, 0.380738874434879, "t",
                                      "Type C"))
})

test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("GroupActive", 10.125, 1.19077449127484e-12, 0.619065391589877,
                                      16.3552996784348, "GroupControl", 11.7083333333333, 8.7743489822692e-14,
                                      0.619065391589877, 18.91291855819, "GroupNo", 12.35, 1.73632870169313e-13,
                                      0.678152159089088, 18.2112522012594, "GroupPassive", 11.375,
                                      1.4783390885313e-13, 0.619065391589877, 18.3744724782415))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 0, 0, 1, 0, 0, 1, 0, -1, 0, 0, -1, 1, 0, 0))
})

### see https://restriktor.org/tutorial/example3.html for comparison
test_that("Ordinal restrictions: Informative Hypothesis Tests table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_ihtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.168856977319199, 2.38400146304897, "F", "Type global", 0.144342629920537,
                                      2.71869356602637, "LRT", "Type global", 0.163454084273826, 2.45267623450673,
                                      "Score", "Type global", 0.168856977319199, 2.38400146304897,
                                      "F", "Type A", 0.144342629920537, 2.71869356602637, "LRT", "Type A",
                                      0.163454084273826, 2.45267623450673, "Score", "Type A", 1, 0,
                                      "F", "Type B", 1, 0, "LRT", "Type B", 1, 0, "Score", "Type B",
                                      0.48645111069787, 0.0344187942489155, "t", "Type C"))
})

test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("GroupActive", 10.125, 1.19077449127485e-12, 0.619065391589877,
                                      16.3552996784348, "GroupControl", 11.7083333333333, 8.77434898226892e-14,
                                      0.619065391589876, 18.91291855819, "GroupNo", 12.35, 1.73632870169313e-13,
                                      0.678152159089088, 18.2112522012594, "GroupPassive", 11.375,
                                      1.47833908853122e-13, 0.619065391589875, 18.3744724782416))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.659630606860158, 0, 0, 0.659630606860158, 0.2, 0, 0.659630606860158,
                                      0, -0.659630606860158, 0.2, 0, -0.659630606860158, 0.659630606860158,
                                      0, 0.2))
})

### see https://restriktor.org/tutorial/example6.html for comparison ----
options <- analysisOptions("Anova")
options$.meta <- list(contrasts = list(shouldEncode = TRUE), customContrasts = list(
  shouldEncode = TRUE), dependent = list(shouldEncode = TRUE),
  fixedFactors = list(shouldEncode = TRUE), kruskalVariablesAssigned = list(
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
options$customContrasts <- list()
options$dependent <- "Anger"
options$fixedFactors <- "Group"
options$modelTerms <- list(list(components = "Group"))
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
options$restrictedModelComparison <- "unconstrained"
options$restrictedModelComparisonCoefficients <- FALSE
options$restrictedModelComparisonHighlightCoefficients <- TRUE
options$restrictedModelComparisonMatrix <- FALSE
options$restrictedModelComparisonReference <- "Model 2"
options$restrictedModelComparisonWeights <- FALSE
options$restrictedModelMarginalMeansTerms <- list()
options$restrictedModelShowAvailableCoefficients <- FALSE
options$restrictedModelSummaryByDefault <- FALSE
options$restrictedModels <- list(list(informedHypothesisTest = FALSE, marginalMeans = FALSE,
                                      modelName = "Model 1", modelSummary = FALSE, restrictionSyntax = "GroupNo = GroupPhysical\nGroupPhysical = GroupBehavioral\nGroupBehavioral = GroupBoth"),
                                 list(informedHypothesisTest = FALSE, marginalMeans = FALSE,
                                      modelName = "Model 2", modelSummary = FALSE, restrictionSyntax = "GroupNo < GroupPhysical\nGroupPhysical = GroupBehavioral\nGroupBehavioral < GroupBoth"),
                                 list(informedHypothesisTest = FALSE, marginalMeans = FALSE,
                                      modelName = "Model 3", modelSummary = FALSE, restrictionSyntax = "GroupNo < GroupPhysical\nGroupPhysical < GroupBehavioral\nGroupBehavioral < GroupBoth"))
options$restrictedSE <- "standard"
set.seed(1)
results <- runAnalysis("Anova", "AngerManagement.csv", options)

test_that("Ordinal restrictions: Model Comparison Table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison_comparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(190.802338005453, 6.15799536281089e-06, -93.4011690027263, "Model 1",
                                      2, 174.107875497759, 0.025977408886342, -84.1621111968489, "Model 2",
                                      2.89182655203061, 167.133864718034, 0.849147292951048, -80.4838987099312,
                                      "Model 3", 3.08303364908579, 170.967797419862, 0.124869140167247,
                                      -80.4838987099312, "Unconstrained", 5))
})
