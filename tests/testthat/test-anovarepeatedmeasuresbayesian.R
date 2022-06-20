context("Bayesian Repeated Measures ANOVA")

# does not test
# - descriptives table (code from regular ANOVA)
# - descriptives plot (code from regular ANOVA)
# - bar plot (code from regular ANOVA)
# - raincloud plot (code is from regular ANOVA)
# - bftype (01, 10)

test_that("Main table and Effects table results match", {
  set.seed(0)
  options <- initOpts("AnovaRepeatedMeasuresBayesian")
  options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2"), name="RM_FACTOR_1")
  )
  options$betweenSubjectFactors <- "facGender"
  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components="RM_FACTOR_1", isNuisance=TRUE),
    list(components="facGender", isNuisance=FALSE),
    list(components="contcor1", isNuisance=FALSE),
    list(components=c("RM_FACTOR_1", "facGender"), isNuisance=FALSE)
  )
  options$priorCovariates <- 0.3
  options$priorFixedEffects <- 0.8
  options$priorRandomEffects <- 0.8

  options$effects <- TRUE

  refTablesModelComparison <- list(
    nullModelTop = list(1, 2.02638501389359, "Null model (incl. RM_FACTOR_1, subject)",
                        0.166666666666667, 0.288396523943212, "", 1.69320444782317,
                        4.77162304127099, "facGender", 0.166666666666667, 0.488314277077388,
                        44.2229019018819, 0.286342191337782, 0.450067041153177, "contcor1",
                        0.166666666666667, 0.0825800926400986, 39.4082320254761, 0.28460479470118,
                        0.447092050862548, "facGender + contcor1", 0.166666666666667,
                        0.082079033489392, 41.1428840377339, 0.171484942888838, 0.260143895615353,
                        "facGender + facGender<unicode><unicode><unicode>RM_FACTOR_1",
                        0.166666666666667, 0.049455661437741, 40.0629705456851, 0.0318117960879948,
                        0.0462968029784322, "facGender + contcor1 + facGender<unicode><unicode><unicode>RM_FACTOR_1",
                        0.166666666666667, 0.00917441141216799, 40.327021932526),
    bestModelTop = list(1, 3.34131157517392, "facGender", 0.166666666666667, 0.400573883982298,
                        "", 0.898667540451198, 2.81228939883351, "Null model (incl. RM_FACTOR_1, subject)",
                        0.166666666666667, 0.359982747087356, 12.5507359207671, 0.229568777946671,
                        0.506360852937598, "contcor1", 0.166666666666667, 0.0919592570231679,
                        27.8060873129689, 0.227346150012038, 0.50096719848448, "facGender + contcor1",
                        0.166666666666667, 0.0910689303187442, 20.5197149287988, 0.117361879179896,
                        0.246656352933523, "facGender + facGender<unicode><unicode><unicode>RM_FACTOR_1",
                        0.166666666666667, 0.047012103774552, 17.071031255608, 0.0234740161300624,
                        0.0474616748915938, "facGender + contcor1 + facGender<unicode><unicode><unicode>RM_FACTOR_1",
                        0.166666666666667, 0.0094030778138822, 19.2275168518371)
  )

  refTablesEffects <- list(
    allModels = list(0.847793843733313, "facGender", 0.333333333333333, 0.370976616583311,
                     0.666666666666667, 0.629023383416689, 0.210409821072135, "contcor1",
                     0.5, 0.826166462458341, 0.5, 0.173833537541659, 0.124563301118841,
                     "RM_FACTOR_1<unicode><unicode><unicode>facGender", 0.666666666666667,
                     0.941369927150091, 0.333333333333333, 0.058630072849909),
    matchedModels = list(1.08784492220115, "facGender", 0.333333333333333, 0.451942004110523,
                         0.333333333333333, 0.491642814301042, 0.238284689405314, "contcor1",
                         0.5, 0.807568734844206, 0.5, 0.192431265155794, 0.1147483090313,
                         "RM_FACTOR_1<unicode><unicode><unicode>facGender", 0.333333333333333,
                         0.491642814301042, 0.333333333333333, 0.0564151815884342)
  )

  orders       <- c("nullModelTop", "bestModelTop")
  effectsTypes <- c("allModels", "matchedModels")

  for (i in 1:2) {

    order <- orders[i]
    effectsType <- effectsTypes[i]

    options$bayesFactorOrder <- order
    options$effectsType      <- effectsType
    results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)

    table <- results[["results"]][["tableModelComparison"]][["data"]]

    jaspTools::expect_equal_tables(table, refTablesModelComparison[[order]], label=paste("Table with order", order))
    table <- results[["results"]][["tableEffects"]][["data"]]

    jaspTools::expect_equal_tables(table, refTablesEffects[[effectsType]], label=paste("Table with effectsType", effectsType))
  }
})

test_that("Post-hoc Comparisons table results match", {
  options <- jaspTools::analysisOptions("AnovaRepeatedMeasuresBayesian")
  options$repeatedMeasuresCells <- c("contNormal", "contGamma", "contcor1")
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2", "Level 3"), name="RM_FACTOR_1")
  )
  options$modelTerms <- list(
    list(components="RM_FACTOR_1", isNuisance=FALSE)
  )
  options$postHocTestsNullControl <- TRUE
  options$postHocTestsVariables <- "RM_FACTOR_1"
  options <- addCommonQMLoptions(options)

  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  table <- results[["results"]][["collectionPosthoc"]][["collection"]][["collectionPosthoc_postHoc_RM_FACTOR_1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Level 1", "Level 2", 142887114837413104, 83932041568197648, 0.587401051968199,
         2.78226779130567e-20, "Level 1", "Level 3", 0.521697047188026,
         0.30644539432695, 0.587401051968199, 0.0395333999732249, "Level 2",
         "Level 3", 82922847973592.7, 48708968131887.4, 0.587401051968199,
         1.07201998003854e-19)
  )
})

test_that("Analysis handles errors", {
  # NOTE: only errors that are not handled in test-anovabayesian or test-ancovabayesian are tested

  options <- initOpts("AnovaRepeatedMeasuresBayesian")
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2"), name="RM_FACTOR_1")
  )

  options$repeatedMeasuresCells <- c("contNormal", "debInf")
  options$modelTerms <- list(list(components="RM_FACTOR_1", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Inf RM factor check")
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="Inf RM factor check")

  # options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  # options$covariates <- "debInf"
  # options$modelTerms <- list(
  #   list(components="RM_FACTOR_1", isNuisance=FALSE),
  #   list(components="debInf", isNuisance=FALSE)
  # )
  # results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                 label="Inf covariate check")

  # options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  # options$betweenSubjectFactors <- "debSame"
  # options$covariates <- list()
  # options$modelTerms <- list(
  #   list(components="RM_FACTOR_1", isNuisance=FALSE),
  #   list(components="debSame", isNuisance=FALSE)
  # )
  # results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="1-level factor check")

  # options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  # options$betweenSubjectFactors <- list()
  # options$modelTerms <- list(list(components="RM_FACTOR_1", isNuisance=TRUE))
  # results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="All nuisance check")

  # options$repeatedMeasuresCells <- c("contNormal", "debSame")
  # options$modelTerms <- list(list(components="RM_FACTOR_1", isNuisance=FALSE))
  # results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="No variance check")

  # options$repeatedMeasuresCells <- c("contNormal", "debMiss99")
  # options$modelTerms <- list(list(components="RM_FACTOR_1", isNuisance=FALSE))
  # results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                 label="Too few obs check")
})

test_that("Analysis fails gracefully if some models error", {

  options <- initOpts("AnovaRepeatedMeasuresBayesian")
  options$covariates = list("contNormal")
  options$betweenSubjectFactors = list("contBinom")
  options$effects <- TRUE
  options$modelTerms = list(list(components = list("RM_FACTOR_1"), isNuisance = FALSE),
                            list(components = list("contBinom"), isNuisance = FALSE),
                            list(components = list("contNormal"), isNuisance = FALSE),
                            list(components = list("RM_FACTOR_1", "contBinom"), isNuisance = FALSE))
  options$repeatedMeasuresCells = list("contcor1", "contcor2")
  options$repeatedMeasuresFactors = list(list(levels = list("Level 1", "Level 2"), name = "RM_FACTOR_1"))

  # NOTE: the option below makes BayesFactor return NaN as BF for models with covariates.
  # It's a nice hack to test how gracefully the analysis recovers when some but not all BFs could be computed.
  # A user can never enter NULL here. This hack exists for BayesFactor version 0.9.12.4.2.
  options$priorCovariates <- NULL

  set.seed(42)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)

  mainTable <- results[["results"]][["tableModelComparison"]][["data"]]
  effectsTable <- results[["results"]][["tableEffects"]][["data"]]

  jaspTools::expect_equal_tables(
    mainTable,
    list(1, 4.81723072651078, "Null model (incl. subject)", 0.1, 0.546342823039303,
         "", 0.57259147990674, 1.82098643786428, "contBinom", 0.1, 0.3128312455805,
         4.73186300857416, 0.138862578799232, 0.328379305953214, "RM_FACTOR_1",
         0.1, 0.0758665733156897, 7.12638585019384, 0.0858219117860418,
         0.196779369864607, "RM_FACTOR_1 + contBinom", 0.1, 0.046888185563816,
         8.86956002564671, 0.0330766173520163, 0.0736149993547432, "RM_FACTOR_1 + contBinom + RM_FACTOR_1<unicode><unicode><unicode>contBinom",
         0.1, 0.0180711725006913, 17.6010066976745, 2, 2, 2, "NaN", "NaN",
         "contNormal", 0.1, "NaN", "", 2, 2, 2, "NaN", "NaN", "RM_FACTOR_1 + contNormal",
         0.1, "NaN", "", 2, 2, 2, "NaN", "NaN", "contBinom + contNormal",
         0.1, "NaN", "", 2, 2, 2, "NaN", "NaN", "RM_FACTOR_1 + contBinom + contNormal",
         0.1, "NaN", "", 2, 2, 2, "NaN", "NaN", "RM_FACTOR_1 + contBinom + contNormal + RM_FACTOR_1<unicode><unicode><unicode>contBinom",
         0.1, "NaN", ""),
    label = "Table where some BFs are NaN")

  jaspTools::expect_equal_tables(
    effectsTable,
    list(0.109272332211192, "RM_FACTOR_1", 0.4, 0.859174068619803, 0.6,
         0.140825931380197, 0.404783990575272, "contBinom", 0.4, 0.622209396354992,
         0.6, 0.377790603645008, "NaN", "contNormal", 1, 1, 0, 0, 0.0736149993547432,
         "RM_FACTOR_1<unicode><unicode><unicode>contBinom", 0.8, 0.981928827499309,
         0.2, 0.0180711725006913),
    label = "Table where one inclusion BF is NaN")

})

# Single model inference
options <- initOpts("AnovaRepeatedMeasuresBayesian")
options$modelTerms <- list(list(components = "RM.Factor.1", isNuisance = FALSE))
options$repeatedMeasuresCells <- c("Stick Insect", "Kangaroo Testicle")
options$repeatedMeasuresFactors <- list(list(levels = c("Level 1", "Level 2"), name = "RM.Factor.1"))
options$singleModelCriTable <- TRUE
options$singleModelEstimates <- TRUE
options$singleModelTerms <- list(list(components = "RM.Factor.1"))
set.seed(1)
results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "Bush Tucker Food.csv", options)

test_that("Single Model Posterior Summary table results match", {
  table <- results[["results"]][["containerSingleModel"]][["collection"]][["containerSingleModel_SMItablePosteriorEstimates"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("", 4.58948410870924, 6.16450279524202, 0.745342080929142, 7.70351606555363,
                           "Intercept", "Level 1", 0.732042615149814, 1.73232598619876,
                           0.452456686599393, 2.5434859267573, "RM.Factor.1", "Level 2",
                           -2.5434859267573, -1.73232598619876, 0.452456686599393, -0.732042615149814,
                           ""))
})

test_that("Single Model R² table results match", {
  table <- results[["results"]][["containerSingleModel"]][["collection"]][["containerSingleModel_tableSMICRI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.308711456209889, 0.638172359225456, 0.815825511804337, "R<unicode>"
                      ))
})

test_that("Model Comparison table results match", {
  table <- results[["results"]][["tableModelComparison"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(1, 75.0836170114885, "RM.Factor.1", 0.5, 0.986856565982542, "",
                           0.0133184846415562, 0.0133184846415562, "Null model (incl. subject)",
                           0.5, 0.0131434340174574, 19.861508543733))
})

# test whether sampling parameters with interactions effects works
options <- initOpts("AnovaRepeatedMeasuresBayesian")
options$repeatedMeasuresCells <- c("contNormal", "contGamma")
options$repeatedMeasuresFactors <- list(list(levels = c("Level 1", "Level 2"), name = "RM_Factor_1"))
options$betweenSubjectFactors <- "facGender"
options$covariates <- "contcor1"
options$modelTerms <- list(
  # main effects are nuisance to reduce the model complexity a bit
  list(components = "RM_Factor_1",                 isNuisance = TRUE),
  list(components = "facGender",                   isNuisance = TRUE),
  list(components = "contcor1",                    isNuisance = TRUE),
  list(components = c("contcor1", "facGender"),    isNuisance = FALSE),
  list(components = c("contcor1", "RM_Factor_1"),  isNuisance = FALSE),
  list(components = c("RM_Factor_1", "facGender"), isNuisance = FALSE)
)
options$posteriorEstimates <- TRUE
options$singleModelTerms <- list(list(components = "contcor1"), list(components = "facGender"))

set.seed(1)
results <- runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)

test_that("Model Comparison table results with interactions match", {
	table <- results[["results"]][["tableModelComparison"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 7.4011019942129, "contcor1<unicode><unicode><unicode>RM_Factor_1",
			 0.125, 0.513926086849954, "", 0.319927954089862, 1.37740768818149,
			 "Null model (incl. RM_Factor_1, facGender, contcor1, subject)",
			 0.125, 0.164419321519314, 25.2097891946442, 0.239255439868133,
			 0.981388423531221, "facGender<unicode><unicode><unicode>contcor1 + contcor1<unicode><unicode><unicode>RM_Factor_1",
			 0.125, 0.122959611968994, 34.233055481541, 0.159227760004727,
			 0.623871296288928, "facGender<unicode><unicode><unicode>RM_Factor_1 + RM_Factor_1<unicode><unicode><unicode>contcor1",
			 0.125, 0.0818312996171132, 34.1722466705555, 0.0850497581680873,
			 0.31994980441561, "facGender<unicode><unicode><unicode>contcor1 + facGender<unicode><unicode><unicode>RM_Factor_1 + contcor1<unicode><unicode><unicode>RM_Factor_1",
			 0.125, 0.04370928940286, 42.0099728869602, 0.0694789594226266,
			 0.259204786338546, "facGender<unicode><unicode><unicode>contcor1",
			 0.125, 0.0357070497344772, 34.7108255032327, 0.0557530731556876,
			 0.206487179651425, "facGender<unicode><unicode><unicode>RM_Factor_1",
			 0.125, 0.0286529587167617, 37.1425116715811, 0.017112153703716,
			 0.0621068668574807, "facGender<unicode><unicode><unicode>contcor1 + facGender<unicode><unicode><unicode>RM_Factor_1",
			 0.125, 0.00879438219052571, 37.9818481428213))
})

test_that("Model Averaged Posterior Summary table results with interactions match", {
	table <- results[["results"]][["tablePosteriorEstimates"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 0.719234259901028, 0.921104000380025, 0.100572386704361, 1.1203456740766,
			 "Intercept", "Level 1", -1.27224612405687, -1.09846291113912,
			 0.0862424425049142, -0.928799400426497, "RM_Factor_1", "Level 2",
			 0.925812907177538, 1.09846291113912, 0.0862424425049142, 1.26925963080791,
			 "", "f", -0.398255866332023, -0.204771680863285, 0.0968950249571478,
			 -0.0135542046150514, "facGender", "m", 0.0114404592210022, 0.204771680863285,
			 0.0968950249571478, 0.396142120937974, "", "", -0.211147243270715,
			 -0.0234696597689474, 0.0938837414182637, 0.164114708332537,
			 "contcor1", "f &amp; contcor1", -0.197666349533573, -0.0103632975300062,
			 0.0926887994083915, 0.173741376044989, "facGender<unicode><unicode><unicode>contcor1",
			 "m &amp; contcor1", -0.175839724777071, 0.0103632975300062,
			 0.0926887994083915, 0.195568000801491, "", "Level 1 &amp; f",
			 -0.175438316624131, -0.0141219719774659, 0.0801968484745378,
			 0.14453349565803, "RM_Factor_1<unicode><unicode><unicode>facGender",
			 "Level 1 &amp; m", -0.146415682789101, 0.0141219719774659, 0.0801968484745378,
			 0.173556129493059, "", "Level 2 &amp; f", -0.145880989401819,
			 0.0141219719774659, 0.0801968484745378, 0.174276368695733, "",
			 "Level 2 &amp; m", -0.176240524266883, -0.0141219719774659,
			 0.0801968484745378, 0.143916833830669, "", "Level 1 &amp; contcor1",
			 0.0208677860721955, 0.185156290606396, 0.0824454986811013, 0.349609596761152,
			 "RM_Factor_1<unicode><unicode><unicode>contcor1", "Level 2 &amp; contcor1",
			 -0.351014476294011, -0.185156290606396, 0.0824454986811013,
			 -0.0222726656050543, ""))
})


options <- initOpts("AnovaRepeatedMeasuresBayesian")
options$plotHorizontalAxis <- "Drink"
options$plotSeparateLines <- "Imagery"
options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg", "winepos", "wineneut", "wineneg", "waterpos", "waterneu", "waterneg")
options$repeatedMeasuresFactors <- list(list(levels = c("Beer", "Wine", "Water"), name = "Drink"),
    list(levels = c("Positive", "Neutral", "Negative"), name = "Imagery"))
options$modelTerms <- list(list(components = "Drink", isNuisance = FALSE), list(components = "Imagery",
    isNuisance = FALSE))
set.seed(1)
results <- runAnalysis("AnovaRepeatedMeasuresBayesian", "Alcohol Attitudes.csv", options)

test_that("Descriptives plot matches for beer data", {
  plotName <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["descriptivesContainer_containerDescriptivesPlots_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Descriptives plot")
})


options <- initOpts("AnovaRepeatedMeasuresBayesian")
options$plotTwoHorizontalAxis <- "Drink"
options$plotTwoSeparatePlots <- "Imagery"
options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg", "winepos", "wineneut", "wineneg", "waterpos", "waterneu", "waterneg")
options$repeatedMeasuresFactors <- list(list(levels = c("Beer", "Wine", "Water"), name = "Drink"),
                                        list(levels = c("Positive", "Neutral", "Negative"), name = "Imagery"))
options$modelTerms <- list(list(components = "Drink", isNuisance = FALSE), list(components = "Imagery", isNuisance = FALSE))
set.seed(1)
results <- runAnalysis("AnovaRepeatedMeasuresBayesian", "Alcohol Attitudes.csv", options)

test_that("Bar plot matches for beer data", {
  plotName <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_containerDescriptivesPlotsTwo"]][["collection"]][["descriptivesContainer_containerDescriptivesPlotsTwo_Imagery: Negative"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Bar plot")
})


test_that("Model Comparison table results matches for beer data", {
  table <- results[["results"]][["tableModelComparison"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(1, 1248.92657020971, "Drink + Imagery", 0.25, 0.997603693322448,
         "", 0.00240206275657685, 0.00720618826973056, "Imagery", 0.25,
         0.00239630667755337, 13.6259323546224, 8.3225913320555e-29,
         2.49079435526158e-28, "Drink", 0.25, 8.30264785087197e-29, 13.7458740674974,
         2.56126043451803e-29, 7.66536860710751e-29, "Null model (incl. subject)",
         0.25, 2.55512286903585e-29, 10.5326335209382))
})

options <- initOpts("AnovaRepeatedMeasuresBayesian")
options$repeatedMeasuresCells <- c("contNormal", "contGamma")
options$repeatedMeasuresFactors <- list(
  list(levels=c("Level 1", "Level 2"), name="RM_FACTOR_1")
)
options$betweenSubjectFactors <- c("facGender", "facFifty")
options$covariates <- "contcor1"
options$modelTerms <- list(
  list(components="RM_FACTOR_1", isNuisance=TRUE),
  list(components="facGender", isNuisance=FALSE),
  list(components="contcor1", isNuisance=FALSE),
  list(components=c("RM_FACTOR_1", "facGender", "facFifty"), isNuisance=FALSE)
)
set.seed(1)
results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)
test_that("Missing cells in interactions with between-subject factors returns an error", {
  expect_true(results[["results"]][["error"]], label = "Missing interaction cells check")
})

