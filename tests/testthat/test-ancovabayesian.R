context("Bayesian ANCOVA")

# does not test
# - descriptives table (code from regular ANOVA)
# - descriptives plot (code from regular ANOVA)
# - bar plot (code from regular ANOVA)
# - raincloud plot (code is from regular ANOVA)
# - bftype (01, 10)


test_that("Main table results match", {
  set.seed(0)
  options <- initOpts("AncovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facGender"
  options$randomFactors <- "facFive"
  options$covariates <- "contGamma"
  options$priorCovariates <- 0.3
  options$priorRandomEffects <- 1.2
  options$modelTerms <- list(
    list(components="facGender", isNuisance=FALSE),
    list(components="facFive", isNuisance=TRUE),
    list(components="contGamma", isNuisance=FALSE)
  )

  refTables <- list(
    nullModelTop = list(1, 1.15069866070698, "Null model (incl. facFive)",
                        0.25, 0.277230113474675, "", 1.83701195521646, 3.11340404608183, "facGender", 0.25, 0.509275032798994,
                        29.6656710072444, 0.502295526791326, 0.485338413495893,
                        "facGender + contGamma", 0.25, 0.139251445890181, 9.83211717425022,
                        0.267804268827858, 0.24059264108273, "contGamma", 0.25, 0.0742434078361496,
                        15.885810482332),
    bestModelTop = list(1, 3.52818212286609, "facGender", 0.25, 0.540453997217391, "",
                        0.485506904639836, 1.0672128467728, "Null model (incl. facFive)",
                        0.25, 0.262394147289242, 10.5414217188718, 0.230284849014787,
                        0.426450425012883, "facGender + contGamma", 0.25, 0.124458367148645,
                        14.932174588461, 0.134504488298719, 0.235176246789085, "contGamma",
                        0.25, 0.0726934883447225, 15.235554204653)
  )

  for (order in c("nullModelTop", "bestModelTop")) {
    options$bayesFactorOrder <- order
    results <- jaspTools::runAnalysis("AncovaBayesian", "test.csv", options)
    table <- results[["results"]][["tableModelComparison"]][["data"]]
    jaspTools::expect_equal_tables(table, refTables[[order]], label=paste("Table with order", order))
  }
})

test_that("Effects table results match", {
  set.seed(0)
  options <- initOpts("AncovaBayesian")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$fixedFactors <- "contBinom"
  options$effects <- TRUE
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE),
    list(components="contBinom", isNuisance=FALSE),
    list(components=c("contGamma", "contBinom"), isNuisance=FALSE)
  )

  refTables <- list(
    allModels = list(0.195147508378382, "contGamma", 0.4, 0.773561964946583, 0.6, 0.226438035053417,
                     0.215922578651091, "contBinom", 0.4, 0.755353263370717, 0.6,
                     0.244646736629283, 0.148059370442441, "contGamma<unicode><unicode><unicode>contBinom",
                     0.8, 0.964306352146872, 0.2, 0.0356936478531281),
    matchedModels = list(0.251463756140853, "contGamma", 0.4, 0.770552715646492, 0.4, 0.193766080181001,
                         0.281631799589681, "contBinom", 0.4, 0.752414848114898, 0.4,
                         0.211903947712595, 0.784742840790344, "contGamma<unicode><unicode><unicode>contBinom",
                         0.2, 0.0454686584162663, 0.2, 0.0356812041725066)
  )

  for (effectsType in c("allModels", "matchedModels")) {
    options$effectsType <- effectsType
    results <- jaspTools::runAnalysis("AncovaBayesian", "test.csv", options)
    table <- results[["results"]][["tableEffects"]][["data"]]
    jaspTools::expect_equal_tables(table, refTables[[effectsType]], label=paste("Table with effects type", effectsType))
  }
})

test_that("Post-hoc Comparisons table results match", {
  options <- jaspTools::analysisOptions("AncovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(
    list(components="facFive", isNuisance=FALSE)
  )
  options$postHocTestsNullControl <- TRUE
  options$postHocTestsVariables <- "facFive"
  options <- addCommonQMLoptions(options)

  results <- jaspTools::runAnalysis("AncovaBayesian", "test.csv", options)
  table <- results[["results"]][["collectionPosthoc"]][["collection"]][["collectionPosthoc_postHoc_facFive"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 2, 0.312140273346732, 0.0997312866050945, 0.319507910772894,
         0.00472785571557159, 1, 3, 0.81481028392952, 0.26033833149459,
         0.319507910772894, 0.0057924107781587, 1, 4, 0.30930072689011,
         0.0988240290491965, 0.319507910772894, 0.0047188266297084, 1,
         5, 0.435141737400649, 0.139031227406969, 0.319507910772894,
         0.00505249263395969, 2, 3, 0.940407207874983, 0.300467542263907,
         0.319507910772894, 0.00601473954955974, 2, 4, 0.309677154158431,
         0.0989443005392557, 0.319507910772894, 0.00472001987206125,
         2, 5, 0.474501390789671, 0.151606948030041, 0.319507910772894,
         0.00513972486890359, 3, 4, 0.743172685138961, 0.237449551972231,
         0.319507910772894, 0.00566275410175843, 3, 5, 0.327698986500113,
         0.104702418539046, 0.319507910772894, 0.00477566690656642, 4,
         5, 0.431922758615608, 0.138002738220538, 0.319507910772894,
         0.00504509537671419)
  )
})

test_that("Analysis handles errors", {
  # NOTE: only errors that are not handled in test-anovabayesian are tested

  options <- initOpts("AncovaBayesian")

  options$dependent <- "contNormal"
  options$fixedFactors <- list()
  options$covariates <- "debInf"
  options$modelTerms <- list(list(components="debInf", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("AncovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label="Inf covariate check")

  options$dependent <- "contNormal"
  options$covariates <- c("debEqual1", "debEqual2")
  options$modelTerms <- list(list(components="debEqual1", isNuisance=FALSE),
                             list(components="debEqual2", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Identical covariates check")
})


options <- initOpts("AncovaBayesian")
options$covariates <- "contcor1"
options$dependent <- "contNormal"
options$fixedFactors <- "facGender"
options$modelTerms <- list(list(components = "facGender", isNuisance = FALSE), list(components = "contcor1", isNuisance = FALSE))
options$plotCredibleInterval <- TRUE
options$plotHorizontalAxis <- "contcor1"
options$plotSeparateLines <- "facGender"
options$singleModelTerms <- list(list(components = "facGender"), list(components = "contcor1"))
set.seed(1)
results <- jaspTools::runAnalysis("AncovaBayesian", "debug.csv", options)


test_that("contcor1 - contNormal plot matches", {
  plotName <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["descriptivesContainer_containerDescriptivesPlots_contcor1 - contNormal"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contcor1-contnormal")
})

test_that("Model Comparison table results match", {
  table <- results[["results"]][["tableModelComparison"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(1, 1.79472415315199, "facGender", 0.25, 0.374312284883409, "",
                           0.798367847301607, 1.27861724000582, "facGender + contcor1",
                           0.25, 0.298838893100913, 12.1608731786381, 0.523794362759307,
                           0.731634131664583, "Null model", 0.25, 0.196062664733485, 0.00991710759730765,
                           0.349403860263177, 0.451394642565488, "contcor1", 0.25, 0.130786157282193,
                           0.010255738862123))
})

# test whether sampling parameters with interactions effects works
options <- initOpts("AncovaBayesian")
options$covariates <- "contcor1"
options$dependent <- "contcor2"
options$fixedFactors <- "facGender"
options$modelTerms <- list(
  list(components = "contcor1", isNuisance = FALSE),
  list(components = "facGender", isNuisance = FALSE),
  list(components = c("facGender", "contcor1"), isNuisance = FALSE)
)
options$posteriorEstimates <- TRUE
options$singleModelTerms <- list(list(components = "contcor1"), list(components = "facGender"))
set.seed(1)
results <- runAnalysis("AncovaBayesian", "test.csv", options)

test_that("Model Comparison table results with interactions match", {
	table <- results[["results"]][["tableModelComparison"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 7.06170555213835, "contcor1", 0.2, 0.638392110407715, "", 0.497219687920235,
			 1.860128626628, "contcor1 + facGender", 0.2, 0.317421125907664,
			 15.6333480655196, 0.0692157107595034, 0.18491798182066, "contcor1 + facGender + contcor1<unicode><unicode><unicode>facGender",
			 0.2, 0.0441867636651294, 18.3840890554849, 2.11947931806262e-11,
			 5.41223549936725e-11, "Null model", 0.2, 1.3530588748235e-11,
			 0.00566422819161401, 9.33499560957271e-12, 2.38375501915096e-11,
			 "facGender", 0.2, 5.95938754784188e-12, 0.0199269143160373
	))
})

test_that("Model Averaged Posterior Summary table results with interactions match", {
	table <- results[["results"]][["tablePosteriorEstimates"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", -0.085353148152257, 0.0695314166904569, 0.0767563813738564,
			 0.221601378816917, "Intercept", "", 0.474732635792994, 0.63207636384486,
			 0.0774072267458595, 0.784874979362299, "contcor1", "f", -0.0535917488388796,
			 0.0937763860288633, 0.0730456277677311, 0.238994337088761, "facGender",
			 "m", -0.240684705902945, -0.0937763860288633, 0.0730456277677311,
			 0.0514589886465098, "", "f", -0.133045698206248, 0.0170589375251757,
			 0.0747339434683253, 0.166183348317297, "contcor1<unicode><unicode><unicode>facGender",
			 "m", -0.167513255190735, -0.0170589375251757, 0.0747339434683253,
			 0.13171579133281, ""))
})
