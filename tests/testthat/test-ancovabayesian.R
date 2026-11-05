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
  options$cauchyPriorScaleCovariates <- 0.3
  options$cauchyPriorScaleRandomEffects <- 1.2
  options$modelTerms <- list(
    list(components="facGender", isNuisance=FALSE),
    list(components="facFive", isNuisance=TRUE),
    list(components="contGamma", isNuisance=FALSE)
  )

  refTables <- list(
    nullModelTop = list(1, 1.14531949734133, "Null model (incl. facFive)", 0.25, 0.276292212958712,
                        "", 1.85402872872699, 3.15073861584734, "facGender", 0.25, 0.512253700349008,
                        25.5120643350272, 0.505639231132807, 0.487172595377931, "facGender + contGamma",
                        0.25, 0.139704182128425, 6.4664439617243, 0.259688479076234,
                        0.231887628937358, "contGamma", 0.25, 0.0717499045638547, 10.2089175510104
                       ),
    bestModelTop = list(1, 3.16608016113432, "facGender", 0.25, 0.513467239866678, "",
                        0.54568894852719, 1.16778617007175, "Null model (incl. facFive)",
                        0.25, 0.280193398226006, 4.51626070078255, 0.254990054887903,
                        0.451962080155266, "facGender + contGamma", 0.25, 0.130929039676744,
                        8.30742467306245, 0.146864914400678, 0.244682557172278, "contGamma",
                        0.25, 0.075410322230572, 13.0657497077892)
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
    allModels = list(0.193768288580985, "contGamma", 0.4, 0.774801933139485, 0.6, 0.225198066860515,
                     0.214510111095262, "contBinom", 0.4, 0.75656404423175, 0.6,
                     0.24343595576825, 0.13701121749157, "contGamma<unicode><unicode><unicode>contBinom",
                     0.8, 0.966881593911982, 0.2, 0.0331184060880176),
    matchedModels = list(0.244034500031783, "contGamma", 0.4, 0.775131799569, 0.4, 0.189158901166557,
                         0.274023452298221, "contBinom", 0.4, 0.756886146009373, 0.4,
                         0.207404554726184, 0.893174432087777, "contGamma<unicode><unicode><unicode>contBinom",
                         0.2, 0.0399802076521299, 0.2, 0.0357092992644426)
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
  options$postHocNullControl <- TRUE
  options$postHocTerms <- "facFive"
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
  options$covariates <- "contInf"
  options$modelTerms <- list(list(components="contInf", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("AncovaBayesian", testDataWithInf(), options)
  expect_true(results[["results"]][["error"]], label="Inf covariate check")

  options$dependent <- "contNormal"
  options$covariates <- c("debEqual1", "debEqual2")
  options$modelTerms <- list(list(components="debEqual1", isNuisance=FALSE),
                             list(components="debEqual2", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("AncovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Identical covariates check")
})


options <- initOpts("AncovaBayesian")
options$covariates <- "contcor1"
options$dependent <- "contNormal"
options$fixedFactors <- "facGender"
options$modelTerms <- list(list(components = "facGender", isNuisance = FALSE), list(components = "contcor1", isNuisance = FALSE))
options$descriptivePlotCi <- TRUE
options$descriptivePlotHorizontalAxis <- "contcor1"
options$descriptivePlotSeparateLines <- "facGender"
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
                      list(1, 1.77992459957487, "facGender", 0.25, 0.372375036989742, "",
                           0.812266431332279, 1.30087636430968, "facGender + contcor1",
                           0.25, 0.302467742412883, 8.41778512659775, 0.523794362759306,
                           0.726930047744762, "Null model", 0.25, 0.195047945207515, 0.00991710756335802,
                           0.349403860263176, 0.44870903336107, "contcor1", 0.25, 0.130109275389859,
                           0.0102557388293021))
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
results <- jaspTools::runAnalysis("AncovaBayesian", "test.csv", options)

test_that("Model Comparison table results with interactions match", {
	table <- results[["results"]][["tableModelComparison"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 6.83513044561752, "contcor1", 0.2, 0.630830471301074, "", 0.518032178524469,
		     1.94168665317185, "contcor1 + facGender", 0.2, 0.326790483327713,
		     10.5636789477548, 0.0671797690186834, 0.177018036818248, "contcor1 + facGender + contcor1<unicode><unicode><unicode>facGender",
		     0.2, 0.0423790453519534, 12.3879233719147, 2.11947931806262e-11,
		     5.3481285485768e-11, "Null model", 0.2, 1.33703213712632e-11,
		     0.00566422819163981, 9.33499560957277e-12, 2.35551987200598e-11,
		     "facGender", 0.2, 5.88879967998025e-12, 0.0199269140350074
		    ))
})

test_that("Model Averaged Posterior Summary table results with interactions match", {
	table <- results[["results"]][["tablePosteriorEstimates"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", -0.085367293132575, 0.0695619231257589, 0.0768133377588807,
		     0.22155903520386, "Intercept", "", 0.474735612915735, 0.632326517543571,
		     0.0774179767694601, 0.784856020539753, "contcor1", "f", -0.0535463890600731,
		     0.0937215606551965, 0.0730587051971681, 0.239032919889055, "facGender",
		     "m", -0.240703829015233, -0.0937215606551965, 0.0730587051971681,
		     0.0514584043482767, "", "f", -0.133036208797364, 0.0171537750324805,
		     0.0747052450812807, 0.166174510541641, "contcor1<unicode><unicode><unicode>facGender",
		     "m", -0.167504335960925, -0.0171537750324805, 0.0747052450812807,
		     0.13170638337808, ""))
})
