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
  options$cauchyPriorScaleCovariates <- 0.3
  options$cauchyPriorScaleFixedEffects <- 0.8
  options$cauchyPriorScaleRandomEffects <- 0.8

  options$effects <- TRUE

  refTablesModelComparison <- list(
    nullModelTop = list(1, 2.7360776590455, "Null model (incl. RM_FACTOR_1, subject)",
                        0.166666666666667, 0.353677636088142, "", 1.09949701717398,
                        3.18153190745891, "facGender", 0.166666666666667, 0.388867505920057,
                        51.1951816404394, 0.265376367487422, 0.517897050553362, "facGender + contcor1",
                        0.166666666666667, 0.0938576863266096, 51.4695058179589, 0.221032049832103,
                        0.424017660389566, "contcor1", 0.166666666666667, 0.0781740928843347,
                        52.0576284917219, 0.204940340665398, 0.390735699647815, "facGender + facGender<unicode><unicode><unicode>RM_FACTOR_1",
                        0.166666666666667, 0.0724828152256365, 55.1288442196558, 0.0365877348037786,
                        0.0655495461795904, "facGender + contcor1 + facGender<unicode><unicode><unicode>RM_FACTOR_1",
                        0.166666666666667, 0.0129402635552203, 52.1705087277665),
    bestModelTop = list(1, 3.44623383882675, "facGender", 0.166666666666667, 0.4080201785303,
                        "", 0.830699972384786, 2.56363685958054, "Null model (incl. RM_FACTOR_1, subject)",
                        0.166666666666667, 0.338942351037555, 16.2541931666176, 0.310764448560518,
                        0.726053020125437, "facGender + contcor1", 0.166666666666667,
                        0.126798165782533, 40.2611287258608, 0.166422866647932, 0.364253679795511,
                        "contcor1", 0.166666666666667, 0.0679038877612134, 17.9732503022292,
                        0.117884109116722, 0.252647597074224, "facGender + facGender<unicode><unicode><unicode>RM_FACTOR_1",
                        0.166666666666667, 0.0480990952476901, 17.840977487766, 0.0250877828581434,
                        0.0517109379972277, "facGender + contcor1 + facGender<unicode><unicode><unicode>RM_FACTOR_1",
                        0.166666666666667, 0.0102363216407091, 22.7635933966214)
  )

  refTablesEffects <- list(
    allModels = list(0.657804789133694, "facGender", 0.333333333333333, 0.431851728972477,
                     0.666666666666667, 0.568148271027523, 0.226951776469056, "contcor1",
                     0.5, 0.815027957233835, 0.5, 0.184972042766165, 0.186803486506059,
                     "RM_FACTOR_1<unicode><unicode><unicode>facGender", 0.666666666666667,
                     0.914576921219143, 0.333333333333333, 0.0854230787808568),
    matchedModels = list(1.31454661075867, "facGender", 0.333333333333333, 0.406846238798769,
                         0.333333333333333, 0.534818344312832, 0.257764139015012, "contcor1",
                         0.5, 0.795061624815545, 0.5, 0.204938375184455, 0.109075198165374,
                         "RM_FACTOR_1<unicode><unicode><unicode>facGender", 0.333333333333333,
                         0.534818344312832, 0.333333333333333, 0.0583354168883992)
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
  options$postHocNullControl <- TRUE
  options$postHocTerms <- "RM_FACTOR_1"
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
  options$covariates = "contNormal"
  options$betweenSubjectFactors = "contBinom"
  options$effects <- TRUE
  options$modelTerms = list(list(components = "RM_FACTOR_1", isNuisance = FALSE),
                            list(components = "contBinom", isNuisance = FALSE),
                            list(components = "contNormal", isNuisance = FALSE),
                            list(components = c("RM_FACTOR_1", "contBinom"), isNuisance = FALSE))
  options$repeatedMeasuresCells = c("contcor1", "contcor2")
  options$repeatedMeasuresFactors = list(list(levels = c("Level 1", "Level 2"), name = "RM_FACTOR_1"))

  # NOTE: the option below makes BayesFactor return NaN as BF for models with covariates.
  # It's a nice hack to test how gracefully the analysis recovers when some but not all BFs could be computed.
  # A user can never enter NULL here. This hack exists for BayesFactor version 0.9.12.4.2.
  options$cauchyPriorScaleCovariates <- NULL

  set.seed(42)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresBayesian", "test.csv", options)

  mainTable <- results[["results"]][["tableModelComparison"]][["data"]]
  effectsTable <- results[["results"]][["tableEffects"]][["data"]]

  jaspTools::expect_equal_tables(
    mainTable,
    list(1, 4.4313455891584, "Null model (incl. subject)", 0.1, 0.329925662305594,
         "", 0.599537363783089, 2.21918597034008, "contBinom", 0.1, 0.197802761823086,
         9.94032969714356, 0.534017152576558, 1.92479563713616, "contNormal",
         0.1, 0.176185962746368, 7.09342268129428, 0.2866717055308, 0.940142146215704,
         "RM_FACTOR_1 + contBinom + contNormal", 0.1, 0.0945803523115235,
         89.1146599980095, 0.260387924249248, 0.845843177876812, "contBinom + contNormal",
         0.1, 0.0859086583643121, 16.9138557428623, 0.137300588278136,
         0.427035147522861, "RM_FACTOR_1", 0.1, 0.0452989875226119, 4.06526948567029,
         0.0861818995539587, 0.263391764006893, "RM_FACTOR_1 + contBinom",
         0.1, 0.028433620289094, 7.69455993848681, 0.0786393100001073,
         0.239725855547286, "RM_FACTOR_1 + contNormal", 0.1, 0.0259451264350403,
         9.83614392444758, 0.0349764654782601, 0.105069161461628, "RM_FACTOR_1 + contBinom + RM_FACTOR_1<unicode><unicode><unicode>contBinom",
         0.1, 0.0115396335380237, 11.206804420014, 0.0132733981156355,
         0.0395864704226359, "RM_FACTOR_1 + contBinom + contNormal + RM_FACTOR_1<unicode><unicode><unicode>contBinom",
         0.1, 0.00437923466434688, 24.1252708485483),
    label = "Table where some BFs are NaN")

  jaspTools::expect_equal_tables(
    effectsTable,
    list(0.177404256162168, "RM_FACTOR_1", 0.4, 0.78982304523936, 0.6,
         0.21017695476064, 0.488022932176249, "contBinom", 0.4, 0.577355739009614,
         0.6, 0.422644260990386, 0.631319599334467, "contNormal", 0.5,
         0.613000665478409, 0.5, 0.386999334521591, 0.0647055113160902,
         "RM_FACTOR_1<unicode><unicode><unicode>contBinom", 0.8, 0.98408113179763,
         0.2, 0.0159188682023706),
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
                      list("", 4.65188637563055, 6.16705595222654, 0.740132582204339, 7.6558851483433,
                           "Intercept", "Level 1", 0.732042615149815, 1.73150566705231,
                           0.454479238285032, 2.5434859267573, "RM.Factor.1", "Level 2",
                           -2.5434859267573, -1.73150566705231, 0.454479238285032, -0.732042615149815,
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
                      list(1, 67.6190442016055, "RM.Factor.1", 0.5, 0.985426786227713, "",
                           0.0147887331417832, 0.0147887331417832, "Null model (incl. subject)",
                           0.5, 0.0145732137722867, 14.706456405613))
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
		list(1, 5.66439542951694, "RM_Factor_1<unicode><unicode><unicode>contcor1",
		     0.125, 0.447269311910059, "", 0.378743051532363, 1.42764412954338,
		     "Null model (incl. RM_Factor_1, facGender, contcor1, subject)",
		     0.125, 0.169400144049596, 14.7865314367803, 0.352662286891772,
		     1.31092370320153, "facGender<unicode><unicode><unicode>contcor1 + RM_Factor_1<unicode><unicode><unicode>contcor1",
		     0.125, 0.157735018394711, 21.4838209370178, 0.230132594804382,
		     0.8031923184662, "RM_Factor_1<unicode><unicode><unicode>facGender + RM_Factor_1<unicode><unicode><unicode>contcor1",
		     0.125, 0.102931247326232, 21.6963106578729, 0.109981772407929,
		     0.36215525146447, "facGender<unicode><unicode><unicode>contcor1",
		     0.125, 0.0491914716675433, 22.7130581491231, 0.0712860124139316,
		     0.230538830665796, "RM_Factor_1<unicode><unicode><unicode>facGender",
		     0.125, 0.0318840457211911, 22.0029904355868, 0.0687557582233999,
		     0.222096367859111, "facGender<unicode><unicode><unicode>contcor1 + RM_Factor_1<unicode><unicode><unicode>facGender + RM_Factor_1<unicode><unicode><unicode>contcor1",
		     0.125, 0.0307523406704345, 20.7882237952499, 0.0242279538785166,
		     0.076685942927241, "facGender<unicode><unicode><unicode>contcor1 + RM_Factor_1<unicode><unicode><unicode>facGender",
		     0.125, 0.0108364202602328, 24.5741000826397))
})

test_that("Model Averaged Posterior Summary table results with interactions match", {
	table <- results[["results"]][["tablePosteriorEstimates"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 0.719150815408306, 0.921201061846532, 0.100569168582941, 1.12021569323217,
		     "Intercept", "Level 1", -1.27223565464035, -1.09850630296919,
		     0.0862976030498998, -0.928791757260916, "RM_Factor_1", "Level 2",
		     0.925805288588052, 1.09850630296919, 0.0862976030498998, 1.26924918596749,
		     "", "f", -0.400356999427258, -0.204605480881675, 0.0969736346990942,
		     -0.0135349887214958, "facGender", "m", 0.0114212072422293, 0.204605480881675,
		     0.0969736346990942, 0.398243217947991, "", "", -0.211229903376271,
		     -0.0235208539320508, 0.0938679101702486, 0.164029283009455,
		     "contcor1", "f &amp; contcor1", -0.197654247217559, -0.0103995720710264,
		     0.0927107092177786, 0.173764123304978, "facGender<unicode><unicode><unicode>contcor1",
		     "m &amp; contcor1", -0.175862532177987, 0.0103995720710264,
		     0.0927107092177786, 0.19555583834455, "", "Level 1 &amp; f",
		     -0.175421153124427, -0.0140209714545526, 0.0801391332324641,
		     0.144548036637595, "RM_Factor_1<unicode><unicode><unicode>facGender",
		     "Level 1 &amp; m", -0.146430208342077, 0.0140209714545526, 0.0801391332324641,
		     0.173538981419945, "", "Level 2 &amp; f", -0.147904471719736,
		     0.0140209714545526, 0.0801391332324641, 0.174180649355409, "",
		     "Level 2 &amp; m", -0.176144583020501, -0.0140209714545526,
		     0.0801391332324641, 0.145940538054643, "", "Level 1 &amp; contcor1",
		     0.019451782912395, 0.184838851451114, 0.082484011672465, 0.349619639549848,
		     "RM_Factor_1<unicode><unicode><unicode>contcor1", "Level 2 &amp; contcor1",
		     -0.35102460915256, -0.184838851451114, 0.082484011672465, -0.0208567525151077,
		     ""))
})


options <- initOpts("AnovaRepeatedMeasuresBayesian")
options$descriptivePlotHorizontalAxis <- "Drink"
options$descriptivePlotSeparateLines <- "Imagery"
options$descriptivePlotCi <- TRUE
options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg", "winepos", "wineneut", "wineneg", "waterpos", "waterneu", "waterneg")
options$repeatedMeasuresFactors <- list(list(levels = c("Beer", "Wine", "Water"), name = "Drink"),
    list(levels = c("Positive", "Neutral", "Negative"), name = "Imagery"))
options$modelTerms <- list(list(components = "Drink", isNuisance = FALSE), list(components = "Imagery",
    isNuisance = FALSE))
options$applyMoreyCorrectionErrorBars <- TRUE

set.seed(1)
results <- runAnalysis("AnovaRepeatedMeasuresBayesian", "Alcohol Attitudes.csv", options)

test_that("Descriptives plot matches for beer data", {
  plotName <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["descriptivesContainer_containerDescriptivesPlots_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Descriptives plot")
})

test_that("Model Comparison table results matches for beer data", {
  table <- results[["results"]][["tableModelComparison"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(1, 1137.7094021171, "Drink + Imagery", 0.25, 0.997370057619905,
         "", 0.00263687721523971, 0.00791063164571925, "Imagery", 0.25,
         0.00262994238010027, 7.47296949183367, 9.16454192941308e-29,
         2.7421319136596e-28, "Drink", 0.25, 9.14043971219877e-29, 6.88600800787927,
         2.92681065258173e-29, 8.75733992762396e-29, "Null model (incl. subject)",
         0.25, 2.91911330920799e-29, 4.29587018960634))
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

