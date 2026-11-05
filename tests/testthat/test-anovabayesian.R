context("Bayesian ANOVA")

# does not test
# - descriptives table (code is from regular ANOVA)
# - descriptives plot (code is from regular ANOVA)
# - bar plot (code is from regular ANOVA)
# - raincloud plot (code is from regular ANOVA)
# - bftype (01, 10)

test_that("Main table results match", {
  set.seed(0)
  options <- initOpts("AnovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facGender", "facFive")
  options$randomFactors <- "facExperim"
  options$cauchyPriorScaleFixedEffects <- 0.4
  options$cauchyPriorScaleRandomEffects <- 1.5
  options$modelTerms <- list(
    list(components="facGender", isNuisance=FALSE),
    list(components="facFive", isNuisance=FALSE),
    list(components="facExperim", isNuisance=TRUE),
    list(components=c("facGender", "facFive"), isNuisance=FALSE)
  )

  refTables <- list(
    nullModelTop = list(1, 1.41515454399297, "Null model (incl. facExperim)", 0.2, 0.261332254231377,
                        "", 2.00332261201345, 4.39512167635538, "facGender", 0.2, 0.523532814150165,
                        9.62720372546739, 0.401181393133429, 0.468483085242258, "facGender + facFive",
                        0.2, 0.104841637823243, 6.2498241589592, 0.222705808690043,
                        0.247187190721878, "facFive", 0.2, 0.0582002110153907, 10.0625720222831,
                        0.199336599047212, 0.219823621216276, "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                        0.2, 0.052093082779824, 11.3817252921811),
    bestModelTop = list(1, 4.21639241016948, "facGender", 0.2, 0.513168334675791, "",
                        0.557229895101266, 1.60187008473189, "Null model (incl. facExperim)",
                        0.2, 0.285952737300683, 9.16694377991762, 0.170933572813501,
                        0.384607688225024, "facGender + facFive", 0.2, 0.0877176969008876,
                        11.1751652460885, 0.111306827359461, 0.242317525409106, "facFive",
                        0.2, 0.0571191392341002, 10.5510401181212, 0.109208008564957,
                        0.237477079886579, "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                        0.2, 0.0560420918885384, 27.3111684178124)
  )

  for (order in c("nullModelTop", "bestModelTop")) {
    options$bayesFactorOrder <- order
    results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
    table <- results[["results"]][["tableModelComparison"]][["data"]]
    jaspTools::expect_equal_tables(table, refTables[[order]], label=paste("Table with order", order))
  }
})

test_that("Effects table results match", {
  set.seed(0)
  options <- initOpts("AnovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- list("facFive", "contBinom")
  options$effects <- TRUE
  options$modelTerms <- list(
    list(components="facFive", isNuisance=FALSE),
    list(components="contBinom", isNuisance=FALSE),
    list(components=c("facFive", "contBinom"), isNuisance=FALSE)
  )

  refTables <- list(
    allModels = list(0.100079365029265, "facFive", 0.4, 0.869475209662443, 0.6, 0.130524790337557,
                     0.182996095173109, "contBinom", 0.4, 0.784625026078738, 0.6,
                     0.215374973921262, 0.0167308526719061, "facFive<unicode><unicode><unicode>contBinom",
                     0.8, 0.995834709049331, 0.2, 0.0041652909506687),
    matchedModels = list(0.144917151448562, "facFive", 0.4, 0.86976502191077, 0.4, 0.126043869404905,
                         0.268729708096219, "contBinom", 0.4, 0.784886556183765, 0.4,
                         0.21092233513191, 0.181766000786924, "facFive<unicode><unicode><unicode>contBinom",
                         0.2, 0.0230577152282617, 0.2, 0.00419110868432489)
  )

  for (effectsType in c("allModels", "matchedModels")) {
    options$effectsType <- effectsType
    results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
    table <- results[["results"]][["tableEffects"]][["data"]]
    jaspTools::expect_equal_tables(table, refTables[[effectsType]], label=paste("Table with effects type", effectsType))
  }
})

test_that("Post-hoc Comparisons table results match", {
  options <- jaspTools::analysisOptions("AnovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(
    list(components="facFive", isNuisance=FALSE)
  )
  options$postHocNullControl <- TRUE
  options$postHocTerms <- "facFive"
  options <- addCommonQMLoptions(options)

  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
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
  options <- initOpts("AnovaBayesian")
  options$dependent <- "debInf"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Inf check")

  # the form refuses a factor with fewer than 2 levels, so the analysis never gets to see it
  options$dependent <- "contNormal"
  options$fixedFactors <- "debSame"
  options$modelTerms <- list(list(components="debSame", isNuisance=FALSE))
  expect_error(jaspTools::runAnalysis("AnovaBayesian", "test.csv", options),
               "Minimum number of levels is 2. Variable debSame has only 1 levels", fixed = TRUE,
               label = "1-level factor check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=TRUE))
  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["tableModelComparison"]][["error"]][["type"]], "badData",
                   label="All nuisance check")

  # options$dependent <- "debSame"
  # options$fixedFactors <- "facFive"
  # options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  # results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="No variance check")

  options$dependent <- "debMiss99"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Too few obs check")

  options$dependent <- "contGamma"
  options$fixedFactors <- c("facFive", "facFifty")
  options$modelTerms <- list(list(components=c("facFive", "facFifty")))
  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Missing interaction cells check")

})

test_that("Model Comparison table results match", {
  options <- initOpts("AnovaBayesian")

  options$dependent <- "contNormal"
  options$fixedFactors <- c("contBinom", "facGender")
  options$modelTerms <- list(
    list(components = c("contBinom", "facGender"), isNuisance = FALSE),
    list(components = "facGender", isNuisance = FALSE),
    list(components = "contBinom", isNuisance = FALSE)
  )

  set.seed(1)
  results <- runAnalysis("AnovaBayesian", "debug.csv", options)
  table <- results[["results"]][["tableModelComparison"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(1, 3.98360985147217, "facGender", 0.2, 0.49897351268206, "", 0.523794362759306,
         1.41535438550996, "Null model", 0.2, 0.261359513109072, 0.00991710756335802,
         0.249082073395591, 0.567697973841935, "facGender + contBinom",
         0.2, 0.124285357108329, 9.34129103306445, 0.144306160515246,
         0.310367827736588, "contBinom", 0.2, 0.0720049518139535, 0.0243234781232945,
         0.0869317993522923, 0.181374063176413, "facGender + contBinom + facGender<unicode><unicode><unicode>contBinom",
         0.2, 0.0433766652865853, 12.6774365252886)
  )
})

test_that("Model prior changes posterior model probabilities", {

  options <- initOpts("AnovaBayesian")

  options$dependent <- "contNormal"
  options$fixedFactors <- c("contBinom", "facGender")
  options$modelTerms <- list(
    list(components = c("contBinom", "facGender"), isNuisance = FALSE),
    list(components = "facGender", isNuisance = FALSE),
    list(components = "contBinom", isNuisance = FALSE)
  )
  options$effects <- TRUE

  # so that the priors actually differ
  options[["bernoulliParameter"]] <- .3
  options[["betaBinomialParameterA"]] <- 1.1
  options[["betaBinomialParameterB"]] <- 1.1
  options[["wilsonParameterLambda"]] <- 0.8
  options[["castilloParameterU"]] <- 2.1
  options[["customPriorSpecification"]] <- list(list(components = "contBinom",                 inclusionProbability = 0.1, scaleFixedEffects = 0.5),
                                                list(components = "facGender",                 inclusionProbability = 0.2, scaleFixedEffects = 0.5),
                                                list(components = c("contBinom", "facGender"), inclusionProbability = 0.3, scaleFixedEffects = 0.5))

  # set to TRUE to regenerate the reference object, set to FALSE to test
  createReference <- FALSE

  referencePath <- testthat::test_path("BANOVA_modelPriors.rds")
  reference <- if (createReference) list() else readRDS(referencePath)

  modelPriors <- c("uniform", "betaBinomial", "Wilson", "Castillo", "Bernoulli", "custom")
  for (modelPrior in modelPriors) {

    options$modelPrior <- modelPrior

    set.seed(1)
    results <- runAnalysis("AnovaBayesian", "debug.csv", options)
    tableModelComparison <- results[["results"]][["tableModelComparison"]][["data"]]
    tableEffects <- results[["results"]][["tableEffects"]][["data"]]

    if (createReference) {
      reference[[modelPrior]][["modelComparison"]]  <- jaspTools:::collapseTestTable(tableModelComparison)
      reference[[modelPrior]][["posteriorSummary"]] <- jaspTools:::collapseTestTable(tableEffects)
    } else {
      jaspTools::expect_equal_tables(tableModelComparison, reference[[modelPrior]][["modelComparison"]])
      jaspTools::expect_equal_tables(tableEffects,         reference[[modelPrior]][["posteriorSummary"]])

      # internal consistency check
      BFM              <- vapply(tableModelComparison, `[[`, "BFM",       FUN.VALUE = numeric(1))
      prior_model_prob <- vapply(tableModelComparison, `[[`, "P(M)",      FUN.VALUE = numeric(1))
      post_model_prob  <- vapply(tableModelComparison, `[[`, "P(M|data)", FUN.VALUE = numeric(1))
      testthat::expect_equal(BFM, post_model_prob / (1 - post_model_prob) * (1 - prior_model_prob) / prior_model_prob)
    }
  }
  if (createReference) saveRDS(reference, referencePath)
})

test_that("Changing the number of models shown affects the main table", {
  set.seed(0)
  options <- initOpts("AnovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facGender", "facFive", "facExperim")
  options$modelTerms <- list(
    list(components="facGender", isNuisance=FALSE),
    list(components="facFive", isNuisance=FALSE),
    list(components="facExperim", isNuisance=FALSE),
    list(components=c("facGender", "facFive"), isNuisance=FALSE),
    list(components=c("facGender", "facExperim"), isNuisance=FALSE),
    list(components=c("facExperim", "facFive"), isNuisance=FALSE),
    list(components=c("facGender", "facExperim", "facFive"), isNuisance=FALSE)
  )

  options$modelsShown <- "limited"
  options$numModelsShown <- 5

  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  table <- results[["results"]][["tableModelComparison"]][["data"]]

  jaspTools::expect_equal_tables(table,
                                 list(1, 13.6661765127333, "facGender", 0.0526315789473684, 0.431570148901241,
                                      "", 0.523794362759306, 5.25743689979335, "Null model", 0.0526315789473684,
                                      0.226054011129664, 0.00991710756335802, 0.196172646097278, 1.66487251183623,
                                      "facGender + facExperim", 0.0526315789473684, 0.0846622580865527,
                                      4.83840274423195, 0.151971369302919, 1.26341632705466, "facGender + facFive",
                                      0.0526315789473684, 0.0655863064787861, 16.1935523992912, 0.11640980592936,
                                      0.952136325266813, "facExperim", 0.0526315789473684, 0.0502389972784985,
                                      0.0269112302981547), label = "Table with 5 rows")

  options$modelsShown <- "limited"
  options$numModelsShown <- 10

  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  table <- results[["results"]][["tableModelComparison"]][["data"]]

  jaspTools::expect_equal_tables(table,
                                 list(1, 13.576199302588, "facGender", 0.0526315789473684, 0.429950393094818,
                                      "", 0.523794362759306, 5.23196943353669, "Null model", 0.0526315789473684,
                                      0.225205592169213, 0.00991710756335802, 0.179507679339473, 1.50541627186094,
                                      "facGender + facExperim", 0.0526315789473684, 0.0771793972955449,
                                      5.30569163685654, 0.130968411022334, 1.07405871663334, "facGender + facFive",
                                      0.0526315789473684, 0.0563099198020563, 4.48075282321566, 0.11640980592936,
                                      0.948374516300686, "facExperim", 0.0526315789473684, 0.0500504418194199,
                                      0.0269112302981547, 0.0791076920342146, 0.633779241651161, "facFive",
                                      0.0526315789473684, 0.0340123832869344, 0.00994061593328108,
                                      0.075001737379914, 0.599787851159487, "facGender + facExperim + facGender<unicode><unicode><unicode>facExperim",
                                      0.0526315789473684, 0.0322470264692883, 16.5812748893767, 0.0682047005422858,
                                      0.543789923102464, "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                                      0.0526315789473684, 0.0293246378090701, 10.5782850221197, 0.0376738195313978,
                                      0.296362160088652, "facGender + facFive + facExperim", 0.0526315789473684,
                                      0.0161978735169077, 25.4648244834012, 0.0291007338673954, 0.228067243532046,
                                      "facGender + facFive + facExperim + facFive<unicode><unicode><unicode>facExperim",
                                      0.0526315789473684, 0.0125118719656343, 13.5371705199621), label = "Table with 10 rows")

})

test_that("Single model inference works", {
  options <- initOpts("AnovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facGender", "facFive")
  options$modelTerms <- list(
    list(components="facGender", isNuisance=FALSE),
    list(components="facFive", isNuisance=FALSE),
    list(components=c("facGender", "facFive"), isNuisance=FALSE)
  )

  # single model inference for full model without interaction effect
  options$singleModelEstimates <- TRUE
  options$singleModelTerms <- list(
    list(components="facGender"),
    list(components="facFive")
  )

  # increase samples to reduce cross-platform MCMC variability
  options$samplingMethodNumericAccuracy <- "manual"
  options$samplesNumericAccuracy <- 10000
  options$samplingMethodMCMC <- "manual"
  options$samplesMCMC <- 10000

  set.seed(123)
  result <- jaspTools::runAnalysis(name = "AnovaBayesian", options = options, dataset = "debug.csv")
  table <- result[["results"]][["containerSingleModel"]][["collection"]][["containerSingleModel_SMItablePosteriorEstimates"]][["data"]]
  if (jaspTools:::getOS() == "osx") {
    # done manually because of arbitrary fail on other OS'es due to rng difference
    options("jaspRoundToPrecision" = function(x) signif(round(x, digits = 1), digits = 1))
    jaspTools::expect_equal_tables(
      table,
      list("", -0.399577590248293, -0.193647203983604, 0.103205982987116,
           0.00515144666288427, "Intercept", "f", -0.403989233769818, -0.211124132123576,
           0.0983853799928718, -0.0208809717530841, "facGender", "m", 0.0208809717530841,
           0.211124132123576, 0.0983853799928718, 0.403989233769818, "",
           1, -0.444210060945971, -0.108868847520005, 0.170061236519146,
           0.220416140465156, "facFive", 2, -0.436867967448596, -0.0940399877297971,
           0.170700698822492, 0.238716680028786, "", 3, -0.0876642849855344,
           0.244896113150637, 0.174113658291136, 0.600882611866599, "",
           4, -0.448227689258666, -0.111349745748732, 0.167269094498603,
           0.216748130608182, "", 5, -0.261709005903208, 0.0693624678478975,
           0.168565284210541, 0.410409500770714, "", "f &amp; 1", -0.246622570094825,
           0.0605690167463655, 0.158354543988497, 0.378545685470656, "facGender<unicode><unicode><unicode>facFive",
           "f &amp; 2", -0.367453069636032, -0.0471197810565586, 0.159013068429516,
           0.26018013047388, "", "f &amp; 3", -0.328774363368925, -0.0134836189824268,
           0.1562283985419, 0.298350961040127, "", "f &amp; 4", -0.0950744398872126,
           0.213351974934343, 0.169573820961159, 0.574737940629873, "",
           "f &amp; 5", -0.573488035668518, -0.213317591641723, 0.170066072132675,
           0.100343232984715, "", "m &amp; 1", -0.378545685470656, -0.0605690167463655,
           0.158354543988497, 0.246622570094825, "", "m &amp; 2", -0.26018013047388,
           0.0471197810565586, 0.159013068429516, 0.367453069636032, "",
           "m &amp; 3", -0.298350961040127, 0.0134836189824268, 0.1562283985419,
           0.328774363368925, "", "m &amp; 4", -0.574737940629873, -0.213351974934343,
           0.169573820961159, 0.0950744398872126, "", "m &amp; 5", -0.100343232984715,
           0.213317591641723, 0.170066072132675, 0.573488035668518, ""
          )
    )
    options("jaspRoundToPrecision" = NULL) # reset default
  }

})
