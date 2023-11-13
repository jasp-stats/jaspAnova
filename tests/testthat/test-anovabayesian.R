context("Bayesian ANOVA")

# does not test
# - descriptives table (code is from regular ANOVA)
# - descriptives plot (code is from regular ANOVA)
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
    nullModelTop = list(1, 1.36400693663762, "Null model (incl. facExperim)",
                        0.2, 0.254288809233464, "", 2.02979323789675, 4.26708821602261, "facGender", 0.2, 0.5161537054549,
                        13.7355911669483, 0.395602125048142, 0.447395505374489,
                        "facGender + facFive", 0.2, 0.10059719330872, 9.32056569412749,
                        0.283048448817154, 0.310233602217543, "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                        0.2, 0.0719760530050931, 27.3699066120891, 0.224092594438576,
                        0.241710653647034, "facFive", 0.2, 0.0569842389978229, 12.4382643482552),
    bestModelTop = list(1, 5.89338824855042, "facGender", 0.2, 0.595689575754183, "",
                        0.387257922203485, 1.19943409151523, "Null model (incl. facExperim)",
                        0.2, 0.230685507384841, 20.3818205689193, 0.144085444036832,
                        0.375554724086775, "facGender + facFive", 0.2, 0.0858301970306536,
                        21.4710504386786, 0.0777010838721742, 0.194128270632499, "facFive",
                        0.2, 0.0462857256874556, 21.0528996183454, 0.0696822570553021,
                        0.173226431502077, "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                        0.2, 0.0415089941428668, 21.7314934303167)
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
    allModels = list(0.0999070274533343, "facFive", 0.4, 0.869670681084323, 0.6, 0.130329318915677,
                     0.182805120830142, "contBinom", 0.4, 0.784801421870848, 0.6,
                     0.215198578129152, 0.0164190701020935, "facFive<unicode><unicode><unicode>contBinom",
                     0.8, 0.995912012711941, 0.2, 0.0040879872880586),
    matchedModels = list(0.144492921488217, "facFive", 0.4, 0.870120605324455, 0.4, 0.125726268310426,
                         0.268259601459593, "contBinom", 0.4, 0.785207438988672, 0.4,
                         0.210639434646209, 0.182973144016899, "facFive<unicode><unicode><unicode>contBinom",
                         0.2, 0.0226980106147986, 0.2, 0.00415312636511864)
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

  options$dependent <- "contNormal"
  options$fixedFactors <- "debSame"
  options$modelTerms <- list(list(components="debSame", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "1-level factor check")

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
    list(1, 4.05523792179671, "facGender", 0.2, 0.503428695858085, "",
         0.523794362759307, 1.43251743302446, "Null model", 0.2, 0.263693112941734,
         0.00991710759730765, 0.244285054612465, 0.560899963009699, "facGender + contBinom",
         0.2, 0.122980106461174, 13.5725526116466, 0.144306160515237,
         0.313356099503816, "contBinom", 0.2, 0.072647862192473, 0.0243234777699117,
         0.0739930457937864, 0.15476595650871, "facGender + contBinom + facGender<unicode><unicode><unicode>contBinom",
         0.2, 0.0372502225465335, 20.2487932367166)
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
                                 list(1, 13.3659218116967, "facGender", 0.0526315789473684, 0.426128774149797,
                                      "", 0.523794362759307, 5.17210247921735, "Null model", 0.0526315789473684,
                                      0.223203849709198, 0.00991710759730765, 0.181670581429487, 1.51039873056377,
                                      "facGender + facExperim", 0.0526315789473684, 0.0774150621636282,
                                      14.3301775340925, 0.160926125807685, 1.32523271745499, "facGender + facFive",
                                      0.0526315789473684, 0.0685752527191049, 23.0898719232616, 0.116409805929351,
                                      0.939504896110524, "facExperim", 0.0526315789473684, 0.0496055678996901,
                                      0.0269112294974812), label = "Table with 5 rows")

  options$modelsShown <- "limited"
  options$numModelsShown <- 10

  results <- jaspTools::runAnalysis("AnovaBayesian", "test.csv", options)
  table <- results[["results"]][["tableModelComparison"]][["data"]]

  jaspTools::expect_equal_tables(table,
                                 list(1, 13.5401945213762, "facGender", 0.0526315789473684, 0.429299651661927,
                                      "", 0.523794362759307, 5.22175350578976, "Null model", 0.0526315789473684,
                                      0.224864737475051, 0.00991710759730765, 0.170863908644435, 1.42484787623232,
                                      "facGender + facExperim", 0.0526315789473684, 0.0733518164626514,
                                      7.64535614840947, 0.16705375323206, 1.39061999288634, "facGender + facFive",
                                      0.0526315789473684, 0.0717161180713408, 13.4935759252231, 0.116409805929351,
                                      0.946863619476086, "facExperim", 0.0526315789473684, 0.0499746891355029,
                                      0.0269112294974812, 0.0902952358051223, 0.725884830441666, "facGender + facExperim + facGender<unicode><unicode><unicode>facExperim",
                                      0.0526315789473684, 0.0387637132778706, 22.7829909603318, 0.0791076920342147,
                                      0.632786277848988, "facFive", 0.0526315789473684, 0.0339609046340674,
                                      0.00994061596714849, 0.0570470263884217, 0.451891785202431,
                                      "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                                      0.0526315789473684, 0.0244902685568982, 7.29922242520545, 0.0252985039010556,
                                      0.197637975104202, "facGender + facFive + facExperim", 0.0526315789473684,
                                      0.0108606389122911, 9.56328348447552, 0.0208881223923882, 0.16287125549643,
                                      "facGender + facFive + facExperim + facFive<unicode><unicode><unicode>facExperim",
                                      0.0526315789473684, 0.00896726366692393, 7.67806876653366), label = "Table with 10 rows")

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

  set.seed(123)
  result <- jaspTools::runAnalysis(name = "AnovaBayesian", options = options, dataset = "debug.csv")
  table <- result[["results"]][["containerSingleModel"]][["collection"]][["containerSingleModel_SMItablePosteriorEstimates"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list("", -0.396031836238149, -0.19346538433122, 0.105924716342584,
         0.0072248062850778, "Intercept", "f", -0.414794277186536, -0.209040764503124,
         0.10117577274577, -0.00976114711277226, "facGender", "m", 0.00976114711277226,
         0.209040764503124, 0.10117577274577, 0.414794277186536, "",
         1, -0.458061854289654, -0.119294134142763, 0.168176609000649,
         0.193397360192855, "facFive", 2, -0.456651110307801, -0.100193505564579,
         0.176506452100615, 0.233224162499312, "", 3, -0.0687243942234638,
         0.255394463800637, 0.179269286210147, 0.627323017115639, "",
         4, -0.443051405395219, -0.119113650358978, 0.164903560647659,
         0.189664348197634, "", 5, -0.238052982190886, 0.083206826265683,
         0.171198782366933, 0.420959595778805, "")
  )
})
