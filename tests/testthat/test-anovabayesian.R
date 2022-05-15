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
  options$priorFixedEffects <- 0.4
  options$priorRandomEffects <- 1.5
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
  options$postHocTestsNullControl <- TRUE
  options$postHocTestsVariables <- "facFive"
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
