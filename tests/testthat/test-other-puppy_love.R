context("Other: puppy_love")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("Ancova (analysis 1) results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "other", "puppy_love.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspAnova::Ancova(
          data = NULL,
          version = "0.96.8",
          formula = Happiness ~ Dose + `Puppy love`,
          covariates = "Puppy love",
          contrastCi = TRUE,
          contrastEffectSize = TRUE,
          contrasts = list(optionKey = "variable", types = "nominal", value = list(list(contrast = "simple", variable = "Dose"))),
          descriptivePlotHorizontalAxis = ~ `Puppy love`,
          descriptives = TRUE,
          effectSizeCi = TRUE,
          effectSizeEstimates = TRUE,
          effectSizeOmegaSquared = FALSE,
          effectSizePartialOmegaSquared = TRUE,
          marginalMeanTerms = ~ Dose,
          postHocCi = TRUE,
          postHocCorrectionSidak = TRUE,
          postHocTerms = ~ Dose,
          postHocTypeStandardEffectSize = TRUE,
          qqPlot = TRUE,
          restrictedModelComparisonReference = "complement",
          restrictedModels = list(list(name = "Model 1"))))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 2, 4.14192880386377, 12.592597104191, 0.0274465428639958,
     25.1851942083821, "Dose", 0.173186039799397, 0.406175871811225,
     0, "FALSE", 1, 4.95868113327519, 15.0757477099169, 0.034833380647441,
     15.0757477099169, "Puppy love", 0.116573465198452, 0.36417512535664,
     0, "TRUE", 26, "", 3.0402736745364, "", 79.0471155379463, "Residuals",
     "", "", ""))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_assumptionsContainer"]][["collection"]][["anovaContainer_assumptionsContainer_qqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_q-q-plot")

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_contrastContainer"]][["collection"]][["anovaContainer_contrastContainer_simpleContrast_Dose"]][["collection"]][["anovaContainer_contrastContainer_simpleContrast_Dose_contrastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "15 mins - No puppies", 0.849355306996752, 1.02411168845202,
     -0.0188584638132323, 2.06708184071727, 26, 1.78568011481422,
     0.0398052774148467, 0.0453535580857103, 2.1023947223315, 3.5315549522136,
     "FALSE", "30 mins - No puppies", 0.802810905470398, 1.27599951873095,
     0.262099735436294, 2.2898993020256, 26, 2.22488132183556, 0.574679871977611,
     0.0101750138508479, 2.77136409916095, 3.8750827716935))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots_Puppy love - Happiness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_puppy-love-happiness")

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_tableDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("No puppies", 3.22222222222222, 9, 1.7873008824606, 0.5957669608202,
     0.554679584211911, "15 mins", 4.875, 8, 1.45773797371133, 0.515388203202208,
     0.299023174094631, "30 mins", 4.84615384615385, 13, 2.11526806205714,
     0.586669804546028, 0.436483885821315))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_marginalMeansContainer"]][["collection"]][["anovaContainer_marginalMeansContainer_Dose"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "No puppies", 0.59620445705692, 1.70085425032354, 2.92637006325413,
     4.15188587618472, "FALSE", "15 mins", 0.620797059348148, 3.43598354715531,
     4.71205017806836, 5.9881168089814, "FALSE", "30 mins", 0.502632306147453,
     4.11807588299064, 5.15125138508969, 6.18442688718873))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_Dose1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.849355306996752, -1.02411168845202, -2.32251222677433,
     0.274288849870288, "No puppies", "15 mins", 26, -1.78568011481422,
     -3.89623786330156, 0.129983128349044, -2.1023947223315, 0.108926453684133,
     0.324877633673111, "FALSE", 0.802810905470398, -1.27599951873095,
     -2.538210159731, -0.0137888777308928, "No puppies", "30 mins",
     26, -2.22488132183556, -4.2197811748804, 0.0302155022603601,
     -2.77136409916095, 0.0266014195455914, -0.229981468790706, "FALSE",
     0.81122140296394, -0.251887830278926, -1.44577746567351, 0.942001805115662,
     "15 mins", "30 mins", 26, -0.439201207021331, -2.45500025326295,
     0.932499470707817, -0.541407321622225, 0.851713069300051, 1.57659783922029
    ))

})

test_that("Ancova (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "other", "puppy_love.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspAnova::Ancova(
          data = NULL,
          version = "0.96.8",
          formula = Happiness ~ Dose * `Puppy love`,
          covariates = "Puppy love",
          contrastCi = TRUE,
          contrasts = list(optionKey = "variable", types = "nominal", value = list(list(variable = "Dose"))),
          descriptivePlotHorizontalAxis = ~ `Puppy love`,
          descriptivePlotSeparateLines = ~ Dose,
          effectSizeCi = TRUE,
          effectSizeEstimates = TRUE,
          marginalMeanBootstrap = TRUE,
          restrictedModelComparisonReference = "complement",
          restrictedModels = list(list(name = "Model 1"))))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 2, 7.483568988423, 18.2787799846846, 0.00297956448464929,
     36.5575599693692, "Dose", 0.234213206998912, 0.474154807526832,
     0, "FALSE", 1, 7.03462486521665, 17.1822242014479, 0.0139474621264158,
     17.1822242014479, "Puppy love", 0.108997748403812, 0.365111631128032,
     0, "TRUE", 2, 4.18145584551366, 10.2132968285663, 0.0276671129121843,
     20.4265936571325, "Dose <unicode> Puppy love", 0.114927284314195,
     0.347288161766163, 0, "TRUE", 24, "", 2.44252174503391, "",
     58.6205218808138, "Residuals", "", "", ""))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots_Puppy love - Happiness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_puppy-love-happiness")

})

