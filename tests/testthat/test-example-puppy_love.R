context("Example: puppy_love")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Ancova (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "puppy_love.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("Ancova", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 2, 4.14192880386377, 12.592597104191, 0.0274465428639959,
     25.1851942083821, "jaspColumn1", 0.173186039799396, 0.406175875964937,
     0, "FALSE", 1, 4.9586811332752, 15.0757477099169, 0.0348333806474409,
     15.0757477099169, "jaspColumn2", 0.116573465198452, 0.364124832054005,
     0, "TRUE", 26, "", 3.0402736745364, "", 79.0471155379464, "Residuals",
     "", "", ""))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_assumptionsContainer"]][["collection"]][["anovaContainer_assumptionsContainer_qqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  if (jaspTools:::getOS() == "osx") {
    # done manually because of arbitrary fail on other OS'es in examples unit tests
    jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_q-q-plot")
  }

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_contrastContainer"]][["collection"]][["anovaContainer_contrastContainer_simpleContrast_jaspColumn1"]][["collection"]][["anovaContainer_contrastContainer_simpleContrast_jaspColumn1_contrastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "30 mins - 15 mins", 0.81122140296394, 0.251887830278927,
     -0.707131530899357, 1.21090719145721, 26, 0.439201207021332,
     -1.22828826802822, 0.592836293276488, 0.541407321622227, 2.10669068207088,
     "FALSE", "No puppies - 15 mins", 0.849355306996752, -1.02411168845202,
     -2.06708184071727, 0.0188584638132328, 26, -1.78568011481422,
     -3.5315549522136, 0.0453535580857104, -2.1023947223315, -0.0398052774148461
    ))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots_jaspColumn2 - jaspColumn3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_jaspcolumn2-jaspcolumn3")

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_tableDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.875, 8, 1.45773797371133, 0.515388203202208, 0.299023174094631,
     "15 mins", 4.84615384615385, 13, 2.11526806205714, 0.586669804546028,
     0.436483885821315, "30 mins", 3.22222222222222, 9, 1.7873008824606,
     0.5957669608202, 0.554679584211911, "No puppies"))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_marginalMeansContainer"]][["collection"]][["anovaContainer_marginalMeansContainer_jaspColumn1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.620797059348148, "15 mins", 3.43598354715531, 4.71205017806836,
     5.9881168089814, "FALSE", 0.502632306147453, "30 mins", 4.11807588299064,
     5.15125138508969, 6.18442688718874, "FALSE", 0.59620445705692,
     "No puppies", 1.70085425032354, 2.92637006325413, 4.15188587618472
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_jaspColumn11"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.81122140296394, -0.251887830278927, -1.44577746567352,
     0.942001805115661, "15 mins", "30 mins", 26, -0.439201207021332,
     -2.45500025326295, 0.932499470707816, -0.541407321622227, 0.85171306930005,
     1.57659783922029, "FALSE", 0.849355306996752, 1.02411168845202,
     -0.274288849870288, 2.32251222677433, "15 mins", "No puppies",
     26, 1.78568011481422, -0.324877633673112, 0.129983128349044,
     2.1023947223315, 0.108926453684133, 3.89623786330156, "FALSE",
     0.802810905470398, 1.27599951873095, 0.0137888777308925, 2.538210159731,
     "30 mins", "No puppies", 26, 2.22488132183556, 0.229981468790706,
     0.0302155022603601, 2.77136409916095, 0.0266014195455914, 4.21978117488041
    ))

})

test_that("Ancova (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "puppy_love.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("Ancova", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 2, 7.483568988423, 18.2787799846846, 0.00297956448464929,
     36.5575599693692, "jaspColumn1", 0.234213206998912, 0.474154790792245,
     0, "FALSE", 1, 7.03462486521666, 17.1822242014479, 0.0139474621264158,
     17.1822242014479, "jaspColumn2", 0.108997748403812, 0.365938525175063,
     0, "TRUE", 2, 4.18145584551367, 10.2132968285663, 0.0276671129121842,
     20.4265936571325, "jaspColumn1 <unicode> jaspColumn2", 0.114927284314195,
     0.347288170029439, 0, "TRUE", 24, "", 2.44252174503391, "",
     58.6205218808138, "Residuals", "", "", ""))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots_jaspColumn2 - jaspColumn3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_jaspcolumn2-jaspcolumn3")

})

