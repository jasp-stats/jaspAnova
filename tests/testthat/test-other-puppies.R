context("Other: puppies")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("Anova results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "puppies.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("Anova", dataset, opts)

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 2, 5.11864406779661, 10.0666666666667, 0.0246942895382226,
     20.1333333333333, "Dose", 0.460365853658537, 0.706565601103752,
     0.000745402224770925, 0.354485776805252, 0.637780996734387,
     0, 0.460365853658537, 0.706565601103752, 0.000745402224770925,
     0.354485776805252, 0.637780996734387, 0, "TRUE", 12, "", 1.96666666666667,
     "", 23.6, "Residuals", "", "", "", "", "", "", "", "", "", "",
     "", ""))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_assumptionsContainer"]][["collection"]][["anovaContainer_assumptionsContainer_qqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_q-q-plot")

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_contrastContainer"]][["collection"]][["anovaContainer_contrastContainer_customContrast_Dose"]][["collection"]][["anovaContainer_contrastContainer_customContrast_Dose_contrastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 1, 1.53622914957372, 1.35484066234336, 0.0179600088837908,
     2.69172131580292, 12, 3.8, 0.452844219599999, 0.0293002196554282,
     2.47358930863565, 7.1471557804, 0, 2, 0.886942313043338, 1.28353325906213,
     -0.20802934345753, 2.77509586158178, 12, 1.8, -0.132481290833552,
     0.0651922067570462, 2.02944427560764, 3.73248129083355))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_contrastContainer"]][["collection"]][["anovaContainer_contrastContainer_customContrast_Dose"]][["collection"]][["anovaContainer_contrastContainer_customContrast_Dose_customCoefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-2, 0, "No puppies", 1, -1, "15 mins", 1, 1, "30 mins"))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_")

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerRainCloudPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerRainCloudPlots_rainCloudPlotSingle"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_happiness")

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_tableDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("No puppies", 2.2, 5, 1.30384048104053, 0.58309518948453, 0.592654764109332,
     "15 mins", 3.2, 5, 1.30384048104053, 0.58309518948453, 0.407450150325166,
     "30 mins", 5, 5, 1.58113883008419, 0.707106781186548, 0.316227766016838
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_kruskalContainer"]][["collection"]][["anovaContainer_kruskalContainer_dunnContainer"]][["collection"]][["anovaContainer_kruskalContainer_dunnContainer_Dose"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "No puppies - 15 mins", 0.333666485082971, 0.333666485082971,
     -0.44, 4.8, 7.5, -0.966755079953234, 2, 2, 2, 0.0404663969628817,
     "No puppies - 30 mins", 0.0404663969628817, 0.0134887989876272,
     -0.84, 4.8, 11.7, -2.47059631543604, 0.397866752621599, "15 mins - 30 mins",
     0.265244501747733, 0.132622250873866, -0.64, 7.5, 11.7, -1.50384123548281
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_kruskalContainer"]][["collection"]][["anovaContainer_kruskalContainer_kruskalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 0.442857142857143, 0.35, "Dose", 0.187258226785718, 0.0518012645833377,
     0.0450492023935578, 6.2, 0.81804788213628, 0.787722529158993
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_marginalMeansContainer"]][["collection"]][["anovaContainer_marginalMeansContainer_Dose"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "No puppies", 0.627162924074226, 0.833529374735462, 2.2,
     3.56647062526454, "FALSE", "15 mins", 0.627162924074226, 1.83352937473546,
     3.2, 4.56647062526454, "FALSE", "30 mins", 0.627162924074226,
     3.63352937473546, 5, 6.36647062526454))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocDunnettContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocDunnettContainer_Dose"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.886942313043338, "15 mins - No puppies", -1.21963511399532,
     1, 0.445888579780105, 1.12746904200424, 3.21963511399532, 2,
     0.886942313043338, "30 mins - No puppies", 0.580364886004677,
     2.8, 0.015237702014807, 3.15691331761188, 5.01963511399532
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocGamesContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocGamesContainer_Dose"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.824621125123532, "No puppies - 15 mins", 8, -3.3563089273419,
     -1, 0.47896489393065, -1.21267812518166, 1.3563089273419, 2,
     0.916515138991168, "No puppies - 30 mins", 7.7199124726477,
     -5.43893919399357, -2.8, 0.0388414107946465, -3.05505046330389,
     -0.161060806006431, 0.916515138991168, "15 mins - 30 mins",
     7.7199124726477, -4.43893919399357, -1.8, 0.185393344481168,
     -1.96396101212393, 0.838939193993569))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_Dose1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.886942313043338, -0.713074032812292, -2.51692089898557,
     1.09077283336099, "No puppies", "15 mins", 12, -1, -3.36624115850688,
     -1.12746904200424, 0.516276123508474, 1.36624115850688, 2, "FALSE",
     0.886942313043338, -1.99660729187442, -4.0878753451391, 0.0946607613902677,
     "No puppies", "30 mins", 12, -2.8, -5.16624115850688, -3.15691331761188,
     0.0209243994922418, -0.433758841493123, "FALSE", 0.886942313043338,
     -1.28353325906213, -3.18629336625279, 0.619226848128532, "15 mins",
     "30 mins", 12, -1.8, -4.16624115850688, -2.02944427560764, 0.147457622995378,
     0.566241158506876))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_DoseLetterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("No puppies", " a ", "15 mins", " ab", "30 mins", "  b"))

})

