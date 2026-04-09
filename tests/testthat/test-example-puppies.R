context("Example: puppies")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Anova results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "puppies.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("Anova", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 2, 5.11864406779661, 10.0666666666667, 0.0246942895382226,
     20.1333333333333, "jaspColumn1", 0.460365853658537, 0.706565600023874,
     0.000745396906660091, 0.354485776805252, 0.637780940760763,
     0, 0.460365853658537, 0.706565600023874, 0.000745396906660091,
     0.354485776805252, 0.637780940760763, 0, "TRUE", 12, "", 1.96666666666667,
     "", 23.6, "Residuals", "", "", "", "", "", "", "", "", "", "",
     "", ""))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_assumptionsContainer"]][["collection"]][["anovaContainer_assumptionsContainer_qqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_q-q-plot")

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_contrastContainer"]][["collection"]][["anovaContainer_contrastContainer_customContrast_jaspColumn1"]][["collection"]][["anovaContainer_contrastContainer_customContrast_jaspColumn1_contrastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 1, 1.53622914957372, 0.570459226249834, -1.82975734175204,
     2.97067579425171, 12, 0.799999999999999, -2.5471557804, 0.612011229409547,
     0.520755643923295, 4.1471557804, 0, 2, 0.886942313043338, -1.99660729187442,
     -3.6359401790344, -0.35727440471444, 12, -2.8, -4.73248129083355,
     0.00826810286112651, -3.15691331761188, -0.867518709166448
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_contrastContainer"]][["collection"]][["anovaContainer_contrastContainer_customContrast_jaspColumn1"]][["collection"]][["anovaContainer_contrastContainer_customContrast_jaspColumn1_customCoefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-2, 0, "15 mins", 1, -1, "30 mins", 1, 1, "No puppies"))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_")

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerRainCloudPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerRainCloudPlots_rainCloudPlotSingle"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_jaspcolumn2")

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_tableDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3.2, 5, 1.30384048104053, 0.58309518948453, 0.407450150325166,
     "15 mins", 5, 5, 1.58113883008419, 0.707106781186548, 0.316227766016838,
     "30 mins", 2.2, 5, 1.30384048104053, 0.58309518948453, 0.592654764109332,
     "No puppies"))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_kruskalContainer"]][["collection"]][["anovaContainer_kruskalContainer_dunnContainer"]][["collection"]][["anovaContainer_kruskalContainer_dunnContainer_jaspColumn1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.397866752621599, "15 mins - 30 mins", 0.265244501747733, 0.132622250873866,
     0.64, 7.5, 11.7, -1.50384123548281, 1, "15 mins - No puppies",
     0.333666485082971, 0.333666485082971, -0.44, 7.5, 4.8, 0.966755079953234,
     2, 2, 2, 0.0404663969628817, "30 mins - No puppies", 0.0404663969628817,
     0.0134887989876272, -0.84, 11.7, 4.8, 2.47059631543604))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_kruskalContainer"]][["collection"]][["anovaContainer_kruskalContainer_kruskalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 0.442857142857143, 0.35, "jaspColumn1", 0.16681314955957, 0.0214984715525216,
     0.0450492023935578, 6.2, 0.787644385009867, 0.773233171256257
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_marginalMeansContainer"]][["collection"]][["anovaContainer_marginalMeansContainer_jaspColumn1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.627162924074226, "15 mins", 1.83352937473546, 3.2, 4.56647062526454,
     "FALSE", 0.627162924074226, "30 mins", 3.63352937473546, 5,
     6.36647062526453, "FALSE", 0.627162924074226, "No puppies",
     0.833529374735462, 2.2, 3.56647062526454))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocDunnettContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocDunnettContainer_jaspColumn1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.886942313043338, "30 mins - 15 mins", -0.419635113995323, 1.8,
     0.114110887428387, 2.02944427560764, 4.01963511399532, 0.886942313043338,
     "No puppies - 15 mins", -3.21963511399532, -1, 0.445888579780104,
     -1.12746904200424, 1.21963511399532))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocGamesContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocGamesContainer_jaspColumn1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.916515138991168, "15 mins - 30 mins", 7.7199124726477, -4.43893919399357,
     -1.8, 0.185393344481168, -1.96396101212393, 0.838939193993569,
     0.824621125123532, "15 mins - No puppies", 8, -1.3563089273419,
     1, 0.47896489393065, 1.21267812518166, 3.3563089273419, 2, 0.916515138991168,
     "30 mins - No puppies", 7.7199124726477, 0.161060806006431,
     2.8, 0.0388414107946465, 3.05505046330389, 5.43893919399357
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_jaspColumn11"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.886942313043338, -1.28353325906213, -3.18629336625279,
     0.619226848128532, "15 mins", "30 mins", 12, -1.8, -4.16624115850688,
     -2.02944427560764, 0.147457622995378, 0.566241158506877, "FALSE",
     0.886942313043338, 0.713074032812292, -1.09077283336099, 2.51692089898558,
     "15 mins", "No puppies", 12, 1, -1.36624115850688, 1.12746904200424,
     0.516276123508473, 3.36624115850688, 2, "FALSE", 0.886942313043338,
     1.99660729187442, -0.0946607613902675, 4.0878753451391, "30 mins",
     "No puppies", 12, 2.8, 0.433758841493124, 3.15691331761188,
     0.0209243994922419, 5.16624115850688))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_jaspColumn1LetterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(" ab", "15 mins", "  b", "30 mins", " a ", "No puppies"))

})

