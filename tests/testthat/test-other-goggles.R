context("Other: goggles")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("Anova results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "goggles.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("Anova", dataset, opts)

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 1, 15.5826086956522, 21.3333333333333, 0.000295223592290035,
     21.3333333333333, "Facetype", 0.233013755731555, 0.432383141714249,
     0.0477237051208246, "FALSE", 2, 6.04130434782608, 8.27083333333333,
     0.00494338949698301, 16.5416666666667, "Alcohol", 0.173590837637548,
     0.362378057995661, 0.0046137581746789, "TRUE", 2, 8.50652173913044,
     11.6458333333333, 0.000791273868880292, 23.2916666666667, "Facetype <unicode> Alcohol",
     0.238252949699855, 0.427907139565879, 0.035994070104449, "TRUE",
     42, "", 1.36904761904762, "", 57.5, "Residuals", "", "", ""
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_contrastContainer"]][["collection"]][["anovaContainer_contrastContainer_HelmertContrast_Alcohol"]][["collection"]][["anovaContainer_contrastContainer_HelmertContrast_Alcohol_contrastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "Placebo - Low dose, High dose", 0.358257190138194, -0.934778658990644,
     -1.58606747817559, -0.283489839805702, 42, -1.09375, -1.81674228032104,
     0.00392140201994098, -3.05297431596026, -0.370757719678961,
     "FALSE", "Low dose - High dose", 0.413679770330811, -0.587575157079834,
     -1.31271000536443, 0.137559691204761, 42, -0.6875, -1.52233957533075,
     0.103977316507001, -1.6619135121116, 0.147339575330746))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_")

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_tableDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Placebo", "Unattractive", 3.5, 8, 1.60356745147455, 0.566946709513841,
     0.458162128992728, "Low dose", "Unattractive", 4.875, 8, 1.24642345475822,
     0.440677238544752, 0.255676606104251, "High dose", "Unattractive",
     6.625, 8, 1.06066017177982, 0.375, 0.160099648570539, "Placebo",
     "Attractive", 6.375, 8, 0.916125381312904, 0.323899234771733,
     0.14370594216673, "Low dose", "Attractive", 6.5, 8, 0.925820099772551,
     0.327326835353989, 0.142433861503469, "High dose", "Attractive",
     6.125, 8, 1.1259916264596, 0.398098157314428, 0.183835367585241
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_marginalMeansContainer"]][["collection"]][["anovaContainer_marginalMeansContainer_Facetype ✻ Alcohol"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "Placebo", "Unattractive", 0.413679770330811, 42, 2.35791693406122,
     3.5, 7.71262387289084e-10, 8.46065060711363, 4.64208306593878,
     "FALSE", "Placebo", "Attractive", 0.413679770330811, 42, 5.23291693406122,
     6.375, 0, 15.4104707486713, 7.51708306593878, "TRUE", "Low dose",
     "Unattractive", 0.413679770330811, 42, 3.73291693406122, 4.875,
     3.99680288865056e-14, 11.7844776313368, 6.01708306593878, "FALSE",
     "Low dose", "Attractive", 0.413679770330811, 42, 5.35791693406122,
     6.5, 0, 15.7126368417824, 7.64208306593878, "TRUE", "High dose",
     "Unattractive", 0.413679770330811, 42, 5.48291693406122, 6.625,
     0, 16.0148029348937, 7.76708306593878, "FALSE", "High dose",
     "Attractive", 0.413679770330811, 42, 4.98291693406122, 6.125,
     0, 14.8061385624488, 7.26708306593878))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_Facetype ✻ Alcohol1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.023530786123731, "Unattractive", 0.58503154168122, -1.17515031415967,
     -2.7817226354439, 0.431422007124569, "Placebo", "Low dose",
     42, -1.375, -2.79633047816801, -2.35030062831934, 0.0597623817693385,
     0.0463304781680054, "FALSE", 1.0468902134484e-05, "Unattractive",
     0.58503154168122, -2.6707961685447, -4.47202937788649, -0.869562959202907,
     "Placebo", "High dose", 42, -3.125, -4.546330478168, -5.3415923370894,
     1.02890549975587e-05, -1.70366952183199, "FALSE", 0.00695053364107305,
     "Unattractive", 0.58503154168122, -1.49564585438503, -3.13265222859043,
     0.141360519820369, "Low dose", "High dose", 42, -1.75, -3.17133047816801,
     -2.99129170877006, 0.0126106651419295, -0.328669521831995, "FALSE",
     0.831843182854305, "Attractive", 0.58503154168122, -0.106831846741787,
     -1.6634725343896, 1.44980884090602, "Placebo", "Low dose", 42,
     -0.124999999999999, -1.546330478168, -0.213663693483574, 0.97516191497691,
     1.29633047816801, "FALSE", 0.831843182854305, "Attractive",
     0.58503154168122, 0.213663693483578, -1.34424478915189, 1.77157217611904,
     "Placebo", "High dose", 42, 0.250000000000002, -1.171330478168,
     0.427327386967154, 0.904478566623265, 1.67133047816801, "FALSE",
     0.831843182854305, "Attractive", 0.58503154168122, 0.320495540225364,
     -1.23952364447901, 1.88051472492974, "Low dose", "High dose",
     42, 0.375, -1.04633047816801, 0.640991080450728, 0.798427857883639,
     1.79633047816801))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_Facetype ✻ Alcohol2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "Placebo", 1.40558381829677e-05, 0.58503154168122, -2.45713247506112,
     -4.22294264605203, -0.691322304070213, "Unattractive", "Attractive",
     42, -2.875, -4.05564144983854, -4.91426495012225, 1.40558381829677e-05,
     -1.69435855016147, "FALSE", "Low dose", 0.00814955006779213,
     0.58503154168122, -1.38881400764324, -3.01492974176152, 0.237301726475031,
     "Unattractive", "Attractive", 42, -1.625, -2.80564144983854,
     -2.77762801528649, 0.00814955006779213, -0.444358550161464,
     "FALSE", "High dose", 0.397591234464833, 0.58503154168122, 0.427327386967153,
     -1.13564199199558, 1.99029676592988, "Unattractive", "Attractive",
     42, 0.500000000000001, -0.680641449838535, 0.854654773934305,
     0.397591234464833, 1.68064144983854))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_simpleEffectsContainer"]][["collection"]][["anovaContainer_simpleEffectsContainer_simpleEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 2, 14.3347826086957, 19.625, 1.79619834116911e-05, 39.25,
     "Unattractive", "FALSE", 2, 0.21304347826087, 0.291666666666668,
     0.808988904940452, 0.583333333333336, "Attractive"))

})

