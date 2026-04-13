context("Example: goggles")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Anova results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "goggles.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("Anova", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 1, 15.5826086956522, 21.3333333333333, 0.000295223592290035,
     21.3333333333333, "jaspColumn1", 0.233013755731555, 0.432383139699537,
     0.0477237124593195, "FALSE", 2, 6.04130434782608, 8.27083333333333,
     0.00494338949698301, 16.5416666666667, "jaspColumn2", 0.173590837637548,
     0.362378065883045, 0.00461375940415347, "TRUE", 2, 8.50652173913044,
     11.6458333333333, 0.000791273868880292, 23.2916666666667, "jaspColumn1 <unicode> jaspColumn2",
     0.238252949699855, 0.427907132120675, 0.0359940655538032, "TRUE",
     42, "", 1.36904761904762, "", 57.5, "Residuals", "", "", ""
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_contrastContainer"]][["collection"]][["anovaContainer_contrastContainer_HelmertContrast_jaspColumn2"]][["collection"]][["anovaContainer_contrastContainer_HelmertContrast_jaspColumn2_contrastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "High dose - Low dose, Placebo", 0.358257190138194, 0.908070697305198,
     0.25861646070861, 1.55752493390179, 42, 1.0625, 0.339507719678962,
     0.00496282593717888, 2.9657464783614, 1.78549228032104, "FALSE",
     "Low dose - Placebo", 0.413679770330811, 0.640991080450728,
     -0.0863343686751347, 1.36831652957659, 42, 0.75, -0.0848395753307462,
     0.0769833618370353, 1.8129965586672, 1.58483957533075))

  plotName <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["anovaContainer_descriptivesContainer_containerDescriptivesPlots_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_")

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_descriptivesContainer"]][["collection"]][["anovaContainer_descriptivesContainer_tableDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(6.125, 8, 1.1259916264596, 0.398098157314428, 0.183835367585241,
     "Attractive", "High dose", 6.5, 8, 0.925820099772551, 0.327326835353989,
     0.142433861503469, "Attractive", "Low dose", 6.375, 8, 0.916125381312904,
     0.323899234771733, 0.14370594216673, "Attractive", "Placebo",
     6.625, 8, 1.06066017177982, 0.375, 0.160099648570539, "Unattractive",
     "High dose", 4.875, 8, 1.24642345475822, 0.440677238544752,
     0.255676606104251, "Unattractive", "Low dose", 3.5, 8, 1.60356745147455,
     0.566946709513841, 0.458162128992728, "Unattractive", "Placebo"
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_marginalMeansContainer"]][["collection"]][["anovaContainer_marginalMeansContainer_jaspColumn1 ✻ jaspColumn2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.413679770330811, "Attractive", "High dose", 5.29016042466925,
     6.125, 6.95983957533075, "FALSE", 0.413679770330811, "Unattractive",
     "High dose", 5.79016042466926, 6.625, 7.45983957533075, "TRUE",
     0.413679770330811, "Attractive", "Low dose", 5.66516042466925,
     6.5, 7.33483957533075, "FALSE", 0.413679770330811, "Unattractive",
     "Low dose", 4.04016042466926, 4.875, 5.70983957533075, "TRUE",
     0.413679770330811, "Attractive", "Placebo", 5.54016042466926,
     6.375, 7.20983957533075, "FALSE", 0.413679770330811, "Unattractive",
     "Placebo", 2.66516042466926, 3.5, 4.33483957533075))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_jaspColumn1 ✻ jaspColumn21"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.58503154168122, -0.320495540225364, -1.88051472492974,
     1.23952364447901, "High dose", "Low dose", 42, -0.375, "Attractive",
     -1.79633047816801, -0.640991080450728, 0.79842785788364, 1.04633047816801,
     "FALSE", 0.58503154168122, -0.213663693483578, -1.77157217611904,
     1.34424478915189, "High dose", "Placebo", 42, -0.250000000000001,
     "Attractive", -1.67133047816801, -0.427327386967153, 0.904478566623266,
     1.171330478168, "FALSE", 0.58503154168122, 0.106831846741787,
     -1.44980884090602, 1.6634725343896, "Low dose", "Placebo", 42,
     0.124999999999999, "Attractive", -1.29633047816801, 0.213663693483575,
     0.975161914976909, 1.546330478168, "FALSE", 0.58503154168122,
     1.49564585438503, -0.141360519820368, 3.13265222859043, "High dose",
     "Low dose", 42, 1.75, "Unattractive", 0.328669521831995, 2.99129170877006,
     0.0126106651419295, 3.17133047816801, "FALSE", 0.58503154168122,
     2.6707961685447, 0.869562959202907, 4.47202937788649, "High dose",
     "Placebo", 42, 3.125, "Unattractive", 1.70366952183199, 5.3415923370894,
     1.02890549975587e-05, 4.546330478168, "FALSE", 0.58503154168122,
     1.17515031415967, -0.431422007124569, 2.7817226354439, "Low dose",
     "Placebo", 42, 1.375, "Unattractive", -0.0463304781680056, 2.35030062831933,
     0.0597623817693386, 2.79633047816801))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_jaspColumn1 ✻ jaspColumn22"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.58503154168122, -0.427327386967153, -1.99029676592988,
     1.13564199199558, "Attractive", "Unattractive", 42, -0.5, "High dose",
     -1.68064144983854, -0.854654773934304, 0.397591234464833, 0.680641449838536,
     "FALSE", 0.58503154168122, 1.38881400764324, -0.237301726475031,
     3.01492974176152, "Attractive", "Unattractive", 42, 1.625, "Low dose",
     0.444358550161464, 2.77762801528649, 0.00814955006779213, 2.80564144983854,
     "FALSE", 0.58503154168122, 2.45713247506112, 0.691322304070213,
     4.22294264605203, "Attractive", "Unattractive", 42, 2.875, "Placebo",
     1.69435855016146, 4.91426495012225, 1.40558381829677e-05, 4.05564144983854
    ))

  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_simpleEffectsContainer"]][["collection"]][["anovaContainer_simpleEffectsContainer_simpleEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 2, 0.21304347826087, 0.291666666666668, 0.808988904940453,
     0.583333333333336, "Attractive", "FALSE", 2, 14.3347826086957,
     19.625, 1.79619834116912e-05, 39.25, "Unattractive"))

})

