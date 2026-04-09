context("Example: alien_scents")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("AnovaRepeatedMeasures results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "alien_scents.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasures", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_assumptionsContainer"]][["collection"]][["rmAnovaContainer_assumptionsContainer_qqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_q-q-plot")

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_betweenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "", 23.4287528344671, "", 1148.00888888889, "Residuals",
     49))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_contrastContainer"]][["collection"]][["rmAnovaContainer_contrastContainer_simpleContrast_Entity"]][["collection"]][["rmAnovaContainer_contrastContainer_simpleContrast_Entity_contrastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Shapeshifter - Human", 0.223371405019754, 1.93187441072862, 1.49541803846378,
     2.36833078299346, 49, 4.52666666666667, 4.0777850224562, 1.81038228610231e-25,
     20.2652021025984, 4.97554831087713, "Alien - Human", 0.239720169441764,
     2.38425737288745, 1.85840227564886, 2.91011247012605, 49, 5.58666666666667,
     5.10493095031607, 3.6171873252127e-28, 23.3049504331501, 6.06840238301726
    ))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_contrastContainer"]][["collection"]][["rmAnovaContainer_contrastContainer_simpleContrast_Scent"]][["collection"]][["rmAnovaContainer_contrastContainer_simpleContrast_Scent_contrastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Human - None", 0.183987084235641, -0.361337334554543, -0.535347214874778,
     -0.187327454234307, 49, -0.846666666666667, -1.21640255509822,
     2.98550177567503e-05, -4.60177229387635, -0.476930778235111,
     "Fox - None", 0.201500043607182, -0.0170710551758049, -0.18992014180116,
     0.15577803144955, 49, -0.04, -0.444929497913454, 0.843466533814663,
     -0.198511123292751, 0.364929497913454))

  plotName <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["rmAnovaContainer_descriptivesContainer_containerDescriptivesPlots_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_")

  plotName <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer_containerRainCloudPlots"]][["collection"]][["rmAnovaContainer_descriptivesContainer_containerRainCloudPlots_rainCloudPlotSingle"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_dependent")

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer_tableDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Human", 2.98, 50, 1.07835854095804, 0.152502927372372, "None",
     0.361865282200683, "Human", 4.16, 50, 1.41939912552848, 0.200733349374289,
     "Human", 0.341201712867424, "Human", 7.32, 50, 1.98401777477225,
     0.280582484507221, "Fox", 0.271040679613696, "Shapeshifter",
     10.42, 50, 2.78545786365619, 0.393923228820137, "None", 0.267318413018828,
     "Shapeshifter", 8.78, 50, 2.46849536919863, 0.349097962977589,
     "Human", 0.281149814259526, "Shapeshifter", 8.84, 50, 2.42739467288679,
     0.343285446762871, "Fox", 0.274592157566379, "Alien", 12.06,
     50, 3.0264817588637, 0.428009154965982, "None", 0.250952052973773,
     "Alien", 9.98, 50, 2.75154872901849, 0.38912775301084, "Human",
     0.275706285472795, "Alien", 9.18, 50, 2.41331341368109, 0.341294055988471,
     "Fox", 0.262888171424955))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_marginalMeansContainer"]][["collection"]][["rmAnovaContainer_marginalMeansContainer_Entity:Scent"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "Human", 0.152502927372372, "None", 2.53757580232777,
     2.98, 3.42242419767223, "FALSE", "Shapeshifter", 0.393923228820137,
     "None", 9.27719460302782, 10.42, 11.5628053969722, "FALSE",
     "Alien", 0.428009154965982, "None", 10.8183083797479, 12.06,
     13.3016916202521, "TRUE", "Human", 0.200733349374289, "Human",
     3.5776551717848, 4.16, 4.7423448282152, "FALSE", "Shapeshifter",
     0.349097962977589, "Human", 7.76723658069694, 8.78, 9.79276341930306,
     "FALSE", "Alien", 0.38912775301084, "Human", 8.85110669359513,
     9.98, 11.1088933064049, "TRUE", "Human", 0.280582484507221,
     "Fox", 6.50600591655608, 7.32, 8.13399408344392, "FALSE", "Shapeshifter",
     0.343285446762871, "Fox", 7.84409919354681, 8.84, 9.83590080645318,
     "FALSE", "Alien", 0.34129405598847, "Fox", 8.18987639061966,
     9.18, 10.1701236093803))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_postHocStandardContainer"]][["collection"]][["rmAnovaContainer_postHocStandardContainer_Entity:Scent1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "Human", 0.197308419092795, -0.503596127686252, -0.837095027643061,
     -0.170097227729443, "None", "Human", 49, -1.18, 2.50814320124361e-07,
     -1.66911830381271, -5.98048479342912, -0.690881696187292, "FALSE",
     "Human", 0.2417791199776, -1.85220948657486, -2.57652648414822,
     -1.1278924890015, "None", "Fox", 49, -4.34, 1.03576967590777e-22,
     -4.93935908262057, -17.950267998337, -3.74064091737943, "FALSE",
     "Human", 0.289066054170097, -1.34861335888861, -1.97163859389594,
     -0.725588123881278, "Human", "Fox", 49, -3.16, 1.92637216095622e-14,
     -3.87658117152627, -10.9317574803873, -2.44341882847374, "TRUE",
     "Shapeshifter", 0.383070303133586, 0.699913262208011, 0.0961072327493637,
     1.30371929166666, "None", "Human", 49, 1.64, 0.000258674663689782,
     0.690386710454559, 4.28119848128267, 2.58961328954544, "FALSE",
     "Shapeshifter", 0.39080241889252, 0.674306679444304, 0.0635861989815267,
     1.28502715990708, "None", "Fox", 49, 1.58, 0.000372324293973272,
     0.611219158647687, 4.04296371674848, 2.54878084135232, "FALSE",
     "Shapeshifter", 0.353882258754553, -0.0256065827637064, -0.53765509879017,
     0.486441933262758, "Human", "Fox", 49, -0.059999999999997, 0.866063729538286,
     -0.93725749842449, -0.169547917465995, 0.817257498424496, "TRUE",
     "Alien", 0.377791649738642, 0.887694869141868, 0.262288271173378,
     1.51310146711036, "None", "Human", 49, 2.08, 2.67669347865697e-06,
     1.14347224429453, 5.50568018493514, 3.01652775570547, "FALSE",
     "Alien", 0.421503625277943, 1.22911597265797, 0.488165005560623,
     1.97006693975532, "None", "Fox", 49, 2.88, 3.61505000596215e-08,
     1.8351123099826, 6.83268144633607, 3.92488769001739, "FALSE",
     "Alien", 0.362530786869986, 0.341421103516101, -0.195937173010942,
     0.878779380043145, "Human", "Fox", 49, 0.799999999999997, 0.0320509736855916,
     -0.09869679342137, 2.20670913746947, 1.69869679342136))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_postHocStandardContainer"]][["collection"]][["rmAnovaContainer_postHocStandardContainer_Entity:Scent2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.350113684160923, "None", -3.17521626269976, -4.3747080158276,
     -1.97572450957192, "Human", "Shapeshifter", 49, -7.44, 4.46647387305725e-26,
     -8.30791537900808, -21.2502405263895, -6.57208462099192, "FALSE",
     0.382088781220666, "None", -3.87512952490777, -5.31263386032494,
     -2.4376251894906, "Human", "Alien", 49, -9.08, 4.50181513747251e-28,
     -10.0271801428231, -23.7641104535756, -8.13281985717688, "FALSE",
     0.433081035919918, "None", -0.69991326220801, -1.37074250596038,
     -0.0290840184556436, "Shapeshifter", "Alien", 49, -1.64, 0.000417275974449562,
     -2.71358754723475, -3.78682016522943, -0.566412452765244, "TRUE",
     0.35443545222779, "Human", -1.9717068728055, -2.81952805964546,
     -1.12388568596553, "Human", "Shapeshifter", 49, -4.62, 3.03201130392962e-17,
     -5.49862883906244, -13.0348134504073, -3.74137116093757, "FALSE",
     0.376547148577759, "Human", -2.48383852807965, -3.49388721248062,
     -1.47378984367868, "Human", "Alien", 49, -5.82, 5.35914165556736e-20,
     -6.75344269577897, -15.456231768007, -4.88655730422103, "FALSE",
     0.342857142857143, "Human", -0.512131655274153, -1.03824352163275,
     0.0139802110844469, "Shapeshifter", "Alien", 49, -1.2, 0.00100132738394426,
     -2.04992675393891, -3.49999999999999, -0.350073246061089, "TRUE",
     0.378223559848524, "Fox", -0.648700096680596, -1.23925920198962,
     -0.0581409913715725, "Human", "Shapeshifter", 49, -1.52, 0.000402176340354156,
     -2.45759844058205, -4.01878719720355, -0.582401559417948, "FALSE",
     0.318170792281794, "Fox", -0.793804065674941, -1.32838091235858,
     -0.2592272189913, "Human", "Alien", 49, -1.86, 1.21178079258008e-06,
     -2.64873045032319, -5.84591686326964, -1.07126954967681, "FALSE",
     0.39446165846632, "Fox", -0.145103968994345, -0.717944052201998,
     0.427736114213308, "Shapeshifter", "Alien", 49, -0.340000000000003,
     0.39292242541417, -1.31785192438978, -0.861934215157778, 0.637851924389775
    ))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_postHocStandardContainer"]][["collection"]][["rmAnovaContainer_postHocStandardContainer_Entity:ScentLetterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Human", " a      ", "None", "Shapeshifter", "      f ", "None",
     "Alien", "       g", "None", "Human", "  b     ", "Human", "Shapeshifter",
     "    d   ", "Human", "Alien", "     ef ", "Human", "Human",
     "   c    ", "Fox", "Shapeshifter", "    de  ", "Fox", "Alien",
     "    def ", "Fox"))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_simpleEffectsContainer"]][["collection"]][["rmAnovaContainer_simpleEffectsContainer_simpleEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 2, 166.974916538843, 251.78, 503.56, "Human", 2.71547303046189e-32,
     "FALSE", 2, 12.2193903048476, 43.2466666666667, 86.4933333333334,
     "Shapeshifter", 1.82829772241959e-05, "FALSE", 2, 29.3497145334971,
     110.506666666666, 221.013333333333, "Alien", 1.02748246013049e-10
    ))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_withinAnovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 315.946781385962, 1320.62888888889, 2641.25777777778,
     "Entity", 2, 1.86363376358594e-43, 0.623565690643518, "TRUE",
     "", 4.17990929705215, 409.631111111111, "Residuals", 98, "",
     "TRUE", 12.913357400722, 34.2288888888889, 68.4577777777777,
     "Scent", 2, 1.05238466482972e-05, 0.0421138580176911, "TRUE",
     "", 2.65065759637188, 259.764444444445, "Residuals", 98, "",
     "TRUE", 60.2611674996136, 185.652222222222, 742.608888888889,
     "Entity <unicode> Scent", 4, 4.07683563505529e-33, 0.2900396798951,
     "TRUE", "", 3.08079365079365, 603.835555555555, "Residuals",
     196, ""))

})

