context("Other: sniffer_dogs")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("AnovaRepeatedMeasures results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "sniffer_dogs.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasures", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_assumptionsContainer"]][["collection"]][["rmAnovaContainer_assumptionsContainer_qqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_q-q-plot", tolerance = 0.2)

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_assumptionsContainer"]][["collection"]][["rmAnovaContainer_assumptionsContainer_sphericityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 0.532845552798474, 0.66576361409737, 0.333333333333333,
     0.136248029372535, 11.4059814340564, "Entity", 5, 0.0468458123067897
    ))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_betweenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "", 2.48214285714286, "", 17.375, "Residuals", 7))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_contrastContainer"]][["collection"]][["rmAnovaContainer_contrastContainer_customContrast_Entity"]][["collection"]][["rmAnovaContainer_contrastContainer_customContrast_Entity_contrastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.640869944461656, 1.1136054643847, 0.179864114766492, 2.04734681400291,
     7, 2.75, 1.23458338720905, 0.0036060675889284, 4.29104223683021,
     4.26541661279095, 2, 1.79222029098785, 0.961750173786787, -0.858840308821286,
     2.78234065639486, 7, 2.375, -1.86292756426655, 0.226729616501591,
     1.32517191772833, 6.61292756426654, 3, 1.20174723988509, 0.0506184301993045,
     -1.10055744130757, 1.20179430170618, 7, 0.125, -2.71668066771697,
     0.920074728368496, 0.104015217053423, 2.96668066771697))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_contrastContainer"]][["collection"]][["rmAnovaContainer_contrastContainer_customContrast_Entity"]][["collection"]][["rmAnovaContainer_contrastContainer_customContrast_Entity_customCoefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.5, 0, -1, "Mannequin", -0.5, 0, 1, "Human", 0.5, -1, 0, "Shapeshifter",
     0.5, 1, 0, "Alien"))

  plotName <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["rmAnovaContainer_descriptivesContainer_containerDescriptivesPlots_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_")

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer"]][["collection"]][["rmAnovaContainer_descriptivesContainer_tableDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Mannequin", 4.125, 8, 2.74837614393871, 0.971697704315199, 0.666273004591203,
     "Human", 4.25, 8, 1.83225076262581, 0.647798469543466, 0.43111782650019,
     "Shapeshifter", 5.75, 8, 2.91547594742265, 1.03077640640442,
     0.507039295203939, "Alien", 8.125, 8, 2.23207142742853, 0.789156421213727,
     0.27471648337582))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_marginalMeansContainer"]][["collection"]][["rmAnovaContainer_marginalMeansContainer_Entity"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", "Mannequin", 0.971697704315199, 1.82730004315924, 4.125,
     6.42269995684075, "FALSE", "Human", 0.647798469543466, 2.71820002877283,
     4.25, 5.78179997122717, "FALSE", "Shapeshifter", 1.03077640640441,
     3.31260111144646, 5.75, 8.18739888855354, "FALSE", "Alien",
     0.789156421213727, 6.25894158809785, 8.125, 9.99105841190215
    ))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_postHocStandardContainer"]][["collection"]][["rmAnovaContainer_postHocStandardContainer_Entity1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 1.20174723988509, 1, -0.0506184301993045, -1.82064754784018,
     1.71941068744157, "Mannequin", "Human", 7, -0.125, -4.49432153408686,
     -0.104015217053423, 4.24432153408686, "FALSE", 1.82186619392628,
     1, -0.65803959259096, -3.41555353532226, 2.09947435014034, "Mannequin",
     "Shapeshifter", 7, -1.625, -8.24895462968413, -0.891942561653215,
     4.99895462968413, "FALSE", 0.7319250547114, 0.00564485983507569,
     -1.61978976637775, -3.52731127948738, 0.287731746731882, "Mannequin",
     "Alien", 7, -4, -6.66113854623374, -5.46504040851178, -1.33886145376625,
     "TRUE", 1.33630620956212, 1, -0.607421162391655, -2.66150784457404,
     1.44666551979073, "Human", "Shapeshifter", 7, -1.5, -6.35855203472909,
     -1.12249721603218, 3.35855203472909, "FALSE", 0.811469126250126,
     0.0121396972553231, -1.56917133617844, -3.50626858994993, 0.367925917593048,
     "Human", "Alien", 7, -3.875, -6.82534547190668, -4.77528950227193,
     -0.924654528093316, "TRUE", 1.79222029098785, 1, -0.961750173786787,
     -3.76106020562226, 1.83755985804869, "Shapeshifter", "Alien",
     7, -2.375, -8.89116783575006, -1.32517191772833, 4.14116783575006
    ))

  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_withinAnovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 1, 1, 1, 1, 1, "TRUE", 3.79380603096984, 27.7083333333333,
     83.1249999999999, "Entity", "None", 0.327424913835549, 0.327424913835549,
     3, 0.238785176929507, 0.0255702968630395, 0.351479915433404,
     0.238785176929507, "FALSE", 3.79380603096984, 52.0006842279357,
     83.1249999999999, "Entity", "Greenhouse-Geisser", 0.327424913835549,
     0.327424913835549, 1.59853665839542, 0.238785176929507, 0.0625841206869633,
     0.351479915433404, 0.238785176929507, "FALSE", 3.79380603096984,
     41.6188760494214, 83.1249999999999, "Entity", "Huynh-Feldt",
     0.327424913835549, 0.327424913835549, 1.99729084229211, 0.238785176929507,
     0.0483306135519431, 0.351479915433404, 0.238785176929507, "TRUE",
     "", 7.30357142857143, 153.375, "Residuals", "None", "", "",
     21, "", "", "", 0, "", 13.7067324484806, 153.375, "Residuals",
     "Greenhouse-Geisser", "", "", 11.1897566087679, "", "", "",
     0, "", 10.9702171670548, 153.375, "Residuals", "Huynh-Feldt",
     "", "", 13.9810358960448, "", "", ""))

})

