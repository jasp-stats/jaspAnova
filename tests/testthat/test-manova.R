context("MANOVA")

# Also verified with Andy Field example (OCD.sav)

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("Manova")
  options$dependent <- c("contNormal", "contGamma")
  options$fixedFactors <- c("contBinom", "facGender")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facGender"))
  options$includeIntercept <- TRUE
  options$VovkSellkeMPR <- TRUE
  options$testWilks <- TRUE

  results <- jaspTools::runAnalysis("Manova", "test.csv", options, view = TRUE)
  table <- results[["results"]][["manovaContainer"]][["collection"]][["manovaContainer_Pillai"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(33580470860819210240, 87.6607294486356, "(Intercept)", 1, 96,
                           2, 2.19674826936402e-22, 0.646176161700694, 1, 0.81123253193528,
                           "contBinom", 1, 96, 2, 0.447332275717289, 0.0166197919998132,
                           2.9720945624123, 3.39021145078575, "facGender", 1, 96, 2, 0.0377850473862948,
                           0.0659699844596362, "", "", "Residuals", 97, "", "", "", ""
                           ))
    
})