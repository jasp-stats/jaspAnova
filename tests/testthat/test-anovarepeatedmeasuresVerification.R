context("Repeated Measures ANOVA -- Verification project")

# Does not test:
#    - type I and type II sum of squares
#    - Contrasts apart from 'repeated'


opts <- options()
on.exit(options(opts))
options(list(
  afex.type = 3,
  afex.set_data_arg = FALSE,
  afex.check_contrasts = TRUE,
  afex.method_mixed = "KR",
  afex.return_aov = "afex_aov",
  afex.es_aov = "ges",
  afex.correction_aov = "GG",
  afex.factorize = TRUE,
  afex.lmer_function = "lmerTest",
  afex.sig_symbols = c(" +", " *", " **", " ***"),
  afex.emmeans_model = c("univariate"),
  afex.include_aov = TRUE
))

## Testing standard RM ANOVA

options <- jaspTools::analysisOptions("AnovaRepeatedMeasures")

options$repeatedMeasuresFactors <- list(
  list(name = "RMFactor1", levels = c("control", "experimental"))
)

options$repeatedMeasuresCells <- c("A2.control.", "A1.experimental.")

options$withinModelTerms <- list(
  list(components = "RMFactor1")
)

results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", 
                                  dataset = "Ranova.csv",
                                  options = options)

# https://jasp-stats.github.io/jasp-verification-project/anova.html#one-way-repeated-measures-anova
test_that("Main results match R, SPSS, SAS, MiniTab", {
  # Main table
  resultTable <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_withinAnovaTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("TRUE", 22.5, 20, 20, "RMFactor1", 1, 0.00105387125701656, "TRUE",
               "", 0.88888888888889, 8.00000000000001, "Residuals", 9, "")
  )
})
  
# https://jasp-stats.github.io/jasp-verification-project/anova.html#one-way-repeated-measures-anova
test_that("Between effects results match R, SPSS, SAS, MiniTab", {
  # Between effects table
  resultTable <- results$results$rmAnovaContainer$collection$rmAnovaContainer_betweenTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("TRUE", "", 20.2222222222222, "", 182, "Residuals", 9)
  )
})


## Testing Friedman ----

options <- jaspTools::analysisOptions("AnovaRepeatedMeasures")

options$repeatedMeasuresFactors <- list(
  list(name = "RMFactor1", levels = c("treatment1", "treatment2", "treatment3", "treatment4"))
)

options$repeatedMeasuresCells <- c("Treatment.I", "Treatment.II", "Treatment.III", "Treatment.IV")

options$withinModelTerms <- list(
  list(components = "RMFactor1")
)

options$friedmanWithinFactor <- "RMFactor1"

results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", 
                                  dataset = "Friedman.csv",
                                  options = options)



# https://jasp-stats.github.io/jasp-verification-project/anova.html#friedman-test
test_that("Main results match R, SPSS, SAS, MiniTab 2", {
  # Main table
  resultTable <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_withinAnovaTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("TRUE", 3.38639427564036, 49.4772727272727, 148.431818181818,
               "RMFactor1", 3, 0.0307909821225901, "TRUE", "", 14.6106060606061,
               438.318181818182, "Residuals", 30, "")
  )
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#friedman-test
test_that("Between effects results match R, SPSS, SAS, MiniTab 2", {
  # Between effects table
  resultTable <- results$results$rmAnovaContainer$collection$rmAnovaContainer_betweenTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("TRUE", "", 24.2045454545455, "", 242.045454545455, "Residuals",
               10)
  )
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#friedman-test
test_that("Friedman results match R, SPSS, SAS, MiniTab 2", {
  # Nonparametric Friedman
  resultContainer <- results$results$rmAnovaContainer$collection$rmAnovaContainer_nonparametricContainer$collection
  resultTable <- resultContainer$rmAnovaContainer_nonparametricContainer_friedmanTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("RMFactor1", 11.9454545454546, 3, 0.345631641086186, 0.00757236506542182
    )
  )
})