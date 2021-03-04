context("ANOVA -- Verification project")

# does not test
# - if analysis handles too few observations

# https://jasp-stats.github.io/jasp-verification-project/anova.html#one-way-independent-anova
test_that("Main table results match R, SPSS, SAS and MiniTab", { 
  options <- jaspTools::analysisOptions("Anova")
  options$dependent <- "Score"
  options$fixedFactors <- "Group"
  
  options$modelTerms <- list(
    list(components="Group")
  )
  
  result <- jaspTools::runAnalysis("Anova", "ANOVA.csv", options)
  
  # Main table
  resultTable <- result$results$anovaContainer$collection$anovaContainer_anovaTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("TRUE", 3, 0.308172852029183, 7.52393902393904, 0.81930549448023,
               22.5718170718171, "Group", "TRUE", 36, "", 24.4146717480051,
               "", 878.928182928183, "Residuals"))
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#factorial-independent-anova
test_that("Main table results match  R, SPSS, SAS and MiniTab 2-factor", { 
  options <- jaspTools::analysisOptions("Anova")
  options$dependent <- "Score"
  options$fixedFactors <- c("Norms", "Standing")
  
  options$modelTerms <- list(
    list(components="Norms"),
    list(components="Standing"),
    list(components=c("Norms", "Standing"))
  )
  
  result <- jaspTools::runAnalysis("Anova", "FIanova.csv", options)
  
  #  Main table
  resultTable <- result$results$anovaContainer$collection$anovaContainer_anovaTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("TRUE", 1, 0.35820895522388, 4.26666666666665, 0.552004973907298,
               4.26666666666665, "Norms", "FALSE", 2, 209.641791044776, 2497.06666666667,
               3.51846028634314e-26, 4994.13333333333, "Standing", "FALSE",
               2, 34.0074626865672, 405.066666666666, 2.76364103266819e-10,
               810.133333333333, "Norms <unicode> Standing", "TRUE", 54, "",
               11.9111111111111, "", 643.199999999999, "Residuals"))
})

## Testing Kruskal-Wallis

options <- jaspTools::analysisOptions("Anova")
options$dependent <- "Score"
options$fixedFactors <- "Treatment"
options$kruskalVariablesAssigned <- "Treatment"

options$modelTerms <- list(
  list(components="Treatment")
)

result <- jaspTools::runAnalysis("Anova", "KWanova.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/anova.html#kruskal-wallis-anova
test_that("Main results match  R, SPSS, SAS and MiniTab 2", { 
  #  Main table
  resultTable <- result$results$anovaContainer$collection$anovaContainer_anovaTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("TRUE", 2, 10.6140474663246, 417.861111111111, 0.000275916880669353,
               835.722222222222, "Treatment", "TRUE", 33, "", 39.3686868686869,
               "", 1299.16666666667, "Residuals"))
})

test_that("Kruskal-Wallis results match  R, SPSS, SAS and MiniTab", { 
  #  Kruskal-Wallis
  kruskalContainer <- result$results$anovaContainer$collection$anovaContainer_kruskalContainer$collection
  resultTable <- kruskalContainer$anovaContainer_kruskalContainer_kruskalTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("Treatment", 13.844384244926, 2, 0.00098566686805503))
})