context("ANCOVA -- Verification project")

# does not test
# - descriptives table/plot & Q-Q plot & raincloud plot (uses same code as ANOVA)
# - if analysis handles too few observations

## Testing standard ---- 

options <- jaspTools::analysisOptions("Ancova")

options$dependent <- "Happiness"
options$fixedFactors <- "Dose"
options$covariates <- "Puppy_love"
options$modelTerms <- list(
  list(components = "Dose"),
  list(components = "Puppy_love")
)

options$contrasts <- list(
  list(contrast = "simple", variable = "Dose") 
)

results <- jaspTools::runAnalysis("Ancova", "Puppy Love.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/anova.html#ancova
test_that("Main table results match R, SPSS, SAS and MiniTab 1", { 
  # main table
  resultTable <- results$result$anovaContainer$collection$anovaContainer_anovaTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", 2, 4.14192880386377, 12.5925971041911, 0.0274465428639958,
               25.1851942083821, "Dose", "TRUE", 1, 4.9586811332752, 15.0757477099169,
               0.0348333806474408, 15.0757477099169, "Puppy_love", "TRUE",
               26, "", 3.0402736745364, "", 79.0471155379464, "Residuals"))
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#ancova
test_that("Contrast table results match R, SPSS, SAS and MiniTab 1", { 
  contrastContainer <- results$results$anovaContainer$collection$anovaContainer_contrastContainer$collection
  contrastContainer2 <- contrastContainer$anovaContainer_contrastContainer_simpleContrast_Dose$collection
  resultTable <- contrastContainer2$anovaContainer_contrastContainer_simpleContrast_Dose_contrastTable$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", "2 - 1", 0.849355306996752, 26, 1.78568011481422, 0.0453535580857103,
               2.1023947223315, "FALSE", "3 - 1", 0.802810905470399, 26, 2.22488132183556,
               0.0101750138508479, 2.77136409916095))
})