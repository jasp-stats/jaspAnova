context("MANOVA -- Verification project")

## Testing Andy Field example
options <- jaspTools::analysisOptions("Manova")
options$dependent <- c("Actions", "Thoughts")
options$fixedFactors <- c("Group")
options$modelTerms <- list(
  list(components = "Group")
)
options$includeIntercept    <- TRUE
options$testPillai          <- TRUE
options$testWilks           <- TRUE
options$testHotellingLawley <- TRUE
options$testRoy             <- TRUE
options$includeAnovaTables  <- TRUE
results <- jaspTools::runAnalysis("Manova", "manova_ocd.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/anova.html#manova
test_that("Andy Field Pillai table results match with R, SPSS, SAS and MiniTab", { 
  # Pillai table
  table <- results[['results']][['manovaContainer']][['collection']][['manovaContainer_Pillai']][['data']]
  jaspTools::expect_equal_tables(table,
                                 list(745.229671237717, "(Intercept)", 1, 26, 2, 1.10625055220703e-23,
                                      0.982854799155012, 2.55665815696287, "Group", 2, 54, 4, 0.0490374099251968,
                                      0.318454579025111, "", "Residuals", 27, "", "", "", ""), label = "Pillai")
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#manova
test_that("Andy Field Wilks table results match with R, SPSS, SAS and MiniTab", { 
  # Wilks table
  table <- results[['results']][['manovaContainer']][['collection']][['manovaContainer_Wilks']][['data']]
  jaspTools::expect_equal_tables(table,
                                 list(745.229671237408, "(Intercept)", 1, 26, 2, 1.10625055221288e-23,
                                      0.0171452008449951, 2.55454581750828, "Group", 2, 52, 4, 0.0496647855390226,
                                      0.698509047267356, "", "Residuals", 27, "", "", "", ""), label = "Wilks")
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#manova
test_that("Andy Field Hotelling table results match with R, SPSS, SAS and MiniTab", { 
  # Hotelling table
  table <- results[['results']][['manovaContainer']][['collection']][['manovaContainer_Hotelling-Lawley']][['data']]
  jaspTools::expect_equal_tables(table,
                                 list(745.229671237403, "(Intercept)", 1, 26, 2, 1.10625055221298e-23,
                                      57.3253593259541, 2.54584503551958, "Group", 2, 50, 4, 0.050798343576486,
                                      0.407335205683133, "", "Residuals", 27, "", "", "", ""), label = "Hotelling")
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#manova
test_that("Andy Field Roy table results match with R, SPSS, SAS and MiniTab", { 
  # Roy
  table <- results[['results']][['manovaContainer']][['collection']][['manovaContainer_Roy']][['data']]
  jaspTools::expect_equal_tables(table,
                                 list(745.229671237403, "(Intercept)", 1, 26, 2, 1.10625055221298e-23,
                                      57.3253593259541, 4.5197644093943, "Group", 2, 27, 2, 0.0202718277388112,
                                      0.334797363658837, "", "Residuals", 27, "", "", "", ""), label="Roy")
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#manova
test_that("Andy Field ANOVA tables for Actions factor results match with R, SPSS, SAS and MiniTab", { 
  # ANOVA tables
  # for Actions
  table <- results[['results']][['anovaContainer']][['collection']][["anovaContainer_ Response Actions"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 326.4, 616.533333333333, 1.31849279790975e-16, 616.533333333333,
                                      "(Intercept)", 2, 2.77058823529412, 5.23333333333333, 0.0804566489996684,
                                      10.4666666666667, "Group      ", 27, "", 1.88888888888889, "",
                                      51, "Residuals  "), label = "ANOVA->Actions")
})

# https://jasp-stats.github.io/jasp-verification-project/anova.html#manova
test_that("Andy Field ANOVA tables for Thoughts factor results match with R, SPSS, SAS and MiniTab", { 
  # for Thoughts
  table <- results[['results']][['anovaContainer']][['collection']][["anovaContainer_ Response Thoughts"]][['data']]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1402.34754098361, 6336.53333333333, 8.22509725190572e-25, 6336.53333333333,
                                      "(Intercept)", 2, 2.15409836065575, 9.73333333333337, 0.135527393903097,
                                      19.4666666666667, "Group      ", 27, "", 4.51851851851852, "",
                                      122, "Residuals  "), label = "ANOVA->Actions")
})