context("ANOVA -- unit tests")

test_that("Test that .normalizeContrastCoefficientsAnova rescales weights to sum(|c|) == 2", {

  # built-in contrast types hand eff_size a named list of bare weight vectors
  builtIn <- list(pairwise = c(1, -1, 0), quadratic = c(1, -2, 1))
  expect_equal(jaspAnova:::.normalizeContrastCoefficientsAnova(builtIn),
               list(pairwise = c(1, -1, 0), quadratic = c(0.5, -1, 0.5)))

  # custom contrasts arrive as a list wrapping each weight vector
  custom <- list(list(c(1, 0, 1, -2, 0)), list(c(0.5, 0, 0.5, -1, 0)))
  expect_equal(jaspAnova:::.normalizeContrastCoefficientsAnova(custom),
               list(list(c(0.5, 0, 0.5, -1, 0)), list(c(0.5, 0, 0.5, -1, 0))))

  # all-zero weights are left alone, the analysis errors on them elsewhere
  expect_equal(jaspAnova:::.normalizeContrastCoefficientsAnova(list(c(0, 0, 0))), list(c(0, 0, 0)))
})
