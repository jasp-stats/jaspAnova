context("MANOVA")

# Also verified with Andy Field example (OCD.sav)

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("Manova")
  options$dependent <- c("contNormal", "contGamma")
  options$fixedFactors <- c("contBinom", "facGender")
  options$fixedFactors.types <- rep("nominal", length(options$fixedFactors))
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facGender"))
  options$includeIntercept <- TRUE
  options$vovkSellke <- TRUE
  options$testWilks <- TRUE
  options$shapiroTest <- TRUE
  results <- jaspTools::runAnalysis("Manova", "test.csv", options, view = TRUE)
  table <- results[["results"]][["manovaContainer"]][["collection"]][["manovaContainer_Pillai"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(33580470860819210240, 87.6607294486356, "(Intercept)", 1, 96,
                           2, 2.19674826936402e-22, 0.646176161700694, 1, 0.81123253193528,
                           "contBinom", 1, 96, 2, 0.447332275717289, 0.0166197919998132,
                           2.9720945624123, 3.39021145078575, "facGender", 1, 96, 2, 0.0377850473862948,
                           0.0659699844596362, "", "", "Residuals", 97, "", "", "", ""
                           ))


  table <- results[["results"]][["assumptionsContainer"]][["collection"]][["assumptionsContainer_shapiroTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920133622718739, 9.26266088028011e-09))


})

test_that("Box's M test matches the chi-square approximation of Box (1949)", {
  set.seed(2613)
  g       <- rep(c("g1", "g2", "g3"), c(12, 18, 15))
  y1      <- round(c(g1 = 0, g2 = 0.6, g3 = 1)[g] + rnorm(length(g), sd = c(g1 = 0.5, g2 = 1, g3 = 1.8)[g]), 3)
  dataset <- data.frame(g = g, y1 = y1, y2 = round(0.5 * y1 + rnorm(length(g)), 3))

  options <- jaspTools::analysisOptions("Manova")
  options$dependent          <- c("y1", "y2")
  options$dependent.types    <- c("scale", "scale")
  options$fixedFactors       <- "g"
  options$fixedFactors.types <- "nominal"
  options$modelTerms         <- list(list(components = "g"))
  options$boxMTest           <- TRUE
  results <- jaspTools::runAnalysis("Manova", dataset, options)
  boxM    <- results$results$assumptionsContainer$collection$assumptionsContainer_boxMTable$data[[1]]

  Y      <- as.matrix(dataset[c("y1", "y2")])
  groups <- split(as.data.frame(Y), dataset$g)
  n      <- vapply(groups, nrow, numeric(1))
  p      <- ncol(Y)
  k      <- length(groups)
  S      <- lapply(groups, stats::cov)
  pooled <- Reduce(`+`, Map(function(Si, ni) (ni - 1) * Si, S, n)) / (sum(n) - k)
  M      <- (sum(n) - k) * log(det(pooled)) - sum((n - 1) * vapply(S, function(Si) log(det(Si)), numeric(1)))
  c1     <- (2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (k - 1)) * (sum(1 / (n - 1)) - 1 / (sum(n) - k))
  chiSq  <- M * (1 - c1)
  df     <- p * (p + 1) * (k - 1) / 2

  expect_equal(boxM[["ChiSq"]], chiSq)
  expect_equal(boxM[["df"]],    df)
  expect_equal(boxM[["p"]],     stats::pchisq(chiSq, df, lower.tail = FALSE))
})
