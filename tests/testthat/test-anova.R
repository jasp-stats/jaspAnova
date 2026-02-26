context("ANOVA")

# does not test
# - if analysis handles too few observations

# Main results ----
test_that("Main table results match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facFive", "contBinom")
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contBinom"),
    list(components=c("facFive", "contBinom"))
  )
  options$contrasts <- list(
    list(contrast="none", variable="facFive"),
    list(contrast="none", variable="contBinom")
  )
  options$effectSizeEstimates <- TRUE
  options$effectSizeEtaSquared <- TRUE
  options$effectSizeOmegaSquared <- TRUE
  options$effectSizePartialEtaSquared <- TRUE
  options$vovkSellke <- TRUE

  refTables <- list(
    type1 = list("facFive", 181.151987151139, 4, 45.2879967877848, 1.82424792091397,
                 0.131040166930968, "TRUE", 0.0733818170125722, 0.0749970945389635,
                 0.0328259131508147, 1.3814133611929, "contBinom", 24.5000310074981,
                 1, 24.5000310074981, 0.98688689714382, 0.323167856084615, "FALSE",
                 0.00992457670748362, 0.0108464739348588, 0, 1.00776449783097,
                 "facFive <unicode> contBinom", 28.6688295533663, 4, 7.16720738834158,
                 0.288702616682483, 0.884618241388975, "TRUE", 0.011613291343553,
                 0.0126686727866262, 0, 1, "Residuals", 2234.30141494065, 90,
                 24.8255712771184, "", "", "TRUE", "", "", "", ""),
    type2 = list("facFive", 174.090039449647, 4, 43.5225098624118, 1.75313225933803,
                 0.145323959824188, "TRUE", 0.0707234506472374, 0.0722847770997921,
                 0.0300789007673393, 1.31245187936232, "contBinom", 24.5000310074979,
                 1, 24.5000310074979, 0.986886897143812, 0.323167856084617, "FALSE",
                 0.00995304923413341, 0.0108464739348588, 0, 1.00776449783097,
                 "facFive <unicode> contBinom", 28.6688295533663, 4, 7.16720738834158,
                 0.288702616682483, 0.884618241388975, "TRUE", 0.0116466086080589,
                 0.0126686727866262, 0, 1, "Residuals", 2234.30141494066, 90,
                 24.8255712771184, "", "", "TRUE", "", "", "", ""),
    type3 = list("TRUE", 4, 1.66677534088084, 41.3786500279805, 0.164641950089634,
                 165.514600111922, "facFive", 0.0675231783920824, 0.0267410840853991,
                 0.0689697039580327, 1.23860485419559, "FALSE", 1, 0.9160552247026,
                 22.7415942746311, 0.341076850898726, 22.7415942746311, "contBinom",
                 0.00927763910910521, 0, 0.0100758355874387, 1.00272840654715,
                 "TRUE", 4, 0.288702616682483, 7.16720738834158, 0.884618241388975,
                 28.6688295533663, "facFive <unicode> contBinom", 0.0116957083599583,
                 0, 0.0126686727866262, 1, "TRUE", 90, "", 24.8255712771184,
                 "", 2234.30141494065, "Residuals", "", "", "", "")
  )

  for (type in c("type1", "type2", "type3")) {
    options$sumOfSquares <- type
    results <- jaspTools::runAnalysis("Anova", "test.csv", options)
    table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
    jaspTools::expect_equal_tables(table, refTables[[type]], label=paste("Table with SS", type))
  }
})

# Additional results ----
test_that("Homogeneity of Variances table results match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facExperim"
  options$modelTerms <- list(list(components="facExperim"))
  options$homogeneityTests <- TRUE
  options$vovkSellke <- TRUE
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_assumptionsContainer"]][["collection"]][["anovaContainer_assumptionsContainer_leveneTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(3.1459013381035, 1, 98, 0.0792241296904395, 1.83142365040653))
})

# Contrasts verified with SPSS
# should we put this in verification project???
test_that("Contrasts table results match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$contrastCi <- TRUE
  options$modelTerms <- list(list(components="facFive"))

  refTables <- list(
    deviation = list("2 - 1, 2, 3, 4, 5", -0.19513913461, 0.211517937182489, -0.922565420263351, 0.358570880186821,
                      95, -0.615055331660948, 0.224777062440949, "TRUE", "3 - 1, 2, 3, 4, 5", 0.331498558639999,
                     0.211517937182488, 1.56723615526752, 0.120384543718471, 95, -0.0884176384109483, 0.751414755690946,
                     "FALSE", "4 - 1, 2, 3, 4, 5", -0.16911442746, 0.211517937182488, -0.799527594267789, 0.425979159402386,
                     95, -0.589030624510948, 0.250801769590948, "FALSE", "5 - 1, 2, 3, 4, 5", 0.18254372644, 0.211517937182488,
                     0.863017713162119, 0.390301247101766, 95, -0.237372470610948, 0.602459923490948, "FALSE"),
    simple = list("2 - 1", -0.0453504116000002, 0.334439223738541, -0.135601354090735,
                  0.892423350599012, 95,  -0.709296216138537, 0.618595392938537,  "TRUE", "3 - 1", 0.48128728165, 0.334439223738541,
                  1.43908742601993, 0.153412521131058, 95, -0.182658522888538, 1.14523308618854, "FALSE", "4 - 1", -0.0193257044500001,
                  0.334439223738541, -0.0577854003904418, 0.954040939034334, 95, -0.683271508988538, 0.644620100088538, "FALSE",
                  "5 - 1", 0.33233244945, 0.334439223738541, 0.993700576550232,
                  0.322892982956191, 95,  -0.331613355088537, 0.996278253988537, "FALSE"),
    difference = list("2 - 1", -0.0453504116, 0.334439223738541, -0.135601354090734,
                      0.892423350599013, 95,  -0.709296216138538, 0.618595392938538,"TRUE", "3 - 1, 2", 0.50396248745, 0.289632863779524,
                      1.74000450388679, 0.0850966309951808, 95, -0.0710314460164715, 1.07895642091647, "FALSE", "4 - 1, 2, 3",
                      -0.164637994466667, 0.273068482710641, -0.602918333278054, 0.547999665617461,
                      95, -0.706747473793691, 0.377471484860357, "FALSE", "5 - 1, 2, 3, 4", 0.22817965805, 0.26439742147811,
                      0.86301771316212, 0.390301247101765, 95,  -0.296715588263685, 0.753074904363685, "FALSE"),
    Helmert = list("1 - 2, 3, 4, 5", -0.1872359037625, 0.26439742147811, -0.708160853898499,
                   0.480579313597282, 95, -0.712131150076185, 0.337659342551185, "TRUE", "2 - 3, 4, 5", -0.31011508715, 0.273068482710641,
                   -1.13566781516348, 0.25895260195633, 95,  -0.852224566477025, 0.231994392177024,  "FALSE", "3 - 4, 5", 0.32478390915,
                   0.289632863779524, 1.12136414670551, 0.264959410721321, 95,  -0.250210024316471, 0.899777842616471, "FALSE",
                   "4 - 5", -0.3516581539, 0.334439223738541, -1.05148597694067,
                   0.295702769469608, 95,  -1.01560395843854, 0.312287650638537, "FALSE"),
    repeated = list("1 - 2", 0.0453504115999998, 0.334439223738541, 0.135601354090733,
                    0.892423350599014, 95, -0.618595392938537, 0.709296216138537, "TRUE", "2 - 3", -0.52663769325, 0.334439223738541,
                    -1.57468878011066, 0.118652819171739, 95,  -1.19058349778854, 0.137308111288537, "FALSE", "3 - 4", 0.5006129861,
                    0.334439223738541, 1.49687282641037, 0.137741463128169, 95, -0.163332818438537, 1.16455879063854, "FALSE",
                    "4 - 5", -0.3516581539, 0.334439223738541, -1.05148597694067,
                    0.295702769469608, 95, -1.01560395843854, 0.312287650638537, "FALSE"),
    polynomial = list("linear", 0.218415231132241, 0.236484243000287, 0.923593167820386,
                      0.358038105335522, 95, -0.251065349597317, 0.687895811861799,  "TRUE", "quadratic", -0.0623342877876619,
                      0.236484243000287, -0.263587488945664, 0.792668695639493, 95,  -0.53181486851722, 0.407146292941896,  "FALSE",
                      "cubic", 0.0886332780579033, 0.236484243000287, 0.374795702806278,
                      0.70864779281998, 95, -0.380847302671655, 0.558113858787461, "FALSE", "quartic", 0.415791419838834, 0.236484243000287,
                      1.75822039795831, 0.0819306308915546, 95,  -0.0536891608907235, 0.885272000568391, "FALSE")
  )

  contrasts <- c("deviation", "simple", "difference", "Helmert", "repeated", "polynomial")
  for (contrast in contrasts) {
    options$contrasts <- list(list(contrast=contrast, variable="facFive"))
    results <- jaspTools::runAnalysis("Anova", "test.csv", options)
    table <- results[["results"]]$anovaContainer$collection$anovaContainer_contrastContainer$collection[[1]]$collection[[1]]$data
    # table <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
    jaspTools::expect_equal_tables(table, refTables[[contrast]], label=paste("Table with contrast", contrast))
  }
})

test_that("Post Hoc table results match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$postHocTypeStandardEffectSize <- TRUE
  options$postHocCorrectionBonferroni <- TRUE
  options$postHocCorrectionHolm <- TRUE
  options$postHocCorrectionScheffe <- TRUE
  options$postHocCorrectionTukey <- TRUE
  options$postHocCorrectionSidak <- TRUE
  options$postHocCi <- TRUE
  options$postHocTerms <- "contBinom"
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  table <- results$results$anovaContainer$collection$anovaContainer_postHocContainer$collection$anovaContainer_postHocContainer_postHocStandardContainer$collection$anovaContainer_postHocContainer_postHocStandardContainer_contBinom$data
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 0.214904085649005, 0.448976320466698, 0.15401876311258,
                                      -0.24864690950018, 0.55668443572534, 0, 1, 0.163364220743842, 98,
                                      0.448976320466698, -0.263105943067512, 0.448976320466698, 0.448976320466698,
                                      0.760172707980336, 0.448976320466698, 0.589834384555196)
    )
})

test_that("Marginal Means table results match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$marginalMeanComparedToZero <- TRUE
  options$marginalMeanTerms <- "contBinom"

  # added df to contrast table
  refTables <- list(
    none = list(0, -0.120135614827586, 0.139273765411964, -0.396519869554477, 98,
                0.156248639899304, -0.862586104943973, 0.390471041862811, "TRUE",
                1, -0.283499835571429, 0.163666075582597, -0.608289835972217, 98,
                0.0412901648293597, -1.73218447721841, 0.0863869418751253, "FALSE"),
    Bonferroni = list(0, -0.120135614827586, 0.139273765411964, -0.396519869554477, 98,
                      0.156248639899304, -0.862586104943973, 0.390471041862811, "TRUE",
                      1, -0.283499835571429, 0.163666075582597, -0.608289835972217, 98,
                      0.0412901648293597, -1.73218447721841, 0.0863869418751253, "FALSE"),
    Sidak = list(0, -0.120135614827586, 0.139273765411964, -0.396519869554477, 98,
                 0.156248639899304, -0.862586104943973, 0.390471041862811, "TRUE",
                 1, -0.283499835571429, 0.163666075582597, -0.608289835972217, 98,
                 0.0412901648293597, -1.73218447721841, 0.0863869418751253, "FALSE")
  )

  for (adjustment in c("none", "Bonferroni", "Sidak")) {
    options$marginalMeanCiCorrection <- adjustment
    results <- jaspTools::runAnalysis("Anova", "test.csv", options)
    table <- results[["results"]]$anovaContainer$collection$anovaContainer_marginalMeansContainer$collection[[1]]$data
    jaspTools::expect_equal_tables(table, refTables[[adjustment]], label=paste("Table with CI adjustment", adjustment))
  }
})

test_that("Descriptives table results match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$descriptives <- TRUE
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  table <- results[["results"]]$anovaContainer$collection$anovaContainer_descriptivesContainer$collection$anovaContainer_descriptivesContainer_tableDescriptives$data
  # removed new group booleans
  jaspTools::expect_equal_tables(table,
        list(-0.120135614827586, 58, 1.10575982846952, 0.145193378675912, -9.20426328242848,
             0, -0.283499835571429, 42, 0.994612407217046, 0.15347202634745,
             -3.50833504087324, 1))
})

test_that("Descriptives table respects factor order", {

  # tests https://github.com/jasp-stats/jasp-issues/issues/1741

  # test 1: one-way ANOVA

  set.seed(42)
  dat <- data.frame(
    x     = rnorm(100),
    group = sample(c("Sedentary", "Low", "Medium", "High"), 100 , TRUE)
  )

  options <- initClassicalAnovaOptions("Anova")

  options$dependent    <- "x"
  options$fixedFactors <- "group"
  options$modelTerms   <- list(list(components = "group"))
  options$descriptives <- TRUE

  # change the level order
  levels1 <- c("Medium", "Sedentary", "Low", "High")
  dat$group <- factor(dat$group, levels = levels1)

  result1 <- runAnalysis(name = "Anova", options = options, dataset = dat)

  tb1 <- result1$results$anovaContainer$collection$anovaContainer_descriptivesContainer$collection$anovaContainer_descriptivesContainer_tableDescriptives$data
  rowNms1 <- sapply(tb1, `[[`, "group.")

  expect_identical(rowNms1, levels1)

  # change the level order
  levels2 <- c("Sedentary", "Low", "Medium", "High")
  dat$group <- factor(dat$group, levels = levels2)

  result2 <- runAnalysis(name = "Anova", options = options, dataset = dat, makeTests = FALSE)

  tb2 <- result2$results$anovaContainer$collection$anovaContainer_descriptivesContainer$collection$anovaContainer_descriptivesContainer_tableDescriptives$data
  rowNms2 <- sapply(tb2, `[[`, "group.")

  expect_identical(rowNms2, levels2)

  # test 2: three-way ANOVA
  set.seed(3141593)
  dat <- data.frame(
    x     = rnorm(20),
    group1 = factor(sample(letters[10:13], 100 , TRUE)),
    group2 = factor(sample(letters[1:3], 100 , TRUE)),
    group3 = factor(sample(LETTERS[1:5], 100 , TRUE))
  )

  # alphabetic level order
  set.seed(2718282)
  for (group in 1:3) {
    colName <- sprintf("group%d", group)
    levels(dat[[colName]]) <- sort(levels(dat[[colName]]))
  }


  options <- initClassicalAnovaOptions("Anova")

  options$dependent    <- "x"
  options$fixedFactors <- paste0("group", 1:3)

  options$modelTerms   <- list(list(components = "group1"), list(components = "group2"), list(components = "group3"))
  options$descriptives <- TRUE

  result <- runAnalysis(name = "Anova", options = options, dataset = dat)

  tb <- result$results$anovaContainer$collection$anovaContainer_descriptivesContainer$collection$anovaContainer_descriptivesContainer_tableDescriptives$data
  for (group in 1:3) {

    rowNmsGroup <- sapply(tb, `[[`, sprintf("group%d.", group))
    rowNmsGroup <- rowNmsGroup[!duplicated(rowNmsGroup)]
    levelsGroup <- levels(dat[[sprintf("group%d", group)]])
    expect_identical(rowNmsGroup, levelsGroup)

  }

  # scramble level order
  set.seed(2718282)
  for (group in 1:3) {
    colName <- sprintf("group%d", group)
    levels(dat[[colName]]) <- sample(levels(dat[[colName]]))
  }

  result <- runAnalysis(name = "Anova", options = options, dataset = dat)

  tb <- result$results$anovaContainer$collection$anovaContainer_descriptivesContainer$collection$anovaContainer_descriptivesContainer_tableDescriptives$data
  for (group in 1:3) {

    rowNmsGroup <- sapply(tb, `[[`, sprintf("group%d.", group))
    rowNmsGroup <- rowNmsGroup[!duplicated(rowNmsGroup)]
    levelsGroup <- levels(dat[[sprintf("group%d", group)]])
    expect_identical(rowNmsGroup, levelsGroup)

  }

})

test_that("Q-Q plot matches", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$qqPlot <- TRUE
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  testPlot <- results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "q-q")
})

test_that("Descriptives plots match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facFive", "contBinom")
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contBinom"),
    list(components=c("facFive", "contBinom"))
  )
  options$descriptivePlotHorizontalAxis <- "contBinom"
  options$descriptivePlotSeparateLines <- "facFive"
  options$descriptivePlotErrorBar <- TRUE
  options$descriptivePlotCiLevel <- 0.90
  options$descriptivePlotErrorBarType <- "ci"
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  testPlot <- results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "descriptives-ci")

  options$descriptivePlotErrorBarType <- "se"
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  testPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "descriptives-se")
})

test_that("Bar plots match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facFive", "contBinom")
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contBinom"),
    list(components=c("facFive", "contBinom"))
  )
  options$barPlotHorizontalAxis <- "contBinom"
  options$barPlotSeparatePlots <- "facFive"
  options$barPlotHorizontalZeroFix <- TRUE
  options$barPlotErrorBars <- TRUE
  options$barPlotCiInterval <- 0.90
  options$barPlotErrorBarType <- "confidenceInterval"
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  testPlot <- results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "barPlot-ci")

  options$barPlotErrorBarType <- "standardError"
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  testPlot <- results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "barPlot-se")
})

test_that("Raincloud plots match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facFive", "contBinom")
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contBinom"),
    list(components=c("facFive", "contBinom"))
  )
  options$rainCloudHorizontalAxis <- "contBinom"
  options$rainCloudSeparatePlots <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  testPlot <- results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "raincloud-plots-vertical")

  options$rainCloudHorizontalDisplay <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  testPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "raincloud-plots-horizontal")
})

test_that("Simple Main Effects table results match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c( "facFive", "facExperim")
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="facFive")
  )
  options$simpleMainEffectFactor <- "facExperim"
  options$simpleMainEffectModeratorFactorOne <- "facFive"
  options$simpleMainEffectModeratorFactorTwo <- ""
  options$homogeneityTests <- TRUE
  options$vovkSellke <- TRUE
  results <- jaspTools::runAnalysis("Anova", "debug.csv", options)
  table <- results$results$anovaContainer$collection$anovaContainer_simpleEffectsContainer$collection$anovaContainer_simpleEffectsContainer_simpleEffectsTable$data
  jaspTools::expect_equal_tables(table, list(1, 0.350864897951646, 1, 0.350864897951646, 0.310783783968887,
                                  0.578524772558188, "TRUE", 2, 2.72259751707838, 1, 2.72259751707838,
                                  2.41158110578085, 0.123801175704108, "FALSE", 3, 0.300954391532799,
                                  1, 0.300954391532799, 0.266574813122249, 0.606851206017453,
                                  "FALSE", 4, 3.47907983036715, 1, 3.47907983036715, 3.08164652754846,
                                  0.0824380354608798, "FALSE", 5, 0.313611321775938, 1, 0.313611321775938,
                                  0.27778587668933, 0.599397784945329, "FALSE"))
})

test_that("Nonparametric table results match", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c( "facFive", "facExperim")
  options$kruskalWallisFactors <- c( "facFive", "facExperim")
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="facFive")
  )
  options$kruskalEffectSizeEstimates <- TRUE
  options$kruskalEpsilon <- TRUE
  options$kruskalEta <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  table <- results[["results"]]$anovaContainer$collection$anovaContainer_kruskalContainer$collection$anovaContainer_kruskalContainer_kruskalTable$data
  jaspTools::expect_equal_tables(table,
                                 list(4, 0.034303030303031, 0, "facFive", 0.011115645383478, 0, 0.493866894607854,
                                      3.39600000000007, 0.193550362611566, 0.14617504991631, 1, 0.0103733573357337,
                                      0.000275126288139123, "facExperim", 1.21480398103053e-05, 0,
                                      0.310873187457312, 1.02696237623763, 0.0878974178624286, 0.0792208198923551)
  )
})

# Error handling ----
test_that("Analysis handles errors", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "debInf"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  expect_identical(results[["results"]][["errorMessage"]],
                   "The following problem(s) occurred while running the analysis:<ul><li>Infinity found in debInf</li></ul>",
                   label="Inf dependent check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$wlsWeights <- "debInf"
  options$modelTerms <- list(list(components="contBinom"))
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  expect_identical(results[["results"]][["errorMessage"]],
                   "The following problem(s) occurred while running the analysis:<ul><li>Infinity found in debInf</li></ul>",
                  label="Inf WLS weights check")
  options$wlsWeights <- ""

  options$dependent <- "contNormal"
  options$fixedFactors <- "debSame"
  options$modelTerms <- list(list(components="debSame"))
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  expect_identical(results[["results"]][["errorMessage"]],
                   "The following problem(s) occurred while running the analysis:<ul><li>Number of factor levels is < 2 in debSame</li></ul>",
                  label="1-level factor check")

  options$dependent <- "contGamma"
  options$fixedFactors <- "facFive"
  options$wlsWeights <- "contNormal"
  options$modelTerms <- list(list(components="facFive"))
  results <- jaspTools::runAnalysis("Anova", "test.csv", options)
  expect_identical(results[["results"]][["errorMessage"]],
                   "The following problem(s) occurred while running the analysis:<ul><li>The WLS weights contain negative and/or zero values.<br><br>(only positive WLS weights allowed).</li></ul>",
                  label="Negative WLS weights check")
})

options <- initClassicalAnovaOptions("Anova")
options$contrasts <- list(list(contrast = "none", variable = "Species"))
options$customContrasts <- list()
options$dependent <- "Sepal.Length"
options$fixedFactors <- "Species"
options$modelTerms <- list(list(components = "Species"))
options$postHocTerms <- list(list(variable = "Species"))
options$rainCloudHorizontalAxis <- ""
options$rainCloudHorizontalDisplay <- FALSE
options$rainCloudYAxisLabel <- ""
options$rainCloudSeparatePlots <- ""

# dataset is created from:
# dd <- read.csv("~/github/jasp-desktop/Resources/Data Sets/Data Library/10. Machine Learning/iris.csv")
# newLabels <- c("First species", "Second species", "Third species")
# oldLabels <- unique(dd$Species)
# dd$Species <- newLabels[match(dd$Species, oldLabels)]
# dataset <- dd[c(1:5, 51:55, 101:105), ]
dataset <- data.frame(
   Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, 7, 6.4, 6.9, 5.5, 6.5, 6.3, 5.8, 7.1, 6.3, 6.5),
   Sepal.Width  = c(3.5, 3, 3.2, 3.1, 3.6, 3.2, 3.2, 3.1, 2.3, 2.8, 3.3, 2.7, 3, 2.9, 3),
   Petal.Length = c(1.4, 1.4, 1.3, 1.5, 1.4, 4.7, 4.5, 4.9, 4, 4.6, 6, 5.1, 5.9, 5.6, 5.8),
   Petal.Width  = c(0.2, 0.2, 0.2, 0.2, 0.2, 1.4, 1.5, 1.5, 1.3, 1.5, 2.5, 1.9, 2.1, 1.8, 2.2),
   Species      = rep(c("First species", "Second species", "Third species"), each = 5)
)

set.seed(1)
results <- jaspTools::runAnalysis("Anova", dataset, options)

test_that("Post Hoc Comparisons - Species table results match and contrast names do not contain commas", {
   table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_postHocContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer"]][["collection"]][["anovaContainer_postHocContainer_postHocStandardContainer_Species1"]][["data"]]
   jaspTools::expect_equal_tables(table,
                                  list("TRUE", 0.286589136802729, "First species", "Second species",
                                       12, -1.6, -5.58290526239083, 0.000324812090923943, "FALSE",
                                       0.286589136802729, "First species", "Third species", 12, -1.54,
                                       -5.37354631505117, 0.000453440993215093, "FALSE", 0.286589136802729,
                                       "Second species", "Third species", 12, 0.0600000000000003, 0.209358947339657,
                                       0.97617415831112)
                                  )

   # following https://github.com/jasp-stats/jasp-issues/issues/1295, assert that these names do not contain commas.
   contrast_A <- vapply(table, `[[`, "contrast_A", FUN.VALUE = character(1L))
   contrast_B <- vapply(table, `[[`, "contrast_B", FUN.VALUE = character(1L))
   expect_identical(contrast_A, c("First species", "First species", "Second species"))
   expect_identical(contrast_B, c("Second species", "Third species", "Third species"))

})


test_that("ANOVA - factor level with zero variance works and Welch homogeneity correction shows a footnote", {
  options <- initClassicalAnovaOptions("Anova")
  options$dependent <- "value"
  options$fixedFactors <- "group"
  options$homogeneityCorrectionWelch <- TRUE
  options$modelTerms <- list(list(components = "group"))

  dataset <- data.frame(
    group = rep(c("A","B","C","D"), c(3, 4, 3, 5)),
    value = c(2.3, 2.1, 2.8, rep(3, 4), c(4, 3, 5, 4, 2), 3.6, 4.5, 5.1)
  )

  set.seed(1)
  results <- jaspTools::runAnalysis("Anova", dataset, options)
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 3, 2.67905056759546, 1.888, 0.0985227092069956, 5.664,
                                      "group", "None", "TRUE", 11, "", 0.704727272727272, "", 7.752,
                                      "Residuals", "", "TRUE", 3, "NaN", 1.888, "NaN", 5.664, "group",
                                      "Welch", "TRUE", "NaN", "", "NaN", "", 7.752, "Residuals",
                                      ""))

  footnote <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["footnotes"]][[1L]][["text"]]
  testthat::expect_identical(
    footnote,
    "The Welch correction could not be computed because 'value' has zero variance after grouping on the following level(s) of 'group': B"
  )
})

# Ordinal restrictions ----
# Tests from https://restriktor.org

## Basic model ----
options <- initClassicalAnovaOptions("Anova")
options$contrasts <- list(list(contrast = "none", variable = "Group"))
options$customContrasts <- list()
options$dependent <- "Age"
options$fixedFactors <- "Group"
options$modelTerms <- list(list(components = "Group"))
options$restrictedBootstrap <- FALSE
options$restrictedBootstrapCiLevel <- 0.95
options$restrictedBootstrapSamples <- 1000
options$restrictedInterceptInclusion <- FALSE
options$restrictedInformedHypothesisTestForAllModels <- FALSE
options$restrictedMarginalMeanForAllModels <- FALSE
options$restrictedModelComparison <- "none"
options$restrictedModelComparisonCoefficients <- FALSE
options$restrictedModelComparisonCoefficientsHighlight <- TRUE
options$restrictedModelComparisonMatrix <- FALSE
options$restrictedModelComparisonReference <- "Model 1"
options$restrictedModelComparisonWeights <- FALSE
options$restrictedModelMarginalMeanTerms <- list()
options$restrictedAvailableCoefficients <- TRUE
options$restrictedModelSummaryForAllModels <- FALSE
options$restrictedModels <- list(list(informedHypothesisTest = TRUE, marginalMean = FALSE,
                                      name = "Model 1", summary = TRUE, syntax = "GroupActive < GroupPassive\nGroupPassive < GroupNo"))
options$restrictedHeterogeneityCorrection <- "none"
set.seed(1)
dataset <- read.csv("ZelazoKolb1972.csv")
dataset <- subset(dataset, Group != "Control")
results <- jaspTools::runAnalysis("Anova", dataset, options)


### see https://restriktor.org/tutorial/restriktor.html for comparison ----
test_that("Ordinal restrictions: Likelihood, penalty and GORIC match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison_comparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(64.7533269649054, 1, -29.5348420070337, "Model 1", 2.84182147541904
                                 ))
})


test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("GroupActive", 10.125, 1.59384623700347e-10, 0.618538022852505,
                                      16.3692442920593, "GroupNo", 12.35, 3.77071859965597e-11, 0.677574455581925,
                                      18.2267792096639, "GroupPassive", 11.375, 3.34358875477653e-11,
                                      0.618538022852504, 18.3901386491037))
})

### see https://restriktor.org/tutorial/contest.html for comparison ----
test_that("Ordinal restrictions: Informative Hypothesis Tests table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_ihtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.028341275692807, 5.97803555494542, "F", "Type global", 0.0276478338044095,
                                      6.04479400936707, "LRT", "Type global", 0.044864172884687, 4.78768638768638,
                                      "Score", "Type global", 0.028341275692807, 5.97803555494542,
                                      "F", "Type A", 0.0276478338044095, 6.04479400936707, "LRT",
                                      "Type A", 0.044864172884687, 4.78768638768638, "Score", "Type A",
                                      1, 0, "F", "Type B", 1, -7.105427357601e-15, "LRT", "Type B",
                                      1, 0, "Score", "Type B", 0.152944344084674, 1.0627396241062,
                                      "t", "Type C"))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 0, 1, 0, 0, 1, -1, 0))
})

## Ordered means and with effect sizes ----

options <- initClassicalAnovaOptions("Anova")
options$contrasts <- list(list(contrast = "none", variable = "Group"))
options$customContrasts <- list()
options$dependent <- "Age"
options$fixedFactors <- "Group"
options$modelTerms <- list(list(components = "Group"))
options$restrictedBootstrap <- FALSE
options$restrictedBootstrapCiLevel <- 0.95
options$restrictedBootstrapSamples <- 1000
options$restrictedInterceptInclusion <- FALSE
options$restrictedInformedHypothesisTestForAllModels <- FALSE
options$restrictedMarginalMeanForAllModels <- FALSE
options$restrictedModelComparison <- "none"
options$restrictedModelComparisonCoefficients <- FALSE
options$restrictedModelComparisonCoefficientsHighlight <- TRUE
options$restrictedModelComparisonMatrix <- FALSE
options$restrictedModelComparisonReference <- "Model 1"
options$restrictedModelComparisonWeights <- FALSE
options$restrictedModelMarginalMeanTerms <- list()
options$restrictedAvailableCoefficients <- FALSE
options$restrictedModelSummaryForAllModels <- FALSE
options$restrictedModels <- list(list(informedHypothesisTest = TRUE, marginalMean = FALSE,
                                      name = "Model 1", summary = TRUE, syntax = "GroupActive  < GroupPassive\nGroupPassive < GroupControl\nGroupControl < GroupNo"),
                                 list(informedHypothesisTest = TRUE, marginalMean = FALSE,
                                      name = "Model 2", summary = TRUE, syntax = "(GroupPassive - GroupActive)  / 1.516 > 0.2\n(GroupControl - GroupPassive) / 1.516 > 0.2\n(GroupNo      - GroupControl) / 1.516 > 0.2"))
options$restrictedHeterogeneityCorrection <- "none"
set.seed(1)
results <- jaspTools::runAnalysis("Anova", "ZelazoKolb1972.csv", options)


### see https://restriktor.org/tutorial/example1.html for comparison ----
test_that("Ordinal restrictions: Informative Hypothesis Tests table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_ihtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0308475934292769, 6.42666580972066, "F", "Type global", 0.0278151519017313,
                                      6.70126802148546, "LRT", "Type global", 0.0431126887497895,
                                      5.56056578050442, "Score", "Type global", 0.0308475934292769,
                                      6.42666580972066, "F", "Type A", 0.0278151519017313, 6.70126802148546,
                                      "LRT", "Type A", 0.0431126887497895, 5.56056578050442, "Score",
                                      "Type A", 1, 0, "F", "Type B", 1, 0, "LRT", "Type B", 1, 0,
                                      "Score", "Type B", 0.35381027878063, 0.380738874434879, "t",
                                      "Type C"))
})

test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("GroupActive", 10.125, 1.19077449127484e-12, 0.619065391589877,
                                      16.3552996784348, "GroupControl", 11.7083333333333, 8.7743489822692e-14,
                                      0.619065391589877, 18.91291855819, "GroupNo", 12.35, 1.73632870169313e-13,
                                      0.678152159089088, 18.2112522012594, "GroupPassive", 11.375,
                                      1.4783390885313e-13, 0.619065391589877, 18.3744724782415))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 0, 0, 1, 0, 0, 1, 0, -1, 0, 0, -1, 1, 0, 0))
})

### see https://restriktor.org/tutorial/example3.html for comparison
test_that("Ordinal restrictions: Informative Hypothesis Tests table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_ihtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.168856977319199, 2.38400146304897, "F", "Type global", 0.144342629920537,
                                      2.71869356602637, "LRT", "Type global", 0.163454084273826, 2.45267623450673,
                                      "Score", "Type global", 0.168856977319199, 2.38400146304897,
                                      "F", "Type A", 0.144342629920537, 2.71869356602637, "LRT", "Type A",
                                      0.163454084273826, 2.45267623450673, "Score", "Type A", 1, 0,
                                      "F", "Type B", 1, 0, "LRT", "Type B", 1, 0, "Score", "Type B",
                                      0.48645111069787, 0.0344187942489155, "t", "Type C"))
})

test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("GroupActive", 10.125, 1.19077449127485e-12, 0.619065391589877,
                                      16.3552996784348, "GroupControl", 11.7083333333333, 8.77434898226892e-14,
                                      0.619065391589876, 18.91291855819, "GroupNo", 12.35, 1.73632870169313e-13,
                                      0.678152159089088, 18.2112522012594, "GroupPassive", 11.375,
                                      1.47833908853122e-13, 0.619065391589875, 18.3744724782416))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.659630606860158, 0, 0, 0.659630606860158, 0.2, 0, 0.659630606860158,
                                      0, -0.659630606860158, 0.2, 0, -0.659630606860158, 0.659630606860158,
                                      0, 0.2))
})

### see https://restriktor.org/tutorial/example6.html for comparison ----
options <- initClassicalAnovaOptions("Anova")
options$contrasts <- list(list(contrast = "none", variable = "Group"))
options$customContrasts <- list()
options$dependent <- "Anger"
options$fixedFactors <- "Group"
options$modelTerms <- list(list(components = "Group"))
options$restrictedBootstrap <- FALSE
options$restrictedBootstrapCiLevel <- 0.95
options$restrictedBootstrapSamples <- 1000
options$restrictedInterceptInclusion <- FALSE
options$restrictedInformedHypothesisTestForAllModels <- FALSE
options$restrictedMarginalMeanForAllModels <- FALSE
options$restrictedModelComparison <- "unconstrained"
options$restrictedModelComparisonCoefficients <- FALSE
options$restrictedModelComparisonCoefficientsHighlight <- TRUE
options$restrictedModelComparisonMatrix <- FALSE
options$restrictedModelComparisonReference <- "Model 2"
options$restrictedModelComparisonWeights <- FALSE
options$restrictedModelMarginalMeanTerms <- list()
options$restrictedAvailableCoefficients <- FALSE
options$restrictedModelSummaryForAllModels <- FALSE
options$restrictedModels <- list(list(informedHypothesisTest = FALSE, marginalMean = FALSE,
                                      name = "Model 1", summary = FALSE, syntax = "GroupNo = GroupPhysical\nGroupPhysical = GroupBehavioral\nGroupBehavioral = GroupBoth"),
                                 list(informedHypothesisTest = FALSE, marginalMean = FALSE,
                                      name = "Model 2", summary = FALSE, syntax = "GroupNo < GroupPhysical\nGroupPhysical = GroupBehavioral\nGroupBehavioral < GroupBoth"),
                                 list(informedHypothesisTest = FALSE, marginalMean = FALSE,
                                      name = "Model 3", summary = FALSE, syntax = "GroupNo < GroupPhysical\nGroupPhysical < GroupBehavioral\nGroupBehavioral < GroupBoth"))
options$restrictedHeterogeneityCorrection <- "none"
set.seed(1)
results <- jaspTools::runAnalysis("Anova", "AngerManagement.csv", options)

test_that("Ordinal restrictions: Model Comparison Table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison_comparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(190.802338005453, 6.15799536281089e-06, -93.4011690027263, "Model 1",
                                      2, 174.107875497759, 0.025977408886342, -84.1621111968489, "Model 2",
                                      2.89182655203061, 167.133864718034, 0.849147292951048, -80.4838987099312,
                                      "Model 3", 3.08303364908579, 170.967797419862, 0.124869140167247,
                                      -80.4838987099312, "Unconstrained", 5))
})
