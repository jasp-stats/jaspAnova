context("ANCOVA")

# does not test
# - descriptives table/plot & Q-Q plot & bar plot & raincloud plot (uses same code as ANOVA)
# - if analysis handles too few observations

# Main results ----
test_that("Main table results match", {
  options <- initClassicalAnovaOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$covariates <- "contGamma"
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contGamma")
  )
  options$effectSizeEstimates <- TRUE
  options$effectSizeEtaSquared <- TRUE
  options$effectSizeOmegaSquared <- TRUE
  options$effectSizePartialEtaSquared <- TRUE
  options$vovkSellke <- TRUE

  refTables <- list(
    type1 = list("facFive", 181.151987151139, 4, 45.2879967877848, 1.86433860843651,
                 0.123166554163148, "TRUE", 0.0733818170125722, 0.0735023545150279,
                 0.0336895280828883, 1.42623435171942, "contGamma", 4.04832694150032,
                 1, 4.04832694150032, 0.166654582934621, 0.684030683889986, "FALSE",
                 0.00163991348646033, 0.00176978340871019, 0, 1, "Residuals",
                 2283.42194856002, 94, 24.2917228570215, "", "", "TRUE", "",
                 "", "", ""),
    type2 = list("facFive", 185.196464516179, 4, 46.2991161290447, 1.90596263597919,
                 0.115857414147638, "TRUE", 0.0748974625326393, 0.0750202880830827,
                 0.0352547206519188, 1.47317659150752, "contGamma", 4.04832694150036,
                 1, 4.04832694150036, 0.166654582934623, 0.684030683889984, "FALSE",
                 0.00163723112216545, 0.00176978340871021, 0, 1, "Residuals",
                 2283.42194856002, 94, 24.2917228570215, "", "", "TRUE", "",
                 "", "", ""),
    type3 = list("TRUE", 4, 1.9059626359792, 46.2991161290448, 0.115857414147637,
                 185.196464516179, "facFive", 0.0748974625326395, 0.035254720651919,
                 0.0750202880830829, 1.47317659150752, "FALSE", 1, 0.166654582934623,
                 4.04832694150036, 0.684030683889984, 4.04832694150036, "contGamma",
                 0.00163723112216545, 0, 0.00176978340871021, 1, "TRUE", 94,
                 "", 24.2917228570215, "", 2283.42194856002, "Residuals", "",
                 "", "", "")
  )

  for (type in c("type1", "type2", "type3")) {
    options$sumOfSquares <- type
    results <- jaspTools::runAnalysis("Ancova", "test.csv", options)
    table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
    jaspTools::expect_equal_tables(table, refTables[[type]], label=paste("Table with SS", type))
  }
})

test_that("Main table results match for three-way ANOVA", {
  options <- initClassicalAnovaOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facFive", "contBinom", "facGender")
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contBinom"),
    list(components="facGender"),
    list(components="contGamma"),
    list(components=c("facFive", "contBinom")),
    list(components=c("facFive", "facGender")),
    list(components=c("contBinom", "facGender")),
    list(components=c("facFive", "contBinom", "facGender"))
  )
  options$covariates <- "contGamma"
  options$effectSizeEstimates <- TRUE
  options$effectSizeEtaSquared <- TRUE
  options$effectSizeOmegaSquared <- TRUE
  options$effectSizePartialEtaSquared <- TRUE
  options$sumOfSquares <- "type3"

  results <- jaspTools::runAnalysis("Ancova", "test.csv", options)
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_anovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("TRUE", 4, 0.751434182416166, 0.849676498436303, 0.560010211401305,
                                             3.39870599374521, "facFive", 0.0319287041344807, 0, 0.036652761739989,
                                             "FALSE", 1, 0.0718225589534213, 0.0812126222473069, 0.789399900467283,
                                             0.0812126222473069, "contBinom", 0.000762941481990983, 0, 0.000908320519612062,
                                             "FALSE", 1, 4.03921343627838, 4.56729918505773, 0.0478686068240796,
                                             4.56729918505773, "facGender", 0.0429069018154959, 0.0319449750880258,
                                             0.0486422410464899, "TRUE", 4, 0.80133099599275, 0.906096808870625,
                                             0.527940897125054, 3.6243872354825, "facFive <unicode> facGender",
                                             0.0340488373879581, 0, 0.038991683611587, "FALSE", 1, 1.08113657469412,
                                             1.22248409855909, 0.301615301439538, 1.22248409855909, "contBinom <unicode> facGender",
                                             0.0114844688430931, 0.000852821268290123, 0.0135005148645175,
                                             "TRUE", 4, 0.702807193128186, 0.79469202879871, 0.592348641591512,
                                             3.17876811519484, "facFive <unicode> contBinom <unicode> facGender",
                                             0.0298625261640639, 0, 0.0343623829478194, "TRUE", 1, 0.0608023312347904,
                                             0.0687516127283772, 0.805871897218356, 0.0687516127283772, "contGamma",
                                             0.000645878138801233, 0, 0.000769057857268532, "TRUE", 4, 0.215937435642924,
                                             0.244169041669679, 0.928844739039309, 0.976676166678715, "facFive <unicode> contBinom",
                                             0.00917525800068401, 0, 0.0108152916104723, "TRUE", 79, "",
                                             1.13073974849567, "", 89.3284401311577, "Residuals", "", "",
                                             ""))

})

# Additional results ----
test_that("Homogeneity of Variances table results match", {
  options <- initClassicalAnovaOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facExperim"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="contGamma")
  )
  options$homogeneityTests <- TRUE
  options$vovkSellke <- TRUE
  results <- jaspTools::runAnalysis("Ancova", "test.csv", options)
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_assumptionsContainer"]][["collection"]][["anovaContainer_assumptionsContainer_leveneTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(2.72159218177061, 1, 98, 0.102201011380302, 1.57819444559362))
})

# Contrasts verified with SPSS
# should we put this in verification project???
test_that("Contrasts table results match", {
  options <- initClassicalAnovaOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contGamma")
  )
  options$contrastCi <- TRUE
  refTables <- list(
    deviation = list("2 - 1, 2, 3, 4, 5", -0.200720248979633, 0.213572066533121, -0.939824445386938,
                     0.349716429527485, 94, -0.6247726, 0.2233321, "TRUE", "3 - 1, 2, 3, 4, 5", 0.326355030521638,
                     0.213419672724688, 1.52917032603005, 0.129580520593874, 94, -0.09739473, 0.7501048, "FALSE",
                     "4 - 1, 2, 3, 4, 5", -0.163006179525351, 0.213771994762986,
                     -0.762523546202018, 0.447656831363019, 94, -0.5874555, 0.2614431, "FALSE", "5 - 1, 2, 3, 4, 5",
                     0.17396802464162, 0.214943819683978, 0.809365093155025, 0.42034917204134, 94, -0.252808, 0.600744,
                     "FALSE"),
    simple = list("2 - 1", -0.0641236223213591, 0.343277644425165, -0.186798130792167,
                  0.852221484436515, 94,  -0.7457094,  0.6174622, "TRUE", "3 - 1", 0.462951657179913, 0.342949539026206,
                  1.34991188060625, 0.180285987179755, 94,  -0.2179827, 1.1438860, "FALSE", "4 - 1", -0.0264095528670768,
                  0.337118098977225, -0.0783391723766838, 0.937724778659558, 94, -0.6957654, 0.6429463, "FALSE",
                  "5 - 1", 0.310564651299894, 0.345720813950643, 0.898310540667163,
                  0.371315247360673, 94,  -0.3758721, 0.9970014, "FALSE"),
    difference = list("2 - 1", -0.0641236223213588, 0.343277644425165, -0.186798130792167,
                      0.852221484436515, 94, -0.7457094,  0.6174622, "TRUE", "3 - 1, 2", 0.495013468340592, 0.292959120347178,
                      1.68970151109808, 0.0943992176806134, 94, -0.08666373, 1.07669067, "FALSE", "4 - 1, 2, 3",
                      -0.159352231153261, 0.275116843578239, -0.579216557883866, 0.563828556165238, 94, -0.7056032, 0.3868987,
                      "FALSE", "5 - 1, 2, 3, 4", 0.217460030802025, 0.268679774604972,
                      0.809365093155025, 0.42034917204134, 94,  -0.31601, 0.75093, "FALSE"),
    Helmert = list("1 - 2, 3, 4, 5", -0.170745783322843, 0.272701151705157, -0.626127840880747,
                   0.532748563022963, 94, -0.7122003,  0.3707087, "TRUE", "2 - 3, 4, 5", -0.313159207525603,
                   0.274645898379152, -1.14022896163293, 0.257087871753455, 94, -0.8584751,  0.2321567, "FALSE",
                   "3 - 4, 5", 0.320874107963505, 0.291421643235482, 1.10106478160314,
                   0.273679968781506, 94, -0.2577504,  0.8994986, "FALSE", "4 - 5", -0.336974204166971, 0.340503320254199,
                   -0.98963558979515, 0.324892868709605, 94, -1.0130515,  0.3391031, "FALSE"),
    repeated = list("1 - 2", 0.0641236223213587, 0.343277644425165, 0.186798130792166,
                    0.852221484436516, 94,  -0.6174622,  0.7457094, "TRUE", "2 - 3", -0.527075279501272, 0.336088495268646,
                    -1.56826337979812, 0.120179976824791, 94,  -1.1943869,  0.1402363, "FALSE", "3 - 4", 0.489361210046989,
                    0.338686060204421, 1.444881462649, 0.151816532688129, 94,  -0.1831079,  1.1618303, "FALSE",
                    "4 - 5", -0.336974204166971, 0.340503320254199, -0.98963558979515,
                    0.324892868709605, 94,  -1.0130515,  0.3391031, "FALSE"),
    polynomial = list("linear", 0.208344567699659, 0.240588115184784, 0.865980298069335,
                      0.388705894709638, 94, -0.2693488,  0.6860379, "TRUE", "quadratic", -0.0572582720504826,
                      0.238398138250912, -0.240179191291413, 0.810714351679878, 94, -0.5306034,  0.4160868, "FALSE",
                      "cubic", 0.0743566940225005, 0.243521087108506, 0.305339857444745,
                      0.760782433052004, 94,  -0.4091601,  0.5578735, "FALSE", "quartic", 0.412402551299919, 0.237982457186854,
                      1.73291156068751, 0.0863908131438485, 94,  -0.0601172, 0.8849223, "FALSE")
  )

  contrasts <- c("deviation", "simple", "difference", "Helmert", "repeated", "polynomial")
  for (contrast in contrasts) {
    options$contrasts <- list(list(contrast=contrast, variable="facFive"))
    results <- jaspTools::runAnalysis("Ancova", "test.csv", options)
    # table <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
    table <- results[["results"]]$anovaContainer$collection$anovaContainer_contrastContainer$collection[[1]]$collection[[1]]$data
    jaspTools::expect_equal_tables(table, refTables[[contrast]], label=paste("Table with contrast", contrast))
  }
})

test_that("Post Hoc table results match", {
  options <- initClassicalAnovaOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facExperim"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="contGamma")
  )
  options$postHocTerms                  <- list("facExperim")
  options$postHocTypeStandard           <- TRUE
  options$postHocTypeStandardEffectSize <- TRUE
  options$postHocCorrectionBonferroni   <- TRUE
  options$postHocCorrectionHolm         <- TRUE
  options$postHocCorrectionScheffe      <- TRUE
  options$postHocCorrectionTukey        <- TRUE
  options$postHocCorrectionSidak        <- TRUE
  options$postHocCi                     <- TRUE
  results <- jaspTools::runAnalysis("Ancova", "test.csv", options)
  table <- results$results$anovaContainer$collection$anovaContainer_postHocContainer$collection$anovaContainer_postHocContainer_postHocStandardContainer$collection[[1]]$data
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 0.21391801479091, 0.698555762823947, -0.0779045273305165,
                                      -0.476129796984023, 0.32032074232299, "control", "experimental",
                                      -0.0830902357515325, 0.698555762823947, -0.507658279613134, 97,
                                      0.698555762823947, 0.698555762823947, -0.388420937024623, 0.698555762823947,
                                      0.341477808110069)
  )
})

test_that("Marginal Means table results match", {
  options <- initClassicalAnovaOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facExperim"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="contGamma")
  )
  options$marginalMeanComparedToZero <- TRUE
  options$marginalMeanTerms <- "facExperim"

  refTables <- list(
    none = list("control", -0.230293705415766, 0.151049119466849, -0.530084395048618, 97,
                0.0694969842170856, -1.52462792387419, 0.13060580966841, "TRUE",
                "experimental", -0.147203469664234, 0.151049119466849, -0.446994159297086, 97,
                0.152587219968618, -0.974540402379113, 0.332212375969363, "FALSE"),
    Bonferroni = list("control", -0.230293705415766, 0.151049119466849, -0.530084395048618,97,
                      0.0694969842170856, -1.52462792387419, 0.13060580966841, "TRUE",
                      "experimental", -0.147203469664234, 0.151049119466849, -0.446994159297086, 97,
                      0.152587219968618, -0.974540402379113, 0.332212375969363, "FALSE"),
    Sidak = list("control", -0.230293705415766, 0.151049119466849, -0.530084395048618, 97,
                 0.0694969842170856, -1.52462792387419, 0.13060580966841, "TRUE",
                 "experimental", -0.147203469664234, 0.151049119466849, -0.446994159297086, 97,
                 0.152587219968618, -0.974540402379113, 0.332212375969363, "FALSE")
  )

  for (adjustment in c("none", "Bonferroni", "Sidak")) {
    options$marginalMeanCiCorrection <- adjustment
    results <- jaspTools::runAnalysis("Ancova", "test.csv", options)
    table <- results[["results"]]$anovaContainer$collection$anovaContainer_marginalMeansContainer$collection[[1]]$data
    jaspTools::expect_equal_tables(table, refTables[[adjustment]], label=paste("Table with CI adjustment", adjustment))
  }
})

test_that("Simple Main Effects table results match", {
  options <- initClassicalAnovaOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c( "facFive", "facExperim")
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="facFive"),
    list(components="contGamma")
  )
  options$simpleMainEffectFactor             <- "facExperim"
  options$simpleMainEffectModeratorFactorOne <- "facFive"
  options$simpleMainEffectModeratorFactorTwo <- ""
  options$homogeneityTests <- TRUE
  options$sumOfSquares <- "type1"
  options$vovkSellke <- TRUE
  results <- jaspTools::runAnalysis("Ancova", "debug.csv", options)
  # table <- results[["results"]][["simpleEffects"]][["data"]]
  table <- results$results$anovaContainer$collection$anovaContainer_simpleEffectsContainer$collection$anovaContainer_simpleEffectsContainer_simpleEffectsTable$data
  jaspTools::expect_equal_tables(table, list(1, 0.350864897951646, 1, 0.350864897951646, 0.307765411627339,
                                  0.580386465552355, "TRUE", 2, 2.72259751707838, 1, 2.72259751707838,
                                  2.38815951789705, 0.125653693703876, "FALSE", 3, 0.300954391532799,
                                  1, 0.300954391532799, 0.263985804028512, 0.608613599742434,
                                  "FALSE", 4, 3.47907983036715, 1, 3.47907983036715, 3.05171717754702,
                                  0.0839531695276169, "FALSE", 5, 0.313611321775938, 1, 0.313611321775938,
                                  0.275087984294933, 0.601186887502708, "FALSE"))
})

# Error handling ----
test_that("Analysis handles errors", {

  # Same as ANOVA

 options <- initClassicalAnovaOptions("Ancova")
 options$dependent <- "contNormal"
 options$covariates <- "debInf"
 options$fixedFactors <- "contBinom"
 options$modelTerms <- list(list(components="contBinom"))
 results <- jaspTools::runAnalysis("Ancova", "test.csv", options)
 expect_identical(results[["results"]][["errorMessage"]],
                  "The following problem(s) occurred while running the analysis:<ul><li>Infinity found in debInf</li></ul>",
                  label="Inf covariate check")

})

# Ordinal Restrictions ----
# this test also tests anova restrictions
options <- initClassicalAnovaOptions("Ancova")
options$contrasts <- list(list(contrast = "none", variable = "contBinom"))
options$covariates <- "contcor1"
options$customContrasts <- list()
options$dependent <- "contNormal"
options$fixedFactors <- "contBinom"
options$modelTerms <- list(list(components = "contBinom"), list(components = "contcor1"))
options$rainCloudHorizontalAxis <- ""
options$rainCloudHorizontalDisplay <- FALSE
options$rainCloudYAxisLabel <- ""
options$rainCloudSeparatePlots <- ""
options$restrictedBootstrap <- TRUE
options$restrictedBootstrapCiLevel <- 0.95
options$restrictedBootstrapSamples <- 100
options$restrictedInterceptInclusion <- TRUE
options$restrictedAvailableCoefficients <- TRUE
options$restrictedInformedHypothesisTestForAllModels <- TRUE
options$restrictedMarginalMeanForAllModels <- TRUE
options$restrictedModelSummaryForAllModels <- TRUE
options$restrictedModelComparison <- "unconstrained"
options$restrictedModelComparisonCoefficients <- TRUE
options$restrictedModelComparisonCoefficientsHighlight <- TRUE
options$restrictedModelComparisonMatrix <- TRUE
options$restrictedModelComparisonReference <- "Model 2"
options$restrictedModelComparisonWeights <- TRUE
options$restrictedMarginalMeanTerms <- list(list(variable = "contBinom"))
options$restrictedModels <- list(list(informedHypothesisTest = TRUE, marginalMean = TRUE,
                                      name = "Model 1", summary = TRUE, syntax = ".Intercept. == 0"),
                                 list(informedHypothesisTest = TRUE, marginalMean = TRUE,
                                      name = "Model 2", summary = TRUE, syntax = ".Intercept. > 0"),
                                 list(informedHypothesisTest = TRUE, marginalMean = TRUE,
                                      name = "Model 3", summary = TRUE, syntax = ".Intercept. == 0\ncontBinom1 > 0"))
options$restrictedHeterogeneityCorrection <- "none"
set.seed(1)
results <- jaspTools::runAnalysis("Ancova", "test.csv", options)


## Models with equality restrictions only ----
test_that("Ordinal restrictions: Informative Hypothesis Tests table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_ihtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.280525836689152, 1.17764407696846, "F", "Type classical", 0.277836137015615,
                                      1.17764407696846, "Wald", "Type classical", 0.278271309124288,
                                      1.17551323040948, "Score", "Type classical"))
})

test_that("Ordinal restrictions: Marginal means table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer_contBinom"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00631082413156547, 0, -0.0052940718699722, 0.00710593238617753,
                                      0.0201427487900576, 0.152623965297405, 1, -0.541673449142385,
                                      -0.283815957286218, 0.0347949556202055))
})

test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(".Intercept.", 0, 0, 0, 0, "contBinom1", -0.28901764982443, -0.545890974144477,
                                      0.152410269162387, 0.0216009611708116, "contcor1", 0.135225724991321,
                                      -0.100746062285325, 0.120094833739718, 0.383316032355158))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0, 0, 0))
})

## Models with in-equality restrictions only ----
test_that("Ordinal restrictions: Informative Hypothesis Tests table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_ihtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.141270077631025, 4.86463661917856, "F", "Type global", 0.143009738633418,
                                      4.83608638110945, "LRT", "Type global", 0.150214932639365, 4.72138311792181,
                                      "Score", "Type global", 1, 0, "F", "Type A", 1, 0, "LRT", "Type A",
                                      0.495597802688024, 0.000122398295544508, "Score", "Type A",
                                      0.140262918344576, 1.17764407696847, "F", "Type B", 0.13734744589723,
                                      1.20675538818887, "LRT", "Type B", 0.141706057440304, 1.16351819744612,
                                      "Score", "Type B", 0.859737081655424, -1.08519310584267, "t",
                                      "Type C"))
})

test_that("Ordinal restrictions: Marginal means table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_marginalMeansContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_marginalMeansContainer_contBinom"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0401499620148025, 0, -0.0052940718699722, 0.00762632152811411,
                                      0.147781545335185, 0.15219130722473, 1, -0.541673449142385,
                                      -0.285158551298243, 0.0345874620687721))
})

test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(".Intercept.", 0, 0, 0.0390913797847908, 0.135449151526049, "contBinom1",
                                      -0.294040143250604, -0.553042198939247, 0.15095032207897, 0.0149620362407961,
                                      "contcor1", 0.133967448915338, -0.100746062285325, 0.119436094843916,
                                      0.367522848917437))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 2_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0, 0, 0))
})

## Models with in-equality and equality restrictions ----
test_that("Ordinal restrictions: Informative Hypothesis Tests table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3_ihtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.227906731091751, 2.29055827266306, "F", "Type global", 0.233102480397295,
                                      2.24800995009798, "LRT", "Type global", 0.236222676760002, 2.22294941447971,
                                      "Score", "Type global", 1, 0, "F", "Type A", 1, 0, "LRT", "Type A",
                                      0.492283243845526, 0.00037613437452225, "Score", "Type A", 0.107200891136129,
                                      3.75172242348396, "F", "Type B", 0.104890547890444, 3.79483181920034,
                                      "LRT", "Type B", 0.112908725975451, 3.64925570161498, "Score",
                                      "Type B"))
})

test_that("Ordinal restrictions: Marginal means table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3_marginalMeansContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3_marginalMeansContainer_contBinom"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00615640047820512, 0, -0.0040934464864444, 0.00784355639438009,
                                      0.021670734045724, 0.0126705139715333, 1, -0.0040934464864444,
                                      0.00795127192950033, 0.0347949556202055))
})

test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(".Intercept.", 0, 0, 0, 0, "contBinom1", 0, 0, 0.0115253766048944,
                                      0.0216009611708116, "contcor1", 0.149262692395377, -0.077898189675906,
                                      0.117156155274091, 0.41239355557723))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3_modelSummaryContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 3_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0, 0, 0, 0, 1, 0, 0))
})

## Model comparison ----
test_that("Ordinal restrictions: Coefficients Comparison table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 0, 0, 0, ".Intercept.", -0.151936095480945, 0, 0, 0,
                                      -0.262153994575154, -0.262153994575154, 0, "contBinom1", -0.107591826354404,
                                      0, 0, 0, 0.141934956659658, 0.141934956659658, 0.158227556203348,
                                      "contcor1", 0.159396509032993))
})

test_that("Ordinal restrictions: Relative GORIC-Weights table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison_comparisonMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1.64872127070013, 2.21231220567314, 1.48679427057462, "Model 1",
                                      0.606530659712633, 1, 1.34183518159724, 0.901786309788586, "Model 2",
                                      0.45201576768218, 0.745248010869458, 1, 0.672054453599252, "Model 3",
                                      0.672588010184839, 1.10891015880962, 1.48797466432133, 1, "Unconstrained"
                                 ))
})

test_that("Ordinal restrictions: Model Comparison Table results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison"]][["collection"]][["anovaContainer_ordinalRestrictions_modelComparison_comparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(298.462647640822, 0.366148215276508, -146.231323820411, "Model 1",
                                      3, 1.64872127070013, 299.462647640822, 0.222080118564264, -146.231323820411,
                                      "Model 2", 3.5, 1, 300.050724071833, 0.165504766613671, -147.525362035916,
                                      "Model 3", 2.5, 0.745248010869458, 299.255892252633, 0.246266899545557,
                                      -145.627946126316, "Unconstrained", 4, 1.10891015880962))
})


## see https://restriktor.org/tutorial/example2.html for comparison ----
options <- initClassicalAnovaOptions("Ancova")
options$contrasts <- list(list(contrast = "none", variable = "Group"))
options$covariates <- "Age"
options$customContrasts <- list()
options$dependent <- "Anger"
options$fixedFactors <- "Group"
options$modelTerms <- list(list(components = "Group"), list(components = "Age"))
options$rainCloudHorizontalAxis <- ""
options$rainCloudHorizontalDisplay <- FALSE
options$rainCloudYAxisLabel <- ""
options$rainCloudSeparatePlots <- ""
options$restrictedBootstrap <- FALSE
options$restrictedBootstrapCiLevel <- 0.95
options$restrictedBootstrapSamples <- 1000
options$restrictedInterceptInclusion <- FALSE
options$restrictedAvailableCoefficients <- FALSE
options$restrictedInformedHypothesisTestForAllModels <- FALSE
options$restrictedMarginalMeanForAllModels <- FALSE
options$restrictedModelSummaryForAllModels <- FALSE
options$restrictedModelComparison <- "none"
options$restrictedModelComparisonCoefficients <- FALSE
options$restrictedModelComparisonCoefficientsHighlight <- TRUE
options$restrictedModelComparisonMatrix <- FALSE
options$restrictedModelComparisonReference <- "Model 1"
options$restrictedModelComparisonWeights <- FALSE
options$restrictedMarginalMeanTerms <- list(list(variable = "Group"))
options$restrictedModels <- list(list(informedHypothesisTest = FALSE, marginalMean = TRUE,
                                      name = "Model 1", summary = FALSE, syntax = "GroupNo < GroupPhysical\nGroupPhysical  == GroupBehavioral\nGroupBehavioral < GroupBoth"))
options$restrictedHeterogeneityCorrection <- "none"
set.seed(1)
results <- jaspTools::runAnalysis("Ancova", "AngerManagement.csv", options)

test_that("Ordinal restrictions: Adjuested marginal means results match", {
  table <- results[["results"]][["anovaContainer"]][["collection"]][["anovaContainer_ordinalRestrictions"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer"]][["collection"]][["anovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer_Group"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Behavioral", 0.467592714510265, 1.95108158220025, "Both", 0.702820099125852,
                                      4.06863411619283, "No", 0.69741742112576, -0.17079728059332,
                                      "Physical", 0.467592714510269, 1.95108158220025))
})
