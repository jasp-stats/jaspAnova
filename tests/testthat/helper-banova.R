
initOpts <- function(analysisName) {
  options <- jaspTools::analysisOptions(analysisName)
  addCommonQMLoptions(options)
  options$sampleModeNumAcc <- "manual"
  options$fixedNumAcc <- 50
  if (analysisName == "AnovaRepeatedMeasuresBayesian")
    options$legacy <- TRUE
}

addCommonQMLoptions <- function(options) {
  # jaspTools doesn't recognize common QML elements so this function adds the defaults manually
  options$bayesFactorOrder <- "bestModelTop"
  options$enforcePrincipleOfMarginality <- TRUE
  options$modelPrior <- "uniform"
  options$betaBinomialParamA <- options$betaBinomialParamB <- options$wilsonParamLambda <- options$castilloPAramU <- 1

  options$sampleModeNumAcc <- "auto"
  options$sampleModeMCMC   <- "auto"

  options$priorFixedEffects  <- 0.5
  options$priorRandomEffects <- 1.0
  options$priorCovariates    <- 0.354

  options
}
