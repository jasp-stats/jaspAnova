
initOpts <- function(analysisName) {
  options <- jaspTools::analysisOptions(analysisName)
  options$sampleModeNumAcc <- "manual"
  options$fixedNumAcc <- 50
  if (analysisName == "AnovaRepeatedMeasuresBayesian")
    options$legacy <- TRUE
  addCommonQMLoptions(options)
}

addCommonQMLoptions <- function(options) {
  # jaspTools doesn't recognize common QML elements so this function adds the defaults manually
  options$bayesFactorOrder <- "bestModelTop"
  options$modelSpaceType <- "type 2"
  options$modelPrior <- "uniform"
  options$betaBinomialParamA <- options$betaBinomialParamB <-
    options$wilsonParamLambda <- options$castilloPAramU <- 1
  options
}
