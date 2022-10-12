
initOpts <- function(analysisName) {
  options <- jaspTools::analysisOptions(analysisName)
  options <- addCommonQMLoptions(options)
  options$modelsShown <- "unlimited"

  # avoid that BayesFactor shows progress bars
  options("BFprogress" = FALSE)

  options$samplingMethodNumericAccuracy <- "manual"
  options$samplesNumericAccuracy <- 50
  if (analysisName == "AnovaRepeatedMeasuresBayesian")
    options$legacyResults <- TRUE
  options
}

addCommonQMLoptions <- function(options) {
  # jaspTools doesn't recognize common QML elements so this function adds the defaults manually
  root <- testthat::test_path(file.path("..", "..", "inst", "qml", "common", "bayesian"))
  c(
    options,
    jaspTools:::readQML(file.path(root, "DefaultOptions.qml")),
    jaspTools:::readQML(file.path(root, "ModelTerms.qml")),
    jaspTools:::readQML(file.path(root, "SingleModelInference.qml")),
    jaspTools:::readQML(file.path(root, "DescriptivesPlots.qml")),
    jaspTools:::readQML(file.path(root, "AdditionalOptions.qml"))
  )
}
