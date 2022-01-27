
initOpts <- function(analysisName) {
  options <- jaspTools::analysisOptions(analysisName)
  options <- addCommonQMLoptions(options)

  options$sampleModeNumAcc <- "manual"
  options$fixedNumAcc <- 50
  if (analysisName == "AnovaRepeatedMeasuresBayesian")
    options$legacy <- TRUE
  options
}

addCommonQMLoptions <- function(options) {
  # jaspTools doesn't recognize common QML elements so this function adds the defaults manually
  root <- testthat::test_path(file.path("..", "..", "inst", "qml", "common"))
  c(
    options,
    jaspTools:::readQML(file.path(root, "DefaultOptions.qml")),
    jaspTools:::readQML(file.path(root, "ModelTerms.qml")),
    jaspTools:::readQML(file.path(root, "SingleModelInference.qml")),
    jaspTools:::readQML(file.path(root, "DescriptivesPlots.qml")),
    jaspTools:::readQML(file.path(root, "AdditionalOptions.qml"))
  )
}
