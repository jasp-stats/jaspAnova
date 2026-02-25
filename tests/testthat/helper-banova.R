
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
  root <- if (nzchar(Sys.getenv("R_COVR"))) {
    # We are running inside covr (installed package structure)
    pkg_path <- system.file("qml", "common", "bayesian", package = "jaspANOVA")
    if (nzchar(pkg_path)) {
      pkg_path
    } else {
      # Fallback if package not found
      testthat::test_path(file.path("..", "..", "inst", "qml", "common", "bayesian"))
    }
  } else {
    testthat::test_path(file.path("..", "..", "inst", "qml", "common", "bayesian"))
  }
  
  if (!dir.exists(root)) {
    stop("Bayesian QML directory not found at: ", root, ". R_COVR=", Sys.getenv("R_COVR"))
  }
  
  # Also get the common path for shared QML files
  commonPath <- if (nzchar(Sys.getenv("R_COVR"))) {
    # We are running inside covr (installed package structure)
    pkg_path <- system.file("qml", "common", package = "jaspANOVA")
    if (nzchar(pkg_path)) {
      pkg_path
    } else {
      # Fallback if package not found
      testthat::test_path(file.path("..", "..", "inst", "qml", "common"))
    }
  } else {
    testthat::test_path(file.path("..", "..", "inst", "qml", "common"))
  }
  
  if (!dir.exists(commonPath)) {
    stop("Common QML directory not found at: ", commonPath, ". R_COVR=", Sys.getenv("R_COVR"))
  }
  c(
    options,
    jaspTools:::readQML(file.path(root, "DefaultOptions.qml")),
    jaspTools:::readQML(file.path(root, "ModelTerms.qml")),
    jaspTools:::readQML(file.path(root, "SingleModelInference.qml")),
    jaspTools:::readQML(file.path(root, "DescriptivesPlots.qml")),
    jaspTools:::readQML(file.path(root, "AdditionalOptions.qml")),
    jaspTools:::readQML(file.path(root, "BayesFactorOrder.qml")),
    jaspTools:::readQML(file.path(root, "PostHocTests.qml")),
    jaspTools:::readQML(file.path(commonPath, "RainCloudPlots.qml")),
    jaspTools:::readQML(file.path(commonPath, "BarPlots.qml")),
    jaspTools:::readQML(file.path(commonPath, "Type.qml"))
  )
}
