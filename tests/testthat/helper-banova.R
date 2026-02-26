
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
  # Find package root directory more robustly
  if (nzchar(Sys.getenv("R_COVR"))) {
    # Try installed package first
    pkg_path <- system.file("qml", "common", "bayesian", package = "jaspANOVA")
    if (nzchar(pkg_path)) {
      root <- pkg_path
      commonPath <- system.file("qml", "common", package = "jaspANOVA")
    } else {
      # Fallback: find package root and use installed structure (no inst/)
      current_dir <- getwd()
      while (!file.exists(file.path(current_dir, "DESCRIPTION"))) {
        parent_dir <- dirname(current_dir)
        if (parent_dir == current_dir) {
          stop("Could not find package root directory")
        }
        current_dir <- parent_dir
      }
      root <- file.path(current_dir, "qml", "common", "bayesian")
      commonPath <- file.path(current_dir, "qml", "common")
    }
  } else {
    # Development: find package root and use source structure (with inst/)
    current_dir <- getwd()
    while (!file.exists(file.path(current_dir, "DESCRIPTION"))) {
      parent_dir <- dirname(current_dir)
      if (parent_dir == current_dir) {
        stop("Could not find package root directory")
      }
      current_dir <- parent_dir
    }
    root <- file.path(current_dir, "inst", "qml", "common", "bayesian")
    commonPath <- file.path(current_dir, "inst", "qml", "common")
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
