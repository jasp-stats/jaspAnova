
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
  pkg_root <- if (nzchar(Sys.getenv("R_COVR"))) {
    # Try installed package first
    pkg_path <- system.file("qml", "common", "bayesian", package = "jaspANOVA")
    if (nzchar(pkg_path)) {
      return(pkg_path)
    } else {
      # Find package root by looking for DESCRIPTION file
      current_dir <- getwd()
      while (!file.exists(file.path(current_dir, "DESCRIPTION"))) {
        parent_dir <- dirname(current_dir)
        if (parent_dir == current_dir) {
          stop("Could not find package root directory")
        }
        current_dir <- parent_dir
      }
      current_dir
    }
  } else {
    # Find package root by looking for DESCRIPTION file
    current_dir <- getwd()
    while (!file.exists(file.path(current_dir, "DESCRIPTION"))) {
      parent_dir <- dirname(current_dir)
      if (parent_dir == current_dir) {
        stop("Could not find package root directory")
      }
      current_dir <- parent_dir
    }
    current_dir
  }
  
  root <- file.path(pkg_root, "inst", "qml", "common", "bayesian")
  
  if (!dir.exists(root)) {
    stop("Bayesian QML directory not found at: ", root, ". R_COVR=", Sys.getenv("R_COVR"), ", Working dir: ", getwd())
  }
  
  # Also get the common path for shared QML files
  commonPath <- file.path(pkg_root, "inst", "qml", "common")
  
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
