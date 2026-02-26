initClassicalAnovaOptions <- function(analysis = c("Anova", "Ancova", "AnovaRepeatedMeasures")) {
  analysis <- match.arg(analysis)
  options <- c(
    jaspTools::analysisOptions(analysis),
    classicalAnovaCommonOptions()
  )

  return(options)
}

classicalAnovaCommonOptions <- function() {
  # Find package root directory more robustly
  if (nzchar(Sys.getenv("R_COVR"))) {
    # Try installed package first
    pkg_path <- system.file("qml", "common", "classical", package = "jaspANOVA")
    if (nzchar(pkg_path)) {
      path <- pkg_path
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
      path <- file.path(current_dir, "qml", "common", "classical")
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
    path <- file.path(current_dir, "inst", "qml", "common", "classical")
    commonPath <- file.path(current_dir, "inst", "qml", "common")
  }
  
  if (!dir.exists(path)) {
    stop("Classical QML directory not found at: ", path, ". R_COVR=", Sys.getenv("R_COVR"), ", Working dir: ", getwd())
  }
  
  files <- list.files(path, full.names = TRUE)
  
  if (!dir.exists(commonPath)) {
    stop("Common QML directory not found at: ", commonPath, ". R_COVR=", Sys.getenv("R_COVR"))
  }
  
  files <- c(
    files,
    file.path(commonPath, "RainCloudPlots.qml"),
    file.path(commonPath, "BarPlots.qml"),
    file.path(commonPath, "Type.qml")
  )
  options <- lapply(files, jaspTools:::readQML) |>
    lapply(function(x) {x$plotWidth <- NULL; x$plotHeight <- NULL; return(x)}) |>
    (function(x) { do.call(c, x)})()

  return(options)
}

