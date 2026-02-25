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
  pkg_root <- if (nzchar(Sys.getenv("R_COVR"))) {
    # Try installed package first
    pkg_path <- system.file("qml", "common", "classical", package = "jaspANOVA")
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
  
  path <- file.path(pkg_root, "inst", "qml", "common", "classical")
  
  if (!dir.exists(path)) {
    stop("Classical QML directory not found at: ", path, ". R_COVR=", Sys.getenv("R_COVR"), ", Working dir: ", getwd())
  }
  
  files <- list.files(path, full.names = TRUE)
  
  commonPath <- file.path(pkg_root, "inst", "qml", "common")
  
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

