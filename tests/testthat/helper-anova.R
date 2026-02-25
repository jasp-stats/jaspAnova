initClassicalAnovaOptions <- function(analysis = c("Anova", "Ancova", "AnovaRepeatedMeasures")) {
  analysis <- match.arg(analysis)
  options <- c(
    jaspTools::analysisOptions(analysis),
    classicalAnovaCommonOptions()
  )

  return(options)
}

classicalAnovaCommonOptions <- function() {
  path <- if (nzchar(Sys.getenv("R_COVR"))) {
    # We are running inside covr (installed package structure)
    system.file("qml", "common", "classical", package = "jaspANOVA")
  } else {
    testthat::test_path(file.path("..", "..", "inst", "qml", "common", "classical"))
  }
  
  files <- list.files(path, full.names = TRUE)
  
  commonPath <- if (nzchar(Sys.getenv("R_COVR"))) {
    # We are running inside covr (installed package structure)
    system.file("qml", "common", package = "jaspANOVA")
  } else {
    testthat::test_path(file.path("..", "..", "inst", "qml", "common"))
  }
  
  files <- c(
    files,
    file.path(commonPath, "RainCloudPlots.qml")
  )
  options <- lapply(files, jaspTools:::readQML) |>
    lapply(function(x) {x$plotWidth <- NULL; x$plotHeight <- NULL; return(x)}) |>
    (function(x) { do.call(c, x)})()

  return(options)
}

