

.computeBonferroniConfidence <- function(confidenceLevel, numberOfLevels) {

  bonfAdjustCIlevel <- 1 - ((1-confidenceLevel) /
                              choose(numberOfLevels, 2))
  return(bonfAdjustCIlevel)
}


.createContrastTableAnova <- function(myTitle, options, dfType = "integer") {

  contrastTable <- createJaspTable(title = myTitle)
  contrastTable$addColumnInfo(name = "Comparison", type = "string")
  contrastTable$addColumnInfo(name = "estimate", title=gettext("Estimate"), type = "number")

  if (options$contrastCi) {

    thisOverTitle <- gettextf("%s%% CI for Mean Difference", options$contrastCiLevel * 100)
    contrastTable$addColumnInfo(name="lower.CL", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
    contrastTable$addColumnInfo(name="upper.CL", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)

  }

  contrastTable$addColumnInfo(name = "SE", title=gettext("SE"), type = "number")
  contrastTable$addColumnInfo(name = "df",      title = gettext("df"), type = dfType)
  contrastTable$addColumnInfo(name = "t.ratio", title = gettext("t"),  type = "number")
  contrastTable$addColumnInfo(name = "p.value", title = gettext("p"),  type = "pvalue")

  if (isTRUE(options$contrastEffectSize)) {
    contrastTable$addColumnInfo(name="cohenD", title=gettext("Cohen's d"), type="number")

    if (isTRUE(options$contrastCi)) {
      thisOverTitleCohenD <- gettextf("%s%% CI for Cohen's d", options$contrastCiLevel * 100)
      contrastTable$addColumnInfo(name="cohenD_LowerCI", type = "number", title = gettext("Lower"), overtitle = thisOverTitleCohenD)
      contrastTable$addColumnInfo(name="cohenD_UpperCI", type = "number", title = gettext("Upper"), overtitle = thisOverTitleCohenD)
    }
  }

  contrastTable$showSpecifiedColumnsOnly <- TRUE

  return(contrastTable)
}

.createContrastCoefficientsTableAnova <- function(contrast, contrCoef, weightType = "number") {

  contrastType <- unlist(strsplit(contrast$contrast, ""))
  contrastType[1] <- toupper(contrastType[1])
  contrastType <- paste0(contrastType, collapse = "")

  myTitle <-  gettextf("%1$s Contrast Coefficients - %2$s",
                       contrastType,
                       paste(contrast$variable, collapse = " \u273B "))

  coefTable <- createJaspTable(title = myTitle)

  for (thisVar in names(contrCoef)[1:length(contrast$variable)])
    coefTable$addColumnInfo(name = thisVar, type = "string", combine = TRUE)

  for (thisComp in paste("Comparison", 1: (ncol(contrCoef) - length(contrast$variable))))
    coefTable$addColumnInfo(name = thisComp, type = weightType)

  coefTable$setData(contrCoef)

  return(coefTable)
}

.createPostHocStandardTable <- function(myTitle, byVariable = NULL, options, makeBootstrapTable = FALSE, dfType = "integer") {

  preTitle <- if (!makeBootstrapTable) gettext("Post Hoc Comparisons - ") else gettext("Bootstrapped Post Hoc Comparisons - ")
  postHocTable <- createJaspTable(title = paste0(preTitle, myTitle)) #this paste is ok

  if (isFALSE(is.null(byVariable))) {
    postHocTable$addColumnInfo(name=byVariable,   title=byVariable, type="string", combine = TRUE)
    postHocTable$title <- paste0(preTitle, myTitle, " - Conditional on ", byVariable)
  }
  postHocTable$addColumnInfo(name="contrast_A", title=" ", type="string", combine = TRUE)
  postHocTable$addColumnInfo(name="contrast_B", title=" ", type="string")

  postHocTable$addColumnInfo(name="estimate", title=gettext("Mean Difference"), type="number")

  if (options$postHocCi || makeBootstrapTable) {

    if (makeBootstrapTable) {
      thisOverTitle <- gettextf("%1$s%% bca%2$s CI", options$postHocCiLevel * 100, "\u2020")
    } else {
      thisOverTitle <- gettextf("%s%% CI for Mean Difference", options$postHocCiLevel * 100)
    }

    postHocTable$addColumnInfo(name="lower.CL", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
    postHocTable$addColumnInfo(name="upper.CL", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
  }

  postHocTable$addColumnInfo(name="SE", title=gettext("SE"), type="number")
  postHocTable$addColumnInfo(name="df", title=gettext("df"), type = dfType)

  if (makeBootstrapTable)
    postHocTable$addColumnInfo(name="bias", title=gettext("bias"), type="number")


  postHocTable$addColumnInfo(name="t.ratio", title=gettext("t"), type="number")

  # postHocTypeStandardEffectSize is from AN(C)OVA
  # postHocEffectSize is from RMANOVA
  if (isTRUE(options$postHocTypeStandardEffectSize) || isTRUE(options$postHocEffectSize)) {
    postHocTable$addColumnInfo(name="cohenD", title=gettext("Cohen's d"), type="number")

    if (options$postHocCi) {
      thisOverTitleCohenD <- gettextf("%s%% CI for Cohen's d", options$postHocCiLevel * 100)
      postHocTable$addColumnInfo(name="cohenD_LowerCI", type = "number", title = gettext("Lower"), overtitle = thisOverTitleCohenD)
      postHocTable$addColumnInfo(name="cohenD_UpperCI", type = "number", title = gettext("Upper"), overtitle = thisOverTitleCohenD)
    }
  }

  if (options$postHocCorrectionTukey)
    postHocTable$addColumnInfo(name="tukey",    title=gettext("p<sub>Tukey</sub>"), type="pvalue")

  if (options$postHocCorrectionScheffe)
    postHocTable$addColumnInfo(name="scheffe", title=gettext("p<sub>Scheffe</sub>"), type="pvalue")

  if (options$postHocCorrectionBonferroni)
    postHocTable$addColumnInfo(name="bonferroni", title=gettext("p<sub>Bonf</sub>"), type="pvalue")

  if (options$postHocCorrectionHolm)
    postHocTable$addColumnInfo(name="holm", title=gettext("p<sub>Holm</sub>"), type="pvalue")

  # Sidak option does not exist in RM-ANOVA
  if (isTRUE(options$postHocCorrectionSidak))
    postHocTable$addColumnInfo(name="sidak", title=gettext("p<sub>Sidak</sub>"), type="pvalue")


  postHocTable$showSpecifiedColumnsOnly <- TRUE

  return(postHocTable)
}

.getCorrectionFootnoteAnova <- function(postHocObject, includeCI = FALSE, includeEffectSize = FALSE, isBetween = FALSE) {

  pvalAdjust <- attr(postHocObject, "mesg")[grep(attr(postHocObject, "mesg"), pattern = "P value adjustment")]
  nEstimates <- regmatches(pvalAdjust, gregexpr("[[:digit:]]+", pvalAdjust))[[1]]
  confAdjust <- attr(postHocObject, "mesg")[grep(attr(postHocObject, "mesg"), pattern = "Conf-level")]
  confAdjust <- gsub(x = confAdjust, pattern = "Conf-level adjustment: ", "")
  confAdjust <- strsplit(confAdjust, " ")[[1]][1]
  substr(confAdjust, 1, 1) <- toupper(substr(confAdjust, 1, 1)) #capitalize first letter of correction name

  if (!includeCI) {
    correctionFootnote <- gettextf("P-value adjusted for comparing a family of %s estimates.", as.character(nEstimates))
  } else if (isFALSE(includeEffectSize) || isFALSE(isBetween)) {
    correctionFootnote <- gettextf("P-value and confidence intervals adjusted for comparing a family of %1$s estimates (confidence intervals corrected using the %2$s method).", nEstimates, confAdjust)
  } else {
    correctionFootnote <- gettextf("P-value and confidence intervals adjusted for comparing a family of %1$s estimates (ci for mean difference corrected using the %2$s method; ci for effect size corrected using the Bonferroni method).",
                                   nEstimates, confAdjust)
  }

  return(correctionFootnote)
}

.createMarginalMeansTableAnova <- function(myTitle, options, individualTerms, makeBootstrapTable = FALSE, dfType = "integer" ) {

  preTitle <- if (!makeBootstrapTable) gettext("Marginal Means - ") else gettext("Bootstrapped Marginal Means - ")
  marginalMeansTable <- createJaspTable(title = paste0(preTitle, myTitle))

  for (i in 1:length(individualTerms))
    marginalMeansTable$addColumnInfo(name=individualTerms[i], type="string", combine = TRUE)

  marginalMeansTable$addColumnInfo(name="lsmean", title=gettext("Marginal Mean"), type="number")

  if (makeBootstrapTable) {
    thisOverTitle <- gettextf("95%% bca%s CI", "\u2020")
    marginalMeansTable$addColumnInfo(name="bias", title=gettext("bias"), type="number")

    marginalMeansTable$addFootnote(message = gettext("Marginal Means estimate is based on the median of the bootstrap distribution."))
    marginalMeansTable$addFootnote(symbol = "\u2020", message = gettext("Bias corrected accelerated."))

  } else {
    thisOverTitle <- gettextf("95%% CI for Mean Difference")
  }

  marginalMeansTable$addColumnInfo(name="lower.CL", type = "number", title = gettext("Lower"), overtitle = thisOverTitle, )
  marginalMeansTable$addColumnInfo(name="upper.CL", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)

  marginalMeansTable$addColumnInfo(name="SE", title=gettext("SE"), type="number")

  if (options$marginalMeanComparedToZero) {
    marginalMeansTable$addColumnInfo(name="t.ratio", title=gettext("t"),  type="number")
    marginalMeansTable$addColumnInfo(name="df",      title=gettext("df"), type=dfType)
    marginalMeansTable$addColumnInfo(name="p.value", title=gettext("p"),  type="pvalue")
  }

  if (isFALSE(options$marginalMeanBootstrap)) {
    if (options$marginalMeanCiCorrection == "bonferroni") {
      marginalMeansTable$addFootnote(message = gettext("Bonferroni CI adjustment"))
    } else if (options$marginalMeanCiCorrection == "sidak") {
      marginalMeansTable$addFootnote(message = gettext("Sidak CI adjustment"))
    }
  }
  marginalMeansTable$showSpecifiedColumnsOnly <- TRUE

  return(marginalMeansTable)
}

.qqPlotFreqAnova <- function(anovaContainer, dataset, options, ready) {

  # create the jaspPlot object
  qqPlot <- createJaspPlot(title = gettext("Q-Q Plot"), width = options$plotWidthQQPlot, height = options$plotHeightQQPlot)

  # now we assign the plot to jaspResults
  anovaContainer[["assumptionsContainer"]][["qqPlot"]] <- qqPlot

  if (!ready || anovaContainer$getError())
    return()

  if (is.null(options[["dependent"]])) { # If RM ANOVA
    residuals <- rstandard(anovaContainer[["anovaResult"]][["object"]][["fullModel"]][["lm"]])
    residuals <- as.vector(unlist(residuals))
  } else {
    residuals <- rstandard(anovaContainer[["model"]][["object"]])
  }
  ciLevel <- if (options[["qqPlotCi"]])  options[["qqPlotCiLevel"]] else NULL

  qqPlot$plotObject <- jaspGraphs::plotQQnorm(residuals,
                                              yName = "Standardized residuals",
                                              ablineColor = "darkred",
                                              ablineOrigin = TRUE,
                                              identicalAxes = TRUE,
                                              ciLevel = ciLevel)
  return()
}

.addSumSquaresFootnote <- function(table, options){

  typeFootnote <- switch(options$sumOfSquares,
                         type1 = gettext("Type I Sum of Squares"),
                         type2 = gettext("Type II Sum of Squares"),
                         type3 = gettext("Type III Sum of Squares"))
  table$addFootnote(message = typeFootnote)

}

.anovaAddSignificanceSigns <- function(someTable, allPvalues, resultRowNames) {

  threeStarSignif <- twoStarSignif <- oneStarSignif <- FALSE

  for (thisP in colnames(allPvalues)) {

    signifComparisonsThreeStars <- resultRowNames[allPvalues[, thisP] < 0.001]
    signifComparisonsTwoStars <- resultRowNames[allPvalues[, thisP] < 0.01 & allPvalues[, thisP] >= 0.001]
    signifComparisonsOneStar <- resultRowNames[allPvalues[, thisP] < 0.05 & allPvalues[, thisP] >= 0.01]

    if (length(signifComparisonsThreeStars) > 0 && !any(allPvalues[, thisP] == ".")) {
      colNames <- rep(thisP, length(signifComparisonsThreeStars))
      someTable$addFootnote(colNames = colNames, rowNames = signifComparisonsThreeStars, symbol = "***")
      threeStarSignif <- TRUE
    }

    if (length(signifComparisonsTwoStars) > 0 && !any(allPvalues[, thisP] == ".")) {
      colNames <- rep(thisP, length(signifComparisonsTwoStars))
      someTable$addFootnote(colNames = colNames, rowNames = signifComparisonsTwoStars, symbol = "**")
      twoStarSignif <- TRUE
    }

    if (length(signifComparisonsOneStar) > 0 && !any(allPvalues[, thisP] == ".")) {
      colNames <- rep(thisP, length(signifComparisonsOneStar))
      someTable$addFootnote(colNames = colNames, rowNames = signifComparisonsOneStar, symbol = "*")
      oneStarSignif <- TRUE
    }

  }

  signifMessage <- c("* p < .05", "** p < .01", "*** p < .001")[c(oneStarSignif, twoStarSignif, threeStarSignif)]

  if (length(signifMessage) > 0)
    someTable$addFootnote(message = paste0(signifMessage, collapse = ", "), symbol = " ")
}


.anovaExportResiduals <- function(container, dataset, options, ready) {

  if (ready &&
      isTRUE(options[["residualsSavedToData"]]) &&
      isTRUE(options[["residualsSavedToDataColumn"]] != "")) {

    model <- container[["model"]]$object

    residuals <- rep(NA, nrow(dataset)) # create vector with MA to account for missinginess

    if (options[["residualsSavedToDataType"]] == "raw") {
      residuals[as.numeric(rownames(model[["model"]]))] <- model[["residuals"]] # extract residuals
    } else if (options[["residualsSavedToDataType"]] == "standard") {
      residuals[as.numeric(rownames(model[["model"]]))] <- rstandard(model)
    } else if (options[["residualsSavedToDataType"]] == "student") {
      residuals[as.numeric(rownames(model[["model"]]))] <- rstudent(model)
    }

    container[["residualsSavedToDataColumn"]] <- createJaspColumn(columnName = options[["residualsSavedToDataColumn"]])
    container[["residualsSavedToDataColumn"]]$dependOn(options = c("residualsSavedToDataColumn", "residualsSavedToData",
                                                                   "residualsSavedToDataType",  "modelTerms"))
    container[["residualsSavedToDataColumn"]]$setScale(residuals)

  }

}

.anovaExportPredictions <- function(container, dataset, options, ready) {

  if (ready &&
      isTRUE(options[["predictionsSavedToData"]]) &&
      isTRUE(options[["predictionsSavedToDataColumn"]] != "")) {


    model <- container[["model"]]$object

    predictions <- rep(NA, nrow(dataset)) # create vector with MA to account for missinginess
    predictions[as.numeric(rownames(model[["model"]]))] <- model[["fitted.values"]] # extract predictions

    container[["predictionsSavedToDataColumn"]] <- createJaspColumn(columnName = options[["predictionsSavedToDataColumn"]])
    container[["predictionsSavedToDataColumn"]]$dependOn(options = c("predictionsSavedToDataColumn", "predictionsSavedToData", "modelTerms"))
    container[["predictionsSavedToDataColumn"]]$setScale(predictions)

  }

}

