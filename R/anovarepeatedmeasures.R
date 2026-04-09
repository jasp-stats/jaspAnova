#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

AnovaRepeatedMeasuresInternal <- function(jaspResults, dataset = NULL, options) {
  initialGlobalOptions <- options()
  on.exit(options(initialGlobalOptions), add = TRUE)

  numericVariables <- c(unlist(options$repeatedMeasuresCells),unlist(options$covariates))
  numericVariables <- numericVariables[numericVariables != ""]
  factorVariables <- c(unlist(options$betweenSubjectFactors))
  factorVariables <- factorVariables[factorVariables != ""]

  if (is.null(dataset))
    dataset <- .readDataSetToEnd(columns.as.numeric=numericVariables, columns.as.factor=factorVariables,
                                 exclude.na.listwise=c(numericVariables, factorVariables))

  longData <- .BANOVAreadRManovaData(dataset, options)
  if (isTryError(longData))
    .quitAnalysis(gettext("Error while loading data. Please verify your repeated measures observations."))

  ready <- all(options$repeatedMeasuresCells != "") &&  length(options$withinModelTerms) > 0

  rmAnovaContainer <- .getRMAnovaContainer(jaspResults)

  .BANOVAerrorhandling(longData, options, "RM-ANOVA")

  .rmAnovaComputeResultsContainer(rmAnovaContainer, longData, options, ready)

  .rmAnovaWithinSubjectsTable(rmAnovaContainer, dataset, options, ready)

  .rmAnovaBetweenSubjectsTable(rmAnovaContainer, dataset, options, ready)

  .referenceGrid(rmAnovaContainer, options, ready)

  .rmAnovaAssumptionsContainer(rmAnovaContainer, dataset, options, ready)

  .anovaOrdinalRestrictions(rmAnovaContainer, dataset, options, ready, analysis = "rmanova")

  .rmAnovaPostHocTable(rmAnovaContainer, dataset, longData, options, ready)

  .rmAnovaContrastTable(rmAnovaContainer, longData, options, ready)

  .rmAnovaMarginalMeansTable(rmAnovaContainer, dataset, options, ready)

  .rmAnovaFriedmanTable(rmAnovaContainer, longData, options, ready)

  .rmAnovaConoverTable(rmAnovaContainer, longData, options, ready)

  .rmAnovaSimpleEffects(rmAnovaContainer, dataset, longData, options, ready)

  .BANOVAdescriptives(rmAnovaContainer, longData, options, list(noVariables=FALSE), "RM-ANOVA", ready)

  return()
}

.getRMAnovaContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["rmAnovaContainer"]])) {

    anovaContainer <- jaspResults[["rmAnovaContainer"]]

  } else {

    anovaContainer <- createJaspContainer()
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    anovaContainer$dependOn(c("withinModelTerms", "betweenModelTerms", "repeatedMeasuresCells", "betweenSubjectFactors",
                              "repeatedMeasuresFactors", "covariates", "sumOfSquares", "poolErrorTermFollowup"))
    jaspResults[["rmAnovaContainer"]] <- anovaContainer
  }

  return(anovaContainer)

}

.rmAnovaCheckErrors <- function(dataset, options, ready) {
  if (!ready)
    return()

  modelTerms <- unlist(options$withinModelTerms, recursive = FALSE)
  betweenModelTerms <- options$betweenModelTerms

  for(betweenTerm in rev(betweenModelTerms)) {
    .hasErrors(
      dataset = dataset,
      type = c('observations', 'variance', 'infinity'),
      all.target = c(options$repeatedMeasuresCells, options$covariates),
      all.grouping = betweenTerm,
      observations.amount = "< 2",
      exitAnalysisIfErrors = TRUE)
  }

  for(betweenTerm in rev(betweenModelTerms)) {
    .hasErrors(
      dataset = dataset,
      type = c('infinity', 'factorLevels'),
      all.target = betweenTerm,
      factorLevels.amount  = "< 2",
      exitAnalysisIfErrors = TRUE)
  }

}

.rmAnovaCheck <- function(dataset, options, perform) {

  error <- FALSE
  errorMessage <- NULL
  ready <- "" %in% options$repeatedMeasuresCells == FALSE && length(options$withinModelTerms) > 0

  if (ready && perform == "run") {

    components <- unique(unlist(options$betweenSubjectFactors))
    independentsWithLessThanTwoLevels <- c()

    for (component in components) {

      nLevels <- length(levels(dataset[[ .v(component) ]]))

      if (nLevels < 2)
        independentsWithLessThanTwoLevels <- c(independentsWithLessThanTwoLevels, component)
    }

    if (length(independentsWithLessThanTwoLevels) > 0) {

      error <- TRUE
      if(length(independentsWithLessThanTwoLevels) == 1) {
        errorMessage <- gettextf("Factor: <em>%s</em>, contains fewer than two levels.", independentsWithLessThanTwoLevels)
      } else {
        errorMessage <- gettextf("Factors: <em>%s</em>, contain fewer than two levels.", paste(independentsWithLessThanTwoLevels, collapse=",", sep=""))
      }
    }

    repeatedMeasuresData <- list()
    for(i in options$repeatedMeasuresCells) {
      repeatedMeasuresData[[i]] <- dataset[[.v(i)]]
    }
    infiniteRM <- unlist(lapply(repeatedMeasuresData,function(x)sum(is.infinite(x)) > 0))

    if (!is.null(infiniteRM) && sum(infiniteRM) > 0) {

      error <- TRUE
      if(sum(infiniteRM) == 1) {
        errorMessage <- gettextf("The repeated measure: <em>%s</em>, contains infinite values.", options$repeatedMeasuresCells[infiniteRM])
      } else {
        errorMessage <- gettextf("The repeated measures: <em>%s</em>, contain infinite values.", paste(options$repeatedMeasuresCells[infiniteRM], collapse=", "))
      }
    }

    covariatesData <- list()
    for(i in options$covariates) {
      covariatesData[[i]] <- dataset[[.v(i)]]
    }
    infiniteCov <- unlist(lapply(covariatesData,function(x)sum(is.infinite(x)) > 0))

    if (!is.null(infiniteCov) && sum(infiniteCov) > 0) {

      error <- TRUE
      if(sum(infiniteCov) == 1) {
        errorMessage <- gettextf("The covariate: <em>%s</em>, contains infinite values.", options$covariates[infiniteCov])
      } else {
        errorMessage <- gettextf("The covariates: <em>%s</em>, contain infinite values.", paste(options$covariates[infiniteCov], collapse=", "))
      }
    }

    allNames <- unlist(lapply(options[['repeatedMeasuresFactors']], function(x) x$name)) # Factornames
    for(factorName in allNames){
      if (any(factorName %in% options$betweenSubjectFactors )) {
        error <- TRUE
        errorMessage <- gettext("Please choose a name for the RM factors that differs from those for the between subjects factors.")
      }
    }

  }

  list(ready=ready, error=error, errorMessage=errorMessage)
}

.rmModelFormula <- function(options) {

  termsRM.base64 <- c()
  termsRM.normal <- c()

  mainEffects <- unlist(lapply(options$withinModelTerms, function(term) {
    if (length(term$components) == 1) term$components
  }))

  for (term in options$withinModelTerms) {

    components <- unlist(term$components)
    if (length(components) > 1) # make sure that interaction gets defined in same order as model terms
      components <- mainEffects[mainEffects %in% components]

    termRM.base64 <- paste(.v(components), collapse=":", sep="")
    termRM.normal <- paste(components, collapse=" \u273B ", sep="")

    termsRM.base64 <- c(termsRM.base64, termRM.base64)
    termsRM.normal <- c(termsRM.normal, termRM.normal)
  }

  termsBS.base64 <- c()
  termsBS.normal <- c()

  for (term in options$betweenModelTerms) {

    components <- unlist(term$components)
    termBS.base64 <- paste(.v(components), collapse=":", sep="")
    termBS.normal <- paste(components, collapse=" \u273B ", sep="")

    termsBS.base64 <- c(termsBS.base64, termBS.base64)
    termsBS.normal <- c(termsBS.normal, termBS.normal)
  }
  terms.base64 <- list()
  terms.normal <- list()
  terms.base64[[1]] <- termsBS.base64
  terms.normal[[1]] <- termsBS.normal


  for (i in 1:length(termsRM.base64)) {
    if (is.null(termsBS.base64)) {
      terms.base64[[i+1]] <- termsRM.base64[i]
      terms.normal[[i+1]] <- termsRM.normal[i]
    } else if (!is.null(termsRM.base64)){
      terms.base64[[i+1]] <- c(termsRM.base64[i], paste(termsRM.base64[i], termsBS.base64, sep = ":"))
      terms.normal[[i+1]] <- c(termsRM.normal[i], paste(termsRM.normal[i], termsBS.normal, sep = " \u273B "))
    }
  }

  main <- paste("(",paste(unlist(terms.base64), collapse=" + "),")", sep="")
  termsBS <- paste("(",paste(termsBS.base64, collapse=" + "),")", sep="")
  errorRM <- paste("Error(",paste0(.BANOVAsubjectName, "/(", termsRM.base64, ")", collapse=" + "),")",sep="")

  if (is.null(termsBS.base64) && is.null(termsRM.base64)) {
    model.def <- dependent ~ 1
  } else if (is.null(termsBS.base64)) {
    model.def <- paste(.BANOVAdependentName, "~", paste(main, errorRM, sep=" + "))
  } else if (is.null(termsRM.base64)) {
    model.def <- paste(.BANOVAdependentName, "~", main)
  } else {
    model.def <- paste(.BANOVAdependentName, "~", paste(main, errorRM, termsBS, sep=" + "))
  }

  list(model.def = model.def, terms.normal = terms.normal, terms.base64 = terms.base64, termsRM.normal = termsRM.normal, termsRM.base64 = termsRM.base64)
}

.rmAnovaComputeResultsContainer <- function(rmAnovaContainer, longData, options, ready) {
  if (!ready || !is.null(rmAnovaContainer[["anovaResult"]]))
    return()

  rmAnovaResult <- .rmAnovaComputeResults(longData, options)

  if (rmAnovaResult[["tryResult"]] == "try-error" && grepl(as.character(rmAnovaResult[["tryMessage"]]), pattern = "allocate vector")) {
    rmAnovaContainer$setError(gettext('Data set too big for univariate follow-up test. Try unselecting "Pool error term for follow-up tests" in the Model tab.'))
    return()
  } else if (rmAnovaResult[["tryResult"]] == "try-error") {
    rmAnovaContainer$setError(gettext("Some parameters are not estimable, most likely due to empty cells of the design."))
    return()
  }

  # Save model to state
  rmAnovaContainer[["anovaResult"]] <- createJaspState(object = rmAnovaResult)
}

.rmAnovaComputeResults <- function(dataset, options, returnResultsEarly = FALSE) {

  modelDef <- .rmModelFormula(options)
  model.formula <- as.formula(modelDef$model.def)
  options(contrasts=c("contr.sum","contr.poly"))

  # set these options once for all afex::aov_car calls,
  # this ensures for instance that afex::aov_car always returns objects of class afex_aov.
  if (options$poolErrorTermFollowup)   followupModelType <- "univariate" else followupModelType <- "multivariate"
  afex::afex_options(
    check_contrasts = TRUE, correction_aov = "GG",
    emmeans_model = followupModelType, es_aov = "ges", factorize = TRUE,
    lmer_function = "lmerTest", method_mixed = "KR", return_aov = "afex_aov",
    set_data_arg = FALSE, sig_symbols = c(" +", " *", " **", " ***"), type = 3
  )

  # Computations:
  if (options$sumOfSquares == "type1") {
    tryResult <- try({

      result <- stats::aov(model.formula, data=dataset)
      summaryResultOne <- summary(result, expand.split = FALSE)

      result <- afex::aov_car(model.formula, data=dataset, type= 3, factorize = FALSE,
                              include_aov = isTRUE(options[["poolErrorTermFollowup"]]))
      summaryResult <- summary(result)

      # Reformat the results to make it consistent with types 2 and 3
      model <- as.data.frame(unclass(summaryResult$univariate.tests))

      for (mySub in unlist(summaryResultOne, recursive = FALSE)) {
        rownames(mySub) <- trimws(rownames(mySub))
        for(term in rownames(mySub)[-nrow(mySub)]) {
          model[term, "Sum Sq"]   <- mySub[term,        "Sum Sq"]
          model[term, "num Df"]   <- mySub[term,        "Df"]
          model[term, "F value"]  <- mySub[term,        "F value"]
          model[term, "Pr(>F)"]   <- mySub[term,        "Pr(>F)"]
          model[term, "Error SS"] <- mySub["Residuals", "Sum Sq"]
          model[term, "den Df"]   <- mySub["Residuals", "Df"]
        }
      }
    })

    } else if (options$sumOfSquares == "type2") {

    tryResult <- try({
      result <- afex::aov_car(model.formula, data=dataset, type= 2, factorize = FALSE,
                              include_aov = isTRUE(options[["poolErrorTermFollowup"]]))
      summaryResult <- summary(result)
      model <- as.data.frame(unclass(summaryResult$univariate.tests))
    })

  } else {

    tryResult <- try({
      result <- afex::aov_car(model.formula, data=dataset, type= 3, factorize = FALSE,
                              include_aov = isTRUE(options[["poolErrorTermFollowup"]]))
      summaryResult <- summary(result)
      model <- as.data.frame(unclass(summaryResult$univariate.tests))
    })

  }

  if (class(tryResult) == "try-error") {
    return(list(tryResult = "try-error", tryMessage = as.character(tryResult)))
  }

  if (returnResultsEarly)
    return(list(result = result, model = model))

  # Now we reformat the results table some more to make it flow with jaspResults later
  interceptRow <- model["(Intercept", ]
  model <- model[-1,]
  rownames(model) <- trimws(rownames(model))
  model[["isWithinTerm"]] <- model[[".isNewGroup"]] <- logical(nrow(model))

  sortedModel <- model
  cases <- unlist(sapply(modelDef$terms.base64, function(x) x[[1]]))
  residualResults <- sortedModel[.mapAnovaTermsToTerms(cases, rownames(model)), ]

  nextNewGroup <- 0
  for (modelTerm in modelDef$terms.base64) {

    if (!is.null(modelTerm)) {
      isWithin <- any(modelTerm %in% modelDef$termsRM.base64)
      indices <- .mapAnovaTermsToTerms(modelTerm, rownames(model))
      nextNewGroup <- c(TRUE, rep(FALSE, length(indices) - 1))
      sortedModel[indices, ] <- model[indices, ]
      sortedModel[indices, c(".isNewGroup", "isWithinTerm")] <- c(nextNewGroup, rep(isWithin, length(indices)))

      residualRow <- c(model[indices[1],  c("Error SS", "den Df")], rep(NA, 4), 0, isWithin)
      residualResults[.mapAnovaTermsToTerms(modelTerm[[1]], rownames(residualResults)), ] <-  residualRow
    }

  }

  # Make sure that order of anova result corresponds to order of specified model terms
  mappedRownamesCases <- .mapAnovaTermsToTerms(rownames(sortedModel), unlist(modelDef$terms.base64))

  # when interactions effects are excluded from the model terms they are still computed by afex
  # their names are mapped to zero by .mapAnovaTermsToTerms so we first exclude them
  idxValid <- mappedRownamesCases != 0L
  sortedModel <- sortedModel[idxValid, ]

  sortedModel[["case"]] <- unlist(modelDef$terms.normal)[mappedRownamesCases]
  sortedModel[["Mean Sq"]] <- sortedModel[["Sum Sq"]] / sortedModel[["num Df"]]
  sortedModel[["vovkSellke"]] <- VovkSellkeMPR(sortedModel[["Pr(>F)"]])

  rownames(residualResults) <- cases
  residualResults[["Mean Sq"]] <- residualResults[["Sum Sq"]] / residualResults[["num Df"]]
  residualResults[["case"]] <- "Residuals"

  # Now we calculate effect sizes
  SSr <- sortedModel[["Error SS"]]
  MSr <- SSr/sortedModel[["den Df"]]

  sortedModel[["eta"]]     <- sortedModel[["Sum Sq"]] / (sum(sortedModel[["Sum Sq"]]) + sum(residualResults[["Sum Sq"]]))
  sortedModel[["partialEta"]] <- sortedModel[["Sum Sq"]] / (sortedModel[["Sum Sq"]] + SSr)
  sortedModel[["genEta"]]  <- result[["anova_table"]][["ges"]][idxValid]

  n <- interceptRow[["den Df"]] + 1
  MSb <- interceptRow[["Error SS"]] / (n-1)
  MSm <- sortedModel[["Mean Sq"]]
  df <- sortedModel[["num Df"]]

  omega <- (df / (n * (df + 1)) * (MSm - MSr)) / (MSr + ((MSb - MSr) / (df + 1)) +
                                                                     (df / (n * (df + 1))) * (MSm - MSr))
  sortedModel[["omega"]] <- sapply(omega, max, 0)
  for (i in seq_along(summaryResult)) {
    if (any(rownames(summaryResult[[i]]) == "(Intercept)"))
      summaryResult[[i]] <- summaryResult[[i]][-1, ]
  }

  partOmegaResult <- effectsize::omega_squared(result,  ci = options[["effectSizeCiLevel"]], partial = TRUE, alternative = "two.sided")
  effectSizeIndexMap <- .mapAnovaTermsToTerms(partOmegaResult[["Parameter"]], rownames(sortedModel))
  sortedModel[["partialOmega"]] <- partOmegaResult[["Omega2_partial"]][effectSizeIndexMap]

  # Now we include the results from the corrections
  withinAnovaTable <- ggTable <- hfTable <- subset(sortedModel, isWithinTerm == TRUE)
  withinAnovaTable[["correction"]] <- "None"
  corrections <- summaryResult$pval.adjustments
  sphericityTests <- as.data.frame(unclass(summaryResult$sphericity.tests))

  if (!is.null(rownames(corrections)) && length(rownames(corrections)) > 0) {
    corrections <- as.data.frame(corrections)
    corrections <- corrections[.mapAnovaTermsToTerms(rownames(withinAnovaTable), rownames(corrections)), ]
    sphericityTests <- sphericityTests[.mapAnovaTermsToTerms(rownames(withinAnovaTable), rownames(corrections)), ]
    rownames(corrections) <- rownames(sphericityTests) <-
      rownames(withinAnovaTable)[.mapAnovaTermsToTerms(rownames(corrections), rownames(withinAnovaTable))]
  }

  # Add NA rows to corrections and sphericity tests for within factors with 2 levels
  if (nrow(sphericityTests) != nrow(withinAnovaTable)) {

    unavailableCases <- rownames(withinAnovaTable)[!rownames(withinAnovaTable) %in% rownames(sphericityTests)]
    emptyCorrections <- matrix(ncol = 4, nrow = length(unavailableCases), NA,
                               dimnames = list(unavailableCases, c("GG eps", "Pr(>F[GG])", "HF eps", "Pr(>F[HF])")))
    if (is.null(rownames(corrections)) || all(is.na(corrections[, "GG eps"]))) {
      corrections <- as.data.frame(emptyCorrections)
    } else {
      corrections <- rbind(corrections, as.data.frame(emptyCorrections))
    }

    emptyTests <- matrix(ncol = 2, nrow = length(unavailableCases), NA,
                         dimnames = list(unavailableCases, colnames(sphericityTests)))
    sphericityTests <- as.data.frame(rbind(sphericityTests, emptyTests))

  }

  # If corrections could not be run, create data frame with NA's
  if (length(rownames(corrections)) == 0 )
    corrections <- matrix(ncol = 4, nrow = nrow(withinAnovaTable), NA,
                          dimnames = list(rownames(withinAnovaTable), c("GG eps", "Pr(>F[GG])", "HF eps", "Pr(>F[HF])")))

  withinIndices <- .mapAnovaTermsToTerms(rownames(withinAnovaTable), rownames(corrections))

  # set 1 as an upper-bound of the correction factors, see https://github.com/jasp-stats/jasp-issues/issues/1709
  ggCorrections <- pmin(corrections[withinIndices, "GG eps"], 1)
  hfCorrections <- pmin(corrections[withinIndices, "HF eps"], 1)

  ggTable[["num Df"]]          <- withinAnovaTable[["num Df"]] * ggCorrections
  ggTable[["Mean Sq"]]         <- withinAnovaTable[["Sum Sq"]] / ggTable[["num Df"]]
  ggTable[["den Df"]]          <- withinAnovaTable[["den Df"]] * ggCorrections
  ggTable[["Pr(>F)"]]          <- pf(withinAnovaTable[["F value"]], ggTable[["num Df"]], ggTable[["den Df"]], lower.tail = FALSE)
  ggTable[["correction"]]      <- gettext("Greenhouse-Geisser")
  ggTable[[".isNewGroup"]]     <- FALSE

  hfTable[["num Df"]]          <- withinAnovaTable[["num Df"]] * hfCorrections
  hfTable[["Mean Sq"]]         <- withinAnovaTable[["Sum Sq"]] / hfTable[["num Df"]]
  hfTable[["den Df"]]          <- withinAnovaTable[["den Df"]] * hfCorrections
  hfTable[["Pr(>F)"]]          <- pf(withinAnovaTable[["F value"]], hfTable[["num Df"]], hfTable[["den Df"]], lower.tail = FALSE)
  hfTable[["correction"]]      <- gettext("Huynh-Feldt")
  hfTable[[".isNewGroup"]]     <- FALSE

  residualResults[["eta"]]     <- residualResults[["etaPart"]] <- residualResults[["genEta"]] <-
  residualResults[["omega"]]   <- residualResults[["p"]] <- as.numeric(NA)

  wResidualResults <- wResidualResultsGG <- wResidualResultsHF <- subset(residualResults, isWithinTerm == TRUE)

  wResidualResults[["correction"]]   <- gettext("None")
  wResidualResults[[".isNewGroup"]]  <- TRUE

  residualIndices                    <- .mapAnovaTermsToTerms(rownames(wResidualResults), rownames(corrections))
  wResidualResultsGG[["num Df"]]     <- wResidualResults[["num Df"]] * corrections[residualIndices, "GG eps"]
  wResidualResultsGG[["Mean Sq"]]    <- wResidualResults[["Sum Sq"]] / wResidualResultsGG[["num Df"]]
  wResidualResultsGG[["correction"]] <- gettext("Greenhouse-Geisser")

  wResidualResultsHF[["num Df"]]     <- wResidualResults[["num Df"]] * corrections[residualIndices, "HF eps"]
  wResidualResultsHF[["Mean Sq"]]    <- wResidualResults[["Sum Sq"]] / wResidualResultsHF[["num Df"]]
  wResidualResultsHF[["correction"]] <- gettext("Huynh-Feldt")

  withinAnovaTable <- cbind(withinAnovaTable, corrections[withinIndices, ], sphericityTests[withinIndices, ])

  # Makes lists with results
  withinAnovaTableCollection <- list("None" = withinAnovaTable, "Huynh-Feldt" = hfTable,            "Greenhouse-Geisser" = ggTable)
  wResidualResultsList       <- list("None" = wResidualResults, "Huynh-Feldt" = wResidualResultsHF, "Greenhouse-Geisser" = wResidualResultsGG)
  # # Corrections not available
  # withinAnovaTableCollection <- list("None" = withinAnovaTable)
  # wResidualResultsList <- list("None" = wResidualResults)


  wResidualResultsList[["None"]]["BetweenResidualResults", c("Sum Sq", "num Df")] <- interceptRow[, c("Error SS", "den Df")]
  wResidualResultsList[["None"]]["BetweenResidualResults", "Mean Sq"] <- interceptRow[["Error SS"]] / interceptRow[["den Df"]]

  for (tableIndex in seq_along(withinAnovaTableCollection)) {
    withinAnovaTableCollection[[tableIndex]][["vovkSellke"]] <- VovkSellkeMPR(withinAnovaTableCollection[[tableIndex]][["Pr(>F)"]])
  }

  return(list(anovaResult = sortedModel,
              residualTable = wResidualResultsList,
              withinAnovaTable = withinAnovaTableCollection,
              assumptionResult = cbind(sphericityTests, corrections[withinIndices, ]),
              fullModel = result,
              tryResult = "tryResult"))
}

.mapAnovaTermsToTerms <- function(oneTerms, twoTerms) {
  nTerms <- length(oneTerms)
  indices <- numeric(nTerms)
  counter <- 1

  for (i in 1:nTerms) {
    splitFirst <- strsplit(oneTerms[[i]],":")[[1]]
    for (j in 1:length(twoTerms)) {
      matchedTerms <- match(splitFirst, strsplit(twoTerms[[j]],":")[[1]])
      if ((length(strsplit(twoTerms[[j]],":")[[1]]) == length(splitFirst)) && !any(is.na(matchedTerms))) {
        indices[counter] <- j
        counter <- counter + 1
      }
    }
  }

  return(indices)
}

.rmAnovaBetweenSubjectsTable <- function(rmAnovaContainer, dataset, options, ready) {
  if(!is.null(rmAnovaContainer[["betweenTable"]]))
    return()

  betweenTable <- createJaspTable(title = gettext("Between Subjects Effects"), position = 2)
  betweenTable$dependOn(c("effectSizeEstimates", "effectSizeEtaSquared", "effectSizePartialEtaSquared",
                          "effectSizeGeneralEtaSquared", "effectSizeOmegaSquared", "vovkSellke",
                          "effectSizePartialOmegaSquared", "effectSizeCi", "effectSizeCiLevel"))

  betweenTable$addColumnInfo(title = gettext("Cases"),          name = "case",    type = "string" )
  betweenTable$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
  betweenTable$addColumnInfo(title = gettext("df"),             name = "num Df",  type = "integer")
  betweenTable$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
  betweenTable$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
  betweenTable$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")

  if (options$vovkSellke && length(options$betweenSubjectFactors) > 0) {
    betweenTable$addColumnInfo(title = gettextf("VS-MPR%s", "\u002A"), name = "vovkSellke", type = "number")
    betweenTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }

  if (options$effectSizeEstimates && length(options$betweenSubjectFactors) > 0) {

    if (options$effectSizeEtaSquared)
      betweenTable$addColumnInfo(title = "\u03B7\u00B2", name = "eta", type = "number")

    if (options$effectSizePartialEtaSquared) {
      betweenTable$addColumnInfo(title = "\u03B7\u00B2\u209A", name = "partialEta", type = "number")
      if (options$effectSizeCi && !is.null(options$dependent)) {
        thisOverTitle <- gettextf("%s%% CI for \u03B7\u00B2\u209A", options$effectSizeCiLevel * 100)
        betweenTable$addColumnInfo(name="partialEtaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        betweenTable$addColumnInfo(name="partialEtaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }

    if (options$effectSizeGeneralEtaSquared)
      betweenTable$addColumnInfo(title = gettextf("%s<sub>G</sub>", "\u03B7\u00B2"), name = "genEta", type = "number")

    if (options$effectSizeOmegaSquared) {
      betweenTable$addColumnInfo(title = "\u03C9\u00B2", name = "omega", type = "number")
      if (options$effectSizeCi && !is.null(options$dependent)) {
        thisOverTitle <- gettextf("%s%% CI for \u03C9\u00B2", options$effectSizeCiLevel * 100)
        betweenTable$addColumnInfo(name="omegaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        betweenTable$addColumnInfo(name="omegaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }

    if (options$effectSizePartialOmegaSquared) {
      betweenTable$addColumnInfo(title = "\u03C9\u00B2\u209A", name = "partialOmega", type = "number")
      if (options$effectSizeCi && !is.null(options$dependent)) {
        thisOverTitle <- gettextf("%s%% CI for \u03C9\u00B2\u209A", options$effectSizeCiLevel * 100)
        betweenTable$addColumnInfo(name="partialOmegaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        betweenTable$addColumnInfo(name="partialOmegaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }
  }

  betweenTable$showSpecifiedColumnsOnly <- TRUE

  .addSumSquaresFootnote(betweenTable, options)

  rmAnovaContainer[["betweenTable"]] <- betweenTable

  if (!ready || rmAnovaContainer$getError()) {
    return()
  }

  result <- rmAnovaContainer[["anovaResult"]]$object$anovaResult
  result <- result[result$isWithinTerm == FALSE, ]
  betweenwResidualResult <- rmAnovaContainer[["anovaResult"]]$object$residualTable$None["BetweenResidualResults", ]

  result["Residuals", "num Df"] <- betweenwResidualResult[["num Df"]]
  result["Residuals", "Sum Sq"] <- betweenwResidualResult[["Sum Sq"]]
  result["Residuals", "Mean Sq"] <- betweenwResidualResult[["Mean Sq"]]
  result["Residuals", "case"] <- "Residuals"
  result["Residuals", ".isNewGroup"] <- TRUE

  betweenTable$setData(result)

  return()
}

.rmAnovaWithinSubjectsTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["withinAnovaTable"]]))
    return()

  anovaTable <- createJaspTable(title = gettext("Within Subjects Effects"), position = 1)
  rmAnovaContainer[["withinAnovaTable"]] <- anovaTable
  anovaTable$showSpecifiedColumnsOnly <- TRUE
  anovaTable$dependOn(c("sphericityCorrectionGreenhouseGeisser", "sphericityCorrectionHuynhFeldt",
                        "sphericityCorrectionNone", "vovkSellke", "effectSizeEstimates", "effectSizeEtaSquared",
                        "effectSizePartialEtaSquared", "effectSizeGeneralEtaSquared", "effectSizeOmegaSquared",
                        "effectSizePartialOmegaSquared", "effectSizeCi", "effectSizeCiLevel"))

  corrections <- c("None", "Greenhouse-Geisser", "Huynh-Feldt")[c(options$sphericityCorrectionNone,
                                                                 options$sphericityCorrectionGreenhouseGeisser,
                                                                 options$sphericityCorrectionHuynhFeldt)]
  if (length(corrections) == 0) corrections <- "None"

  anovaTable$addColumnInfo(title = gettext("Cases"), name = "case", type = "string", combine = TRUE)

  dfType <- "integer" # Make df an integer unless corrections are applied
  if ((length(corrections) > 1 || any(!"None" %in% corrections))) {
    anovaTable$addColumnInfo(title = gettext("Sphericity Correction"), name = "correction", type = "string")
    dfType <- "number"
  }

  anovaTable$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
  anovaTable$addColumnInfo(title = gettext("df"),             name = "num Df",  type = dfType)
  anovaTable$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
  anovaTable$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
  anovaTable$addColumnInfo(title = gettext("p"),              name = "p",       type = "pvalue")

  if (options$vovkSellke) {
    anovaTable$addColumnInfo(title = gettextf("VS-MPR%s", "\u002A"), name = "vovkSellke", type = "number")
    anovaTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }

  if (options$effectSizeEstimates) {

    if (options$effectSizeEtaSquared) {
      anovaTable$addColumnInfo(title = "\u03B7\u00B2", name = "eta", type = "number")
    }

    if (options$effectSizePartialEtaSquared) {
      anovaTable$addColumnInfo(title = "\u03B7\u00B2\u209A", name = "partialEta", type = "number")
      if (options$effectSizeCi && !is.null(options$dependent)) {
        thisOverTitle <- gettextf("%s%% CI for \u03B7\u00B2\u209A", options$effectSizeCiLevel * 100)
        anovaTable$addColumnInfo(name="partialEtaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        anovaTable$addColumnInfo(name="partialEtaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }

    if(options$effectSizeGeneralEtaSquared) {
      anovaTable$addColumnInfo(name="genEta", type="number", title=gettextf("%s<sub>G</sub>", "\u03B7\u00B2"))
    }

    if (options$effectSizeOmegaSquared) {
      anovaTable$addColumnInfo(title = "\u03C9\u00B2", name = "omega", type = "number")
      if (options$effectSizeCi && !is.null(options$dependent)) {
        thisOverTitle <- gettextf("%s%% CI for \u03C9\u00B2", options$effectSizeCiLevel * 100)
        anovaTable$addColumnInfo(name="omegaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        anovaTable$addColumnInfo(name="omegaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }

    if (options$effectSizePartialOmegaSquared) {
      anovaTable$addColumnInfo(title = "\u03C9\u00B2\u209A", name = "partialOmega", type = "number")
      if (options$effectSizeCi && !is.null(options$dependent)) {
        thisOverTitle <- gettextf("%s%% CI for \u03C9\u00B2\u209A", options$effectSizeCiLevel * 100)
        anovaTable$addColumnInfo(name="partialOmegaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        anovaTable$addColumnInfo(name="partialOmegaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }

  }

  .addSumSquaresFootnote(anovaTable, options)

  if (!ready || rmAnovaContainer$getError())
    return()

  withinResults   <- rmAnovaContainer[["anovaResult"]]$object$withinAnovaTable
  residualResults <- rmAnovaContainer[["anovaResult"]]$object$residualTable
  mauchlyResult   <- rmAnovaContainer[["anovaResult"]]$object$assumptionResult

  for(i in seq_along(withinResults)) {
    names(withinResults[[i]])[names(withinResults[[i]]) == "Pr(>F)"] <- "p"
  }

  modelTerms <- .rmModelFormula(options)$termsRM.base64
  allCases <- rownames(withinResults[[1]])
  addResidualAfter <- allCases[.mapAnovaTermsToTerms(modelTerms, allCases) + (length(allCases) / length(modelTerms)) - 1]

  for (case in allCases) {

    for (i in seq_along(corrections)) {

      withinResults[[corrections[i]]][case, ".isNewGroup"] <- i == 1
      if (!is.na(withinResults[[corrections[i]]][case, "num Df"])) {
        anovaTable$addRows(as.list(withinResults[[corrections[i]]][case, ]),
                           rowNames=paste0(case, corrections[i]))
      } else {
        anovaTable$addFootnote(gettext("Sphericity corrections not available for factors with 2 levels."))
      }

    }

    if (case %in% modelTerms) {
      currentCase <- case
    }
    if (case %in% addResidualAfter) {

      for (i in seq_along(corrections)) {

        if (!is.na(withinResults[[corrections[i]]][currentCase, "num Df"])) {
          residualResults[[corrections[i]]][currentCase, ".isNewGroup"] <- i == 1
          anovaTable$addRows(as.list(residualResults[[corrections[i]]][currentCase, ]),
                           rowNames=paste0(currentCase, "Resid", corrections[i]))
        }
      }
    }
  }

  if (!all(is.na(withinResults[[1]][["Test statistic"]]))) {
    # when a RM factor consists of only 2 levels the p-value is NA, hence the is.na check
    violatedMauchlyCases <- rownames(mauchlyResult)[mauchlyResult[, "p-value"] < 0.05 & !is.na(mauchlyResult[, "p-value"])]
    if (length(violatedMauchlyCases) > 0)
      anovaTable$addFootnote(message = gettext("Mauchly's test of sphericity indicates that the assumption of sphericity is violated (p < .05)."),
                             colNames = c("Sum Sq", "num Df", "F value", "Mean Sq", "Pr(F)", "p"),
                             rowNames = paste0(violatedMauchlyCases, "None"))
  }

  return()
}

.rmAnovaAssumptionsContainer <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["assumptionsContainer"]]))
    return()

  assumptionsContainer <- createJaspContainer(title = gettext("Assumption Checks"),
                                              dependencies = c("homogeneityTests", "sphericityTests", "qqPlot",
                                                               "qqPlotCi", "qqPlotCiLevel"))

  rmAnovaContainer[["assumptionsContainer"]] <- assumptionsContainer

  if (options$homogeneityTests == TRUE)
    .rmAnovaLevenesTable(rmAnovaContainer, dataset, options, ready)

  if (options$sphericityTests == TRUE)
    .rmAnovaSphericityTable(rmAnovaContainer, dataset, options, ready)

  if (options$qqPlot == TRUE)
    .qqPlotFreqAnova(rmAnovaContainer, dataset, options, ready)

  return()
}

.rmAnovaLevenesTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["rmAnovaLevenesTable"]]))
    return()

  rmAnovaLevenesTable <- createJaspTable(gettext("Test for Equality of Variances (Levene's)"))
  rmAnovaContainer[["assumptionsContainer"]][["rmAnovaLevenesTable"]] <- rmAnovaLevenesTable

  rmAnovaLevenesTable$addColumnInfo(name="case", type="string", title="")
  rmAnovaLevenesTable$addColumnInfo(name="F", type="number")
  rmAnovaLevenesTable$addColumnInfo(name="df1", type="integer")
  rmAnovaLevenesTable$addColumnInfo(name="df2", type="integer")
  rmAnovaLevenesTable$addColumnInfo(name="p", type="pvalue")

  if (options$vovkSellke) {
    rmAnovaLevenesTable$addColumnInfo(title = "VS-MPR\u002A", name = "vovkSellke", type = "number")
    rmAnovaLevenesTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
  rmAnovaLevenesTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready || rmAnovaContainer$getError())
    return()

  rmAnovaLevenesTable$setExpectedSize(length(options$repeatedMeasuresCells))
  if (length(options[["betweenSubjectFactors"]]) == 0) {
    rmAnovaLevenesTable$setError(gettext("Cannot perform homogeneity tests because there are no between subjects factors specified."))
    return()
  }

  for (i in seq_along(options$repeatedMeasuresCells)) {

    interaction <- paste(.v(options$betweenSubjectFactors), collapse=":", sep="")
    if (length(options$covariates) > 0 ) {

      covterms <- paste(.v(options$covariates), collapse="+", sep="")
      combterms <- paste(c(interaction,covterms), collapse="+", sep="")
      levene.def <- paste(.v(options$repeatedMeasuresCells[i]), "~", combterms)

    } else {

      levene.def <- paste(.v(options$repeatedMeasuresCells[i]), "~", interaction)

    }

    levene.formula <- as.formula(levene.def)

    dummyAov <- aov(levene.formula, data = dataset, qr = T)
    resids <- abs(dummyAov$residuals)
    levene.def <- paste("resids", "~", interaction)
    levene.formula <- as.formula(levene.def)

    r <- summary(aov(levene.formula, dataset))
    error <- base::tryCatch(summary(aov(levene.formula, dataset)),error=function(e) e, warning=function(w) w)

    row <- data.frame(case = options$repeatedMeasuresCells[i],
                      F = r[[1]]$`F value`[1],
                      df1 = r[[1]]$Df[1],
                      df2 = r[[1]]$Df[2],
                      p=r[[1]]$`Pr(>F)`[1],
                      ".isNewGroup" = i == 1,
                      vovkSellke = VovkSellkeMPR(r[[1]]$`Pr(>F)`[1]))

    rmAnovaLevenesTable$addRows(row)

  }

  return()
}

.rmAnovaSphericityTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["sphericityTable"]]) || !options$sphericityTests)
    return()

  sphericityTable <- createJaspTable(gettext("Test of Sphericity"))

  sphericityTable$addColumnInfo(name = "case",            type = "string",  title = "")
  sphericityTable$addColumnInfo(name = "Test statistic",  type = "number",  title = gettext("Mauchly's W"))
  sphericityTable$addColumnInfo(name = "approxChi",       type = "number",  title = gettextf("Approx. %s", "\u03A7\u00B2"))
  sphericityTable$addColumnInfo(name = "dfSphericity",    type = "integer", title = gettext("df"))
  sphericityTable$addColumnInfo(name = "p-value",         type = "pvalue",  title = gettext("p-value"))
  sphericityTable$addColumnInfo(name = "GG eps",          type = "number",  title = gettextf("Greenhouse-Geisser %s", "\u03B5"))
  sphericityTable$addColumnInfo(name = "HF eps",          type = "number",  title = gettextf("Huynh-Feldt %s", "\u03B5"))
  sphericityTable$addColumnInfo(name = "LB",              type = "number",  title = gettextf("Lower Bound %s", "\u03B5"))

  sphericityTable$showSpecifiedColumnsOnly <- TRUE

  rmAnovaContainer[["assumptionsContainer"]][["sphericityTable"]] <- sphericityTable

  if(!ready)
    return()

  .approxChi <- function(df, n, W){
    d <- 1 - (2*df^2 + df + 2) / (6*df*(n-1))
    -(n-1)*d*log(W)
  }

  assumptionResult <- rmAnovaContainer[["anovaResult"]]$object$assumptionResult

  anovaResult <- rmAnovaContainer[["anovaResult"]]$object$withinAnovaTable$None

  # if (nrow(assumptionResult) == 0 || all(is.na(assumptionResult[["GG eps"]]))) {
  if (all(is.na(anovaResult[["Test statistic"]]))) {
    sphericityTable$setError(gettext("Cannot perform sphericity tests because there are only two levels of the RM factor, or because the SSP matrix is singular."))
    return()
  }

  df <- anovaResult[["num Df"]]
  anovaResult[["dfSphericity"]] <- df * (df + 1) / 2 - 1
  n  <- anovaResult[["den Df"]] / df + 1

  anovaResult[["approxChi"]] <- .approxChi(df, n, anovaResult[["Test statistic"]])
  anovaResult[["LB"]] <- 1 / df
  anovaResult[["HF eps"]] <- sapply(anovaResult[["HF eps"]], min, 1)
  anovaResult[[".isNewGroup"]] <- FALSE

  includeInTable <- rownames(anovaResult) %in% sapply(options$withinModelTerms, function(x) paste(.v(unlist(x)), collapse=":"))
  sphericityTable$setData(anovaResult[includeInTable & (!is.na(anovaResult[["approxChi"]])), ])

  return()
}

.referenceGrid <- function(rmAnovaContainer, options, ready) {
  if (!is.null(rmAnovaContainer[["referenceGrid"]]) || !ready || rmAnovaContainer$getError())
    return()

  fullModel <- rmAnovaContainer[["anovaResult"]]$object$fullModel

  referenceGridList <- list()

  variables <- sapply(c(options$withinModelTerms, options$betweenModelTerms),
                      function(x) {paste(.v(x$components), collapse = ":")})

  if (length(options$betweenModelTerms) > 0) {
    mixedTerms <- sapply(options$withinModelTerms,
                         function(x) {sapply(options$betweenModelTerms,
                                             function(y) {paste(c(.v(y$components), .v(x$components)), collapse = ":")})})
    variables <- union(variables, mixedTerms)
  }

  for (var in variables) {
    formula <- as.formula(paste("~", var))
    referenceGrid <- emmeans::emmeans(fullModel, formula)
    # after updating emmeans, check if this is still necessary by following the steps in https://github.com/jasp-stats/jasp-desktop/pull/4323
    environment(referenceGrid@dffun) <- baseenv()
    referenceGridList[[var]] <- referenceGrid
  }

  rmAnovaContainer[["referenceGrid"]] <- createJaspState(object = referenceGridList,
                                                         dependencies = c("withinModelTerms",
                                                                          "betweenModelterms"))

  return()
}

.rmAnovaPostHocTable <- function(rmAnovaContainer, dataset, longData, options, ready) {
  if(!is.null(rmAnovaContainer[["postHocStandardContainer"]]) || length(options$postHocTerms) ==0)
    return()

  postHocContainer <- createJaspContainer(title = gettext("Post Hoc Tests"))
  postHocContainer$dependOn(c("postHocTerms", "postHocEffectSize", "postHocCorrectionBonferroni",
                              "postHocCorrectionHolm", "postHocCorrectionScheffe", "postHocCorrectionTukey",
                              "postHocSignificanceFlag", "postHocCi", "postHocLetterAlpha", "postHocLetterTable",
                              "postHocCiLevel", "postHocConditionalTable"))

  rmAnovaContainer[["postHocStandardContainer"]] <- postHocContainer

  postHocVariables <- unlist(options$postHocTerms, recursive = FALSE)
  postHocVariablesListV <- unname(lapply(postHocVariables, .v))
  variables <- unname(sapply(postHocVariables, function(x) paste(.v(x), collapse = ":")))

  for (postHocVarIndex in 1:length(postHocVariables)) {
    thisTitle <- paste(postHocVariables[[postHocVarIndex]], collapse = " \u273B ")
    thisVarName <- paste(postHocVariables[[postHocVarIndex]], collapse = ":")
    byVariable <- if (options[["postHocConditionalTable"]] && length(postHocVariables[[postHocVarIndex]]) > 1) postHocVariables[[postHocVarIndex]] else NULL
    termsToLoop <- if (options[["postHocConditionalTable"]]) postHocVariables[[postHocVarIndex]] else 1
    dfType <- if (options[["poolErrorTermFollowup"]] && (length(postHocVariables[[postHocVarIndex]]) > 1)) "number" else "integer"
    for (termIndex in seq_along(termsToLoop))
      postHocContainer[[ paste0(thisVarName, termIndex)]] <- .createPostHocStandardTable(thisTitle, byVariable[termIndex], options, dfType = dfType)

    if (options[["postHocLetterTable"]]) {
      letterTable <- createJaspTable(title = paste0("Letter-Based Grouping - ", thisVarName))
      for (letterVar in postHocVariables[[postHocVarIndex]])
        letterTable$addColumnInfo(name=letterVar, type="string", combine = TRUE)

      letterTable$addColumnInfo(name="Letter", type="string")
      letterTable$addFootnote("If two or more means share the same grouping symbol,
        then we cannot show them to be different, but we also did not show them to be the same. ")
      letterTable$showSpecifiedColumnsOnly <- TRUE

      postHocContainer[[paste0(thisVarName, "LetterTable")]] <- letterTable
    }
  }

  if (!ready || rmAnovaContainer$getError())
    return()

  referenceGrid <- rmAnovaContainer[["referenceGrid"]]$object
  fullModel <- rmAnovaContainer[["anovaResult"]]$object$fullModel
  allNames <- unlist(lapply(options$repeatedMeasuresFactors, function(x) x$name)) # Factornames

  for (postHocVarIndex in seq_along(variables)) {

    thisVarName <- variables[postHocVarIndex]
    # create vector to loop over for conditional post hoc tables
    termsToLoop <- if (options[["postHocConditionalTable"]]) postHocVariables[[postHocVarIndex]] else 1

    for (termIndex in seq_along(termsToLoop)) {
      thisVarNameRef <- paste0(thisVarName, termIndex)
      byVariable <- if (length(termsToLoop) > 1) postHocVariables[[postHocVarIndex]] else NULL

      resultPostHoc <- summary(pairs(referenceGrid[[thisVarName]], adjust="holm", by = byVariable[termIndex]),
                               infer = TRUE, level = options$postHocCiLevel)
      numberOfLevels <- nrow(as.data.frame(referenceGrid[[thisVarName]]))
      bonfAdjustCIlevel <- .computeBonferroniConfidence(options$postHocCiLevel,
                                                        numberOfLevels = numberOfLevels)

      effectSizeResult <- as.data.frame(emmeans::eff_size(referenceGrid[[thisVarName]],
                                                          sigma = sqrt(mean(sigma(fullModel$lm)^2)),
                                                          edf = df.residual(fullModel$lm),
                                                          level = bonfAdjustCIlevel,
                                                          by = byVariable[termIndex]))

      resultPostHoc[["holm"]] <- resultPostHoc[["p.value"]]

      resultPostHoc[["tukey"]] <-  summary(pairs(referenceGrid[[thisVarName]], adjust="tukey",
                                                 by = byVariable[termIndex]))[["p.value"]]

      resultPostHoc[["scheffe"]] <-  summary(pairs(referenceGrid[[thisVarName]], adjust="scheffe",
                                                   by = byVariable[termIndex]))[["p.value"]]

      resultPostHoc[["bonferroni"]] <-  summary(pairs(referenceGrid[[thisVarName]], adjust="bonferroni",
                                                by = byVariable[termIndex]))[["p.value"]]

      resultPostHoc[["cohenD"]] <- effectSizeResult[["effect.size"]]
      resultPostHoc[["cohenD_LowerCI"]] <- effectSizeResult[["lower.CL"]]
      resultPostHoc[["cohenD_UpperCI"]] <- effectSizeResult[["upper.CL"]]

      comparisons <- strsplit(as.character(resultPostHoc$contrast), " - ")

      orderOfTerms <- unlist(lapply(options$repeatedMeasuresFactors, function(x) x$name))
      indexofOrderFactors <- match(allNames,orderOfTerms)

      if (any(postHocVariables[[postHocVarIndex]] %in% allNames)) {     ## If the variable is a repeated measures factor

        resultPostHoc[["scheffe"]] <- "."
        resultPostHoc[["tukey"]] <-  "."
        if (options$postHocCorrectionScheffe || options$postHocCorrectionTukey) {
          cors <- paste(c("Tukey", "Scheffe")[c(options$postHocCorrectionTukey, options$postHocCorrectionScheffe)], collapse = " and ")

          postHocContainer[[thisVarNameRef]]$addFootnote(gettextf("%s corrected p-values are not appropriate for repeated measures post-hoc tests (Maxwell, 1980; Field, 2012).", cors))
        }
      }

      resultPostHoc[["contrast_A"]] <- lapply(comparisons, function(x) paste(.unv(strsplit(x[[1]], "[ ,]")[[1]]),
                                                                             collapse = ", "))
      resultPostHoc[["contrast_B"]] <- lapply(comparisons, function(x) paste(.unv(strsplit(x[[2]], "[ ,]")[[1]]),
                                                                             collapse = ", "))

      if (nrow(resultPostHoc) > 1 && any(grepl(attr(resultPostHoc, "mesg"), pattern = "P value adjustment")))
        postHocContainer[[thisVarNameRef]]$addFootnote(.getCorrectionFootnoteAnova(resultPostHoc, options[["postHocCi"]], options[["postHocEffectSize"]]))

      avFootnote <- attr(resultPostHoc, "mesg")[grep(attr(resultPostHoc, "mesg"), pattern = "Results are averaged")]
      if (length(avFootnote) != 0) {
        avTerms <- .unv(strsplit(gsub(avFootnote, pattern = "Results are averaged over the levels of: ", replacement = ""),
                                 ", ")[[1]])
        postHocContainer[[thisVarNameRef]]$addFootnote(gettextf("Results are averaged over the levels of: %s", paste(avTerms, collapse = ", ")))
      }

      if (options[["postHocConditionalTable"]] & isFALSE(is.null(byVariable))) {
        resultPostHoc[[".isNewGroup"]] <- !duplicated(resultPostHoc[[byVariable[termIndex]]])
      } else {
        resultPostHoc[[".isNewGroup"]] <- !duplicated(resultPostHoc[["contrast_A"]])
      }

      postHocContainer[[thisVarNameRef]]$setData(resultPostHoc)

      if (options$postHocSignificanceFlag)
        .anovaAddSignificanceSigns(someTable = postHocContainer[[thisVarNameRef]],
                                   allPvalues = resultPostHoc[c("bonferroni", "scheffe", "tukey", "holm")],
                                   resultRowNames = rownames(resultPostHoc))

    }
    if (isTRUE(options[["postHocLetterTable"]])) {
      letterResult <- multcomp::cld(referenceGrid[[thisVarName]],
                                    method = "pairwise",
                                    Letters = letters,
                                    alpha = options[["postHocLetterAlpha"]])
      letterResult <- letterResult[c(postHocVariables[[postHocVarIndex]], ".group")]
      colnames(letterResult)[ncol(letterResult)] <- "Letter"
      letterResult <- letterResult[order(as.numeric(rownames(letterResult))), ]
      postHocContainer[[paste0(thisVarName, "LetterTable")]]$setData(letterResult)
    }
  }

  return()
}

.rmAnovaContrastTable <- function(rmAnovaContainer, longData, options, ready) {
  if (!ready || is.null(options$contrasts))
      return()

  #contrasts are encoded so first decode that so we can later check for things like "none" and "custom"
  decodedContrasts <- list()
  for (i in 1:length(options$contrasts)) {
    options$contrasts[[i]]$decoded <- decodedContrasts[[i]] <- jaspBase::decodeColNames(options$contrasts[[i]]$contrast)
  }

  if (!is.null(rmAnovaContainer[["contrastContainer"]]) || all(grepl("none", decodedContrasts)))
    return()

  contrastContainer <- createJaspContainer(title = gettext("Contrast Tables"))
  contrastContainer$dependOn(c("contrasts", "contrastCiLevel", "contrastEffectSize",
                               "contrastCi", "customContrasts"))

  for (contrast in options$contrasts) {

    if (contrast$decoded != "none") {

      contrastType <- unlist(strsplit(contrast$contrast, ""))
      contrastType[1] <- toupper(contrastType[1])
      contrastType <- paste0(contrastType, collapse = "")

      if (length(contrast$variable) == 1) {
        contrastVariable <- contrast$variable
      } else {
        contrastVariable <- paste(contrast$variable, collapse = " \u273B ")
      }

      myTitle <- gettextf("%1$s Contrast - %2$s", contrastType,  contrastVariable)
      contrastContainerName <- paste0(contrast$contrast, "Contrast_",  paste(contrast$variable, collapse = ":"))
      dfType <- if (length(contrast$variable) > 1 || contrast$decoded == "custom") "number" else "integer"
      contrastContainer[[contrastContainerName]] <- createJaspContainer()
      contrastContainer[[contrastContainerName]][["contrastTable"]] <- .createContrastTableAnova(myTitle,
                                                                                                 options,
                                                                                                 dfType)
    }

  }

  rmAnovaContainer[["contrastContainer"]] <- contrastContainer

  if (!ready || rmAnovaContainer$getError())
    return()

  referenceGrid <- rmAnovaContainer[["referenceGrid"]]$object

  for (contrast in options$contrasts) {

    contrastContainerName <- paste0(contrast$contrast, "Contrast_",  paste(contrast$variable, collapse = ":"))

    if (contrast$decoded != "none") {

      if (contrast$decoded == "custom") {
        customContrastSetup <- options$customContrasts[[which(sapply(options$customContrasts,
                                                                     function(x) all(contrast$variable %in% x$value) &&
                                                                       length(contrast$variable) == length(x$value)))]]
      } else {
        customContrastSetup <- NULL
      }

      if (length(contrast$variable) == 1) {
        column <- longData[[ .v(contrast$variable) ]]
      } else {
        column <- factor(apply(longData[ .v(contrast$variable) ], 1, paste, collapse =", "))
      }

      contrastMatrix    <- .createContrastAnova(column, contrast$decoded, customContrastSetup)
      contrCoef         <- lapply(as.data.frame(contrastMatrix), as.vector)

      if (contrast$decoded != "custom") {
        contrCoef         <- lapply(as.data.frame(contrastMatrix), as.vector)
        names(contrCoef)  <- .anovaContrastCases(column, contrast$decoded)
      } else {
        contrCoef         <- apply(contrastMatrix, 1, list)
      }

      contrastResult    <- try(emmeans::contrast(referenceGrid[[paste(contrast$variable, collapse = ":")]], contrCoef),
                               silent = TRUE)
      contrCoefEmmeans <- coef(contrastResult)
      colnames(contrCoefEmmeans) <- c(contrast$variable, paste("Comparison", 1: (ncol(contrCoefEmmeans) - length(contrast$variable))))

      fullModel <- rmAnovaContainer[["anovaResult"]]$object$fullModel
      effectSizeResult <- as.data.frame(emmeans::eff_size(referenceGrid[[paste(contrast$variable, collapse = ":")]],
                                                          sigma = sqrt(mean(sigma(fullModel$lm)^2)),
                                                          edf = df.residual(fullModel$lm),
                                                          method = contrCoef,
                                                          level = options[["contrastCiLevel"]]))

      if (contrast$decoded == "custom") {
        if (isTryError(contrastResult)) {
          if (grepl(contrastResult[1], pattern = "Nonconforming number")) {
            contrastContainer[[contrastContainerName]]$setError(gettext("Please specify an additional contrast."))
          } else if (grepl(contrastResult[1], pattern = "number of contrast matrix rows")) {
            contrastContainer[[contrastContainerName]]$setError(gettext("Wrong number of custom contrast matrix rows."))
          }
          return()
        } else if (any(apply(contrastMatrix, 1, function(x) all(x == 0) ))) {
          contrastContainer[[contrastContainerName]]$setError(gettext("Please specify non-zero contrast weights."))
          return()
        }
      }

      if (length(contrastResult@misc$avgd.over) != 0)
        contrastContainer[[contrastContainerName]][["contrastTable"]]$addFootnote(
          message = gettextf("Results are averaged over the levels of: %s", paste(.unv(contrastResult@misc$avgd.over), collapse = ", ")))

      contrastResult <- cbind(contrastResult, confint(contrastResult, level = options$contrastCiLevel)[,5:6])
      contrastResult[["Comparison"]] <- contrastResult[["contrast"]]
      contrastResult[["df"]] <- round(contrastResult[["df"]], 3)

      contrastResult[["cohenD"]] <- effectSizeResult[["effect.size"]]
      contrastResult[["cohenD_LowerCI"]] <- effectSizeResult[["lower.CL"]]
      contrastResult[["cohenD_UpperCI"]] <- effectSizeResult[["upper.CL"]]

      if (contrast$decoded == "custom" | length(contrast$variable) > 1) {
        contrastResult$Comparison <- 1:nrow(contrastResult)
        weightType <-  if (all(apply(contrastMatrix, 2, function(x) x %% 1 == 0))) "integer" else "number"
        contrastContainer[[contrastContainerName]][["customCoefTable"]] <- .createContrastCoefficientsTableAnova(contrast,
                                                                                                                 contrCoefEmmeans,
                                                                                                                 weightType)
      }
      contrastContainer[[contrastContainerName]][["contrastTable"]]$setData(contrastResult)

    }
  }

}

.rmAnovaMarginalMeansTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["marginalMeansContainer"]]) || length(options$marginalMeanTerms) == 0)
    return ()

  marginalMeansContainer <- createJaspContainer(title = gettext("Marginal Means"))
  marginalMeansContainer$dependOn(c("marginalMeanTerms",  "marginalMeanComparedToZero", "marginalMeanCiCorrection",
                                    "marginalMeanBootstrap", "marginalMeanBootstrapSamples"))

  rmAnovaContainer[["marginalMeansContainer"]] <- marginalMeansContainer

  marginalTerms <- unlist(options$marginalMeanTerms, recursive = FALSE)


  for (term in marginalTerms) {
    thisVarName <- paste(term, collapse = " \u273B ")
    individualTerms <- term
    if (any(term %in% unlist(options$withinModelTerms))) dfType <- "number" else dfType <- "integer"
    marginalMeansContainer[[paste0(.v(term), collapse = ":")]] <- .createMarginalMeansTableAnova(thisVarName, options,
                                                                                                 individualTerms,
                                                                                                 options$marginalMeanBootstrap,
                                                                                                 dfType = dfType)
  }

  if (!ready || rmAnovaContainer$getError())
    return()

  fullModel <- rmAnovaContainer[["anovaResult"]]$object$fullModel

  for (term in marginalTerms) {

    termBase64 <- paste0(.v(term), collapse = ":")

    formula <- as.formula(paste("~", termBase64))

    if(options$marginalMeanCiCorrection == "bonferroni") {
      adjMethod <- "bonferroni"
    } else if(options$marginalMeanCiCorrection == "sidak") {
      adjMethod <- "sidak"
    } else {
      adjMethod <- "none"
    }

    marginalResult <- summary(emmeans::lsmeans(fullModel, formula), adjust = adjMethod, infer = c(TRUE,TRUE))

    marginalResult[[".isNewGroup"]] <- FALSE
    marginalResult[[".isNewGroup"]][which(marginalResult[, 1] == marginalResult[1, 1])] <- TRUE

    names(marginalResult)[1:length(term)] <- term

    for (var in term)
      marginalResult[[var]] <- .unv(marginalResult[[var]])


    ### Bootstrapping
    if (options$marginalMeanBootstrap) {

      startProgressbar(options[["marginalMeanBootstrapSamples"]],
                       label = gettext("Bootstrapping Marginal Means"))

      nRows <- nrow(marginalResult)

      bootstrapMarginalMeans <- try(boot::boot(data = dataset, statistic = .bootstrapMarginalMeansRmAnova,
                                               R = options[["marginalMeanBootstrapSamples"]],
                                               options = options,
                                               nRows = nRows,
                                               termLength = length(term),
                                               formula = formula), silent = TRUE)

      if (class(bootstrapMarginalMeans) == "try-error") {
        marginalMeansContainer[[termBase64]]$setError(bootstrapMarginalMeans)
        next
      }

      bootstrapSummary <- summary(bootstrapMarginalMeans)

      ci.fails <- FALSE
      # bootstrapMarginalMeansCI <- confint(bootstrapMarginalMeans, level = 0.95, type = c("bca"))
      bootstrapMarginalMeansCI <- t(sapply(1:nrow(bootstrapSummary), function(index){
        res <- try(boot::boot.ci(boot.out = bootstrapMarginalMeans, conf = 0.95, type = "bca",
                                 index = index)[['bca']][1,4:5])
        if (!inherits(res, "try-error")){
          return(res)
        } else {
          ci.fails <<- TRUE
          return(c(NA, NA))
        }
      }))

      if (ci.fails)
        marginalMeansContainer[[termBase64]]$addFootnote(message = gettext("Some confidence intervals could not be computed. Possibly too few bootstrap replicates."))

      marginalResult[["lower.CL"]] <- bootstrapMarginalMeansCI[,1]
      marginalResult[["upper.CL"]] <- bootstrapMarginalMeansCI[,2]

      marginalResult[["lsmean"]]   <- bootstrapSummary[["bootMed"]]
      marginalResult[["bias"]]     <- bootstrapSummary[["bootBias"]]
      marginalResult[["SE"]]       <- bootstrapSummary[["bootSE"]]

      marginalMeansContainer[[termBase64]]$addFootnote(message = gettextf("Bootstrapping based on %s successful replicates.", as.character(bootstrapSummary$R[1])))
    }

    marginalMeansContainer[[termBase64]]$setData(as.data.frame(marginalResult))

  }

  return()
}

.bootstrapMarginalMeansRmAnova <- function(data, indices, options, nRows, termLength, formula){

  # indices <- sample(indices, replace = TRUE)
  resamples <- data[indices, , drop=FALSE]

  dataset <- .shortToLong(resamples, options$repeatedMeasuresFactors, options$repeatedMeasuresCells,
                          c(options$betweenSubjectFactors, options$covariates),
                          dependentName = .BANOVAdependentName, subjectName = .BANOVAsubjectName)

  anovaModelBoots <- .rmAnovaComputeResults(dataset, options, returnResultsEarly = TRUE)$result # refit model

  if (!is.null(anovaModelBoots[["tryResult"]]) || is.null(anovaModelBoots))
    return(rep(NA, termLength))

  resultBoots <- summary(emmeans::lsmeans(anovaModelBoots, formula), infer = c(FALSE,FALSE))
  progressbarTick()

  if(length(resultBoots$lsmean) == nRows){ # ensure that the bootstrap has all levels
    return(resultBoots$lsmean)
  } else {
    return(rep(NA, termLength))
  }
}

.rmAnovaFriedmanTable <- function(rmAnovaContainer, longData, options, ready) {
  if (!is.null(rmAnovaContainer[["nonparametricContainer"]]) || length(options$friedmanWithinFactor) == 0)
    return ()

  rmAnovaContainer[["nonparametricContainer"]] <- createJaspContainer(gettext("Nonparametrics"))
  rmAnovaContainer[["nonparametricContainer"]]$dependOn(c("friedmanWithinFactor",
                                                          "friedmanBetweenFactor"))

  friedmanTable <- createJaspTable(title = gettext("Friedman Test"))
  friedmanTable$addColumnInfo(name="Factor",  title=gettext("Factor"),      type="string")
  friedmanTable$addColumnInfo(name="chiSqr",  title=gettext("\u03a7\u00b2<sub>F</sub>"), type="number")
  friedmanTable$addColumnInfo(name="df",      title=gettext("df"),          type="integer")
  friedmanTable$addColumnInfo(name="p",       title=gettext("p"),           type="pvalue")
  friedmanTable$addColumnInfo(name="kendall", title=gettext("Kendall's W"), type="number")

  rmAnovaContainer[["nonparametricContainer"]][["friedmanTable"]] <- friedmanTable

  if (!ready || rmAnovaContainer$getError())
    return()

  withinTerms <- options$friedmanWithinFactor
  betweenTerm <- options$friedmanBetweenFactor

  withinTerms.base64 <- .v(withinTerms)
  betweenTerms.base64 <- .v(betweenTerm)

  result <- list()

  if( any(!(withinTerms %in% unlist(options$withinModelTerms))) ||
      (betweenTerm %in% unlist(options$withinModelTerms)) ) {
    friedmanTable$setError(gettext("Please specify appropriate terms for the Friedman/Durbin test."))
    return()
  }

  if (identical(betweenTerm, "")) {
    betweenTerms.base64 <- .BANOVAsubjectName
  }

  rows <- list()

  for (withinTermIndex in 1:length(withinTerms)) {

    groups <- as.factor(longData[, withinTerms.base64[withinTermIndex]])
    blocks <- as.factor(longData[, betweenTerms.base64])
    y <- longData[, .BANOVAdependentName]

    useDurbin <- any(table(groups, blocks) < 1) && length(unique(table(groups))) == 1

    if (any(table(groups, blocks) > 1)) {
      friedmanTable$setError(gettextf("Not an unreplicated complete block design. The Friedman test is only available as an alternative to one-way RM ANOVA."))
      return()
    }

    t <- nlevels(groups)
    b <- nlevels(blocks)
    r <- unique(table(groups))
    k <- unique(table(blocks))

    if (length(r) == 1 && length(k) == 1) {
      rankPerBlock <- unlist(tapply(y, blocks, rank))
      rankPerGroup <- unlist(tapply(y, groups, rank))

      rankJ <- tapply(rankPerBlock, groups, sum)

      sumRanks <- sum(rankPerBlock^2)
      cVal <- (b * k * (k + 1)^2) / 4
      dVal <- sum(rankJ^2) - r * cVal
      testStatOne <- (t - 1) / (sumRanks - cVal) * dVal
      testStatTwo <- (testStatOne / (t - 1)) / ((b * k - b - testStatOne) / (b * k - b - t + 1))

      ## Code from PMCMRplus
      dfChi <- t - 1
      dfOneF <- k - 1
      dfTwoF <- b * k - b - t + 1
      pValOne <- pchisq(testStatOne, dfChi, lower.tail = FALSE)
      pValTwo <- pf(testStatTwo, dfOneF, dfTwoF, lower.tail = FALSE)

      # Kendall W, from descTools https://github.com/cran/DescTools/blob/513a58f635798dacf49982c396e2af9a81f3491d/R/StatsAndCIs.r#L5553-L5625
      ratings <- t(do.call(cbind, tapply(y, groups, cbind)))
      ratings <- as.matrix(na.omit(ratings))

      ns <- nrow(ratings)
      nr <- ncol(ratings)

      ratings.rank <- apply(ratings, 2, rank)

      tieAdjust <- 0
      for (i in 1:nr) {
        rater <- table(ratings.rank[,i])
        ties  <- rater[rater>1]
        l 	  <- as.numeric(ties)
        tieAdjust	  <- tieAdjust + sum(l^3-l)
      }
      kendallWcor <- (12 * var(apply(ratings.rank, 1, sum)) * (ns - 1)) /
        (nr^2 * (ns^3 - ns) - nr * tieAdjust)

      row <- list()
      row[["Factor"]] <- withinTerms[withinTermIndex]
      row[["chiSqr"]] <- testStatOne
      row[["df"]] <- dfChi
      row[["p"]] <- pValOne
      row[["kendall"]] <- kendallWcor

      if (useDurbin) {
        friedmanTable$title <- gettext("Durbin Test")

        row[["F"]] <- testStatTwo
        row[["dfnum"]] <- dfOneF
        row[["dfden"]] <- dfTwoF
        row[["pF"]] <- pValTwo

        if (withinTermIndex == 1) {
          friedmanTable$addColumnInfo(name="F",     title=gettext("F"),             type="number")
          friedmanTable$addColumnInfo(name="dfnum", title=gettext("df num"),        type="integer")
          friedmanTable$addColumnInfo(name="dfden", title=gettext("df den"),        type="integer")
          friedmanTable$addColumnInfo(name="pF",    title=gettext("p<sub>F</sub>"), type="pvalue")
        }

      }

      rows[[withinTermIndex]] <- row

    } else {

      friedmanTable$setError("Specified ANOVA design is not balanced.")
      return()

    }

  }

  friedmanTable$setData(as.data.frame(do.call(rbind,rows)))

  return()
}

.rmAnovaConoverTable <- function(rmAnovaContainer, longData, options, ready) {
  if (!is.null(rmAnovaContainer[["nonparametricContainer"]][["conoverContainer"]]) || options$conoverTest == FALSE)
    return ()

  conoverContainer <- createJaspContainer(gettext("Conover Test"))
  rmAnovaContainer[["nonparametricContainer"]][["conoverContainer"]] <- conoverContainer
  conoverContainer$dependOn("conoverTest")

  createConoverTable <- function(myTitle) {

    conoverTable <- createJaspTable(title = gettextf("Conover's Post Hoc Comparisons - %s", myTitle))

    conoverTable$addColumnInfo(name="(I)",        title="",                          type="string", combine=TRUE)
    conoverTable$addColumnInfo(name="(J)",        title="",                          type="string")
    conoverTable$addColumnInfo(name="t",          title=gettext("T-Stat"),           type="number")
    conoverTable$addColumnInfo(name="df",         title=gettext("df"),               type="integer")
    conoverTable$addColumnInfo(name="wA",         title=gettext("W<sub>i</sub>"),    type="number")
    conoverTable$addColumnInfo(name="wB",         title=gettext("W<sub>j</sub>"),    type="number")
    conoverTable$addColumnInfo(name="rbs",        title=gettext("r<sub>rb</sub>"),   type="number")
    conoverTable$addColumnInfo(name="pval",       title=gettext("p"),                type="pvalue")
    conoverTable$addColumnInfo(name="bonferroni", title=gettext("p<sub>Bonf</sub>"), type="pvalue")
    conoverTable$addColumnInfo(name="holm",       title=gettext("p<sub>Holm</sub>"), type="pvalue")

    return(conoverTable)
  }


  if (!ready || rmAnovaContainer$getError())
    return()


  groupingVariables <- unlist(options$friedmanWithinFactor)
  blockingVar <- ifelse( identical(options$friedmanBetweenFactor, ""), .BANOVAsubjectName, .v(options$friedmanBetweenFactor))
  y <- longData[, .BANOVAdependentName]

  for (groupingVar in groupingVariables) {

    conoverTable <- createConoverTable(groupingVar)
    noteBlockName <- ifelse(blockingVar == .BANOVAsubjectName, "subject", blockingVar)
    conoverTable$addFootnote(gettextf("Grouped by %s.", noteBlockName))
    conoverTable$addFootnote(message = gettext("Rank-biserial correlation based on individual signed-rank tests."))

    rows <- list()

    groups <- as.factor(longData[, .v(groupingVar)])
    blocks <- as.factor(longData[, blockingVar])

    groupNames <- levels(groups)
    ## Code from PMCMRplus
    t <- nlevels(groups)
    b <- nlevels(blocks)
    r <- unique(table(groups))
    k <- unique(table(blocks))
    rij <- unlist(tapply(y, blocks, rank))
    Rj <- unname(tapply(rij, groups, sum))

    df <- b * k - b - t + 1

    S2 <- 1 / ( 1 * t -1 ) * (sum(rij^2) - t * b * ( t + 1)^2 / 4)
    T2 <- 1 / S2 * (sum((Rj - b * (t + 1) / 2)^2))
    A <- S2 * (2 * b * (t - 1)) / ( b * t - t - b + 1)
    B <- 1 - T2 / (b * (t- 1))
    denom <- sqrt(A) * sqrt(B)

    for (i in 1:(t - 1)) {

      for (j in (i + 1):t) {

        row <- list("(I)"=groupNames[[i]], "(J)"=groupNames[[j]])

        diff <-  abs(Rj[i] - Rj[j])
        tval <- diff / denom
        pval <- 2 * pt(q = abs(tval), df = df, lower.tail=FALSE)

        # Rank biserial correlation calculation
        y1 <- y[groups == levels(groups)[i]]
        y2 <- y[groups == levels(groups)[j]]
        res <- stats::wilcox.test(y1, y2, paired = TRUE, conf.int = FALSE)$statistic
        nd   <- sum(y1 - y2 != 0)
        maxw <- (nd * (nd + 1)) / 2
        rbs    <- as.numeric((res / maxw) * 2 - 1)
        # rbs <- (Rj[i] - n_i * (n_i + 1) / 2 - (Rj[j] - n_j * (n_j + 1) / 2)) / (n_i * n_j)

        row[["t"]] <- tval
        row[["wA"]]  <- Rj[i]
        row[["wB"]] <- Rj[j]
        row[["pval"]] <- pval
        row[["bonferroni"]] <- pval
        row[["holm"]] <- pval
        row[["df"]] <- df
        row[["rbs"]] <- rbs

        rows[[length(rows)+1]] <- row

      }

      row[[".isNewGroup"]] <- length(rows) == 0

    }

    allP <- unlist(lapply(rows, function(x) x$p))
    allBonf <- p.adjust(allP, method = "bonferroni")
    allHolm <- p.adjust(allP, method = "holm")

    for (p in 1:length(rows)) {
      rows[[p]][['bonferroni']] <- allBonf[p]
      rows[[p]][['holm']] <- allHolm[p]
    }

    conoverTable$setData(as.data.frame(do.call(rbind, rows)))
    rmAnovaContainer[["nonparametricContainer"]][["conoverContainer"]][[groupingVar]] <- conoverTable
  }

  return()
}

.rmAnovaSimpleEffects <- function(rmAnovaContainer, dataset, longData, options, ready) {
  if (!is.null(rmAnovaContainer[["simpleEffectsContainer"]]) || identical(options$simpleMainEffectFactor, "") ||
      identical(options$simpleMainEffectModeratorFactorOne, ""))
    return()

  rmAnovaContainer[["simpleEffectsContainer"]] <- createJaspContainer(title = gettext("Simple Main Effects"),
                                                                      dependencies = c("simpleMainEffectFactor",
                                                                                       "simpleMainEffectModeratorFactorOne",
                                                                                       "simpleMainEffectModeratorFactorTwo",
                                                                                       "simpleMainEffectErrorTermPooled"))

  simpleEffectsTable <- createJaspTable(title = gettextf("Simple Main Effects - %s", options$simpleMainEffectFactor))
  rmAnovaContainer[["simpleEffectsContainer"]][["simpleEffectsTable"]] <- simpleEffectsTable

  # the simple factor must appear in the model terms, but the moderator factors may be missing.
  withinModelTerms <- unique(unlist(lapply(options[["withinModelTerms"]], `[[`, "components"), use.names = FALSE))
  betweenModelTerms <- unique(unlist(lapply(options[["betweenModelTerms"]], `[[`, "components"), use.names = FALSE))

  if (!(options[["simpleMainEffectFactor"]] %in% c(withinModelTerms, betweenModelTerms))) {
    rmAnovaContainer[["simpleEffectsContainer"]]$setError(gettext("Moderator factors must also appear in the model terms!"))
    return()
  }

  moderatorTerms <- c(options$simpleMainEffectModeratorFactorOne, options$simpleMainEffectModeratorFactorTwo[!identical(options$simpleMainEffectModeratorFactorTwo, "")])
  nMods <- length(moderatorTerms)
  simpleMainEffectFactor <- options[['simpleMainEffectFactor']]
  simpleMainEffectFactorBase64 <- simpleMainEffectFactor

  simpleEffectsTable[["title"]] <- gettextf("Simple Main Effects - %s", options$simpleMainEffectFactor)

  simpleEffectsTable$addColumnInfo(name = "modOne", title = gettextf("Level of %s", moderatorTerms[1]),
                                   type = "string", combine = TRUE)

  if (nMods == 2)
    simpleEffectsTable$addColumnInfo(name = "modTwo", title = gettextf("Level of %s", moderatorTerms[2]),
                                     type = "string", combine = TRUE)


  simpleEffectsTable$addColumnInfo(name = "SumSq",  type = "number",  title = gettext("Sum of Squares"))
  simpleEffectsTable$addColumnInfo(name = "Df",     type = "integer", title = gettext("df"))
  simpleEffectsTable$addColumnInfo(name = "MeanSq", type = "number",  title = gettext("Mean Square"))
  simpleEffectsTable$addColumnInfo(name = "F",      type = "number",  title = gettext("F"))
  simpleEffectsTable$addColumnInfo(name = "p",      type = "pvalue",  title = gettext("p"))

  simpleEffectsTable$showSpecifiedColumnsOnly <- TRUE

  .addSumSquaresFootnote(simpleEffectsTable, options)

  simpleEffectsTable$addCitation("Howell, D. C. (2002). Statistical Methods for Psychology (8th. ed.). Pacific Grove, CA: Duxberry. ")

  if (!ready || rmAnovaContainer$getError())
    return()

  lvls <- list()
  factors <- list()

  for (variable in moderatorTerms) {

    factor <- longData[[ .v(variable) ]]
    factors[[length(factors)+1]] <- factor
    lvls[[variable]] <- levels(factor)

  }

  simpleEffectResult <- rev(expand.grid(rev(lvls), stringsAsFactors = FALSE))
  colnames(simpleEffectResult) <- c("modOne", "modTwo")[1:nMods]

  simpleEffectResult[[".isNewGroup"]] <- FALSE

  emptyCaseIndices <- emptyCases <- NULL
  allSimpleModels <- list()

  fullResidualTable <- rmAnovaContainer[["anovaResult"]][["object"]][["residualTable"]][["None"]]

  isMixedAnova <-   length(options[['betweenModelTerms']]) > 0
  isSimpleMainEffectFactorWithin <- !simpleMainEffectFactor %in% unlist(options[['betweenModelTerms']] )
  isModeratorOneWithin <- !moderatorTerms[1] %in% unlist(options[['betweenModelTerms']] )
  isModeratorTwoWithin <- !moderatorTerms[2] %in% unlist(options[['betweenModelTerms']] )


  if (isMixedAnova && !isSimpleMainEffectFactorWithin) {

    fullAnovaMS <- fullResidualTable["BetweenResidualResults", "Mean Sq"]
    fullAnovaDf <- fullResidualTable["BetweenResidualResults", "num Df"]

  } else {

    fullAnovaMS <- fullResidualTable[simpleMainEffectFactorBase64, "Mean Sq"]
    fullAnovaDf <- fullResidualTable[simpleMainEffectFactorBase64, "num Df"]

  }

  # Remove moderator factors from model terms
  simpleOptions <- options
  simpleOptions$betweenModelTerms <-  options$betweenModelTerms[!(grepl(moderatorTerms[1], options$betweenModelTerms) |
                                                      grepl(moderatorTerms[nMods], options$betweenModelTerms))]
  simpleOptions$withinModelTerms <-  options$withinModelTerms[!(grepl(moderatorTerms[1], options$withinModelTerms) |
                                                            grepl(moderatorTerms[nMods], options$withinModelTerms))]

  performBetweenAnova <- length(simpleOptions$withinModelTerms) == 0

  if (performBetweenAnova) {

    simpleOptions[["fixedFactors"]]  <- simpleOptions[['betweenSubjectFactors']]
    simpleOptions[["modelTerms"]] <- simpleOptions[['betweenModelTerms']]
    simpleOptions[["dependent"]] <- .BANOVAdependentName
    simpleOptions[["homogeneityCorrectionBrown"]] <- simpleOptions[["homogeneityCorrectionWelch"]] <- FALSE
    simpleOptions[["homogeneityCorrectionNone"]] <- TRUE

  }

  emptyCaseIndices <- emptyCases <- NULL

  for (i in 1:nrow(simpleEffectResult)) {

    subsetStatement  <- eval(parse(text=paste("longData$", .v(moderatorTerms), " == \"",
                                              simpleEffectResult[i, 1:nMods],
                                              "\"", sep = "", collapse = " & ")))
    simpleDataset <- base::subset(longData, subsetStatement)

    if (simpleEffectResult[i, nMods] == lvls[[ nMods ]][1])
      simpleEffectResult[[i, ".isNewGroup"]] <- TRUE

    if (nrow(simpleDataset) < 2 ||
        nrow(unique(simpleDataset[simpleMainEffectFactorBase64])) <  nrow(unique(longData[simpleMainEffectFactorBase64])) ||
        length(unique(simpleDataset[["JaspColumn_.subject._Encoded"]])) == 1) {

      emptyCaseIndices <- c(emptyCaseIndices, i)
      emptyCases <- c(emptyCases, paste(simpleEffectResult[i, 1:nMods], collapse = ", "))
      thisRow <- rep(NA, 5)

    } else if (performBetweenAnova) {

      .anovaModelContainer(rmAnovaContainer[["simpleEffectsContainer"]], simpleDataset, simpleOptions, TRUE)
      .anovaResult(rmAnovaContainer[["simpleEffectsContainer"]], options = simpleOptions)
      anovaResult <- rmAnovaContainer[["simpleEffectsContainer"]][["anovaResult"]]$object$result

      rmAnovaContainer[["simpleEffectsContainer"]][["model"]] <- NULL

      if (!options$simpleMainEffectErrorTermPooled) {
        fullAnovaMS <-  anovaResult["Residuals", "Mean Sq"]
        fullAnovaDf <-  anovaResult["Residuals", "Df"]
      }

      anovaResult <- anovaResult[simpleMainEffectFactorBase64, ]
      df <- anovaResult[["Df"]]
      MS <- anovaResult[["Mean Sq"]] <- anovaResult[["Sum Sq"]] /  df
      fStat <- MS / fullAnovaMS
      p <- pf(fStat, df, fullAnovaDf, lower.tail = FALSE)
      thisRow <- c(anovaResult[["Sum Sq"]], MS, df, fStat, p)
    } else {

      anovaResult <-  .rmAnovaComputeResults(simpleDataset, simpleOptions, returnResultsEarly = TRUE)$model[simpleMainEffectFactorBase64, ]

      if (!options$simpleMainEffectErrorTermPooled) {
        fullAnovaMS <-  anovaResult[["Error SS"]] / anovaResult[["den Df"]]
        fullAnovaDf <-  anovaResult[["den Df"]]
      }

      df <- anovaResult[["num Df"]]
      MS <- anovaResult[["Mean Sq"]] <- anovaResult[["Sum Sq"]] /  df
      fStat <- MS / fullAnovaMS
      p <- pf(fStat, df, fullAnovaDf, lower.tail = FALSE)
      thisRow <- c(anovaResult[["Sum Sq"]], MS, df, fStat, p)
    }


    simpleEffectResult[i, c("SumSq", "MeanSq", "Df", "F", "p")] <- thisRow
  }

  if (!is.null(emptyCaseIndices)) {
    simpleEffectsTable$addFootnote(gettextf("Not enough observations in cells %s.",
                                          paste0(" (", emptyCases, ")", collapse = ",")))
  }

  simpleEffectsTable$setData(simpleEffectResult)

  return()
}
