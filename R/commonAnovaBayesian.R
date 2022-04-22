#
# Copyright (C) 2013-2022 University of Amsterdam
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

# Abbreviations:
# HF = helper function

.BANOVArunAnalysis <- function(jaspResults, dataset, options, analysisType = c("ANOVA", "ANCOVA", "RM-ANOVA")) {

  # the main workhorse of the Bayesian ANOVA, ANCOVA, and Repeated Measures.

  analysisType <- match.arg(analysisType)
  dataset  <- .BANOVAreadData(dataset, options, analysisType)
  if (isTryError(dataset))
    .quitAnalysis(gettext("Error while loading data. Please verify your repeated measures observations."))

  dataInfo <- .BANOVAerrorhandling(dataset, options, analysisType)

  model <- .BANOVAestimateModels(jaspResults, dataset, options, dataInfo, analysisType)
  model[["posteriors"]] <- .BANOVAestimatePosteriors(jaspResults, dataset, options, model)

  .BANOVAeffectsTable  (jaspResults, options, model)
  .BANOVAestimatesTable(jaspResults, options, model)
  .BANOVArsquaredTable (jaspResults, options, model)

  # model averaged plots
  .BANOVAposteriorPlot(jaspResults, dataset, options, model)
  .BANOVAqqplot       (jaspResults, options, model)
  .BANOVArsqplot      (jaspResults, options, model)

  .BANOVAnullControlPostHocTable(jaspResults, dataset, options, model)

  # single model plots and tables
  .BANOVAsmi(jaspResults, dataset, options, model)

  # descriptives
  .BANOVAdescriptives(jaspResults, dataset, options, dataInfo, analysisType)

  return()
}

.BANOVAerrorhandling <- function(dataset, options, analysisType) {

  customChecks <- list()

  if (analysisType != "RM-ANOVA") {
    hasDV <- options$dependent != ""
    hasIV <- any(lengths(options[c("fixedFactors", "covariates")]) != 0)
    fixed <- options$fixedFactors
    noVariables <- !(hasDV && hasIV)
    target <- c(options$covariates, options$dependent)

    # check if the last model has an interaction effect
    mostComplexModel <- NULL
    if (length(options[["modelTerms"]]) > 0L) {
      mostComplexModel <- options[["modelTerms"]][[length(options[["modelTerms"]])]][["components"]]
      mostComplexModel <- intersect(mostComplexModel, fixed)
    }

    if (length(mostComplexModel) > 1L)
      customChecks[["missingInteractionCells"]] <- .BANOVAmissingInteractionCells

    if (!noVariables) {
      .hasErrors(
        dataset = dataset,
        type    = c("infinity", "observations", "variance", "factorLevels", "duplicateColumns"),
        infinity.target     = target,
        variance.target     = target,
        observations.target = target,
        observations.amount = paste("<", length(options$modelTerms) + 1),
        factorLevels.target = fixed,
        factorLevels.amount = " < 2",
        # custom checks
        custom              = customChecks,
        missingInteractionCells.target = mostComplexModel,
        exitAnalysisIfErrors = TRUE
      )
    }

  } else {
    hasDV  <- !any(options$repeatedMeasuresCells == "")
    hasIV  <- any(lengths(options[c("betweenSubjectFactors", "covariates")]) != 0)
    fixed  <- unlist(options$betweenSubjectFactors)
    covariates <- unlist(options$covariates)
    noVariables <- !hasDV
    target <- c(covariates, fixed)

    # check if the last model has an interaction effect
    mostComplexModel <- NULL
    if (length(options[["modelTerms"]]) > 0L) {
      mostComplexModel <- options[["modelTerms"]][[length(options[["modelTerms"]])]][["components"]]
      mostComplexModel <- intersect(mostComplexModel, fixed)
    }

    if (length(mostComplexModel) > 1L)
      customChecks[["missingInteractionCells"]] <- .BANOVAmissingInteractionCells

    if (length(target) > 0L) {
      .hasErrors(
        dataset = dataset,
        type    = c("infinity", "observations", "variance", "factorLevels", "duplicateColumns"),
        infinity.target     = target,
        variance.target     = covariates,
        observations.target = target,
        observations.amount = paste("<", length(options$modelTerms) + 1),
        factorLevels.target = fixed,
        factorLevels.amount = " < 2",
        duplicateColumns.target = target,
        # custom checks
        custom              = customChecks,
        missingInteractionCells.target = mostComplexModel,
        exitAnalysisIfErrors = TRUE
      )
    }

    if (!noVariables) {

      # messages from commonmessages.R
      customChecks <- list(
        infinity = function() {
          x <- dataset[, .BANOVAdependentName]
          if (any(is.infinite(x)))
            return(gettext("Infinity found in repeated measures cells."))
          return(NULL)
        },
        observations = function() {
          x <- dataset[, .BANOVAdependentName]
          nObs <- length(options$modelTerms) + 1
          if (length(na.omit(x)) <= nObs)
            return(gettextf("Number of observations is < %s in repeated measures cells", nObs))
          return(NULL)
        },
        variance = function() {
          x <- dataset[, .BANOVAdependentName]
          validValues <- x[is.finite(x)]
          variance <- 0
          if (length(validValues) > 1)
            variance <- stats::var(validValues)
          if (variance == 0)
            return(gettext("The variance in the repeated measures cells is 0."))
          return(NULL)
        },
        duplicateColumns = function() {
          cols <- c(.BANOVAsubjectName, target)
          if (length(cols) == 1L)
            return()

          datasetList <- as.list(dataset[, cols])
          duplicatedCols <- duplicated(datasetList) | duplicated(datasetList, fromLast = TRUE)
          if (any(duplicatedCols)) {
            if (duplicatedCols[1L]) {
              msg <- gettextf("Duplicate variables encountered in repeated measures cells, %s",
                              paste(target[duplicatedCols[-1L]], collapse = ", "))
            } else {
              msg <- gettextf("Duplicate variables encountered in %s",
                              paste(target[duplicatedCols[-1L]], collapse = ", "))
            }
            return(msg)
          }
          return(NULL)
        },
        redundantParticipantColumn = function() {

          if (nlevels(dataset[[.BANOVAsubjectName]]) == nlevels(dataset[[target]])) {
            return(gettextf("Duplicate participant column added to model terms, %s", target))
          } else {
            return()
          }

        }
      )

      .hasErrors(
        dataset = dataset,
        target = target,
        custom = customChecks,
        exitAnalysisIfErrors = TRUE
      )

    }

  }

  return(list(noVariables = noVariables, hasIV = hasIV, hasDV = hasDV))
}

.BANOVAmissingInteractionCells <- function(dataset, target) {

  # sorts a data frame by all its columns
  sortDataFrame <- function(df) {
    df[do.call(order, df), ]
  }

  uniqueObservedCombinations <- sortDataFrame(unique(dataset[, target, drop = FALSE]))
  uniqueExpectedCombinations <- sortDataFrame(do.call(expand.grid, c(lapply(uniqueObservedCombinations, unique), stringsAsFactors = FALSE)))

  # if the rows match then it must be
  if (nrow(uniqueObservedCombinations) == nrow(uniqueExpectedCombinations))
    return(NULL)

  # find missing combinations
  observedStrings <- unname(apply(uniqueObservedCombinations, 1L, paste, collapse = jaspBase::interactionSymbol))
  expectedStrings <- unname(apply(uniqueExpectedCombinations, 1L, paste, collapse = jaspBase::interactionSymbol))
  missingStrings  <- setdiff(expectedStrings, observedStrings)
  missingStringsCollapsed <- paste(missingStrings[1:min(length(missingStrings), 10)], collapse = ", ")

  interactionVariable <- paste(colnames(uniqueObservedCombinations), collapse = jaspBase::interactionSymbol)

  if (length(missingStrings) <= 10) {
    return(gettextf(
      "The interaction effect of %1$s has no observations for the combination(s): %2$s. Please adjust the model and remove any interactions with missing cells",
      interactionVariable, missingStringsCollapsed
    ))
  } else {
    return(gettextf(
      "The interaction effect of %1$s has no observations for the combination(s): %2$s. Only the first 10 missing combinations are shown. Please adjust the model and remove any interactions with missing cells",
      interactionVariable, missingStringsCollapsed
    ))
  }

}

# model comparison ----
.BANOVAestimateModels <- function(jaspResults, dataset, options, errors,
                                  analysisType = c("ANOVA", "ANCOVA", "RM-ANOVA")) {

  # also makes the model comparison table
  stateObj <- jaspResults[["tableModelComparisonState"]]$object
  if (!is.null(jaspResults[["tableModelComparison"]]) && !is.null(stateObj)) {
    stateObj$completelyReused <- TRUE # means that posteriors won't need to be resampled
    return(stateObj)
  } else if (errors$noVariables) {
    modelTable <- .BANOVAinitModelComparisonTable(options)
    jaspResults[["tableModelComparison"]] <- modelTable
    return(list(analysisType = analysisType))
  } else if (!is.null(stateObj) && .BANOVAmodelBFTypeOrOrderChanged(stateObj, options)) {

    # if the statement above is TRUE then no new variables were added (or seed changed)
    # and the only change is in the Bayes factor type or the ordering
    modelTable <- .BANOVAinitModelComparisonTable(options)
    modelTable[["Models"]] <- .BANOVAgetModelTitlesWithAllTerms(stateObj[["models"]], stateObj[["model.list"]], analysisType, options[["hideNuisanceEffects"]])

    if (.BANOVAmodelPriorOptionsChanged(stateObj, options)) {

      priorProbs <- .BANOVAcomputePriorModelProbs(stateObj$model.list, stateObj$nuisance, options)
      internalTable <- stateObj$internalTableObj$internalTable
      internalTable[, "P(M)"] <- priorProbs
      stateObj$completelyReused <- FALSE # model-averaged posteriors need to be resampled

    } else {

      internalTable <- stateObj$internalTableObj$internalTable
      stateObj$completelyReused <- TRUE # model-averaged posteriors do not need to be resampled

    }

    internalTableObj <- .BANOVAfinalizeInternalTable(options, internalTable)
    stateObj$internalTableObj <- internalTableObj
    modelTable$setData(internalTableObj$table)
    jaspResults[["tableModelComparison"]] <- modelTable

    return(stateObj)

  }

  modelTerms    <- options$modelTerms
  dependent     <- options$dependent
  if (analysisType == "RM-ANOVA") {
    modelTerms[[length(modelTerms) + 1L]] <- list(components = .BANOVAsubjectName, isNuisance = TRUE)
    dependent <- .BANOVAdependentName
  }
  fixedFactors  <- options$fixedFactors
  randomFactors <- .BANOVAgetRandomFactors(options, analysisType)

  tempRScale    <- .BANOVAgetRScale(options, analysisType)
  rscaleFixed   <- tempRScale[["rscaleFixed"]]
  rscaleRandom  <- tempRScale[["rscaleRandom"]]
  rscaleCont    <- tempRScale[["rscaleCont"]]
  rscaleEffects <- tempRScale[["rscaleEffects"]]

  iter <- NA

  tmp <- .BANOVAcreateModelFormula(dependent, modelTerms)
  model.formula <- tmp$model.formula
  nuisance      <- tmp$nuisance
  effects       <- tmp$effects

  if (analysisType == "RM-ANOVA") {
    legacy    <- options[["legacy"]]
    rmFactors <- vapply(options[["repeatedMeasuresFactors"]], `[[`, character(1L), "name")
  } else {
    legacy    <- FALSE
    rmFactors <- NULL
  }

  #Make a list of models to compare
  temp <- .BANOVAgenerateAllModelFormulas(
    formula                                   = model.formula,
    nuisance                                  = nuisance,
    analysisType                              = analysisType,
    enforcePrincipleOfMarginalityFixedEffects = options[["enforcePrincipleOfMarginalityFixedEffects"]],
    enforcePrincipleOfMarginalityRandomSlopes = options[["enforcePrincipleOfMarginalityRandomSlopes"]],
    rmFactors                                 = rmFactors,
    legacy                                    = legacy
  )
  model.list           <- temp$modelList
  nuisance             <- temp$nuisance
  nuisanceRandomSlopes <- temp$nuisanceRandomSlopes

  if (length(model.list) == 1L) {
    modelTable <- .BANOVAinitModelComparisonTable(options)
    modelTable$setError(gettext("Bayes factor is undefined -- all effects are specified as nuisance."))
    jaspResults[["tableModelComparison"]] <- modelTable
    return(list(analysisType = analysisType))
  }

  # TODO: discuss if we want a hard limit if length(model.list) > ...
  # that would avoid peoples computers from slowing down tremendously.
  # JCG: A hard limit would be a bit cruel right? How about a warning instead? (in QML already?)

  #Run all other models and store the results in the model.object
  neffects <- length(effects)
  nmodels <- length(model.list)
  modelObject <- vector("list", nmodels)
  reorderedEffects  <- .BANOVAreorderTerms(effects)
  reorderedNuisance <- if (is.null(nuisance)) NULL else .BANOVAreorderTerms(nuisance)
  if (nmodels > 0L && neffects > 0L) {

    # effects.matrix contains for each row (model), which predictors (columns) are in the model
    # interactions.matrix contains for each predictor (column) what predictors (rows) is it composed of

    interactions.matrix <- .BANOVAcomputeInteractionsMatrix(effects)

    effects.matrix <- matrix(data = FALSE, nrow = nmodels, ncol = neffects)
    colnames(effects.matrix) <- effects

    for (m in seq_len(nmodels)) {
      modelObject[[m]] <- list()
      if (m == 1L) {
        if (is.null(nuisance)) { # intercept only
          modelObject[[m]]$title <- gettext("Null model")
          next # all effects are FALSE anyway
        } else {
          if (analysisType == "RM-ANOVA" && !legacy) {
            tempNuisance <- setdiff(nuisance, nuisanceRandomSlopes)
            modelObject[[m]]$title <- sprintf(ngettext(
              length(tempNuisance),
              "Null model (incl. %s and random slopes)",
              "Null model (incl. %s, and random slopes)"
            ), paste(.BANOVAdecodeNuisance(tempNuisance), collapse = ", "))
          } else {
            modelObject[[m]]$title <- gettextf("Null model (incl. %s)", paste(.BANOVAdecodeNuisance(nuisance), collapse = ", "))
          }
        }
      }
      model.effects <- .BANOVAgetFormulaComponents(model.list[[m]])

      reorderedModelEffects <- .BANOVAreorderTerms(model.effects)
      idx <- match(reorderedModelEffects, reorderedEffects, nomatch = 0L)
      idx <- idx[!is.na(idx)]
      effects.matrix[m, idx] <- TRUE

      if (m > 1L) {
        model.title <- model.effects[match(reorderedModelEffects, reorderedNuisance, 0L) == 0L]
        modelObject[[m]]$title <- jaspBase::gsubInteractionSymbol(paste(model.title, collapse = " + "))
      }
    }

    rownames(effects.matrix) <- sapply(modelObject, `[[`, "title")
  }

  modelTable <- .BANOVAinitModelComparisonTable(options)
  modelNames <- .BANOVAgetModelTitlesWithAllTerms(modelObject, model.list, analysisType, options[["hideNuisanceEffects"]])

  modelTable[["Models"]] <- modelNames
  jaspResults[["tableModelComparison"]] <- modelTable
  # internalTable is an internal representation of the model comparison table
  internalTable <- matrix(NA, nmodels, 5L, dimnames = list(modelNames, c("P(M)", "P(M|data)", "BFM", "BF10", "error %")))
  # set BF null model and p(M)
  internalTable[1L, 4L] <- 0
  internalTable[, 1L] <- .BANOVAcomputePriorModelProbs(model.list, nuisance, options)

  #Now compute Bayes Factors for each model in the list, and populate the tables accordingly
  startProgressbar(nmodels, gettext("Computing Bayes factors"))

  # check if any models can be resued from the state
  if (!is.null(stateObj[["modelTerms"]])) {

    oldFormulas <- sapply(stateObj$model.list, .BANOVAreorderFormulas)
    newFormulas <- sapply(model.list, .BANOVAreorderFormulas)

    reuseable   <- match(newFormulas, oldFormulas)

  } else {
    reuseable <- rep(NA, nmodels)
  }

  if (analysisType == "RM-ANOVA") {
    # the default null-model contains subject
    anyNuisance <- TRUE
  } else {
    # the default null-model is intercept only
    anyNuisance <- length(nuisance) > 0L
  }

  # without these there is no error
  bfIterations <- if (options[["sampleModeNumAcc"]] == "auto") 1e4L else options[["fixedNumAcc"]]

  for (m in seq_len(nmodels)) {
    # loop over all models, where the first is the null-model and the last the most complex model
    if (is.na(reuseable[m])) {
      if (!is.null(model.list[[m]])) {
        .setSeedJASP(options)
        bf <- try(BayesFactor::lmBF(
          formula       = model.list[[m]],
          data          = dataset,
          whichRandom   = randomFactors,
          progress      = FALSE,
          posterior     = FALSE,
          rscaleFixed   = rscaleFixed,
          rscaleRandom  = rscaleRandom,
          rscaleCont    = rscaleCont,
          rscaleEffects = rscaleEffects,
          iterations    = bfIterations))
        if (isTryError(bf)) {
          modelName <- strsplit(.BANOVAas.character.formula(model.list[[m]]), "~ ")[[1L]][2L]
          .quitAnalysis(gettextf("Bayes factor could not be computed for model: %1$s.\nThe error message was: %2$s.",
                                .BANOVAdecodeSubject(modelName), .extractErrorMessage(bf)))
        } else {
          # delete the data object -- otherwise it gets saved in the state
          bf@data <- data.frame()
        }
      } else {
        bf <- NULL
      }
    } else {
      bf <- stateObj$models[[reuseable[m]]]$bf
    }
    modelObject[[m]]$bf <- bf

    if (!is.null(bf)) {
      # if (isTryError(bf)) {
      #   message <- .extractErrorMessage(bf)
      #   modelObject[[m]]$error.message <- paste("Bayes factor computation failed with error:", message)
      # } else {
        # bfObj can have a modified denominator, but the saved objects are always compared against intercept only
      bfObj <- modelObject[[m]]$bf
      if (anyNuisance)
        bfObj <- bfObj / modelObject[[1L]]$bf

      internalTable[m, "BF10"]    <- bfObj@bayesFactor[, "bf"] # always LogBF10!
      internalTable[m, "error %"] <- bfObj@bayesFactor[, "error"]
      # }

      # disable filling of Bayes factor column (see https://github.com/jasp-stats/jasp-test-release/issues/1018 for discussion)
      # modelTable[["BF10"]]    <- .recodeBFtype(internalTable[, "BF10"],
      #                                          newBFtype = options$bayesFactorType,
      #                                          oldBFtype = "LogBF10")
      modelTable[["error %"]] <- internalTable[, "error %"]
    }
    progressbarTick()
  }

  internalTableObj <- .BANOVAfinalizeInternalTable(options, internalTable)
  modelTable$setData(internalTableObj$table)
  if (length(internalTableObj[["footnotes"]]) > 0L) {
    idxRow <- internalTableObj[["footnotes"]][["rows"]]
    idxCol <- internalTableObj[["footnotes"]][["cols"]]
    tb <- internalTableObj$table
    modelTable$addFootnote(message  = internalTableObj[["footnotes"]][["message"]],
                           symbol   = " <sup>&#10013;</sup>", # latin cross
                           rowNames = rownames(tb)[idxRow],
                           colNames = colnames(tb)[idxCol])
  }

  if (anyNuisance) {
    if (analysisType == "RM-ANOVA" && !options[["legacy"]]) {
      message <- gettextf("All models include %s, and random slopes for all repeated measures factors.",
               paste0(.BANOVAdecodeNuisance(setdiff(nuisance, nuisanceRandomSlopes)), collapse = ", "))

    } else {

      if (analysisType == "RM-ANOVA")
        modelTable$addFootnote(message = gettext("Random slopes for repeated measures factors are excluded."), symbol = .BANOVAGetWarningSymbol())

      message <- gettextf("All models include %s.", paste0(.BANOVAdecodeNuisance(nuisance), collapse = ", "))
    }
    modelTable$addFootnote(message = message)

  }

  model <- list(
    models               = modelObject,
    postProbs            = internalTableObj$internalTable[, "P(M|data)"],
    priorProbs           = internalTableObj$internalTable[, "P(M)"],
    internalTableObj     = internalTableObj,
    effects              = effects.matrix,
    interactions.matrix  = interactions.matrix,
    nuisance             = nuisance,
    nuisanceRandomSlopes = nuisanceRandomSlopes,
    analysisType         = analysisType,
    # these are necessary for partial reusage of the state (e.g., when a fixedFactor is added/ removed)
    model.list           = model.list,
    fixedFactors         = fixedFactors,
    randomFactors        = randomFactors, # stored because they are modified in RM-ANOVA
    modelTerms           = modelTerms,
    covariates           = options[["covariates"]],
    seed                 = options[["seed"]],
    setSeed              = options[["setSeed"]],
    reuseable            = reuseable,
    RMFactors            = options[["repeatedMeasuresFactors"]],
    modelPriorOptions    = options[.BANOVAmodelSpaceDependencies(options[["modelPrior"]])],
    hideNuisanceEffects  = options[["hideNuisanceEffects"]]
  )

  # save state
  stateObj <- createJaspState(object = model, dependencies = c(
    # does NOT depend on any factors or covariates, to facilitate reusing previous models
    "dependent", "repeatedMeasuresCells", "sampleModeNumAcc", "fixedNumAcc", "seed", "setSeed",
    .BANOVArscaleDependencies(options[["coefficientsPrior"]])
  ))

  jaspResults[["tableModelComparisonState"]] <- stateObj

  return(model)
}

.BANOVAeffectsTable <- function(jaspResults, options, model) {

  if (!is.null(jaspResults[["tableEffects"]]) || !options[["effects"]])
    return()

  # isTRUE should handle a state issue, see https://github.com/jasp-stats/jasp-test-release/issues/839
  if (isTRUE(model[["analysisType"]] != "RM-ANOVA" && options[["dependent"]] != "")) {
    title <- gettextf("Analysis of Effects - %s", options[["dependent"]])
  } else {
    title <- gettext("Analysis of Effects")
  }

  effectsTable <- createJaspTable(title = title)
  effectsTable$position <- 2
  effectsTable$dependOn(c(
    .BANOVAdataDependencies(),
    "effects", "effectsType",
    "sampleModeNumAcc", "fixedNumAcc", "bayesFactorType",
    .BANOVAmodelSpaceDependencies(options[["modelPrior"]]),
    .BANOVArscaleDependencies(options[["coefficientsPrior"]])
  ))

  effectsTable$addCitation(.BANOVAcitations[1:2])

  inclusion.title <- switch(
    options$bayesFactorType,
    "LogBF10" = gettext("Log(BF<sub>incl</sub>)"),
    "BF01"    = gettext("BF<sub>excl</sub>"),
    "BF10"    = gettext("BF<sub>incl</sub>")
  )

  effectsTable$addColumnInfo(name = "Effects",      title = gettext("Effects"),      type = "string")
  effectsTable$addColumnInfo(name = "P(incl)",      title = gettext("P(incl)"),      type = "number")
  effectsTable$addColumnInfo(name = "P(excl)",      title = gettext("P(excl)"),      type = "number")
  effectsTable$addColumnInfo(name = "P(incl|data)", title = gettext("P(incl|data)"), type = "number")
  effectsTable$addColumnInfo(name = "P(excl|data)", title = gettext("P(excl|data)"), type = "number")
  effectsTable$addColumnInfo(name = "BFInclusion",  title = inclusion.title,         type = "number")

  if (options$effectsType == "matchedModels") {
    effectsTable$addFootnote(gettextf("Compares models that contain the effect to equivalent models stripped of the effect. Higher-order interactions are excluded. Analysis suggested by Sebastiaan Math%st.", "\u00F4"))
  }

  if (is.null(model$models)) {
    jaspResults[["tableEffects"]] <- effectsTable
    return()
  }

  effects.matrix <- model$effects
  no.effects <- ncol(effects.matrix)
  effectNames <- colnames(model$effects)

  idxNotNan <- c(TRUE, !is.nan(model$postProbs[-1L]))
  if (options$effectsType == "allModels") {

    if (any(!idxNotNan)) {
      # renormalize the prior and posterior probabilities
      priorInclProb <- crossprod(effects.matrix[idxNotNan, , drop = FALSE], model[["priorProbs"]][idxNotNan] / sum(model[["priorProbs"]][idxNotNan]))
      postInclProb  <- crossprod(effects.matrix[idxNotNan, , drop = FALSE], model[["postProbs"]][idxNotNan]  / sum(model[["postProbs"]][idxNotNan]))
    } else {
      # do not renormalize
      priorInclProb <- crossprod(effects.matrix, model[["priorProbs"]])
      postInclProb  <- crossprod(effects.matrix, model[["postProbs"]])
    }

    # deal with numerical error
    postInclProb[postInclProb > 1] <- 1
    postInclProb[postInclProb < 0] <- 0
    bfIncl <- (postInclProb / (1 - postInclProb)) / (priorInclProb / (1 - priorInclProb))

    priorExclProb <- 1 - priorInclProb
    postExclProb  <- 1 - postInclProb

  } else {

    tmp <- BANOVAcomputMatchedInclusion(
      effectNames, effects.matrix, model$interactions.matrix,
      model$priorProbs, model$postProbs
    )
    priorInclProb <- tmp[["priorInclProb"]]
    postInclProb  <- tmp[["postInclProb"]]
    bfIncl        <- tmp[["bfIncl"]]
    priorExclProb <- tmp[["priorExclProb"]]
    postExclProb  <- tmp[["postExclProb"]]

    # show BFinclusion for nuisance predictors as 1, rather than NaN
    priorInclIs1 <- is.nan(bfIncl) & abs(1 - priorInclProb) <= sqrt(.Machine$double.eps)
    bfIncl[priorInclIs1] <- 1

  }

  if (sum(!idxNotNan) > 1L) { # null model is always omitted, so 2 or more omitted indicates some models failed
    effectsTable$addFootnote(message = gettext("Some Bayes factors could not be calculated. Inclusion probabilities and Bayes factors are calculated while excluding these models. The results may be uninterpretable!"),
      symbol = .BANOVAGetWarningSymbol()
    )
  }

  effectsTable[["Effects"]]      <- jaspBase::gsubInteractionSymbol(effectNames)
  effectsTable[["P(incl)"]]      <- priorInclProb
  effectsTable[["P(incl|data)"]] <- postInclProb
  effectsTable[["BFInclusion"]] <- switch(
    options$bayesFactorType,
    "LogBF10" = log(bfIncl),
    "BF01"    = 1 / bfIncl,
    "BF10"    = bfIncl
  )

  effectsTable[["P(excl)"]]      <- priorExclProb
  effectsTable[["P(excl|data)"]] <- postExclProb

  jaspResults[["tableEffects"]] <- effectsTable
  return()
}

BANOVAcomputMatchedInclusion <- function(effectNames, effects.matrix, interactions.matrix,
                                          priorProbs, postProbs) {
  # this method is inspired by this post: https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp

  priorInclProb <- postInclProb <- bfIncl <- priorExclProb <- postExclProb <-
    numeric(length(effectNames))

  for (i in seq_along(effectNames)) {
    effect <- effectNames[i]

    # get all higher order interactions of which this effect is a component
    # e.g., V1 is a component of V1:V2
    idx1 <- interactions.matrix[effect, ]

    # get all models that exclude the predictor, but that always include the lower order main effects
    # e.g., V1:V2 is compared against models that always include V1 and V2
    idx2 <- interactions.matrix[, effect] # if effect is V1:V2, idx2 contains c(V1, V2)

    # idx3 is FALSE if a model contains higher order interactions of effect, TRUE otherwise
    idx3 <- !matrixStats::rowAnys(effects.matrix[, idx1, drop = FALSE])

    # all models that include the effect, without higher order interactions
    idx4 <- idx3 & effects.matrix[, i]
    priorInclProb[i] <- sum(idx4 * priorProbs)
    postInclProb[i]  <- sum(idx4 * postProbs)

    # the models to consider for the prior/ posterior exclusion probability.
    # idx5 includes models that have: all subcomponents & no higher order interaction & not the effect
    idx5 <- matrixStats::rowAlls(effects.matrix[, idx2, drop = FALSE]) & idx3 & !effects.matrix[, i]

    priorExclProb[i] <- sum(idx5 * priorProbs)
    postExclProb[i]  <- sum(idx5 * postProbs)

    # compute inclusion BF
    bfIncl[i]     <- (postInclProb[i] / postExclProb[i]) / (priorInclProb[i] / priorExclProb[i])
  }
  return(list(
    priorInclProb = priorInclProb,
    postInclProb  = postInclProb,
    priorExclProb = priorExclProb,
    postExclProb  = postExclProb,
    bfIncl        = bfIncl
  ))
}

.BANOVAinitModelComparisonTable <- function(options) {

  # function that creates an empty JASP table to be filled later
  modelTable <- createJaspTable(title = gettext("Model Comparison"))
  modelTable$position <- 1L
  modelTable$addCitation(.BANOVAcitations[1:2])
  modelTable$dependOn(c(
    .BANOVAdataDependencies(),
    "sampleModeNumAcc", "fixedNumAcc",
    "bayesFactorType", "bayesFactorOrder",
    "hideNuisanceEffects", "legacy",
    .BANOVAmodelSpaceDependencies(options[["modelPrior"]]),
    .BANOVArscaleDependencies(options[["coefficientsPrior"]])
  ))

  switch(options$bayesFactorType,
    BF10 = {
      bfm.title <- gettext("BF<sub>M</sub>")
      bf.title  <- gettext("BF<sub>10</sub>")
    },
    BF01 = {
      bfm.title <- gettext("BF<sub>M</sub>")
      bf.title  <- gettext("BF<sub>01</sub>")
    },
    LogBF10 = {
      bfm.title <- gettext("Log(BF<sub>M</sub>)")
      bf.title  <- gettext("Log(BF<sub>10</sub>)")
    }
  )

  modelTable$addColumnInfo(name = "Models",    title = gettext("Models"),    type = "string")
  modelTable$addColumnInfo(name = "P(M)",      title = gettext("P(M)"),      type = "number")
  modelTable$addColumnInfo(name = "P(M|data)", title = gettext("P(M|data)"), type = "number")
  modelTable$addColumnInfo(name = "BFM",       title = bfm.title,            type = "number")
  modelTable$addColumnInfo(name = "BF10",      title = bf.title,             type = "number")
  modelTable$addColumnInfo(name = "error %",   title = gettext("error %"),   type = "number")

  return(modelTable)
}

.BANOVAfinalizeInternalTable <- function(options, internalTable) {

  # function that actually fills in the table created by .BANOVAinitModelComparisonTable
  footnotes <- NULL
  if (anyNA(internalTable[, "P(M|data)"])) {
    # if TRUE, called from analysis

    logSumExp <- matrixStats::logSumExp
    logbfs <- internalTable[, "BF10"]
    if (!anyNA(internalTable[, "BF10"])) {
      # no errors, proceed normally and complete the table

      logsumbfs <- logSumExp(logbfs)
      internalTable[, "P(M|data)"] <-  exp(logbfs - logsumbfs)

      nmodels <- nrow(internalTable)
      mm <- max(logbfs)
      for (i in seq_len(nmodels)) {
        internalTable[i, "BFM"] <- logbfs[i] - logSumExp(logbfs[-i]) + log(nmodels - 1L)
      }

    } else {
      # create table excluding failed models

      idxGood <- !is.na(logbfs)
      widxGood <- which(idxGood)
      logsumbfs <- logSumExp(logbfs[idxGood])
      internalTable[, "P(M|data)"] <-  exp(logbfs - logsumbfs)

      nmodels <- sum(idxGood)
      mm <- max(logbfs, na.rm = TRUE)
      widxBad <- which(!idxGood)
      for (i in widxGood) {
        internalTable[i, "BFM"] <- logbfs[i] - logSumExp(logbfs[-c(i, widxBad)]) + log(nmodels - 1L)
      }

      internalTable[widxBad, "P(M|data)"] <- NaN
      internalTable[widxBad, "BFM"]       <- NaN
      internalTable[widxBad, "BF10"]      <- NaN
      footnotes <- list(
        message = gettext("<b><em>Warning.</em></b> Some Bayes factors could not be calculated. Multi model inference is carried out while excluding these models. The results may be uninterpretable!")
      )
    }
  } # else: results already computed

  # create the output table
  table <- as.data.frame(internalTable)
  table[["Models"]] <- rownames(internalTable)
  if (options[["bayesFactorType"]] == "LogBF10") {
    table[["BFM"]]  <- internalTable[, "BFM"]
  } else {
    table[["BFM"]]  <- exp(internalTable[, "BFM"])
  }

  o <- order(table[["BF10"]], decreasing = TRUE)
  table <- table[o, ]
  idxNull <- which(o == 1L)
  if (options[["bayesFactorOrder"]] == "nullModelTop") {
    table[idxNull, "error %"] <- NA
    table <- table[c(idxNull, seq_len(nrow(table))[-idxNull]), ]
  } else {

    table[["BF10"]] <- table[["BF10"]] - table[1L, "BF10"]

    # recompute error (see BayesFactor:::`.__T__/:base`$`BFBayesFactor#BFBayesFactor`)
    table[idxNull, "error %"] <- 0
    table[["error %"]] <- sqrt(table[["error %"]]^2 + table[["error %"]][1L]^2)
    table[1L, "error %"] <- NA
  }

  table[["BF10"]] <- .recodeBFtype(table[["BF10"]], newBFtype = options[["bayesFactorType"]], oldBFtype = "LogBF10")
  table[["error %"]] <- 100 * table[["error %"]]

  if (!is.null(footnotes)) {
    idxNan <- which(is.nan(as.matrix(table[-ncol(table)])), arr.ind = TRUE)
    footnotes[["rows"]] <- idxNan[, "row"]
    footnotes[["cols"]] <- idxNan[, "col"]
  }

  return(list(table = table, internalTable = internalTable, footnotes = footnotes))

}

# posterior inference ----
.BANOVAestimatePosteriors <- function(jaspResults, dataset, options, model) {

  userNeedsPosteriorSamples <- options$posteriorEstimates || options$posteriorPlot || options$qqPlot ||
    options$rsqPlot || options$criTable
  if (is.null(model$models) || !userNeedsPosteriorSamples)
    return()

  # model[["completelyReused"]] is needed because it can happen that some posterior samples can be reused (e.g.,
  # when the modelTerms change)
  posteriors <- jaspResults[["statePosteriors"]]$object
  if (!is.null(posteriors) && isTRUE(model[["completelyReused"]])) { # can all posterior be reused?

    posteriorsCRI <- jaspResults[["statePosteriorsCRI"]]$object
    if (is.null(posteriorsCRI)) {# do we need to recompute credible intervals?
      posteriorsCRI <- .BANOVAcomputePosteriorCRI(dataset, options, model, posteriors)
      statePosteriorsCRI <- createJaspState(object = posteriorsCRI)
      statePosteriorsCRI$dependOn(options = "credibleInterval",
                                  optionsFromObject = jaspResults[["tableModelComparisonState"]])
      jaspResults[["statePosteriorCRI"]] <- statePosteriorsCRI
    }

  } else { # compute posteriors and credible intervals
    posteriors    <- .BANOVAsamplePosteriors(jaspResults, dataset, options, model, posteriors)
    posteriorsCRI <- .BANOVAcomputePosteriorCRI(dataset, options, model, posteriors)

    statePosteriors <- createJaspState(object = posteriors)
    statePosteriors$dependOn(
      c(.BANOVAmodelSpaceDependencies(options[["modelPrior"]]), "sampleModeMCMC", "fixedMCMCSamples"),
      optionsFromObject = jaspResults[["tableModelComparisonState"]]
    )
    jaspResults[["statePosteriors"]] <- statePosteriors

    statePosteriorsCRI <- createJaspState(object = posteriorsCRI)
    statePosteriorsCRI$dependOn(options = "credibleInterval",
                                optionsFromObject = jaspResults[["statePosteriors"]])
    jaspResults[["statePosteriorCRI"]] <- statePosteriorsCRI

  }

  .BANOVASamplingIssuesTable(jaspResults, posteriors[["samplingIssues"]])

  return(c(posteriors, posteriorsCRI))
}

.BANOVAestimatesTable <- function(jaspResults, options, model) {

  if (!is.null(jaspResults[["tablePosteriorEstimates"]]) || !options[["posteriorEstimates"]])
    return()

  estsTable <- createJaspTable(title = gettext("Model Averaged Posterior Summary"))
  estsTable$position <- 3
  estsTable$dependOn(c(
    .BANOVAdataDependencies(),
    "posteriorEstimates",
    "sampleModeMCMC", "fixedMCMCSamples", "credibleInterval",
    .BANOVAmodelSpaceDependencies(options[["modelPrior"]]),
    .BANOVArscaleDependencies(options[["coefficientsPrior"]])
  ))

  overTitle <- gettextf("%s%% Credible Interval", format(100 * options[["credibleInterval"]], digits = 3))
  estsTable$addColumnInfo(name = "Variable", type = "string")
  estsTable$addColumnInfo(name = "Level",    type = "string")
  estsTable$addColumnInfo(name = "Mean",     type = "number")
  estsTable$addColumnInfo(name = "SD",       type = "number")
  estsTable$addColumnInfo(name = "Lower",    type = "number", overtitle = overTitle)
  estsTable$addColumnInfo(name = "Upper",    type = "number", overtitle = overTitle)

  if (!is.null(model[["posteriors"]])) {
    .BANOVAfillEstimatesTable(
      jaspTable   = estsTable,
      mus         = model$posteriors$weightedMeans,
      sds         = model$posteriors$weightedSds,
      cri         = model$posteriors$weightedCRIs,
      hasNoLevels = model$posteriors$allContinuous,
      isRandom    = model$posteriors$isRandom
    )
  }
  jaspResults[["tablePosteriorEstimates"]] <- estsTable
  return()
}

.BANOVAfillEstimatesTable <- function(jaspTable, mus, sds, cri, hasNoLevels, isRandom = NULL) {

  if (!is.null(isRandom) && any(isRandom)) {
    # remove random effects
    mus <- mus[!isRandom]
    sds <- sds[!isRandom]
    cri <- cri[!isRandom, ]
  }

  table <- .BANOVAgetLevelsFromParamNames(names(mus))
  colnames(table) <- c("Variable", "Level")

  # set repeated parameter names to ""
  idxDup <- duplicated(table[, "Variable"])
  table[idxDup, "Variable"]  <- ""
  # decode base64 variables
  table[!idxDup, "Variable"] <- jaspBase::gsubInteractionSymbol(table[!idxDup, "Variable"])
  # rename mu to Intercept
  table[1L, "Variable"] <- "Intercept"
  # attach posterior means and sds
  table <- cbind(as.data.frame(table),
                 "Mean"  = mus,
                 "SD"    = sds,
                 "Lower" = cri[, 1L],
                 "Upper" = cri[, 2L])
  if (hasNoLevels)
    table <- table[, -2L]
  jaspTable$setData(table)
  return()
}

.BANOVArsquaredTable <- function(jaspResults, options, model) {

  if (!is.null(jaspResults[["tableBMACRI"]]) || !options[["criTable"]])
    return()

  criTable <- createJaspTable(title = gettextf("Model Averaged R%s", "\u00B2"))
  criTable$position <- 3.5
  criTable$dependOn(c(
    .BANOVAdataDependencies(),
    "criTable",
    "sampleModeMCMC", "fixedMCMCSamples", "bayesFactorType", "credibleInterval",
    .BANOVAmodelSpaceDependencies(options[["modelPrior"]]),
    .BANOVArscaleDependencies(options[["coefficientsPrior"]])
  ))

  overTitle <- gettextf("%s%% Credible Interval", format(100 * options[["credibleInterval"]], digits = 3))
  criTable$addColumnInfo(name = "rsq",   type = "string", title = "")
  criTable$addColumnInfo(name = "Mean",  type = "number")
  criTable$addColumnInfo(name = "Lower", type = "number", overtitle = overTitle)
  criTable$addColumnInfo(name = "Upper", type = "number", overtitle = overTitle)

  if (!is.null(model[["posteriors"]])) {
    cri <- model[["posteriors"]][["weightedRsqCri"]]
    df <- data.frame(
      rsq   = "R\u00B2",
      Mean  = model[["posteriors"]][["weightedRsqMean"]],
      Lower = cri[1L],
      Upper = cri[2L]
    )
    criTable$setData(df)
  } else {
    criTable[["rsq"]] <- "R\u00B2"
  }
  jaspResults[["tableBMACRI"]] <- criTable
  return()

}

.BANOVASamplingIssuesTable <- function(jaspResults, samplingIssues) {

  if (is.null(samplingIssues) || length(samplingIssues) == 0L || !is.null(jaspResults[["tableSamplingIssues"]]))
    return()

  issuesTable <- createJaspTable(title = gettext("<b><em>Warning: sampling issues encountered!</em></b>"),
                           # below model and effects table (which may be unaffected) but above estimates table
                           position = 2.5)

  issuesTable$addColumnInfo(name = "Models",           title = gettext("Affected model"),      type = "string")
  issuesTable$addColumnInfo(name = "percentageSucces", title = gettext("%% useful samples"),    type = "number")
  issuesTable$addColumnInfo(name = "noSuccess",        title = gettext("Useful samples"),      type = "integer")
  issuesTable$addColumnInfo(name = "total",            title = gettext("Total samples drawn"), type = "integer")

  df <- data.frame(
    Models    = vapply(samplingIssues, `[[`, FUN.VALUE = character(1L), "model"),
    noSuccess = vapply(samplingIssues, `[[`, FUN.VALUE = integer(1L),   "remainingRows"),
    total     = vapply(samplingIssues, `[[`, FUN.VALUE = integer(1L),   "originalRows")
  )
  df[["percentageSucces"]] <- df[["noSuccess"]] / df[["total"]]

  if (any(df[["percentageSucces"]] < 0.25) || any(df[["remainingRows"]] < 1000L))
    issuesTable$addFootnote(
      gettext("For some affected models, more than 75%% of the posterior samples failed, or fewer than 1000 samples remained for subsequent results. All model-averaged output may be biased and uninterpretable. Check the model specification and data for any odd patterns."),
      symbol = .BANOVAGetWarningSymbol()
    )

  issuesTable$setData(df)
  issuesTable$dependOn(
    # these options correspond to userNeedsPosteriorSamples inside .BANOVAestimatePosteriors
    options           = c("posteriorEstimates", "posteriorPlot", "qqPlot", "rsqPlot", "criTable", "modelTerms"),
    optionsFromObject = jaspResults[["statePosteriors"]]
  )
  jaspResults[["tableSamplingIssues"]] <- issuesTable

}

.BANOVAGetWarningSymbol <- function() {
  gettext("<b><em>Warning.</em></b>")
}

# Plots wrappers ----
.BANOVAposteriorPlot <- function(jaspResults, dataset, options, model) {

  # meta wrapper for model averaged posterior plots
  if (!is.null(jaspResults[["posteriorPlot"]]) || !options$posteriorPlot)
    return()

  posteriorPlotContainer <- createJaspContainer(title = gettext("Model Averaged Posterior Distributions"))
  jaspResults[["posteriorPlot"]] <- posteriorPlotContainer
  posteriorPlotContainer$dependOn(c("posteriorPlot", "modelTerms", "credibleInterval", "groupPosterior",
                                    "repeatedMeasuresCells", "dependent"))
  posteriorPlotContainer$position <- 4

  if (is.null(model$models)) {
    posteriorPlotContainer[["dummyplot"]] <- createJaspPlot(title = gettext("Posterior distribution"), width = 400, height = 400,
                                                            plot = NULL)
  } else {
    posteriorPlotContainer$dependOn(optionsFromObject = jaspResults[["tableModelComparisonState"]])
    .BANOVAfillPosteriorPlotContainer(
      container       = posteriorPlotContainer,
      densities       = model$posterior$weightedDensities[, -1L, ], # omit intercept
      cris            = model$posterior$weightedCRIs[-1L, ],        # omit intercept
      isRandom        = model$posteriors$isRandom[-1L],             # omit intercept
      groupParameters = identical(options[["groupPosterior"]], "grouped")
    )
  }
  return()
}

.BANOVAfillPosteriorPlotContainer <- function(container, densities, cris, isRandom = NULL, groupParameters = FALSE) {

  allParamNames <- colnames(densities)

  tmp <- .BANOVAgetLevelsFromParamNames(allParamNames)
  plotTitles <- jaspBase::gsubInteractionSymbol(tmp[, "parameter"])
  xNames <- tmp[, "level"]

  if (is.null(isRandom)) {
    indices <- seq_along(allParamNames)
  } else {
    indices <- which(!isRandom)
  }

  if (groupParameters) {

    indices <- split(indices, plotTitles[indices])[order(unique(plotTitles[indices]))]
    nms <- names(indices)
    for (i in seq_along(indices)) {

      ind <- indices[[i]]

      if (all(c(densities[, ind, "y"]) == 0)) { # only TRUE is never sampled and set to 0
        plot <- createJaspPlot(title = nms[i], width = 400, height = 400)
        plot$setError(gettext("Could not plot posterior distribution. Check if an error occurred for models including this parameter."))
      } else {
        # make prior posterior plot
        dfLines <- data.frame(x = c(densities[, ind, "x"]),
                              y = c(densities[, ind, "y"]),
                              g = rep(xNames[ind], each = nrow(densities)))

        xBreaks <- jaspGraphs::getPrettyAxisBreaks(dfLines$x)
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, dfLines$y))
        breaksYmax <- yBreaks[length(yBreaks)]
        obsYmax <- max(dfLines$y)
        newymax <- max(1.25 * obsYmax, breaksYmax)

        lInd <- length(ind)
        if (lInd == 1L) {
          plusmin <- 0
          showLegend <- FALSE
        } else {
          # some jitter in CRI height
          showLegend <- TRUE
          plusmin <- rep(-1, lInd)
          plusmin[seq(2, lInd, 2)] <- 0
          if (lInd > 2)
            plusmin[seq(3, lInd, 3)] <- 1
        }

        dfCri <- data.frame(
          xmin = cris[ind, 1L],
          xmax = cris[ind, 2L],
          y = (newymax - obsYmax)/2 + obsYmax + plusmin * (newymax - obsYmax) / 10,
          g = xNames[ind]
        )

        if (showLegend) {
          # if two distributions are remarkably similar, i.e., have nearly identical credible intervals,
          # we add a linetype aestethic
          tol <- 1e-4
          distMin <- which(stats::dist(dfCri$xmin) < tol)
          distMax <- which(stats::dist(dfCri$xmin) < tol)
          distBoth <- intersect(distMin, distMax)
          if (length(distBoth) > 0) {
            aesLine <- ggplot2::aes(x = x, y = y, g = g, color = g, linetype = g)
            aesErrorbar <- ggplot2::aes(xmin = xmin, xmax = xmax, y = y, group = g, color = g, linetype = g)
          } else {
            aesLine <- ggplot2::aes(x = x, y = y, g = g, color = g)
            aesErrorbar <- ggplot2::aes(xmin = xmin, xmax = xmax, y = y, group = g, color = g)
          }
        } else {
          # don't use color for single density plots (covariates)
          aesLine <- ggplot2::aes(x = x, y = y, g = g)
          aesErrorbar <- ggplot2::aes(xmin = xmin, xmax = xmax, y = y, group = g)
        }


        maxheight <- min(newymax - dfCri$y[1:min(lInd, 3)])
        xlab <- nms[i]
        # ggplot doesn't like our fancy unicode * so we need to escape it
        xlab <- stringi::stri_escape_unicode(xlab)
        # change escaped version into x
        xlab <- gsub(pattern = "\\u2009\\u273b\\u2009", replacement = " x ", x = xlab, fixed = TRUE)

        ncolLegend <- ceiling(lInd / 14)
        guideLegend <- ggplot2::guide_legend(title = gettext("Level"), keywidth = 0.25, keyheight = 0.1, default.unit = "inch",
                                             ncol = ncolLegend)
        p <- ggplot2::ggplot(data = dfLines, mapping = aesLine) +
          ggplot2::geom_line(size = 1.1) +
          ggplot2::geom_errorbarh(data = dfCri, mapping = aesErrorbar, height = maxheight, size = 1.1,
                                  inherit.aes = FALSE) +
          ggplot2::scale_x_continuous(name = xlab,      breaks = xBreaks, limits = range(xBreaks)) +
          ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = c(0, newymax)) +
          colorspace::scale_color_discrete_qualitative() +
          ggplot2::scale_linetype() +
          ggplot2::guides(color = guideLegend, linetype = guideLegend)
        p <- jaspGraphs::themeJasp(p, legend.position = if (showLegend) "right" else "none")

        plot <- createJaspPlot(title = nms[i], width = 400, height = 400, plot = p)
      }
      container[[allParamNames[i]]] <- plot
    }
  } else {
    for (i in indices) {

      # make prior posterior plot
      df <- data.frame(x = densities[, i, "x"],
                       y = densities[, i, "y"])

      p <- jaspGraphs::PlotPriorAndPosterior(
        dfLines    = df,
        xName      = xNames[i],
        CRI        = cris[i, ],
        drawCRItxt = FALSE
      )

      plot <- createJaspPlot(title = plotTitles[i], width = 400, height = 400, plot = p)
      container[[allParamNames[i]]] <- plot
    }
  }
  return()

}

.BANOVAqqplot <- function(jaspResults, options, model) {

  if (!is.null(jaspResults[["QQplot"]]) || !options[["qqPlot"]])
    return()

  plot <- createJaspPlot(
    title       = gettext("Model Averaged Q-Q Plot"),
    width       = 400,
    height      = 400,
    aspectRatio = 1
  )

  if (!is.null(model[["models"]])) {
    plot$plotObject <- jaspGraphs::plotQQnorm(
      residuals = model[["posteriors"]][["weightedResidSumStats"]][,"mean"],
      lower     = model[["posteriors"]][["weightedResidSumStats"]][,"cri.2.5%"],
      upper     = model[["posteriors"]][["weightedResidSumStats"]][,"cri.97.5%"]
    )
    plot$dependOn(optionsFromObject = jaspResults[["tableModelComparisonState"]])
  }

  plot$dependOn(c("qqPlot", "modelTerms", "credibleInterval", "repeatedMeasuresCells", "dependent"))
  plot$position <- 5
  jaspResults[["QQplot"]] <- plot
  return()
}

.BANOVArsqplot <- function(jaspResults, options, model) {

  if (!is.null(jaspResults[["rsqplot"]]) || !options[["rsqPlot"]])
    return()

  plot <- createJaspPlot(
    title       = gettextf("Model Averaged Posterior R%s","\u00B2"),
    width       = 400,
    height      = 400,
    aspectRatio = 1
  )

  if (!is.null(model[["models"]])) {
    dd     <- model[["posteriors"]][["weightedRsqDens"]]
    rsqCri <- model[["posteriors"]][["weightedRsqCri"]]

    df <- data.frame(x = dd$x, y = dd$y)
    xName <- expression(R^2)
    plot$plotObject <- jaspGraphs::PlotPriorAndPosterior(dfLines = df, xName = xName, CRI = rsqCri, drawCRItxt = FALSE)
    plot$dependOn(optionsFromObject = jaspResults[["tableModelComparisonState"]])
  }

  plot$dependOn(c("rsqPlot", "modelTerms", "credibleInterval", "repeatedMeasuresCells", "dependent"))
  plot$position <- 6
  jaspResults[["rsqplot"]] <- plot
  return()
}

# Post hoc comparison ----
.BANOVAnullControlPostHocTable <- function(jaspResults, dataset, options, model) {

  if (length(options$postHocTestsVariables) == 0L)
    return()

  postHocCollection <- jaspResults[["collectionPosthoc"]]
  if (is.null(postHocCollection)) {
    postHocCollection <- createJaspContainer(title = gettext("Post Hoc Tests"))
    postHocCollection$position <- 8
    postHocCollection$addCitation(.BANOVAcitations[3:4])
    postHocCollection$dependOn(c("dependent", "repeatedMeasuresCells", "postHocTestsNullControl", "bayesFactorType"))
    jaspResults[["collectionPosthoc"]] <- postHocCollection
  }

  # the same footnote for all the tables
  footnote <- gsub("[\r\n\t]", "",
    gettext("The posterior odds have been corrected for multiple testing by fixing to 0.5 the prior probability that the null hypothesis holds across all comparisons (Westfall, Johnson, & Utts, 1997). Individual comparisons are based on the default t-test with a Cauchy (0, r = 1/sqrt(2)) prior. The \"U\" in the Bayes factor denotes that it is uncorrected."))

  bfTxt <- if (options[["postHocTestsNullControl"]]) ", U" else ""

  switch(options[["bayesFactorType"]],
    BF10    = { bf.title <- paste0("BF<sub>10",     bfTxt, "</sub>")  },
    BF01    = { bf.title <- paste0("BF<sub>01",     bfTxt, "</sub>")  },
    LogBF10 = { bf.title <- paste0("Log(BF<sub>10", bfTxt, "</sub>)") }
   )

  priorWidth <- 1 / sqrt(2)
  posthoc.variables <- unlist(options[["postHocTestsVariables"]])
  if (model[["analysisType"]] == "RM-ANOVA") {
    dependent <- .BANOVAdependentName
  } else {
    dependent <- options[["dependent"]]
  }

  for (posthoc.var in posthoc.variables) {

    # does the table already exist?
    if (!is.null(postHocCollection[[paste0("postHoc_", posthoc.var)]]))
      next

    postHocTable <- createJaspTable(title = paste0("Post Hoc Comparisons - ", posthoc.var))

    postHocTable$addColumnInfo(name = "(I)",            type = "string", title = "", combine=TRUE)
    postHocTable$addColumnInfo(name = "(J)",            type = "string", title = "")
    postHocTable$addColumnInfo(name = "Prior Odds",     type = "number", title = gettext("Prior Odds"))
    postHocTable$addColumnInfo(name = "Posterior Odds", type = "number", title = gettext("Posterior Odds"))
    postHocTable$addColumnInfo(name = "BF",             type = "number", title = bf.title)
    postHocTable$addColumnInfo(name = "error %",        type = "number", title = gettext("error %"))

    postHocTable$dependOn(optionContainsValue = list("postHocTestsVariables" = posthoc.var))

    if (options[["postHocTestsNullControl"]])
      postHocTable$addFootnote(footnote)

    if (is.null(model$models)) { # only show empty table
      postHocCollection[[paste0("postHoc_", posthoc.var)]] <- postHocTable
      next
    }

    fixed <- unlist(c(options$fixedFactors, sapply(options$repeatedMeasuresFactors, `[[`, "name")))
    if (model$analysisType == "RM-ANOVA" && posthoc.var %in% fixed && !posthoc.var %in% options$betweenSubjectFactors) {
      variable.levels <- options$repeatedMeasuresFactors[[which(lapply(options$repeatedMeasuresFactors, function(x) x$name) == posthoc.var)]]$levels
      paired <- TRUE
    } else if (posthoc.var %in% c(options$fixedFactors, options$betweenSubjectFactors, options$randomFactors)) {
      variable.levels <- levels(dataset[[posthoc.var]])
      paired <- FALSE
    } else {
      next
    }

    if (length(variable.levels) < 2L)
      next

    pairs <- utils::combn(variable.levels, 2)

    splitname <- if (model[["analysisType"]] == "RM-ANOVA") .BANOVAdependentName else dependent
    allSplits <- split(dataset[[splitname]], dataset[[posthoc.var]])

    errMessages <- NULL
    for (i in 1:ncol(pairs)) {

      if (!is.null(model$models)) {

        x <- na.omit(allSplits[[pairs[1L, i]]])
        y <- na.omit(allSplits[[pairs[2L, i]]])

        ttest <- try(BayesFactor::ttestBF(x = x, y = y, rscale = priorWidth, paired = paired), silent=TRUE)

        if (isTryError(ttest)) {

          priorOdds <- postOdds <- logBF <- error <- "NaN"
          errorMsg <- .extractErrorMessage(ttest)
          if (errorMsg %in% names(errMessages)) {
            errMessages[[errorMsg]][["row_names"]] <- c(errMessages[[errorMsg]][["row_names"]], i)
          } else {
            errMessages[[errorMsg]] <- list(
              message = if (endsWith(errorMsg, ".")) errorMsg else paste0(errorMsg, "."),
              row_names = i
            )
          }

        } else {

          if (options[["postHocTestsNullControl"]]) {
            pH0 <- 0.5^(2 / length(variable.levels))
          } else {
            pH0 <- 0.5
          }

          logBF <- ttest@bayesFactor$bf # <- log(BF10)
          if (options[["bayesFactorType"]] == "BF01") {
            priorOdds <- pH0 / (1 - pH0)
            logBF <- -logBF
          } else {
            priorOdds <- (1 - pH0) / pH0
          }

          postOdds <- log(priorOdds) + logBF
          postOdds <- exp(postOdds)
          if (options[["bayesFactorType"]] != "LogBF10")
            logBF <- exp(logBF)

          error <- ttest@bayesFactor$error * 100
        }
      }

      row <- list(
        "(I)"            = pairs[1L, i],
        "(J)"            = pairs[2L, i],
        "Prior Odds"     = priorOdds,
        "Posterior Odds" = postOdds,
        "BF"             = logBF,
        "error %"        = error
      )
      postHocTable$addRows(row, rowNames = paste0("row", i))
    }

    if (!is.null(errMessages)) {
      for (i in seq_along(errMessages))
        postHocTable$addFootnote(message  = errMessages[[i]][["message"]],
                                 rowNames = paste0("row", errMessages[[i]][["row_names"]]))

    }
    postHocCollection[[paste0("postHoc_", posthoc.var)]] <- postHocTable
  }
  return()
}

# Data reading ----
.BANOVAreadData <- function(dataset, options, analysisType) {

  if (is.null(dataset)) {
    if (analysisType == "RM-ANOVA")
      return(.BANOVAreadRManovaData(dataset, options))

    numeric.vars <- c(unlist(options$covariates), unlist(options$dependent))
    numeric.vars <- numeric.vars[numeric.vars != ""]

    factor.vars <- c(unlist(options$fixedFactors), unlist(options$randomFactors))
    factor.vars <- factor.vars[factor.vars != ""]

    dataset <- .readDataSetToEnd(
      columns.as.numeric  = numeric.vars,
      columns.as.factor   = factor.vars,
      exclude.na.listwise = c(numeric.vars, factor.vars)
    )
  }

  return(dataset)
}

.BANOVAdependentName <- "JaspColumn_.dependent._Encoded"
.BANOVAsubjectName <- "JaspColumn_.subject._Encoded"

.BANOVAdecodeSubject <- function(string) {
  return(gsub(.BANOVAsubjectName, "subject", string))
}

.BANOVAdecodeNuisance <- function(nuisance) {
  # .BANOVAsubjectName needs to be handled separately
  idx <- nuisance == .BANOVAsubjectName
  nuisance[idx]  <- "subject"
  nuisance[!idx] <- jaspBase::gsubInteractionSymbol(nuisance[!idx])
  return(nuisance)
}

.BANOVAreadRManovaData <- function(dataset, options) {

  if (!("" %in% options$repeatedMeasuresCells)) {
    rm.vars       <- options$repeatedMeasuresCells

    bs.factors    <- options$betweenSubjectFactors
    bs.covariates <- options$covariates
    rm.factors    <- options$repeatedMeasuresFactors
    all.variables <- c (bs.factors, bs.covariates, rm.vars)

    dataset <- .readDataSetToEnd(
      columns.as.numeric  = c(rm.vars, bs.covariates),
      columns.as.factor   = bs.factors,
      exclude.na.listwise = all.variables
    )
    dataset <- try(
      .shortToLong(dataset, rm.factors, rm.vars, c(bs.factors, bs.covariates), dependentName = .BANOVAdependentName, subjectName = .BANOVAsubjectName),
      silent = TRUE
    )

  }
  return(dataset)
}

# Descriptives ----
.BANOVAdescriptives <- function(jaspResults, dataset, options, errors, analysisType, ready = TRUE, position = 9001) {
  if (!ready)
    return()
  # the main use of this function is that descriptives can now be reused for the frequentist ANOVAs
  # without the container, the position could mess things up
  descriptivesContainer <- jaspResults[["descriptivesContainer"]]
  if (is.null(descriptivesContainer)) {
    descriptivesContainer <- createJaspContainer(title = gettext("Descriptives"))
    descriptivesContainer$dependOn(c("dependent", "repeatedMeasuresCells"))
    descriptivesContainer$position <- position

    jaspResults[["descriptivesContainer"]] <- descriptivesContainer
  }

  .BANOVAdescriptivesTable(descriptivesContainer, dataset, options, errors, analysisType)
  .BANOVAdescriptivesPlots(descriptivesContainer, dataset, options, errors, analysisType)
  .BANOVArainCloudPlots   (descriptivesContainer, dataset, options, errors, analysisType)
  return()

}

.BANOVAdescriptivesTable <- function(jaspContainer, dataset, options, errors, analysisType) {

  if (!options[["descriptives"]] || !is.null(jaspContainer[["tableDescriptives"]]))
    return()

  if (analysisType == "RM-ANOVA") {
    dependent <- .BANOVAdependentName
    fixed <- unlist(c(lapply(options[["repeatedMeasuresFactors"]], `[[`, "name"), options[["betweenSubjectFactors"]]))
    title <- gettext("Descriptives")
  } else {
    dependent <- options[["dependent"]]
    fixed <- options[["fixedFactors"]]
    if (!is.null(dependent) && dependent != "") {
      title <- gettextf("Descriptives - %s", options$dependent)
    } else {
      title <- gettext("Descriptives")
    }
  }

  descriptivesTable <- createJaspTable(title = title)
  descriptivesTable$position <- 1
  descriptivesTable$dependOn(c("dependent", "fixedFactors", "betweenSubjectFactors", "descriptives",
                               "credibleInterval"))

  # internal names use base64 so they don't have " " which R changes into "." because R does that to dataframe names.
  # Also adds a . in case the base64 is magically "Mean", "SD" or "N"
  fixedDot <- paste0(fixed, ".")
  for (i in seq_along(fixed))
    descriptivesTable$addColumnInfo(name = fixedDot[i], type = "string", title = fixed[i], combine = TRUE)

  overTitle <- gettextf("%s%% Credible Interval", format(100 * options[["credibleInterval"]], digits = 3))
  descriptivesTable$addColumnInfo(name = "Mean", title = gettext("Mean"),  type = "number")
  descriptivesTable$addColumnInfo(name = "SD",   title = gettext("SD"),    type = "number")
  descriptivesTable$addColumnInfo(name = "N",    title = gettext("N"),     type = "integer")
  if (is.null(options$confidenceIntervalInterval)) {
    descriptivesTable$addColumnInfo(name = gettext("Lower"), type = "number", overtitle = overTitle)
    descriptivesTable$addColumnInfo(name = gettext("Upper"), type = "number", overtitle = overTitle)
  }
  descriptivesTable$showSpecifiedColumnsOnly <- TRUE
  jaspContainer[["tableDescriptives"]] <- descriptivesTable

  if (errors$noVariables)
    return()

  # order the data to show
  dataset2 <- dataset[do.call(order, dataset[, fixed, drop = FALSE]), c(dependent, fixed)]

  # by pasting the fixedFactors together we obtain the unique indices to group on. This excludes
  # non-existent combinations. A "." is added to deal with the level "".
  ind <- apply(dataset2[, fixed, drop = FALSE], 1L, paste0, ".", collapse = "")

  # temporary function to calculate all descriptives
  tmpFun <- function(data, fixed, dependent, cri) {

    row <- list()
    for (j in fixed)
      row[[paste0(j, ".")]] <- as.character(data[1L, j])

    N <- nrow(data)
    row[["N"]] <- N

    if (N == 0L) {

      row[["Mean"]] <- row[["SD"]] <- NA

    } else if (N == 1L) {

      row[["Mean"]] <- data[[dependent]]
      row[["SD"]] <- row[["Lower"]] <- row[["Upper"]] <- NA

    } else {

      row[["Mean"]] <- mean(data[[dependent]])
      row[["SD"]]   <- stats::sd(data[[dependent]])

      tmp <- jaspTTests::.posteriorSummaryGroupMean(data[[dependent]], cri)
      row[["Lower"]] <- tmp[["ciLower"]]
      row[["Upper"]] <- tmp[["ciUpper"]]

    }
    return(row)
  }

  # apply tempFun on each subset defined by ind
  rows <- by(dataset2, ind, tmpFun, fixed = fixed,
             dependent = dependent, cri = options[["credibleInterval"]])

  # do.call(rbind, rows) turns rows into a data.frame (from a list) for jaspResults
  data <- do.call(rbind.data.frame, rows)

  # add footnote if there are unobserved combinations
  nObserved <- nrow(data)
  nPossible <- prod(sapply(dataset2[, fixed, drop = FALSE], nlevels))
  if (nObserved != nPossible) {
    descriptivesTable$addFootnote(
      message = gettextf(
        "Some combinations of factors are not observed and hence omitted (%g out of %g combinations are unobserved).",
        nPossible - nObserved, nPossible
      )
    )
  }

  descriptivesTable$setData(data)

  return()
}

.BANOVAdescriptivesPlotsO <- function(jaspContainer, dataset, options, errors, analysisType) {

  if (length(options[["plotHorizontalAxis"]]) == 0L
      || options[["plotHorizontalAxis"]] == ""
      || !is.null(jaspContainer[["containerDescriptivesPlots"]]))
    return()

  descriptivesPlotContainer <- createJaspContainer(title = gettext("Descriptives plots"))
  descriptivesPlotContainer$position <- 2
  jaspContainer[["containerDescriptivesPlots"]] <- descriptivesPlotContainer

  # either Bayesian or Frequentist anova
  if (is.null(options$confidenceIntervalInterval)) { # TRUE implies Bayesian
    plotErrorBars <- options$plotCredibleInterval
    errorBarType  <- "confidenceInterval"
    conf.interval <- options$plotCredibleIntervalInterval
    descriptivesPlotContainer$dependOn(c("dependent", "plotCredibleInterval", "plotCredibleIntervalInterval"))

  } else {
    plotErrorBars <- options$plotErrorBars
    errorBarType  <- options$errorBarType
    conf.interval <- options$confidenceIntervalInterval
    descriptivesPlotContainer$dependOn(c("dependent", "plotErrorBars", "errorBarType", "confidenceIntervalInterval",
                                         "usePooledStandErrorCI"))

  }
  usePooledSE <- if (is.null(options[["usePooledStandErrorCI"]])) FALSE else options[["usePooledStandErrorCI"]]

  descriptivesPlotContainer$dependOn(c("plotHorizontalAxis", "plotSeparateLines", "plotSeparatePlots", "labelYAxis"))

  if (errors$noVariables) {
    descriptivesPlotContainer[["dummyplot"]] <- createJaspPlot(title = gettext("Descriptives Plot"))
    return()
  }

  groupVars <- c(options$plotHorizontalAxis, options$plotSeparateLines, options$plotSeparatePlots)
  groupVars <- groupVars[groupVars != ""]
  if (analysisType == "RM-ANOVA") {
    dependent <- .BANOVAdependentName
    yLabel <- options[["labelYAxis"]]
  } else {
    dependent<- options$dependent
    yLabel <- options[["dependent"]]
  }

  betweenSubjectFactors <- groupVars[groupVars %in% options$betweenSubjectFactors]
  repeatedMeasuresFactors <- groupVars[groupVars %in% sapply(options$repeatedMeasuresFactors, `[[`, "name")]

  if (length(repeatedMeasuresFactors) == 0) {
    summaryStat <- jaspTTests::summarySE(as.data.frame(dataset), measurevar = dependent, groupvars = groupVars,
                                         conf.interval = conf.interval, na.rm = TRUE, .drop = FALSE,
                                         errorBarType = errorBarType, dependentName = .BANOVAdependentName,
                                         subjectName = .BANOVAsubjectName)
  } else {
    summaryStat <- jaspTTests::summarySEwithin(as.data.frame(dataset), measurevar= .BANOVAdependentName,
                                               betweenvars = betweenSubjectFactors,
                                               withinvars = repeatedMeasuresFactors,
                                               idvar = .BANOVAsubjectName,
                                               conf.interval = conf.interval,
                                               na.rm=TRUE, .drop = FALSE, errorBarType = errorBarType,
                                               usePooledSE = usePooledSE, dependentName = .BANOVAdependentName,
                                               subjectName = .BANOVAsubjectName)
  }

  if (options[["plotHorizontalAxis"]] %in% options[["covariates"]]) {
    splitScatterOptions <- options
    splitScatterOptions[["colorPalette"]] <- "ggplot2"
    splitScatterOptions[["showLegend"]] <- TRUE
    splitScatterOptions[["addSmooth"]] <- TRUE
    splitScatterOptions[["addSmoothCI"]] <- plotErrorBars
    splitScatterOptions[["addSmoothCIValue"]] <- TRUE
    splitScatterOptions[["regressionType"]] <- "linear"
    splitScatterOptions[["graphTypeAbove"]] <- "none"
    splitScatterOptions[["graphTypeRight"]] <- "none"
    splitScatterOptions[["addSmoothCIValue"]] <- if (is.null(options[["confidenceIntervalInterval"]]))
      options[["plotCredibleIntervalInterval"]]
    else options[["confidenceIntervalInterval"]]

    if (options$plotSeparatePlots != "") {

      for (thisLevel in levels(dataset[[options[["plotSeparatePlots"]]]])) {

        subData <- dataset[dataset[[options[["plotSeparatePlots"]]]] == thisLevel, ]
        thisPlotName <- paste0(options[["plotHorizontalAxis"]], " - ", options[["dependent"]], ": ",
                               options[["plotSeparatePlots"]], " = ", thisLevel)
        jaspDescriptives::.descriptivesScatterPlots(descriptivesPlotContainer, subData, c(options[["plotHorizontalAxis"]], options[["dependent"]]),
                                  split = options[["plotSeparateLines"]], options = splitScatterOptions, name = thisPlotName,
                                  dependOnVariables = FALSE)
      }

    } else {
      jaspDescriptives::.descriptivesScatterPlots(descriptivesPlotContainer, dataset, c(options[["plotHorizontalAxis"]], options[["dependent"]]),
                                split = options[["plotSeparateLines"]], options = splitScatterOptions, dependOnVariables = FALSE)
    }

    return()

  }

  colnames(summaryStat)[colnames(summaryStat) == dependent] <- "dependent"

  if (options$plotHorizontalAxis != "") {
    colnames(summaryStat)[colnames(summaryStat) == options$plotHorizontalAxis] <- "plotHorizontalAxis"
  }

  if (options$plotSeparateLines != "") {
    colnames(summaryStat)[colnames(summaryStat) == options$plotSeparateLines] <- "plotSeparateLines"
  }

  if (options$plotSeparatePlots != "") {
    colnames(summaryStat)[colnames(summaryStat) == options$plotSeparatePlots] <- "plotSeparatePlots"
  }

  base_breaks_x <- function(x){
    b <- unique(as.numeric(x))
    d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
    list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1))
  }

  base_breaks_y <- function(x, plotErrorBars){
    if (plotErrorBars) {
      ci.pos <- c(x[,"dependent"], x[,"dependent"]-x[,"ci"],x[,"dependent"]+x[,"ci"])
      b <- pretty(ci.pos)
      d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
      list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
           ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
    } else {
      b <- pretty(x[,"dependent"])
      d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
      list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
           ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
    }
  }

  if (options$plotSeparatePlots != "") {
    subsetPlots <- levels(summaryStat[,"plotSeparatePlots"])
    nPlots <- length(subsetPlots)
  } else {
    nPlots <- 1
  }

  for (i in seq_len(nPlots)) {

    if (nPlots > 1L) {
      title <- paste(options$plotSeparatePlots,": ",subsetPlots[i], sep = "")
    } else {
      title <- ""
    }
    descriptivesPlot <- createJaspPlot(title = title)
    descriptivesPlotContainer[[title]] <- descriptivesPlot

    descriptivesPlot$height <- 300
    if (options$plotSeparateLines != "") {
      descriptivesPlot$width <- 430
    } else {
      descriptivesPlot$width <- 300
    }

    if (options$plotSeparatePlots != "") {
      summaryStatSubset <- subset(summaryStat,summaryStat[,"plotSeparatePlots"] == subsetPlots[i])
    } else {
      summaryStatSubset <- summaryStat
    }

    if (options$plotSeparateLines == "") {

      p <- ggplot2::ggplot(summaryStatSubset, ggplot2::aes(x=plotHorizontalAxis,
                                                           y=dependent,
                                                           group=1))

    } else {

      p <- ggplot2::ggplot(summaryStatSubset, ggplot2::aes(x=plotHorizontalAxis,
                                                           y=dependent,
                                                           group=plotSeparateLines,
                                                           shape=plotSeparateLines,
                                                           fill=plotSeparateLines))

    }

    if (plotErrorBars && !(options[["plotHorizontalAxis"]] %in% options[["covariates"]])) {

      pd <- ggplot2::position_dodge(.2)
      p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower,
                                                  ymax=ciUpper),
                                     colour="black", width=.2, position=pd)

    } else {

      pd <- ggplot2::position_dodge(0)

    }

    guideLegend <- ggplot2::guide_legend(nrow = min(10, nlevels(summaryStatSubset$plotSeparateLines)),
                                         title = options$plotSeparateLines, keywidth = 0.1, keyheight = 0.3,
                                         default.unit = "inch")

    if (options[["plotHorizontalAxis"]] %in% options[["covariates"]]) {
      line <- ggplot2::geom_smooth(method = "lm", size = .7, color = "black", se = FALSE)
      addHorizontalVar <- summaryStatSubset[,"plotHorizontalAxis"]
    } else {
      line <- ggplot2::geom_line(position=pd, size = .7)
    }

    if (plotErrorBars) {
      ci.pos <- c(summaryStatSubset[,"dependent"],
                  summaryStatSubset[,"dependent"]-summaryStatSubset[,"ci"],
                  summaryStatSubset[,"dependent"]+summaryStatSubset[,"ci"],
                  min(summaryStatSubset[,"dependent"])*1.1,
                  max(summaryStatSubset[,"dependent"])*1.1)
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(ci.pos)
    } else {
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(summaryStatSubset[,"dependent"],
                                                   min(summaryStatSubset[,"dependent"])*1.1,
                                                   max(summaryStatSubset[,"dependent"])*1.1))
    }

    if (options[["plotHorizontalAxis"]] %in% options[["covariates"]]) {
      ggXaxis <- ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(summaryStatSubset[,"plotHorizontalAxis"]))
    } else {
      ggXaxis <- ggplot2::scale_x_discrete(breaks = jaspGraphs::getPrettyAxisBreaks(summaryStatSubset[,"plotHorizontalAxis"]))
    }

    p <- p + line +
      ggplot2::geom_point(position=pd, size=4) +
      ggplot2::scale_fill_manual(values = c(rep(c("white","black"),5),rep("grey",100)), guide=guideLegend) +
      ggplot2::scale_shape_manual(values = c(rep(c(21:25),each=2),21:25,7:14,33:112), guide=guideLegend) +
      ggplot2::scale_color_manual(values = rep("black",200),guide=guideLegend) +
      ggplot2::labs(y = yLabel, x = options[["plotHorizontalAxis"]]) +
      ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
      ggXaxis +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = "right")

    descriptivesPlot$plotObject <- p
  }
  return()
}












.BANOVAdescriptivesPlots <- function(jaspContainer, dataset, options, errors, analysisType) {

  if (length(options[["plotHorizontalAxis"]]) == 0L
      || options[["plotHorizontalAxis"]] == ""
      || !is.null(jaspContainer[["containerDescriptivesPlots"]]))
    return()

  descriptivesPlotContainer <- createJaspContainer(title = gettext("Descriptives plots"))
  descriptivesPlotContainer$position <- 2
  jaspContainer[["containerDescriptivesPlots"]] <- descriptivesPlotContainer

  # either Bayesian or Frequentist anova
  if (is.null(options$confidenceIntervalInterval)) { # TRUE implies Bayesian
    plotErrorBars <- options$plotCredibleInterval
    errorBarType  <- "confidenceInterval"
    conf.interval <- options$plotCredibleIntervalInterval
    descriptivesPlotContainer$dependOn(c("dependent", "plotCredibleInterval", "plotCredibleIntervalInterval"))

  } else {
    plotErrorBars <- options$plotErrorBars
    errorBarType  <- options$errorBarType
    conf.interval <- options$confidenceIntervalInterval
    descriptivesPlotContainer$dependOn(c("dependent", "plotErrorBars", "errorBarType", "confidenceIntervalInterval",
                                         "usePooledStandErrorCI"))

  }
  usePooledSE <- if (is.null(options[["usePooledStandErrorCI"]])) FALSE else options[["usePooledStandErrorCI"]]

  descriptivesPlotContainer$dependOn(c("plotHorizontalAxis", "plotSeparatePlots", "labelYAxis"))

  if (errors$noVariables) {
    descriptivesPlotContainer[["dummyplot"]] <- createJaspPlot(title = gettext("Descriptives Plot"))
    return()
  }

  groupVars <- c(options$plotHorizontalAxis, options$plotSeparatePlots)
  groupVars <- groupVars[groupVars != ""]
  if (analysisType == "RM-ANOVA") {
    dependent <- .BANOVAdependentName
    yLabel <- options[["labelYAxis"]]
  } else {
    dependent<- options$dependent
    yLabel <- options[["dependent"]]
  }

  betweenSubjectFactors <- groupVars[groupVars %in% options$betweenSubjectFactors]
  repeatedMeasuresFactors <- groupVars[groupVars %in% sapply(options$repeatedMeasuresFactors, `[[`, "name")]

  if (length(repeatedMeasuresFactors) == 0) {
    summaryStat <- jaspTTests::summarySE(as.data.frame(dataset), measurevar = dependent, groupvars = groupVars,
                                         conf.interval = conf.interval, na.rm = TRUE, .drop = FALSE,
                                         errorBarType = errorBarType, dependentName = .BANOVAdependentName,
                                         subjectName = .BANOVAsubjectName)
  } else {
    summaryStat <- jaspTTests::summarySEwithin(as.data.frame(dataset), measurevar= .BANOVAdependentName,
                                               betweenvars = betweenSubjectFactors,
                                               withinvars = repeatedMeasuresFactors,
                                               idvar = .BANOVAsubjectName,
                                               conf.interval = conf.interval,
                                               na.rm=TRUE, .drop = FALSE, errorBarType = errorBarType,
                                               usePooledSE = usePooledSE, dependentName = .BANOVAdependentName,
                                               subjectName = .BANOVAsubjectName)
  }

  if (options[["plotHorizontalAxis"]] %in% options[["covariates"]]) {
    splitScatterOptions <- options
    splitScatterOptions[["colorPalette"]] <- "ggplot2"
    splitScatterOptions[["showLegend"]] <- TRUE
    splitScatterOptions[["addSmooth"]] <- TRUE
    splitScatterOptions[["addSmoothCI"]] <- plotErrorBars
    splitScatterOptions[["addSmoothCIValue"]] <- TRUE
    splitScatterOptions[["regressionType"]] <- "linear"
    splitScatterOptions[["graphTypeAbove"]] <- "none"
    splitScatterOptions[["graphTypeRight"]] <- "none"
    splitScatterOptions[["addSmoothCIValue"]] <- if (is.null(options[["confidenceIntervalInterval"]]))
      options[["plotCredibleIntervalInterval"]]
    else options[["confidenceIntervalInterval"]]

    if (options$plotSeparatePlots != "") {

      for (thisLevel in levels(dataset[[options[["plotSeparatePlots"]]]])) {

        subData <- dataset[dataset[[options[["plotSeparatePlots"]]]] == thisLevel, ]
        thisPlotName <- paste0(options[["plotHorizontalAxis"]], " - ", options[["dependent"]], ": ",
                               options[["plotSeparatePlots"]], " = ", thisLevel)
        jaspDescriptives::.descriptivesScatterPlots(descriptivesPlotContainer, subData, c(options[["plotHorizontalAxis"]], options[["dependent"]]),
                                                    split = "", options = splitScatterOptions, name = thisPlotName, dependOnVariables = FALSE)
      }

    } else {
      jaspDescriptives::.descriptivesScatterPlots(descriptivesPlotContainer, dataset, c(options[["plotHorizontalAxis"]], options[["dependent"]]),
                                                  split = "", options = splitScatterOptions, dependOnVariables = FALSE)
    }

    return()

  }

  colnames(summaryStat)[colnames(summaryStat) == dependent] <- "dependent"

  if (options$plotHorizontalAxis != "") {
    colnames(summaryStat)[colnames(summaryStat) == options$plotHorizontalAxis] <- "plotHorizontalAxis"
  }

  if (options$plotSeparatePlots != "") {
    colnames(summaryStat)[colnames(summaryStat) == options$plotSeparatePlots] <- "plotSeparatePlots"
  }

  if (options$plotSeparatePlots != "") {
    subsetPlots <- levels(summaryStat[,"plotSeparatePlots"])
    nPlots <- length(subsetPlots)
  } else {
    nPlots <- 1
  }

  for (i in seq_len(nPlots)) {

    if (nPlots > 1L) {
      title <- paste(options$plotSeparatePlots,": ",subsetPlots[i], sep = "")
    } else {
      title <- ""
    }
    descriptivesPlot <- createJaspPlot(title = title)
    descriptivesPlotContainer[[title]] <- descriptivesPlot

    descriptivesPlot$height <- 500
    descriptivesPlot$width <- 500

    if (options$plotSeparatePlots != "") {
      summaryStatSubset <- subset(summaryStat,summaryStat[,"plotSeparatePlots"] == subsetPlots[i])
    } else {
      summaryStatSubset <- summaryStat
    }

    p <- ggplot2::ggplot(summaryStatSubset, ggplot2::aes(x = plotHorizontalAxis,
                                                         y = dependent, group = 1))

    if (plotErrorBars){ #&& !(options[["plotHorizontalAxis"]] %in% options[["covariates"]])) {

      pd <- ggplot2::position_dodge(.2)
      p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper),
                                     colour = "black", width = .2, position = pd)

    }# else {

      #pd <- ggplot2::position_dodge(0)

    #}

    guideLegend <- ggplot2::guide_legend(nrow = min(10, nlevels(NULL)), keywidth = 0.1,
                                         keyheight = 0.3, default.unit = "inch")

    #if (options[["plotHorizontalAxis"]] %in% options[["covariates"]]) {
    #  line <- ggplot2::geom_smooth(method = "lm", size = .7, color = "black", se = FALSE)
    #  addHorizontalVar <- summaryStatSubset[,"plotHorizontalAxis"]
    #} else {
    #  line <- ggplot2::geom_line(position=pd, size = .7)
    #}

    pd2 <- ggplot2::position_dodge2(preserve = "single")

    if (plotErrorBars) {
      ci.pos <- c(summaryStatSubset[,"dependent"],
                  summaryStatSubset[,"dependent"]-summaryStatSubset[,"ci"],
                  summaryStatSubset[,"dependent"]+summaryStatSubset[,"ci"],
                  min(summaryStatSubset[,"dependent"])*1.1,
                  max(summaryStatSubset[,"dependent"])*1.1)
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(ci.pos)
    } else {
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(summaryStatSubset[,"dependent"],
                                                   min(summaryStatSubset[,"dependent"])*1.1,
                                                   max(summaryStatSubset[,"dependent"])*1.1))
    }

    if (options[["plotHorizontalAxis"]] %in% options[["covariates"]]) {
      ggXaxis <- ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(summaryStatSubset[,"plotHorizontalAxis"]))
    } else {
      ggXaxis <- ggplot2::scale_x_discrete(breaks = jaspGraphs::getPrettyAxisBreaks(summaryStatSubset[,"plotHorizontalAxis"]))
    }

    p <- p + ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", width = .2, position = pd2) +
      #ggplot2::geom_point(position=pd, size=4) +
      ggplot2::labs(y = yLabel, x = options[["plotHorizontalAxis"]]) +
      ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
      ggXaxis +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = "right")
      #ggplot2::scale_fill_manual(values = c(rep(c("white", "black"), 5), rep("grey", 100)), guide = guideLegend) +
      #ggplot2::scale_shape_manual(values = c(rep(c(21:25), each = 2), 21:25, 7:14, 33:112), guide = guideLegend) +
      #ggplot2::scale_color_manual(values = rep("black",200), guide = guideLegend) +


    descriptivesPlot$plotObject <- p
  }
  return()
}



















.BANOVArainCloudPlots <- function(jaspContainer, dataset, options, errors, analysisType) {

  if (length(options[["rainCloudPlotsHorizontalAxis"]]) == 0L
      || options[["rainCloudPlotsHorizontalAxis"]] == ""
      || !is.null(jaspContainer[["containerRainCloudPlots"]]))
    return()

  rainCloudPlotsContainer <- createJaspContainer(title = gettext("Raincloud plots"))
  rainCloudPlotsContainer$position <- 3
  jaspContainer[["containerRainCloudPlots"]] <- rainCloudPlotsContainer
  rainCloudPlotsContainer$dependOn(c("dependent", "rainCloudPlotsHorizontalAxis", "rainCloudPlotsSeparatePlots",
                                     "rainCloudPlotsLabelYAxis", "rainCloudPlotsHorizontalDisplay"))

  if (errors$noVariables) {
    rainCloudPlotsContainer[["dummyplot"]] <- createJaspPlot(title = "")
    return()
  }

  groupVar <- options[["rainCloudPlotsHorizontalAxis"]]
  if (analysisType == "RM-ANOVA") {
    addLines   <- !(groupVar %in% unlist(options[["betweenSubjectFactors"]]))
    dependentV <- .BANOVAdependentName
    yLabel     <- options[["rainCloudPlotsLabelYAxis"]]
  } else {
    addLines   <- FALSE
    dependentV <- options[["dependent"]]
    yLabel     <- options[["dependent"]]
  }

  if (!is.null(options$rainCloudPlotsHorizontalDisplay) && options$rainCloudPlotsHorizontalDisplay)
    horiz <- TRUE
  else
    horiz <- FALSE

  if (options$rainCloudPlotsSeparatePlots != "") {
    for (thisLevel in levels(dataset[[options[["rainCloudPlotsSeparatePlots"]]]])) {
      subData      <- dataset[dataset[[options[["rainCloudPlotsSeparatePlots"]]]] == thisLevel, ]
      thisPlotName <- paste0(dependentV, ": ", options[["rainCloudPlotsSeparatePlots"]], ": ", thisLevel)
      subPlot      <- createJaspPlot(title = thisPlotName, width = 480, height = 320)
      rainCloudPlotsContainer[[thisLevel]] <- subPlot
      p <- try(jaspTTests::.descriptivesPlotsRainCloudFill(subData, dependentV, groupVar, yLabel, groupVar, addLines, horiz, NULL))
      if(isTryError(p))
        subPlot$setError(.extractErrorMessage(p))
      else
        subPlot$plotObject <- p
    }
  } else {
    singlePlot <- createJaspPlot(title = dependentV, width = 480, height = 320)
    rainCloudPlotsContainer[["rainCloudPlotSingle"]] <- singlePlot
    p <- try(jaspTTests::.descriptivesPlotsRainCloudFill(dataset, dependentV, groupVar, yLabel, groupVar, addLines, horiz, NULL))
    if(isTryError(p))
      singlePlot$setError(.extractErrorMessage(p))
    else
      singlePlot$plotObject <- p
  }
  return()
}

# Sample posteriors ----
.BANOVAsamplePosteriors <- function(jaspResults, dataset, options, model, state) {

  # TODO: the density approximation can become more efficient with a fast parametric density approximation
  # TODO: Bayes factor samples unobserved interaction levels, what to do?

  # if the most complex model is retrieved from the state?
  nIter <- if (options[["sampleModeMCMC"]] == "auto") 1e4L else options[["fixedMCMCSamples"]]
  nmodels    <- length(model[["models"]])
  postProbs  <- model[["postProbs"]]
  statistics <- vector("list", nmodels)

  randomFactors <- .BANOVAgetRandomFactors(options, model[["analysisType"]])
  dataTypes     <- .BANOVAgetDataTypes(dataset, model[["model.list"]][[nmodels]], randomFactors)
  levelInfo     <- .BANOVAgetLevelInfo(dataset, model[["model.list"]][[nmodels]], dataTypes)

  renameFrom <- renameTo <- NULL
  if (!is.null(state)) { # can we reuse some posteriors?
    reuseable <- model[["reuseable"]]

    # it's possible that a user just renames a level of a repeated measures factor
    # in that case everything can be reused, but we have to rename some parameters.
    # the same may happen when a variable becomes a nuisance parameter, because then
    # BayesFactor changes its order in the names.

    oldLevelInfo <- state[["levelInfo"]]$levelNames
    newLevelInfo <- levelInfo$levelNames

    sortTerms <- function(x) {
      # split b:a to c("b", "a"), sort it, and then paste it back
      # otherwise posteriors samples between different runs are not correctly retrieved from the state.
      sapply(strsplit(x, ":", fixed = TRUE), function(x) paste(sort(x), collapse = ":"))
    }

    tmp_old <- sortTerms(names(oldLevelInfo))
    tmp_new <- sortTerms(names(newLevelInfo))
    matches <- match(tmp_new, tmp_old)
    for (i in seq_along(matches)) {
      if (is.na(matches[i]))
        next

      j <- matches[i]
      if (!identical(oldLevelInfo[[j]], newLevelInfo[[i]])) {
        if (length(oldLevelInfo[[j]]) == length(newLevelInfo[[i]])) {
          renameFrom <- c(renameFrom, oldLevelInfo[[j]])
          renameTo   <- c(renameTo,   newLevelInfo[[i]])
        }
      }
    }

  } else {
    reuseable <- rep(NA, nmodels)
  }

  # NOTE: some code checks require saving all samples. To do so, change all samples to samples[[i]] and uncomment:
  # samples <- vector("list", nmodels)

  allParamNames <- c("mu", unlist(levelInfo$levelNames))
  nparam <- length(allParamNames)
  weightedMeans <- weights <- numeric(nparam)
  names(weights) <- names(weightedMeans) <- allParamNames
  allContinuous <- TRUE

  h <- (1 - options[["credibleInterval"]]) / 2
  probs <- c(h, 1-h)

  originalFun <- BayesFactor:::makeChainNeater
  on.exit(jaspBase:::assignFunctionInPackage(originalFun, "makeChainNeater", "BayesFactor"))
  jaspBase:::assignFunctionInPackage(.BANOVAmakeChainNeater, "makeChainNeater", "BayesFactor")

  samplingIssues <- list()
  startProgressbar(nmodels, gettext("Sampling posteriors"))
  for (i in seq_len(nmodels)) {
    # check if the state is reuseable and if it's not NULL, which means it didn't crash
    if (is.na(reuseable[i]) || is.null(state$statistics[[reuseable[i]]])) {

      if (i == 1L && is.null(model[["models"]][[i]][["bf"]])) {

        # NULL model only contains an intercept, use custom sampler
        # NOTE: RM-ANOVA never enters here (and would crash if it did)
        .setSeedJASP(options)
        samples <- .BANOVAsampleNullModel(dataset[[options[["dependent"]]]], nsamples = nIter)
        types <- NULL

      } else {

        # NOTE: we have to sample the random effects, otherwise we cant make predictions (needed for residuals and R^2)
        # put the dataset back in
        .setSeedJASP(options)
        bfObj <- model[["models"]][[i]][["bf"]]
        bfObj@data <- dataset
        samples <- try(BayesFactor::posterior(bfObj, iterations = nIter))
        if (isTryError(samples)) {
          # this may error whenever the Bayes factor couldn't be calculated
          next
        }
        types <- samples@model@dataTypes

        # BayesFactor stores an internal copy of the dataset, so it can still have old names
        if (length(renameFrom) > 0) {
          cnms <- colnames(samples)
          cnms <- plyr::mapvalues(cnms, renameFrom, renameTo, warn_missing = FALSE) # NOTE: dependency could be removed
          colnames(samples) <- cnms
        }

        # keep only relevant columns, drop sig2, g_xxx, ...
        idx <- match(allParamNames, colnames(samples), nomatch = 0L)
        samples <- samples[, idx, drop = FALSE]

        # for some odd reason, Bayesfactor uses as column name contcor1-contcor1
        # if there is a covariate AND fixed factors, but only contcor1 if all variables are continuous...
        if (all(types == "continuous")) {
          cnms <- colnames(samples)[-1L] # omit the intercept (mu) which is not changed by Bayesfactor
          colnames(samples)[-1L] <- paste0(cnms, "-", cnms)
        } else {
          allContinuous <- FALSE
        }

        # so some models may yield a bunch of NAs, for example,
        # BayesFactor::lmBF(contNormal ~ facGender + contGamma + facGender * contGamma, data = "debug.csv", whichRandom = "facGender")
        # we add a footnote and try to not to crash.
        if (anyNA(samples)) {

          originalRows <- nrow(samples)
          samples  <- samples[complete.cases(samples), , drop = FALSE]
          remainingRows     <- nrow(samples)

          samplingIssues[[length(samplingIssues) + 1L]] <- list(
            model         = model[["models"]][[i]][["title"]],
            originalRows  = originalRows,
            remainingRows = remainingRows
          )
          next
        }

      }

      # although matrixStats::colMeans2 is faster than .colMeans the cost of matrixStats:: is not worth it.
      nms <- colnames(samples)
      statistics[[i]]$names  <- nms # <- these are the names for all objects within one sublist
      statistics[[i]]$mean   <- .colMeans                (samples, m = nIter, n = NCOL(samples))
      statistics[[i]]$var    <- matrixStats::colVars     (samples)
      statistics[[i]]$cri    <- matrixStats::colQuantiles(samples, probs = probs)

      statistics[[i]]$approx <- .BANOVAfitDensity(samples = samples)
      statistics[[i]]$types  <- types

    } else { # reuse state
      statistics[[i]] <- state$statistics[[reuseable[i]]]
      nms             <- statistics[[i]]$names
      types           <- statistics[[i]]$types
      if (length(renameFrom) > 0) {
        nms <- plyr::mapvalues(nms, renameFrom, renameTo, warn_missing = FALSE) # NOTE: dependency could be removed
        statistics[[i]]$names <- nms
      }
      if (allContinuous)
        allContinuous <- all(types == "continuous")
    }

    # compute model averaged posterior means
    weightedMeans[nms] <- weightedMeans[nms] + postProbs[i] * statistics[[i]]$mean
    weights      [nms] <- weights      [nms] + postProbs[i]
    progressbarTick()
  }

  # find out which parameters are random -- this uses types from the last iteration above
  isRandom <- logical(nparam)
  idx <- which(types == "random")
  for (i in idx)
    isRandom <- isRandom | startsWith(names(weights), names(types)[i])

  # the weights used above don't sum to 1 because we consider a subset of the models.
  # Now we renormalize to ensure the weights used in each weighted mean do sum to 1.
  weightedMeans <- weightedMeans / weights

  # given the model averaged posterior means calculate the weighted posterior standard deviations
  weightedSds <- 0 * weightedMeans # keeps the names
  r <- nIter / (nIter - 1)
  # statistics has length 0 iff the posterior sampling errored
  widxGoodStatistics <- which(lengths(statistics) > 0L)
  for (i in widxGoodStatistics) {
    nms <- statistics[[i]]$names

    var <- statistics[[i]]$var # ~ sum((x - mean(x))^2
    mu  <- statistics[[i]]$mean

    # cc = sum((x - y)^2) - sum((x - mean(x))^2), where y is the weighted mean
    # hence, we can get the weighted sum of squares from the individual posterior variances
    cc <- weightedMeans[nms]^2 + mu^2 - 2 * mu * weightedMeans[nms]
    weightedSds[nms] <- weightedSds[nms] + postProbs[i] * (var + cc * r)

  }
  weightedSds <- sqrt(weightedSds / weights)

  # the loops above are optimized versions of the code below that also calculates the
  # weighted mean and weighted sd, but needs to store all samples at once.
  # xx <- lapply(samples, function(x, y) {
  #   if (y %in% colnames(x)) {dd <- model$posteriors$weightedRsqDens
  #     x[, y]
  #   } else {
  #     NULL
  #   }
  # }, y = "XY29udEJpbm9t-1") # XY29udEJpbm9t = contBinom
  # idx <- lengths(xx) > 0
  # xx <- unlist(xx[idx])
  # w <- postProbs[idx]
  # sum(w) # equals weights[2]
  # weighted.mean(xx,  rep(w / sum(w), each = nIter)) # equals weightedMeans[2]
  # sqrt(Hmisc::wtd.var(xx, rep(w / sum(w), each = nIter), method = "unbiased")) # equals weightedSds[2]

  # compute model averaged densities
  steps <- 2^9 # grid size for densities is 2^steps
  weightedDensities <- array(0, dim = c(steps, nparam, 2), dimnames = list(NULL, names(weightedMeans), c("x", "y")))
  ranges <- matrix(0, nparam, 2L, dimnames = list(allParamNames, NULL))

  # get the outermost x-values for each densities this could be vectorized with matrixStats::rowRanges if the
  # data are stored as a matrix but that is memory inefficient
  for (i in widxGoodStatistics) {
    nms <- statistics[[i]]$names
    indices <- which(nms %in% allParamNames)
    for (j in indices) {
      ranges[j, ] <- range(ranges[j, ], statistics[[i]]$approx$xRanges[j, ])
    }
  }

  # construct one common grid for each observed density
  for (i in seq_len(nparam)) {
    weightedDensities[, i, 1L] <- seq(ranges[i, 1], ranges[i, 2], length.out = steps)
  }

  for (i in widxGoodStatistics) {
    nms <- statistics[[i]]$names
    ind <- match(nms, allParamNames)
    for (j in seq_along(ind)) {
      # approximate all distributions on a common grid
      ap <- approx(x    = statistics[[i]]$approx$fit[, j,      1L],
                   y    = statistics[[i]]$approx$fit[, j,      2L],
                   xout = weightedDensities         [, ind[j], 1L],
                   # not observed is approximated to 0
                   yleft = 0, yright = 0)

      weightedDensities[, ind[j], 2L] <- weightedDensities[, ind[j], 2L] + ap$y * postProbs[i]

    }
  }
  # postProbs don't sum to one so we renormalize the densities
  weightedDensities[, , 2L] <- sweep(weightedDensities[, , 2L], 2, weights, FUN = `/`)

  # if all model with one parameter failed, the line above introduces NaNs since 0 / 0 is NaN
  weightedDensities[is.nan(weightedDensities)] <- 0

  # # compute weighted CRIs
  # weightedCRIs <- matrix(NA, nparam, 2L, dimnames = list(names(weights), NULL))
  # cri <- options[["credibleInterval"]]
  # for (i in seq_len(nparam)) {
  #   weightedCRIs[i, ] <- .BANOVAapproxCRI(weightedDensities[, i, 1L], weightedDensities[, i, 2L], cri)
  # }
  #
  # # compute residuals and r-squared
  # # sample from the joint posterior over models and parameters
  # tmp  <- .BANOVAgetSMIResidRsq(weightedDensities, dataset, model$model.list[[nmodels]], nIter, weights)
  # means  <- rowMeans(tmp$resid)
  # h <- (1 - cri) / 2
  # quants <- matrixStats::rowQuantiles(tmp$resid, probs = c(h, 1 - h))
  #
  # # the code above is equivalent to the code below, but the code below needs to keep all posterior samples of
  # # all models in memory.
  # # weights <- rep(postProbs, each = nIter)
  # # independentVariable <- all.vars(.BANOVAgetModelFormulaFromBFobj(model$models[[2L]]))[1L]
  # # resids <- matrix(NA, nrow(dataset), 0)
  # # rsq <- vector("list", nmodels)
  # # # get residuals of all models individually
  # # for (i in seq_len(nmodels)) {
  # #   if (is.null(model$models[[i]]$bf)) {
  # #     tmp2    <- .BANOVAresidualsNullModel(nIter, dataset[[independentVariable]])
  # #   } else {
  # #     tmp2 <- .BANOVAgetSMIResidRsq(
  # #       posterior = samples[[i]],
  # #       dataset   = dataset,
  # #       formula   = .BANOVAgetModelFormulaFromBFobj(model$models[[i]])
  # #     )
  # #   }
  # #   resids <- cbind(resids, tmp2$resid)
  # #   rsq[[i]] <- tmp2$rsq
  # # }
  # # # compute weighted mean for each row
  # # means2 <- tcrossprod(weights / nIter, resids)
  # # plot(means, means2); abline(0, 1)
  # # quants2 <- apply(resids, 1L, Hmisc::wtd.quantile, weights = weights, probs = c(0.025, 0.975))
  # # plot(quants[, 1], quants2[1, ]); abline(0, 1)
  # # plot(quants[, 2], quants2[2, ]); abline(0, 1)
  #
  # # all information for q-q plot of residuals
  # weightedResidSumStats <- matrix(c(means, quants), nrow = length(means), ncol = 3L,
  #                                 dimnames = list(NULL, c("mean", "cri.2.5%", "cri.97.5%")))
  #
  # # all information for r-squared density plot
  # weightedRsqDens <- density(tmp$rsq, n = 2^11, from = 0, to = 1)
  # weightedRsqCri <- quantile(tmp$rsq, probs   = c(h, 1 - h))

  return(list(
    statistics = statistics, weights = weights, weightedMeans = weightedMeans, weightedSds = weightedSds,
    weightedDensities = weightedDensities,
    #weightedCRIs = weightedCRIs, weightedResidSumStats = weightedResidSumStats,
    #weightedRsqDens = weightedRsqDens, weightedRsqCri = weightedRsqCri,
    allContinuous = allContinuous, isRandom = isRandom, levelInfo = levelInfo,
    samplingIssues = samplingIssues
  ))
}

.BANOVAcomputePosteriorCRI <- function(dataset, options, model, posterior) {

  nIter <- if (options[["sampleModeMCMC"]] == "auto") 1e4L else options[["fixedMCMCSamples"]]
  weightedDensities <- posterior[["weightedDensities"]]
  weights <- posterior[["weights"]]
  nmodels    <- length(model[["models"]])
  nparam <- length(weights)

  # compute weighted CRIs
  weightedCRIs <- matrix(0, nparam, 2L, dimnames = list(names(weights), NULL))
  cri <- options[["credibleInterval"]]
  for (i in seq_len(nparam)) {
    if (!all(weightedDensities[, i, 2L] == 0)) { # only TRUE if we explicitly set it to 0
      weightedCRIs[i, ] <- .BANOVAapproxCRI(weightedDensities[, i, 1L], weightedDensities[, i, 2L], cri)
    }
  }

  # compute residuals and r-squared
  # sample from the joint posterior over models and parameters
  tmp  <- .BANOVAgetSMIResidRsq(weightedDensities, dataset, model$model.list[[nmodels]], nIter, model, options)
  means  <- rowMeans(tmp$resid)
  h <- (1 - cri) / 2
  quants <- matrixStats::rowQuantiles(tmp$resid, probs = c(h, 1 - h))

  # the code above is equivalent to the code below, but the code below needs to keep all posterior samples of
  # all models in memory. plug this body of this function into the call inside .BANOVAsamplePosteriors() to check
  # weights <- rep(postProbs, each = nIter)
  # independentVariable <- all.vars(.BANOVAgetModelFormulaFromBFobj(model$models[[2L]]))[1L]
  # resids <- matrix(NA, nrow(dataset), 0)
  # rsq <- vector("list", nmodels)
  # # get residuals of all models individually
  # for (i in seq_len(nmodels)) {
  #   if (is.null(model$models[[i]]$bf)) {
  #     tmp2    <- .BANOVAresidualsNullModel(nIter, dataset[[independentVariable]])
  #   } else {
  #     tmp2 <- .BANOVAgetSMIResidRsq(
  #       posterior = samples[[i]],
  #       dataset   = dataset,
  #       formula   = .BANOVAgetModelFormulaFromBFobj(model$models[[i]])
  #     )
  #   }
  #   resids <- cbind(resids, tmp2$resid)
  #   rsq[[i]] <- tmp2$rsq
  # }
  # # compute weighted mean for each row
  # means2 <- tcrossprod(weights / nIter, resids)
  # plot(means, means2); abline(0, 1)
  # quants2 <- apply(resids, 1L, Hmisc::wtd.quantile, weights = weights, probs = c(0.025, 0.975))
  # plot(quants[, 1], quants2[1, ]); abline(0, 1)
  # plot(quants[, 2], quants2[2, ]); abline(0, 1)

  # all information for q-q plot of residuals
  weightedResidSumStats <- matrix(c(means, quants), nrow = length(means), ncol = 3L,
                                  dimnames = list(NULL, c("mean", "cri.2.5%", "cri.97.5%")))

  # all information for r-squared density plot
  weightedRsqDens <- density(tmp$rsq, n = 2^11, from = 0, to = 1)
  weightedRsqCri <- quantile(tmp$rsq, probs   = c(h, 1 - h))
  weightedRsqMean <- mean(tmp$rsq)

  return(list(
    weightedCRIs = weightedCRIs, weightedResidSumStats = weightedResidSumStats,
    weightedRsqDens = weightedRsqDens, weightedRsqCri = weightedRsqCri, weightedRsqMean = weightedRsqMean
  ))
}

.BANOVAsampleNullModel <- function(nsamples, dependent) {

  # sample from posterior under NULL model, needed to compute model averaged residuals.

  rt.scaled <- function (n, df, mean = 0, sd = 1, ncp) {
    mean + sd * stats::rt(n, df, ncp = ncp)
  }

  # sample from the marginal posterior distribution of the mean t-distribution, based on Murphy (2007)
  n      <- length(dependent)
  muObs  <- mean(dependent)
  varObs <- var(dependent)

  # uninformative priors
  k0  <- 1e-6
  a0  <- 1e-5
  b0  <- 1e-5

  an  <- a0 + n / 2
  bn  <- b0 + 0.5 * (n - 1L) * varObs + k0 * n * muObs / (2 * (k0 + n))
  kn  <- k0 + n
  mun <- n * muObs / kn

  samples <- matrix(rt.scaled(n = nsamples, df = 2 * an, mean = mun, sd = bn / (an * kn), ncp = 0), nsamples,
                    1L, dimnames = list(NULL, "mu"))
  return(samples)
}

.BANOVAresidualsNullModel <- function(nsamples, dependent) {

  # sample from posterior under NULL model, needed to compute model averaged residuals.

  rt.scaled <- function (n, df, mean = 0, sd = 1, ncp) {
    mean + sd * stats::rt(n, df, ncp = ncp)
  }

  # sample from the marginal posterior distribution of the mean t-distribution, based on Murphy (2007)
  n      <- length(dependent)
  muObs  <- mean(dependent)
  varObs <- var(dependent)

  # uninformative priors
  k0  <- 1e-6
  a0  <- 1e-5
  b0  <- 1e-5

  an  <- a0 + n / 2
  bn  <- b0 + 0.5 * (n - 1L) * varObs + k0 * n * muObs / (2 * (k0 + n))
  kn  <- k0 + n
  mun <- n * muObs / kn

  samples <- rt.scaled(n = nsamples, df = 2 * an, mean = mun, sd = bn / (an * kn), ncp = 0)

  # compute all pairwise differences (residuals)
  preds <- tcrossprod(rep(1, n), samples)

  resids <- dependent - preds

  rsq <- .BANOVAcomputeRsq(dependent, preds)

  return(list(resids = resids, rsq = rsq))

}

.BANOVAfitDensity <- function(samples, gridsize = 100L, getXRange = TRUE) {

  # ideally we don't do kernel density estimation but instead use some parametric approximation that is suited
  # for unimodal distributions. It should also be fast, e.g., fit using method of moments (or another analytic method)
  nc <- ncol(samples)
  fits  <- array(
    data     = NA,
    dim      = c(gridsize, nc, 2),
    dimnames = list(NULL, colnames(samples), c("x", "y"))
  )

  n <- nrow(samples)
  del0 <- (1/(4 * pi))^(1/10) # normal kernel
  for (i in seq_len(nc)) {
    # bandwidth definition from KernSmooth::bkde, but adjusted when sqrt(var(samples[, i])) is infinite
    vx <- sqrt(var(samples[, i]))
    if (is.infinite(vx)) # contWide can cause the variance to be infinite
      vx <- sqrt(.Machine$double.xmax) # some big number as a fallback
    bandwidth <- del0 * (243/(35 * n))^(1/5) * vx
    fits[, i, ] <- do.call(cbind, KernSmooth::bkde(samples[, i], gridsize = gridsize, bandwidth = bandwidth))
  }
  if (getXRange) {
    if (nc == 1L) {
      xRanges <- matrix(range(fits[, , "x"]), 1L, 2L)
    } else {
      xRanges <- matrixStats::colRanges(fits[, , "x"])
    }
    rownames(xRanges) <- colnames(samples)
  } else {
    xRanges <- NULL
  }
  return(list(fit = fits, xRanges = xRanges))
}

.BANOVAapproxCRI <- function(x, y, cri = 0.95) {

  # approximate cdf
  y2 <- cumsum(y)
  y2 <- y2 / y2[length(y2)]

  h <- (1 - cri) / 2
  # find closest observed match
  idx <- c(
    which.min(abs(y2 - h)),
    which.min(abs(y2 - 1 + h))
  )
  return(x[idx])
}

# plot posteriors ----
.BANOVAgetBMAdensity <- function(samples, weights, fromTo = NULL, n = 2^10) {

  # @param samples, list of samples
  # @param weigths, vector of numeric weights
  # @param fromTo vector of length 2 specifying lower and upper bound (optional).
  # @return a list with $x the x-coordinates and $y the y-coordinates.

  if (length(samples) != length(weights))
    stop(gettext("length of samples must be equal to length of weights!"))

  # remove NULL indices
  idxNonNull <- lengths(samples) > 0
  weights <- weights[idxNonNull]
  samples <- samples[idxNonNull]

  # renormalize the weights so output is a proper density function (TODO: do we want this, or BAS style?)
  weights <- weights / sum(weights)

  # create x-grid for density
  if (is.null(fromTo))
    fromTo <- range(sapply(samples, range))
  xs <- seq(fromTo[1L], fromTo[2L], length.out = 2^10)

  # compute weighted density
  ys <- numeric(n)
  for (i in seq_along(samples)) {
    ys <- ys + weights[i] * density(samples[[i]], from = fromTo[1L], to = fromTo[2L], n = n)$y
  }

  return(list(x = xs, y = ys))
}

.BANOVAreSample <- function(n, x, y, prop0 = NULL) {

  cdf <- cumsum(y)
  max <- cdf[length(cdf)]
  if (max == 0) # can't salvage this
    return(numeric(n))
  cdf <- cdf / max
  if (!is.null(prop0) && prop0 <= (1 - sqrt(.Machine$double.eps))) {
    i1 <- runif(n) <= prop0
    samples <- numeric(n)
    samples[i1] <- approx(cdf, x, runif(sum(i1)), rule = 2)$y
  } else {
    samples <- approx(cdf, x, runif(n), rule = 2)$y
  }
  return(samples)
}

.BANOVAgetSMIResidRsq <- function(posterior, dataset, formula, nIter, model, options) {

  # @param posterior  object from Bayesfactor package, or SxPx2 array of weighted densities
  # @param dataset    dataset
  # @param formula    the formula for this specific model. Supply the formula of the most complex model for BMA inference.
  # @param nIter      number of posterior samples
  # @param model      model estimates
  # @param levelInfo  for each variable, how many levels there are.
  #
  # @return           a list with residuals, predictions, and r-squared

  # @details the matrix multiplication in this function allocates an array of nobs * nsamples, which can be enormous.
  # if is more memory efficient to use for loops (but slower in R) to calculate predictions for each observation
  # and posterior sample. This could be done in the future if performance is an issue. However, this likely cannot be
  # done efficiently in R. For modelAveraged inference, model and levelInfo need to be given.

  # here we need the data in a one-hot encoded way
  # first one is dependent variable, rest are independent variables
  dvs <- all.vars(formula)[1L]
  ivs <- all.vars(formula)[-1L]

  # idx contains variables that need to be one-hot encoded
  idx <- sapply(dataset[ivs], is.factor)
  # idx <- isFactor & names(isFactor) %in% dvs

  # do one-hot encoding (thank base R for calling a function `contrast` and giving it the argument `contrast`...)
  datOneHot <- model.matrix(formula, data = dataset[c(dvs, ivs)],
                            contrasts.arg = lapply(dataset[ivs][idx], contrasts, contrasts = FALSE))

  nobs <- nrow(datOneHot)

  # possible switch for memory efficient alternatives:
  # size of 1 numeric element in bytes
  # doubleSize <- as.numeric(utils::object.size(double(2)) - utils::object.size(double(1)))
  # 1073741824 is 1   GB in bytes
  # 536870912  is 512 MB in bytes
  # the size of an empty matrix is ignored here
  # useMatmult <- ((doubleSize * nobs * nIter) / 536870912.0) <= 1

  # if (useMatmult) {
  if (length(dim(posterior)) == 3L) {
    # we're doing model averaged inference

    # recompute the levelInfo, this is relatively cheap and we cannot be sure that the levelInfo in the
    # posterior was not retrieved from state (and contains too many or too few variables)
    nmodels   <- length(model[["model.list"]])
    randomFactors <- .BANOVAgetRandomFactors(options, model[["analysisType"]])
    dataTypes <- .BANOVAgetDataTypes(dataset, model[["model.list"]][[nmodels]], randomFactors)
    levelInfo <- .BANOVAgetLevelInfo(dataset, model[["model.list"]][[nmodels]], dataTypes)

    # which parameters are nuisance?
    parameterNames <- c("mu", names(levelInfo[["levelNames"]]))
    # the variables that are nuisance, e.g., contBinom
    isNuisanceVar   <- c(TRUE, parameterNames[-1L] %in% model[["nuisance"]])
    names(isNuisanceVar) <- parameterNames
    # the parameters that are nuisance, e.g., contBinom-0 and contBinom-1
    isNuisanceParam <- rep(isNuisanceVar, c(1, lengths(levelInfo[["levelNames"]])))
    names(isNuisanceParam) <- c("mu", unlist(levelInfo[["levelNames"]]))

    if (!all(names(isNuisanceParam) %in% colnames(posterior)))
      stop("!all(names(isNuisance) %in% colnames(posterior)) was not true.")

    # we cannot use model[["effects"]] because that exludes nuisance parameters, so we rebuild one that doesn't exclude these
    effects <- matrix(FALSE, nmodels, length(isNuisanceParam), dimnames = list(NULL, names(isNuisanceParam)))
    for (i in seq_along(model[["model.list"]])) {
      if (is.null(model[["model.list"]][[i]])) { # implies an intercept-only null model
        cnms <- "mu"
      } else {
        idx <- names(levelInfo[["levelNames"]]) %in% attr(terms(model[["model.list"]][[i]]), "term.labels")
        cnms <- c("mu", unlist(levelInfo[["levelNames"]][idx]))
      }
      effects[i, cnms] <- TRUE#c(TRUE, names(levelInfo[["levelNames"]]) %in% attr(terms(model[["model.list"]][[i]]), "term.labels"))
    }

    # sample from the BMA posterior
    postProbs <- model[["postProbs"]]
    postProbs[is.na(postProbs)] <- 0
    .setSeedJASP(options)

    modelIdx <- sample(length(postProbs), nIter, TRUE, postProbs) # resample the models according to posterior prob
    samples <- matrix(0, nrow = nIter, ncol = ncol(datOneHot), dimnames = list(NULL, names(isNuisanceParam)))

    # resample nuisance parameters
    for (i in which(isNuisanceParam)) {
      .setSeedJASP(options)
      samples[, i] <- .BANOVAreSample(n = nIter, x = posterior[, i, "x"], y = posterior[, i, "y"])
    }

    # resample non nuisance parameters
    for (i in which(!isNuisanceParam)) {
      idx <- names(isNuisanceParam)[i]
      mult <- effects[modelIdx, idx] # how often models with this variable are sampled
      nTemp <- sum(mult)
      if (nTemp > 0L){
        .setSeedJASP(options)
        samples[mult, i] <- .BANOVAreSample(n = sum(mult), x = posterior[, i, "x"], y = posterior[, i, "y"])
      }
    }

    preds <- tcrossprod(datOneHot, samples)

  } else {
    # TODO: ensure that column order is always correct for both!
    # otherwise we're doing inference for a single model
    preds <- tcrossprod(datOneHot, posterior)
  }

  # calculate residuals (correctly recycles dat[[dvs]])
  resid <- dataset[[dvs]] - preds

  rsq <- .BANOVAcomputeRsq(dataset[[dvs]], preds)
    # eta <- .BANOVAcomputeEtasq(dat[[dvs]], preds)

  # } else {
    # slow but memory efficient stuff


  # }

  return(list(resid = resid, preds = preds, rsq = rsq))
}


# HF computation ----
.BANOVAinitBayesFactor <- function() {

  defaults <- list(
    BFMaxModels         = 50000,
    BFpretestIterations = 100,
    BFapproxOptimizer   = "optim",
    BFapproxLimits      = c(-15, 15),
    BFprogress          = interactive(),
    BFfactorsMax        = 5
  )
  idx <- setdiff(names(defaults), names(options()))
  options(defaults[idx])

}

.BANOVAcomputeRsq <- function(obs, preds) {

  # NOTE: R^2 != cor(obs, predict) because the predictions from the posterior samples are not OLS estimates.
  if (is.null(dim(preds)))
    preds <- matrix(preds, ncol = 1L)

  # definition from http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2.pdf
  colVars2 <- function(x) {matrixStats::colVars(sweep(x, 2L, matrixStats::colMeans2(x)))}
  ee <- colVars2(preds)
  ff <- colVars2(obs - preds)
  gg <- ee / (ee + ff)
  if (anyNA(gg)) {
    idx <- is.na(gg)
    if (all(idx)) {
      # this means that either the posterior sampling failed or the input data is bogus.
      .quitAnalysis(gettext("All predictions had zero variance. Check the predictors for anomalies or increase the number of posterior samples."))
    } else {
      # this shouldn't happen, but is necessary because otherwise stuff will crash downstream.
      gg[is.na(gg)] <- mean(gg, na.rm = TRUE)
    }
  }
  return(gg)

}

.BANOVAcomputeEtasq <- function(obs, preds) {

  # partial eta^2
  if (is.null(dim(preds)))
    preds <- matrix(preds, ncol = 1L)

  # return(1 - matrixStats::colVars(preds - obs) / var(obs))
  # return(1 - matrixStats::rowVars(preds - obs) / var(obs))
  return(matrixStats::colVars(preds) / var(obs))

}

.BANOVAmakeChainNeater <- function(chains, Xnames, formula, data, dataTypes, gMap, unreduce, continuous, columnFilter) {

  # so BayesFactor:::makeChainNeater doesn't always make things neater
  # see also https://github.com/jasp-stats/jaspAnova/pull/43#discussion_r656430090

  # example where BayesFactor goes wrong:
  # dd <- structure(list(
  #   rm_factor = structure(c(1L, 2L, 1L, 2L, 1L, 2L), .Label = c("Level 1", "Level 2"), class = "factor"),
  #   dependent = c(6, 6, 5, 10, 6, 9),
  #   age = c(59.2161793866222, 59.2161793866222, 53.4457078686802, 53.4457078686802, 50.3248450015473, 50.3248450015473),
  #   subject = structure(c(1L, 1L, 2L, 2L, 3L, 3L), .Label = c("1", "2", "3"), class = "factor")),
  #   row.names = c(NA, 6L), class = "data.frame"
  # )
  #
  # bf_obj_1   <- BayesFactor::lmBF(dependent ~ age + subject,             data = dd, whichRandom = "subject")
  # bf_obj_2   <- BayesFactor::lmBF(dependent ~ age + subject + rm_factor, data = dd, whichRandom = "subject")
  #
  # print(BayesFactor::posterior(bf_obj_1, iterations = 3)) # <- ugly
  # print(BayesFactor::posterior(bf_obj_2, iterations = 3)) # <- good
  #
  # originalFun <- BayesFactor:::makeChainNeater
  # jaspBase:::assignFunctionInPackage(.BANOVAmakeChainNeater, "makeChainNeater", "BayesFactor")
  # print(BayesFactor::posterior(bf_obj_1, iterations = 3)) # <- good
  #
  # jaspBase:::assignFunctionInPackage(originalFun, "makeChainNeater", "BayesFactor")
  # print(BayesFactor::posterior(bf_obj_1, iterations = 3)) # <- ugly

  # this part is identical to BayesFactor:::makeChainNeater
  P = length(gMap)
  nGs = max(gMap) + 1
  factors = BayesFactor:::fmlaFactors(formula, data)[-1]
  dataTypes = dataTypes[names(dataTypes) %in% factors]
  types = BayesFactor:::termTypes(formula, data, dataTypes)
  lastPars = ncol(chains) + (-nGs):0
  if (any(continuous)) {
    gNames = paste("g", c(names(types[types != "continuous"]), "continuous"), sep = "_")
  } else {
    gNames = paste("g", names(types), sep = "_")
  }
  if (is.null(columnFilter)) {
    ignoreCols = ignoreCols = rep(0, P)
  } else {
    ignoreCols = BayesFactor:::filterVectorLogical(columnFilter, names(gMap))
  }
  # here we're also checking here if any dataTypes are "random" instead of only checking for fixed dataTypes
  if (!unreduce | !any(dataTypes %in% c("fixed", "random"))) {
    labels = c("mu", Xnames[!ignoreCols], "sig2", gNames)
    colnames(chains) = labels
    return(chains)
  }
  parLabels = BayesFactor:::makeLabelList(formula, data, dataTypes, unreduce,
                                          columnFilter)
  labels = c("mu", parLabels)
  betaChains = chains[, 1:(ncol(chains) - 2 - nGs) + 1, drop = FALSE]
  betaChains = BayesFactor:::ureduceChains(betaChains, formula, data, dataTypes, gMap, ignoreCols)
  newChains = cbind(chains[, 1], betaChains, chains[, lastPars])
  labels = c(labels, "sig2", gNames)
  colnames(newChains) = labels
  return(newChains)
}

.BANOVAcomputeInteractionsMatrix <- function(effects) {

  neffects <- length(effects)
  interactions.matrix <- matrix(FALSE, nrow = neffects, ncol = neffects)
  rownames(interactions.matrix) <- colnames(interactions.matrix) <- effects
  if (neffects > 1L) {
    effect.components <- sapply(effects, strsplit, split = ":", fixed = TRUE)

    for (e in seq_len(neffects)) {
      interactions.matrix[e, ] <- sapply(1:neffects, function(ee) {
        (sum(effect.components[[e]] %in% effect.components[[ee]]) == length(effect.components[[e]]))
      })
    }
    diag(interactions.matrix) <- FALSE
  }
  return(interactions.matrix)
}

# Model prior ----
.BANOVAcomputePriorModelProbs <- function(models, nuisance, options) {

  if (options[["modelPrior"]] == "uniform") {
    modelprobs <- rep(1 / length(models), length(models))
  } else if (options[["modelPrior"]] == "custom") {

    inclusionProbabilities <- vapply(options[["modelTermsCustomPrior"]], `[[`, FUN.VALUE = numeric(1L), "priorIncl")
    modelprobs <- .BANOVAcustomInclusionProbabilitiesToModelProbabilities(models, nuisance, inclusionProbabilities, enforceMarginality = options[["enforcePrincipleOfMarginalityFixedEffects"]])

  } else {

    noPredictorsPerModel <- vapply(models, function(x) if (is.null(x)) 0L else length(attr(terms(x), "term.labels")), FUN.VALUE = integer(1L))
    noNuisancePredictors <- noPredictorsPerModel[1L]
    noPredictorsPerModel <- noPredictorsPerModel - noNuisancePredictors
    totalNoPredictors    <- noPredictorsPerModel[length(noPredictorsPerModel)]

    if (options[["modelPrior"]] %in% c("beta.binomial", "Wilson", "Castillo")) {

      switch (options[["modelPrior"]],
              "beta.binomial" = {alpha = options[["betaBinomialParamA"]]; beta = options[["betaBinomialParamB"]]                    },
              "Wilson"        = {alpha = 1.0;                             beta = totalNoPredictors * options[["wilsonParamLambda"]] },
              "Castillo"      = {alpha = 1.0;                             beta = totalNoPredictors ^ options[["castilloParamU"]]    }
      )

      modelprobs <- dBetaBinomialModelPrior(noPredictorsPerModel, totalNoPredictors, alpha, beta)

    } else if (options[["modelPrior"]] == "Bernoulli") {
      modelprobs <- dBernoulliModelPrior(noPredictorsPerModel, totalNoPredictors, options[["bernoulliParam"]])
    }

    # both the betabinomial and bernoulli model priors do not sum to 1 when marginality is respected
    modelprobs <- modelprobs / sum(modelprobs)
  }

  return(modelprobs)
}

dBetaBinomialModelPrior <- function(k, n, alpha = 1.0, beta = 1.0, log = FALSE) {
  # NOTE: this is not the pdf of the betabinomial, but the pdf divided by choose(n, k), so that it's the probabiltiy of a particular model
  logprobability <- lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta)
  if (log)
    return(logprobability)
  return(exp(logprobability))
}

dBernoulliModelPrior <- function(k, n, prob = 0.5, log = FALSE) {
  logprobability <- k * log(prob) + (n - k) * log(1 - prob)
  if (log)
    return(logprobability)
  return(exp(logprobability))
}

.BANOVAcustomInclusionProbabilitiesToModelProbabilities <- function(modelList, nuisance, inclusionProbabilities, enforceMarginality = TRUE) {

  # TODO: discuss with the team:
  # when marginality is enforced, how should the prior on interaction effects be interpreted when a user wants matched models?

  formulaFullModel <- modelList[[length(modelList)]]
  termsFullModel <- terms(formulaFullModel)
  termLabels <- attr(termsFullModel, "term.labels")
  fullNonNuisance <- !(termLabels %in% nuisance)
  termLabels  <- termLabels[fullNonNuisance]

  # the principle of marginality is enforced (or not) through the function getPresentInteractions
  if (enforceMarginality) {
    # in this case, interaction effects do not appear without the main effects, so the user provided probability
    # is interpreted as a conditional probability given that the main effects are included.
    # thus, p(interaction) = 0 if the main effects are missing, and the user provided probability otherwise.
    #
    # for example, consider p(A) = P(B) = .5, P(A:B) = .2
    #
    # model        P(model)
    # A            P(A) * (1 - P(B))                # <- here we do NOT do '* (1 - P(A:B))'
    # A, B         p(A) * P(B)       * (1 - P(A:B))
    # A, B, A:B    p(A) * P(B)       * P(A:B)

    termFactors <- attr(termsFullModel, "factors")[, termLabels, drop = FALSE]
    termFactors <- termFactors[rownames(termFactors) %in% termLabels, , drop = FALSE]
    rnmsTermFactors <- rownames(termFactors)
    termOrders  <- attr(termsFullModel, "order")[fullNonNuisance]

    # create a list where the names refer to the interactions and the elements refer to all children of that interaction
    listOfDescendants <- setNames(lapply(termLabels, function(x) {
      idx <- which(termFactors[, x] == 1L)
      rnmsTermFactors[idx]
    }), termLabels)
    termIsNotInteraction <- termOrders == 1L

    getPresentInteractions <- function(currentTermLabels) {
      vapply(listOfDescendants, function(descendants, currentTermLabels) {
        all(descendants %in% currentTermLabels)
      }, logical(1L), currentTermLabels) | termIsNotInteraction
    }

    # NOTE: the loop below fails for some combinations of user priors, but I can see how this behavior is desirable
    # the users provide the raw p(interaction) however, the code below assumes that this is
    # p(interaction | main effects), so to compensate we divide each interaction by the product of the components.
    # however, for some inclusion probabilities of the main effects you get impossible values, for example:
    # p(A_) = 0.1, p(B) = 0.1, P(A*B) = 0.9 => 0.9 / 0.1^2 = 90, which is bigger than 1 and should be impossible.
    # for (i in which(!termIsNotInteraction)) { # loop over all interaction terms
    #   idxSubterms <- which(termLabels %in% listOfDescendants[[termLabels[i]]])
    #   inclusionProbabilities[i] <- inclusionProbabilities[i] / prod(inclusionProbabilities[idxSubterms])
    # }

  } else {
    # in this case, interaction effects do appear without the main effects, so the user provided probability
    # is interpreted in the same way for interaction effects and fixed effects.
    # it's interpreted as an unconditional inclusion probability
    getPresentInteractions <- function(...) {
      rep(TRUE, length(termLabels))
    }
  }

  exclusionProbabilties <- 1 - inclusionProbabilities

  # so terms(y ~ b + a:b) reorders a:b to b:a but terms(y ~ a + a:b) does not...
  termLabelOrder <- strsplit(termLabels, ":", fixed = TRUE)

  modelProbs <- numeric(length(modelList))
  for (i in seq_along(modelList)) {
    if (is.null(modelList[[i]])) {

      excludedTerms <- rep(TRUE, length(termLabels))
      excludedTerms <- excludedTerms & getPresentInteractions(NULL)
      modelProbs[i] <- prod(exclusionProbabilties)

    } else {

      currentTerms <- terms(modelList[[i]])
      currentTermLabels <- setdiff(labels(currentTerms), nuisance)

      currentTermLabels <- .BANOVAorderTermsByKnownOrder(currentTerms, currentTermLabels, termLabelOrder)

      presentInteractions <- getPresentInteractions(currentTermLabels)

      includedTerms <- termLabels %in% currentTermLabels
      excludedTerms <- !includedTerms

      includedTerms <- includedTerms & presentInteractions
      excludedTerms <- excludedTerms & presentInteractions

      modelProbs[i] <- prod(inclusionProbabilities[includedTerms], exclusionProbabilties[excludedTerms])

    }
  }

  return(modelProbs)

}

# Other ----
.BANOVAgetRScale <- function(options, analysisType) {

  if (options[["coefficientsPrior"]] == "rscalesAcrossParameters") {

    rscaleFixed   <- options[["priorFixedEffects"]]
    rscaleRandom  <- options[["priorRandomEffects"]]
    rscaleCont    <- if (analysisType == "ANOVA") "medium" else options[["priorCovariates"]]
    rscaleEffects <- NULL

  } else {

    # NOTE: this still need a value otherwise BayesFactor returns NA for the bf
    # default values of lmBF
    rscaleFixed   <- "medium"
    rscaleRandom  <- "nuisance"
    rscaleCont    <- if (analysisType == "ANOVA") "medium" else options[["priorCovariates"]]

    rscaleEffectsNames <- vapply(options[["modelTermsCustomPrior"]], FUN.VALUE = character(1L), function(x) {
      paste(x[["components"]], collapse = ":")
    })
    rscaleEffects <- vapply(options[["modelTermsCustomPrior"]], FUN.VALUE = numeric(1L), `[[`, "rscaleFixed")

    rscaleEffectsKeep <- if (analysisType == "ANOVA") {
      rep(TRUE, length(rscaleEffects))
    } else {
      vapply(options[["modelTermsCustomPrior"]], FUN.VALUE = logical(1L), function(x) {
        is.null(options[["covariates"]]) || !(x[["components"]] %in% options[["covariates"]])
      })
    }

    rscaleEffects <- rscaleEffects[rscaleEffectsKeep]
    names(rscaleEffects) <- rscaleEffectsNames[rscaleEffectsKeep]

  }
  return(list(rscaleFixed = rscaleFixed, rscaleRandom = rscaleRandom, rscaleCont = rscaleCont, rscaleEffects = rscaleEffects))
}

# # .BANOVAupdateRscales <- function() {
#
#   # for some reason, BayesFactor supports custom rscales for but throws a error if you try to set them for continuous parameters
#   # this temporarily overwrite the stop call in this BayesFactor:::createRscales
#   # the function will throw an error if BayesFactor:::createRscales is updated
#
#   # originalCreateRscales <- BayesFactor:::createRscales
#   # originalBody <- body(originalCreateRscales)
#   # if (!identical(originalBody[[17]][[3]][[2]][[1]], quote(invisible))) {
#   #   if (!identical(originalBody[[17]][[3]][[2]][[1]], quote(stop)))
#   #     stop("Bayesfactor got an update, and createRscales/ .BANOVAupdateRscales needs to be fixed!", domain = NA)
#   #   originalBody[[17L]][[3L]][[2L]][[1L]] <- substitute(invisible)
#   #   newCreateRscales <- originalCreateRscales
#   #   body(newCreateRscales) <- originalBody
#   #
#   #   jaspBase::assignFunctionInPackage(newCreateRscales, "createRscales", "BayesFactor")
#   # }
#   #
#   # tmp <- methods::getMethod(BayesFactor::compare, list(numerator="BFlinearModel", denominator="missing", data="data.frame"))
#   # fun <- tmp@.Data
#   # newfun <- fun
#   # body <- body(fun)
#   # if (!identical(body[[19L]][[2L]][[2L]][[4L]][[2L]], quote(FALSE))) {
#   #   if (!identical(body[[19L]][[2L]][[2L]][[4L]][[2L]], quote(all(relevantDataTypes == "continuous"))))
#   #     stop("Bayesfactor got an update, and BayesFactor::`.__T__compare:BayesFactor`$`BFlinearModel#missing#data.frame`/ .BANOVAupdateRscales needs to be fixed!", domain = NA)
#   #   body[[19L]][[2L]][[2L]][[4L]][[2L]] <- substitute(FALSE) # do not take the fast path
#   #   body(newfun) <- body
#   #
#   #   # horrible but necessary
#   #   env <- environment()#getNamespace(jaspAnova)
#   #   assign("compare", BayesFactor::compare, envir = env)
#   #   setMethod("compare", signature(numerator = "BFlinearModel", denominator = "missing", data = "data.frame"), newfun, where = env)
#
#     # env <- getNamespace("BayesFactor")
#     # unlockBinding("compare", env)
#
#     # debugonce(setMethod)
#     # setMethod("compare", signature(numerator = "BFlinearModel", denominator = "missing", data = "data.frame"), newfun, where = env)
#
#     # tmp@.Data <- newfun
#     # s4env <- BayesFactor:::`.__T__compare:BayesFactor`
#     # unlockBinding("BFlinearModel#missing#data.frame", s4env)
#     # s4env$`BFlinearModel#missing#data.frame` <- tmp
#     # lockBinding("BFlinearModel#missing#data.frame", s4env)
#     # jaspBase::assignFunctionInPackage(s4env, ".__T__compare:BayesFactor", "BayesFactor")
#   # }
#
#   # return()
#
#   # we could reset the changes with on.exit, but I'm not sure if it's necessary and doing that cleanly requires withr::defer and adds a dependency
#   # do.call(on.exit({
#   #   jaspBase::assignFunctionInPackage(tempRscales[["originalCreateRscales"]], "createRscales", "BayesFactor")
#   #
#   #   s4env <- BayesFactor:::`.__T__compare:BayesFactor`
#   #   unlockBinding("BFlinearModel#missing#data.frame", s4env)
#   #   s4env$`BFlinearModel#missing#data.frame` <- newfun
#   #   lockBinding("BFlinearModel#missing#data.frame", s4env)
#   #   jaspBase::assignFunctionInPackage(s4env, ".__T__compare:BayesFactor", "BayesFactor")
#   # }, add = TRUE, after = FALSE)
# }

# Dependencies ----
.BANOVAdataDependencies <- function() {
  # seed is in here because basically everything depends on the seed
  c("dependent", "randomFactors", "covariates", "fixedFactors", "betweenSubjectFactors", "repeatedMeasuresFactors", "repeatedMeasuresCells", "modelTerms",
    "seed", "setSeed")
}
.BANOVAmodelSpaceDependencies <- function(modelPrior) {
  c("modelPrior", "betaBinomialParamA", "betaBinomialParamB", "wilsonParamLambda", "castilloParamU", "enforcePrincipleOfMarginalityFixedEffects", "enforcePrincipleOfMarginalityRandomSlopes",
    if (modelPrior == "custom") "modelTermsCustomPrior"
  )
}
.BANOVArscaleDependencies <- function(coefficientsPrior) {
  c("priorFixedEffects", "priorRandomEffects", "priorCovariates", "coefficientsPrior",
    if (coefficientsPrior == "rscalesPerTerm") "modelTermsCustomPrior"
  )
}

.BANOVAmodelPriorOptionsChanged <- function(state, options) {
  !identical(state[["modelPriorOptions"]][.BANOVAmodelSpaceDependencies(options[["modelPrior"]])], options[.BANOVAmodelSpaceDependencies(options[["modelPrior"]])])
}

.BANOVAmodelBFTypeOrOrderChanged <- function(state, options) {
  nms <- c("fixedFactors", "modelTerms", "randomFactors", "covariates", "seed", "setSeed")
  identical(state[nms], options[nms])
}


# HF formulas ----
.BANOVAgetFormulaComponents <- function(x, what = c("components", "variables")) {
  what <- match.arg(what)
  if (what == "components") {
    return(colnames(attr(terms(x), "factors")))
  } else {
    return(all.vars(x)[-1L])
  }
}

.BANOVAgetModelTitlesWithAllTerms <- function(modelObjects, modelList, analysisType, hideNuisance) {

  if (hideNuisance)
    return(vapply(modelObjects, FUN = `[[`, FUN.VALUE = character(1L), "title"))

  nullModelName <- gettext("Null model")
  res <- gsub("(.*)~\\s+", "", vapply(modelList, function(x) if (is.null(x)) nullModelName else .BANOVAas.character.formula(x), character(1L)))
  if (analysisType == "RM-ANOVA")
    res <- gsub(.BANOVAsubjectName, "subject", res)
  res[res == ""] <- nullModelName
  return(jaspBase::gsubInteractionSymbol(res))
}

.BANOVAgenerateAllModelFormulas <- function(formula, nuisance = NULL, analysisType = "RM-ANOVA",
                                            enforcePrincipleOfMarginalityFixedEffects = TRUE,
                                            enforcePrincipleOfMarginalityRandomSlopes = FALSE,
                                            rmFactors = NULL, legacy = FALSE
                                            ) {

  # TODO: it might be nicer to represent the NULL model as
  # y ~ 1, rather than NULL. Right now the null model is a formula
  # when there are nuisance variables and otherwise NULL, which leads to a bunch
  # of annoying if (is.null(model[i])) exceptions.

  neverExclude <- paste0("^", nuisance, "$")
  if (!legacy && analysisType == "RM-ANOVA" && is.null(rmFactors))
    stop(".BANOVAgenerateAllModelFormulas called with invalid arguments: analysisType = \"RM-ANOVA\", legacy = FALSE, rmFactors = NULL", domain = NA)

  modelSpace <- try(BayesFactor::enumerateGeneralModels(
    formula,
    whichModels  = if (enforcePrincipleOfMarginalityFixedEffects) "withmain" else "all",
    neverExclude = neverExclude)
  )
  nuisanceRandomSlopes <- NULL

  if (isTryError(modelSpace))
    .quitAnalysis(gettextf("The following error occured in BayesFactor::enumerateGeneralModels: %s",
                           .extractErrorMessage(modelSpace)))

    if (analysisType == "RM-ANOVA" && !legacy) {
    # adjust the nuisance to include the random slopes, only done here because BayesFactor::enumerateGeneralModels
    # does not handle this properly

    allPossibleSlopes <- labels(stats::terms(stats::as.formula(paste("y~", paste(rmFactors, collapse = "*")))))
    # drop the most complex interaction
    allPossibleSlopes <- allPossibleSlopes[-length(allPossibleSlopes)]

    # if at least one interaction among the repeated measures is considered
    if (length(allPossibleSlopes) > 0L) {

      # add interaction with subject
      nuisanceRandomSlopes <- paste0(allPossibleSlopes, ":", .BANOVAsubjectName)

      if (enforcePrincipleOfMarginalityRandomSlopes) {

        for (i in seq_along(modelSpace)) {
          presentLabels <- labels(stats::terms(modelSpace[[i]]))
          termsToAddChar <- intersect(allPossibleSlopes, presentLabels)
          if (length(termsToAddChar) > 0L) {
            termsToAddChar <- paste0(termsToAddChar, ":", .BANOVAsubjectName)
            termsToAdd <- as.formula(paste("~ . +", paste0(termsToAddChar, collapse = " + ")))
            modelSpace[[i]] <- update.formula(modelSpace[[i]], new = termsToAdd)
          }
        }

      } else {

        termsToAdd <- as.formula(paste("~ . +", paste0(nuisanceRandomSlopes, collapse = " + ")))
        modelSpace <- lapply(modelSpace, update.formula, new = termsToAdd)
      }

      # for the reordering done below. termsToAdd always contains the most complex random effects
      formula    <- update.formula(formula, new = termsToAdd)

      # update nuisance
      nuisance <- c(nuisance, nuisanceRandomSlopes)

    }
  }

  if (is.null(nuisance)) {
    return(list(modelList = c(list(NULL), modelSpace), nuisance = nuisance, nuisanceRandomSlopes = nuisanceRandomSlopes))
  } else {
    # put the null-model first
    i <- length(modelSpace)
    modelSpace <- c(modelSpace[[i]], modelSpace[-i])

    # BayesFactor has the nasty habit of changing the order of interactions whenever one of the components is
    # a nuisance variable. Here we ensure the order matches that of the input formula (which matches the order a user entered the factors).
    # see also https://github.com/jasp-stats/jasp-test-release/issues/1181
    originalTerms  <- terms(formula)
    originalOrd    <- attr(originalTerms, "order")
    originalIdxNonInteraction <- which(originalOrd == 1L)

    originalLabels <- attr(originalTerms, "term.labels")
    originalLabelsPieces <- strsplit(originalLabels[originalOrd > 1L], ":")
    originalLabelsPiecesSorted <- lapply(originalLabelsPieces, sort)

    dependent <- all.vars(modelSpace[[1]])[1L]

    for (i in seq_along(modelSpace)) {

      term <- terms(modelSpace[[i]])
      ord  <- attr(term, "order") # interaction order
      idxNonInteraction <- which(ord == 1L)

      termLabels <- attr(term, "term.labels")
      termLabels[idxNonInteraction] <- originalLabels[originalIdxNonInteraction][match(termLabels[idxNonInteraction], originalLabels[originalIdxNonInteraction])]

      for (j in which(ord > 1L)) {
        labelPieces <- strsplit(termLabels[j], ":")[[1L]]
        for (k in seq_along(originalLabelsPieces))
          if (all(sort(labelPieces) == originalLabelsPiecesSorted[[k]]))
            termLabels[j] <- paste(originalLabelsPieces[[k]], collapse = ":")
      }

      modelSpace[[i]] <- as.formula(paste(dependent, "~", paste(termLabels, collapse = " + ")))

    }

    return(list(modelList = modelSpace, nuisance = nuisance, nuisanceRandomSlopes = nuisanceRandomSlopes))
  }
}

.BANOVAcreateModelFormula <- function(dependent, modelTerms) {

  rhs <- "" # right hand side of the formula
  nuisance <- NULL
  effects <- NULL
  for (term in modelTerms) {

    comp <- term[["components"]]
    newValue <- paste(comp, collapse = ":")

    rhs <- if (rhs != "") paste0(rhs, " + ", newValue) else newValue

    if (!is.null(term[["isNuisance"]]) && term[["isNuisance"]]) {
      nuisance <- c(nuisance, newValue)
    } else {
      effects <- c(effects, newValue)
    }
  }
  model.formula <- formula(paste(dependent, "~", rhs))

  # this would be cleaner ideal if BayesFactor::enumerateGeneralModels would handle the nuisance properly.
  # if (isRMANOVA && !legacy) {
  #   randomSlopes <- paste0(rmFactors, ":", .BANOVAsubjectName)
  #   termsToAdd <- as.formula(paste("~ . +", paste0(randomSlopes, collapse = " + ")))
  #   model.formula <- update.formula(model.formula, termsToAdd)
  #   nuisance <- c(nuisance, randomSlopes)
  # }

  return(list(model.formula = model.formula, nuisance = nuisance, effects = effects))
}

.BANOVAgetModelFormulaFromBFobj <- function(BayesFactorObj, asCharacter = FALSE) {
  out <- BayesFactorObj$bf@numerator[[1L]]@identifier$formula
  if (asCharacter) {
    return(out)
  } else {
    return(as.formula(out))
  }
}

.BANOVAgetLevelInfo <- function(dataset, formula, dataTypes) {

  levelNames <- .BANOVAmakeLabelList(formula, dataset, dataTypes)
  levelCounts <- lengths(levelNames) # counts of the factors including interaction terms
  return(list(levelCounts = levelCounts, levelNames = levelNames))
}

.BANOVAmakeLabelList <- function(formula, data, dataTypes, unreduce = TRUE, columnFilter = NULL) {

  # from BayesFactor 0.9.12.4.2
  # identical to BayesFactor:::makeLabelList, except that we return a named list
  # with
  #   names:  the variables that make up a particular (interaction) effect
  #   values: the parameter names as returned by BayesFactor::posterior(...)

  terms = attr(terms(formula, data = data), "term.labels")
  if (!is.null(columnFilter))
    terms = terms[!BayesFactor:::filterVectorLogical(columnFilter, terms)]

  if (unreduce)
    dataTypes[dataTypes == "fixed"] = "random"

  labelList = lapply(terms, function(term, data, dataTypes) {
    effects = strsplit(term, ":", fixed = TRUE)[[1]]
    my.names = BayesFactor:::design.names.intList(effects, data, dataTypes)
    return(paste(term, "-", my.names, sep = ""))
  }, data = data, dataTypes = dataTypes)

  # this part is different from BayesFactor
  names(labelList) <- terms
  return(labelList)
}

.BANOVAgetDataTypes <- function(dataset, formula, whichRandom = NULL) {

  # from BayesFactor:::lmBF
  BayesFactor:::createDataTypes(
    formula,
    whichRandom = whichRandom,
    data = dataset,
    analysis = "lm"
  )
}

.BANOVAgetRandomFactors <- function(options, analysisType) {
  if (analysisType == "RM-ANOVA")
    return(.BANOVAsubjectName)
  else
    return(unlist(options[["randomFactors"]]))
}

.BANOVAgetLevelsFromParamNames <- function(names) {

  # NOTE: this works because base64 does not contain "-"
  # split on first "-"; 2 implies output of length 2, i.e., only split once
  out <- do.call(rbind, stringr::str_split(names, "-", 2L))

  # continous variables have as level name the variable name
  idx <- out[, 1L] == out[, 2L]
  out[idx, 2L] <- ""

  # change dots into spaces for aesthetic purposes
  out[, 2L] <- gsub(".", " ", out[, 2L], fixed = TRUE)
  colnames(out) <- c("parameter", "level")
  return(out)

}

.BANOVAas.character.formula <- function(x, ...) {
  # we could also extend the S3 function as.character
  Reduce(paste, trimws(deparse(x)))
}

.BANOVAreorderFormulas <- function(x) {

  # This function reorders the terms of a formula such that they are alphabetical
  # e.g., a ~ c + b + c:b becomes a ~ b + b:c
  # This is necessary because BayesFactor::enumerateGeneralModels always appends the nuisance terms
  # and a ~ b + c != a ~ c + b
  # so without this function the state does not get reused whenever a user modifies the nuisance terms
  if (is.null(x))
    return("NULL") # as a string so it can become one big character vector for match()

  s <- strsplit(attr(stats::terms.formula(x), "term.labels"), ":")
  for (i in which(lengths(s) > 1L))
    s[[i]] <- paste0(sort(s[[i]]), collapse = ":")
  return(paste(all.vars(x)[1L], "~", paste(sort(unlist(s)), collapse = " + ")))
}

.BANOVAreorderTerms <- function(x) {

  # This function reorders the terms of a formula such that they are alphabetical
  # it essentially does the same as .BANOVAreorderFormulas but expects x to be character
  # x should be a character string of the form c("a" "a:b"), so not a ~ b + c:d.
  # For example, output from `all.vars(formula)` or `.BANOVAgetFormulaComponents(formula)`.
  s <- strsplit(x, ":")
  for (i in which(lengths(s) > 1L))
    s[[i]] <- paste0(sort(s[[i]]), collapse = ":")
  return(unlist(s))
}

.BANOVAorderTermsByKnownOrder <- function(currentTerms, currentTermLabels, termLabelOrder) {

  # This function reorders the terms of a formula such that they follow the order of termLabelOrder
  # currentTerms:      output of stats::terms(formula)
  # currentTermLabels: output of labels(stats::formula(formula)), possibly after filtering out nuisance
  # termLabelOrder:    character vector of term labels in the desired order

  currentFactors <- attr(currentTerms, "factors")
  if (any(currentFactors > 1L)) { # TRUE implies may need to reorder some terms according to termLabelOrder
    for (j in grep(":", currentTermLabels, fixed = TRUE)) {
      tmp <- strsplit(currentTermLabels[j], ":", fixed = TRUE)[[1L]]

      for (order in termLabelOrder) {
        if (all(tmp %in% order) && length(tmp) == length(order)) {
          currentTermLabels[j] <- paste(order, collapse = ":")
          break
        }
      }
    }
  }
  return(currentTermLabels)
}

# Single Model Inference (SMI) ----
.BANOVAsmi <- function(jaspResults, dataset, options, model) {

  userWantsSMI <- any(unlist(options[c(
    "singleModelPosteriorPlot", "singleModelqqPlot", "singleModelrsqPlot", "singleModelEstimates", "singleModelCriTable"
  )]))
  if (!userWantsSMI)
    return()

  if (!is.null(jaspResults[["containerSingleModel"]])) {
    singleModelContainer <- jaspResults[["containerSingleModel"]]
  } else {
    singleModelContainer <- createJaspContainer(title = gettext("Single Model Inference"))
    singleModelContainer$dependOn(c(
      "singleModelTerms", "dependent", "sampleModeMCMC", "fixedMCMCSamples",
      "repeatedMeasuresCells", "seed", "setSeed",
      .BANOVArscaleDependencies(options[["coefficientsPrior"]])
    ))

    jaspResults[["containerSingleModel"]] <- singleModelContainer
    singleModelContainer$position <- 7
  }

  singleModel <- jaspResults[["singleModelState"]]$object
  if (!is.null(model[["models"]]) && is.null(singleModel) && length(options$singleModelTerms) > 0L && userWantsSMI) {
    singleModel <- try(.BANOVAsmiSamplePosterior(dataset, options, model[["analysisType"]]))
    if (isTryError(singleModel)) {
      singleModelContainer$setError(gettextf("Error in single model inference:\n%s", singleModel))
      singleModel <- NULL
    } else {
      singleModelState <- createJaspState(object = singleModel)
      singleModelState$dependOn(optionsFromObject = singleModelContainer)
      jaspResults[["singleModelState"]] <- singleModelState
    }
  }

  .BANOVAsmiEstimates(singleModelContainer, options, singleModel)
  .BANOVAsmiRsquared (singleModelContainer, options, singleModel)
  .BANOVAsmiQqPlot   (singleModelContainer, options, singleModel)
  .BANOVAsmiRsqPlot  (singleModelContainer, options, singleModel)

  .BANOVAsmiPosteriorPlot(singleModelContainer, dataset, options, singleModel)

  return()

}

.BANOVAsmiSamplePosterior <- function(dataset, options, analysisType) {

  nIter <- if (options[["sampleModeMCMC"]] == "auto") 1e3L else options[["fixedMCMCSamples"]]
  modelTerms <- options$singleModelTerms

  modelTerms    <- options$modelTerms
  dependent     <- options$dependent
  if (analysisType == "RM-ANOVA") {
    modelTerms[[length(modelTerms) + 1L]] <- list(components = .BANOVAsubjectName, isNuisance = TRUE)
    dependent <- .BANOVAdependentName
  }

  tempRScale    <- .BANOVAgetRScale(options, analysisType)
  rscaleFixed   <- tempRScale[["rscaleFixed"]]
  rscaleRandom  <- tempRScale[["rscaleRandom"]]
  rscaleCont    <- tempRScale[["rscaleCont"]]
  rscaleEffects <- tempRScale[["rscaleEffects"]]

  formula <- .BANOVAcreateModelFormula(dependent, modelTerms)$model.formula
  randomFactors <- .BANOVAgetRandomFactors(options, analysisType)
  dataTypes     <- .BANOVAgetDataTypes(dataset, formula, randomFactors)
  levelInfo     <- .BANOVAgetLevelInfo(dataset, formula, dataTypes)
  allParamNames <- c("mu", unlist(levelInfo$levelNames))

  .setSeedJASP(options)
  samples <- BayesFactor::lmBF(
    formula       = formula,
    data          = dataset,
    whichRandom   = unlist(randomFactors),
    progress      = TRUE,
    posterior     = TRUE,
    rscaleFixed   = rscaleFixed,
    rscaleRandom  = rscaleRandom,
    rscaleCont    = rscaleCont,
    rscaleEffects = rscaleEffects,
    iterations    = nIter
  )

  types <- samples@model@dataTypes

  # keep only relevant columns, drop sig2, g_xxx, ...
  idx <- match(allParamNames, colnames(samples), nomatch = 0L)
  samples <- samples[, idx, drop = FALSE]

  # for some odd reason, Bayesfactor uses as column name contcor1-contcor1
  # if there is a covariate AND fixed factors, but only contcor1 if all variables are continuous...
  allContinuous <- all(types == "continuous")
  if (allContinuous) {
    cnms <- colnames(samples)[-1L] # omit the intercept (mu) which is not changed by Bayesfactor
    colnames(samples)[-1L] <- paste0(cnms, "-", cnms)
  }

  # find out which parameters are random
  isRandom <- logical(ncol(samples))
  idx <- which(types == "random")
  for (i in idx)
    isRandom <- isRandom | startsWith(colnames(samples), names(types)[i])

  means <- colMeans(samples)
  sds <- matrixStats::colSds(samples)
  names(means) <- names(sds) <- colnames(samples)

  h <- (1 - options[["credibleInterval"]]) / 2
  probs <- c(h, 1-h)
  cri <- matrixStats::colQuantiles(samples, probs = probs)

  densities <- .BANOVAfitDensity(samples, 2^9, FALSE)

  tmp  <- .BANOVAgetSMIResidRsq(samples, dataset, formula, nIter, options = options)
  residmeans  <- rowMeans(tmp$resid)
  residquants <- matrixStats::rowQuantiles(tmp$resid, probs = c(h, 1-h))

  # all information for q-q plot of residuals
  residSumStats <- matrix(c(residmeans, residquants), nrow = length(residmeans), ncol = 3L,
                          dimnames = list(NULL, c("mean", "cri.2.5%", "cri.97.5%")))

  # all information for r-squared density plot
  rsqDens <- density(tmp$rsq, n = 2^11, from = 0, to = 1)
  rsqCri <- quantile(tmp$rsq, probs = probs)
  rsqMean <- mean(tmp$rsq)

  return(list(
    means = means, sds = sds, CRIs = cri, densities = densities$fit,
    residSumStats = residSumStats, rsqDens = rsqDens, rsqCri = rsqCri, rsqMean = rsqMean,
    allContinuous = allContinuous, isRandom = isRandom
  ))
}

.BANOVAsmiEstimates <- function(jaspContainer, options, model) {

  if (!is.null(jaspContainer[["SMItablePosteriorEstimates"]]) || !options[["singleModelEstimates"]])
    return()

  estsTable <- createJaspTable(title = gettext("Single Model Posterior Summary"))
  estsTable$position <- 1
  estsTable$dependOn(c("singleModelEstimates", "credibleInterval"))

  overTitle <- gettextf("%s%% Credible Interval", format(100 * options[["credibleInterval"]], digits = 3))
  estsTable$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")
  estsTable$addColumnInfo(name = "Level",    title = gettext("Level"),    type = "string")
  estsTable$addColumnInfo(name = "Mean",     title = gettext("Mean"),     type = "number")
  estsTable$addColumnInfo(name = "SD",       title = gettext("SD"),       type = "number")
  estsTable$addColumnInfo(name = "Lower",    title = gettext("Lower"),    type = "number", overtitle = overTitle)
  estsTable$addColumnInfo(name = "Upper",    title = gettext("Upper"),    type = "number", overtitle = overTitle)

  if (!(is.null(model) || estsTable$getError())) {
    .BANOVAfillEstimatesTable(
      jaspTable   = estsTable,
      mus         = model$means,
      sds         = model$sds,
      cri         = model$CRIs,
      hasNoLevels = model$allContinuous,
      isRandom    = model$isRandom
    )
  }
  jaspContainer[["SMItablePosteriorEstimates"]] <- estsTable
  return()

}

.BANOVAsmiPosteriorPlot <- function(jaspContainer, dataset, options, model) {

  # meta wrapper for model averaged posterior plots, single model posterior plots, and Q-Q plots
  if (!is.null(jaspContainer[["SMIposteriorPlot"]]) || !options$singleModelPosteriorPlot)
    return()

  posteriorPlotContainer <- createJaspContainer(title = gettext("Single Model Posterior Distributions"))
  jaspContainer[["SMIposteriorPlot"]] <- posteriorPlotContainer
  posteriorPlotContainer$position <- 2
  posteriorPlotContainer$dependOn(c("singleModelPosteriorPlot", "singleModelGroupPosterior"))
  if (is.null(model) || posteriorPlotContainer$getError()) {
    posteriorPlotContainer[["dummyplot"]] <- createJaspPlot(title = gettext("Posterior distribution"), width = 400, height = 400,
                                                            plot = NULL)
  } else {
    .BANOVAfillPosteriorPlotContainer(
      container       = posteriorPlotContainer,
      densities       = model$densities[, -1L, ], # omit intercept
      cris            = model$CRIs[-1L, ],        # omit intercept
      isRandom        = model$isRandom[-1L],      # omit intercept
      groupParameters = identical(options[["groupPosterior"]], "grouped")
    )
  }
  return()
}

.BANOVAsmiQqPlot <- function(jaspContainer, options, model) {

  if (!is.null(jaspContainer[["QQplot"]]) || !options$singleModelqqPlot)
    return()

  if (is.null(model) || jaspContainer$getError()) {
    p <- NULL
  } else {
    p <- jaspGraphs::plotQQnorm(
      residuals = model$residSumStats[,"mean"],
      lower     = model$residSumStats[,"cri.2.5%"],
      upper     = model$residSumStats[,"cri.97.5%"]
    )
  }
  plot <- createJaspPlot(
    title       = gettext("Q-Q Plot"),
    width       = 400,
    height      = 400,
    plot        = p,
    aspectRatio = 1
  )
  plot$dependOn("singleModelqqPlot")
  plot$position <- 3
  jaspContainer[["QQplot"]] <- plot
  return()
}

.BANOVAsmiRsqPlot <- function(jaspContainer, options, model) {

  if (!is.null(jaspContainer[["smirsqplot"]]) || !options$singleModelrsqPlot)
    return()

  if (is.null(model) || jaspContainer$getError()) {
    p <- NULL
  } else {
    dd     <- model$rsqDens
    rsqCri <- model$rsqCri
    df     <- data.frame(x = dd$x, y = dd$y)
    xName <- expression(R^2)
    p <- jaspGraphs::PlotPriorAndPosterior(dfLines = df, xName = xName, CRI = rsqCri, drawCRItxt = FALSE)
  }
  plot <- createJaspPlot(
    title       = gettextf("Posterior R%s", "\u00B2"),
    width       = 400,
    height      = 400,
    plot        = p,
    aspectRatio = 1
  )
  plot$dependOn("singleModelrsqPlot")
  plot$position <- 4
  jaspContainer[["smirsqplot"]] <- plot
  return()
}

.BANOVAsmiRsquared <- function(jaspResults, options, model) {

  if (!is.null(jaspResults[["tableSMICRI"]]) || !options[["singleModelCriTable"]])
    return()

  criTable <- createJaspTable(title = gettextf("Single Model R%s", "\u00B2"))
  criTable$position <- 3.5
  criTable$dependOn(c("singleModelCriTable", "credibleInterval"))

  overTitle <- gettextf("%s%% Credible Interval", format(100 * options[["credibleInterval"]], digits = 3))
  criTable$addColumnInfo(name = "rsq",   title = "",               type = "string")
  criTable$addColumnInfo(name = "Mean",  title = gettext("Mean"),  type = "number")
  criTable$addColumnInfo(name = "Lower", title = gettext("Lower"), type = "number", overtitle = overTitle)
  criTable$addColumnInfo(name = "Upper", title = gettext("Upper"), type = "number", overtitle = overTitle)

  if (!is.null(model)) {
    cri <- model[["rsqCri"]]
    df <- data.frame(
      rsq   = "R\u00B2",
      Mean  = model[["rsqMean"]],
      Lower = cri[1L],
      Upper = cri[2L],
      row.names = NULL
    )
    criTable$setData(df)
  } else {
    criTable[["rsq"]] <- "R\u00B2"
  }
  jaspResults[["tableSMICRI"]] <- criTable
  return()

}

# Citations ----
.BANOVAcitations <- c(
  "MoreyEtal2015"    = "Morey, R. D. & Rouder, J. N. (2015). BayesFactor (Version 0.9.10-2)[Computer software].",
  "RouderEtal2012"   = "Rouder, J. N., Morey, R. D., Speckman, P. L., Province, J. M., (2012) Default Bayes Factors for ANOVA Designs. Journal of Mathematical Psychology. 56. p. 356-374.",
  "Jeffreys1938"     = "Jeffreys, H. (1938). Significance tests when several degrees of freedom arise simultaneously. Proceedings of the Royal Society of London. Series A, Mathematical and Physical Sciences, 165, 161-198.",
  "WestfallEtal1997" = "Westfall, P. H., Johnson, W. O., & Utts, J. M. (1997). A Bayesian perspective on the Bonferroni adjustment. Biometrika, 84, 419-427."
)
