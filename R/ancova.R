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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

AncovaInternal <- function(jaspResults, dataset = NULL, options) {
  initialGlobalOptions <- options()
  on.exit(options(initialGlobalOptions), add = TRUE)

  numericVariables <- c(unlist(options$dependent),unlist(options$covariates),unlist(options$wlsWeight))
  numericVariables <- numericVariables[numericVariables != ""]
  factorVariables <- c(unlist(options$fixedFactors),unlist(options$randomFactors))
  factorVariables <- factorVariables[factorVariables != ""]
  nFactorModelTerms <- sum(unlist(options$modelTerms) %in% factorVariables)

  ready <- options$dependent != "" && length(options$fixedFactors) > 0 && nFactorModelTerms > 0
  options(contrasts = c("contr.sum","contr.poly"))

  # Set corrections to FALSE when performing ANCOVA
  if (is.null(options$homogeneityCorrectionBrown)) {
    options$homogeneityCorrectionNone <- TRUE
    options$homogeneityCorrectionBrown <- FALSE
    options$homogeneityCorrectionWelch <- FALSE
  }

  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = numericVariables,
                                 columns.as.factor = factorVariables,
                                 exclude.na.listwise = c(numericVariables, factorVariables))
    dataset <- droplevels(dataset)
  }

  anovaContainer <- .getAnovaContainer(jaspResults)

  .anovaCheckErrors(dataset, options, ready)

  .anovaModelContainer(anovaContainer, dataset, options, ready)

  .anovaTable(anovaContainer, options, ready)

  .anovaAssumptionsContainer(anovaContainer, dataset, options, ready)

  .anovaContrastsTable(anovaContainer, dataset, options, ready)

  .anovaOrdinalRestrictions(anovaContainer, dataset, options, ready, analysis = "anova")

  .anovaPostHocTableCollection(anovaContainer, dataset, options, ready)

  .anovaMarginalMeans(anovaContainer, dataset, options, ready)

  .anovaSimpleEffects(anovaContainer, dataset, options, ready)

  .anovaKruskal(anovaContainer, dataset, options, ready)

  .BANOVAdescriptives(anovaContainer, dataset, options, list(noVariables=FALSE), "ANCOVA", ready)

  .anovaExportResiduals(anovaContainer, dataset, options, ready)

  .anovaExportPredictions(anovaContainer, dataset, options, ready)

  return()
}

.getAnovaContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["anovaContainer"]])) {
    anovaContainer <- jaspResults[["anovaContainer"]]
  } else {
    anovaContainer <- createJaspContainer()
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    anovaContainer$dependOn(c("dependent", "modelTerms", "covariates", "sumOfSquares", "wlsWeights", "customContrasts"))
    jaspResults[["anovaContainer"]] <- anovaContainer
  }
  return(anovaContainer)
}

.anovaContrastCases <- function(column, contrastType) {

  levels <- levels(column)
  nLevels <- length(levels)

  cases <- list()

  if (nLevels == 1) {

    cases[[1]] <- "."

  } else {
    switch(contrastType,
      deviation = {

        for (i in 1:(nLevels - 1))
          cases[[i]] <- paste(levels[i + 1], " - ", paste(levels,collapse=", "), sep="")

      },
      simple = {

        for (i in 1:(nLevels - 1))
          cases[[i]] <- paste(levels[i+1], " - ", levels[1], sep="")

      },
      Helmert = {

        for (i in 1:(nLevels - 1))
          cases[[i]] <- paste(levels[i], " - ", paste(levels[-(1:i)], collapse=", "), sep="")

      },
      repeated = {

        for (i in 1:(nLevels - 1))
          cases[[i]] <- paste(levels[i], " - ", levels[i+1], sep="")

      },
      difference = {

        for (i in 1:(nLevels - 1))
          cases[[i]] <- paste(levels[i + 1], " - ", paste(levels[1:i], collapse=", "), sep="")

      },
      polynomial = {

        polyNames <- c("linear", "quadratic", "cubic", "quartic", "quintic", "sextic", "septic", "octic")
        for (i in 1:(nLevels - 1)) {
          if (i <= 8) {
            cases[[i]] <- polyNames[i]
          } else {
            cases[[i]] <- paste("degree", i, "polynomial", sep=" ")
          }
        }
      }
    )
  }

  cases
}

.anovaCheckErrors <- function(dataset, options, ready) {
  if (!ready)
    return()

  modelTerms <- unlist(options$modelTerms, recursive = FALSE)
  factorModelTerms <- options$modelTerms[sapply(modelTerms, function(x) !any(x %in% options$covariates))]
  allComponents <- unique(unlist(lapply(factorModelTerms, `[[`, "components"), use.names = FALSE))

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "factorLevels"),
    infinity.target      = c(options$dependent, options$covariates, allComponents, options$wlsWeights),
    factorLevels.target  = options[["fixedFactors"]],
    factorLevels.amount  = "< 2",
    exitAnalysisIfErrors = TRUE
  )

  nWayInteractions <- unlist(lapply(factorModelTerms, lengths), use.names = FALSE)
  if (any(nWayInteractions > 1L)) {

    # ensure that the largest n-way interaction effects come last
    factorModelTerms <- factorModelTerms[order(nWayInteractions)]

    # For each model term, check if it is a strict subset of another term.
    # For example, if grouping on a three-way interaction doesn't violate any error checks
    # then all the two-way interactions composed of variables from the three-way interaction will pass.
    idxToRemove <- integer()
    for (i in 1:(length(factorModelTerms) - 1)) {
      for (j in length(factorModelTerms):(i + 1)) {

        if (all(factorModelTerms[[i]][["components"]] %in% factorModelTerms[[j]][["components"]])) {
          idxToRemove <- c(idxToRemove, i)
          break
        }

      }
    }
    componentsToGroupOn <- factorModelTerms[-idxToRemove]

  } else {
    componentsToGroupOn <- factorModelTerms
  }

  observations.amount <- paste("<", (length(options[["dependent"]])+all(nWayInteractions == 1L)))
  for(i in rev(seq_along(componentsToGroupOn))) {

    componentsToGroupBy <- componentsToGroupOn[[i]][["components"]]

    .hasErrors(
      dataset              = dataset,
      type                 = c("observations", "variance"),
      all.target           = c(options$dependent, options$covariates),
      observations.grouping= componentsToGroupBy,
      observations.amount  = observations.amount,
      exitAnalysisIfErrors = TRUE
    )

  }

  .hasErrors(dataset = dataset,
             custom = function() {
               if (any(dataset[[.v(options$wlsWeights)]] <= 0))
                 return(gettext("The WLS weights contain negative and/or zero values.<br><br>(only positive WLS weights allowed)."))
             },
             exitAnalysisIfErrors = TRUE)
}

.reorderModelTerms <- function(options) {

  if(length(options$modelTerms) > 0) {

    orderedModelTerms <- options$modelTerms[order(sapply(options$modelTerms, function(x) length(x$components)))]

    fixedFactors <- list()
    covariates <- list()

    k <- 1
    l <- 1

    for(i in 1:length(orderedModelTerms)) {
      if (sum(unlist(orderedModelTerms[[i]]$components) %in% options$covariates) > 0) {
        covariates[[k]] <- orderedModelTerms[[i]]
        k <- k + 1
      } else {
        fixedFactors[[l]] <- orderedModelTerms[[i]]
        l <- l + 1
      }
    }

    if (length(covariates) > length(options$covariates)) {
      modelTerms <- orderedModelTerms
      interactions <- TRUE
    } else {
      modelTerms <- c(fixedFactors, covariates)
      modelTerms <- modelTerms[match(modelTerms, orderedModelTerms)]
      interactions <- FALSE
    }

  } else {

    modelTerms <- list()
    interactions <- FALSE
  }

  list(modelTerms = modelTerms, interactions = interactions)
}

.modelFormula <- function(modelTerms, options) {

  dependent.normal <- options$dependent
  dependent.base64 <- .v(options$dependent)

  terms.base64 <- c()
  terms.normal <- c()

  for (term in modelTerms) {

    components <- unlist(term$components)
    term.base64 <- paste(.v(components), collapse=":", sep="")
    term.normal <- paste(components, collapse=" \u273B ", sep="")

    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }

  model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse="+"))

  list(model.def = model.def, terms.base64 = terms.base64, terms.normal = terms.normal)
}

.anovaModel <- function(dataset, options) {

  afex::afex_options(
    check_contrasts = TRUE, correction_aov = "GG",
    emmeans_model = "univariate", es_aov = "ges", factorize = TRUE,
    lmer_function = "lmerTest", method_mixed = "KR", return_aov = "afex_aov",
    set_data_arg = FALSE, sig_symbols = c(" +", " *", " **", " ***"), type = 3
  )

  reorderModelTerms <-  .reorderModelTerms(options)
  modelTerms <- reorderModelTerms$modelTerms

  modelDef <- .modelFormula(modelTerms, options)
  model.formula <- as.formula(modelDef$model.def)

  WLS <- NULL
  if ( ! is.null(options$wlsWeights))
    WLS <- dataset[[ .v(options$wlsWeights) ]]

  model <- aov(model.formula, dataset, weights=WLS)

  modelError <- try(silent = TRUE, lm(model.formula, dataset, weights=WLS, singular.ok = TRUE))

  # Make afex model for later use in contrasts
  if (!isTryError(modelError)) {
    afexFormula <- as.formula(paste0(modelDef$model.def, "+Error(ID)"))
    afexData <- dataset
    afexData[["ID"]] <- as.factor(1:nrow(dataset))
    afexModel <- try(afex::aov_car(afexFormula, data=afexData, type= 3, factorize = FALSE, include_aov = TRUE))

    if (isTryError(afexModel))
      modelError <- afexModel

  } else {
    afexModel <- NULL
  }


  return(list(model = model, modelError = modelError, afexModel = afexModel))
}

.anovaModelContainer <- function(anovaContainer, dataset, options, ready) {
  if (!ready)
    return()

  # Take results from state if possible
  if (!is.null(anovaContainer[["model"]]))
    return()

  model <- .anovaModel(dataset, options)

  if (isTryError(model$modelError)) {
    if (.extractErrorMessage(model$modelError) == "singular fit encountered") {
      anovaContainer$setError(gettext("Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables"))
      return()
    } else if (.extractErrorMessage(model$modelError) == "residual sum of squares is 0 (within rounding error)") {
      anovaContainer$setError(gettext("Residual sum of squares is 0; this might be due to extremely low variance of your dependent variable"))
      return()
    } else if (grepl(x = .extractErrorMessage(model$modelError), "Rank deficient model matrix")) {
      anovaContainer[["model"]] <- createJaspState(object = model$model)
      return()
    }else {
      anovaContainer$setError(gettextf("An error occurred while computing the ANOVA: %s", .extractErrorMessage(model$modelError)))
      return()
    }
  }

  # Save model to state
  anovaContainer[["model"]] <- createJaspState(object = model$model)
  anovaContainer[["afexModel"]] <- createJaspState(object = model$afexModel)
}

.anovaResult <- function(anovaContainer, options) {

  if (anovaContainer$getError())
    return()

  model <- anovaContainer[["model"]]$object

  reorderModelTerms <-  .reorderModelTerms(options)
  modelTerms <- reorderModelTerms$modelTerms

  modelDef <- .modelFormula(modelTerms, options)
  termsNormal <- modelDef$terms.normal
  termsBase64 <- modelDef$terms.base64

  ## Computation
  modelCoefficients <- model[["coefficients"]]

  if (options$sumOfSquares == "type1") {

    result <- base::tryCatch(stats::anova(model),error=function(e) e, warning=function(w) w)

    if (!is.null(result$message)) {
      anovaContainer$setError(result$message)
      return()
    }

    result['SSt'] <- sum(result[,"Sum Sq"], na.rm = TRUE)

  } else if (options$sumOfSquares == "type2") {

    result <- car::Anova(model, type=2)
    result <- result[result['Df'] > 0, ] # omit terms that cannot be computed due to missingness
    result['Mean Sq'] <- result[['Sum Sq']] / result[['Df']]

  } else if (options$sumOfSquares == "type3") {

    # For each model term, including all interactions, check if there are empty cells
    if (anyNA(modelCoefficients)) {
      anovaContainer$setError(gettext("Your design contains empty cells. Please try a different type of sum of squares or remove an interaction effect from the model."))
      return()
    }

    result <- car::Anova(model, type=3, singular.ok=FALSE)
    result <- result[-1, ]
    result['Mean Sq'] <- result[['Sum Sq']] / result[['Df']]
  }

  # calculate effect sizes before altering results structure
  omegaResult <- effectsize::omega_squared(result, ci = options[["effectSizeCiLevel"]], alternative = "two.sided", partial = FALSE)
  partOmegaResult <- effectsize::omega_squared(result, ci = options[["effectSizeCiLevel"]], alternative = "two.sided", partial = TRUE)
  etaResult <- effectsize::eta_squared(result, ci = options[["effectSizeCiLevel"]], alternative = "two.sided", partial = FALSE)
  partEtaResult <- effectsize::eta_squared(result, ci = options[["effectSizeCiLevel"]], alternative = "two.sided", partial = TRUE)

  # Make sure that the order of the result is same order as reordered modelterms
  result <- result[.mapAnovaTermsToTerms(c(termsBase64, "Residuals"), rownames(result) ), ]
  result[['cases']] <- c(termsNormal[.mapAnovaTermsToTerms(c(termsBase64), rownames(result))], "Residuals")
  result <- as.data.frame(result)

  # see where a new block starts, where block 1 = main, block 2 = two-way inter, block 3 = three-way, etc.
  termCount <- sapply(strsplit(as.character(rownames(result)), ":"), function(x) length(x) - 1)
  newGroupIndices <- c(1, diff(termCount)) != 0
  newGroupIndices[length(newGroupIndices)] <- TRUE
  result[[".isNewGroup"]] <- newGroupIndices

  # Identify indices where changes occur (diffs is not equal to 0)
  change_indices <-
  result[1, 'correction'] <- "None"

  if (options$effectSizeEstimates) {

      # Legacy code (without CI):
      # SSt <- result['SSt']
      # SSr <- result["Residuals", "Sum Sq"]
      # MSr <- SSr/result["Residuals", "Df"]
      #
      # eta <- result[['Sum Sq']] / result[['SSt']]
      # etaPart <- result[['Sum Sq']] / (result[['Sum Sq']] + SSr)
      # omega <- (result[['Sum Sq']] - (result[['Df']] * MSr)) / (SSt + MSr)
      # omega <- sapply(omega[,1], function(x) max(x, 0))

      effectSizeColnames <- paste0(rep(c("eta", "partialEta", "omega", "partialOmega"), each = 3), c("", "Low", "High"))
      relColumns <- c(2, 4:5)
      result[partOmegaResult$Parameter, effectSizeColnames] <- cbind(etaResult[ , relColumns], partEtaResult[, relColumns],
                                                                     omegaResult[, relColumns], partOmegaResult[, relColumns])
      result["Residuals", effectSizeColnames] <- NA

  }

  if (options$vovkSellke) {
    result[["vovkSellke"]] <-  ifelse(result[['Pr(>F)']] != "", VovkSellkeMPR(na.omit(result[['Pr(>F)']])), "")
  }

  if ((options$homogeneityCorrectionBrown || options$homogeneityCorrectionWelch) && length(options$modelTerms) > 1)
    return()

  anovaResult <- list()
  if (options$homogeneityCorrectionNone) {
    anovaResult[["result"]] <- result
  }

  if (options$homogeneityCorrectionBrown) {

    tempResult <- onewaytests::bf.test(as.formula(modelDef$model.def), model$model)
    brownResult <- result
    brownResult[[1, 'correction']] <- "Brown-Forsythe"

    brownResult[[termsBase64, 'Df']] <- tempResult[['parameter']][[1]]
    brownResult[[termsBase64, 'Pr(>F)']] <- tempResult[['p.value']]
    brownResult[[termsBase64, 'F value']] <- tempResult[['statistic']]
    brownResult[['Residuals', 'Df']] <- tempResult[['parameter']][[2]]
    brownResult[['Mean Sq']] <- brownResult[['Sum Sq']] / brownResult[['Df']]

    if (options$vovkSellke) {
      brownResult[['vovkSellke']] <-  ifelse(brownResult[['Pr(>F)']] != "",
                                                VovkSellkeMPR(na.omit(brownResult[['Pr(>F)']])), "")
    }

    anovaResult[['brownResult']] <- brownResult
  }

  if (options$homogeneityCorrectionWelch) {

    tempResult <- stats::oneway.test(as.formula(modelDef$model.def), model$model, var.equal = FALSE)
    welchResult <- result
    welchResult[[1, 'correction']] <- "Welch"

    welchResult[[termsBase64, 'Df']] <- tempResult[['parameter']][[1]]
    welchResult[[termsBase64, 'Pr(>F)']] <- tempResult[['p.value']]
    welchResult[[termsBase64, 'F value']] <- tempResult[['statistic']]
    welchResult[["Residuals", 'Df']] <- tempResult[['parameter']][[2]]
    welchResult[['Mean Sq']] <- welchResult[['Sum Sq']] / welchResult[['Df']]

    if (options$vovkSellke) {
      welchResult[["vovkSellke"]] <-  ifelse(!is.na(welchResult[['Pr(>F)']]),
                                                VovkSellkeMPR(na.omit(welchResult[["Pr(>F)"]])), NA)
    }

    welchFootnote <- NULL
    if (is.na(tempResult[["statistic"]]) && length(options[["fixedFactors"]]) == 1L) {

      # we only do this when length(options[["fixedFactors"]]) == 1L because the table function throws an error when
      # length(options[["fixedFactors"]]) > 1L anyway
      # if the welch correction ever supports more than one fixed factor, the code below already does the 'right thing'
      # by computing the variance for all fixed factors and only the error message needs to be adjusted for multiple
      # factors and levels

      groupVariances <- lapply(options[["fixedFactors"]], function(fixedFactor) {
        vars <- tapply(model[["model"]][[options[["dependent"]]]], model[["model"]][[fixedFactor]], var)
        badLevelNames <- names(vars)[vars == 0]
        if (length(badLevelNames) == 0)
          return(NULL)
        return(badLevelNames)
      })
      names(groupVariances) <- options[["fixedFactors"]]
      groupVariances <- groupVariances[lengths(groupVariances) > 0L]

      welchFootnote <- gettextf(
        "The Welch correction could not be computed because '%1$s' has zero variance after grouping on the following level(s) of '%2$s': %3$s",
        options[["dependent"]], options[["fixedFactors"]], paste(groupVariances[[1L]], collapse = ", ")
      )

    }
    anovaResult[["welchFootnote"]] <- welchFootnote
    anovaResult[['welchResult']]   <- welchResult
  }

  emptyCellFootnote <- NULL
  if (anyNA(modelCoefficients)) {
    emptyCellFootnote <- gettextf(
      "Due to empty cells in the design, only %1$s of %2$s effects are estimable.", sum(!is.na(modelCoefficients)), length(modelCoefficients))
    if ((length(modelTerms)+1) > nrow(result))
      emptyCellFootnote <- paste(emptyCellFootnote, gettext("Some higher-order interaction effects are not supported by the data and are therefore omitted."))
  }
  anovaResult[["emptyCellFootnote"]] <- emptyCellFootnote

  # Save model to state
  anovaContainer[["anovaResult"]] <- createJaspState(object = anovaResult)
  anovaContainer[["anovaResult"]]$dependOn(c("sumOfSquares", "homogeneityCorrectionBrown", "homogeneityCorrectionWelch",
                                             "homogeneityCorrectionNone", "effectSizeEstimates", "effectSizeEtaSquared",
                                             "effectSizePartialEtaSquared", "effectSizeOmegaSquared", "effectSizePartialOmegaSquared",
                                             "effectSizeCi", "effectSizeCiLevel"))
}

.anovaTable <- function(anovaContainer, options, ready) {
  if (!is.null(anovaContainer[["anovaTable"]]))
    return()

  title <- ifelse(is.null(options$covariates), gettext("ANOVA"), gettext("ANCOVA"))
  anovaTable <- createJaspTable(title = title, position = 1,
                           dependencies = c("homogeneityCorrectionWelch", "homogeneityCorrectionBrown", "homogeneityCorrectionNone",
                                            "vovkSellke", "effectSizeEstimates", "effectSizeEtaSquared",
                                            "effectSizePartialEtaSquared", "effectSizeOmegaSquared",  "effectSizePartialOmegaSquared",
                                            "effectSizeCi", "effectSizeCiLevel"))

  corrections <- c("None", "Brown-Forsythe", "Welch")[c(options$homogeneityCorrectionNone,
                                                        options$homogeneityCorrectionBrown,
                                                        options$homogeneityCorrectionWelch)]

  dfType <- "integer" # Make df an integer unless corrections are applied
  if ((length(corrections) > 1 || any(!"None" %in% corrections)) && is.null(options$covariates)) {
    anovaTable$addColumnInfo(title = gettext("Homogeneity Correction"), name = "correction", type = "string")
    dfType <- "number"
  }

  anovaTable$addColumnInfo(title = gettext("Cases"),          name = "cases",   type = "string" )
  anovaTable$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
  anovaTable$addColumnInfo(title = gettext("df"),             name = "Df",      type = dfType)
  anovaTable$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
  anovaTable$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
  anovaTable$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")

  if (options$vovkSellke) {
    anovaTable$addColumnInfo(title = gettextf("VS-MPR%s", "\u002A"), name = "vovkSellke", type = "number")
    anovaTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }

  if (options$effectSizeEstimates) {

    if (options$effectSizeEtaSquared) {
      anovaTable$addColumnInfo(title = "\u03B7\u00B2", name = "eta", type = "number")
      if (options$effectSizeCi) {
        thisOverTitle <- gettextf("%s%% CI for \u03B7\u00B2", options$effectSizeCiLevel * 100)
        anovaTable$addColumnInfo(name="etaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        anovaTable$addColumnInfo(name="etaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }

    if (options$effectSizePartialEtaSquared) {
      anovaTable$addColumnInfo(title = "\u03B7\u00B2\u209A", name = "partialEta", type = "number")
      if (options$effectSizeCi) {
        thisOverTitle <- gettextf("%s%% CI for \u03B7\u00B2\u209A", options$effectSizeCiLevel * 100)
        anovaTable$addColumnInfo(name="partialEtaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        anovaTable$addColumnInfo(name="partialEtaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }

    if (options$effectSizeOmegaSquared) {
      anovaTable$addColumnInfo(title = "\u03C9\u00B2", name = "omega", type = "number")
      if (options$effectSizeCi) {
        thisOverTitle <- gettextf("%s%% CI for \u03C9\u00B2", options$effectSizeCiLevel * 100)
        anovaTable$addColumnInfo(name="omegaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        anovaTable$addColumnInfo(name="omegaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }

    if (options$effectSizePartialOmegaSquared) {
      anovaTable$addColumnInfo(title = "\u03C9\u00B2\u209A", name = "partialOmega", type = "number")
      if (options$effectSizeCi) {
        thisOverTitle <- gettextf("%s%% CI for \u03C9\u00B2\u209A", options$effectSizeCiLevel * 100)
        anovaTable$addColumnInfo(name="partialOmegaLow", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
        anovaTable$addColumnInfo(name="partialOmegaHigh", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
      }
    }

  }

  anovaTable$showSpecifiedColumnsOnly <- TRUE

  # set the type footnote already
  .addSumSquaresFootnote(anovaTable, options)

  anovaContainer[["anovaTable"]] <- anovaTable

  if (!ready || anovaContainer$getError())
    return()

  anovaTable$title <- paste0(title, " - ", options$dependent)

  anovaTable$setExpectedSize(rows = length(options$modelTerms) * length(corrections))

  # here we ask for the model to be computed
  .anovaResult(anovaContainer, options)

  if ((options$homogeneityCorrectionBrown || options$homogeneityCorrectionWelch) && length(options$modelTerms) > 1) {
    anovaTable$setError(gettext("The Brown-Forsythe and Welch corrections are only available for one-way ANOVA"))
    return()
  }

  if (anovaContainer$getError())
    return()

  model <- anovaContainer[["anovaResult"]]$object
  anovaTable$setData(do.call(rbind, model[setdiff(names(model), c("emptyCellFootnote", "welchFootnote"))]))

  if (!is.null(model[["emptyCellFootnote"]]))
    anovaTable$addFootnote(model[["emptyCellFootnote"]])

  if (!is.null(model[["welchFootnote"]]))
    anovaTable$addFootnote(model[["welchFootnote"]])

  return()
}

.anovaContrastsTable <- function(anovaContainer, dataset, options, ready) {
  if (!ready || is.null(options$contrasts))
    return()

  #contrasts are encoded so first decode that so we can later check for things like "none" and "custom"
  decodedContrasts <- list()
  for (i in 1:length(options$contrasts)) {
    options$contrasts[[i]]$decoded <- decodedContrasts[[i]] <- jaspBase::decodeColNames(options$contrasts[[i]]$contrast)
  }

  if (!is.null(anovaContainer[["contrastContainer"]]) || all(grepl("none", decodedContrasts)))
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
      contrastContainer[[contrastContainerName]] <- createJaspContainer()
      contrastContainer[[contrastContainerName]][["contrastTable"]] <- .createContrastTableAnova(myTitle,
                                                                                                 options)
    }

  }

  anovaContainer[["contrastContainer"]] <- contrastContainer

  if (!ready || anovaContainer$getError()) {
    return()
  } else if(is.null(anovaContainer[["afexModel"]]$object)) {
    contrastContainer$setError(gettext("Insufficient data to estimate full model/contrasts. Likely empty cells in between-subjects design (i.e., bad data structure)"))
    return()
  }
  afexModel <- anovaContainer[["afexModel"]]$object

  ## Computation
  model <- anovaContainer[["model"]]$object
  contrastSummary <- summary.lm(model)[["coefficients"]]

  for (contrast in options$contrasts) {

    contrastContainerName <- paste0(contrast$contrast, "Contrast_",  paste(contrast$variable, collapse = ":"))

    if (contrast$decoded != "none") {

      variable <- contrast$variable
      v <- .v(variable)

      if (contrast$decoded == "custom") {
        customContrastSetup <- options$customContrasts[[which(sapply(options$customContrasts,
                                                                     function(x)  all(x$value %in% contrast$variable) &&
                                                                       length(contrast$variable) == length(x$value)))]]
      } else {
        customContrastSetup <- NULL
      }

      if (length(v) == 1) {
        column <- dataset[[ v ]]
      } else {
        column <- factor(apply(dataset[ v ], 1, paste, collapse =", "))
      }

      contrastMatrix    <- .createContrastAnova(column, contrast$decoded, customContrastSetup)

      if (contrast$decoded != "custom") {
        cases <- .anovaContrastCases(column, contrast$decoded)
        contrCoef         <- lapply(as.data.frame(contrastMatrix), as.vector)
        names(contrCoef)  <- cases
      } else {
        contrCoef         <- apply(contrastMatrix, 1, list)
      }

      referenceGrid <- emmeans::emmeans(afexModel, v, model = "multivariate")
      contrastResult    <- try(emmeans::contrast(referenceGrid, contrCoef), silent = TRUE)
      # is input the same as used by emmeans?
      # all(as.matrix( coef(contrastResult)[, -(1:length(v)) ]) == t(contrastMatrix))
      fullModel <- model
      effectSizeResult <- as.data.frame(emmeans::eff_size(referenceGrid,
                                                          sigma = sqrt(mean(sigma(model)^2)),
                                                          edf = df.residual(model),
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

      contrCoef <- coef(contrastResult)
      colnames(contrCoef) <- c(contrast$variable, paste("Comparison", 1: (ncol(contrCoef) - length(contrast$variable))))

      contrastResult <- cbind(contrastResult, confint(contrastResult, level = options$contrastCiLevel)[,5:6])
      contrastResult[["Comparison"]] <- .unv(contrastResult[["contrast"]])
      contrastResult[[".isNewGroup"]] <- c(TRUE, rep(FALSE, nrow(contrastResult)-1))

      contrastResult[["cohenD"]] <- effectSizeResult[["effect.size"]]
      contrastResult[["cohenD_LowerCI"]] <- effectSizeResult[["lower.CL"]]
      contrastResult[["cohenD_UpperCI"]] <- effectSizeResult[["upper.CL"]]


      if (contrast$decoded == "custom" | length(contrast$variable) > 1) {
        contrastResult$Comparison <- 1:nrow(contrastResult)
        weightType <-  if (all(apply(contrastMatrix, 2, function(x) x %% 1 == 0))) "integer" else "number"
        contrastContainer[[contrastContainerName]][["customCoefTable"]] <- .createContrastCoefficientsTableAnova(contrast,
                                                                                                                 contrCoef,
                                                                                                                 weightType)
      }

      contrastContainer[[contrastContainerName]][["contrastTable"]]$setData(contrastResult)

    }

  }

  return()
}

.createContrastAnova <- function (column, contrast.type, customContrast) {

  levels <- levels(column)
  n.levels <- length(levels)

  contr <- NULL

  switch(contrast.type,
         none = {

           options(contrasts = c("contr.sum","contr.poly"))
           contr <- NULL

         },
         deviation = {

           contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
           for (i in 2:n.levels) {
             contr[,(i-1)] <-  -1 / n.levels
             contr[i,(i-1)] <- (n.levels - 1) / n.levels
           }

         },
         simple = {

           contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
           for (i in 1:n.levels-1) {
             contr[c(1,i+1),i]<- c(1,-1) * -1
           }

         },
         Helmert = {

           contr <- contr.helmert(levels)
           contr <- apply(contr, 2, function(x){ x/max(abs(x))})
           contr <- matrix(rev(contr), ncol = ncol(contr), nrow = nrow(contr))

         },
         repeated = {

           contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)

           for (i in 1:(n.levels-1)) {
             contr[i,i] <- 1
             contr[i+1,i] <- -1
           }

         },
         difference = {

           contr <- contr.helmert(levels)
           contr <- apply(contr, 2, function(x){ x/max(abs(x))})

         },
         polynomial = {

           contr <- contr.poly(levels)

         },
         custom = {

           isContrast <- sapply(customContrast[["values"]], function(x) x$isContrast)
           customContrMat <- as.matrix(sapply(customContrast[["values"]][isContrast], function(x) as.numeric(x$values)))
           contr <- t(customContrMat)

         }
  )

  if (! is.null(contr)) {
    dimnames(contr) <- list(NULL, 1:dim(contr)[2])
  }

  contr
}

.postHocContrasts <- function(variableLevels, dataset, options) {

  contrasts <- NULL
  nLevels <- length(variableLevels)

  for (i in 1:(nLevels - 1)) {

    for (j in (i + 1):nLevels) {

      name <- paste(variableLevels[[i]], "-", variableLevels[[j]], sep = " ")
      contrast <- rep(0, nLevels)
      contrast[i] <- -1
      contrast[j] <- 1

      arg <- list(contrasts, contrast)
      names(arg)[2] <- name
      contrasts <- do.call(rbind, arg)

    }
  }

  return(contrasts)
}

.anovaPostHocTableCollection <- function(anovaContainer, dataset, options, ready) {
  if (length(options$postHocTerms) == 0 || !ready)
    return()

  if (is.null(anovaContainer[["postHocContainer"]])) {

    postHocContainer <- createJaspContainer(title = gettext("Post Hoc Tests"))
    postHocContainer$dependOn(c("postHocTerms"))
    anovaContainer[["postHocContainer"]] <- postHocContainer

  } else {

    postHocContainer <- anovaContainer[["postHocContainer"]]

  }

  if (options$postHocTypeStandard)
    .anovaStandardPostHocTable(postHocContainer, dataset, options,  anovaContainer[["model"]]$object, anovaContainer[["afexModel"]]$object)

  if (options$postHocTypeGames)
    .anovaGamesPostHocTable(postHocContainer, dataset, options,  anovaContainer[["model"]]$object)

  if (options$postHocTypeDunnet)
    .anovaDunnettPostHocTable(postHocContainer, dataset, options,  anovaContainer[["model"]]$object)

  return()
}

.anovaStandardPostHocTable <- function(postHocContainer, dataset, options, model, afexModel = NULL) {
  if (!is.null(postHocContainer[["postHocStandardContainer"]]))
    return()

  myTitle <- if (options[["postHocCorrectionTukey"]])  gettext("Standard (HSD)") else  gettext("Standard")
  postHocStandardContainer <- createJaspContainer(title = myTitle)
  postHocStandardContainer$dependOn(c("postHocTerms", "postHocTypeStandardEffectSize", "postHocTypeStandard",
                                      "postHocCorrectionBonferroni", "postHocCorrectionHolm", "postHocCorrectionScheffe",
                                      "postHocCorrectionTukey", "postHocCorrectionSidak", "postHocSignificanceFlag",
                                      "postHocTypeStandardBootstrap", "postHocTypeStandardBootstrapSamples", "postHocConditionalTable",
                                      "postHocCi", "postHocCiLevel", "postHocLetterTable", "postHocLetterAlpha"))

  postHocContainer[["postHocStandardContainer"]] <- postHocStandardContainer

  postHocVariables <- unlist(options$postHocTerms, recursive = FALSE)
  postHocVariablesListV <- unname(lapply(postHocVariables, .v))

  for (postHocVarIndex in 1:length(postHocVariables)) {

    thisVarName <- paste(postHocVariables[[postHocVarIndex]], collapse = " \u273B ")
    interactionTerm <- length(postHocVariables[[postHocVarIndex]]) > 1

    # create vector to loop over for conditional post hoc tables
    termsToLoop <- if (options[["postHocConditionalTable"]]) postHocVariables[[postHocVarIndex]] else 1

    for (termIndex in seq_along(termsToLoop)) {
      thisVarNameRef <- paste0(thisVarName, termIndex)
      byVariable <- if (length(termsToLoop) > 1) postHocVariables[[postHocVarIndex]] else NULL

      postHocStandardContainer[[thisVarNameRef]] <- .createPostHocStandardTable(thisVarName,
                                                                                byVariable[termIndex],
                                                                                options,
                                                                                options$postHocTypeStandardBootstrap)

      postHocRef <- emmeans::lsmeans(model, postHocVariablesListV)
      wantsCorrections <- c(options[["postHocCorrectionTukey"]], options[["postHocCorrectionScheffe"]],
                            options[["postHocCorrectionBonferroni"]], options[["postHocCorrectionHolm"]],
                            options[["postHocCorrectionSidak"]])
      postHocCorrections <- c("tukey", "scheffe", "bonferroni", "holm", "sidak")[wantsCorrections]
      if (sum(wantsCorrections) == 0)
        postHocCorrections <- "none"
      ## Computation
      resultPostHoc <- lapply(postHocCorrections, function(x)
        summary(emmeans::contrast(postHocRef[[postHocVarIndex]],
                                  method = "pairwise",
                                  by = byVariable[termIndex],
                                  enhance.levels = FALSE),
                adjust = x,
                infer = c(TRUE, TRUE),
                level = options$postHocCiLevel))

      numberOfLevels <- nrow(as.data.frame(postHocRef[[postHocVarIndex]]))
      bonfAdjustCIlevel <- if ("none" %in% postHocCorrections) options[["postHocCiLevel"]] else .computeBonferroniConfidence(options[["postHocCiLevel"]],
                                                                                                                             numberOfLevels = numberOfLevels)

      allContrasts <- strsplit(as.character(resultPostHoc[[1]]$contrast), split = " - ")
      # if there is p-adjustment, then add footnote
      if (nrow(resultPostHoc[[1]]) > 1 && any(grepl(attr(resultPostHoc[[1]], "mesg"), pattern = "P value adjustment")))
        postHocStandardContainer[[thisVarNameRef]]$addFootnote(.getCorrectionFootnoteAnova(resultPostHoc[[1]],
                                                                                           (options$postHocCi && isFALSE(options$postHocTypeStandardBootstrap)),
                                                                                           includeEffectSize = options[["postHocTypeStandardEffectSize"]],
                                                                                           isBetween = TRUE))
      avFootnote <- attr(resultPostHoc[[1]], "mesg")[grep(attr(resultPostHoc[[1]], "mesg"), pattern = "Results are averaged")]
      if (length(avFootnote) != 0) {
        avTerms <- strsplit(gsub(avFootnote, pattern = "Results are averaged over the levels of: ", replacement = ""),
                                 ", ")[[1]]
        postHocStandardContainer[[thisVarNameRef]]$addFootnote(gettextf("Results are averaged over the levels of: %s", paste(avTerms, collapse = ", ")))
      }
      allPvalues <- do.call(cbind, lapply(resultPostHoc, function(x) x$p.value))
      colnames(allPvalues) <- postHocCorrections
      resultPostHoc <- cbind(resultPostHoc[[1]], allPvalues)

      resultPostHoc[["contrast_A"]] <- lapply(allContrasts, `[[`, 1L)
      resultPostHoc[["contrast_B"]] <- lapply(allContrasts, `[[`, 2L)

      if (isFALSE(is.null(afexModel))) {
        effectSizeResult <- as.data.frame(emmeans::eff_size(postHocRef[[postHocVarIndex]],
                                                            sigma = sqrt(mean(sigma(model)^2)),
                                                            edf = df.residual(model),
                                                            level = bonfAdjustCIlevel,
                                                            by = byVariable[termIndex]))
        resultPostHoc[["cohenD"]] <- effectSizeResult[["effect.size"]]
        resultPostHoc[["cohenD_LowerCI"]] <- effectSizeResult[["lower.CL"]]
        resultPostHoc[["cohenD_UpperCI"]] <- effectSizeResult[["upper.CL"]]
      } else {
        postHocStandardContainer[[thisVarNameRef]]$addFootnote(message = gettext("Some parameters were not estimable due to missingness."))
      }

      if (options$postHocTypeStandardBootstrap) {

        postHocStandardContainer[[thisVarNameRef]]$addFootnote(message = gettextf("Bootstrapping based on %s successful replicates.", as.character(options[['postHocTypeStandardBootstrapSamples']])))
        postHocStandardContainer[[thisVarNameRef]]$addFootnote(message = gettext("Mean Difference estimate is based on the median of the bootstrap distribution."))
        postHocStandardContainer[[thisVarNameRef]]$addFootnote(symbol = "\u2020", message = gettext("Bias corrected accelerated."))

        startProgressbar(options[["postHocTypeStandardBootstrapSamples"]] * length(postHocVariables),
                         label = gettext("Bootstrapping Post Hoc Test"))

        ## Computation
        bootstrapPostHoc <- try(boot::boot(data = dataset, statistic = .bootstrapPostHoc,
                                           R = options[["postHocTypeStandardBootstrapSamples"]],
                                           options = options,
                                           nComparisons = nrow(resultPostHoc),
                                           postHocVariablesListV = postHocVariablesListV,
                                           postHocVarIndex = postHocVarIndex,
                                           by = byVariable[termIndex]),
                                silent = TRUE)

        if (jaspBase::isTryError(bootstrapPostHoc)) {
          postHocStandardContainer[[thisVarNameRef]]$setError(bootstrapPostHoc)
          next
        }

        bootstrapSummary <- summary(bootstrapPostHoc)

        ci.fails <- FALSE
        bootstrapPostHocConf <- t(sapply(1:nrow(bootstrapSummary), function(comparison){
          res <- try(boot::boot.ci(boot.out = bootstrapPostHoc, conf = options$postHocCiLevel, type = "bca",
                                   index = comparison)[['bca']][1,4:5])

          if(!inherits(res, "try-error")){
            return(res)
          } else {
            ci.fails <<- TRUE
            return(c(NA, NA))
          }
        }))

        if (ci.fails)
          postHocStandardContainer[[thisVarNameRef]]$addFootnote(message = gettext("Some confidence intervals could not be computed.\nPossibly too few bootstrap replicates."))

        resultPostHoc[["lower.CL"]] <- bootstrapPostHocConf[,1]
        resultPostHoc[["upper.CL"]] <- bootstrapPostHocConf[,2]

        resultPostHoc[["bias"]] <- bootstrapSummary[["bootBias"]]
        resultPostHoc[["SE"]] <- bootstrapSummary[["bootSE"]]
        resultPostHoc[["estimate"]] <- bootstrapSummary[["bootMed"]]

      }

      resultPostHoc[[".isNewGroup"]] <- c(TRUE, rep(FALSE, nrow(resultPostHoc)-1))
      postHocStandardContainer[[thisVarNameRef]]$setData(resultPostHoc)

      whichPvalues <- c(options$postHocCorrectionTukey, options$postHocCorrectionScheffe, options$postHocCorrectionBonferroni,
                        options$postHocCorrectionHolm, options$postHocCorrectionSidak)
      allPvalues <- as.data.frame(allPvalues)

      if (options$postHocSignificanceFlag && length(postHocCorrections[whichPvalues]) > 0)
        .anovaAddSignificanceSigns(someTable = postHocStandardContainer[[thisVarNameRef]],
                                   allPvalues = allPvalues,
                                   resultRowNames = rownames(resultPostHoc))

    }
    if (options$postHocLetterTable) {
      letterTable <- createJaspTable(title = paste0("Letter-Based Grouping - ", thisVarName))
      for (letterVar in postHocVariables[[postHocVarIndex]])
        letterTable$addColumnInfo(name=letterVar, type="string", combine = TRUE)

      letterTable$addColumnInfo(name="Letter", type="string")
      letterTable$addFootnote("If two or more means share the same grouping symbol,
      then we cannot show them to be different, but we also did not show them to be the same. ")
      letterTable$showSpecifiedColumnsOnly <- TRUE

      postHocStandardContainer[[paste0(thisVarName, "LetterTable")]] <- letterTable
      letterResult <- multcomp::cld(postHocRef[[postHocVarIndex]],
                                    method = "pairwise",
                                    Letters = letters,
                                    alpha = options[["postHocLetterAlpha"]])
      letterResult <- letterResult[c(postHocVariables[[postHocVarIndex]], ".group")]
      colnames(letterResult)[ncol(letterResult)] <- "Letter"
      letterResult <- letterResult[order(as.numeric(rownames(letterResult))), ]
      postHocStandardContainer[[paste0(thisVarName, "LetterTable")]]$setData(letterResult)
    }
  }


  return()
}


.bootstrapPostHoc <- function(data, indices, options, nComparisons, postHocVariablesListV, postHocVarIndex, by) {

  resamples <- data[indices, , drop = FALSE] # allows boot to select sample

  model <- .anovaModel(resamples, options)$model # refit model

  postHocRefBoots <- suppressMessages(
    emmeans::lsmeans(model, postHocVariablesListV)
  )

  postHocTableBoots <- suppressMessages(
    summary(emmeans::contrast(postHocRefBoots[[postHocVarIndex]], method = "pairwise", by = by),
            infer = c(FALSE, FALSE))
  )

  progressbarTick()

  if (nrow(postHocTableBoots) == nComparisons) {
    return(postHocTableBoots[['estimate']])
  } else{
    return(rep(NA, nComparisons))
  }
}


.anovaGamesPostHocTable <- function(postHocContainer, dataset, options, model) {
  if (!is.null(postHocContainer[["postHocGamesContainer"]]))
    return()

  postHocGamesContainer <- createJaspContainer(title = gettext("Games-Howell"))
  postHocGamesContainer$dependOn(c("postHocTypeGames", "postHocCiLevel",
                                   "postHocCi", "postHocSignificanceFlag"))
  postHocContainer[["postHocGamesContainer"]] <- postHocGamesContainer

  gamesVariables <- unique(unlist(options$postHocTerms))
  dependentVar <- dataset[[ .v(options$dependent) ]]
  postHocInterval  <- options$postHocCiLevel


  .createPostHocGamesTable <- function(myTitle) {

    postHocTable <- createJaspTable(title = gettextf("Games-Howell Post Hoc Comparisons - %s", myTitle))

    postHocTable$addColumnInfo(name="contrast", title=gettext("Comparison"),      type="string")
    postHocTable$addColumnInfo(name="meanDiff", title=gettext("Mean Difference"), type="number")

    if (options$postHocCi) {
      thisOverTitle <- gettextf("%.0f%% CI for Mean Difference", options$postHocCiLevel * 100)
      postHocTable$addColumnInfo(name="lowerCI", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
      postHocTable$addColumnInfo(name="upperCI", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
    }

    postHocTable$addColumnInfo(name="SE",                                         type="number")
    postHocTable$addColumnInfo(name="t",                                          type="number")
    postHocTable$addColumnInfo(name="df",                                         type="number")
    postHocTable$addColumnInfo(name="pTukey", title=gettext("p<sub>tukey</sub>"), type="pvalue")

    postHocTable$showSpecifiedColumnsOnly <- TRUE
    postHocTable$addFootnote(message = gettext("Results based on uncorrected means."))

    return(postHocTable)
  }

  for (gamesVar in gamesVariables) {

    postHocGamesContainer[[gamesVar]] <- .createPostHocGamesTable(gamesVar)

    groupingVar <- dataset[[ .v(gamesVar) ]]
    variableLevels <- levels(droplevels(groupingVar))
    nLevels <- length(variableLevels)
    meanPerLevel <- tapply(dependentVar, groupingVar, mean)
    nPerLevel <- tapply(dependentVar, groupingVar, length)
    varPerLevel <- tapply(dependentVar, groupingVar, var)

    gamesResult <- data.frame(contrast = character(),
                              meanDiff = numeric(),
                              lowerCI = numeric(),
                              upperCI = numeric(),
                              SE = numeric(),
                              t = numeric(),
                              df = numeric(),
                              pTukey = numeric())

    for (i in 1:(nLevels - 1)) {

      for (j in (i + 1):nLevels) {

        contrast <- paste0(variableLevels[[i]], " - ", variableLevels[[j]])

        meanDiff <- meanPerLevel[[i]] - meanPerLevel[[j]]
        t <- abs(meanDiff) / sqrt((varPerLevel[[i]] / nPerLevel[[i]]) + (varPerLevel[[j]] / nPerLevel[[j]]))
        se <- sqrt((varPerLevel[[i]] / nPerLevel[[i]] + varPerLevel[[j]] / nPerLevel[[j]]))

        df <- se^4 / ((varPerLevel[[i]] / nPerLevel[[i]])^2 / (nPerLevel[[i]] - 1) +
                        (varPerLevel[[j]] / nPerLevel[[j]])^2 / (nPerLevel[[j]] - 1))

        pVal <- ptukey(t * sqrt(2), nLevels, df, lower.tail = FALSE)

        upperConf <- meanDiff + qtukey(p = postHocInterval, nmeans = nLevels, df = df) * se * sqrt(0.5)
        lowerConf <- meanDiff - qtukey(p = postHocInterval, nmeans = nLevels, df = df) * se * sqrt(0.5)

        gamesResult <- rbind(gamesResult, data.frame(contrast = contrast,
                                                     meanDiff = meanDiff,
                                                     lowerCI = lowerConf,
                                                     upperCI = upperConf,
                                                     SE = se,
                                                     t = t*sign(meanDiff),
                                                     df = df,
                                                     pTukey = pVal))
      }
    }

    postHocGamesContainer[[gamesVar]]$setData(gamesResult)

    if (options$postHocSignificanceFlag)
      .anovaAddSignificanceSigns(someTable = postHocGamesContainer[[gamesVar]],
                                 allPvalues = gamesResult["pTukey"],
                                 resultRowNames = rownames(gamesResult))

  }

  return()
}

.anovaDunnettPostHocTable <- function(postHocContainer, dataset, options, model) {
  if (!is.null(postHocContainer[["postHocDunnettContainer"]]))
    return()

  postHocDunnettContainer <- createJaspContainer(title = gettext("Dunnett"))
  postHocDunnettContainer$dependOn(c("postHocTypeDunnet", "postHocCiLevel",
                                     "postHocCi", "postHocSignificanceFlag"))
  postHocContainer[["postHocDunnettContainer"]] <- postHocDunnettContainer

  dunnettVariables <- unique(unlist(options$postHocTerms))
  dependentVariable <- dataset[[ .v(options$dependent) ]]

  .createPostHocDunnettTable <- function(myTitle) {

    postHocTable <- createJaspTable(title = gettextf("Dunnett Post Hoc Comparisons - %s", myTitle))

    postHocTable$addColumnInfo(name="contrast",title=gettext("Comparison"),      type="string")
    postHocTable$addColumnInfo(name="meanDiff",title=gettext("Mean Difference"), type="number")

    if (options$postHocCi) {
      thisOverTitle <- gettextf("%s%% CI for Mean Difference", options$postHocCiLevel * 100)
      postHocTable$addColumnInfo(name="lowerCI", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
      postHocTable$addColumnInfo(name="upperCI", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
    }

    postHocTable$addColumnInfo(name="SE", type="number")
    postHocTable$addColumnInfo(name="t", type="number")
    postHocTable$addColumnInfo(name="p", title=gettext("p<sub>Dunnett</sub>"), type="pvalue")

    postHocTable$showSpecifiedColumnsOnly <- TRUE
    postHocTable$addFootnote(message = gettext("Results based on uncorrected means."))

    return(postHocTable)
  }

  for (dunnettVar in dunnettVariables) {

    postHocDunnettContainer[[dunnettVar]] <- .createPostHocDunnettTable(dunnettVar)

    Group <- dataset[[ .v(dunnettVar) ]]
    nLevels <- length(unique(Group))

    dunAOV <- aov(dependentVariable ~ Group)

    dunnettFit <- multcomp::glht(dunAOV, linfct=multcomp::mcp(Group="Dunnett"))
    dunnettResult <- summary(dunnettFit)[["test"]]
    dunnettConfInt <- try(confint(dunnettFit, level = options$postHocCiLevel), silent = TRUE)

    if (options$postHocCi && jaspBase::isTryError(dunnettConfInt)) {
      postHocDunnettContainer$setError(gettext("Confidence interval is too narrow, please select a different confidence level."))
    } else {
      dunnettResult <- data.frame(contrast = names(dunnettResult$coefficients),
                                  meanDiff = dunnettResult$coefficients,
                                  lowerCI = dunnettConfInt$confint[,2],
                                  upperCI = dunnettConfInt$confint[,3],
                                  SE = dunnettResult$sigma,
                                  t = dunnettResult$tstat,
                                  p = dunnettResult$pvalues)

      postHocDunnettContainer[[dunnettVar]]$setData(dunnettResult)

      if (options$postHocSignificanceFlag)
        .anovaAddSignificanceSigns(someTable = postHocDunnettContainer[[dunnettVar]],
                                   allPvalues = dunnettResult["p"],
                                   resultRowNames = rownames(dunnettResult))

    }
  }

  return()
}

.anovaAssumptionsContainer <- function(anovaContainer, dataset, options, ready) {
  if (!is.null(anovaContainer[["assumptionsContainer"]]))
    return()

  assumptionsContainer <- createJaspContainer(title = gettext("Assumption Checks"),
                                              dependencies = c("homogeneityTests", "qqPlot"))

  anovaContainer[["assumptionsContainer"]] <- assumptionsContainer

  if (options$homogeneityTests == TRUE)
    .anovaLevenesTable(anovaContainer, dataset, options, ready)

  if (options$qqPlot == TRUE)
    .qqPlotFreqAnova(anovaContainer, dataset, options, ready)

  return()
}

.anovaLevenesTable <- function(anovaContainer, dataset, options, ready) {

  leveneTable <- createJaspTable(title = gettext("Test for Equality of Variances (Levene's)"))

  leveneTable$addColumnInfo(name="F",   title=gettext("F"),   type="number")
  leveneTable$addColumnInfo(name="df1", title=gettext("df1"), type="number")
  leveneTable$addColumnInfo(name="df2", title=gettext("df2"), type="number")
  leveneTable$addColumnInfo(name="p",   title=gettext("p"),   type="pvalue")


  if (options$vovkSellke) {
    leveneTable$addColumnInfo(title = gettextf("VS-MPR%s", "\u002A"), name = "vovkSellke", type = "number")
    leveneTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }

  leveneTable$showSpecifiedColumnsOnly <- TRUE

  anovaContainer[["assumptionsContainer"]][["leveneTable"]] <- leveneTable

  if (!ready || anovaContainer$getError())
    return()

  # Start Levene computations
  model <- anovaContainer[["model"]]$object
  interaction <- paste(.v(options$fixedFactors), collapse=":", sep="")
  resids <- abs(model$residuals)

  leveneResult <- summary(aov(as.formula(paste("resids", "~", interaction)), dataset))[[1]]
  error <- base::tryCatch(summary(aov(levene.formula, dataset)),error=function(e) e, warning=function(w) w)

  if (!is.null(error$message) && error$message == "ANOVA F-tests on an essentially perfect fit are unreliable") {
    leveneTable$setError(gettext("F-value equal to zero indicating perfect fit.<br><br>(Levene's tests on an essentially perfect fit are unreliable)"))
    return()
  }

  leveneTable$setData(data.frame(F = leveneResult$`F value`[1],
                                 df1 = leveneResult$Df[1],
                                 df2 = leveneResult$Df[2],
                                 p = leveneResult$`Pr(>F)`[1],
                                 vovkSellke = VovkSellkeMPR(leveneResult$`Pr(>F)`[1])))

  return()
}

.anovaMarginalMeans <- function(anovaContainer, dataset, options, ready) {
  if (!is.null(anovaContainer[["marginalMeansContainer"]]) || length(options$marginalMeanTerms) == 0 || !ready)
    return()

  marginalMeansContainer <- createJaspContainer(title = gettext("Marginal Means"))
  marginalMeansContainer$dependOn(c("marginalMeanTerms",  "marginalMeanComparedToZero", "marginalMeanCiCorrection",
                                    "marginalMeanBootstrap", "marginalMeanBootstrapSamples"))

  anovaContainer[["marginalMeansContainer"]] <- marginalMeansContainer

  model <- anovaContainer[["model"]]$object

  terms <- options$marginalMeanTerms

  marginalVariables <- unlist(options$marginalMeanTerms, recursive = FALSE)
  marginalVariablesListV <- unname(lapply(marginalVariables, .v))

  for (i in seq_along(marginalVariables)) {
    thisVarName <- paste(marginalVariables[[i]], collapse = " \u273B ")
    individualTerms <- marginalVariables[[i]]
    marginalMeansContainer[[thisVarName]] <- .createMarginalMeansTableAnova(thisVarName, options, individualTerms,
                                                                            options[["marginalMeanBootstrap"]])
  }


  terms.base64 <- c()
  terms.normal <- c()

  for (term in terms) {

    components <- unlist(term)
    term.base64 <- paste(.v(components), collapse=":", sep="")
    term.normal <- paste(components, collapse=" \u273B ", sep="")

    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }


  for (i in seq_along(marginalVariables)) {

    thisVarName <- paste(marginalVariables[[i]], collapse = " \u273B ")
    thisTermNameV <- paste(marginalVariablesListV[[i]], collapse = ":")

    individualTerms <- marginalVariables[[i]]

    lvls <- list()
    factors <- list()

    for (variable in individualTerms) {

      factor <- dataset[[ .v(variable) ]]
      factors[[length(factors) + 1]] <- factor
      lvls[[variable]] <- levels(factor)

    }

    cases <- rev(expand.grid(rev(lvls)))
    cases <- as.data.frame(apply(cases, 2, as.character))

    nRows <- dim(cases)[1]
    nCol <- dim(cases)[2]

    formula <- as.formula(paste("~", thisTermNameV))

    if(options$marginalMeanCiCorrection == "bonferroni") {
      adjMethod <- "bonferroni"
    } else if(options$marginalMeanCiCorrection == "sidak") {
      adjMethod <- "sidak"
    } else {
      adjMethod <- "none"
    }

    marginalResult <- summary(emmeans::lsmeans(model, formula), adjust = adjMethod, infer = c(TRUE,TRUE))

    marginalResult[[".isNewGroup"]] <- FALSE
    marginalResult[[".isNewGroup"]][which(marginalResult[, 1] == marginalResult[1, 1])] <- TRUE

    names(marginalResult)[1:length(individualTerms)] <- individualTerms

    if (options$marginalMeanBootstrap) {

      startProgressbar(options[["marginalMeanBootstrapSamples"]],
                       label = gettext("Bootstrapping Marginal Means"))

      anovaFormula <- as.formula(paste("~", terms.base64[i]))
      bootstrapMarginalMeans <- try(boot::boot(data = dataset, statistic = .bootstrapMarginalMeans,
                                               R = options[["marginalMeanBootstrapSamples"]],
                                               options = options, nRows = nRows,
                                               anovaFormula = anovaFormula), silent = TRUE)

      if (jaspBase::isTryError(bootstrapMarginalMeans)) {
        marginalMeansContainer[[thisVarName]]$setError(bootstrapMarginalMeans)
        next
      }

      bootstrapSummary <- summary(bootstrapMarginalMeans)

      ci.fails <- FALSE
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

      if(ci.fails)
        marginalMeansContainer[[thisVarName]]$addFootnote(message = gettext("Some confidence intervals could not be computed. Possibly too few bootstrap replicates."))

      marginalResult[["lower.CL"]] <- bootstrapMarginalMeansCI[,1]
      marginalResult[["upper.CL"]] <- bootstrapMarginalMeansCI[,2]

      marginalResult[["bias"]] <- bootstrapSummary[["bootBias"]]
      marginalResult[["SE"]] <- bootstrapSummary[["bootSE"]]
      marginalResult[["lsmean"]] <- bootstrapSummary[["bootMed"]]

      marginalMeansContainer[[thisVarName]]$addFootnote(message = gettextf("Bootstrapping based on %s replicates.", as.character(bootstrapSummary$R[1])))
    }
    marginalMeansContainer[[thisVarName]]$setData(marginalResult)
  }

  return()
}

.bootstrapMarginalMeans <- function(data, indices, options, nRows, anovaFormula){

  resamples <- data[indices, , drop=FALSE]

  model <- .anovaModel(resamples, options)$model # refit model

  r <- suppressMessages( # to remove clutter
    summary(emmeans::lsmeans(model, anovaFormula), infer = c(FALSE,FALSE))
  )

  progressbarTick()

  if(length(r$lsmean) == nRows){ # ensure that the bootstrap has all levels
    return(r$lsmean)
  } else {
    return(rep(NA, nRows))
  }
}

.anovaSimpleEffects <- function(anovaContainer, dataset, options, ready) {
  if (!is.null(anovaContainer[["simpleEffectsContainer"]]) || identical(options$simpleMainEffectFactor, "") ||
      identical(options$simpleMainEffectModeratorFactorOne, ""))
    return()

  anovaContainer[["simpleEffectsContainer"]] <- createJaspContainer(title = gettext("Simple Main Effects"),
                                                                    dependencies = c("simpleMainEffectFactor",
                                                                                     "simpleMainEffectModeratorFactorOne",
                                                                                     "simpleMainEffectModeratorFactorTwo"))
  simpleEffectsTable <- createJaspTable(title = gettextf("Simple Main Effects - %s", options$simpleMainEffectFactor))

  anovaContainer[["simpleEffectsContainer"]][["simpleEffectsTable"]] <- simpleEffectsTable

  moderatorTerms <- c(options$simpleMainEffectModeratorFactorOne, options$simpleMainEffectModeratorFactorTwo[!identical(options$simpleMainEffectModeratorFactorTwo, "")])
  nMods <- length(moderatorTerms)
  simpleMainEffectFactorBase64 <- .v(options$simpleMainEffectFactor)

  simpleEffectsTable[["title"]] <- gettextf("Simple Main Effects - %s", options$simpleMainEffectFactor)

  simpleEffectsTable$addColumnInfo(name = "modOne", title = gettextf("Level of %s", moderatorTerms[1]),
                                   type = "string", combine = TRUE)

  if (nMods == 2)
    simpleEffectsTable$addColumnInfo(name = "modTwo", title = gettextf("Level of %s", moderatorTerms[2]),
                                   type = "string", combine = TRUE)


  simpleEffectsTable$addColumnInfo(name = "Sum Sq",  type = "number",  title = gettext("Sum of Squares"))
  simpleEffectsTable$addColumnInfo(name = "Df",      type = "integer", title = gettext("df"))
  simpleEffectsTable$addColumnInfo(name = "Mean Sq", type = "number",  title = gettext("Mean Square"))
  simpleEffectsTable$addColumnInfo(name = "F value", type = "number",  title = gettext("F"))
  simpleEffectsTable$addColumnInfo(name = "Pr(>F)",  type = "pvalue",  title = gettext("p"))

  simpleEffectsTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready || anovaContainer$getError())
    return()

  fullAnovaMS <- anovaContainer[["anovaResult"]]$object$result["Residuals", "Mean Sq"]
  fullAnovaDf <- anovaContainer[["anovaResult"]]$object$result["Residuals", "Df"]

  # Remove moderator factors from model terms
  simpleOptions <- options
  simpleOptions$modelTerms <-  options$modelTerms[!(grepl(moderatorTerms[1], options$modelTerms) |
                                                      grepl(moderatorTerms[nMods], options$modelTerms))]
  simpleOptions$fixedFactors <- options$fixedFactors[!(grepl(moderatorTerms[1], options$fixedFactors) |
                                                         grepl(moderatorTerms[nMods], options$fixedFactors))]

  # simpleOptions$covariates <- NULL
  # simpleOptions$modelTerms <- list(list(components = "facExperim"))
  lvls <- list()
  factors <- list()

  for (variable in moderatorTerms) {

    factor <- dataset[[ .v(variable) ]]
    factors[[length(factors)+1]] <- factor
    lvls[[variable]] <- levels(factor)
  }

  simpleEffectResult <- rev(expand.grid(rev(lvls), stringsAsFactors = FALSE))
  colnames(simpleEffectResult) <- c("modOne", "modTwo")[1:nMods]

  simpleEffectResult[[".isNewGroup"]] <- c(TRUE, rep(FALSE, nrow(simpleEffectResult)-1))
  emptyCaseIndices <- emptyCases <- NULL
  allSimpleModels <- list()

  for (i in 1:nrow(simpleEffectResult)) {

    subsetStatement  <- eval(parse(text=paste("dataset$", .v(moderatorTerms), " == \"",
                                              simpleEffectResult[i, 1:nMods],
                                              "\"", sep = "", collapse = " & ")))
    simpleDataset <- base::subset(dataset, subsetStatement)

    if (simpleEffectResult[i, nMods] == lvls[[ nMods ]][1] && nMods == 2)
      simpleEffectResult[[i, ".isNewGroup"]] <- TRUE

    if (nrow(simpleDataset) < 2 ||
        nrow(unique(simpleDataset[simpleMainEffectFactorBase64])) <  nrow(unique(dataset[simpleMainEffectFactorBase64]))) {

      emptyCaseIndices <- c(emptyCaseIndices, i)
      emptyCases <- c(emptyCases, paste(simpleEffectResult[i, 1:nMods], collapse = ", "))
      allSimpleModels[[i]] <- NA

    } else {

      .anovaModelContainer(anovaContainer[["simpleEffectsContainer"]], dataset = simpleDataset, options = simpleOptions, TRUE)
      .anovaResult(anovaContainer[["simpleEffectsContainer"]], options = simpleOptions)
      simpleResult <- anovaContainer[["simpleEffectsContainer"]][["anovaResult"]]$object$result
      simpleResult[[".isNewGroup"]] <- NULL

      allSimpleModels[[i]] <- simpleResult[simpleMainEffectFactorBase64, ]
      anovaContainer[["simpleEffectsContainer"]][["model"]] <- NULL

    }
  }

  if (!is.null(emptyCaseIndices)) {
    simpleEffectsTable$addFootnote(gettextf("Not enough observations in cells %s.",
                                          paste0(" (", emptyCases, ")", collapse = ",")))
  }

  # Combine the ANOVA results with the cases
  simpleEffectResult <- cbind(simpleEffectResult, do.call(rbind, allSimpleModels))

  # Apply corrections to F and p based on the original ANOVA
  simpleEffectResult[["F value"]] <- simpleEffectResult[["Mean Sq"]] / fullAnovaMS
  simpleEffectResult[["Pr(>F)"]] <-  pf(simpleEffectResult[["F value"]], simpleEffectResult[["Df"]],
                                        fullAnovaDf, lower.tail = FALSE)

  simpleEffectsTable$setData(simpleEffectResult)

  return()
}

.anovaKruskal <- function(anovaContainer, dataset, options, ready) {
  if (!is.null(anovaContainer[["kruskalContainer"]]) || !length(options$kruskalWallisFactors))
    return()


  anovaContainer[["kruskalContainer"]] <- createJaspContainer(title = gettext("Kruskal-Wallis Test"),
                                                              dependencies = c("kruskalWallisFactors", "kruskalCiLevel",
                                                                               "kruskalEpsilon", "kruskalEta", "postHocTypeDunn",
                                                                               "kruskalEffectSizeEstimates"))
  kruskalTable <- createJaspTable(title = gettext("Kruskal-Wallis Test"))

  anovaContainer[["kruskalContainer"]][["kruskalTable"]] <- kruskalTable

  kruskalTable$addColumnInfo(name = "factor",    title=gettext("Factor"),    type = "string")
  kruskalTable$addColumnInfo(name = "statistic", title=gettext("Statistic"), type = "number")
  kruskalTable$addColumnInfo(name = "df",        title=gettext("df"),        type = "integer")
  kruskalTable$addColumnInfo(name = "p",         title=gettext("p"),         type = "pvalue")

  if (options[["kruskalEpsilon"]] && options[["kruskalEffectSizeEstimates"]]) {
    kruskalTable$addColumnInfo(name = "epsiSqr", title=gettextf("Rank %s", "\u03B5\u00B2"), type = "number")
    thisOverTitle <- gettextf("%1$i%% CI for Rank %2$s", options$kruskalCiLevel * 100, "\u03B5\u00B2")
    kruskalTable$addColumnInfo(name="lowerEpsiCI", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
    kruskalTable$addColumnInfo(name="upperEpsiCI", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
  }

  if (options[["kruskalEta"]] && options[["kruskalEffectSizeEstimates"]]) {
    kruskalTable$addColumnInfo(name = "etaSqr", title=gettextf("Rank %s", "\u03B7\u00B2"), type = "number")
    thisOverTitle <- gettextf("%1$i%% CI for Rank %2$s", options$kruskalCiLevel * 100, "\u03B7\u00B2")
    kruskalTable$addColumnInfo(name="lowerEtaCI", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
    kruskalTable$addColumnInfo(name="upperEtaCI", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
  }

  kruskalTable$showSpecifiedColumnsOnly <- TRUE
  if (length(options[["kruskalWallisFactors"]]) > 1)
    kruskalTable$addFootnote(gettext("Tests are conducted separately for each variable, without accounting for multivariate effects."))

  if (!ready || anovaContainer$getError())
    return()

  kruskalFactors <- options$kruskalWallisFactors
  kruskalResultsList <- kruskalEtaList <- kruskalEpsilonList <- list()

  for (term in kruskalFactors) {

    kruskalResultsList[[term]] <- kruskal.test(dataset[[options$dependent]], dataset[[term]])
    kruskalEpsilonList[[term]] <- effectsize::rank_epsilon_squared(dataset[[options$dependent]], dataset[[term]], alternative = "two.sided")
    kruskalEtaList[[term]] <- effectsize::rank_eta_squared(dataset[[options$dependent]], dataset[[term]], alternative = "two.sided")

  }

  kruskalTable$setData(data.frame(factor = names(kruskalResultsList),
                                  statistic = sapply(kruskalResultsList, function(x) x$statistic),
                                  df = sapply(kruskalResultsList, function(x) x$parameter),
                                  p = sapply(kruskalResultsList, function(x) x$p.value),
                                  epsiSqr = sapply(kruskalEpsilonList, function(x) x$rank_epsilon_squared),
                                  lowerEpsiCI = sapply(kruskalEpsilonList, function(x) x$CI_low),
                                  upperEpsiCI = sapply(kruskalEpsilonList, function(x) x$CI_high),
                                  etaSqr = sapply(kruskalEtaList, function(x) x$rank_eta_squared),
                                  lowerEtaCI = sapply(kruskalEtaList, function(x) x$CI_low),
                                  upperEtaCI = sapply(kruskalEtaList, function(x) x$CI_high)))

  if (options$postHocTypeDunn)
    .anovaDunnTable(anovaContainer[["kruskalContainer"]], dataset, options,  anovaContainer[["model"]]$object)



  return()
}

.anovaDunnTable <- function(kruskalContainer, dataset, options, model) {
  if (!is.null(kruskalContainer[["dunnContainer"]]))
    return()

  kruskalContainer[["dunnContainer"]] <- createJaspContainer(title = gettext("Dunn"))

  dunnVariables <- options[["kruskalWallisFactors"]]
  dependentVar <- options[["dependent"]]

  .createPostHocDunnTable <- function(myTitle) {

    postHocTable <- createJaspTable(title = gettextf("Dunn's Post Hoc Comparisons - %s", myTitle))

    postHocTable$addColumnInfo(name="contrast",   title=gettext("Comparison"),       type="string")
    postHocTable$addColumnInfo(name="z",                                             type="number")
    postHocTable$addColumnInfo(name="wA",         title=gettext("W<sub>i</sub>"),    type="number")
    postHocTable$addColumnInfo(name="wB",         title=gettext("W<sub>j</sub>"),    type="number")
    postHocTable$addColumnInfo(name="rbs",        title=gettext("r<sub>rb</sub>"),   type="number")
    postHocTable$addColumnInfo(name="pval",       title=gettext("p"),                type="pvalue")
    postHocTable$addColumnInfo(name="bonferroni", title=gettext("p<sub>Bonf</sub>"), type="pvalue")
    postHocTable$addColumnInfo(name="holm",       title=gettext("p<sub>Holm</sub>"), type="pvalue")

    return(postHocTable)
  }

  for (dunnVar in dunnVariables) {

    kruskalContainer[["dunnContainer"]][[dunnVar]] <- .createPostHocDunnTable(dunnVar)
    dunnResult <- data.frame(contrast = character(),
                             z = numeric(),
                             wA = numeric(),
                             wB = numeric(),
                             rbs = numeric(),
                             pval = numeric(),
                             bonferroni = numeric(),
                             holm = numeric())

    variableLevels <- levels(droplevels(dataset[[ .v(dunnVar) ]]))
    nLevels <- length(variableLevels)
    nPerGroup <- unname(unlist(table(dataset[[ .v(dunnVar) ]])))
    bigN <- sum(nPerGroup)

    fullRanks <- rank(dataset[[ dependentVar ]])
    ranksPerGroup <- by(fullRanks, dataset[[ dunnVar ]], list)
    sumPerGroup <- unlist(lapply(ranksPerGroup, FUN = sum))
    meanPerGroup <- unname(sumPerGroup/nPerGroup)

    tab <- table(unlist(ranksPerGroup))
    nTies <- tab[tab > 1]
    nTies <- sum(nTies^3 - nTies)

    for (i in 1:(nLevels - 1)) {

      for (j in (i + 1):nLevels) {

        contrast <- paste0(variableLevels[[i]], " - ", variableLevels[[j]])

        sigmaAB <- sqrt( ( (bigN * (bigN + 1))/12 - nTies/(12 * (bigN - 1)) ) * (1/nPerGroup[i] + 1/nPerGroup[j] )  )
        zAB <- (meanPerGroup[i] - meanPerGroup[j]) / sigmaAB
        pValAB <- 2 * pnorm(abs(zAB), lower.tail = FALSE) # make two-sided p-value

        a <- dataset[[ dependentVar ]][dataset[[ dunnVar ]] == variableLevels[[i]]]
        b <- dataset[[ dependentVar ]][dataset[[ dunnVar ]] == variableLevels[[j]]]
        u <- wilcox.test(a, b)$statistic
        rbs <- as.numeric(1-(2*u)/(nPerGroup[i]*nPerGroup[j]))

        dunnResult <- rbind(dunnResult, data.frame(contrast = contrast,
                                                   z = zAB,
                                                   wA = meanPerGroup[i],
                                                   wB = meanPerGroup[j],
                                                   rbs = rbs,
                                                   pval = pValAB,
                                                   bonferroni = pValAB,
                                                   holm = pValAB))

      }

    }

    allP <- dunnResult[["pval"]]
    dunnResult[["bonferroni"]] <- p.adjust(allP, method = "bonferroni")
    dunnResult[["holm"]] <- p.adjust(allP, method = "holm")

    kruskalContainer[["dunnContainer"]][[dunnVar]]$setData(dunnResult)
    kruskalContainer[["dunnContainer"]][[dunnVar]]$addFootnote(message = gettext("Rank-biserial correlation based on individual Mann-Whitney tests."))

    if (options$postHocSignificanceFlag)
      .anovaAddSignificanceSigns(someTable = kruskalContainer[["dunnContainer"]][[dunnVar]],
                                 allPvalues = cbind(dunnResult[, c("pval", "bonferroni", "holm")]),
                                 resultRowNames = rownames(dunnResult))
  }

  return()
}


