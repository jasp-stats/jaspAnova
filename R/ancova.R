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

Ancova <- function(jaspResults, dataset = NULL, options) {

  numericVariables <- c(unlist(options$dependent),unlist(options$covariates),unlist(options$wlsWeight))
  numericVariables <- numericVariables[numericVariables != ""]
  factorVariables <- c(unlist(options$fixedFactors),unlist(options$randomFactors))
  factorVariables <- factorVariables[factorVariables != ""]
  nFactorModelTerms <- sum(unlist(options$modelTerms) %in% factorVariables)

  ready <- options$dependent != "" && length(options$fixedFactors) > 0 && nFactorModelTerms > 0
  options(contrasts = c("contr.sum","contr.poly"))

  # Set corrections to FALSE when performing ANCOVA
  if (is.null(options$homogeneityBrown)) {
    options$homogeneityNone <- TRUE
    options$homogeneityBrown <- FALSE
    options$homogeneityWelch <- FALSE
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

  .anovaOrdinalRestrictions(anovaContainer, dataset, options, ready)

  .anovaPostHocTableCollection(anovaContainer, dataset, options, ready)

  .anovaMarginalMeans(anovaContainer, dataset, options, ready)

  .anovaSimpleEffects(anovaContainer, dataset, options, ready)

  .anovaKruskal(anovaContainer, dataset, options, ready)

  .BANOVAdescriptives(anovaContainer, dataset, options, list(noVariables=FALSE), "ANCOVA", ready)

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

  observations.amount <- paste("<", length(options[["dependent"]]) + 1L)
  for(i in rev(seq_along(componentsToGroupOn))) {

    componentsToGroupBy <- componentsToGroupOn[[i]][["components"]]

    .hasErrors(
      dataset              = dataset,
      type                 = c("observations", "variance"),
      all.target           = c(options$dependent, options$covariates),
      all.grouping         = componentsToGroupBy,
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

    fixedFactors <- list()
    covariates <- list()

    k <- 1
    l <- 1

    for(i in 1:length(options$modelTerms)) {
      if (sum(unlist(options$modelTerms[[i]]$components) %in% options$covariates) > 0) {
        covariates[[k]] <- options$modelTerms[[i]]
        k <- k + 1
      } else {
        fixedFactors[[l]] <- options$modelTerms[[i]]
        l <- l + 1
      }
    }

    if(length(covariates) > length(options$covariates)) {
      modelTerms <- options$modelTerms
      interactions <- TRUE
    } else {
      modelTerms <- c(fixedFactors, covariates)
      modelTerms <- modelTerms[match(modelTerms, options$modelTerms)]
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

  modelError <- try(silent = TRUE, lm(model.formula, dataset, weights=WLS, singular.ok = FALSE))

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
    } else {
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
  if (options$sumOfSquares == "type1") {

    result <- base::tryCatch(stats::anova(model),error=function(e) e, warning=function(w) w)

    if (!is.null(result$message)) {
      anovaContainer$setError(result$message)
      return()
    }

    result['SSt'] <- sum(result[,"Sum Sq"], na.rm = TRUE)

  } else if (options$sumOfSquares == "type2") {

    result <- car::Anova(model, type=2)
    result['Mean Sq'] <- result[['Sum Sq']] / result[['Df']]
    result['SSt'] <- sum(result[['Sum Sq']], na.rm = TRUE)

  } else if (options$sumOfSquares == "type3") {

    modelTerms <- unlist(options$modelTerms, recursive = FALSE)
    factorModelTerms <- options$modelTerms[sapply(modelTerms, function(x) !any(x %in% options$covariates))]
    # For each model term, including all interactions, check if there are empty cells
    if (any(sapply(factorModelTerms, function(x) any(table(model$model[, .v(x$components)]) == 0)))) {
      anovaContainer$setError(gettext("Your design contains empty cells. Please try a different type of sum of squares."))
      return()
    }

    result <- car::Anova(model, type=3, singular.ok=FALSE)
    result <- result[-1, ]
    result['Mean Sq'] <- result[['Sum Sq']] / result[['Df']]
    result['SSt'] <- sum(result["Sum Sq"], na.rm = TRUE)

  }

  # Make sure that the order of the result is same order as reordered modelterms
  result <- result[.mapAnovaTermsToTerms(c(termsBase64, "Residuals"), rownames(result) ), ]
  result[['cases']] <- c(termsNormal, "Residuals")
  result <- as.data.frame(result)
  result[['.isNewGroup']] <- c(TRUE, rep(FALSE, nrow(result)-2), TRUE)
  if (length(options$covariates) > 0)
    result[.v(options$covariates), ][[".isNewGroup"]] <- TRUE

  result[1, 'correction'] <- "None"

  if (options$effectSizeEstimates) {

      SSt <- result['SSt']
      SSr <- result["Residuals", "Sum Sq"]
      MSr <- SSr/result["Residuals", "Df"]

      eta <- result[['Sum Sq']] / result[['SSt']]
      etaPart <- result[['Sum Sq']] / (result[['Sum Sq']] + SSr)
      omega <- (result[['Sum Sq']] - (result[['Df']] * MSr)) / (SSt + MSr)
      omega <- sapply(omega[,1], function(x) max(x, 0))

      result[, c("eta", "etaPart", "omega")] <- cbind(eta, etaPart, omega)
      result["Residuals", c("eta", "etaPart", "omega")] <- NA

  }

  if (options$VovkSellkeMPR) {
    result[["VovkSellkeMPR"]] <-  ifelse(result[['Pr(>F)']] != "", VovkSellkeMPR(na.omit(result[['Pr(>F)']])), "")
  }

  if ((options$homogeneityBrown || options$homogeneityWelch) && length(options$modelTerms) > 1)
    return()

  anovaResult <- list()
  if (options$homogeneityNone) {
    anovaResult[["result"]] <- result
  }

  if (options$homogeneityBrown) {

    tempResult <- onewaytests::bf.test(as.formula(modelDef$model.def), model$model)
    brownResult <- result
    brownResult[[1, 'correction']] <- "Brown-Forsythe"

    if (options$homogeneityNone)
      brownResult[['.isNewGroup']] <- c(TRUE, rep(FALSE, nrow(result)-1))

    brownResult[[termsBase64, 'Df']] <- tempResult[['parameter']][[1]]
    brownResult[[termsBase64, 'Pr(>F)']] <- tempResult[['p.value']]
    brownResult[[termsBase64, 'F value']] <- tempResult[['statistic']]
    brownResult[['Residuals', 'Df']] <- tempResult[['parameter']][[2]]
    brownResult[['Mean Sq']] <- brownResult[['Sum Sq']] / brownResult[['Df']]

    if (options$VovkSellkeMPR) {
      brownResult[['VovkSellkeMPR']] <-  ifelse(brownResult[['Pr(>F)']] != "",
                                                VovkSellkeMPR(na.omit(brownResult[['Pr(>F)']])), "")
    }

    anovaResult[['brownResult']] <- brownResult
  }

  if (options$homogeneityWelch) {

    tempResult <- stats::oneway.test(as.formula(modelDef$model.def), model$model, var.equal = FALSE)
    welchResult <- result
    welchResult[[1, 'correction']] <- "Welch"

    if (options$homogeneityNone || options$homogeneityBrown)
      welchResult[['.isNewGroup']] <- c(TRUE, rep(FALSE, nrow(result)-1))

    welchResult[[termsBase64, 'Df']] <- tempResult[['parameter']][[1]]
    welchResult[[termsBase64, 'Pr(>F)']] <- tempResult[['p.value']]
    welchResult[[termsBase64, 'F value']] <- tempResult[['statistic']]
    welchResult[["Residuals", 'Df']] <- tempResult[['parameter']][[2]]
    welchResult[['Mean Sq']] <- welchResult[['Sum Sq']] / welchResult[['Df']]

    if (options$VovkSellkeMPR) {
      welchResult[["VovkSellkeMPR"]] <-  ifelse(!is.na(welchResult[['Pr(>F)']]),
                                                VovkSellkeMPR(na.omit(welchResult[["Pr(>F)"]])), NA)
    }

    anovaResult[['welchResult']] <- welchResult
  }

  # Save model to state
  anovaContainer[["anovaResult"]] <- createJaspState(object = anovaResult)
  anovaContainer[["anovaResult"]]$dependOn(c("sumOfSquares", "homogeneityBrown", "homogeneityWelch",
                                             "homogeneityNone", "effectSizeEstimates", "effectSizeEtaSquared",
                                              "effectSizePartialEtaSquared", "effectSizeOmegaSquared"))
}

.anovaTable <- function(anovaContainer, options, ready) {
  if (!is.null(anovaContainer[["anovaTable"]]))
    return()

  title <- ifelse(is.null(options$covariates), gettext("ANOVA"), gettext("ANCOVA"))
  anovaTable <- createJaspTable(title = title, position = 1,
                           dependencies = c("homogeneityWelch", "homogeneityBrown", "homogeneityNone",
                                            "VovkSellkeMPR", "effectSizeEstimates", "effectSizeEtaSquared",
                                            "effectSizePartialEtaSquared", "effectSizeOmegaSquared"))

  corrections <- c("None", "Brown-Forsythe", "Welch")[c(options$homogeneityNone,
                                                        options$homogeneityBrown,
                                                        options$homogeneityWelch)]

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

  if (options$VovkSellkeMPR) {
    anovaTable$addColumnInfo(title = gettextf("VS-MPR%s", "\u002A"), name = "VovkSellkeMPR", type = "number")
    anovaTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }

  if (options$effectSizeEstimates) {

    if (options$effectSizeEtaSquared) {
      anovaTable$addColumnInfo(title = "\u03B7\u00B2", name = "eta", type = "number")
    }

    if (options$effectSizePartialEtaSquared) {
      anovaTable$addColumnInfo(title = "\u03B7\u00B2\u209A", name = "etaPart", type = "number")
    }

    if (options$effectSizeOmegaSquared) {
      anovaTable$addColumnInfo(title = "\u03C9\u00B2", name = "omega", type = "number")
    }

  }

  anovaTable$showSpecifiedColumnsOnly <- TRUE

  # set the type footnote already
  typeFootnote <- switch(options$sumOfSquares,
                         type1 = gettext("Type I Sum of Squares"),
                         type2 = gettext("Type II Sum of Squares"),
                         type3 = gettext("Type III Sum of Squares"))
  anovaTable$addFootnote(typeFootnote)

  anovaContainer[["anovaTable"]] <- anovaTable

  if (!ready || anovaContainer$getError())
    return()

  anovaTable$title <- paste0(title, " - ", options$dependent)

  anovaTable$setExpectedSize(rows = length(options$modelTerms) * length(corrections))

  # here we ask for the model to be computed
  .anovaResult(anovaContainer, options)

  if ((options$homogeneityBrown || options$homogeneityWelch) && length(options$modelTerms) > 1) {
    anovaTable$setError(gettext("The Brown-Forsythe and Welch corrections are only available for one-way ANOVA"))
    return()
  }

  if (anovaContainer$getError())
    return()

  model <- anovaContainer[["anovaResult"]]$object
  anovaTable$setData(do.call("rbind", model))

  return()
}

.anovaContrastsTable <- function(anovaContainer, dataset, options, ready) {
  if (!is.null(anovaContainer[["contrastContainer"]]) || all(grepl("none", options$contrasts)))
    return()

  contrastContainer <- createJaspContainer(title = gettext("Contrast Tables"))
  contrastContainer$dependOn(c("contrasts", "confidenceIntervalIntervalContrast",
                               "confidenceIntervalsContrast", "customContrasts"))

  for (contrast in options$contrasts) {

    if (contrast$contrast != "none") {
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

  if (!ready || anovaContainer$getError())
    return()

  afexModel <- anovaContainer[["afexModel"]]$object

  ## Computation
  model <- anovaContainer[["model"]]$object
  contrastSummary <- summary.lm(model)[["coefficients"]]

  for (contrast in options$contrasts) {

    contrastContainerName <- paste0(contrast$contrast, "Contrast_",  paste(contrast$variable, collapse = ":"))

    if (contrast$contrast != "none") {

      variable <- contrast$variable
      v <- .v(variable)

      if (contrast$contrast == "custom") {
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

      contrastMatrix    <- .createContrastAnova(column, contrast$contrast, customContrastSetup)

      if (contrast$contrast != "custom") {
        cases <- .anovaContrastCases(column, contrast$contrast)
        contrCoef         <- lapply(as.data.frame(contrastMatrix), as.vector)
        names(contrCoef)  <- cases
      } else {
        contrCoef         <- apply(contrastMatrix, 1, list)
      }

      referenceGrid <- emmeans::emmeans(afexModel, v, model = "multivariate")
      contrastResult    <- try(emmeans::contrast(referenceGrid, contrCoef), silent = TRUE)
      # is input the same as used by emmeans?
      # all(as.matrix( coef(contrastResult)[, -(1:length(v)) ]) == t(contrastMatrix))

      if (contrast$contrast == "custom") {
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

      contrastResult <- cbind(contrastResult, confint(contrastResult, level = options$confidenceIntervalIntervalContrast)[,5:6])
      contrastResult[["Comparison"]] <- .unv(contrastResult[["contrast"]])
      contrastResult[[".isNewGroup"]] <- c(TRUE, rep(FALSE, nrow(contrastResult)-1))

      if (contrast$contrast == "custom" | length(contrast$variable) > 1) {
        contrastResult$Comparison <- 1:nrow(contrastResult)
        weightType <-  if (all(apply(contrastMatrix, 2, function(x) x %% 1 == 0))) "integer" else "number"
        contrastContainer[[contrastContainerName]][["customCoefTable"]] <- .createCoefficientsTableAnova(contrast,
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


.createContrastTableAnova <- function(myTitle, options, dfType = "integer") {

  contrastTable <- createJaspTable(title = myTitle)
  contrastTable$addColumnInfo(name = "Comparison", type = "string")
  contrastTable$addColumnInfo(name = "estimate", title=gettext("Estimate"), type = "number")

  if (options$confidenceIntervalsContrast) {

    thisOverTitle <- gettextf("%s%% CI for Mean Difference", options$confidenceIntervalIntervalContrast * 100)
    contrastTable$addColumnInfo(name="lower.CL", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
    contrastTable$addColumnInfo(name="upper.CL", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)

  }

  contrastTable$addColumnInfo(name = "SE", title=gettext("SE"), type = "number")
  contrastTable$addColumnInfo(name = "df",      title = gettext("df"), type = dfType)
  contrastTable$addColumnInfo(name = "t.ratio", title = gettext("t"),  type = "number")
  contrastTable$addColumnInfo(name = "p.value", title = gettext("p"),  type = "pvalue")

  contrastTable$showSpecifiedColumnsOnly <- TRUE

  return(contrastTable)
}

.createCoefficientsTableAnova <- function(contrast, contrCoef, weightType = "number") {

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

.anovaOrdinalRestrictions <- function(anovaContainer, dataset, options, ready) {
  if (!ready) return()

  restrictedModels <- options[["restrictedModels"]]
  restrictedModels <- restrictedModels[vapply(restrictedModels, function(mod) mod[["restrictionSyntax"]] != "", logical(1))]
  if (length(restrictedModels) == 0L) return()

  ordinalRestrictionsContainer <- createJaspContainer(title = gettext("Order Restrictions"))
  anovaContainer[["ordinalRestrictions"]] <- ordinalRestrictionsContainer

  baseModel <- .anovaOrdinalRestrictionsCalcBaseModel(ordinalRestrictionsContainer, dataset, options)

  modelList  <- lapply(restrictedModels,
                       .anovaOrdinalRestrictionsComputeModel,
                       baseModel = baseModel$fit,
                       container = ordinalRestrictionsContainer,
                       dataset   = dataset,
                       options   = options)

  .anovaOrdinalRestrictionsGetSyntaxErrors(modelList, ordinalRestrictionsContainer)

  modelNames <- vapply(restrictedModels, "[[", "modelName", FUN.VALUE = character(1))
  names(modelList) <- modelNames

  compareGoric <- .anovaOrdinalRestrictionsCompareModels(modelList, ordinalRestrictionsContainer, options)
  .ordinalRestrictionsCreateComparisonTable(compareGoric, modelNames, ordinalRestrictionsContainer, type = "goric", options)

  if (options[["restrictedModelComparisonCoefficients"]])
    .anovaOrdinalRestrictionsCreateCoefficientsTable(compareGoric, modelNames, ordinalRestrictionsContainer, options)

  if (length(options[["restrictedModelMarginalMeansTerm"]]) > 0L) {
    modelSummaryList <- .anovaOrdinalRestrictionsCalcModelSummaries(modelList, modelNames, baseModel, dataset, ordinalRestrictionsContainer, options)
    .ordinalRestrictionsCreateModelSummaryTables(modelSummaryList, ordinalRestrictionsContainer, type = "goric", options)
  }

  if (any(sapply(restrictedModels, function(mod) mod[["informedHypothesisTest"]])))
    .anovaOrdinalRestrictionsCreateInformedHypothesisTestTables(modelList, modelNames, ordinalRestrictionsContainer, options)

  if (length(options[["plotRestrictedModels"]]) > 0L && length(options[["restrictedModelMarginalMeansTerm"]]) > 0L)
    .ordinalRestrictionsCreateMarginalMeansPlot(modelSummaryList, modelNames, ordinalRestrictionsContainer, options)

  return()
}

.anovaOrdinalRestrictionsCalcBaseModel <- function(ordinalRestrictionsContainer, dataset, options) {
  if (!is.null(ordinalRestrictionsContainer[["baseModel"]])) return()

  baseModel <- createJaspState()
  baseModel$dependOn(c("includeIntercept"))
  ordinalRestrictionsContainer[["baseModel"]] <- baseModel

  reorderModelTerms <- .reorderModelTerms(options)
  modelTerms        <- reorderModelTerms$modelTerms
  modelDef          <- .modelFormula(modelTerms, options)

  if (options[["includeIntercept"]]) {
    modelFormula <- as.formula(modelDef[["model.def"]])
  } else {
    modelFormula <- as.formula(paste(modelDef[["model.def"]], "- 1"))
  }

  WLS <- NULL
  if (options$wlsWeights == "")
    WLS <- dataset[[options[["wlsWeights"]]]]

  fit <- lm(modelFormula, dataset)

  xLevels   <- fit[["xlevels"]]
  coefNames <- names(coef(fit))
  terms     <- strsplit(coefNames, ":")

  for (i in seq_along(xLevels)) {
    idxFac  <- which(stringr::str_detect(coefNames, names(xLevels)[i]))
    lvls    <- if (options[["includeIntercept"]]) xLevels[[i]][-1] else xLevels[[i]]
    newLvls <- numeric(length(idxFac))
    newLvls[1:length(idxFac)] <- lvls

    for (j in seq_along(idxFac)) {
      idxInt <- which(stringr::str_detect(terms[[idxFac[j]]], names(xLevels)[i]))
      terms[[idxFac[j]]][idxInt] <- paste0(names(xLevels)[i], newLvls[j])
    }
  }

  names(fit[["coefficients"]]) <- vapply(terms, paste, collapse = ":", FUN.VALUE = character(1))

  baseModel$object <- fit

  return(list(fit = fit, modelFormula = modelFormula))
}

# the following two functions should not be necessary if the QML component
# for the restrictions encodes the column names
.anovaOrdinalRestrictionsGetUsedVars <- function(syntax, availablevars) {
  allVars <- decodeColNames(availablevars)
  inSyntax <- stringr::str_detect(syntax, pattern = allVars)
  return(allVars[inSyntax])
}

.anovaOrdinalRestrictionsTranslateSyntax <- function(syntax, dataset, options) {
  usedvars <- .anovaOrdinalRestrictionsGetUsedVars(syntax, colnames(dataset))

  new.names <- encodeColNames(usedvars)

  for (i in seq_along(usedvars)) {
    syntax <- try(gsub(usedvars[i], new.names[i], syntax))
  }

  return(syntax)
}

.anovaOrdinalRestrictionsComputeModel <- function(model, baseModel, container, dataset, options) {
  modelName <- model[["modelName"]]
  if (!is.null(container[[modelName]]) || model[["restrictionSyntax"]] == "") return()

  translatedSyntax <- .anovaOrdinalRestrictionsTranslateSyntax(model[["restrictionSyntax"]], dataset)

  if (any(vapply(translatedSyntax, isTryError, logical(1)))) {
    container$setError(gettextf("There are errors in the restriction syntax for %s.", modelName))
    return()
  }

  newModel <- createJaspState()
  restrictedModel <- try(restriktor::restriktor(baseModel, constraints = translatedSyntax))
  newModel$object <- restrictedModel
  container[[modelName]] <- newModel

  return(restrictedModel)
}

.anovaOrdinalRestrictionsGetSyntaxErrors <- function(modelList, ordinalRestrictionsContainer) {
  if(ordinalRestrictionsContainer$getError()) return()

  modelHasErrors <- vapply(modelList, isTryError, logical(1))

  if (any(modelHasErrors)) {

    syntaxErrors   <- sapply(modelList[modelHasErrors], function(msg) {

      msgSplit      <- stringr::str_split(msg, "lavaan ERROR: ")
      msgExtracted  <- msgSplit[[1]][length(msgSplit[[1]])]
      msgExtracted  <- gsub("\n", "", msgExtracted)
      msgFinal      <- gettextf("Syntax error: %1$s%2$s.", toupper(substr(msgExtracted, 1, 1)), substr(msgExtracted, 2, nchar(msgExtracted)))

      return(msgFinal)
    })

    finMsg <- paste(unlist(syntaxErrors), collapse = "\n")
    ordinalRestrictionsContainer$setError(finMsg)
  }
  return()
}

.anovaOrdinalRestrictionsCompareModels <- function(modelList, ordinalRestrictionsContainer, options) {
  if (!is.null(ordinalRestrictionsContainer[["modelComparison"]]) || ordinalRestrictionsContainer$getError()) return()

  modelComparison <- createJaspState()
  modelComparison$dependOn(c("restrictedModelComparison"))
  names(modelList)[1] <- "object"
  compareGoric <- do.call(restriktor::goric, c(modelList, comparison = options[["restrictedModelComparison"]]))
  modelComparison$object <- compareGoric
  ordinalRestrictionsContainer[["modelComparison"]] <- modelComparison

  return(compareGoric)
}

.ordinalRestrictionsCreateComparisonTable <- function(compareGoric, modelNames, ordinalRestrictionsContainer, type, options) {
  if (!is.null(ordinalRestrictionsContainer[["comparisonTable"]])) return()

  comparisonTable <- createJaspTable(gettext("Model Comparison Table"))
  comparisonTable$dependOn(c("restrictedModelComparison", "restrictedModelComparisonReference"))
  comparisonTable$addColumnInfo(name = "model",   title = gettext("Model"),   type = "string")
  comparisonTable$addColumnInfo(name = "loglik",  title = gettext("LL"),      type = "number")
  comparisonTable$addColumnInfo(name = "penalty", title = gettext("Penalty"), type = "number")
  if (type == "goric") {
    comparisonTable$addColumnInfo(name = "goric",         title = gettext("GORIC"),  type = "number")
    comparisonTable$addColumnInfo(name = "goric.weights", title = gettext("Weight"), type = "number")
  } else {
    comparisonTable$addColumnInfo(name = "gorica",         title = gettext("GORICA"), type = "number")
    comparisonTable$addColumnInfo(name = "gorica.weights", title = gettext("Weight"), type = "number")
  }
  comparisonTable$addColumnInfo(name = "ratio", title = gettext("Ratio"), type = "number")

  comparison <- options[["restrictedModelComparison"]]
  reference  <- options[["restrictedModelComparisonReference"]]
  abbrev     <- switch(type,
                       goric = gettext("GORIC = Generalized Order-Restricted Information Criterion (Kuiper, Hoijtink, & Silvapulle, 2011)."),
                               gettext("GORICA = Generalized Order-Restricted Information Criterion Approximation.")
                       )
  comparisonTable$addFootnote(gettextf('Ratios indicate the relative weight for each model against the "%1$s" model. %2$s', reference, abbrev))
  comparisonTable$addCitation(c("Kuiper, R. M., Hoijtink, H., Silvapulle, M. J. (2011). An Akaike-type information criterion for model selection under equality constarints. Biometrika, 98(2), 495-501.",
                                "Vanbrabant, L., Van Loey, N., & Kuiper, R. M. (2020). Evaluating a theory-based hypothesis against its complement using an AIC-type information criterion with an application to facial burn injury. Psychological Methods, 25(2), 129-142."))

  ordinalRestrictionsContainer[["comparisonTable"]] <- comparisonTable

  if (ordinalRestrictionsContainer$getError()) return()

  .ordinalRestrictionsComparisonTableFill(comparisonTable, compareGoric, modelNames, comparison, reference, type)

  return()
}

.ordinalRestrictionsComparisonTableFill <- function(comparisonTable, compareGoric, modelNames, comparison, reference, type) {
  nms <- switch(comparison,
                unconstrained = c(modelNames, gettext("Unconstrained")),
                complement    = c(modelNames, gettext("Complement")),
                modelNames
                )

  compareDf <- compareGoric[["result"]]
  compareDf[["model"]] <- nms

  if (type == "goric")
    weights <- compareDf[["goric.weights"]]
  else
    weights <- compareDf[["gorica.weights"]]

  ratio <- weights/weights[nms == reference]
  compareDf[["ratio"]] <- ratio

  if (length(nms) == 1)
    comparisonTable$addRows(as.list(compareDf))
  else
    comparisonTable$addColumns(as.list(compareDf))

  return()
}

.anovaOrdinalRestrictionsCreateCoefficientsTable <- function(compareGoric, modelNames, ordinalRestrictionsContainer, options) {
  if (!is.null(ordinalRestrictionsContainer[["coefficientsTable"]])) return()

  coefTable <- createJaspTable(gettext("Model Coefficients Table"))
  coefTable$dependOn(c("restrictedModelComparison", "restrictedModelComparisonCoefficients", "highlightEstimates"))
  overtitle <- gettext("Estimates")
  coefTable$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "string")

  for (name in modelNames) {
    coefTable$addColumnInfo(name = name, title = name, type = "number", overtitle = overtitle)
  }

  compareAgainst <- options[["restrictedModelComparison"]]

  if (compareAgainst == "complement")
    coefTable$addColumnInfo(name = "complement", title = gettext("Complement"), type = "number", overtitle = overtitle)

  coefTable$addColumnInfo(name = "unconstrained", title = gettext("Unconstrained"), type = "number", overtitle = overtitle)

  ordinalRestrictionsContainer[["coefficientsTable"]] <- coefTable

  if (ordinalRestrictionsContainer$getError()) return()

  .anovaOrdinalRestrictionsCoefficientsTableFill(coefTable, compareGoric, modelNames, compareAgainst, options[["includeIntercept"]], options[["highlightEstimates"]])

  return()
}

.anovaOrdinalRestrictionsCoefficientsTableFill <- function(coefTable, compareGoric, modelNames, compareAgainst, includeIntercept, highlightEstimates) {

  xLevels    <- compareGoric[["model.org"]][["xlevels"]]
  coefs      <- coef(compareGoric)
  coefNames  <- names(coefs)
  terms      <- strsplit(coefNames, ":")

  if (compareAgainst != "unconstrained")
    coefs    <- rbind(coefs, coef(compareGoric[["model.org"]]))

  for (i in seq_along(xLevels)) {
    idxFac  <- which(stringr::str_detect(coefNames, names(xLevels)[i]))
    idxLvl  <- if (includeIntercept || i > 1) 1:(length(xLevels[[i]])-1) else 1:length(xLevels[[i]])
    newLvls <- numeric(length(idxFac))
    newLvls[1:length(idxFac)] <- idxLvl

    for (j in seq_along(idxFac)) {
      idxInt <- which(stringr::str_detect(terms[[idxFac[j]]], names(xLevels)[i]))

      terms[[idxFac[j]]][idxInt] <- paste0(names(xLevels)[i], " (", newLvls[j], ")")
    }
  }

  coefNamesPretty  <- sapply(terms, paste, collapse = jaspBase::interactionSymbol)
  coefNamesPrettyV <- decodeColNames(coefNamesPretty)
  coefDf           <- cbind.data.frame(coef = coefNamesPrettyV, t(coefs), stringsAsFactors = FALSE)

  if (compareAgainst == "complement")
    names(coefDf) <- c("coef", modelNames, compareAgainst, "unconstrained")
  else
    names(coefDf) <- c("coef", modelNames, "unconstrained")

  coefTable$setData(coefDf)

  if (highlightEstimates)
    .ordinalRestrictionsCoefficientsTableAddDifferenceSymbols(coefTable, coefDf[,-1])

  return()
}

.ordinalRestrictionsCoefficientsTableAddDifferenceSymbols <- function(coefTable, df) {
  diff     <- which(round(df[["unconstrained"]], 3) != round(df, 3), arr.ind = TRUE)
  colNames <- names(df)[diff[,2]]
  rowNames <- row.names(df)[diff[,1]]

  coefTable$addFootnote(message  = gettext("Coefficients differ from unconstrained model."),
                        symbol   = "\u2020",
                        colNames = colNames,
                        rowNames = rowNames)

  return()
}

.anovaOrdinalRestrictionsCalcModelSummaries <- function(modelList, modelNames, baseModel, dataset, container, options) {
  if (!is.null(container[["modelSummaries"]]) || container$getError()) return()

  modelSummaryContainer <- createJaspContainer()
  modelSummaryContainer$dependOn(c("restrictedConfidenceIntervalLevel",
                                   "restrictedModelMarginalMeansTerm",
                                   "restrictedModelHeteroskedasticity",
                                   "restrictedConfidenceIntervalBootstrap",
                                   "restrictedConfidenceIntervalBootstrapSamples"))

  ciLvl  <- options[["restrictedConfidenceIntervalLevel"]]

  modelTerm  <- unlist(options[["restrictedModelMarginalMeansTerm"]])
  emmTerm    <- paste(modelTerm, collapse = ":")
  emmFormula <- as.formula(paste("~", emmTerm))

  modelSummaryList <- list()

  for (name in modelNames) {
    newState <- createJaspState()
    modelSummaryContainer[[name]] <- newState
    newModel <- modelList[[name]]
    if (options[["restrictedModelHeteroskedasticity"]] == "none")
       newVcov <- attr(newModel[["information"]], "inverted")
    else
      newVcov <- restriktor:::sandwich(newModel,
                                      bread. = restriktor:::bread.conLM(newModel),
                                      meat.  = restriktor:::meatHC(newModel, type = options[["restrictedModelHeteroskedasticity"]]))

    newEmmObj <- emmeans::qdrg(
      formula = formula(newModel[["model.org"]]),
      data    = newModel[["model.org"]][["model"]],
      coef    = newModel[["b.restr"]],
      vcov    = newVcov,
      df      = newModel[["df.residual"]]
    )

    newEmmSummary <- summary(emmeans::lsmeans(newEmmObj, emmFormula), level = ciLvl)

    for (i in 1:(ncol(newEmmSummary)-5)) {
      newEmmSummary[,i] <- as.character(newEmmSummary[,i])
    }

    if (options[["restrictedConfidenceIntervalBootstrap"]]) {
      samples <- options[["restrictedConfidenceIntervalBootstrapSamples"]]
      startProgressbar(samples,
                       label = paste(gettext("Bootstrapping Restricted Marginal Means:"), name))
      bootstrapEmm <- try(boot::boot(data = dataset,
                                     statistic = .anovaOrdinalRestrictionsBootstrapMarginalMeans,
                                     R = samples,
                                     baseModel = baseModel[["fit"]],
                                     restrModel = newModel,
                                     emmFormula = emmFormula,
                                     comparison = "restricted"))

      if (class(bootstrapEmm) == "try-error") {
        modelSummaryList[[name]] <- bootstrapEmm
        next
      }

      bootstrapSummary <- summary(bootstrapEmm)

      bootstrapEmmCI <- t(sapply(1:nrow(bootstrapSummary), function(index) {
        res <- try(boot::boot.ci(boot.out = bootstrapEmm, conf = ciLvl, type = "bca",
                             index = index)[['bca']][1,4:5])
        if (!isTryError(res)){
          return(res)
        } else {
          return(c(NA, NA))
        }
      }))

      newEmmSummary[["SE"]]       <- bootstrapSummary[["bootSE"]]
      newEmmSummary[["lower.CL"]] <- bootstrapEmmCI[,1]
      newEmmSummary[["upper.CL"]] <- bootstrapEmmCI[,2]
    }
    modelSummaryList[[name]] <- newEmmSummary
    newState$object <- newEmmSummary
  }

  if (options[["restrictedModelComparison"]] == "unconstrained") {
    unrestrState  <- createJaspState()
    modelSummaryContainer[["Unconstrained"]] <- unrestrState
    if (options[["restrictedModelHeteroskedasticity"]] == "none")
      unrestrVcov <- vcov(baseModel[["fit"]])
    else
      unrestrVcov <- sandwich::sandwich(baseModel[["fit"]],
                                        bread. = sandwich::bread(baseModel[["fit"]]),
                                        meat. = sandwich::meatHC(baseModel[["fit"]], type = options[["restrictedModelHeteroskedasticity"]]))

    unrestrEmmObj <- emmeans::qdrg(
      formula = baseModel[["modelFormula"]],
      data    = baseModel[["fit"]][["model"]],
      coef    = coef(baseModel[["fit"]]),
      vcov    = unrestrVcov,
      df      = baseModel[["fit"]][["df.residual"]]
    )

    unrestrEmmSummary <- summary(emmeans::lsmeans(unrestrEmmObj, emmFormula), level = ciLvl)

    for (i in 1:(ncol(unrestrEmmSummary)-5)) {
      unrestrEmmSummary[,i] <- as.character(unrestrEmmSummary[,i])
    }

    if (options[["restrictedConfidenceIntervalBootstrap"]]) {
      samples <- options[["restrictedConfidenceIntervalBootstrapSamples"]]
      startProgressbar(samples,
                       label = paste(gettext("Bootstrapping Restricted Marginal Means:"), "Unconstrained"))
      bootstrapEmm <- try(boot::boot(data = dataset,
                                     statistic = .anovaOrdinalRestrictionsBootstrapMarginalMeans,
                                     R = samples,
                                     baseModel = baseModel[["fit"]],
                                     restrModel = NULL,
                                     emmFormula = emmFormula,
                                     comparison = "unrestricted"))

      if (!isTryError(bootstrapEmm)) {
        bootstrapSummary <- summary(bootstrapEmm)
        bootstrapEmmCI <- t(sapply(1:nrow(bootstrapSummary), function(index) {
          res <- try(boot::boot.ci(boot.out = bootstrapEmm, conf = ciLvl, type = "bca",
                              index = index)[['bca']][1,4:5])
          return(res)
        }))
        if (!isTryError(bootstrapEmmCI)) {
          unrestrEmmSummary[["SE"]] <- bootstrapSummary[["bootSE"]]
          unrestrEmmSummary[["lower.CL"]] <- bootstrapEmmCI[,1]
          unrestrEmmSummary[["upper.CL"]] <- bootstrapEmmCI[,2]
        } else {
          modelSummaryContainer[["Unconstrained"]]$setError(bootstrapEmmCI)
        }
      } else {
        modelSummaryList[["Unconstrained"]] <- gettext("Some confidence intervals could not be computed. Possibly too few bootstrap replicates.")
      }
    }
    modelSummaryList[["Unconstrained"]] <- unrestrEmmSummary
    unrestrState$object <- unrestrEmmSummary
  }

  if (options[["restrictedModelComparison"]] == "complement") {
    complementState  <- createJaspState()
    modelSummaryContainer[["Complement"]] <- complementState
    goricObj <- container[["modelComparison"]]$object

    complementEmmObj <- emmeans::qdrg(
      formula = baseModel[["modelFormula"]],
      data    = baseModel[["fit"]][["model"]],
      coef    = as.numeric(coef(goricObj)[row.names(coef(goricObj)) == "complement",]),
      vcov    = vcov(baseModel[["fit"]]),
      df      = baseModel[["fit"]][["df.residual"]]
    )

    complementEmmSummary <- summary(emmeans::lsmeans(complementEmmObj, emmFormula), level = ciLvl)

    for (i in 1:(ncol(complementEmmSummary)-5)) {
      complementEmmSummary[,i] <- as.character(complementEmmSummary[,i])
    }

    samples <- options[["restrictedConfidenceIntervalBootstrapSamples"]]
    startProgressbar(samples,
                     label = gettextf("Bootstrapping Restricted Marginal Means: %s", "Complement"))
    bootstrapEmm <- try(boot::boot(data = dataset,
                                   statistic = .anovaOrdinalRestrictionsBootstrapMarginalMeans,
                                   R = samples,
                                   baseModel = baseModel[["fit"]],
                                   restrModel = goricObj,
                                   emmFormula = emmFormula,
                                   comparison = "complement"))

    if (!isTryError(bootstrapEmm)) {
      bootstrapSummary <- summary(bootstrapEmm)
      bootstrapEmmCI <- t(sapply(1:nrow(bootstrapSummary), function(index) {
        res <- try(boot::boot.ci(boot.out = bootstrapEmm, conf = ciLvl, type = "bca",
                                 index = index)[['bca']][1,4:5])
        return(res)
      }))
      if (!isTryError(bootstrapEmmCI)) {
        complementEmmSummary[["SE"]] <- bootstrapSummary[["bootSE"]]
        complementEmmSummary[["lower.CL"]] <- bootstrapEmmCI[,1]
        complementEmmSummary[["upper.CL"]] <- bootstrapEmmCI[,2]
      } else {
        modelSummaryContainer[["Complement"]]$setError(bootstrapEmmCI)
      }
    } else {
      modelSummaryList[["Complement"]] <- gettext("Some confidence intervals could not be computed. Possibly too few bootstrap replicates.")
    }
    modelSummaryList[["Complement"]] <- complementEmmSummary
    complementState$object <- complementEmmSummary
  }

  container[["modelSummaries"]] <- modelSummaryContainer

  return(modelSummaryList)
}

.anovaOrdinalRestrictionsBootstrapMarginalMeans <- function(data, indices, baseModel, restrModel, emmFormula, comparison) {

  resamples   <- data[indices, , drop = FALSE]
  baseModelRefit  <- update(baseModel, formula = formula(baseModel), data = resamples)

  xLevels   <- baseModelRefit[["xlevels"]]
  coefNames <- names(coef(baseModelRefit))
  terms     <- strsplit(coefNames, ":")

  for (i in seq_along(xLevels)) {
    idxFac  <- which(stringr::str_detect(coefNames, names(xLevels)[i]))
    lvls    <- xLevels[[i]][-1]
    newLvls <- numeric(length(idxFac))
    newLvls[1:length(idxFac)] <- lvls

    for (j in seq_along(idxFac)) {
      idxInt <- which(stringr::str_detect(terms[[idxFac[j]]], names(xLevels)[i]))
      terms[[idxFac[j]]][idxInt] <- paste0(names(xLevels)[i], newLvls[j])
    }
  }

  names(baseModelRefit[["coefficients"]]) <- sapply(terms, paste, collapse = ":")

  if (comparison == "unrestricted") {
    emmObj <- emmeans::lsmeans(baseModelRefit, emmFormula, data = resamples)
  } else if (comparison == "complement") {
    modelList <- restrModel[["objectList"]]
    modelListRefit <- lapply(modelList, function(mod) {
      constraints     <- mod[["CON"]][["constraints"]]
      restrModelRefit <- restriktor::restriktor(baseModelRefit, constraints = constraints)
      return(restrModelRefit)
    })
    names(modelListRefit)[1] <- "object"
    newGoricObj <- do.call(restriktor::goric, c(modelListRefit, comparison = "complement"))
    emmObj <- emmeans::qdrg(
      formula = formula(baseModel),
      data    = resamples,
      coef    = as.numeric(coef(newGoricObj)[row.names(coef(newGoricObj)) == "complement",]),
      vcov    = vcov(baseModel),
      df      = baseModel[["df.residual"]]
    )
  } else {
    constraints <- restrModel[["CON"]][["constraints"]]
    restrModelRefit <- restriktor::restriktor(baseModelRefit, constraints = constraints)
    emmObj <- emmeans::qdrg(
      formula = formula(baseModel),
      data    = resamples,
      coef    = restrModelRefit[["b.restr"]],
      vcov    = attr(restrModelRefit[["information"]], "inverted"),
      df      = restrModelRefit[["df.residual"]]
    )
  }
  emmSummary <- summary(emmeans::lsmeans(emmObj, emmFormula))
  progressbarTick()
  return(emmSummary[["lsmean"]])
}

.anovaOrdinalRestrictionsInformedHypothesisTests <- function(modelList, modelNames, options) {

  whichIhts <- sapply(options[["restrictedModels"]], function(mod) mod[["informedHypothesisTest"]])
  names(whichIhts) <- modelNames

  ihts <- list()

  for (name in modelNames) {
    if (whichIhts[[name]]) {
      model <- modelList[[name]]
      constraints <- model[["CON"]][["constraints"]]
      ihts[[name]] <- restriktor::iht(model, constraints = constraints)
    }
  }

  return(ihts)
}

.anovaOrdinalRestrictionsCreateInformedHypothesisTestTables <- function(modelList, modelNames, ordinalRestrictionsContainer, options) {
  if (!is.null(ordinalRestrictionsContainer[["informedHypothesisTests"]]) || ordinalRestrictionsContainer$getError()) return()

  ihtsContainer <- createJaspContainer(gettext("Informed Hypothesis Tests"))
  ihtsContainer$dependOn(c("informedHypothesisTest"))
  ordinalRestrictionsContainer[["informedHypothesisTests"]] <- ihtsContainer

  ihtsList   <- .anovaOrdinalRestrictionsInformedHypothesisTests(modelList, modelNames, options)
  whichTests <- lapply(options[["restrictedModels"]], function(mod) list("global" = mod[["informedHypothesisTestGlobal"]],
                                                                         "A"      = mod[["informedHypothesisTestA"]],
                                                                         "B"      = mod[["informedHypothesisTestB"]],
                                                                         "C"      = FALSE))
  names(whichTests) <- modelNames

  for (name in names(ihtsList)) {
    if (is.null(ihtsContainer[[name]])) {
      newContainer <- createJaspContainer(name)
      ihtObject <- ihtsList[[name]]

      if (all(sapply(ihtObject, function(t) is.list(t)))) {
        newConstraintTable <- createJaspTable(gettext("Restriction Summary"))
        newConstraintTable$addColumnInfo(name = "restriction", title = gettext("Restriction"), type = "string")
        newConstraintTable$addCitation(.anovaOrdinalRestrictionsInformedHypothesisTestsCitations)
        .anovaOrdinalRestrictionsConstraintTableFill(newConstraintTable, ihtObject[[1]])
        newContainer[["constraintTable"]] <- newConstraintTable

        newReductionTable <- createJaspTable(gettext("Reduction Summary"))
        newReductionTable$addColumnInfo(name = "unrestricted", title = gettextf("Unrestricted R%s", "\u00B2"), type = "number")
        newReductionTable$addColumnInfo(name = "restricted", title = gettextf("Restricted R%s", "\u00B2"), type = "number")
        newReductionTable$addCitation(.anovaOrdinalRestrictionsInformedHypothesisTestsCitations)
        .anovaOrdinalRestrictionsReductionTableFill(newReductionTable, ihtObject[[1]])
        newContainer[["reductionTable"]] <- newReductionTable

        for (type in names(ihtObject)) {
          if (whichTests[[name]][[type]]) {
            newTest <- ihtObject[[type]]

            newTestTable <- createJaspTable(paste0(gettext("Test Type "), type))
            newTestTable$addColumnInfo(name = "stat", title = gettext("F\u0305"), type = "number")
            newTestTable$addColumnInfo(name = "df", title = gettext("df"), type = "integer")
            newTestTable$addColumnInfo(name = "dfresid", title = gettext("Residual df"), type = "integer")
            newTestTable$addColumnInfo(name = "pval", title = gettext("p"), type = "pvalue")
            newTestTable$addCitation(.anovaOrdinalRestrictionsInformedHypothesisTestsCitations)

            .anovaOrdinalRestrictionsInformedHypothesisTableFill(newTestTable, newTest)

            if (type == "global") {
              newReductionTable$dependOn(c("informedHypothesisTestGlobal"))
              newTestTable$addFootnote("H0: All parameters are restricted to be equal. HA: At least one inequality restriction is stricly true.")
            } else if (type == "A") {
              newReductionTable$dependOn(c("informedHypothesisTestA"))
              newTestTable$addFootnote("H0: All restrictions are equalities. HA: At least one inequality restriction is stricly true.")
            } else if (type == "B") {
              newReductionTable$dependOn(c("informedHypothesisTestGlobalB"))
              newTestTable$addFootnote("H0: All restrictions hold in the population. HA: At least one restriction is violated.")
            }

            newContainer[[type]] <- newTestTable
          }
        }
      } else {
        newConstraintTable <- createJaspTable(gettext("Restriction Summary"))
        newConstraintTable$addColumnInfo(name = "restriction", title = gettext("Restriction"), type = "string")
        newConstraintTable$addCitation(.anovaOrdinalRestrictionsInformedHypothesisTestsCitations)
        .anovaOrdinalRestrictionsConstraintTableFill(newConstraintTable, ihtObject)
        newContainer[["constraintTable"]] <- newConstraintTable

        newReductionTable <- createJaspTable(gettext("Reduction Summary"))
        newReductionTable$addColumnInfo(name = "unrestricted", title = gettextf("Unrestricted R%s", "\u00B2"), type = "number")
        newReductionTable$addColumnInfo(name = "restricted", title = gettextf("Restricted R%s", "\u00B2"), type = "number")
        newReductionTable$addCitation(.anovaOrdinalRestrictionsInformedHypothesisTestsCitations)
        .anovaOrdinalRestrictionsReductionTableFill(newReductionTable, ihtObject)
        newContainer[["reductionTable"]] <- newReductionTable

        newTestTable <- createJaspTable(gettext("Classical Test"))
        newTestTable$addColumnInfo(name = "stat", title = gettext("F"), type = "number")
        newTestTable$addColumnInfo(name = "df", title = gettext("df"), type = "integer")
        newTestTable$addColumnInfo(name = "dfresid", title = gettext("Residual df"), type = "integer")
        newTestTable$addColumnInfo(name = "pval", title = gettext("p"), type = "pvalue")
        newTestTable$addFootnote("H0: All restrictions are active (==). HA: At least one equality restriction is violated. The classical test is computed when only equality restrictions are specified.")
        newTestTable$addCitation(.anovaOrdinalRestrictionsInformedHypothesisTestsCitations)

        .anovaOrdinalRestrictionsInformedHypothesisTableFill(newTestTable, ihtObject)
        newContainer[["classical"]] <- newTestTable
      }

      ihtsContainer[[name]] <- newContainer
    }
  }

  return()
}

.anovaOrdinalRestrictionsConstraintTableFill <- function(table, test) {
  if (is.null(test[["CON"]]))
    syntax <- test[["constraints"]]
  else
    syntax <- test[["CON"]][["constraints"]]

  restrictions <- gsub("\n", ";", gsub("<", " < ", gsub(">", " > ", gsub("==", " = ", gsub(" ", "", syntax)))))
  restrictions <- strsplit(restrictions, ";")

  restrictionsList <- list(restriction = restrictions[[1]])

  table$addColumns(restrictionsList)

  return()
}

.anovaOrdinalRestrictionsReductionTableFill <- function(table, test) {
  reductionList <- list(unrestricted = test[["R2.org"]],
                        restricted = test[["R2.reduced"]])

  table$addRows(reductionList)

  return()
}

.anovaOrdinalRestrictionsInformedHypothesisTableFill <- function(table, test) {
  ihtDf <- list(
    stat = test[["Ts"]],
    df = ifelse(is.null(test[["df"]]), nrow(test[["Amat"]]), test[["df"]]),
    dfresid = test[["df.residual"]],
    pval = test[["pvalue"]]
  )

  table$addRows(ihtDf)

  return()
}

.ordinalRestrictionsCreateModelSummaryTables <- function(modelSummaryList, ordinalRestrictionsContainer, type, options) {
  if (!is.null(ordinalRestrictionsContainer[["modelSummaryTables"]]) || ordinalRestrictionsContainer$getError()) return()

  summaryContainer <- createJaspContainer(gettext("Model Summaries"))
  summaryContainer$dependOn(c("restrictedConfidenceIntervalLevel",
                              "restrictedModelMarginalMeansTerm",
                              "restrictedModelHeteroskedasticity",
                              "restrictedConfidenceIntervalBootstrap",
                              "restrictedConfidenceIntervalBootstrapSamples"))
  ordinalRestrictionsContainer[["modelSummaryTables"]] <- summaryContainer

  whichModels <- sapply(options[["restrictedModels"]], function(mod) mod[["modelSummary"]])
  ciLvl  <- options[["restrictedConfidenceIntervalLevel"]]
  isBoot <- options[["restrictedConfidenceIntervalBootstrap"]]
  nBoot  <- options[["restrictedConfidenceIntervalBootstrapSamples"]]

  for (name in names(modelSummaryList)[whichModels]) {
    if (is.null(summaryContainer[[name]])) {
      overtitle <- gettextf("%.0f%% CI", options[["restrictedConfidenceIntervalLevel"]] * 100)
      newSummaryTable <- createJaspTable(paste(gettext("Marginal Means -"), name))
      newSummaryObj   <- modelSummaryList[[name]]
      if (isTryError(newSummaryObj)) {
        newSummaryTable$setError(.extractErrorMessage(newSummaryObj))
        next
      } else if (isBoot && any(is.na(newSummaryObj[["lower.CL"]]) | is.na(newSummaryObj[["upper.CL"]]))) {
        newSummaryTable$addFootnote(message = gettext("Some confidence intervals could not be computed. Possibly too few successful bootstrap replicates."))
      }
      for (i in 1:(length(names(newSummaryObj)) - 5)) {
        newSummaryTable$addColumnInfo(name = names(newSummaryObj)[i], type = "string", combine = TRUE)
      }
      newSummaryTable$addColumnInfo(name = "lsmean", title = gettext("Marginal Mean"), type = "number")
      newSummaryTable$addColumnInfo(name = "SE", title = gettext("SE"), type = "number")
      newSummaryTable$addColumnInfo(name = "lower.CL", title = gettext("Lower"), type = "number", overtitle = overtitle)
      newSummaryTable$addColumnInfo(name = "upper.CL", title = gettext("Upper"), type = "number", overtitle = overtitle)
      if (isBoot || name == "Complement" || type == "gorica")
        newSummaryTable$addFootnote(gettextf("Bootstrapped SE and CI based on %i samples.", nBoot))
      newSummaryTable$showSpecifiedColumnsOnly <- TRUE

      newSummaryTable$addColumns(as.list(newSummaryObj))
      summaryContainer[[name]] <- newSummaryTable
    }
  }

  return()
}

.ordinalRestrictionsCreateMarginalMeansPlot <- function(modelSummaryList, modelNames, ordinalRestrictionsContainer, options) {
  if (!is.null(ordinalRestrictionsContainer[["marginalMeansPlotContainer"]]) || ordinalRestrictionsContainer$getError()) return()

  marginalMeansContainer <- createJaspContainer(title = gettext("Marginal Means Plots"))
  marginalMeansContainer$dependOn(c("plotRestrictedModels",
                                    "restrictedConfidenceIntervalLevel",
                                    "restrictedModelMarginalMeansTerm",
                                    "restrictedModelHeteroskedasticity",
                                    "restrictedConfidenceIntervalBootstrap",
                                    "restrictedConfidenceIntervalBootstrapSamples"))
  ordinalRestrictionsContainer[["marginalMeansPlotContainer"]] <- marginalMeansContainer

  factors <- encodeColNames(unlist(options[["restrictedModelMarginalMeansTerm"]]))

  if (length(factors) > 1) {
    factorX    <- factors[1]
    facNames   <- factors[2:length(factors)]
    args       <- lapply(factors[2:length(factors)], function(f) unique(modelSummaryList[[1]][[f]]))
    levelsGrid <- do.call(expand.grid, c(args, stringsAsFactors = FALSE))
    names(levelsGrid) <- facNames
    for (i in 1:nrow(levelsGrid)) {
      facLevels    <- levelsGrid[i,]
      thisPlotName <- paste(facNames, facLevels, sep = ": ", collapse = " - ")
      subPlot      <- createJaspPlot(title = thisPlotName, width = 480, height = 320)
      marginalMeansContainer[[thisPlotName]] <- subPlot
      p <- try(.ordinalRestrictionsMarginalMeansPlotFill(modelSummaryList, factorX, facNames, facLevels, options))
      if(isTryError(p))
        subPlot$setError(.extractErrorMessage(p))
      else
        subPlot$plotObject <- p
    }
  } else {
    singlePlot <- createJaspPlot(title = "", width = 480, height = 320)
    marginalMeansContainer[["marginalMeansPlotSingle"]] <- singlePlot
    p <- try(.ordinalRestrictionsMarginalMeansPlotFill(modelSummaryList, factors, NULL, NULL, options))
    if(isTryError(p))
      singlePlot$setError(.extractErrorMessage(p))
    else
      singlePlot$plotObject <- p
  }

  return()
}

.ordinalRestrictionsMarginalMeansPlotFill <- function(modelSummaryList, factorX, factors, levels, options) {

  modelsToPlot <- options[["plotRestrictedModels"]]
  modelsToPlot <- modelsToPlot[modelsToPlot %in% names(modelSummaryList)]

  plotDf <- NULL

  for (name in modelsToPlot) {
    newModelSummary <- modelSummaryList[[name]]
    newModelSummary$model <- name

    if (is.null(plotDf))
      plotDf <- newModelSummary
    else
      plotDf <- rbind(plotDf, newModelSummary)
  }

  if(options[["restrictedConfidenceIntervalBootstrap"]] &&
     any(is.na(plotDf[["lower.CL"]]) | is.na(plotDf[["upper.CL"]]))) {
    stop(gettext("Some confidence intervals could not be computed. Possibly too few successful bootstrap replicates."))
  }

  if (!is.null(factors) && !is.null(levels)) {
    for (i in 1:length(factors)) {
      plotDf <- plotDf[as.character(plotDf[[factors[i]]]) == c(levels[i]),]
    }
  }

  yBreaks <- pretty(range(plotDf[["lower.CL"]], plotDf[["upper.CL"]]))
  pd      <- ggplot2::position_dodge(0.4)

  p <- ggplot2::ggplot(plotDf, mapping = ggplot2::aes_string(x = factorX, y = "lsmean", ymin = "lower.CL", ymax = "upper.CL", color = "model")) +
    ggplot2::geom_errorbar(width = 0.2, position = pd) +
    ggplot2::geom_point(size = 3, position = pd) +
    ggplot2::scale_y_continuous(name = options[["dependent"]], breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::labs(color = "") +
    ggplot2::scale_color_brewer(palette="Dark2")

  p <- jaspGraphs::themeJasp(p, legend.position = "right")

  return(p)
}

.anovaOrdinalRestrictionsInformedHypothesisTestsCitations <- c(
  "Silvapulle, M. J. & Sen, P. K. (2005). Constrained statistical inference: Order, inequality, and shape constraints. Hoboken, NJ: Wiley.",
  "Vanbrabant, L. & Rosseel, Y. (2020). Restricted statistical estimation and inference for linear models. http://restriktor.org"
)

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
  if (length(options$postHocTestsVariables) == 0 || !ready)
    return()

  if (is.null(anovaContainer[["postHocContainer"]])) {

    postHocContainer <- createJaspContainer(title = gettext("Post Hoc Tests"))
    postHocContainer$dependOn(c("postHocTestsVariables"))
    anovaContainer[["postHocContainer"]] <- postHocContainer

  } else {

    postHocContainer <- anovaContainer[["postHocContainer"]]

  }

  if (options$postHocTestsTypeStandard)
    .anovaPostHocTable(postHocContainer, dataset, options,  anovaContainer[["model"]]$object)

  if (options$postHocTestsTypeGames)
    gamesPostHoc <- .anovaGamesTable(postHocContainer, dataset, options,  anovaContainer[["model"]]$object)

  if (options$postHocTestsTypeDunnett)
    dunnettPostHoc <- .anovaDunnettTable(postHocContainer, dataset, options,  anovaContainer[["model"]]$object)

  if (options$postHocTestsTypeDunn)
    dunnPostHoc <- .anovaDunnTable(postHocContainer, dataset, options,  anovaContainer[["model"]]$object)

  return()
}

.anovaPostHocTable <- function(postHocContainer, dataset, options, model) {
  if (!is.null(postHocContainer[["postHocStandardContainer"]]))
    return()

  postHocStandardContainer <- createJaspContainer(title = gettext("Standard"))
  postHocStandardContainer$dependOn(c("postHocTestsVariables", "postHocTestEffectSize", "postHocTestsTypeStandard",
                                      "postHocTestsBonferroni", "postHocTestsHolm", "postHocTestsScheffe",
                                      "postHocTestsTukey", "postHocTestsSidak", "postHocFlagSignificant",
                                      "postHocTestsBootstrapping", "postHocTestsBootstrappingReplicates",
                                      "confidenceIntervalsPostHoc", "confidenceIntervalIntervalPostHoc"))

  postHocContainer[["postHocStandardContainer"]] <- postHocStandardContainer

  postHocVariables <- unlist(options$postHocTestsVariables, recursive = FALSE)
  postHocVariablesListV <- unname(lapply(postHocVariables, .v))

  for (postHocVarIndex in 1:length(postHocVariables)) {

    thisVarName <- paste(postHocVariables[[postHocVarIndex]], collapse = " \u273B ")
    interactionTerm <- length(postHocVariables[[postHocVarIndex]]) > 1

    postHocStandardContainer[[thisVarName]] <- .createPostHocStandardTable(thisVarName, interactionTerm, options,
                                                                           options$postHocTestsBootstrapping)
  }

  for (postHocVarIndex in 1:length(postHocVariables)) {

    thisVarName <- paste(postHocVariables[[postHocVarIndex]], collapse = " \u273B ")
    interactionTerm <- length(postHocVariables[[postHocVarIndex]]) > 1
    postHocInterval  <- options$confidenceIntervalIntervalPostHoc

    postHocRef <- emmeans::lsmeans(model, postHocVariablesListV)

    postHocCorrections <- c("tukey", "scheffe", "bonferroni", "holm", "sidak")

    ## Computation
    resultPostHoc <- lapply(postHocCorrections, function(x)
      summary(emmeans::contrast(postHocRef[[postHocVarIndex]], method = "pairwise"),
              adjust = x, infer = c(TRUE, TRUE), level = options$confidenceIntervalIntervalPostHoc))

    allContrasts <- strsplit(as.character(resultPostHoc[[1]]$contrast), split = " - ")

    if (nrow(resultPostHoc[[1]]) > 1)
      postHocStandardContainer[[thisVarName]]$addFootnote(.getCorrectionFootnoteAnova(resultPostHoc[[1]],
                                                                                      options$confidenceIntervalsPostHoc))

    avFootnote <- attr(resultPostHoc[[1]], "mesg")[grep(attr(resultPostHoc[[1]], "mesg"), pattern = "Results are averaged")]
    if (length(avFootnote) != 0) {
      avTerms <- .unv(strsplit(gsub(avFootnote, pattern = "Results are averaged over the levels of: ", replacement = ""),
                               ", ")[[1]])
      postHocStandardContainer[[thisVarName]]$addFootnote(gettextf("Results are averaged over the levels of: %s", paste(avTerms, collapse = ", ")))
    }

    # Calculate effect sizes
    if (options$postHocTestEffectSize && nrow(dataset) > 0 && !interactionTerm) {

      den <- numeric(length(allContrasts))

      for(i in 1:length(allContrasts)) {
        x <- dataset[(dataset[.v(thisVarName)] == allContrasts[[i]][1]), .v(options$dependent)]
        y <- dataset[(dataset[.v(thisVarName)] == allContrasts[[i]][2]), .v(options$dependent)]
        n1 <- length(x)
        n2 <- length(y)
        den[i] <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
      }
      resultPostHoc[[1]][["cohenD"]] <- resultPostHoc[[1]][["estimate"]] / den
      postHocStandardContainer[[thisVarName]]$addFootnote(gettext("Cohen's d does not correct for multiple comparisons."))
    }

    allPvalues <- do.call(cbind, lapply(resultPostHoc, function(x) x$p.value))
    colnames(allPvalues) <- postHocCorrections
    resultPostHoc <- cbind(resultPostHoc[[1]], allPvalues)

    resultPostHoc[["contrast_A"]] <- lapply(allContrasts, `[[`, 1L)
    resultPostHoc[["contrast_B"]] <- lapply(allContrasts, `[[`, 2L)

    if (options$postHocTestsBootstrapping) {

      postHocStandardContainer[[thisVarName]]$addFootnote(message = gettextf("Bootstrapping based on %s successful replicates.", as.character(options[['postHocTestsBootstrappingReplicates']])))
      postHocStandardContainer[[thisVarName]]$addFootnote(message = gettext("Mean Difference estimate is based on the median of the bootstrap distribution."))
      postHocStandardContainer[[thisVarName]]$addFootnote(symbol = "\u2020", message = gettext("Bias corrected accelerated."))

      startProgressbar(options[["postHocTestsBootstrappingReplicates"]] * length(postHocVariables),
                       label = gettext("Bootstrapping Post Hoc Test"))

      ## Computation
      bootstrapPostHoc <- try(boot::boot(data = dataset, statistic = .bootstrapPostHoc,
                                         R = options[["postHocTestsBootstrappingReplicates"]],
                                         options = options,
                                         nComparisons = nrow(resultPostHoc),
                                         postHocVariablesListV = postHocVariablesListV,
                                         postHocVarIndex = postHocVarIndex),
                              silent = TRUE)

      if (class(bootstrapPostHoc) == "try-error") {
        postHocStandardContainer[[thisVarName]]$setError(bootstrapPostHoc)
        next
      }

      bootstrapSummary <- summary(bootstrapPostHoc)

      ci.fails <- FALSE
      bootstrapPostHocConf <- t(sapply(1:nrow(bootstrapSummary), function(comparison){
        res <- try(boot::boot.ci(boot.out = bootstrapPostHoc, conf = options$confidenceIntervalIntervalPostHoc, type = "bca",
                                 index = comparison)[['bca']][1,4:5])

        if(!inherits(res, "try-error")){
          return(res)
        } else {
          ci.fails <<- TRUE
          return(c(NA, NA))
        }
      }))

      if (ci.fails)
        postHocStandardContainer[[thisVarName]]$addFootnote(message = gettext("Some confidence intervals could not be computed.\nPossibly too few bootstrap replicates."))

      resultPostHoc[["lower.CL"]] <- bootstrapPostHocConf[,1]
      resultPostHoc[["upper.CL"]] <- bootstrapPostHocConf[,2]

      resultPostHoc[["bias"]] <- bootstrapSummary[["bootBias"]]
      resultPostHoc[["SE"]] <- bootstrapSummary[["bootSE"]]
      resultPostHoc[["estimate"]] <- bootstrapSummary[["bootMed"]]

    }

    resultPostHoc[[".isNewGroup"]] <- c(TRUE, rep(FALSE, nrow(resultPostHoc)-1))
    postHocStandardContainer[[thisVarName]]$setData(resultPostHoc)

    whichPvalues <- c(options$postHocTestsTukey, options$postHocTestsScheffe, options$postHocTestsBonferroni,
                      options$postHocTestsHolm, options$postHocTestsSidak)
    allPvalues <- as.data.frame(allPvalues)

    if (options$postHocFlagSignificant && length(postHocCorrections[whichPvalues]) > 0)
      .anovaAddSignificanceSigns(someTable = postHocStandardContainer[[thisVarName]],
                                 allPvalues = allPvalues,
                                 resultRowNames = rownames(resultPostHoc))

  }


  return()
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

.bootstrapPostHoc <- function(data, indices, options, nComparisons, postHocVariablesListV, postHocVarIndex) {

  resamples <- data[indices, , drop = FALSE] # allows boot to select sample

  model <- .anovaModel(resamples, options)$model # refit model

  postHocRefBoots <- suppressMessages(
    emmeans::lsmeans(model, postHocVariablesListV)
  )

  postHocTableBoots <- suppressMessages(
    summary(emmeans::contrast(postHocRefBoots[[postHocVarIndex]], method = "pairwise"),
            infer = c(FALSE, FALSE))
  )

  progressbarTick()

  if (nrow(postHocTableBoots) == nComparisons) {
    return(postHocTableBoots[['estimate']])
  } else{
    return(rep(NA, nComparisons))
  }
}

.anovaDunnTable <- function(postHocContainer, dataset, options, model) {
  if (!is.null(postHocContainer[["postHocDunnContainer"]]))
    return()

  postHocDunnContainer <- createJaspContainer(title = gettext("Dunn"))
  postHocDunnContainer$dependOn(c("postHocTestsTypeDunn", "postHocFlagSignificant"))
  postHocContainer[["postHocDunnContainer"]] <- postHocDunnContainer

  postHocVariables <- unlist(options$postHocTestsVariables, recursive = FALSE)
  postHocVariablesListV <- unname(lapply(postHocVariables, .v))

  dunnVariables <- unique(unlist(options$postHocTestsVariables))
  dependentVar <- options$dependent

  .createPostHocDunnTable <- function(myTitle) {

    postHocTable <- createJaspTable(title = gettextf("Dunn's Post Hoc Comparisons - %s", myTitle))

    postHocTable$addColumnInfo(name="contrast",   title=gettext("Comparison"),       type="string")
    postHocTable$addColumnInfo(name="z",                                             type="number")
    postHocTable$addColumnInfo(name="wA",         title=gettext("W<sub>i</sub>"),    type="number")
    postHocTable$addColumnInfo(name="wB",         title=gettext("W<sub>j</sub>"),    type="number")
    postHocTable$addColumnInfo(name="pval",       title=gettext("p"),                type="pvalue")
    postHocTable$addColumnInfo(name="bonferroni", title=gettext("p<sub>bonf</sub>"), type="pvalue")
    postHocTable$addColumnInfo(name="holm",       title=gettext("p<sub>holm</sub>"), type="pvalue")

    return(postHocTable)
  }

  for (dunnVar in dunnVariables) {

    postHocDunnContainer[[dunnVar]] <- .createPostHocDunnTable(dunnVar)
    dunnResult <- data.frame(contrast = character(),
                             z = numeric(),
                             wA = numeric(),
                             wB = numeric(),
                             pval = numeric(),
                             bonferroni = numeric(),
                             holm = numeric())

    variableLevels <- levels(droplevels(dataset[[ .v(dunnVar) ]]))
    nLevels <- length(variableLevels)
    nPerGroup <- unname(unlist(table(dataset[[ .v(dunnVar) ]])))
    bigN <- sum(nPerGroup)

    fullRanks <- rank(dataset[[ .v(dependentVar) ]])
    ranksPerGroup <- by(fullRanks, dataset[[ .v(dunnVar) ]], list)
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
        pValAB <- pnorm(abs(zAB), lower.tail = FALSE)

        dunnResult <- rbind(dunnResult, data.frame(contrast = contrast,
                                                   z = zAB,
                                                   wA = meanPerGroup[i],
                                                   wB = meanPerGroup[j],
                                                   pval = pValAB,
                                                   bonferroni = pValAB,
                                                   holm = pValAB))

      }

    }

    allP <- dunnResult[["pval"]]
    dunnResult[["bonferroni"]] <- p.adjust(allP, method = "bonferroni")
    dunnResult[["holm"]] <- p.adjust(allP, method = "holm")

    postHocDunnContainer[[dunnVar]]$setData(dunnResult)

    if (options$postHocFlagSignificant)
      .anovaAddSignificanceSigns(someTable = postHocDunnContainer[[dunnVar]],
                                 allPvalues = cbind(dunnResult[, c("pval", "bonferroni", "holm")]),
                                 resultRowNames = rownames(dunnResult))
  }

  return()
}

.anovaGamesTable <- function(postHocContainer, dataset, options, model) {
  if (!is.null(postHocContainer[["postHocGamesContainer"]]))
    return()

  postHocGamesContainer <- createJaspContainer(title = gettext("Games-Howell"))
  postHocGamesContainer$dependOn(c("postHocTestsTypeGames", "confidenceIntervalIntervalPostHoc",
                                   "confidenceIntervalsPostHoc", "postHocFlagSignificant"))
  postHocContainer[["postHocGamesContainer"]] <- postHocGamesContainer

  gamesVariables <- unique(unlist(options$postHocTestsVariables))
  dependentVar <- dataset[[ .v(options$dependent) ]]
  postHocInterval  <- options$confidenceIntervalIntervalPostHoc


  .createPostHocGamesTable <- function(myTitle) {

    postHocTable <- createJaspTable(title = gettextf("Games-Howell Post Hoc Comparisons - %s", myTitle))

    postHocTable$addColumnInfo(name="contrast", title=gettext("Comparison"),      type="string")
    postHocTable$addColumnInfo(name="meanDiff", title=gettext("Mean Difference"), type="number")

    if (options$confidenceIntervalsPostHoc) {
      thisOverTitle <- gettextf("%.0f%% CI for Mean Difference", options$confidenceIntervalIntervalPostHoc * 100)
      postHocTable$addColumnInfo(name="lowerCI", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
      postHocTable$addColumnInfo(name="upperCI", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
    }

    postHocTable$addColumnInfo(name="SE",                                         type="number")
    postHocTable$addColumnInfo(name="t",                                          type="number")
    postHocTable$addColumnInfo(name="df",                                         type="number")
    postHocTable$addColumnInfo(name="pTukey", title=gettext("p<sub>tukey</sub>"), type="pvalue")

    postHocTable$showSpecifiedColumnsOnly <- TRUE
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

    if (options$postHocFlagSignificant)
      .anovaAddSignificanceSigns(someTable = postHocGamesContainer[[gamesVar]],
                                 allPvalues = gamesResult["pTukey"],
                                 resultRowNames = rownames(gamesResult))

  }

  return()
}

.anovaDunnettTable <- function(postHocContainer, dataset, options, model) {
  if (!is.null(postHocContainer[["postHocDunnettContainer"]]))
    return()

  postHocDunnettContainer <- createJaspContainer(title = gettext("Dunnett"))
  postHocDunnettContainer$dependOn(c("postHocTestsTypeDunnett", "confidenceIntervalIntervalPostHoc",
                                     "confidenceIntervalsPostHoc", "postHocFlagSignificant"))
  postHocContainer[["postHocDunnettContainer"]] <- postHocDunnettContainer

  dunnettVariables <- unique(unlist(options$postHocTestsVariables))
  dependentVariable <- dataset[[ .v(options$dependent) ]]

  .createPostHocDunnettTable <- function(myTitle) {

    postHocTable <- createJaspTable(title = gettextf("Dunnett Post Hoc Comparisons - %s", myTitle))

    postHocTable$addColumnInfo(name="contrast",title=gettext("Comparison"),      type="string")
    postHocTable$addColumnInfo(name="meanDiff",title=gettext("Mean Difference"), type="number")

    if (options$confidenceIntervalsPostHoc) {
      thisOverTitle <- gettextf("%s%% CI for Mean Difference", options$confidenceIntervalIntervalPostHoc * 100)
      postHocTable$addColumnInfo(name="lowerCI", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
      postHocTable$addColumnInfo(name="upperCI", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
    }

    postHocTable$addColumnInfo(name="SE", type="number")
    postHocTable$addColumnInfo(name="t", type="number")
    postHocTable$addColumnInfo(name="p", title=gettext("p<sub>dunnett</sub>"), type="pvalue")

    postHocTable$showSpecifiedColumnsOnly <- TRUE

    return(postHocTable)
  }

  for (dunnettVar in dunnettVariables) {

    postHocDunnettContainer[[dunnettVar]] <- .createPostHocDunnettTable(dunnettVar)

    Group <- dataset[[ .v(dunnettVar) ]]
    nLevels <- length(unique(Group))

    dunAOV <- aov(dependentVariable ~ Group)

    dunnettFit <- multcomp::glht(dunAOV, linfct=multcomp::mcp(Group="Dunnett"))
    dunnettResult <- summary(dunnettFit)[["test"]]
    dunnettConfInt <- try(confint(dunnettFit, level = options$confidenceIntervalIntervalPostHoc), silent = TRUE)

    if (options$confidenceIntervalsPostHoc && class(dunnettConfInt) == "try-error") {
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

      if (options$postHocFlagSignificant)
        .anovaAddSignificanceSigns(someTable = postHocDunnettContainer[[dunnettVar]],
                                   allPvalues = dunnettResult["p"],
                                   resultRowNames = rownames(dunnettResult))

    }
  }

  return()
}

.anovaDescriptivesTable <- function(anovaContainer, dataset, options, ready) {
  if (options$descriptives == FALSE || !is.null(anovaContainer[["descriptivesTable"]]) || !ready)
    return()

  descriptivesTable <- createJaspTable(title = paste0("Descriptives - ", options$dependent))
  anovaContainer[["descriptivesTable"]] <- descriptivesTable

  for (variable in options$fixedFactors) {

    name <- paste0(variable, "_DescriptivesVar")  # in case variable is "Mean", "SD" or "N"
    descriptivesTable$addColumnInfo(title = variable, name = name, type = "string", combine = TRUE)

  }

  descriptivesTable$addColumnInfo(name = "Mean", title=gettext("Mean"), type = "number")
  descriptivesTable$addColumnInfo(name = "SD",   title=gettext("SD"),   type = "number")
  descriptivesTable$addColumnInfo(name = "N",    title=gettext("N"),    type = "integer")

  lvls <- list()
  factors <- list()

  for (variable in options$fixedFactors) {

    factor <- dataset[[ .v(variable) ]]
    factors[[length(factors)+1]] <- factor
    lvls[[ variable ]] <- levels(factor)

  }

  descriptiveResult <- rev(expand.grid(rev(lvls), stringsAsFactors = FALSE))
  descriptiveResult[[".isNewGroup"]] <- FALSE

  columnNames <- paste0(unlist(options$fixedFactors), "_DescriptivesVar")
  nSubsetVars <- length(columnNames)

  allSubsets <- list()

  for (i in 1:nrow(descriptiveResult)) {
    # Here we generate a logical statement to make of a subset of all relevant variables
    subsetStatement  <- eval(parse(text=paste("dataset$", .v(unlist(options$fixedFactors)), " == \"",
                                              descriptiveResult[i, 1:nSubsetVars],
                                              "\"", sep = "", collapse = " & ")))

    # Now we use that statement to make a subset and store in the list of all subsets
    allSubsets[[i]] <- base::subset(dataset, subsetStatement, select = .v(options$dependent))[[1]]

    if (descriptiveResult[i, nSubsetVars] == lvls[[ nSubsetVars ]][1]) {
      descriptiveResult[[i, ".isNewGroup"]] <- TRUE
    }
  }

  allMeans <- sapply(allSubsets, mean)
  descriptiveResult[["Mean"]] <- ifelse(is.nan(allMeans), NA, allMeans)
  descriptiveResult[["N"]]    <- sapply(allSubsets, length)
  descriptiveResult[["SD"]]   <- sapply(allSubsets, sd)

  colnames(descriptiveResult)[1:nSubsetVars] <- columnNames

  descriptivesTable$setData(descriptiveResult)

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
    .qqPlot(anovaContainer, dataset, options, ready)

  return()
}

.anovaLevenesTable <- function(anovaContainer, dataset, options, ready) {

  leveneTable <- createJaspTable(title = gettext("Test for Equality of Variances (Levene's)"))

  leveneTable$addColumnInfo(name="F",   title=gettext("F"),   type="number")
  leveneTable$addColumnInfo(name="df1", title=gettext("df1"), type="number")
  leveneTable$addColumnInfo(name="df2", title=gettext("df2"), type="number")
  leveneTable$addColumnInfo(name="p",   title=gettext("p"),   type="pvalue")


  if (options$VovkSellkeMPR) {
    leveneTable$addColumnInfo(title = gettextf("VS-MPR%s", "\u002A"), name = "VovkSellkeMPR", type = "number")
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
                                 VovkSellkeMPR = VovkSellkeMPR(leveneResult$`Pr(>F)`[1])))

  return()
}

.anovaMarginalMeans <- function(anovaContainer, dataset, options, ready) {
  if (!is.null(anovaContainer[["marginalMeansContainer"]]) || length(options$marginalMeansTerms) == 0 || !ready)
    return()

  marginalMeansContainer <- createJaspContainer(title = gettext("Marginal Means"))
  marginalMeansContainer$dependOn(c("marginalMeansTerms",  "marginalMeansCompareMainEffects", "marginalMeansCIAdjustment",
                                    "marginalMeansBootstrapping", "marginalMeansBootstrappingReplicates"))

  anovaContainer[["marginalMeansContainer"]] <- marginalMeansContainer

  model <- anovaContainer[["model"]]$object

  terms <- options$marginalMeansTerms

  marginalVariables <- unlist(options$marginalMeansTerms, recursive = FALSE)
  marginalVariablesListV <- unname(lapply(marginalVariables, .v))

  for (i in seq_along(marginalVariables)) {
    thisVarName <- paste(marginalVariables[[i]], collapse = " \u273B ")
    individualTerms <- marginalVariables[[i]]
    marginalMeansContainer[[thisVarName]] <- .createMarginalMeansTableAnova(thisVarName, options, individualTerms,
                                                                            options[["marginalMeansBootstrapping"]])
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

    if(options$marginalMeansCIAdjustment == "bonferroni") {
      adjMethod <- "bonferroni"
    } else if(options$marginalMeansCIAdjustment == "sidak") {
      adjMethod <- "sidak"
    } else {
      adjMethod <- "none"
    }

    marginalResult <- summary(emmeans::lsmeans(model, formula), adjust = adjMethod, infer = c(TRUE,TRUE))

    marginalResult[[".isNewGroup"]] <- FALSE
    marginalResult[[".isNewGroup"]][which(marginalResult[, 1] == marginalResult[1, 1])] <- TRUE

    names(marginalResult)[1:length(individualTerms)] <- individualTerms

    if (options$marginalMeansBootstrapping) {

      startProgressbar(options[["marginalMeansBootstrappingReplicates"]],
                       label = gettext("Bootstrapping Marginal Means"))

      anovaFormula <- as.formula(paste("~", terms.base64[i]))
      bootstrapMarginalMeans <- try(boot::boot(data = dataset, statistic = .bootstrapMarginalMeans,
                                               R = options[["marginalMeansBootstrappingReplicates"]],
                                               options = options, nRows = nRows,
                                               anovaFormula = anovaFormula), silent = TRUE)

      if (class(bootstrapMarginalMeans) == "try-error") {
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
  if (!is.null(anovaContainer[["simpleEffectsContainer"]]) || identical(options$simpleFactor, "") ||
      identical(options$moderatorFactorOne, ""))
    return()

  anovaContainer[["simpleEffectsContainer"]] <- createJaspContainer(title = gettext("Simple Main Effects"),
                                                                    dependencies = c("simpleFactor",
                                                                                     "moderatorFactorOne",
                                                                                     "moderatorFactorTwo"))
  simpleEffectsTable <- createJaspTable(title = gettextf("Simple Main Effects - %s", options$simpleFactor))

  anovaContainer[["simpleEffectsContainer"]][["simpleEffectsTable"]] <- simpleEffectsTable

  moderatorTerms <- c(options$moderatorFactorOne, options$moderatorFactorTwo[!identical(options$moderatorFactorTwo, "")])
  nMods <- length(moderatorTerms)
  simpleFactorBase64 <- .v(options$simpleFactor)

  simpleEffectsTable[["title"]] <- gettextf("Simple Main Effects - %s", options$simpleFactor)

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
        nrow(unique(simpleDataset[simpleFactorBase64])) <  nrow(unique(dataset[simpleFactorBase64]))) {

      emptyCaseIndices <- c(emptyCaseIndices, i)
      emptyCases <- c(emptyCases, paste(simpleEffectResult[i, 1:nMods], collapse = ", "))
      allSimpleModels[[i]] <- NA

    } else {

      .anovaModelContainer(anovaContainer[["simpleEffectsContainer"]], dataset = simpleDataset, options = simpleOptions, TRUE)
      .anovaResult(anovaContainer[["simpleEffectsContainer"]], options = simpleOptions)
      simpleResult <- anovaContainer[["simpleEffectsContainer"]][["anovaResult"]]$object$result
      simpleResult[[".isNewGroup"]] <- NULL

      allSimpleModels[[i]] <- simpleResult[simpleFactorBase64, ]
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
  if (!is.null(anovaContainer[["kruskalContainer"]]) || !length(options$kruskalVariablesAssigned))
    return()


  anovaContainer[["kruskalContainer"]] <- createJaspContainer(title = gettext("Kruskal-Wallis Test"),
                                                              dependencies = "kruskalVariablesAssigned")
  kruskalTable <- createJaspTable(title = gettext("Kruskal-Wallis Test"))

  anovaContainer[["kruskalContainer"]][["kruskalTable"]] <- kruskalTable

  kruskalTable$addColumnInfo(name = "Factor",    title=gettext("Factor"),    type = "string")
  kruskalTable$addColumnInfo(name = "Statistic", title=gettext("Statistic"), type = "number")
  kruskalTable$addColumnInfo(name = "df",        title=gettext("df"),        type = "integer")
  kruskalTable$addColumnInfo(name = "p",         title=gettext("p"),         type = "pvalue")


  if (!ready || anovaContainer$getError())
    return()

  kruskalFactors <- options$kruskalVariablesAssigned
  kruskalResultsList <- list()

  for (term in kruskalFactors) {

    kruskalResultsList[[term]] <- kruskal.test(dataset[[.v(options$dependent)]], dataset[[.v(term)]])

  }

  kruskalTable$setData(data.frame(Factor = names(kruskalResultsList),
                                  Statistic = sapply(kruskalResultsList, function(x) x$statistic),
                                  df = sapply(kruskalResultsList, function(x) x$parameter),
                                  p = sapply(kruskalResultsList, function(x) x$p.value)))

  return()
}

.qqPlot <- function(anovaContainer, dataset, options, ready) {

  # create the jaspPlot object
  qqPlot <- createJaspPlot(title = gettext("Q-Q Plot"), width = options$plotWidthQQPlot, height = options$plotHeightQQPlot)

  # now we assign the plot to jaspResults
  anovaContainer[["assumptionsContainer"]][["qqPlot"]] <- qqPlot

  if (!ready || anovaContainer$getError())
    return()

  model <- anovaContainer[["model"]]$object
  standResid <- as.data.frame(stats::qqnorm(rstandard(model), plot.it=FALSE))

  standResid <- na.omit(standResid)
  xVar <- standResid$x
  yVar <- standResid$y

  # Format x ticks
  xlow <- min(pretty(xVar))
  xhigh <- max(pretty(xVar))
  xticks <- pretty(c(xlow, xhigh))

  # format x labels
  xLabs <- vector("character", length(xticks))
  for (i in seq_along(xticks)) {
    if (xticks[i] < 10^6) {
      xLabs[i] <- format(xticks[i], digits= 3, scientific = FALSE)
    } else {
      xLabs[i] <- format(xticks[i], digits= 3, scientific = TRUE)
    }
  }

  # Format y ticks
  ylow <- min(pretty(yVar))
  yhigh <- max(pretty(yVar))
  yticks <- pretty(c(ylow, yhigh))

  # format axes labels
  xLabs <- jaspGraphs::axesLabeller(xticks)
  yLabs <- jaspGraphs::axesLabeller(yticks)

  # format y labels
  yLabs <- vector("character", length(yticks))
  for (i in seq_along(yticks)) {
    if (yticks[i] < 10^6) {
      yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
    } else {
      yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
    }
  }

  p <- jaspGraphs::drawAxis(xName = gettext("Theoretical Quantiles"),
                            yName = gettext("Standardized Residuals"),
                            xBreaks = xticks,
                            yBreaks = xticks,
                            yLabels = xLabs,
                            xLabels = xLabs,
                            force = TRUE)

  p <- p + ggplot2::geom_line(data = data.frame(x = c(min(xticks), max(xticks)), y = c(min(xticks), max(xticks))),
                              mapping = ggplot2::aes(x = x, y = y),
                              col = "darkred",
                              size = 1)

  p <- jaspGraphs::drawPoints(p, dat = data.frame(xVar, yVar), size = 3)

  qqPlot$plotObject <- jaspGraphs::themeJasp(p)

  return()
}
