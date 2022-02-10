# Getters ----
.aorGetContainer <- function(container) {
  if (is.null(container[["ordinalRestrictions"]])) {
    ordinalRestrictionsContainer <- createJaspContainer(title        = gettext("Order Restrictions"),
                                                        dependencies = c("restrictedModels", "includeIntercept"))
    container[["ordinalRestrictions"]] <- ordinalRestrictionsContainer
  } else {
    ordinalRestrictionsContainer <- container[["ordinalRestrictions"]]
  }

  return(ordinalRestrictionsContainer)
}

.aorGetModelSyntax <- function(model) {
  return(model[["restrictionSyntax"]])
}

.aorGetModelSyntaxes <- function(models) {
  return(vapply(models, .aorGetModelSyntax, character(1)))
}

.aorGetModelName <- function(model) {
  return(model[["modelName"]])
}

.aorGetModelNames <- function(models) {
  return(vapply(models, .aorGetModelName, character(1)))
}

.aorPruneEmptyModels <- function(restrictedModels) {
  syntaxes <- .aorGetModelSyntaxes(restrictedModels)
  syntaxes <- trimws(syntaxes)

  return(restrictedModels[syntaxes != ""])
}

# Syntax checks and converters ----
.aorCheckSyntax <- function(modelName, modelSyntax) {
  # restriktor package does not handle multiple constraints on a single line (https://github.com/LeonardV/restriktor/issues/3)
  # so we need to check for this and ask the user to split constraints on separate lines
  lines <- strsplit(modelSyntax, "\n")[[1]]
  for (line in lines) {
    terms <- strsplit(line, "<|>|==")[[1]]

    if(length(terms) > 2) {
      stop(gettext("Syntax error found in model %1$s, line: %2$s.\n\nMultiple restrictions on one line.\n\nPlease use one restriction per line!", modelName, line))
    }
  }

  # check duplication of order restrictions
  lines <- strsplit(modelSyntax, "\n")[[1]]
  lines <- trimws(lines)
  lines <- gsub(" ", "", lines)
  lines <- lines[lines != ""]

  if(length(lines) != length(unique(lines))) {
    stop(gettextf("Syntax error found in model %1$s.\n\nSome restrictions are duplicate!", modelName))
  }

  return(TRUE)
}

# the following two functions should not be necessary if the QML component
# for the restrictions encodes the column names
.aorGetUsedVars <- function(syntax, availablevars) {
  allVars <- decodeColNames(availablevars)
  inSyntax <- stringr::str_detect(syntax, pattern = allVars)
  return(allVars[inSyntax])
}

.aorTranslateSyntax <- function(syntax, dataset, options, modelName) {
  usedvars <- .aorGetUsedVars(syntax, colnames(dataset))

  new.names <- encodeColNames(usedvars)

  for (i in seq_along(usedvars)) {
    syntax <- try(gsub(usedvars[i], new.names[i], syntax))
  }

  if(isTryError(syntax)) {
    stop(gettext("There are errors in the restriction syntax for %s. The syntax could not be decoded!", modelName))
  }

  return(syntax)
}

# Fitting ----
.aorGetUnrestrictedModel <- function(dataset, options) {
  reorderModelTerms <- .reorderModelTerms(options)
  modelTerms        <- reorderModelTerms$modelTerms
  modelDef          <- .modelFormula(modelTerms, options)

  if (options[["includeIntercept"]]) {
    modelFormula <- as.formula(modelDef[["model.def"]])
  } else {
    modelFormula <- as.formula(paste(modelDef[["model.def"]], "- 1"))
  }

  if(options[["wlsWeights"]] == "" || is.na(options[["wlsWeights"]])) {
    weights <- NULL
  } else {
    weights <- dataset[[options[["wlsWeights"]]]]
  }

  allFactors <- c(unlist(options$fixedFactors), unlist(options$randomFactors))
  factorsInFormula <- allFactors[allFactors %in% modelDef[["terms.base64"]]]
  contrasts <- replicate(length(factorsInFormula), "contr.treatment", simplify = FALSE)
  names(contrasts) <- factorsInFormula

  modelMatrix <- model.matrix(modelFormula, dataset, contrasts.arg = contrasts)

  fit <- lm(
    formula   = modelFormula,
    data      = dataset,
    weights   = weights,
    contrasts = contrasts
    )

  model <- list(
    fit          = fit,
    modelMatrix  = modelMatrix,
    modelFormula = modelFormula
  )


  return(model)
}

.aorGetRestrictedModel <- function(restrictedModelOption, dataset, options, unrestrictedModel) {
  modelName                 <- .aorGetModelName   (restrictedModelOption)
  restrictionSyntaxOriginal <- .aorGetModelSyntax (restrictedModelOption)
  restrictionSyntax         <- try(.aorTranslateSyntax(restrictionSyntaxOriginal, dataset, options, modelName))

  if(isTryError(restrictionSyntax)) {
    message <- .aorExtractErrorMessageSoft(restrictionSyntax)
    stop(gettextf("Error in %1$s - Could not encode the model syntax! Error message %2$s", modelName, message))
  }

  syntaxCheck <- try(.aorCheckSyntax(modelName, restrictionSyntax))

  if(isTryError(syntaxCheck)) {
    message <- .aorExtractErrorMessageSoft(syntaxCheck)
    stop(gettextf("Error in %1$s - Syntax error in the model! Error message %2$s", modelName, message))
  }

  fit <- try(restriktor::restriktor(unrestrictedModel[["fit"]], constraints = restrictionSyntax))

  if(isTryError(fit)) {
    message <- .aorExtractErrorMessageSoft(fit)
    stop(gettextf("Error in %1$s - Could not estimate the model! Error message %2$s", modelName, message))
  }

  model <- list(
    fit                       = fit,
    modelName                 = modelName,
    restrictionSyntax         = restrictionSyntax,
    restrictionSyntaxOriginal = restrictionSyntaxOriginal
  )

  return(model)
}

.aorGetRestrictedModels <- function(dataset, options, unrestrictedModel) {

  restrictedModels <- lapply(options[["restrictedModels"]], function(x) try(.aorGetRestrictedModel(x, dataset, options, unrestrictedModel)))

  return(restrictedModels)
}

.aorGetFittedModels <- function(container, dataset, options) {
  if(!is.null(container[["modelState"]])) return(container[["modelState"]]$object)

  models <- list()

  models[["unrestricted"]] <- try(.aorGetUnrestrictedModel(dataset, options))
  if(isTryError(models[["unrestricted"]])) {
    message <- .aorExtractErrorMessageSoft(models[["unrestricted"]])
    container$setError(gettextf("Could not fit the unrestricted model. As a result, none of the restricted models could be estimated. Error message: %s.\n"), message)
    return()
  }

  models[["restricted"]] <- .aorGetRestrictedModels(dataset, options, models[["unrestricted"]])

  if(isTryError(models[["restricted"]])) {
    modelFailed            <- vapply(models[["restricted"]], isTryError, logical(1))
    models[["failed"]]     <- models[["restricted"]][ modelFailed]
    models[["restricted"]] <- models[["restricted"]][!modelFailed]
  } else {
    models[["failed"]] <- list()
  }

  if(length(models[["failed"]]) > 0 && length(models[["restricted"]]) == 0) {
    save(models, file = "~/Downloads/errors.Rdata")
    errors <- vapply(models[["failed"]], .aorExtractErrorMessageSoft, character(1))
    errors <- sprintf("<li>%s</li>", errors)
    errors <- paste(errors, collapse = "\n")
    errors <- sprintf("<ul>%s</ul>", errors)
    message <- gettextf("Could not fit any of the specified restricted models. Reason(s): %s", errors)
    container$setError(message)
  }

  container[["modelState"]] <- createJaspState(object = models)

  return(models)
}

# Syntax info ----
.aorBasicInfo <- function(container, dataset, options) {
  models <- .aorGetFittedModels(container, dataset, options)

  if(is.null(container[["basicInfoContainer"]])) {
    basicInfoContainer <- createJaspContainer(title = gettext("Syntax information"), position = 1)
    container[["basicInfoContainer"]] <- basicInfoContainer
  } else {
    basicInfoContainer <- container[["basicInfoContainer"]]
  }

  .aorBasicInfoAvailableParameters(basicInfoContainer, options, models)

}

.aorBasicInfoAvailableParameters <- function(container, options, models) {
  if(!is.null(container[["availableParameters"]]) || !options[["restrictedModelShowAvailableCoefficients"]]) return()

  availableParameters <- createJaspHtml(title        = gettext("Available coefficients for restriction syntax"),
                                        position     = 1,
                                        dependencies = c("restrictedModelShowAvailableCoefficients")
                                        )
  container[["availableParameters"]] <- availableParameters

  pars <- names(coefficients(models[["unrestricted"]][["fit"]]))
  pars <- gsub("\\(Intercept\\)", ".Intercept.",  pars)
  pars <- gsub(":JaspColumn_",    ".JaspColumn_", pars)
  pars <- sprintf("<li><div class='jasp-code'>%s</div></li>", pars)
  availableParameters$text <- sprintf("<ul>%s</ul>", paste(pars, collapse = "\n"))
}

# Model summary ----
.aorModelSummary <- function(container, dataset, options) {
  models <- .aorGetFittedModels(container, dataset, options)

  if(container$getError()) return()

  if(is.null(container[["modelSummary"]])) {
    modelSummaryContainer <- createJaspContainer(title = gettext("Model Summary"), position = 2)
    container[["modelSummary"]] <- modelSummaryContainer
  } else {
    modelSummaryContainer <- container[["modelSummary"]]
  }

}

# Model Comparison ----
.aorModelComparison <- function(container, dataset, options) {
  if(is.null(container[["modelComparison"]])) {
    modelComparisonContainer <- createJaspContainer(title = gettext("Model Comparison"), position = 3)
    modelComparisonContainer$dependOn(c("restrictedModelComparison"))
    container[["modelComparison"]] <- modelComparisonContainer
  } else {
    modelComparisonContainer <- container[["modelComparison"]]
  }

  models          <- .aorGetFittedModels   (container, dataset, options)
  ready <- !container$getError() && length(models[["restricted"]]) > 0

  if(ready) {
    modelComparison <- try(.aorGetModelComparison(modelComparisonContainer, options, models))
  } else {
    modelComparison <- NULL
  }

  if(isTryError(modelComparison)) {
    message <- .extractErrorMessage(modelComparison)
    modelComparisonContainer$setError(gettext("Could not compute model comparison! Error message: %", message))
    ready <- FALSE
  }

  .aorModelComparisonTable              (modelComparisonContainer, options, modelComparison, ready)
  .aorModelComparisonMatrix             (modelComparisonContainer, options, modelComparison, ready)
  .aorModelComparisonCompareCoefficients(modelComparisonContainer, options, modelComparison, models, ready)
}

.aorModelComparisonTable <- function(container, options, modelComparison, ready) {
  if(!is.null(container[["comparisonTable"]])) return()

  if(ready) {
    type <- modelComparison[["type"]]
  } else {
    type <- "none"
  }
  comparison <- options[["restrictedModelComparison"]]
  reference  <- options[["restrictedModelComparisonReference"]]

  # define
  comparisonTable <- createJaspTable(title        = gettext("Model Comparison Table"),
                                     position     = 1,
                                     dependencies = c("restrictedModelComparisonWeights", "restrictedModelComparisonReference")
                                     )
  comparisonTable$showSpecifiedColumnsOnly <- TRUE
  comparisonTable$addColumnInfo(name = "modelNames", title = gettext("Model"),   type = "string")
  comparisonTable$addColumnInfo(name = "loglik",     title = gettext("LL"),      type = "number")
  comparisonTable$addColumnInfo(name = "penalty",    title = gettext("Penalty"), type = "number")
  if (type == "goric") {
    comparisonTable$addColumnInfo(name = "goric",         title = gettext("GORIC"),  type = "number")
    comparisonTable$addColumnInfo(name = "goric.weights", title = gettext("Weight"), type = "number")
  } else if (type == "gorica"){
    comparisonTable$addColumnInfo(name = "gorica",         title = gettext("GORICA"), type = "number")
    comparisonTable$addColumnInfo(name = "gorica.weights", title = gettext("Weight"), type = "number")
  }

  if(options[["restrictedModelComparisonWeights"]])
    comparisonTable$addColumnInfo(name = "ratio", title = gettext("Weights ratio"), type = "number")

  abbrev     <- switch(type,
                       goric = gettext("GORIC = Generalized Order-Restricted Information Criterion (Kuiper, Hoijtink, & Silvapulle, 2011)."),
                       gettext("GORICA = Generalized Order-Restricted Information Criterion Approximation.")
  )
  comparisonTable$addFootnote(gettextf('Weights ratios indicate the relative weight for each model against the "%1$s" model. %2$s', reference, abbrev))
  comparisonTable$addCitation(c("Kuiper, R. M., Hoijtink, H., Silvapulle, M. J. (2011). An Akaike-type information criterion for model selection under equality constarints. Biometrika, 98(2), 495-501.",
                                "Vanbrabant, L., Van Loey, N., & Kuiper, R. M. (2020). Evaluating a theory-based hypothesis against its complement using an AIC-type information criterion with an application to facial burn injury. Psychological Methods, 25(2), 129-142."))

  container[["comparisonTable"]] <- comparisonTable

  if(!ready) return()
  # fill
  if(options[["restrictedModelComparisonWeights"]]) {
    result  <- modelComparison[["result"]]
    weights <- modelComparison[["relative.gw"]]

    if(nrow(result) == 1) {
      modelComparison[["result"]][["ratio"]] <- 1
    } else if (is.na(weights)) {
      modelComparison[["result"]][["ratio"]] <- NA
      comparisonTable$addFootnote(colNames = "ratio", message = gettext("Could not compute weights!"))
    } else if (! reference %in% result[["model"]]) {
      modelComparison[["result"]][["ratio"]] <- NA
      comparisonTable$addFootnote(colNames = "ratio", message = gettextf("Model '%s' cannot be a reference model as it is empty!", reference))
    } else {
      modelComparison[["result"]][["ratio"]] <- weights[, result[["model"]] == reference]
    }
  }

  comparisonTable$setData(modelComparison[["result"]])

  return()
}


.aorModelComparisonMatrix <- function(container, options, modelComparison, ready) {
  if(!is.null(container[["comparisonMatrix"]]) || !options[["restrictedModelComparisonMatrix"]]) return()


  if(ready) {
    type <- modelComparison[["type"]]
  } else {
    type <- ""
  }
  # define
  comparisonMatrix <- createJaspTable(title        = gettextf("Relative %s-Weights", toupper(type)),
                                      position     = 2,
                                      dependencies = c("restrictedModelComparisonMatrix")
                                      )
  comparisonMatrix$addColumnInfo(name = "model", title = gettext("Model"))
  container[["comparisonMatrix"]] <- comparisonMatrix

  if(!ready) return()
  # fill
  if(is.null(modelComparison[["relative.gw"]])) {
    # only one model in the comparison leads to empty relative weight matrix
    comparisonMatrix$setError(gettext("Only one model in the comparison: Cannot compute relative weights matrix"))
  } else {
    modelNames <- rownames(modelComparison[["relative.gw"]])
    df <- as.data.frame(modelComparison[["relative.gw"]])
    vsModelNames <- colnames(df)
    rownames(df) <- NULL

    df[["model"]] <- modelNames

    for(colName in vsModelNames)
      comparisonMatrix$addColumnInfo(name = colName, title = colName, type = "number", overtitle = gettext("vs."))

    comparisonMatrix$setData(df)
  }
}

.aorModelComparisonCompareCoefficients <- function(container, options, modelComparison, models, ready) {
  if(!is.null(container[["coefficientsTable"]]) || !options[["restrictedModelComparisonCoefficients"]]) return()

  coefficientsTable <- createJaspTable(title        = gettext("Coefficients Comparison"),
                                       position     = 3,
                                       dependencies = c("restrictedModelComparisonCoefficients", "highlightEstimates")
                                       )
  coefficientsTable$showSpecifiedColumnsOnly <- TRUE
  coefficientsTable$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "string")


  container[["coefficientsTable"]] <- coefficientsTable

  if(!ready) return()
  # fill
  result <- modelComparison[["result"]]
  df <- t(coefficients(modelComparison))
  df <- as.data.frame(df)
  colnames(df) <- result[["model"]]
  df[["coef"]] <- rownames(df)

  # bug in restriktor cannot handle user-defined parameters, so we will remove them
  basicParameters <- names(coefficients(models[["unrestricted"]][["fit"]]))
  df <- subset(df, subset = coef %in% basicParameters)

  for(index in seq_len(nrow(result)))
    coefficientsTable$addColumnInfo(name = result[["model"]][[index]], title = result[["modelNames"]][[index]], type = "number")

  coefficientsTable$setData(df)

  if(options[["highlightEstimates"]])
    .aorModelComparisonHighlightCoefficients(coefficientsTable, coefficients(models[["unrestricted"]][["fit"]]), df)
}

.aorModelComparisonHighlightCoefficients <- function(table, unrestricted, df) {
  for(row in seq_len(nrow(df))) {
    coef <- df[["coef"]][[row]]
    table$setRowName(rowIndex = row, newName = coef)

    for(col in seq_len(ncol(df))) {
      model <- colnames(df)[[col]]

      cell <- df[row, col]
      ref  <- unrestricted[[coef]]
      if(is.numeric(cell) && !isTRUE(all.equal(cell, ref))) {
        table$addFootnote(
          message  = gettext("Coefficients differ from unconstrained model."),
          symbol   = "\u2020",
          colNames = model,
          rowNames = coef
          )
      }
    }
  }
}

.aorGetModelComparison <- function(container, options, models) {
  if (!is.null(container[["modelComparisonState"]])) return(container[["modelComparisonState"]]$object)

  comparison <- options[["restrictedModelComparison"]]
  reference  <- options[["restrictedModelComparisonReference"]]

  modelList <- lapply(models[["restricted"]], "[[", "fit")
  names(modelList)[1] <- "object"
  modelComparison <- try(do.call(restriktor::goric, c(modelList, comparison = comparison)))

  if(!isTryError(modelComparison)) {
    modelNames <- vapply(models[["restricted"]], "[[", character(1), "modelName")
    names(modelNames) <- modelNames
    modelNames <- switch(comparison,
                         unconstrained = c(modelNames, unconstrained = gettext("Unconstrained")),
                         complement    = c(modelNames, complement    = gettext("Complement")),
                         modelNames
                         )

    if(!is.null(modelComparison[["relative.gw"]])) {
      rownames(modelComparison[["relative.gw"]]) <- colnames(modelComparison[["relative.gw"]]) <- modelNames
    }

    modelComparison[["result"]][["modelNames"]] <- modelNames
    modelComparison[["result"]][["model"]]      <- names(modelNames)

  }

  container[["modelComparisonState"]] <- createJaspState(object = modelComparison)

  return(modelComparison)
}


# Utilities ----

.aorExtractErrorMessageSoft <- function(error) {
  split <- base::strsplit(as.character(error), ":")[[1]]
  message <- split[2:length(split)]
  message <- unlist(message)
  message <- paste(message, collapse = ":")
  trimws(message)
}
