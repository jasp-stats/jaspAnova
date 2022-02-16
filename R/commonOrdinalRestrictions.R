# Getters ----
.aorGetContainer <- function(container, name, title, dependencies = NULL, position = 1, initCollapsed = FALSE) {
  if(is.null(container[[name]])) {
    newContainer <- createJaspContainer(
      title         = title,
      dependencies  = dependencies,
      position      = position,
      initCollapsed = initCollapsed
    )
    container[[name]] <- newContainer
  } else {
    newContainer <- container[[name]]
  }

  return(newContainer)
}

.aorGetMainContainer <- function(container) {
  ordinalRestrictionsContainer <- .aorGetContainer(
    container    = container,
    name         = "ordinalRestrictions",
    title        = gettext("Order Restricted Hypotheses"),
    dependencies = c("restrictedModels", "includeIntercept")
    )

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

.aorRenameInterceptRemoveColon <- function(coefs) {
  coefs <- gsub("\\(Intercept\\)", ".Intercept.",  coefs)
  coefs <- gsub(":JaspColumn_",    ".JaspColumn_", coefs)
  return(coefs)
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
    stop(gettextf("Error in %1$s - Could not encode the model syntax! Error message: %2$s", modelName, message))
  }

  syntaxCheck <- try(.aorCheckSyntax(modelName, restrictionSyntax))

  if(isTryError(syntaxCheck)) {
    message <- .aorExtractErrorMessageSoft(syntaxCheck)
    stop(gettextf("Error in %1$s - Syntax error in the model! Error message: %2$s", modelName, message))
  }

  fit <- try(restriktor::restriktor(unrestrictedModel[["fit"]], constraints = restrictionSyntax))

  if(isTryError(fit)) {
    message <- .aorExtractErrorMessageSoft(fit)
    stop(gettextf("Error in %1$s - Could not estimate the model! Error message: %2$s", modelName, message))
  }

  model <- list(
    fit                       = fit,
    modelName                 = modelName,
    modelSummary              = restrictedModelOption[["modelSummary"]],
    informedHypothesisTest    = restrictedModelOption[["informedHypothesisTest"]],
    marginalMeans             = restrictedModelOption[["marginalMeans"]],
    restrictionSyntax         = restrictionSyntax,
    restrictionSyntaxOriginal = restrictionSyntaxOriginal
  )

  return(model)
}

.aorGetRestrictedModels <- function(dataset, options, unrestrictedModel) {

  restrictedModels        <- lapply(options[["restrictedModels"]],
                                    function(x) try(.aorGetRestrictedModel(x, dataset, options, unrestrictedModel)))
  modelNames              <- .aorGetModelNames(options[["restrictedModels"]])
  names(restrictedModels) <- modelNames

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

  coefs <- names(coefficients(models[["unrestricted"]][["fit"]]))
  coefs <- .aorRenameInterceptRemoveColon(coefs)
  coefs <- sprintf("<li><div class='jasp-code'>%s</div></li>", coefs)

  availableParameters$text <- sprintf("<ul>%s</ul>", paste(coefs, collapse = "\n"))
}

# Model Comparison ----
.aorModelComparison <- function(container, dataset, options) {
  if(is.null(container[["modelComparison"]])) {
    modelComparisonContainer <- createJaspContainer(title = gettext("Model Comparison"), position = 2)
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
  comparisonTable$addColumnInfo(name = "loglik",     title = gettext("Log-likelihood"),      type = "number")
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

  if(!is.null(modelComparison[["excludedModels"]])) {
    comparisonTable$addFootnote(
      message = gettextf("The following models were excluded: %s.", paste(modelComparison[["excludedModels"]], collapse = ", "))
      )
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

  if(length(models[["failed"]]) != 0) {
    modelComparison[["excludedModels"]] <- names(models[["failed"]])
  }

  container[["modelComparisonState"]] <- createJaspState(object = modelComparison)

  return(modelComparison)
}

# Single Model inference ----
.aorSingleModelsInference <- function(container, dataset, options) {
  models <- .aorGetFittedModels(container, dataset, options)

  if(container$getError()) return()

  if(is.null(container[["singleModelsInference"]])) {
    singleModelsContainer <- createJaspContainer(title = gettext("Single Model Inference"), position = 3)
    container[["singleModelsInference"]] <- singleModelsContainer
  } else {
    singleModelsContainer <- container[["singleModelsInference"]]
  }

  allModelNames    <- .aorGetModelNames(options[["restrictedModels"]])
  fittedModelNames <- .aorGetModelNames(models[["restricted"]])

  for(modelName in allModelNames) {

    modelContainer <- .aorGetContainer(
      container     = singleModelsContainer,
      name          = modelName,
      title         = modelName,
      initCollapsed = TRUE
    )
    singleModelsContainer[[modelName]] <- modelContainer

    if(modelName %in% fittedModelNames) {
      model <- models[["restricted"]][[which(modelName == fittedModelNames)]]
      .aorModelSummary             (modelContainer, options, model)
      .aorInformativeHypothesisTest(modelContainer, options, model)
      .aorMarginalMeans            (modelContainer, options, model, dataset)
    } else {
      model <- models[["failed"]][[modelName]]
      .aorDisplayModelError(modelContainer, model)
    }
  }
}

## Model Summary ----
.aorModelSummary <- function(container, options, model) {
  if(!is.null(container[["modelSummaryContainer"]]) || !model[["modelSummary"]]) return()

  modelSummaryContainer <- .aorGetContainer(
    container = container,
    name      = "modelSummaryContainer",
    title     = gettext("Model Summary"),
    position  = 1
  )

  .aorModelRestrictionMatrix(modelSummaryContainer, model)
  .aorModelCoefficients     (modelSummaryContainer, model)
}

.aorModelRestrictionMatrix <- function(container, model) {
  restrictionMatrix <- createJaspTable(title = gettext("Restriction Matrix"))
  container[["restrictionMatrix"]] <- restrictionMatrix

  coefs <- names(model[["fit"]][["b.restr"]])
  coefs <- .aorRenameInterceptRemoveColon(coefs)
  coefNames <- character(length(coefs))
  for(i in seq_along(coefs)) {
    coef <- paste0("coef", i)
    restrictionMatrix$addColumnInfo(name = coef, title = coefs[i], type = "number", overtitle = gettext("Left-hand side (lhs)"))
    coefNames[i] <- coef
  }

  restrictionMatrix$addColumnInfo(name = "rhs", title = gettext("Right-hand side (rhs)"), type = "number")


  # fill
  df <- as.data.frame(model[["fit"]][["constraints"]])
  colnames(df) <- coefNames

  df[["rhs"]] <- model[["fit"]][["rhs"]]

  restrictionMatrix$setData(df)
}

.aorModelCoefficients <- function(container, model) {
  coefficientsTable <- createJaspTable(title = gettext("Coefficients"))
  container[["coefficientsTable"]] <- coefficientsTable

  coefs <- try(coefficients(summary(model[["fit"]])))

  if(!isTryError(coefs)) {
    coefs <- as.data.frame(coefs)
    coefs[["name"]] <- rownames(coefs)

    coefficientsTable$addColumnInfo(name = "name", title = gettext("Coefficient"), type = "string")
    coefficientsTable$addColumnInfo(name = "Estimate", title = gettext("Estimate"), type = "number")
    coefficientsTable$addColumnInfo(name = "Std. Error", title = gettext("S.E."), type = "number")
    coefficientsTable$addColumnInfo(name = "t value", title = gettext("t"), type = "number")
    coefficientsTable$addColumnInfo(name = "Pr(>|t|)", title = gettext("p"), type = "pvalue")

    coefficientsTable$setData(coefs)
  }
}

## Informative Hypothesis tests ----
.aorInformativeHypothesisTest <- function(container, options, model) {
  if(!is.null(container[["ihtTable"]]) || !model[["informedHypothesisTest"]]) return()

  ihtTable <- createJaspTable(title = gettext("Informative Hypothesis Tests"), position = 2)
  ihtTable$showSpecifiedColumnsOnly <- TRUE
  container[["ihtTable"]] <- ihtTable

  ihtTable$addColumnInfo(name = "typeName", title = gettext("Hypothesis"),      type = "string")
  ihtTable$addColumnInfo(name = "test",     title = gettext("Test"),            type = "string")
  ihtTable$addColumnInfo(name = "stat",     title = gettext("Test Statistic"),  type = "number")
  ihtTable$addColumnInfo(name = "pvalue",   title = gettext("p"),               type = "pvalue")

  result <- try(.aorCalculateIHT(model[["fit"]]))

  if(!isTryError(result)) {
    result[["typeName"]] <- gettextf("Type %s", result[["type"]])
    ihtTable$setData(result)

    .aorAddInformativeHypothesisTestFootnotes(ihtTable, result)
  } else {
    message <- .aorExtractErrorMessageSoft(result)
    ihtTable$setError(gettextf("Could not compute the informative hypothesis tests. Error message: %s", message))
  }
}

.aorAddInformativeHypothesisTestFootnotes <- function(table, result) {
  for(i in seq_len(nrow(result)))
    table$setRowName(rowIndex = i, newName = sprintf("row%s", i))

  # hypothesis types
  .aorInformativeHypothesisTestFootnoteHelper(
    table   = table,
    rows    = result[["type"]] == "classical",
    colName = "typeName",
    message = gettextf("H%1$s: All equality restrictions are active (==), H%2$s: At least one equality restriction is violated.", "\u2080", "\u2081")
    )

  .aorInformativeHypothesisTestFootnoteHelper(
    table   = table,
    rows    = result[["type"]] == "global",
    colName = "typeName",
    message = gettextf("H%1$s: All parameters are restricted to be equal (==), H%2$s: At least one inequality restriction is strictly true (>).", "\u2080", "\u2081")
  )

  .aorInformativeHypothesisTestFootnoteHelper(
    table   = table,
    rows    = result[["type"]] == "A",
    colName = "typeName",
    message = gettextf("H%1$s: All restrictions are equalities (==), H%2$s: At least one inequality restriction is strictly true (>).", "\u2080", "\u2081")
  )

  .aorInformativeHypothesisTestFootnoteHelper(
    table   = table,
    rows    = result[["type"]] == "B",
    colName = "typeName",
    message = gettextf("H%1$s: All restrictions hold, H%2$s: At least one restriction is violated.", "\u2080", "\u2081")
  )

  .aorInformativeHypothesisTestFootnoteHelper(
    table   = table,
    rows    = result[["type"]] == "C",
    colName = "typeName",
    message = gettextf("H%1$s: At least one restriction is false or active (==), H%2$s: All restrictions are strictly true (>).", "\u2080", "\u2081")
  )

}

.aorInformativeHypothesisTestFootnoteHelper <- function(table, rows, colName, message) {
  if(is.logical(rows)) rows <- which(rows)

  for(row in rows) {
    table$addFootnote(
      message  = message,
      colNames = colName,
      rowNames = table$getRowName(rowIndex = row)
    )
  }
}

## Marginal means ----
.aorMarginalMeans <- function(container, options, model, dataset) {
  if(!model[["marginalMeans"]]) return()

  marginalMeansContainer <- .aorGetContainer(
    container    = container,
    name         = "marginalMeansContainer",
    title        = gettext("Marginal Means"),
    position     = 3,
    dependencies = c("restrictedMarginalMeansModelSE",
                     "restrictedMarginalMeansBootstrappingReplicates",
                     "restrictedModelMarginalMeansTerms",
                     "restrictedConfidenceIntervalLevel")
  )

  if(length(options[["restrictedModelMarginalMeansTerms"]]) == 0 || options[["restrictedModelMarginalMeansTerms"]] == "") {
    # settting an error on empty container does not show up, so we will make an empty table
    marginalMeansContainer[["table"]] <- createJaspTable(title = gettext("Marginal Means"))
    marginalMeansContainer$setError(gettext("No marginal means terms specified. Please, select model terms in the 'Restricted Marginal Means' section."))
  } else {
    terms <- options[["restrictedModelMarginalMeansTerms"]]
    for(i in seq_along(terms))
      .aorMarginalMeansTerm(marginalMeansContainer, dataset, options, model, terms[[i]][["variable"]], i)
  }
}

.aorMarginalMeansTerm <- function(container, dataset, options, model, variables, position) {
  tableName <- paste(variables, collapse="_")
  if(!is.null(container[[tableName]])) return()

  marginalMeansTable <- createJaspTable(title    = paste(variables, collapse=" \u273B "),
                                        position = position)
  container[[tableName]] <- marginalMeansTable

  marginalMeansTable$showSpecifiedColumnsOnly <- TRUE
  for(var in variables)
    marginalMeansTable$addColumnInfo(name = var, title = var, type = "string", combine = TRUE)

  marginalMeansTable$addColumnInfo(name="lsmean",   title = gettext("Marginal Mean"), type="number")
  marginalMeansTable$addColumnInfo(name="SE",       title = gettext("SE"),            type="number")
  marginalMeansTable$addColumnInfo(name="lower.CL", title = gettext("Lower"),         type="number", overtitle = gettextf("%s%% CI", 100*options$restrictedConfidenceIntervalLevel))
  marginalMeansTable$addColumnInfo(name="upper.CL", title = gettext("Upper"),         type="number", overtitle = gettextf("%s%% CI", 100*options$restrictedConfidenceIntervalLevel))

  # fill
  result <- try(.aorCalculateMarginalMeans(dataset, model, variables))

  if(isTryError(result)) {
    message <- .extractErrorMessage(result)
    marginalMeansTable$setError(gettextf("Could not compute the marginal means. Error message: %s", message))
  } else {
    marginalMeansTable$setData(result)
  }
}

## Model Error ----
.aorDisplayModelError <- function(container, model) {
  # settting an error on empty container does not show up, so we will make an
  # empty table
  container[["restrictionMatrix"]] <- createJaspTable(title = gettext("Restriction Matrix"))

  message <- .aorExtractErrorMessageSoft(model)
  container$setError(message)
}


# Utilities ----

.aorExtractErrorMessageSoft <- function(error) {
  split <- base::strsplit(as.character(error), ":")[[1]]
  message <- split[2:length(split)]
  message <- unlist(message)
  message <- paste(message, collapse = ":")
  trimws(message)
}

.aorCalculateIHT <- function(model) {
  nconst <- nrow(model[["constraints"]])
  neq    <- model[["neq"]]

  if(nconst == neq) { # only equality constraints
    types <- "classical"
    tests <- c("F", "Wald", "score")
  } else if(neq > 0) { # some equality constraints
    types <- c("global", "A", "B")
    tests <- c("F", "LRT", "score")
  } else { # no equality constraints
    types = c("global", "A", "B", "C")
    tests <- c("F", "LRT", "score")
  }

  results <- list()
  for(type in types) {
    results[[type]] <- list()
    for(test in tests) {
      if(type == "classical") {
        res <- restriktor::conTest_ceq(model, test = test)
      } else {
        if(type == "C" && test != "F") next # type C is only done with t-test (which can be called by test = "F")

        res <- restriktor::conTest(model, type = type, test = test)
      }
      results[[type]][[test]] <- data.frame(
        type   = type,
        test   = res[["test"]],
        stat   = res[["Ts"]],
        pvalue = res[["pvalue"]]
      )
    }

    results[[type]] <- data.frame(do.call(rbind, results[[type]]))
  }
  results <- data.frame(do.call(rbind, results))

  return(results)
}

.aorCalculateMarginalMeans <- function(dataset, model, variables) {
  refGrid <- emmeans::qdrg(
    formula   = formula(model[["fit"]][["model.org"]]),
    data      = dataset,
    coef      = model[["fit"]][["b.restr"]],
    vcov      = attr(model[["fit"]][["information"]], "inverted"),
    df        = model[["fit"]][["df.residual"]],
    contrasts = model[["fit"]][["model.org"]][["contrasts"]]
  )


  means <- emmeans::lsmeans(refGrid, variables, infer=c(TRUE, FALSE), level = 0.95)
  return(summary(means))
}
