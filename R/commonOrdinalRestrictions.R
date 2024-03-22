# Main function  ----
.anovaOrdinalRestrictions <- function(anovaContainer, dataset, options, ready, analysis = c("anova", "rmanova")) {
  if (!ready) return()
  # currently '.aorBasicInfo' shows only available coefficients
  # this can be displayed in JASP before any restricted model is defined
  # (i.e., when length(options[["restrictedModels"]]) == 0)
  # however, unit tests for other parts of the analysis could fail because
  # jaspTools::analysisOptions() does not bring default options from OrderedRestrictions.qml
  # so we will simply jump out if one of the restricted options is NULL
  # to avoid people having to specify default 'restricted options' in the options list manually
  # for unit tests that do not aim to test ordinal restrictions
  if(is.null(options[["restrictedModels"]])) return()

  analysis <- match.arg(analysis)

  options[["restrictedModels"]] <- .aorPruneEmptyModels(options[["restrictedModels"]])

  container <- .aorGetMainContainer(anovaContainer)

  .aorBasicInfo(container, dataset, options, analysis)

  if(length(options[["restrictedModels"]]) == 0) return()

  models <- .aorGetFittedModels(container, dataset, options, analysis)

  .aorModelComparison      (container, dataset, options, models, analysis)
  .aorSingleModelsInference(container, dataset, options, models, analysis)

  return()
}

# Getters ----
.aorGetContainer <- function(container, name, title, dependencies = NULL, position = NULL, initCollapsed = FALSE) {
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
    dependencies = c("restrictedInterceptInclusion", "restrictedHeterogeneityCorrection")
    )

  return(ordinalRestrictionsContainer)
}

.aorGetModelSyntax <- function(model) {
  return(model[["syntax"]])
}

.aorGetModelSyntaxes <- function(models) {
  return(vapply(models, .aorGetModelSyntax, character(1)))
}

.aorGetModelName <- function(model) {
  return(model[["name"]])
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
  # check duplication of order restrictions
  lines <- strsplit(modelSyntax, "\n", fixed = TRUE)[[1]]
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
    stop(gettextf("There are errors in the restriction syntax for %s. The syntax could not be decoded!", modelName))
  }

  return(syntax)
}

.aorRenameInterceptRemoveColon <- function(coefs) {
  coefs <- gsub(":\\(Intercept\\)", "..Intercept.", coefs)
  coefs <- gsub("\\(Intercept\\)",  ".Intercept.",  coefs)
  coefs <- gsub(":JaspColumn_",     ".JaspColumn_", coefs)
  return(coefs)
}

# Fitting ----
.aorGetUnrestrictedModel <- function(container, dataset, options, analysis) {
  if(!is.null(container[["stateUnrestricted"]])) return(container[["stateUnrestricted"]]$object)

  if(analysis == "anova") {
    model <- .aorCalculateUnrestrictedModel(dataset, options)
  } else {
    model <- .rmaorCalculateUnrestrictedModel(dataset, options)
  }

  container[["stateUnrestricted"]] <- createJaspState(object = model)

  return(model)
}

.aorGetUnrestrictedBootstrap <- function(container, dataset, unrestricted, samples){
  if(!is.null(container[["stateUnrestrictedBootstrap"]])) return(container[["stateUnrestrictedBootstrap"]]$object)
  startProgressbar(
    expectedTicks = samples %/% 100,
    label         = gettext("Bootstrapping unrestricted model")
  )

  contrasts <- unrestricted[["contrasts"]]
  weights   <- unrestricted[["weights"]]
  formula   <- formula(unrestricted)

  unrestrictedBootstrap <- vector("list", samples)
  for(i in seq_len(samples)) {
    n <- nrow(dataset)
    idx <- sample.int(n = n, size = n, replace = TRUE)
    resamples <- dataset[idx,,drop=FALSE]

    unrestrictedBootstrap[[i]] <-
      try(
        update(unrestricted, data = resamples, contrasts = contrasts, weights = weights, formula = formula)
      )

    if(i %% 100 == 0) progressbarTick()
  }
  unrestrictedBootstrap[vapply(unrestrictedBootstrap, isTryError, logical(1))] <- NULL

  container[["stateUnrestrictedBootstrap"]] <- createJaspState(
    object = unrestrictedBootstrap, dependencies = c("restrictedBootstrap", "restrictedBootstrapSamples")
  )

  return(unrestrictedBootstrap)
}

.aorGetRestrictedModel <- function(container, restrictedModelOption, dataset, options, unrestrictedModel, unrestrictedBootstrap = list(), analysis) {
  modelName <- .aorGetModelName   (restrictedModelOption)
  stateName <- sprintf("state_%s", modelName)

  if(!is.null(container[[stateName]]$object)) {
    if(length(unrestrictedBootstrap) > 0)
      progressbarTick()

    return(container[[stateName]]$object)
  } else {
    container[[stateName]] <- createJaspState()
    container[[stateName]]$dependOn(
      options             = c("restrictedBootstrap", "restrictedBootstrapSamples", "restrictedBootstrapCiLevel"),
      optionContainsValue = list(restrictedModels = restrictedModelOption)
    )
  }

  syntaxOriginal <- .aorGetModelSyntax (restrictedModelOption)
  syntax         <- try(.aorTranslateSyntax(syntaxOriginal, dataset, options, modelName))

  if(isTryError(syntax)) {
    message <- .aorExtractErrorMessageSoft(syntax)
    stop(gettextf("Error in %1$s - Could not encode the model syntax! Error message: %2$s", modelName, message))
  }

  syntaxCheck <- try(.aorCheckSyntax(modelName, syntax))

  if(isTryError(syntaxCheck)) {
    message <- .aorExtractErrorMessageSoft(syntaxCheck)
    stop(gettextf("Error in %1$s - Syntax error in the model! Error message: %2$s", modelName, message))
  }

  if(analysis == "anova") {
    unrestricted <- unrestrictedModel[["fit"]]
    se <- switch(
      options[["restrictedHeterogeneityCorrection"]],
      huberWhite0  = "HC0",
      huberWhite1  = "HC1",
      huberWhite2  = "HC2",
      huberWhite3  = "HC3",
      huberWhite4  = "HC4",
      huberWhite4m = "HC4m",
      huberWhite5  = "HC5",
      "standard"
    )
    fit <- try(restriktor::restriktor(object = unrestricted, constraints = syntax, se = se))
  } else {
    fit <- try(.rmaorCalculateRestrictedModel(unrestrictedModel, syntax))
  }

  if(isTryError(fit)) {
    message <- .aorExtractErrorMessageSoft(fit)
    stop(gettextf("Error in %1$s - Could not estimate the model! Error message: %2$s", modelName, message))
  }

  if(length(unrestrictedBootstrap) > 0) {
    bootstraps <- try(.aorCalculateBootstrapping(unrestrictedBootstrap, fit, syntax, modelName))
    progressbarTick()
  } else {
    bootstraps <- NULL
  }

  ciLevel <- options[["restrictedBootstrapCiLevel"]]
  coefficients  <- try(.aorCalculateCoefficients(fit, bootstraps, ciLevel))
  marginalMeans <- .aorCalculateMarginalMeans   (fit, bootstraps, ciLevel, c(options[["modelTerms"]], options[["withinModelTerms"]], options[["betweenModelTerms"]]), dataset)

  model <- list(
    fit                       = fit,
    name                      = modelName,
    summary                   = restrictedModelOption[["summary"]],
    informedHypothesisTest    = restrictedModelOption[["informedHypothesisTest"]],
    marginalMean              = restrictedModelOption[["marginalMean"]],
    syntax                    = syntax,
    syntaxOriginal            = syntaxOriginal,
    bootstrapSamples          = nrow(bootstraps),
    coefficients              = coefficients,
    marginalMeansResult       = marginalMeans
  )

  container[[stateName]]$object <- model
  return(model)
}

.aorGetRestrictedModels <- function(container, dataset, options, unrestrictedModel, unrestrictedBootstrap = list(), analysis) {
  if(length(unrestrictedBootstrap) > 0)
    startProgressbar(expectedTicks = length(options[["restrictedModels"]]),
                     label         = gettext("Bootstrapping restricted models"))


  restrictedModels <- vector("list", length(options[["restrictedModels"]]))
  for(i in seq_along(restrictedModels)) {
    restrictedModelOption <- options[["restrictedModels"]][[i]]
    restrictedModels[[i]] <- try(.aorGetRestrictedModel(container, restrictedModelOption, dataset, options, unrestrictedModel, unrestrictedBootstrap, analysis))
  }
  modelNames              <- .aorGetModelNames(options[["restrictedModels"]])
  names(restrictedModels) <- modelNames

  return(restrictedModels)
}

.aorGetFittedModels <- function(container, dataset, options, analysis) {
  models <- list()

  models[["unrestricted"]] <- try(.aorGetUnrestrictedModel(container, dataset, options, analysis))
  if(isTryError(models[["unrestricted"]])) {
    message <- .aorExtractErrorMessageSoft(models[["unrestricted"]])
    container$setError(gettextf("Could not fit the unrestricted model. As a result, none of the restricted models could be estimated. Error message: %s.", message))
    return()
  }

  if(options[["restrictedBootstrap"]]) {
    unrestrictedBootstrap <- try(.aorGetUnrestrictedBootstrap(
      container    = container,
      dataset      = dataset,
      unrestricted = models[["unrestricted"]][["fit"]],
      samples      = options[["restrictedBootstrapSamples"]]
      ))

    if(isTryError(unrestrictedBootstrap)) {
      message <- .aorExtractErrorMessageSoft(unrestrictedBootstrap)
      container$setError(gettextf("Could not obtain bootstrapping for the unrestricted model. As a result, bootstrapping for the restricted models could not be performed. Error message: %s.", message))
    }
  } else {
    unrestrictedBootstrap <- list()
  }

  models[["restricted"]] <- .aorGetRestrictedModels(container, dataset, options, models[["unrestricted"]], unrestrictedBootstrap, analysis)

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
    # if all models fail, we want to rerun the restricted models if any model changes
    container$dependOn(options = "restrictedModels")
  }

  return(models)
}

.aorGetModelComparison <- function(container, options, models, analysis) {
  if (!is.null(container[["modelComparisonState"]])) return(container[["modelComparisonState"]]$object)

  comparison <- options[["restrictedModelComparison"]]
  reference  <- options[["restrictedModelComparisonReference"]]

  if(analysis == "anova") {
    modelComparison <- try(.aorCalculateModelComparison(options, models, comparison))
  } else {
    modelComparison <- try(.rmaorCalculateModelComparison(options, models, comparison))
  }

  if(!isTryError(modelComparison)) {
    modelNames <- .aorGetModelNames(models[["restricted"]])
    names(modelNames) <- modelNames
    modelNames <- switch(comparison,
                         unconstrained = c(modelNames, unconstrained = gettext("Unconstrained")),
                         complement    = c(modelNames, complement    = gettext("Complement")),
                         modelNames
    )

    if(!is.null(modelComparison[["ratio.gw"]])) {
      rownames(modelComparison[["ratio.gw"]]) <- colnames(modelComparison[["ratio.gw"]]) <- modelNames
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

# Syntax info ----
.aorBasicInfo <- function(container, dataset, options, analysis) {
  unrestricted <- .aorGetUnrestrictedModel(container, dataset, options, analysis)

  if(is.null(container[["basicInfoContainer"]])) {
    basicInfoContainer <- createJaspContainer(title = gettext("Syntax information"), position = 1)
    container[["basicInfoContainer"]] <- basicInfoContainer
  } else {
    basicInfoContainer <- container[["basicInfoContainer"]]
  }

  .aorBasicInfoAvailableParameters(basicInfoContainer, options, unrestricted, analysis)

}

.aorBasicInfoAvailableParameters <- function(container, options, unrestricted, analysis) {
  if(!is.null(container[["availableParameters"]]) || !options[["restrictedAvailableCoefficients"]]) return()

  availableParameters <- createJaspHtml(title        = gettext("Available coefficients for restriction syntax"),
                                        position     = 1,
                                        dependencies = c("restrictedAvailableCoefficients")
                                        )
  container[["availableParameters"]] <- availableParameters

  if(analysis == "anova") {
    coefs <- names(coefficients(unrestricted[["fit"]]))
  } else {
    coefs <- names(unrestricted[["parsForGorica"]][["coef"]])
  }
  coefs <- .aorRenameInterceptRemoveColon(coefs)
  coefs <- sprintf("<li><div class='jasp-code'>%s</div></li>", coefs)

  availableParameters$text <- sprintf("<ul>%s</ul>", paste(coefs, collapse = "\n"))
}

# Model Comparison ----
.aorModelComparison <- function(container, dataset, options, models, analysis) {
  modelComparisonContainer <- .aorGetContainer(
    container    = container,
    name         = "modelComparison",
    title        = gettext("Model Comparison"),
    dependencies = c("restrictedModels", "restrictedModelComparison"),
    position     = 2
  )

  ready <- !container$getError() && length(models[["restricted"]]) > 0

  if(ready) {
    modelComparison <- try(.aorGetModelComparison(modelComparisonContainer, options, models, analysis))
  } else {
    modelComparison <- NULL
  }

  if(isTryError(modelComparison)) {
    message <- .extractErrorMessage(modelComparison)
    modelComparisonContainer$setError(gettextf("Could not compute model comparison! Error message: %s", message))
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
    weights <- modelComparison[["ratio.gw"]]

    if(nrow(result) == 1) {
      modelComparison[["result"]][["ratio"]] <- 1
    } else if (is.null(weights)) {
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
  if(is.null(modelComparison[["ratio.gw"]])) {
    # only one model in the comparison leads to empty relative weight matrix
    comparisonMatrix$setError(gettext("Only one model in the comparison: Cannot compute relative weights matrix"))
  } else {
    modelNames <- rownames(modelComparison[["ratio.gw"]])
    df <- as.data.frame(modelComparison[["ratio.gw"]])
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
                                       dependencies = c("restrictedModelComparisonCoefficients", "restrictedModelComparisonCoefficientsHighlight")
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
  if(is.null(models[["unrestricted"]][["parsForGorica"]])) {
    unrestrictedCoefficients <- coefficients(models[["unrestricted"]][["fit"]])
  } else {
    unrestrictedCoefficients <- models[["unrestricted"]][["parsForGorica"]][["coef"]]
  }
  basicParameters <- names(unrestrictedCoefficients)
  df <- subset(df, subset = coef %in% basicParameters)

  # make sure to return coefficients formatted in the restriktor way
  df[["coef"]] <- .aorRenameInterceptRemoveColon(df[["coef"]])
  names(unrestrictedCoefficients) <- .aorRenameInterceptRemoveColon(basicParameters)

  for(index in seq_len(nrow(result)))
    coefficientsTable$addColumnInfo(name = result[["model"]][[index]], title = result[["modelNames"]][[index]], type = "number")

  coefficientsTable$setData(df)

  if(options[["restrictedModelComparisonCoefficientsHighlight"]])
    .aorModelComparisonHighlightCoefficients(coefficientsTable, unrestrictedCoefficients, df)
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

# Single Model inference ----
.aorSingleModelsInference <- function(container, dataset, options, models, analysis) {
  if(container$getError()) return()

  allModelNames    <- .aorGetModelNames(options[["restrictedModels"]])
  fittedModelNames <- .aorGetModelNames(models[["restricted"]])

  for(modelName in allModelNames) {

    modelContainer <- .aorGetContainer(
      container     = container,
      name          = modelName,
      title         = modelName,
      initCollapsed = TRUE,
      position      = which(modelName == allModelNames) + 3
    )
    modelContainer$dependOn(optionsFromObject = container[[sprintf("state_%s", modelName)]])
    container[[modelName]] <- modelContainer

    if(modelName %in% fittedModelNames) {
      model <- models[["restricted"]][[which(modelName == fittedModelNames)]]
      .aorModelSummary (modelContainer, options, model)
      .aorMarginalMeans(modelContainer, options, model, dataset)
      if(analysis == "anova")
        .aorInformativeHypothesisTest(modelContainer, options, model)
    } else {
      model <- models[["failed"]][[modelName]]
      .aorDisplayModelError(modelContainer, model)
    }
  }
}

## Model Summary ----
.aorModelSummary <- function(container, options, model) {
  if(!is.null(container[["modelSummaryContainer"]]) || !model[["summary"]]) return()

  modelSummaryContainer <- .aorGetContainer(
    container = container,
    name      = "modelSummaryContainer",
    title     = gettext("Model Summary"),
    position  = 1
  )

  .aorModelRestrictionMatrix(modelSummaryContainer, model)
  .aorModelCoefficients     (modelSummaryContainer, model, options)
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

.aorModelCoefficients <- function(container, model, options) {
  coefficientsTable <- createJaspTable(title = gettext("Coefficients"))
  container[["coefficientsTable"]] <- coefficientsTable

  coefs <- model[["coefficients"]]

  if(!isTryError(coefs)) {

    coefficientsTable$addColumnInfo(name = "coef",     title = gettext("Coefficient"), type = "string")
    coefficientsTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type = "number")
    if(!is.null(coefs[["se"]]))
      coefficientsTable$addColumnInfo(name = "se",     title = gettext("SE"),          type = "number")
    if(!is.null(coefs[["t"]]))
      coefficientsTable$addColumnInfo(name = "t",      title = gettext("t"),           type = "number")
    if(!is.null(coefs[["p"]]))
      coefficientsTable$addColumnInfo(name = "p",      title = gettext("p"),           type = "pvalue")


    if(!is.null(model[["bootstrapSamples"]])) {
      overtitle <- gettextf("%s%% CI", 100*options[["restrictedBootstrapCiLevel"]])
      coefficientsTable$addColumnInfo(name = "lower",    title = gettext("Lower"),       type = "number", overtitle = overtitle)
      coefficientsTable$addColumnInfo(name = "upper",    title = gettext("Upper"),       type = "number", overtitle = overtitle)
      coefficientsTable$addFootnote(gettextf("Estimates based on %s successful bootstrap replicates.", model[["bootstrapSamples"]]))
    }

    coefficientsTable$setData(coefs)
  } else {
    message <- .aorExtractErrorMessageSoft(coefs)
    coefficientsTable$setError(gettextf("Could not compute table of coefficients. Error message: %s", message))
  }
}

## Informative Hypothesis tests ----
.aorInformativeHypothesisTest <- function(container, options, model) {
  if(!is.null(container[["ihtTable"]]) || !model[["informedHypothesisTest"]]) return()

  ihtTable <- createJaspTable(title = gettext("Informative Hypothesis Tests"), position = 3)
  container[["ihtTable"]] <- ihtTable
  ihtTable$showSpecifiedColumnsOnly <- TRUE

  ihtTable$addColumnInfo(name = "typeName", title = gettext("Hypothesis"),      type = "string", combine = TRUE)
  ihtTable$addColumnInfo(name = "test",     title = gettext("Test"),            type = "string", combine = TRUE)
  ihtTable$addColumnInfo(name = "stat",     title = gettext("Test Statistic"),  type = "number")
  ihtTable$addColumnInfo(name = "pvalue",   title = gettext("p"),               type = "pvalue")

  result <- try(.aorCalculateIHT(model[["fit"]]))

  if(!isTryError(result)) {
    result[["typeName"]] <- gettextf("Type %s", result[["type"]])
    rownames(result) <- sprintf("row%s", seq_len(nrow(result)))
    ihtTable$setData(result)
    ihtTable$addCitation(c(
      "Silvapulle, M. J. & Sen, P. K. (2005). Constrained statistical inference: Order, inequality, and shape constraints. Hoboken, NJ: Wiley.",
      "Vanbrabant, L. & Rosseel, Y. (2020). Restricted statistical estimation and inference for linear models. http://restriktor.org"
    ))
    # for some reason, this takes forever
    # .aorAddInformativeHypothesisTestFootnotes(ihtTable, result)
  } else {
    message <- .aorExtractErrorMessageSoft(result)
    ihtTable$setError(gettextf("Could not compute the informative hypothesis tests. Error message: %s", message))
  }

}

.aorAddInformativeHypothesisTestFootnotes <- function(table, result) {
  rownames <- rownames(result)

  .aorInformativeHypothesisTestFootnoteHelper(
    table     = table,
    message   = gettextf("H%1$s: All equality restrictions are active (==), H%2$s: At least one equality restriction is violated.", "\u2080", "\u2081"),
    condition = result[["type"]] == "classical",
    rownames  = rownames
    )

  .aorInformativeHypothesisTestFootnoteHelper(
    table     = table,
    message   = gettextf("H%1$s: All parameters are restricted to be equal (==), H%2$s: At least one inequality restriction is strictly true (>).", "\u2080", "\u2081"),
    condition = result[["type"]] == "global",
    rownames  = rownames
  )

  .aorInformativeHypothesisTestFootnoteHelper(
    table     = table,
    message   = gettextf("H%1$s: All restrictions are equalities (==), H%2$s: At least one inequality restriction is strictly true (>).", "\u2080", "\u2081"),
    condition = result[["type"]] == "A",
    rownames  = rownames
  )

  .aorInformativeHypothesisTestFootnoteHelper(
    table     = table,
    message   = gettextf("H%1$s: All restrictions hold, H%2$s: At least one restriction is violated.", "\u2080", "\u2081"),
    condition = result[["type"]] == "B",
    rownames  = rownames
  )

  .aorInformativeHypothesisTestFootnoteHelper(
    table     = table,
    message   = gettextf("H%1$s: At least one restriction is false or active (==), H%2$s: All restrictions are strictly true (>).", "\u2080", "\u2081"),
    condition = result[["type"]] == "C",
    rownames  = rownames
  )

  return()
}

.aorInformativeHypothesisTestFootnoteHelper <- function(table, message, condition, rownames) {
  if(any(condition)) {
    table$addFootnote(
      message  = message,
      rowNames = rownames[min(which(condition))],
      colName  = "typeName"
    )
  }
}

## Marginal means ----
.aorMarginalMeans <- function(container, options, model, dataset) {
  if(!model[["marginalMean"]]) return()

  marginalMeansContainer <- .aorGetContainer(
    container    = container,
    name         = "marginalMeansContainer",
    title        = gettext("Marginal Means"),
    position     = 2,
    dependencies = c("restrictedMarginalMeanTerms",
                     "restrictedBootstrapCiLevel")
  )

  if(length(options[["restrictedMarginalMeanTerms"]]) == 0 || all(options[["restrictedMarginalMeanTerms"]] == "")) {
    # settting an error on empty container does not show up, so we will make an empty table
    marginalMeansContainer[["table"]] <- createJaspTable(title = gettext("Marginal Means"))
    marginalMeansContainer$setError(gettext("No marginal means terms specified. Please, select model terms in the 'Restricted Marginal Means' section."))
  } else {
    terms <- options[["restrictedMarginalMeanTerms"]]
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

  # analytic SEs are only available for ANOVAs, not RM anovas
  if(inherits(model[["fit"]], "restriktor") || !is.null(model[["bootstrapSamples"]]))
    marginalMeansTable$addColumnInfo(name="SE",     title = gettext("SE"),            type="number")

  if(!is.null(model[["bootstrapSamples"]])) {
    marginalMeansTable$addColumnInfo(name="lower.CL", title = gettext("Lower"),         type="number", overtitle = gettextf("%s%% CI", 100*options[["restrictedBootstrapCiLevel"]]))
    marginalMeansTable$addColumnInfo(name="upper.CL", title = gettext("Upper"),         type="number", overtitle = gettextf("%s%% CI", 100*options[["restrictedBootstrapCiLevel"]]))

    marginalMeansTable$addFootnote(gettextf("Estimates based on %s successful bootstrap replicates.", model[["bootstrapSamples"]]))
  }

  # fill
  result <- model[["marginalMeansResult"]][[paste0(variables, collapse = "_")]]

  if(isTryError(result)) {
    message <- .extractErrorMessage(result)
    marginalMeansTable$setError(gettextf("Could not compute the marginal means. Error message: %s", message))
  } else {
    marginalMeansTable$setData(result)

    # add footnotes
    if(!is.null(attr(result, "avgd.over")))
      marginalMeansTable$addFootnote(
        message = gettextf("Results are averaged over the levels of: %s.",
                          paste(attr(result, "avgd.over"), collapse = ", ")
                          )
        )
    if(!is.null(options[["covariates"]]) && length(options[["covariates"]]) != 0) {
      activeCovariates <- options[["covariates"]][options[["covariates"]] %in% unique(unlist(options[["modelTerms"]]))]
      marginalMeansTable$addFootnote(
        message = gettextf("Covariates (%s) held fixed at their mean values.", paste(activeCovariates, collapse = ", "))
      )
    }
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

.aorCalculateBootstrapping <- function(unrestrictedBootstrap, fit, syntax, modelName) {
  samples    <- length(unrestrictedBootstrap)
  ncoefs     <- length(coefficients(fit))
  bootstraps <- matrix(nrow = samples, ncol = ncoefs)
  keep       <- c()

  for(i in seq_len(samples)) {
    unconstrained <- unrestrictedBootstrap[[i]]
    if(inherits(fit, "restriktor")) { # an(c)ova
      boot <- try(restriktor::restriktor(object = unconstrained, constraints = syntax, se = fit[["se"]]))
    } else { # rm anova
      model <- list(parsForGorica = .rmaorExtractPars(unconstrained))
      boot <- try(.rmaorCalculateRestrictedModel(model, syntax))
    }

    if(!isTryError(boot)) {
      bootstraps[i, ] <- coefficients(boot)
      keep <- c(keep, i)
    }

  }
  bootstraps <- bootstraps[keep,,drop=FALSE]
  colnames(bootstraps) <- names(coefficients(fit))

  return(bootstraps)
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

.aorCalculateMarginalMeans <- function(fit, bootstraps = NULL, ciLevel, terms, dataset) {
  results <- list()
  for(i in seq_along(terms)) {
    variables <- terms[[i]][["components"]]
    results[[paste0(variables, collapse = "_")]] <- try(.aorCalculateMarginalMeansTerm(fit, bootstraps, ciLevel, variables, dataset))
  }

  return(results)
}

.aorCalculateMarginalMeansTerm <- function(fit, bootstraps, ciLevel, variables, dataset) {
  if(is.null(bootstraps)) {
    if(inherits(fit, "restriktor")) { # an(c)ova
      vcov    <- summary(fit)[["V"]]
      refGrid <- emmeans::qdrg(
        formula   = formula(fit[["model.org"]]),
        data      = dataset,
        coef      = fit[["b.restr"]],
        vcov      = vcov,
        df        = fit[["df.residual"]],
        contrasts = fit[["model.org"]][["contrasts"]]
      )
    } else { # rmanova
      refGrid <- emmeans::ref_grid(fit[["model.org"]], mult.levs = fit[["rmFactors"]])
      refGrid@bhat <- fit[["b.restr"]]
    }

    means <- emmeans::lsmeans(refGrid, variables, infer=c(FALSE, FALSE))
    avgd.over <- slot(means, "misc")[["avgd.over"]]
    means <- summary(means)
  } else {
    if(inherits(fit, "restriktor")) { # an(c)ova
      refGrid <- emmeans::qdrg(
        formula   = formula(fit[["model.org"]]),
        data      = dataset,
        mcmc      = bootstraps[, names(fit[["b.restr"]]), drop = FALSE],
        contrasts = fit[["model.org"]][["contrasts"]]
      )
      means <- emmeans::lsmeans(refGrid, variables)
      avgd.over <- slot(means, "misc")[["avgd.over"]]
      boots <- emmeans::as.mcmc.emmGrid(means)
      boots <- as.data.frame(boots)
    } else { # rm anova
      refGrid <- emmeans::ref_grid(fit[["model.org"]], mult.levs = fit[["rmFactors"]])
      res <- list()
      for(i in seq_len(nrow(bootstraps))) {
        b.restr <- bootstraps[i, names(fit[["b.restr"]]), drop = TRUE]
        refGrid@bhat <- b.restr
        means <- emmeans::lsmeans(refGrid, variables, infer=c(FALSE, FALSE))
        res[[i]] <- summary(means)[["lsmean"]]
      }
      avgd.over <- slot(means, "misc")[["avgd.over"]]
      boots <- do.call(rbind, res)
    }

    alpha <- 1-ciLevel
    means <- summary(means)
    means[["lsmean"]]   <- apply(boots, 2, median,                      na.rm = TRUE)
    means[["SE"]]       <- apply(boots, 2, sd,                          na.rm = TRUE)
    means[["lower.CL"]] <- apply(boots, 2, quantile, probs =   alpha/2, na.rm = TRUE)
    means[["upper.CL"]] <- apply(boots, 2, quantile, probs = 1-alpha/2, na.rm = TRUE)
  }

  if(!is.null(avgd.over) && length(avgd.over))
    attr(means, "avgd.over") <- avgd.over

  return(means)
}

.aorCalculateCoefficients <- function(fit, bootstraps, ciLevel) {
  if(is.null(bootstraps) && inherits(fit, "restriktor")) { #an(c)ova
    result <- coefficients(summary(fit))
    result <- as.data.frame(result)
    result[["coef"]] <- .aorRenameInterceptRemoveColon(rownames(result))
    colnames(result) <- c("estimate", "se", "t", "p", "coef")
  } else if(is.null(bootstraps)) { # rm anova
    result <- coefficients(fit)
    result <- data.frame(
      coef     = .aorRenameInterceptRemoveColon(names(result)),
      estimate = result
    )
  } else {
    alpha <- 1-ciLevel
    result <- data.frame(
      coef     = .aorRenameInterceptRemoveColon(colnames(bootstraps)),
      estimate = apply(bootstraps, 2, median,                      na.rm = TRUE),
      se       = apply(bootstraps, 2, sd    ,                      na.rm = TRUE),
      lower    = apply(bootstraps, 2, quantile, probs =   alpha/2, na.rm = TRUE),
      upper    = apply(bootstraps, 2, quantile, probs = 1-alpha/2, na.rm = TRUE)
    )
  }

  return(result)
}

.aorCalculateUnrestrictedModel <- function(dataset, options) {
  reorderModelTerms <- .reorderModelTerms(options)
  modelTerms        <- reorderModelTerms$modelTerms
  modelDef          <- .modelFormula(modelTerms, options)

  if (options[["restrictedInterceptInclusion"]]) {
    formula <- as.formula(modelDef[["model.def"]])
  } else {
    formula <- as.formula(paste(modelDef[["model.def"]], "- 1"))
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

  modelMatrix <- model.matrix(formula, dataset, contrasts.arg = contrasts)

  fit <- lm(
    formula   = formula,
    data      = dataset,
    weights   = weights,
    contrasts = contrasts
  )

  model <- list(
    fit         = fit,
    formula     = formula,
    modelMatrix = modelMatrix,
    weights     = weights,
    contrasts   = contrasts
  )

  return(model)
}

.aorCalculateModelComparison <- function(options, models, comparison) {
  modelList <- lapply(models[["restricted"]], "[[", "fit")
  names(modelList)[1] <- "object"
  modelComparison <- try(do.call(restriktor::goric, c(modelList, comparison = comparison)))
  return(modelComparison)
}

.rmaorCalculateUnrestrictedModel <- function(dataset, options) {
  # get formula for a lm fit
  lhs <- sprintf("cbind(%s)", paste(options[["repeatedMeasuresCells"]], collapse = ","))

  if(!options[["restrictedInterceptInclusion"]] && length(options[["betweenModelTerms"]]) > 0) {
    rhs <- "0"
  } else {
    rhs <- "1"
  }

  for(term in options[["betweenModelTerms"]]) {
    components <- term[["components"]]
    rhs <- c(rhs, paste(components, collapse = ":"))
  }

  rhs <- paste(rhs, collapse = "+")

  formula <- as.formula(sprintf("%s~%s", lhs, rhs))

  # weights (are none for rm anova)
  weights <- NULL

  # get dummy coding contrasts for between subject factors
  allFactors <- options[["betweenSubjectFactors"]]
  factorsInFormula <- allFactors[allFactors %in% all.vars(formula)]
  contrasts <- replicate(length(factorsInFormula), "contr.treatment", simplify = FALSE)
  names(contrasts) <- factorsInFormula

  fit <- lm(
    formula   = formula,
    data      = dataset,
    weights   = weights,
    contrasts = contrasts
  )

  # this is needed later for marginal means (to create a refgrid object)
  rmFactors        <- lapply(options[["repeatedMeasuresFactors"]], "[[", "levels")
  names(rmFactors) <- sapply(options[["repeatedMeasuresFactors"]], "[[", "name"  )
  # ref_grid argument mult.levs combines the levels according to expand.grid()
  # but repeated measured cells are in the reversed order -> we reverse the list of factors here
  rmFactors        <- rev(rmFactors)

  model <- list(
    fit           = fit,
    formula       = formula,
    weights       = weights,
    contrasts     = contrasts,
    parsForGorica = .rmaorExtractPars(fit),
    rmFactors     = rmFactors
  )

  return(model)
}

.rmaorExtractPars <- function(fit) {
  coef <- coefficients(fit)
  vcov <- vcov(fit)

  if(nrow(coef) == 1) { # there is no between subjects factor or a covariate, so we do not need to include the intercept in the syntax
    names <- colnames(coef)
  } else { # we need to combine row and col names as in vcov
    names <- colnames(vcov)
  }

  coef <- as.vector(coef)
  colnames(vcov) <- rownames(vcov) <- names(coef) <- names

  return(list(
    coef = coef,
    vcov = vcov
  ))
}

.rmaorCalculateRestrictedModel <- function(unrestrictedModel, syntax) {
  args <- list(
    object      = unrestrictedModel[["parsForGorica"]][["coef"]],
    VCOV        = unrestrictedModel[["parsForGorica"]][["vcov"]],
    constraints = syntax,
    comparison  = "none",
    type        = "gorica"
  )
  fit <- do.call(restriktor::goric, args)
  fit <- fit[["objectList"]][[1]]

  if(!is.null(unrestrictedModel[["fit"]])) {
    fit[["model.org"]] <- unrestrictedModel[["fit"]]
  }

  if(!is.null(unrestrictedModel[["rmFactors"]])) {
    fit[["rmFactors"]] <- unrestrictedModel[["rmFactors"]]
  }

  return(fit)
}

.rmaorCalculateModelComparison <- function(options, models, comparison) {
  args <- as.list(.aorGetModelSyntaxes(models = models[["restricted"]]))
  args[["object"]]     <- models[["unrestricted"]][["parsForGorica"]][["coef"]]
  args[["VCOV"]]       <- models[["unrestricted"]][["parsForGorica"]][["vcov"]]
  args[["comparison"]] <- comparison
  args[["type"]]       <- "gorica"

  modelComparison <- do.call(restriktor::goric, args)
  return(modelComparison)
}

coef.gorica_est <- restriktor::coef.restriktor
