.getAnovaOrdinalRestrictionsContainer <- function(container) {
  if (is.null(anovaContainer[["ordinalRestrictions"]])) {
    ordinalRestrictionsContainer <- createJaspContainer(title = gettext("Order Restrictions"), dependencies = "restrictedModels")
    anovaContainer[["ordinalRestrictions"]] <- ordinalRestrictionsContainer
  } else {
    ordinalRestrictionsContainer <- anovaContainer[["ordinalRestrictions"]]
  }

  return(ordinalRestrictionsContainer)
}

.getAnovaOrdinalRestrictionsModelSyntax <- function(options, prune = TRUE) {
  restrictedModels <- options[["restrictedModels"]]

  if(prune)
    restrictedModels <- restrictedModels[vapply(restrictedModels, function(mod) mod[["restrictionSyntax"]] != "", logical(1))]

  return(restrictedModels)
}

.isAnovaOrdinalRestrictionsReady <- function(options) {
  restrictedModels <- .getAnovaOrdinalRestrictionsModelSyntax(options)
  return(length(restrictedModels) != 0L)
}

.checkAnovaOrdinalRestrictionsSyntax <- function(modelName, modelSyntax) {
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
.anovaOrdinalRestrictionsGetUsedVars <- function(syntax, availablevars) {
  allVars <- decodeColNames(availablevars)
  inSyntax <- stringr::str_detect(syntax, pattern = allVars)
  return(allVars[inSyntax])
}

.anovaOrdinalRestrictionsTranslateSyntax <- function(syntax, dataset, options, modelName) {
  usedvars <- .anovaOrdinalRestrictionsGetUsedVars(syntax, colnames(dataset))

  new.names <- encodeColNames(usedvars)

  for (i in seq_along(usedvars)) {
    syntax <- try(gsub(usedvars[i], new.names[i], syntax))
  }

  if(isTryError(syntax)) {
    stop(gettext("There are errors in the restriction syntax for %s. The syntax could not be decoded!", modelName))
  }

  return(syntax)
}

.getAnovaOrdinalRestrictionsUnrestrictedModel <- function(dataset, options) {
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

  modelMatrix <- model.matrix(modelFormula, dataset)

  fit <- lm(
    formula   = modelFormula,
    data      = dataset,
    weights   = weights,
    contrasts = modelMatrix
    )

  model <- list(
    fit          = fit,
    modelMatrix  = modelMatrix,
    modelFormula = modelFormula
  )

  return(model)
}

.getAnovaOrdinalRestrictionsRestrictedModel <- function(restrictedModel, dataset, options, unrestrictedModel) {
  modelName           <- restrictedModel[["modelName"]]
  modelSyntaxOriginal <- restrictedModel[["modelSyntax"]]
  modelSyntax         <- .anovaOrdinalRestrictionsTranslateSyntax(modelSyntaxOriginal, dataset, options, modelName)

  .checkAnovaOrdinalRestrictionsSyntax(modelName, modelSyntax)

  fit <- restriktor::restriktor(unrestrictedModel[["fit"]], constraints = modelSyntax)

  model <- list(
    fit         = fit,
    modelName   = modelName,
    modelSyntax = modelSyntax
  )

  return(model)
}

.getAnovaOrdinalRestrictionsRestricedModels <- function(dataset, options, unrestrictedModel) {
  modelSyntaxes <- .getAnovaOrdinalRestrictionsModelSyntax(options = options)

  restrictedModels <- lapply(modelSyntaxes, function(x) try(.getAnovaOrdinalRestrictionsRestrictedModel(x, dataset, options, unrestrictedModel)))

  return(restrictedModels)
}

.getAnovaOrdinalRestrictionsFittedModels <- function(container, dataset, options, which = NULL) {
  if(!is.null(container[["modelState"]])) {
    models <- container[["modelState"]]$object
  } else {
    models <- list()

    models[["unrestricted"]] <- try(.getAnovaOrdinalRestrictionsUnrestrictedModel(dataset, options))
    if(isTryError(models[["unrestricted"]])) {
      message <- .extractErrorMessage(models[["unrestricted"]])
      container$setError(gettextf("Could not fit the unrestricted model. As a result, none of the restricted models could be estimated. Error message: %s.\n"), message)
      return()
    }

    models[["restricted"]] <- .getAnovaOrdinalRestrictionsRestricedModels(dataset, options, models[["unrestricted"]])

    container[["modelState"]] <- createJaspState(
      object = models,
      dependencies = c("includeIntercept")
    )
  }

  if(is.null(which)) {
    return(models)
  } else {
    return(models[[which]])
  }
}

.anovaOrdinalRestrictionsModelSummary <- function(container, dataset, options) {
  models <- .getAnovaOrdinalRestrictionsFittedModels(container, dataset, options)
}
