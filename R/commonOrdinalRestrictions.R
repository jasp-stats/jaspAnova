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

.aorGetModelSyntax <- function(options, prune = TRUE) {
  restrictedModels <- options[["restrictedModels"]]

  if(prune)
    restrictedModels <- restrictedModels[vapply(restrictedModels, function(mod) mod[["restrictionSyntax"]] != "", logical(1))]

  return(restrictedModels)
}

.aorIsReady <- function(options) {
  restrictedModels <- .aorGetModelSyntax(options)
  return(length(restrictedModels) != 0L)
}

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

.aorGetRestrictedModel <- function(restrictedModel, dataset, options, unrestrictedModel) {
  modelName           <- restrictedModel[["modelName"]]
  modelSyntaxOriginal <- restrictedModel[["restrictionSyntax"]]
  modelSyntax         <- .aorTranslateSyntax(modelSyntaxOriginal, dataset, options, modelName)

  .aorCheckSyntax(modelName, modelSyntax)

  fit <- restriktor::restriktor(unrestrictedModel[["fit"]], constraints = modelSyntax)

  model <- list(
    fit         = fit,
    modelName   = modelName,
    modelSyntax = modelSyntax
  )

  return(model)
}

.aorGetRestrictedModels <- function(dataset, options, unrestrictedModel) {
  modelSyntaxes <- .aorGetModelSyntax(options = options)

  restrictedModels <- lapply(modelSyntaxes, function(x) try(.aorGetRestrictedModel(x, dataset, options, unrestrictedModel)))

  return(restrictedModels)
}

.aorGetFittedModels <- function(container, dataset, options) {
  if(!is.null(container[["modelState"]])) return(container[["modelState"]]$object)

  models <- list()

  models[["unrestricted"]] <- try(.aorGetUnrestrictedModel(dataset, options))
  if(isTryError(models[["unrestricted"]])) {
    message <- .extractErrorMessage(models[["unrestricted"]])
    container$setError(gettextf("Could not fit the unrestricted model. As a result, none of the restricted models could be estimated. Error message: %s.\n"), message)
    return()
  }

  models[["restricted"]] <- .aorGetRestrictedModels(dataset, options, models[["unrestricted"]])

  container[["modelState"]] <- createJaspState(
    object = models,
    dependencies = c("includeIntercept")
  )

  return(models)
}

.aorBasicInfo <- function(container, dataset, options) {
  models <- .aorGetFittedModels(container, dataset, options)

  if(container$getError()) return()

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

.aorModelComparison <- function(container, dataset, options) {
  models <- .aorGetFittedModels(container, dataset, options)

  if(container$getError()) return()

  if(is.null(container[["modelComparison"]])) {
    modelComparisonContainer <- createJaspContainer(title = gettext("Model Comparison"), position = 3)
    modelComparisonContainer$dependOn(c("restrictedModelComparison"))
    container[["modelComparison"]] <- modelComparisonContainer
  } else {
    modelComparisonContainer <- container[["modelComparison"]]
  }

  modelComparison <- .aorGetModelComparison(modelComparisonContainer, options, models)

  if(isTryError(modelComparison)) {
    message <- .extractErrorMessage(modelComparison)
    modelComparisonContainer$setError(gettext("Could not compute model comparison! Error message: %", message))
  } else {
    .aorModelComparisonTable              (modelComparisonContainer, options, modelComparison)
    .aorModelComparisonMatrix             (modelComparisonContainer, options, modelComparison)
    .aorModelComparisonCompareCoefficients(modelComparisonContainer, options, modelComparison, models)
  }
}

.aorModelComparisonTable <- function(container, options, modelComparison) {
  if(!is.null(container[["comparisonTable"]])) return()

  type       <- modelComparison[["type"]]
  comparison <- modelComparison[["comparison"]]
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
  } else {
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


.aorModelComparisonMatrix <- function(container, options, modelComparison) {
  if(!is.null(container[["comparisonMatrix"]]) || !options[["restrictedModelComparisonMatrix"]]) return()

  # define
  comparisonMatrix <- createJaspTable(title        = gettextf("Relative %s-Weights", toupper(modelComparison[["type"]])),
                                      position     = 2,
                                      dependencies = c("restrictedModelComparisonMatrix")
                                      )
  comparisonMatrix$addColumnInfo(name = "model", title = gettext("Model"))
  container[["comparisonMatrix"]] <- comparisonMatrix

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

.aorModelComparisonCompareCoefficients <- function(container, options, modelComparison, models) {
  if(!is.null(container[["coefficientsTable"]]) || !options[["restrictedModelComparisonCoefficients"]]) return()

  coefficientsTable <- createJaspTable(title        = gettext("Coefficients Comparison"),
                                       position     = 3,
                                       dependencies = c("restrictedModelComparisonCoefficients", "highlightEstimates")
                                       )
  coefficientsTable$showSpecifiedColumnsOnly <- TRUE
  coefficientsTable$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "string")


  container[["coefficientsTable"]] <- coefficientsTable

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
  if (!is.null(container[["modelComparison"]])) return(container[["modelComparison"]]$object)

  # fail early
  if(isTryError(models[["restricted"]])) {
    modelComparison <- gettext("Model comparison can be computed only when all models can be computed.")
    class(modelComparison) <- "try-error"
    return(modelComparison)
  }

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

  container[["modelComparison"]] <- createJaspState(object = modelComparison)

  return(modelComparison)
}


# Utilities ----

# The following function is an adaptation of stats::contr.sum2
# which removes colnames, which leads to dropping the level names
# from the coefficients if sum contrasts are used in a lm()
# which is important to keep for restriktor syntax.
# Example:
# df <- data.frame(y = rnorm(10), group = rep(c("experimental", "control"), each = 5), home = rep(c("city", "rural"), times = 5))
# lm(y ~ group*home, df, contrasts = list(group = "contr.sum", home = "contr.sum"))
# lm(y ~ group*home, df, contrasts = list(group = "contr.sum2", home = "contr.sum2"))
# compare with:
# lm(y ~ group*home, df, contrasts = list(group = "contr.treatment", home = "contr.treatment"))
#
# Original code taken from: https://svn.r-project.org/R/trunk/src/library/stats/R/contrast.R
# R - Revision 81690
contr.sum2 <- function (n, contrasts = TRUE, sparse = FALSE) {
  if (length(n) <= 1L) {
    if (is.numeric(n) && length(n) == 1L && n > 1L)
      levels <- seq_len(n)
    else stop("not enough degrees of freedom to define contrasts")
  } else levels <- n

  levels <- as.character(levels)
  cont <- stats:::.Diag(levels, sparse=sparse)
  if (contrasts) {
    cont <- cont[, -length(levels), drop = FALSE]
    cont[length(levels), ] <- -1
    # this is the line that caused problems
    # colnames(cont) <- NULL
  }
  cont
}
