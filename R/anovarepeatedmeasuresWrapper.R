#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' AnovaRepeatedMeasures
#'
AnovaRepeatedMeasures <- function(
          data = NULL,
          version = "0.95",
          barPlotCiInterval = 0.95,
          barPlotErrorBarType = "ci",
          barPlotErrorBars = FALSE,
          barPlotHorizontalAxis = list(types = list(), value = ""),
          barPlotHorizontalZeroFix = TRUE,
          barPlotSeparatePlots = list(types = list(), value = ""),
          betweenModelTerms = list(optionKey = "components", types = list(), value = list()),
          betweenSubjectFactors = list(types = list(), value = list()),
          conoverTest = FALSE,
          contrastCi = FALSE,
          contrastCiLevel = 0.95,
          contrastEffectSize = FALSE,
          contrasts = list(optionKey = "variable", types = "unknown", value = list(list(contrast = "none", variable = "RM Factor 1"))),
          covariates = list(types = list(), value = list()),
          customContrasts = list(),
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = FALSE,
          descriptivePlotErrorBarPooled = FALSE,
          descriptivePlotErrorBarType = "ci",
          descriptivePlotHorizontalAxis = list(types = list(), value = ""),
          descriptivePlotSeparateLines = list(types = list(), value = ""),
          descriptivePlotSeparatePlot = list(types = list(), value = ""),
          descriptivePlotYAxisLabel = "",
          descriptives = FALSE,
          effectSizeCi = FALSE,
          effectSizeCiLevel = 0.95,
          effectSizeEstimates = FALSE,
          effectSizeEtaSquared = FALSE,
          effectSizeGeneralEtaSquared = FALSE,
          effectSizeOmegaSquared = TRUE,
          effectSizePartialEtaSquared = FALSE,
          effectSizePartialOmegaSquared = FALSE,
          friedmanBetweenFactor = list(types = list(), value = ""),
          friedmanWithinFactor = list(types = list(), value = list()),
          homogeneityTests = FALSE,
          labelYAxisTwo = "",
          marginalMeanBootstrap = FALSE,
          marginalMeanBootstrapSamples = 1000,
          marginalMeanCiCorrection = "none",
          marginalMeanComparedToZero = FALSE,
          marginalMeanTerms = list(optionKey = "variable", types = list(), value = list()),
          normalizeErrorBarsBarplot = TRUE,
          normalizeErrorBarsDescriptives = TRUE,
          plotHeight = 320,
          plotHeightDescriptivesPlotLegend = 300,
          plotHeightDescriptivesPlotNoLegend = 300,
          plotHeightQQPlot = 300,
          plotWidth = 480,
          plotWidthDescriptivesPlotLegend = 430,
          plotWidthDescriptivesPlotNoLegend = 350,
          plotWidthQQPlot = 300,
          poolErrorTermFollowup = FALSE,
          postHocCi = FALSE,
          postHocCiLevel = 0.95,
          postHocConditionalTable = FALSE,
          postHocCorrectionBonferroni = FALSE,
          postHocCorrectionHolm = TRUE,
          postHocCorrectionScheffe = FALSE,
          postHocCorrectionTukey = FALSE,
          postHocEffectSize = FALSE,
          postHocLetterAlpha = 0.05,
          postHocLetterTable = FALSE,
          postHocSignificanceFlag = FALSE,
          postHocTerms = list(optionKey = "variable", types = list(), value = list()),
          qqPlot = FALSE,
          rainCloudHorizontalAxis = list(types = list(), value = ""),
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = list(types = list(), value = ""),
          rainCloudYAxisLabel = "",
          repeatedMeasuresCells = list("", ""),
          repeatedMeasuresFactors = list(list(levels = list("Level 1", "Level 2"), name = "RM Factor 1")),
          restrictedAvailableCoefficients = FALSE,
          restrictedBootstrap = FALSE,
          restrictedBootstrapCiLevel = 0.95,
          restrictedBootstrapSamples = 1000,
          restrictedHeterogeneityCorrection = "none",
          restrictedInformedHypothesisTestForAllModels = FALSE,
          restrictedInterceptInclusion = FALSE,
          restrictedMarginalMeanForAllModels = FALSE,
          restrictedMarginalMeanTerms = list(optionKey = "variable", types = list(), value = list()),
          restrictedModelComparison = "complement",
          restrictedModelComparisonCoefficients = FALSE,
          restrictedModelComparisonCoefficientsHighlight = TRUE,
          restrictedModelComparisonMatrix = FALSE,
          restrictedModelComparisonReference = "Model 1",
          restrictedModelComparisonWeights = TRUE,
          restrictedModelSummaryForAllModels = FALSE,
          restrictedModels = list(list(informedHypothesisTest = FALSE, marginalMean = FALSE, name = "Model 1", summary = FALSE, syntax = "")),
          simpleMainEffectErrorTermPooled = FALSE,
          simpleMainEffectFactor = list(types = list(), value = ""),
          simpleMainEffectModeratorFactorOne = list(types = list(), value = ""),
          simpleMainEffectModeratorFactorTwo = list(types = list(), value = ""),
          sphericityCorrectionGreenhouseGeisser = FALSE,
          sphericityCorrectionHuynhFeldt = FALSE,
          sphericityCorrectionNone = TRUE,
          sphericityTests = FALSE,
          sumOfSquares = "type3",
          usePooledStandErrorCITwo = FALSE,
          vovkSellke = FALSE,
          withinModelTerms = list(optionKey = "components", types = "unknown", value = list(list(components = "RM Factor 1")))) {

   defaultArgCalls <- formals(jaspAnova::AnovaRepeatedMeasures)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("barPlotHorizontalAxis", "barPlotSeparatePlots", "betweenModelTerms", "betweenSubjectFactors", "contrasts", "covariates", "customContrasts", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "friedmanBetweenFactor", "friedmanWithinFactor", "marginalMeanCiCorrection", "marginalMeanTerms", "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "repeatedMeasuresCells", "repeatedMeasuresFactors", "restrictedHeterogeneityCorrection", "restrictedMarginalMeanTerms", "restrictedModelComparison", "restrictedModelComparisonReference", "restrictedModels", "simpleMainEffectFactor", "simpleMainEffectModeratorFactorOne", "simpleMainEffectModeratorFactorTwo", "sumOfSquares", "withinModelTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspAnova", "AnovaRepeatedMeasures", "AnovaRepeatedMeasures.qml", options, version, FALSE))
}