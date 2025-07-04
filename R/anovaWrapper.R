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

#' Anova
#'
Anova <- function(
          data = NULL,
          version = "0.95",
          formula = NULL,
          barPlotCiInterval = 0.95,
          barPlotErrorBarType = "ci",
          barPlotErrorBars = FALSE,
          barPlotHorizontalAxis = list(types = list(), value = ""),
          barPlotHorizontalZeroFix = TRUE,
          barPlotSeparatePlots = list(types = list(), value = ""),
          contrastCi = FALSE,
          contrastCiLevel = 0.95,
          contrastEffectSize = FALSE,
          contrasts = list(optionKey = "variable", types = list(), value = list()),
          customContrasts = list(),
          dependent = list(types = list(), value = ""),
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = FALSE,
          descriptivePlotErrorBarType = "ci",
          descriptivePlotHorizontalAxis = list(types = list(), value = ""),
          descriptivePlotSeparateLines = list(types = list(), value = ""),
          descriptivePlotSeparatePlot = list(types = list(), value = ""),
          descriptives = FALSE,
          effectSizeCi = FALSE,
          effectSizeCiLevel = 0.95,
          effectSizeEstimates = FALSE,
          effectSizeEtaSquared = FALSE,
          effectSizeGeneralEtaSquared = FALSE,
          effectSizeOmegaSquared = TRUE,
          effectSizePartialEtaSquared = FALSE,
          effectSizePartialOmegaSquared = FALSE,
          fixedFactors = list(types = list(), value = list()),
          homogeneityCorrectionBrown = FALSE,
          homogeneityCorrectionNone = TRUE,
          homogeneityCorrectionWelch = FALSE,
          homogeneityTests = FALSE,
          kruskalCiLevel = 0.95,
          kruskalEffectSizeEstimates = FALSE,
          kruskalEpsilon = TRUE,
          kruskalEta = FALSE,
          kruskalWallisFactors = list(types = list(), value = list()),
          marginalMeanBootstrap = FALSE,
          marginalMeanBootstrapSamples = 1000,
          marginalMeanCiCorrection = "none",
          marginalMeanComparedToZero = FALSE,
          marginalMeanTerms = list(optionKey = "variable", types = list(), value = list()),
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          normalizeErrorBarsDescriptives = TRUE,
          plotHeight = 320,
          plotHeightDescriptivesPlotLegend = 300,
          plotHeightDescriptivesPlotNoLegend = 300,
          plotHeightQQPlot = 300,
          plotWidth = 480,
          plotWidthDescriptivesPlotLegend = 430,
          plotWidthDescriptivesPlotNoLegend = 350,
          plotWidthQQPlot = 300,
          postHocCi = FALSE,
          postHocCiLevel = 0.95,
          postHocConditionalTable = FALSE,
          postHocCorrectionBonferroni = FALSE,
          postHocCorrectionHolm = FALSE,
          postHocCorrectionScheffe = FALSE,
          postHocCorrectionSidak = FALSE,
          postHocCorrectionTukey = TRUE,
          postHocLetterAlpha = 0.05,
          postHocLetterTable = FALSE,
          postHocSignificanceFlag = FALSE,
          postHocTerms = list(optionKey = "variable", types = list(), value = list()),
          postHocTypeDunn = FALSE,
          postHocTypeDunnet = FALSE,
          postHocTypeGames = FALSE,
          postHocTypeStandard = TRUE,
          postHocTypeStandardBootstrap = FALSE,
          postHocTypeStandardBootstrapSamples = 1000,
          postHocTypeStandardEffectSize = FALSE,
          predictionsSavedToData = FALSE,
          predictionsSavedToDataColumn = "",
          qqPlot = FALSE,
          rainCloudHorizontalAxis = list(types = list(), value = ""),
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = list(types = list(), value = ""),
          rainCloudYAxisLabel = "",
          randomFactors = list(types = list(), value = list()),
          residualsSavedToData = FALSE,
          residualsSavedToDataColumn = "",
          residualsSavedToDataType = "raw",
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
          simpleMainEffectFactor = list(types = list(), value = ""),
          simpleMainEffectModeratorFactorOne = list(types = list(), value = ""),
          simpleMainEffectModeratorFactorTwo = list(types = list(), value = ""),
          sumOfSquares = "type3",
          vovkSellke = FALSE,
          wlsWeights = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspAnova::Anova)
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

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }
   optionsWithFormula <- c("barPlotHorizontalAxis", "barPlotSeparatePlots", "contrasts", "customContrasts", "dependent", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "fixedFactors", "kruskalWallisFactors", "marginalMeanCiCorrection", "marginalMeanTerms", "modelTerms", "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "randomFactors", "restrictedHeterogeneityCorrection", "restrictedMarginalMeanTerms", "restrictedModelComparison", "restrictedModelComparisonReference", "restrictedModels", "simpleMainEffectFactor", "simpleMainEffectModeratorFactorOne", "simpleMainEffectModeratorFactorTwo", "sumOfSquares", "wlsWeights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspAnova", "Anova", "Anova.qml", options, version, FALSE))
}