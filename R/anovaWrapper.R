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

# This is a generated file. Don't change it

Anova <- function(
          data = NULL,
          version = "0.19",
          formula = NULL,
          applyMoreyCorrectionErrorBars = TRUE,
          barPlotCiInterval = 0.95,
          barPlotErrorBarType = "ci",
          barPlotErrorBars = FALSE,
          barPlotHorizontalAxis = "",
          barPlotHorizontalZeroFix = TRUE,
          barPlotSeparatePlots = "",
          contrastCi = FALSE,
          contrastCiLevel = 0.95,
          contrastEffectSize = FALSE,
          contrasts = list(),
          customContrasts = list(),
          dependent = "",
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = FALSE,
          descriptivePlotErrorBarType = "ci",
          descriptivePlotHorizontalAxis = "",
          descriptivePlotSeparateLines = "",
          descriptivePlotSeparatePlot = "",
          descriptives = FALSE,
          effectSizeCi = FALSE,
          effectSizeCiLevel = 0.95,
          effectSizeEstimates = FALSE,
          effectSizeEtaSquared = FALSE,
          effectSizeGeneralEtaSquared = FALSE,
          effectSizeOmegaSquared = TRUE,
          effectSizePartialEtaSquared = FALSE,
          effectSizePartialOmegaSquared = FALSE,
          fixedFactors = list(),
          homogeneityCorrectionBrown = FALSE,
          homogeneityCorrectionNone = TRUE,
          homogeneityCorrectionWelch = FALSE,
          homogeneityTests = FALSE,
          kruskalCiLevel = 0.95,
          kruskalEffectSizeEstimates = FALSE,
          kruskalEpsilon = TRUE,
          kruskalEta = FALSE,
          kruskalWallisFactors = list(),
          marginalMeanBootstrap = FALSE,
          marginalMeanBootstrapSamples = 1000,
          marginalMeanCiCorrection = "none",
          marginalMeanComparedToZero = FALSE,
          marginalMeanTerms = list(),
          modelTerms = list(),
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
          postHocTerms = list(),
          postHocTypeDunn = FALSE,
          postHocTypeDunnet = FALSE,
          postHocTypeGames = FALSE,
          postHocTypeStandard = TRUE,
          postHocTypeStandardBootstrap = FALSE,
          postHocTypeStandardBootstrapSamples = 1000,
          postHocTypeStandardEffectSize = FALSE,
          qqPlot = FALSE,
          rainCloudHorizontalAxis = "",
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = "",
          rainCloudYAxisLabel = "",
          randomFactors = list(),
          restrictedAvailableCoefficients = FALSE,
          restrictedBootstrap = FALSE,
          restrictedBootstrapCiLevel = 0.95,
          restrictedBootstrapSamples = 1000,
          restrictedHeterogeneityCorrection = "none",
          restrictedInformedHypothesisTestForAllModels = FALSE,
          restrictedInterceptInclusion = FALSE,
          restrictedMarginalMeanForAllModels = FALSE,
          restrictedMarginalMeanTerms = list(),
          restrictedModelComparison = "complement",
          restrictedModelComparisonCoefficients = FALSE,
          restrictedModelComparisonCoefficientsHighlight = TRUE,
          restrictedModelComparisonMatrix = FALSE,
          restrictedModelComparisonReference = "Model 1",
          restrictedModelComparisonWeights = TRUE,
          restrictedModelSummaryForAllModels = FALSE,
          restrictedModels = list(list(informedHypothesisTest = FALSE, marginalMean = FALSE, name = "Model 1", summary = FALSE, syntax = "")),
          simpleMainEffectFactor = "",
          simpleMainEffectModeratorFactorOne = "",
          simpleMainEffectModeratorFactorTwo = "",
          sumOfSquares = "type3",
          vovkSellke = FALSE,
          wlsWeights = "") {

   defaultArgCalls <- formals(jaspAnova::Anova)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }

   optionsWithFormula <- c("barPlotHorizontalAxis", "barPlotSeparatePlots", "contrasts", "customContrasts", "dependent", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "fixedFactors", "kruskalWallisFactors", "marginalMeanCiCorrection", "marginalMeanTerms", "modelTerms", "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "randomFactors", "restrictedHeterogeneityCorrection", "restrictedMarginalMeanTerms", "restrictedModelComparison", "restrictedModelComparisonReference", "restrictedModels", "simpleMainEffectFactor", "simpleMainEffectModeratorFactorOne", "simpleMainEffectModeratorFactorTwo", "sumOfSquares", "wlsWeights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspAnova::Anova", data, options, version))
}
