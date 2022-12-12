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

AnovaRepeatedMeasures <- function(
          data = NULL,
          version = "0.17",
          betweenModelTerms = list(),
          betweenSubjectFactors = list(),
          conoverTest = FALSE,
          contrastCi = FALSE,
          contrastCiLevel = 0.95,
          contrastEqualVariance = TRUE,
          contrasts = list(list(contrast = "none", variable = "RM Factor 1")),
          covariates = list(),
          customContrasts = list(),
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = FALSE,
          descriptivePlotErrorBarPooled = FALSE,
          descriptivePlotErrorBarType = "ci",
          descriptivePlotHorizontalAxis = "",
          descriptivePlotSeparateLines = "",
          descriptivePlotSeparatePlot = "",
          descriptivePlotYAxisLabel = "",
          descriptives = FALSE,
          effectSizeEstimates = FALSE,
          effectSizeEtaSquared = TRUE,
          effectSizeGeneralEtaSquared = FALSE,
          effectSizeOmegaSquared = FALSE,
          effectSizePartialEtaSquared = FALSE,
          friedmanBetweenFactor = "",
          friedmanWithinFactor = list(),
          homogeneityTests = FALSE,
          marginalMeanBootstrap = FALSE,
          marginalMeanBootstrapSamples = 1000,
          marginalMeanCiCorrection = "none",
          marginalMeanComparedToZero = FALSE,
          marginalMeanTerms = list(),
          multivariateModelFollowup = FALSE,
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
          postHocCorrectionBonferroni = FALSE,
          postHocCorrectionHolm = TRUE,
          postHocCorrectionScheffe = FALSE,
          postHocCorrectionTukey = FALSE,
          postHocEffectSize = FALSE,
          postHocPooledError = TRUE,
          postHocSignificanceFlag = FALSE,
          postHocTerms = list(),
          rainCloudHorizontalAxis = "",
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = "",
          rainCloudYAxisLabel = "",
          repeatedMeasuresCells = list(),
          repeatedMeasuresFactors = list(list(levels = list("Level 1", "Level 2"), name = "RM Factor 1")),
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
          restrictedModelComparisonReference = "complement",
          restrictedModelComparisonWeights = TRUE,
          restrictedModelSummaryForAllModels = FALSE,
          restrictedModels = list(list(informedHypothesisTest = FALSE, marginalMean = FALSE, name = "Model 1", summary = FALSE, syntax = "")),
          simpleMainEffectErrorTermPooled = FALSE,
          simpleMainEffectFactor = "",
          simpleMainEffectModeratorFactorOne = "",
          simpleMainEffectModeratorFactorTwo = "",
          sphericityCorrectionGreenhouseGeisser = FALSE,
          sphericityCorrectionHuynhFeldt = FALSE,
          sphericityCorrectionNone = TRUE,
          sphericityTests = FALSE,
          sumOfSquares = "type3",
          vovkSellke = FALSE,
          withinModelTerms = list()) {

   defaultArgCalls <- formals(jaspAnova::AnovaRepeatedMeasures)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("betweenModelTerms", "betweenSubjectFactors", "contrasts", "covariates", "customContrasts", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "friedmanBetweenFactor", "friedmanWithinFactor", "marginalMeanCiCorrection", "marginalMeanTerms", "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "repeatedMeasuresCells", "repeatedMeasuresFactors", "restrictedHeterogeneityCorrection", "restrictedMarginalMeanTerms", "restrictedModelComparison", "restrictedModelComparisonReference", "restrictedModels", "simpleMainEffectFactor", "simpleMainEffectModeratorFactorOne", "simpleMainEffectModeratorFactorTwo", "sumOfSquares", "withinModelTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspAnova::AnovaRepeatedMeasures", data, options, version))
}
