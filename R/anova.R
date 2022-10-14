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

AnovaWrapper <- function(
          data = NULL,
          formula = NULL,
          contrastCi = FALSE,
          contrastCiLevel = 0.95,
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
          effectSizeEstimates = FALSE,
          effectSizeEtaSquared = TRUE,
          effectSizeOmegaSquared = FALSE,
          effectSizePartialEtaSquared = FALSE,
          fixedFactors = list(),
          homogeneityCorrectionBrown = FALSE,
          homogeneityCorrectionNone = TRUE,
          homogeneityCorrectionWelch = FALSE,
          homogeneityTests = FALSE,
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
          postHocCorrectionBonferroni = FALSE,
          postHocCorrectionHolm = FALSE,
          postHocCorrectionScheffe = FALSE,
          postHocCorrectionSidak = FALSE,
          postHocCorrectionTukey = TRUE,
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
          restrictedModelComparisonReference = "complement",
          restrictedModelComparisonWeights = TRUE,
          restrictedModelSummaryForAllModels = FALSE,
          restrictedModels = list(list(informedHypothesisTest = FALSE, marginalMean = FALSE, name = "Model 1", summary = FALSE, syntax = "")),
          simpleMainEffectFactor = "",
          simpleMainEffectModeratorFactorOne = "",
          simpleMainEffectModeratorFactorTwo = "",
          sumOfSquares = "type3",
          vovkSellke = FALSE,
          wlsWeights = "") {

   defaultArgCalls <- formals(jaspAnova::AnovaWrapper)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- deparse1(formula)
   }

   optionsWithFormula <- c("marginalMeanTerms", "modelTerms", "postHocTerms", "restrictedMarginalMeanTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = deparse1(options[[name]])   }

   if (jaspResultsCalledFromJasp()) {
      result <- list("options" = options, "analysis"="jaspAnova::Anova")
      result <- jsonlite::toJSON(result, auto_unbox = TRUE, digits = NA, null="null")
      toString(result)
   } else {
      options <- checkAnalysisOptions("jaspAnova::Anova", options)
      jaspTools::runAnalysis("jaspAnova::Anova", data, options)
   }

}

Anova <- function(jaspResults, dataset = NULL, options) {

    options$covariates <- NULL

    Ancova(jaspResults, dataset = NULL, options)

    return()
}
