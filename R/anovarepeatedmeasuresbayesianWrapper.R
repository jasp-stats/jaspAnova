#
# Copyright (C) 2013-2024 University of Amsterdam
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

AnovaRepeatedMeasuresBayesian <- function(
          data = NULL,
          version = "0.19.2",
          barPlotCiInterval = 0.95,
          barPlotErrorBarType = "ci",
          barPlotErrorBars = FALSE,
          barPlotHorizontalAxis = list(types = list(), value = ""),
          barPlotHorizontalZeroFix = TRUE,
          barPlotSeparatePlots = list(types = list(), value = ""),
          bayesFactorOrder = "bestModelTop",
          bayesFactorType = "BF10",
          bernoulliParameter = 0.5,
          betaBinomialParameterA = 1,
          betaBinomialParameterB = 1,
          betweenSubjectFactors = list(types = list(), value = NULL),
          castilloParameterU = 1,
          cauchyPriorScaleCovariates = 0.354,
          cauchyPriorScaleFixedEffects = 0.5,
          cauchyPriorScaleRandomEffects = 1,
          covariates = list(types = list(), value = NULL),
          credibleInterval = 0.95,
          criTable = FALSE,
          customPriorSpecification = list(optionKey = "components", types = "unknown", value = list(list(components = "RM Factor 1", inclusionProbability = 0.5, scaleFixedEffects = 0.5))),
          descriptivePlotCi = FALSE,
          descriptivePlotCiLevel = 0.95,
          descriptivePlotHorizontalAxis = list(types = list(), value = ""),
          descriptivePlotSeparateLines = list(types = list(), value = ""),
          descriptivePlotSeparatePlot = list(types = list(), value = ""),
          descriptivePlotYAxisLabel = "",
          descriptives = FALSE,
          effects = FALSE,
          effectsType = "allModels",
          enforcePrincipleOfMarginalityFixedEffects = TRUE,
          enforcePrincipleOfMarginalityRandomSlopes = FALSE,
          groupPosterior = "grouped",
          hideNuisanceParameters = TRUE,
          integrationMethod = "automatic",
          labelYAxisTwo = "",
          legacyResults = FALSE,
          modelAveragedPosteriorPlot = FALSE,
          modelPrior = "uniform",
          modelTerms = list(optionKey = "components", types = "unknown", value = list(list(components = "RM Factor 1", isNuisance = FALSE))),
          modelsShown = "limited",
          numModelsShown = 10,
          plotHeight = 320,
          plotWidth = 480,
          postHocNullControl = TRUE,
          postHocTerms = list(types = list(), value = NULL),
          posteriorEstimates = FALSE,
          priorSpecificationMode = "acrossParameters",
          qqPlot = FALSE,
          rainCloudHorizontalAxis = list(types = list(), value = ""),
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = list(types = list(), value = ""),
          rainCloudYAxisLabel = "",
          repeatedMeasuresCells = list("", ""),
          repeatedMeasuresFactors = list(list(levels = list("Level 1", "Level 2"), name = "RM Factor 1")),
          rsqPlot = FALSE,
          samplesMCMC = 1000,
          samplesNumericAccuracy = 10000,
          samplingMethodMCMC = "auto",
          samplingMethodNumericAccuracy = "auto",
          seed = 1,
          setSeed = FALSE,
          singleModelCriTable = FALSE,
          singleModelEstimates = FALSE,
          singleModelGroupPosterior = "grouped",
          singleModelPosteriorPlot = FALSE,
          singleModelQqPlot = FALSE,
          singleModelRsqPlot = FALSE,
          singleModelTerms = list(optionKey = "components", types = "unknown", value = list(list(components = "RM Factor 1"))),
          wilsonParameterLambda = 1) {

   defaultArgCalls <- formals(jaspAnova::AnovaRepeatedMeasuresBayesian)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("barPlotHorizontalAxis", "barPlotSeparatePlots", "betweenSubjectFactors", "covariates", "customPriorSpecification", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "modelTerms", "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "repeatedMeasuresCells", "repeatedMeasuresFactors", "singleModelTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspAnova::AnovaRepeatedMeasuresBayesian", data, options, version))
}
