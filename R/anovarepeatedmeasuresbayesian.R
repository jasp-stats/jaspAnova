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

AnovaRepeatedMeasuresBayesianWrapper <- function(
          data = NULL,
          bayesFactorOrder = "bestModelTop",
          bayesFactorType = "BF10",
          bernoulliParameter = 0.5,
          betaBinomialParameterA = 1,
          betaBinomialParameterB = 1,
          betweenSubjectFactors = list(),
          castilloParameterU = 1,
          cauchyPriorScaleCovariates = 0.354,
          cauchyPriorScaleFixedEffects = 0.5,
          cauchyPriorScaleRandomEffects = 1,
          covariates = list(),
          credibleInterval = 0.95,
          criTable = FALSE,
          customPriorSpecification = list(list(components = "RM Factor 1", inclusionProbability = 0.5, scaleFixedEffects = 0.5)),
          descriptivePlotCi = FALSE,
          descriptivePlotCiLevel = 0.95,
          descriptivePlotHorizontalAxis = "",
          descriptivePlotSeparateLines = "",
          descriptivePlotSeparatePlot = "",
          descriptivePlotYAxisLabel = "",
          descriptives = FALSE,
          effects = FALSE,
          effectsType = "allModels",
          enforcePrincipleOfMarginalityFixedEffects = TRUE,
          enforcePrincipleOfMarginalityRandomSlopes = FALSE,
          groupPosterior = "grouped",
          hideNuisanceParameters = TRUE,
          integrationMethod = "automatic",
          legacyResults = FALSE,
          modelAveragedPosteriorPlot = FALSE,
          modelPrior = "uniform",
          modelTerms = list(),
          modelsShown = "limited",
          numModelsShown = 10,
          plotHeight = 320,
          plotWidth = 480,
          postHocNullControl = TRUE,
          postHocTerms = list(),
          posteriorEstimates = FALSE,
          priorSpecificationMode = "acrossParameters",
          qqPlot = FALSE,
          rainCloudHorizontalAxis = "",
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = "",
          rainCloudYAxisLabel = "",
          repeatedMeasuresCells = list(),
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
          singleModelTerms = list(),
          wilsonParameterLambda = 1) {

   defaultArgCalls <- formals(jaspAnova::AnovaRepeatedMeasuresBayesianWrapper)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL

   optionsWithFormula <- c("singleModelTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = deparse1(options[[name]])   }

   if (jaspResultsCalledFromJasp()) {
      result <- list("options" = options, "analysis"="jaspAnova::AnovaRepeatedMeasuresBayesian")
      result <- jsonlite::toJSON(result, auto_unbox = TRUE, digits = NA, null="null")
      toString(result)
   } else {
      options <- checkAnalysisOptions("jaspAnova::AnovaRepeatedMeasuresBayesian", options)
      jaspTools::runAnalysis("jaspAnova::AnovaRepeatedMeasuresBayesian", data, options)
   }

}

AnovaRepeatedMeasuresBayesian <- function(jaspResults, dataset, options, ...) {

  .BANOVArunAnalysis(jaspResults, dataset, options, "RM-ANOVA")

}
