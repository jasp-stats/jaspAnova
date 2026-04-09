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

#' Bayesian ANCOVA
#'
#' The Bayesian ANCOVA allows the user to analyze the difference between multiple group means, while taking into account the effect of variables that have an influence on the dependent variable but are not part of the experimental manipulation (i.e., covariates).
#' ## Assumptions
#' - The residuals are normally distributed for every group.
#' - The independent variables are categorical, the dependent variable is continuous.
#' - The variance of the dependent variable is the same for every group. This is called homogeneity of variances.
#' - The groups are independent.
#' - For each independent variable, the relationship between the dependent variable and the covariate is linear.
#' - The effect of the covariate on the dependent variable does not differ between groups. This is called homogeneity of the regression slopes.
#'
#' @param barPlotErrorBars, By selecting this option, error bars will be displayed in the plot. The error bars can represent either confidence intervals/credible intervals or standard errors.
#'    Defaults to \code{FALSE}.
#' @param barPlotHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param barPlotHorizontalZeroFix, Forces the graphs to show the default x-axis at y = 0.
#'    Defaults to \code{TRUE}.
#' @param barPlotSeparatePlots, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param bayesFactorOrder, Compares each model against the model selected here
#' \itemize{
#'   \item \code{"nullModelTop"}: All models are compared to the null model
#'   \item \code{"bestModelTop"}: All models are compared to the best model
#' }
#' @param criTable, Displays a table with the mean and credible interval of the averaged R², meaning the proportion of variance in the outcome variable explained by the predictors, which are based on the model averaged posterior distribution.
#'    Defaults to \code{FALSE}.
#' @param descriptivePlotCi, Display central credible intervals in the plot. By default this is set to 95%. This can be changed into the desired percentage.
#'    Defaults to \code{TRUE}.
#' @param descriptivePlotHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param descriptivePlotSeparateLines, By placing an independent variable in this box, different lines corresponding to the different levels of the independent variable will be displayed.
#' @param descriptivePlotSeparatePlot,  By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param descriptives, When this option is selected, the mean, standard deviation, and the sample size will be displayed for each level combination of the independent variable. Moreover, it displays the credible interval, which refers to the interval in which the true value of the mean lies based on a certain probability. By default this is set to 95%.
#'    Defaults to \code{FALSE}.
#' @param effects, By selecting this option, the inclusion probability of each component (i.e., model term) in the models will be calculated. The inclusion probability is the probability that a fixed factor is included in the model given the observed data. It also calculates the BF inclusion, which are the odds of obtaining the observed data under models with the predictor vs models without it.
#'    Defaults to \code{FALSE}.
#' @param enforcePrincipleOfMarginalityFixedEffects, Enforces the principle of marginality for fixed effects(User specified)
#'    Defaults to \code{TRUE}.
#' @param enforcePrincipleOfMarginalityRandomSlopes, Enforces the principle of marginality for random slopes (See the 'Legacy results' options)
#'    Defaults to \code{FALSE}.
#' @param hideNuisanceParameters, When checked, the nuisance parameters common to all models are omitted from the model specification.
#'    Defaults to \code{TRUE}.
#' @param integrationMethod, Specify how the marginal likelihoods should be approximated.
#' \itemize{
#'   \item \code{"automatic"}: Numerical integration is used to approximate the marginal likelihood
#'   \item \code{"laplace"}: Laplace approximation of the marginal likelihood. Works better for large sample sizes and very large models.
#' }
#' @param legacyResults, When checked, the random slopes of repeated measures factors are omitted as in JASP <=0.16. Omitting the random slopes may yield completely different results from the frequentist ANOVA.
#'    Defaults to \code{FALSE}.
#' @param modelAveragedPosteriorPlot, By selecting this option, plots illustrating the model averaged posterior distribution of each fixed factor and interaction will be displayed.
#'    Defaults to \code{FALSE}.
#' @param modelPrior, prior distribution of the models
#' \itemize{
#'   \item \code{"uniform"}: Assumes that all possible values are equally likely.
#'   \item \code{"Wilson"}: Default lambda = 1. Equivalent to a Beta binomial with a = 1 and b = lambda * p, where p is the number of predictors in the model.
#'   \item \code{"betaBinomial"}: Default Beta(a = 1, b = 1).
#'   \item \code{"Bernoulli"}: Default p = 0.5.
#'   \item \code{"Castillo"}: Default u = 1. Equivalent to a Beta binomial with a = 1 and b = p^u, where p is the number of predictors in the model.
#'   \item \code{"custom"}: Allows the user to select the prior inclusion probability for the desired variable
#' }
#' @param modelsShown, Gives the option to limit the number of models being displayed.
#' \itemize{
#'   \item \code{"limited"}: Select this to limit the number of displayed models. Set to 10 by default, but this can be changed by the user.
#'   \item \code{"unlimited"}: Select this so that there is no limit.
#' }
#' @param postHocNullControl, When selecting this option, the prior odds will be corrected for multiple testing. This option is selected by default. At the moment, no output will be generated for the post hoc test when this option is not selected.
#'    Defaults to \code{TRUE}.
#' @param posteriorEstimates, By selecting this option, a table with the model averaged posterior summary will be displayed. This table includes information about the model averaged posterior mean, the standard deviation, and the credible interval for each level of the fixed factors and their interactions.
#'    Defaults to \code{FALSE}.
#' @param priorSpecificationMode, Prior: Here it is possible to set the prior distributions for the fixed and random effect sizes.
#' \itemize{
#'   \item \code{"perTerm"}: Allows specifying the prior of each coefficient individually
#'   \item \code{"acrossParameters"}: Allows specifying the same prior for all the coefficients at once
#' }
#' @param qqPlot, Checks the validity of the distributional assumption of the data set. Specifically, the plot shows whether the residuals are normally distributed. Systematic deviations from the straight line indicate that the residuals might not be normally distributed.
#'    Defaults to \code{FALSE}.
#' @param rainCloudHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param rainCloudHorizontalDisplay, Changes the orientation of the raincloud difference plot so that the x-axis represents the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param rainCloudSeparatePlots, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param rsqPlot, By selecting this option, a plot of the posterior distribution of the R2 (i.e., explained variance) will be displayed.
#'    Defaults to \code{FALSE}.
#' @param samplingMethodMCMC, It is possible to set the number of Markov Chain Monte Carlo samples, used to approximate the posterior distribution and error %.
#' \itemize{
#'   \item \code{"auto"}: If this option is selected, 10000 samples will be used. This option is selected by default.
#'   \item \code{"manual"}: If this option is selected, the number of samples can be specified manually. When selecting this option a sample size of 1000 is used by default.
#' }
#' @param singleModelCriTable, Displays a table with the mean and credible interval for the model R², meaning the proportion of variance in the outcome variable explained by the specified predictor.
#'    Defaults to \code{FALSE}.
#' @param singleModelEstimates, A table with the posterior summary for the single model, specified in the assignment box, will be displayed. This table provides information about the single model posterior mean, the standard deviation, and the credible interval for each level of the fixed factors and their interaction included in the model. This is different from the estimate option in Output, since the estimates option provides the posterior summary averaged over all the models included in the analysis, while this option gives the posterior summary for the single specified model only.
#'    Defaults to \code{FALSE}.
#' @param singleModelPosteriorPlot, By selecting this option, plots illustrating the posterior distribution of each fixed factor and interaction included in the single model will be generated.
#'    Defaults to \code{FALSE}.
#' @param singleModelQqPlot, Checks the validity of the distributional assumption of the data set. Specifically, the plot shows whether the residuals are normally distributed. Systematic deviations from the straight line indicate that the residuals might not be normally distributed.
#'    Defaults to \code{FALSE}.
#' @param singleModelRsqPlot, By selecting this option, a plot of the posterior distribution of the R2 (i.e., explained variance) will be displayed.
#'    Defaults to \code{FALSE}.
#' @param singleModelTerms, Select the factors that should be included in the model.
AncovaBayesian <- function(
          data = NULL,
          version = "0.96.1",
          formula = NULL,
          isNuisance = NULL,
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
          castilloParameterU = 1,
          cauchyPriorScaleCovariates = 0.354,
          cauchyPriorScaleFixedEffects = 0.5,
          cauchyPriorScaleRandomEffects = 1,
          covariates = list(types = list(), value = list()),
          credibleInterval = 0.95,
          criTable = FALSE,
          customPriorSpecification = list(optionKey = "components", types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          descriptivePlotCi = TRUE,
          descriptivePlotCiLevel = 0.95,
          descriptivePlotHorizontalAxis = list(types = list(), value = ""),
          descriptivePlotSeparateLines = list(types = list(), value = ""),
          descriptivePlotSeparatePlot = list(types = list(), value = ""),
          descriptives = FALSE,
          effects = FALSE,
          effectsType = "allModels",
          enforcePrincipleOfMarginalityFixedEffects = TRUE,
          enforcePrincipleOfMarginalityRandomSlopes = FALSE,
          fixedFactors = list(types = list(), value = list()),
          groupPosterior = "grouped",
          hideNuisanceParameters = TRUE,
          integrationMethod = "automatic",
          legacyResults = FALSE,
          modelAveragedPosteriorPlot = FALSE,
          modelPrior = "uniform",
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          modelsShown = "limited",
          numModelsShown = 10,
          plotHeight = 320,
          plotWidth = 480,
          postHocNullControl = TRUE,
          postHocTerms = list(types = list(), value = list()),
          posteriorEstimates = FALSE,
          priorSpecificationMode = "acrossParameters",
          qqPlot = FALSE,
          rainCloudHorizontalAxis = list(types = list(), value = ""),
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = list(types = list(), value = ""),
          rainCloudYAxisLabel = "",
          randomFactors = list(types = list(), value = list()),
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
          singleModelTerms = list(optionKey = "components", types = list(), value = list()),
          wilsonParameterLambda = 1) {

   defaultArgCalls <- formals(jaspAnova::AncovaBayesian)
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
   optionsWithFormula <- c("isNuisance", "barPlotHorizontalAxis", "barPlotSeparatePlots", "covariates", "customPriorSpecification", "dependent", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "fixedFactors", "modelTerms", "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "randomFactors", "singleModelTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspAnova", "AncovaBayesian", "AncovaBayesian.qml", options, version, FALSE))
}