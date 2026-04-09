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

#' ANCOVA
#'
#' ANCOVA allows the user to analyze the difference between multiple group means, while taking into account the effect of variables that have an influence on the dependent variable but are not part of the experimental manipulation (i.e., covariates).
#' ## Assumptions
#' - The residuals are normally distributed for every group.
#' - The independent variables are categorical, the dependent variable is continuous.
#' - The variance of the dependent variable is the same for every group. This is called homogeneity of variances.
#' - The groups are independent.
#' - The covariate and the experiment effect are independent.
#' - The effect of the covariate on the dependent variable does not differ between groups. This is called homogeneity of the regression slopes.
#'
#' @param barPlotErrorBars, By selecting this option, error bars will be displayed in the plot. The error bars can represent either confidence intervals/credible intervals or standard errors.
#'    Defaults to \code{FALSE}.
#' @param barPlotHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param barPlotHorizontalZeroFix, Forces the graphs to show the default x-axis at y = 0.
#'    Defaults to \code{TRUE}.
#' @param barPlotSeparatePlots, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param contrastCi, By selecting this option, confidence intervals for the estimated mean difference and effect size will be included. By default the confidence level is set to 95%. This can be changed into the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param contrastEffectSize, Include standardized mean differences, based on the effectsize function in the emmeans package.
#'    Defaults to \code{FALSE}.
#' @param covariates, In this box the variable that is the covariate can be selected. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.
#' @param dependent, The variable of interest. This is also called the outcome variable.
#' @param descriptivePlotErrorBar, Display error bars in the plot. The error bars can represent either confidence intervals or standard errors.
#'    Defaults to \code{TRUE}.
#' @param descriptivePlotHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param descriptivePlotSeparateLines, By placing an independent variable in this box, different lines corresponding to the different levels of the independent variable will be displayed.
#' @param descriptivePlotSeparatePlot, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param descriptives, When this option is selected, the mean, standard deviation, and the sample size will be displayed for each level combination of the independent variables.
#'    Defaults to \code{FALSE}.
#' @param effectSizeCi, Displays confidence intervals for the effect size. Set at 95% by default but can be changed into the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param effectSizeEstimates, By selecting this option, the specific types of calculations to estimate the effect size can be specified.
#'    Defaults to \code{FALSE}.
#' @param effectSizeEtaSquared, Eta-squared is calculated as an estimate of the effect size. However, this method is considered to overestimate the population variance, making it hard to compare the effect of the same variable across different studies (Goss-Sampson, 2018; Kroes & Finley, 2023).
#'    Defaults to \code{FALSE}.
#' @param effectSizeOmegaSquared, Omega squared is calculated as an estimate of the effect size. This is considered a less biased estimate of the effect size, compared to η2 . (Kroes & Finley, 2023).
#'    Defaults to \code{TRUE}.
#' @param effectSizePartialEtaSquared, Partial eta-squared is calculated as an estimate of the effect size. Partial η2 measures the effect size of the predictor in the context of multiple factors or covariates, isolating its unique contribution.
#'    Defaults to \code{FALSE}.
#' @param effectSizePartialOmegaSquared, Partial Omega squared is calculated as an estimate of the effect size. Partial ω2 measures the effect size of the predictor in the context of multiple factors or covariates, isolating its unique contribution.
#'    Defaults to \code{FALSE}.
#' @param fixedFactors,  The variables that are manipulated/define the different groups. These are also called the independent variables.
#' @param homogeneityCorrectionBrown,  If the homogeneity assumption is not met, this correction could be used. This correction is only available for one-way ANOVA.
#'    Defaults to \code{FALSE}.
#' @param homogeneityCorrectionNone, No homogeneity correction.
#'    Defaults to \code{TRUE}.
#' @param homogeneityCorrectionWelch, If the homogeneity assumption is not met, this correction could be used. This correction is only available for one-way ANOVA.
#'    Defaults to \code{FALSE}.
#' @param homogeneityTests, By selecting this option, it will be checked whether the variance of the dependent variable is equal between the groups by performing Levene's test of equal variances.
#'    Defaults to \code{FALSE}.
#' @param kruskalEffectSizeEstimates, Request effect size estimates for the Kruskal-Wallis test: rank epsilon squared and rank eta squared, including their confidence interval. Based on the effectsize package.
#'    Defaults to \code{FALSE}.
#' @param marginalMeanBootstrap, When this option is selected, the bootstrapped marginal means are calculated. By default, the number of replications is set to 1000. This can be changed into the desired number.
#'    Defaults to \code{FALSE}.
#' @param marginalMeanComparedToZero, By selecting this option, the adjusted means are compared to 0 and the confidence intervals of the adjusted means are calculated.
#'    Defaults to \code{FALSE}.
#' @param modelTerms, The independent variables and covariates included in the model. By default, all the main effects and interaction effects of factor variables, and the main effects of covariates are included in the model.
#' @param postHocCi, When this option is selected, the confidence interval for the mean difference is calculated for every post hoc method except Dunn. By default, this is set to 95%, but it can be adjusted to the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param postHocConditionalTable, Instead of pairwise comparisons for all possible combination of cells in the interaction, list pairwise comparisons conditional on each of the interaction terms. This provides as many tables as there are terms in the interaction effect.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionBonferroni, This correction is considered conservative. The risk of Type I error is reduced, however the statistical power decreases as well.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionHolm, This method is also called sequential Bonferroni, and considered less conservative than the Bonferroni method.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionScheffe, Adjusting significance levels in a linear regression, to account for multiple comparisons. This method is considered to be quite conservative.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionSidak, This method is considered less conservative than Bonferroni but still maintains statistical power. It is usually used if there is not a big amount of tests to be performed.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionTukey, Compare all possible pairs of group means. This correction can be used when the groups of the independent variable have an equal sample size and variance. This method is commonly used and is selected by default.
#'    Defaults to \code{TRUE}.
#' @param postHocLetterTable, Set up a compact letter display of all pairwise comparisons, based on 'multcomp::cld' and emmeans.
#'    Defaults to \code{FALSE}.
#' @param postHocSignificanceFlag, Add asterisks to the table to indicate 3 levels of significance.
#'    Defaults to \code{FALSE}.
#' @param postHocTypeDunn, This non-parametric follow-up test is used for pairwise comparisons after performing the Kruskal-Wallis test. The p-values are corrected using the Bonferroni and Holm methods.
#'    Defaults to \code{FALSE}.
#' @param postHocTypeDunnet, With this method, all levels are compared to the first occurring level in the data. To change which level is used as the reference group, adjust the order of the level labels in the Variable Settings.
#'    Defaults to \code{FALSE}.
#' @param postHocTypeGames, This method can be used when equal group variances cannot be assumed. The p-values are adjusted using the Tukey method.
#'    Defaults to \code{FALSE}.
#' @param postHocTypeStandard, Pairwise t-tests are performed. All the corrections can be applied to this method. When Tukey's p-value correction is selected, this is equivalent to Tukey's HSD. This option is selected by default.
#'    Defaults to \code{TRUE}.
#' @param postHocTypeStandardBootstrap, By selecting this option, the bootstrapped post hoc test is applied. By default, the number of replications is set to 1000. This can be changed into the desired number.
#'    Defaults to \code{FALSE}.
#' @param postHocTypeStandardEffectSize, By selecting this option, the effect size (i.e., the magnitude of the observed effect) will be displayed. The used measure for the effect size is Cohen's d. The effect size will only be displayed for the post hoc type Standard.
#'    Defaults to \code{FALSE}.
#' @param predictionsSavedToData, Save the predictions of the most complex model as a new column in the data file.
#'    Defaults to \code{FALSE}.
#' @param qqPlot, Displays Q-Q plot of the standardized residuals. The confidence band shows the expected range of residuals under normality; points outside the band suggest deviations from normality.
#'    Defaults to \code{FALSE}.
#' @param rainCloudHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param rainCloudHorizontalDisplay, Changes the orientation of the raincloud difference plot so that the x-axis represents the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param rainCloudSeparatePlots, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param residualsSavedToData, Save the residuals of the most complex model as a new column in the data file.
#'    Defaults to \code{FALSE}.
#' @param simpleMainEffectFactor, Select the independent variable to determine the effect of this variable, conditional on the levels of the moderator factor(s).
#' @param simpleMainEffectModeratorFactorOne, Select the independent variable that will represent the different levels.
#' @param simpleMainEffectModeratorFactorTwo, Select an optional, additional independent variable.
#' @param sumOfSquares,  There are different types of the sum of squares. The choice of the type is important when there are multiple factors and when the data are unbalanced. In an unbalanced design, the different levels of the independent variable do not contain an equal number of observations (e.g., one group contains more observations than another group). In this scenario, the sum of squares type can influence the results.
#' \itemize{
#'   \item \code{"type1"}: Sequential sum of squares. It is the reduction of error when each factor of the model is added to the factors already included, preserving the order of factors in the model. The results depend on the order in which the factors are added to the model. This is important to consider when the model contains more than one factor.
#'   \item \code{"type2"}: Hierarchical/partially sequential sum of squares. It is the reduction of error when each factor is added to the model that includes all the other factors, except the factors where the added factor is a part of, such as interactions containing that factor. Langsrud (2003) advises to apply this type for an ANOVA with unbalanced data.
#'   \item \code{"type3"} (default) : Partial sum of squares. It is the reduction of error when each factor is added to the model that includes all the other factors, including interactions with this factor. This type is often selected, because it takes interactions into account (Langsrud, 2003). This type is selected by default.
#' }
#' @param vovkSellke, Shows the maximum ratio of the likelihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0
#'    Defaults to \code{FALSE}.
#' @param wlsWeights, Weighted Least Squares, here the variable specifying which points have more weight and are therefore considered more informative can be selected. For this last option it is important to know the weights a priori. This option is primarily used when the errors are heteroskedastic, meaning they are not equally distributed across levels of the independent variable.
Ancova <- function(
          data = NULL,
          version = "0.96.1",
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
          covariates = list(types = list(), value = list()),
          customContrasts = list(),
          dependent = list(types = list(), value = ""),
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = TRUE,
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
          factorCovariateIndependenceCheck = FALSE,
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
          qqPlotCi = FALSE,
          qqPlotCiLevel = 0.95,
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

   defaultArgCalls <- formals(jaspAnova::Ancova)
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
   optionsWithFormula <- c("barPlotHorizontalAxis", "barPlotSeparatePlots", "contrasts", "covariates", "customContrasts", "dependent", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "fixedFactors", "kruskalWallisFactors", "marginalMeanCiCorrection", "marginalMeanTerms", "modelTerms", "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "randomFactors", "restrictedHeterogeneityCorrection", "restrictedMarginalMeanTerms", "restrictedModelComparison", "restrictedModelComparisonReference", "restrictedModels", "simpleMainEffectFactor", "simpleMainEffectModeratorFactorOne", "simpleMainEffectModeratorFactorTwo", "sumOfSquares", "wlsWeights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspAnova", "Ancova", "Ancova.qml", options, version, FALSE))
}