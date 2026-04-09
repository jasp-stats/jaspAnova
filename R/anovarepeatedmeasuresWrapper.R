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

#' Repeated Measures ANOVA
#'
#' The repeated Measures ANOVA allows the user to analyze the differences between means, when observations are dependent i.e Within subject factors. It also allows combining with between subject factors.
#' ## Assumptions
#' - The dependent variable is normally distributed for every group.
#' - The covariate and the experiment effect are independent.
#' - The assumption of sphericity is met. Sphericity entails that the variances of the differences between all possible pairs of the repeated measures conditions are the same.
#'
#' @param barPlotErrorBars, By selecting this option, error bars will be displayed in the plot. The error bars can represent either confidence intervals/credible intervals or standard errors.
#'    Defaults to \code{FALSE}.
#' @param barPlotHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param barPlotHorizontalZeroFix, Forces the graphs to show the default x-axis at y = 0.
#'    Defaults to \code{TRUE}.
#' @param barPlotSeparatePlots, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param betweenModelTerms, The between measures factors and covariates that can be included in the model. Can also include interaction effects.
#' @param betweenSubjectFactors, When the subjects have been assigned into two or more separate groups this variable can be selected.
#' @param conoverTest, Conover's post-hoc test for pairwise comparisons, if the non-parametric test indicates significance.
#'    Defaults to \code{FALSE}.
#' @param contrastCi, By selecting this option, confidence intervals for the estimated mean difference and effect size will be included. By default the confidence level is set to 95%. This can be changed into the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param contrastEffectSize, Include standardized mean differences, based on the effectsize function in the emmeans package.
#'    Defaults to \code{FALSE}.
#' @param covariates, In this box the variables that are covariates can be selected. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.
#' @param descriptivePlotErrorBar, Display error bars in the plot. The error bars can represent either confidence intervals or standard errors.
#'    Defaults to \code{TRUE}.
#' @param descriptivePlotErrorBarPooled, When there are multiple RM factors in the model, but only plotting a subset of these factors, the mean is taken across the unused RM factors. For instance, when there are two RM factors with two levels in the model, A (1&2) and B (1&2), and only A is selected to be plotted, the average is taken of B across its levels. This means that when the mean of A1 is plotted, it is actually the average of A1B1 and A1B2). This procedure is discussed by Loftus & Masson (1994). When the box is not ticked, the averages are not taken, and the columns A1B1 and A1B2 are simply concatenated.
#'    Defaults to \code{FALSE}.
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
#' @param friedmanBetweenFactor, Possible to select the between subjects factor here.
#' @param friedmanWithinFactor, The repeated measures factor(s) of interest.
#' @param homogeneityTests, By selecting this option, it will be checked whether the variance of the dependent variable is equal between the groups by performing Levene's test of equal variances. This test is only suitable for assessing equal variance for between subjects factors. For repeated measures, the sphericity test is more suitable.
#'    Defaults to \code{FALSE}.
#' @param marginalMeanBootstrap, When this option is selected, the bootstrapped marginal means are calculated. By default, the number of replications is set to 1000. This can be changed into the desired number.
#'    Defaults to \code{FALSE}.
#' @param marginalMeanComparedToZero, By selecting this option, the adjusted means are compared to 0 and the confidence intervals of the adjusted means are calculated.
#'    Defaults to \code{FALSE}.
#' @param normalizeErrorBarsBarplot, In order to get accurate confidence intervals and standard errors for within subjects effects, the data are normalized by subtracting the appropriate participantʹs mean performance from each observation, and then adding the grand mean score to every observation. The variances of the resulting normalized values in each condition, and thus the size of the bars, no longer depend on the participant effects and are therefore a more accurate representation of the experimental manipulation.
#'    Defaults to \code{TRUE}.
#' @param poolErrorTermFollowup,  By selecting this option, the univariate linear model, rather than the multivariate model, will be used for follow-up tests (contrasts, post-hoc tests, marginal means). Caution: multivariate models (i.e., unpooled error terms) handle departures from sphericity better, since these models allow the standard errors to differ for each level of the repeated measure(s) factor(s).
#'    Defaults to \code{FALSE}.
#' @param postHocCi, When this option is selected, the confidence interval for the mean difference is calculated for every post hoc method except Dunn. By default, this is set to 95%, but it can be adjusted to the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param postHocConditionalTable, Instead of pairwise comparisons for all possible combination of cells in the interaction, list pairwise comparisons conditional on each of the interaction terms. This provides as many tables as there are terms in the interaction effect.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionBonferroni, This correction is considered conservative. The risk of Type I error is reduced, however the statistical power decreases as well.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionHolm, This method is also called sequential Bonferroni, and considered less conservative than the Bonferroni method. Selected by default.
#'    Defaults to \code{TRUE}.
#' @param postHocCorrectionScheffe, Adjusting significance levels in a linear regression, to account for multiple comparisons. This method is considered to be quite conservative.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionTukey,  Compare all possible pairs of group means. This correction can be used when the groups of the repeated measures have an equal sample size and variance.
#'    Defaults to \code{FALSE}.
#' @param postHocEffectSize, By selecting this option, the effect size (i.e., the magnitude of the observed effect) will be displayed. The used measure for the effect size is Cohen's d. The effect size will only be displayed for the post hoc type Standard.
#'    Defaults to \code{FALSE}.
#' @param postHocLetterTable, Set up a compact letter display of all pairwise comparisons, based on 'multcomp::cld' and emmeans.
#'    Defaults to \code{FALSE}.
#' @param postHocSignificanceFlag, Add asterisks to the table to indicate 3 levels of significance.
#'    Defaults to \code{FALSE}.
#' @param qqPlot, Displays Q-Q plot of the standardized residuals. The confidence band shows the expected range of residuals under normality; points outside the band suggest deviations from normality.
#'    Defaults to \code{FALSE}.
#' @param rainCloudHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param rainCloudHorizontalDisplay, Changes the orientation of the raincloud difference plot so that the x-axis represents the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param rainCloudSeparatePlots, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param repeatedMeasuresCells, The separate columns in the data frame that represent the levels of the repeated measure(s) factor(s). These are made based on the input on the repeated measures factors box.
#' @param repeatedMeasuresFactors, The within-subjects (repeated measures) variables. Here the repeated measures factors of interest and the different levels that belong to each factor can be labelled.
#' @param simpleMainEffectErrorTermPooled, A pooled error term assumes that the variances of the contrast scores are approximately equal (i.e., sphericity assumption).
#'    Defaults to \code{FALSE}.
#' @param simpleMainEffectFactor, Select the independent variable to determine the effect of this variable, conditional on the levels of the moderator factor(s).
#' @param simpleMainEffectModeratorFactorOne, Select the independent variable that will represent the different levels.
#' @param simpleMainEffectModeratorFactorTwo, Select an optional, additional independent variable.
#' @param sphericityCorrectionGreenhouseGeisser, This correction varies between 1/(k − 1), where k is the number of repeated-measures conditions.
#'    Defaults to \code{FALSE}.
#' @param sphericityCorrectionHuynhFeldt, Another common method to correct the degrees of freedom is Huynh-Feldt correction.
#'    Defaults to \code{FALSE}.
#' @param sphericityCorrectionNone, No correction is performed.
#'    Defaults to \code{TRUE}.
#' @param sphericityTests, Sphericity entails that the variances of the differences of the repeated measures conditions are equal.
#'    Defaults to \code{FALSE}.
#' @param sumOfSquares,  There are different types of the sum of squares. The choice of the type is important when there are multiple factors and when the data are unbalanced. In an unbalanced design, the different levels of the independent variable do not contain an equal number of observations (e.g., one group contains more observations than another group). In this scenario, the sum of squares type can influence the results.
#' \itemize{
#'   \item \code{"type1"}: Sequential sum of squares. It is the reduction of error when each factor of the model is added to the factors already included, preserving the order of factors in the model. The results depend on the order in which the factors are added to the model. This is important to consider when the model contains more than one factor.
#'   \item \code{"type2"}: Hierarchical/partially sequential sum of squares. It is the reduction of error when each factor is added to the model that includes all the other factors, except the factors where the added factor is a part of, such as interactions containing that factor. Langsrud (2003) advises to apply this type for an ANOVA with unbalanced data.
#'   \item \code{"type3"} (default) : Partial sum of squares. It is the reduction of error when each factor is added to the model that includes all the other factors, including interactions with this factor. This type is often selected, because it takes interactions into account (Langsrud, 2003). This type is selected by default.
#' }
#' @param usePooledStandErrorCITwo, When there are multiple RM factors in the model, but only plotting a subset of these factors, the mean is taken across the unused RM factors. For instance, when there are two RM factors with two levels in the model, A (1&2) and B (1&2), and only A is selected to be plotted, the average is taken of B across its levels. This means that when the mean of A1 is plotted, it is actually the average of A1B1 and A1B2). This procedure is discussed by Loftus & Masson (1994). When the box is not ticked, the averages are not taken, and the columns A1B1 and A1B2 are simply concatenated.
#'    Defaults to \code{FALSE}.
#' @param vovkSellke, Shows the maximum ratio of the likelihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0
#'    Defaults to \code{FALSE}.
#' @param withinModelTerms, The repeated measures factors and covariates that can be included in the model. Can also include interaction effects.
AnovaRepeatedMeasures <- function(
          data = NULL,
          version = "0.96.1",
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
          descriptivePlotErrorBar = TRUE,
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
          qqPlotCi = FALSE,
          qqPlotCiLevel = 0.95,
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