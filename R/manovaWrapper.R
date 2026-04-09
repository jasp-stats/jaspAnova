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

#' MANOVA
#'
#' MANOVA allows the user to analyze the difference among groups when there are multiple dependent variables
#' ## Assumptions
#' - The dependent variables are normally distributed for every group.
#' - The independent variables are categorical, the dependent variables are continuous.
#' - The population covariance matrices of each group are equal.
#' - The groups are independent.
#'
#' @param anovaTables, Outputs individual ANOVA tables per dependent variable.
#'    Defaults to \code{FALSE}.
#' @param boxMTest,  Box's M-test for homogeneity of covariance matrices.
#'    Defaults to \code{FALSE}.
#' @param dependent, The variable of interest. Also called the outcome variable.
#' @param fixedFactors, The variables that are manipulated/define the different groups. These are also called the independent variables.
#' @param includeIntercept, display the intercept term in the MANOVA and ANOVA tables.
#'    Defaults to \code{TRUE}.
#' @param modelTerms, The independent variables included in the model. By default all the main effects and interaction effects of the specified independent variables are included in the model.
#' @param shapiroTest, Generalized Shapiro-Wilk test for multivariate normality.
#'    Defaults to \code{FALSE}.
#' @param testHotellingLawley, Hotelling-Lawley's trace. Measures multivariate separation between group means relative to within group variance. Meaning how well can two groups be distinguished taking all the dependent variables into account.
#'    Defaults to \code{FALSE}.
#' @param testPillai, Pillai's trace. Produces a value between 0 and 1. The closer to 1, the more evidence there is of an effect of the independent variable on the dependent variable.
#'    Defaults to \code{TRUE}.
#' @param testRoy, Roy's largest root. Measures the largest separation between group means along the most discriminating direction in the multuvariate space.
#'    Defaults to \code{FALSE}.
#' @param testWilks, Wilks' lambda. This can be interpreted as the proportion of the variance in the outcomes that is not explained by an effect.
#'    Defaults to \code{FALSE}.
#' @param vovkSellke, Shows the maximum ratio of the likelihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0
#'    Defaults to \code{FALSE}.
Manova <- function(
          data = NULL,
          version = "0.96.1",
          formula = NULL,
          anovaTables = FALSE,
          boxMTest = FALSE,
          dependent = list(types = list(), value = list()),
          fixedFactors = list(types = list(), value = list()),
          includeIntercept = TRUE,
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          plotHeight = 320,
          plotHeightDescriptivesPlotLegend = 300,
          plotHeightDescriptivesPlotNoLegend = 300,
          plotHeightQQPlot = 300,
          plotWidth = 480,
          plotWidthDescriptivesPlotLegend = 430,
          plotWidthDescriptivesPlotNoLegend = 350,
          plotWidthQQPlot = 300,
          randomFactors = list(types = list(), value = list()),
          shapiroTest = FALSE,
          testHotellingLawley = FALSE,
          testPillai = TRUE,
          testRoy = FALSE,
          testWilks = FALSE,
          vovkSellke = FALSE) {

   defaultArgCalls <- formals(jaspAnova::Manova)
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
   optionsWithFormula <- c("dependent", "fixedFactors", "modelTerms", "randomFactors")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspAnova", "Manova", "Manova.qml", options, version, FALSE))
}