//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import JASP
import JASP.Controls
import "./common" as Common
import "./common/classical" as Classical

Form
{
	info: qsTr("The repeated Measures ANOVA allows the user to analyze the differences between means, when observations are dependent i.e Within subject factors. It also allows combining with between subject factors.") + "\n" +
"## " + qsTr("Assumptions") + "\n" +
"- " + qsTr("The dependent variable is normally distributed for every group.") + "\n" +
"- " + qsTr("The covariate and the experiment effect are independent.") + "\n" +
"- " + qsTr("The assumption of sphericity is met. Sphericity entails that the variances of the differences between all possible pairs of the repeated measures conditions are the same.")

	id: form
	property int analysis:	Common.Type.Analysis.RMANOVA
	property int framework:	Common.Type.Framework.Classical

	Classical.InvisiblePlotSizes{}

	VariablesForm
	{
		preferredHeight: 520 * preferencesModel.uiScale
		AvailableVariablesList			{ name: "allVariablesList" }
		FactorLevelList					{ name: "repeatedMeasuresFactors";	title: qsTr("Repeated Measures Factors"); info: qsTr("The within-subjects (repeated measures) variables. Here the repeated measures factors of interest and the different levels that belong to each factor can be labelled.")	; height: 180 * preferencesModel.uiScale;	factorName: qsTr("RM Factor")	}
		AssignedRepeatedMeasuresCells	{ name: "repeatedMeasuresCells";	title: qsTr("Repeated Measures Cells"); info: qsTr("The separate columns in the data frame that represent the levels of the repeated measure(s) factor(s). These are made based on the input on the repeated measures factors box.")	;	source: "repeatedMeasuresFactors"										}
		AssignedVariablesList			{ name: "betweenSubjectFactors";	title: qsTr("Between Subject Factors");	info: qsTr("When the subjects have been assigned into two or more separate groups this variable can be selected.")	; allowedColumns: ["nominal"]; minLevels: 2;	itemType: "fixedFactors"	}
		AssignedVariablesList			{ name: "covariates";				title: qsTr("Covariates"); info: qsTr("In this box the variables that are covariates can be selected. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.")	;				allowedColumns: ["scale"]; minNumericLevels: 2							}
	}

	Classical.Display
	{
		analysis: form.analysis
	}

	Section
	{
		title: qsTr("Model"); info: qsTr("Components and model terms:")

		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList	{ name: "withinComponents"; title: qsTr("Repeated Measures Components"); info: qsTr("All the repeated measures factors and covariates that can be included in the model.") ; source: ["repeatedMeasuresFactors"] }
			AssignedVariablesList	{ name: "withinModelTerms"; title: qsTr("Model Terms"); info: qsTr("The repeated measures factors and covariates that can be included in the model. Can also include interaction effects.")	;listViewType: JASP.Interaction	}
		}

		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList	{ name: "betweenComponents"; title: qsTr("Between Subjects Components"); info: qsTr("All the between subjects factors and covariates that can be included in the model.") ;source: ["betweenSubjectFactors", "covariates"] }
			AssignedVariablesList	{ name: "betweenModelTerms"; title: qsTr("Model terms"); info: qsTr("The between measures factors and covariates that can be included in the model. Can also include interaction effects.") ;listViewType: JASP.Interaction }
		}

		Classical.SumOfSquares{}

		CheckBox
		{ 
			id: poolErrorTermFollowup
			name: "poolErrorTermFollowup"
			label: qsTr("Pool error term for follow-up tests"); info: qsTr(" By selecting this option, the univariate linear model, rather than the multivariate model, will be used for follow-up tests (contrasts, post-hoc tests, marginal means). Caution: multivariate models (i.e., unpooled error terms) handle departures from sphericity better, since these models allow the standard errors to differ for each level of the repeated measure(s) factor(s).")
			checked: false 
		}

	}

	Section
	{
		title: qsTr("Assumption Checks")

		Group
		{
			CheckBox { name: "sphericityTests";	label: qsTr("Sphericity tests"); info: qsTr("Sphericity entails that the variances of the differences of the repeated measures conditions are equal.") }
			Group
			{
				title: qsTr("Sphericity corrections"); info: qsTr("If sphericity is not met, then the test will have increased false positive rate (accepting the alternative when the null is true). One approach to correcting this is to reduce the degrees of freedom through sphericity corrections.")
				columns: 3
				CheckBox { name: "sphericityCorrectionNone";				label: qsTr("None"); info: qsTr("No correction is performed.")	;			checked: true }
				CheckBox { name: "sphericityCorrectionGreenhouseGeisser";	label: qsTr("Greenhouse-Geisser"); info: qsTr("This correction varies between 1/(k − 1), where k is the number of repeated-measures conditions.")	;checked: false }
				CheckBox { name: "sphericityCorrectionHuynhFeldt";			label: qsTr("Huynh-Feldt");	info: qsTr("Another common method to correct the degrees of freedom is Huynh-Feldt correction.")	;	checked: false }
			}
			CheckBox { name: "homogeneityTests"; label: qsTr("Homogeneity tests"); info: qsTr("By selecting this option, it will be checked whether the variance of the dependent variable is equal between the groups by performing Levene's test of equal variances. This test is only suitable for assessing equal variance for between subjects factors. For repeated measures, the sphericity test is more suitable.") }
			
			CheckBox 
			{ 
				name: "qqPlot";		 	label: qsTr("Q-Q plot residuals"); info: qsTr("Displays Q-Q plot of the standardized residuals. The confidence band shows the expected range of residuals under normality; points outside the band suggest deviations from normality.") 
				CheckBox
				{
					name:               "qqPlotCi"
					label:              qsTr("Confidence interval")
					childrenOnSameRow:  true
					CIField{ name: "qqPlotCiLevel" }
				}		
			}		
		}
	}

	Classical.Contrasts
	{
		analysis:	form.analysis
		source:		["withinModelTerms", { name: "betweenModelTerms", discard: "covariates", combineWithOtherModels: true }]
	}

	Classical.OrderRestrictions
	{
		analysis:	form.analysis
		source:		[{ name: "betweenModelTerms", discard: "covariates" }, { name: "withinModelTerms" }]
	}
	
	Section
	{
		title: qsTr("Post Hoc Tests")
		columns: 1

		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList { name: "postHocAvailableTerms"; source: ["withinModelTerms", { name: "betweenModelTerms", discard: "covariates", combineWithOtherModels: true }] }
			AssignedVariablesList {  name: "postHocTerms" }
		}

		Group
		{
			columns: 2
			CheckBox { name: "postHocEffectSize";	label: qsTr("Effect size")	; info: qsTr("By selecting this option, the effect size (i.e., the magnitude of the observed effect) will be displayed. The used measure for the effect size is Cohen's d. The effect size will only be displayed for the post hoc type Standard.")					}
			CheckBox
			{
				isBound: false
				label: qsTr("Pool error term for follow-up tests"); info: qsTr("By selecting this option, the univariate linear model, rather than the multivariate model, will be used for follow-up tests (contrasts, post-hoc tests, marginal means). Caution: multivariate models (i.e., unpooled error terms) handle departures from sphericity better, since these models allow the standard errors to differ for each level of the repeated measure(s) factor(s).")
				checked: poolErrorTermFollowup.checked
				onCheckedChanged: poolErrorTermFollowup.checked = checked
			}
			CheckBox 
			{ 
				name: "postHocConditionalTable"
				label: qsTr("Conditional comparisons for interactions"); info: qsTr("Instead of pairwise comparisons for all possible combination of cells in the interaction, list pairwise comparisons conditional on each of the interaction terms. This provides as many tables as there are terms in the interaction effect.") 
			}

		}

		Group
		{
			title: qsTr("Correction"); info: qsTr("To correct for multiple comparison testing and avoid Type I errors, different methods for correcting the p-value are available (note that the confidence intervals can only be adjusted using the Bonferroni method):")
			CheckBox { name: "postHocCorrectionHolm";			label: qsTr("Holm"); info: qsTr("This method is also called sequential Bonferroni, and considered less conservative than the Bonferroni method. Selected by default.")	;	checked: true	}
			CheckBox { name: "postHocCorrectionBonferroni";		label: qsTr("Bonferroni")	; info: qsTr("This correction is considered conservative. The risk of Type I error is reduced, however the statistical power decreases as well.")		}
			CheckBox { name: "postHocCorrectionTukey";			label: qsTr("Tukey"); info: qsTr(" Compare all possible pairs of group means. This correction can be used when the groups of the repeated measures have an equal sample size and variance.")				}
			CheckBox { name: "postHocCorrectionScheffe";		label: qsTr("Scheffé"); info: qsTr("Adjusting significance levels in a linear regression, to account for multiple comparisons. This method is considered to be quite conservative.")				}
		}

		Classical.PostHocDisplay{}
	}

	Classical.DescriptivePlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		TextField	{ name: "descriptivePlotYAxisLabel";		label: qsTr("Label y-axis"); fieldWidth: 200	}
		CheckBox	{ name: "descriptivePlotErrorBarPooled";	label: qsTr("Average across unused RM factors"); info: qsTr("When there are multiple RM factors in the model, but only plotting a subset of these factors, the mean is taken across the unused RM factors. For instance, when there are two RM factors with two levels in the model, A (1&2) and B (1&2), and only A is selected to be plotted, the average is taken of B across its levels. This means that when the mean of A1 is plotted, it is actually the average of A1B1 and A1B2). This procedure is discussed by Loftus & Masson (1994). When the box is not ticked, the averages are not taken, and the columns A1B1 and A1B2 are simply concatenated.")	}
	}
	
	Common.BarPlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		framework:	form.framework
		TextField 	{ name: "labelYAxisTwo"; 			label: qsTr("Label y-axis"); 	fieldWidth: 200 }
		CheckBox 	{ name: "usePooledStandErrorCITwo"; label: qsTr("Average across unused RM factors"); info: qsTr("When there are multiple RM factors in the model, but only plotting a subset of these factors, the mean is taken across the unused RM factors. For instance, when there are two RM factors with two levels in the model, A (1&2) and B (1&2), and only A is selected to be plotted, the average is taken of B across its levels. This means that when the mean of A1 is plotted, it is actually the average of A1B1 and A1B2). This procedure is discussed by Loftus & Masson (1994). When the box is not ticked, the averages are not taken, and the columns A1B1 and A1B2 are simply concatenated.") }
		CheckBox	{ name: "normalizeErrorBarsBarplot";	label: qsTr("Normalize error bars"); info: qsTr("In order to get accurate confidence intervals and standard errors for within subjects effects, the data are normalized by subtracting the appropriate participantʹs mean performance from each observation, and then adding the grand mean score to every observation. The variances of the resulting normalized values in each condition, and thus the size of the bars, no longer depend on the participant effects and are therefore a more accurate representation of the experimental manipulation.") ;checked: true}
	}

	Common.RainCloudPlots
	{
		source:				["repeatedMeasuresFactors", "betweenSubjectFactors"]
		enableHorizontal:	false
		enableYAxisLabel:	true
	}

	Classical.MarginalMeans
	{
		source: ["withinModelTerms", { name: "betweenModelTerms", discard: "covariates", combineWithOtherModels: true }]
	}

	Classical.SimpleMainEffects
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		CheckBox { name: "simpleMainEffectErrorTermPooled";	label: qsTr("Pool error terms"); info: qsTr("A pooled error term assumes that the variances of the contrast scores are approximately equal (i.e., sphericity assumption).") }
	}

	Section
	{
		title: qsTr("Nonparametrics"); info: qsTr("The Friedman test is a non-parametric alternative to the Repeated-Measures ANOVA when there is a complete block design. The Durbin test will automatically be selected when there is an incomplete block design.")

		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList	{ name: "friedmanAvailableFactors";		title: qsTr("Factors"); info: qsTr("This box contains all the repeated measures and between subjects factors that can be included in the analysis.") ;source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]	}
			AssignedVariablesList	{ name: "friedmanWithinFactor";			title: qsTr("RM Factor");	info: qsTr("The repeated measures factor(s) of interest.")															}
			AssignedVariablesList	{ name: "friedmanBetweenFactor";		title: qsTr("Optional Grouping Factor"); info: qsTr("Possible to select the between subjects factor here.") ;singleVariable: true							}
		}

		CheckBox { name: "conoverTest"; label: qsTr("Conover's post hoc tests"); info: qsTr("Conover's post-hoc test for pairwise comparisons, if the non-parametric test indicates significance.") }
	}

}
