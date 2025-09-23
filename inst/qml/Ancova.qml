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
	info: qsTr("ANCOVA allows the user to analyze the difference between multiple group means, while taking into account the effect of variables that have an influence on the dependent variable but are not part of the experimental manipulation (i.e., covariates).") + "\n" +
	"## " + qsTr("Assumptions") + "\n" +
	"- " + qsTr("The residuals are normally distributed for every group.") + "\n" +
	"- " + qsTr("The independent variables are categorical, the dependent variable is continuous.") + "\n" +
	"- " + qsTr("The variance of the dependent variable is the same for every group. This is called homogeneity of variances.") + "\n" +
	"- " + qsTr("The groups are independent.") + "\n" +
	"- " + qsTr("The covariate and the experiment effect are independent.") + "\n" +
	"- " + qsTr("The effect of the covariate on the dependent variable does not differ between groups. This is called homogeneity of the regression slopes.")

	id: form
	property int analysis:	Common.Type.Analysis.ANCOVA
	property int framework:	Common.Type.Framework.Classical

	Classical.InvisiblePlotSizes{}

	Formula
	{
		lhs: "dependent"
		rhs: "modelTerms"
		userMustSpecify: ["randomFactors", "covariates"]
	}

	VariablesForm
	{
		preferredHeight:	400 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";		title: qsTr("Dependent Variable"); info: qsTr("The variable of interest. This is also called the outcome variable.") ; allowedColumns: ["scale"]; 		singleVariable: true	}
		AssignedVariablesList	{ name: "fixedFactors";		title: qsTr("Fixed Factors"); info: qsTr(" The variables that are manipulated/define the different groups. These are also called the independent variables.")	;	allowedColumns: ["nominal"]; minLevels: 2						}
		AssignedVariablesList	{ name: "randomFactors";	title: qsTr("Random Factors")	; allowedColumns: ["nominal"]; minLevels: 2;	debug: true				}
		AssignedVariablesList	{ name: "covariates";		title: qsTr("Covariates"); info: qsTr("In this box the variable that is the covariate can be selected. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.")	;		allowedColumns: ["scale"]; minNumericLevels: 2					}
		AssignedVariablesList	{ name: "wlsWeights";		title: qsTr("WLS Weights");	info: qsTr("Weighted Least Squares, here the variable specifying which points have more weight and are therefore considered more informative can be selected. For this last option it is important to know the weights a priori. This option is primarily used when the errors are heteroskedastic, meaning they are not equally distributed across levels of the independent variable.")	;	allowedColumns: ["scale"]; minNumericLevels: 2;		singleVariable: true	}
	}

	Classical.Display
	{
		analysis: form.analysis
	}

	Classical.Models
	{
		source: ["fixedFactors", "randomFactors", "covariates"]
	}

	Classical.AssumptionChecks
	{
		analysis: form.analysis
		CheckBox { name: "factorCovariateIndependenceCheck";	label: qsTr("Factor covariate independence check");	debug: true	}
	}

	Classical.Contrasts
	{
		analysis:	form.analysis
		source:		[{ name : "modelTerms", discard: "covariates" }]
	}

	Classical.OrderRestrictions
	{
		analysis:	form.analysis
		source:		[{ name: "modelTerms", discard: "covariates" }]
	}

	Classical.PostHoc
	{
		source: [{ name: "modelTerms", discard: "covariates" }]
	}

	Classical.DescriptivePlots
	{
		source: ["fixedFactors", "randomFactors", "covariates"]
	}

	Common.BarPlots
	{
		source: ["fixedFactors", "randomFactors"]
		framework:	form.framework
	}
	
	Common.RainCloudPlots
	{
		source: ["fixedFactors", "randomFactors"]
	}

	Classical.MarginalMeans
	{
		source: [{ name: "modelTerms", discard: "covariates" }]
	}

	Classical.SimpleMainEffects
	{
		source: ["fixedFactors", "randomFactors"]
	}

	Classical.Nonparametrics
	{
		source: ["fixedFactors", "randomFactors"]
	}

	Classical.Export { id: exportComponent}
}
