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
	info: qsTr("ANOVA allows the user to analyze the difference between multiple group means.") + "\n" +
	"## " + qsTr("Assumptions") + "\n" +
	"- " + qsTr("The residuals are normally distributed for every group.") + "\n" +
	"- " + qsTr("The independent variables are categorical, the dependent variable is continuous.") + "\n" +
	"- " + qsTr("The variance of the dependent variable is the same for every group. This is called homogeneity of variances.") + "\n" +
	"- " + qsTr("The groups are independent.")

	id: form
	property int analysis:	Common.Type.Analysis.ANOVA
	property int framework:	Common.Type.Framework.Classical
	
	Classical.InvisiblePlotSizes{}

	Formula
	{
		lhs: "dependent"
		rhs: "modelTerms"
		userMustSpecify: "randomFactors"
	}

	VariablesForm
	{
		preferredHeight: 400 * preferencesModel.uiScale
		AvailableVariablesList	{	name:	"allVariablesList" }
		AssignedVariablesList	{	name:	"dependent";		title: qsTr("Dependent Variable"); info: qsTr("The variable of interest. This is also called the outcome variable.")	;allowedColumns: ["scale"];				singleVariable: true		}
		AssignedVariablesList	{	name:	"fixedFactors";		title: qsTr("Fixed Factors"); info: qsTr("The variables that are manipulated/define the different groups. These are also called the independent variables.")	;	allowedColumns: ["nominal"]; minLevels: 2 }
		AssignedVariablesList	{	name:	"randomFactors";	title: qsTr("Random Factors")	;	allowedColumns: ["nominal"]; minLevels: 2;	debug:	true				}
		AssignedVariablesList	{	name:	"wlsWeights";		title: qsTr("WLS Weights");	info: qsTr("Weighted Least Squares, here the variable specifying which points have more weight and are therefore considered more informative can be selected. For this last option it is important to know the weights a priori. This option is primarily used when the errors are heteroskedastic, meaning they are not equally distributed across levels of the independent variable.")	;	allowedColumns: ["scale"];				singleVariable: true		}
	}

	Classical.Display
	{
		analysis: form.analysis
	}

	Classical.Models
	{
		source: ["fixedFactors", "randomFactors"]
	}

	Classical.AssumptionChecks
	{
		analysis: form.analysis
	}

	Classical.Contrasts
	{
		analysis:	form.analysis
		source:		"modelTerms"
	}

	Classical.OrderRestrictions
	{
		analysis:	form.analysis
		source:		"modelTerms"
	}
	
	Classical.PostHoc
	{
		source: "modelTerms"
	}
	
	Classical.DescriptivePlots
	{
		source: ["fixedFactors", "randomFactors"]
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
		source: "modelTerms"
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
