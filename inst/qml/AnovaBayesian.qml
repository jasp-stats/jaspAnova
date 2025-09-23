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
import "./common/bayesian" as Bayesian

Form
{
	info: qsTr("The Bayesian ANOVA allows the user to analyze the difference between multiple group means.") + "\n" +
	"## " + qsTr("Assumptions") + "\n" +
	"- " + qsTr("The residuals are normally distributed for every group.") + "\n" +
	"- " + qsTr("The independent variables are categorical, the dependent variable is continuous.") + "\n" +
	"- " + qsTr("The variance of the dependent variable is the same for every group. This is called homogeneity of variances.") + "\n" +
	"- " + qsTr("The groups are independent.")
	

	id: form
	property int analysis:	Common.Type.Analysis.ANOVA
	property int framework:	Common.Type.Framework.Bayesian

	Formula
	{
		lhs: "dependent"
		rhs: [{ name: "modelTerms", extraOptions: "isNuisance" }]
		userMustSpecify: "randomFactors"
	}

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"																							}
		AssignedVariablesList	{ name: "dependent";		title: qsTr("Dependent Variable"); info: qsTr("The variable of interest. This is also called the outcome variable.")	;allowedColumns: ["scale"]; singleVariable: true	}
		AssignedVariablesList	{ name: "fixedFactors";		title: qsTr("Fixed Factors"); info: qsTr("The variables that are manipulated/define the different groups. These are also called the independent variables.")	;	allowedColumns: ["nominal"]; minLevels: 2			}
		AssignedVariablesList	{ name: "randomFactors";	title: qsTr("Random Factors"); info: qsTr("In this box, the variable can be selected that should be included in all models, including the null model.")	;	allowedColumns: ["nominal"]; minLevels: 2			}
	}

	Bayesian.DefaultOptions { matchedModelsEnabled: additionalOptions.marginalityEnforced	}

	Bayesian.ModelTerms { source: ["fixedFactors", "randomFactors"] }

	Bayesian.SingleModelInference { source: ["fixedFactors", "randomFactors"] }

	Bayesian.PostHocTests { source: "fixedFactors" }

	Bayesian.DescriptivesPlots { source: "fixedFactors" }
	
	Common.BarPlots 
	{ 
		source: ["fixedFactors", "randomFactors"]
		framework:	form.framework
	}

	Common.RainCloudPlots { source: ["fixedFactors", "randomFactors"] }

	Bayesian.AdditionalOptions { analysis: form.analysis; id: additionalOptions }

}
