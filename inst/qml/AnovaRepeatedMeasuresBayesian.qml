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
	info: qsTr("The repeated Measures ANOVA allows the user to analyze the differences between means, when observations are dependent i.e Within subject factors. It also allows to combine with between subject factors.") + "\n" +
"## " + qsTr("Assumptions") + "\n" +
"- " + qsTr("The dependent variable is normally distributed for every group.") + "\n" +
"- " + qsTr("The covariate and the experiment effect are independent.") + "\n" +
"- " + qsTr("The assumption of sphericity is met. Sphericity entails that the variances of the differences between all possible pairs of the repeated measures conditions are the same.")

	id: form
	property int analysis:	Common.Type.Analysis.RMANOVA
	property int framework:	Common.Type.Framework.Bayesian

	// The following part is used for spawning upgrade notifications about multigroup analysis
	Rectangle
	{
		visible:		needsRefresh && (repeatedMeasuresFactors.nbFactors > 1)
		color:			jaspTheme.controlWarningBackgroundColor
		width:			form.implicitWidth
		height:			warningMessageUpdate.height
		radius:			jaspTheme.borderRadius

		Text
		{
			id:					warningMessageUpdate
			text:				qsTr("This analysis was created with an older version of JASP (or a dynamic module). Since then, the model specification of the Bayesian Repeated Measures ANOVA has changed. By default, all models now include random slopes for all but the highest order repeated-measures interaction to avoid discrepancies with the frequentist repeated measures ANOVA.")
			color:				jaspTheme.controlWarningTextColor
			anchors.top:		parent.top
			padding:			5 * jaspTheme.uiScale
			wrapMode:			Text.Wrap
			width:				parent.width - 10 * jaspTheme.uiScale
			verticalAlignment:	Text.AlignVCenter
		}
	}
	// end upgrade notifications

	VariablesForm
	{
		preferredHeight: 520 * preferencesModel.uiScale
		AvailableVariablesList { name: "allVariablesList" }
		FactorLevelList
		{
			id: repeatedMeasuresFactors
			name: "repeatedMeasuresFactors"
			title: qsTr("Repeated Measures Factors")
			info: qsTr("The within-subjects (repeated measures variable). Here the repeated measures factors of interest and the different levels that belong to the factor can be labelled.")
			height: 180 * preferencesModel.uiScale
			factorName: qsTr("RM Factor")
		}
		AssignedRepeatedMeasuresCells
		{
			name: "repeatedMeasuresCells"
			title: qsTr("Repeated Measures Cells")
			info: qsTr("The separate columns in the data frame that represent the levels of the repeated measure(s) factor(s). These are made based on the input on the repeated measures factors box.")
			source: "repeatedMeasuresFactors"
		}
		AssignedVariablesList
		{
			name: "betweenSubjectFactors"
			title: qsTr("Between Subject Factors")
			info: qsTr("Select when the subjects have been assigned into two or more separate groups")
			allowedColumns: ["nominal"]
			minLevels: 2
			itemType: "fixedFactors"
		}
		AssignedVariablesList
		{
			name: "covariates"
			title: qsTr("Covariates")
			info: qsTr("In this box the variable that is the covariate can be selected. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.")
			allowedColumns: ["scale"]
			id: covariates
			minNumericLevels: 2
		}
	}

	Bayesian.DefaultOptions { matchedModelsEnabled: additionalOptions.marginalityEnforced	}

	Bayesian.ModelTerms { source: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"]	}

	Bayesian.SingleModelInference { source: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"] }

	Bayesian.PostHocTests { source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }

	Bayesian.DescriptivesPlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		TextField	{ name: "descriptivePlotYAxisLabel";	label: qsTr("Label y-axis"); fieldWidth: 200	}
	}
	
	Common.BarPlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		framework:	form.framework
		TextField{ name: 	"labelYAxisTwo"; label: 	qsTr("Label y-axis"); fieldWidth: 	200 }
	}

	Common.RainCloudPlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		enableHorizontal: false
		enableYAxisLabel: true
	}

	Bayesian.AdditionalOptions { analysis: form.analysis; id: additionalOptions; covariates: covariates }
}
