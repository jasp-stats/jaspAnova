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
import QtQuick			2.8
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import "./common" as Common
import "./common/bayesian" as Bayesian


Form
{
	id: form
	property int analysis:	Common.Type.Analysis.RMANOVA
	property int framework:	Common.Type.Framework.Bayesian

	// The following part is used for spawning upgrade notifications about multigroup analysis
	Rectangle
	{
		visible:		myAnalysis !== null && myAnalysis.needsRefresh && (repeatedMeasuresFactors.nbFactors > 1)
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
			height: 180 * preferencesModel.uiScale
			factorName: qsTr("RM Factor")
		}
		AssignedRepeatedMeasuresCells
		{
			name: "repeatedMeasuresCells"
			title: qsTr("Repeated Measures Cells")
			source: "repeatedMeasuresFactors"
		}
		AssignedVariablesList
		{
			name: "betweenSubjectFactors"
			title: qsTr("Between Subject Factors")
			suggestedColumns: ["ordinal", "nominal"]
			itemType: "fixedFactors"
		}
		AssignedVariablesList
		{
			name: "covariates"
			title: qsTr("Covariates")
			suggestedColumns: ["scale"]
			id: covariates
		}
	}

	Bayesian.DefaultOptions { matchedModelsEnabled: additionalOptions.marginalityEnforced	}

	Bayesian.ModelTerms { source: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"]	}

	Bayesian.SingleModelInference { source: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"] }

	Bayesian.PostHocTests { source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }

	Bayesian.DescriptivesPlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		showLabel: true
	}
	
	Section
	{
		title: qsTr("Bar Plots")
		columns: 1
		
		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList { name: "descriptivePlotsTwoVariables"; title: qsTr("Factors"); source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList { name: "plotTwoHorizontalAxis";			title: qsTr("Horizontal Axis"); singleVariable: true }
			AssignedVariablesList { name: "plotTwoSeparatePlots";			title: qsTr("Separate Plots");	singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
		}
		
		TextField { name: "labelYAxisTwo"; label: qsTr("Label y-axis"); fieldWidth: 200 }
		Group
		{
			title: qsTr("Display")
			columns: 2
			CheckBox
			{
				name: "plotTwoErrorBars"; label: qsTr("Display error bars")
				RadioButtonGroup
				{
					name: "errorBarTypeTwo"
					RadioButton
					{
						value: "confidenceInterval";		label: qsTr("Credible interval"); checked: true
						childrenOnSameRow: true
						CIField { name: "plotTwoCredibleIntervalInterval" }
					}
					RadioButton { value: "standardErrorTwo";	label: qsTr("Standard error") }
				}
			}
			CheckBox { name: "zeroFix";		label: qsTr("Fix Horizontal Axis to 0")							 }
		}
	}

	Common.RainCloudPlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		enableHorizontal: false
		enableYAxisLabel: true
	}

	Bayesian.AdditionalOptions { analysis: form.analysis; id: additionalOptions; covariates: covariates }
}
