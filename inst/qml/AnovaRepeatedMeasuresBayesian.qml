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
import "./common" as ANOVA

Form
{

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

	ANOVA.DefaultOptions { matchedModelsEnabled: additionalOptions.marginalityEnforced	}

	ANOVA.ModelTerms { source: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"]	}

	ANOVA.SingleModelInference { source: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"] }

	ANOVA.PostHocTests { source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }

	ANOVA.DescriptivesPlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		showLabel: true
	}

	ANOVA.RainCloudPlots
	{
		availableVariableSource: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
		enableHorizontal: false
		enableYAxisLabel: true
	}

	ANOVA.AdditionalOptions { analysisType: ANOVA.AnalysisType.AnalysisType.BRMANOVA; id: additionalOptions; covariates: covariates }


}
