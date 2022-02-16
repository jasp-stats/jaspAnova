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

import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Section
{
	property var type
	
	title: qsTr("Order Restricted Hypotheses")
	columns: 1
	
	Text
	{
		text: qsTr("Enter each restriction on a new line, e.g., factorLow == factorHigh, \nwhere 'factor' is the factor name and 'Low'/'High' are the factor level names. \nClick the information icon for more examples.")
	}

	HelpButton
	{
		toolTip: qsTr("Click to learn more about the syntax for order restrictions.")
		helpPage: "goric/restriktorSyntax"
	}
	
	Group
	{
		title:		qsTr("Global settings")
		columns:	2

		CheckBox
		{
			name: "includeIntercept"
			id: incInt
			label: qsTr("Include intercept")
			visible: type !== "RM-Anova"
		}

		CheckBox
		{
			name:	"restrictedModelShowAvailableCoefficients"
			label:	qsTr("Show coefficients available in the syntax")
		}

		Group
		{
			columns: 3
			title: qsTr("Set for all models")
			CheckBox
			{
				name:	"restrictedModelSummaryByDefault"
				id:	modelSummaryByDefault
				label:	qsTr("Model summary")
			}

			CheckBox
			{
				name:	"restrictedInformedHypothesisTestByDefault"
				id:	informedHypothesisTestByDefault
				label:	qsTr("Informed hypothesis tests")
			}

			CheckBox
			{
				name:	"restrictedMarginalMeansByDefault"
				id:		marginalMeansByDefault
				label: qsTr("Marginal means")
			}
		}


	}

	TabView
	{
		id: models
		name: "restrictedModels"
		maximumItems: 8
		newItemName: qsTr("Model 1")
		optionKey: "modelName"

		content: Group
		{
			TextArea
			{
				name: "restrictionSyntax"
				width: models.width
				textType: JASP.TextTypeModel
				trim: true
				applyScriptInfo: qsTr("Ctrl + Enter to apply. Click on the blue button above for help on the restriction syntax")
			}

			Group
			{
				columns: 3
				CheckBox
				{
					name: "modelSummary"
					label: qsTr("Summary for %1").arg(rowValue)
					checked: modelSummaryByDefault.checked
				}


				CheckBox
				{
					name: "informedHypothesisTest"
					label: qsTr("Informed hypothesis tests for %1").arg(rowValue)
					checked: informedHypothesisTestByDefault.checked
					visible: type !== "RM-Anova"
				}

				CheckBox
				{
					name: "marginalMeans"
					label:	qsTr("Marginal means for %1").arg(rowValue)
					checked: marginalMeansByDefault.checked
				}
			}
		}
	}

	Group
	{
		title: qsTr("Model comparison")
		columns: 1
		
		DropDown
		{
			property var comparisonValuesInclComplement: 
			[
				{ label: qsTr("Complement model"), value: "complement"},
				{ label: qsTr("Unconstrained model"), value: "unconstrained" },
				{ label: qsTr("None"), value: "none" }
			]
			property var comparisonValuesExclComplement: 
			[
				{ label: qsTr("Unconstrained model"), value: "unconstrained" },
				{ label: qsTr("None"), value: "none" }
			]
			property var comparisonValues: (models.count > 1) ? comparisonValuesExclComplement : comparisonValuesInclComplement
			
			name:				"restrictedModelComparison"
			label:				qsTr("Add to comparison")
			indexDefaultValue:	0
			values:				comparisonValues
			id:					modelComparison
		}
		
		CheckBox
		{
			name:				"restrictedModelComparisonWeights"
			label:				qsTr("Add weight ratios")
			checked:			true
			childrenOnSameRow:	false

			DropDown
			{
				property var modelsToReference: (modelComparison.value === "none") ? [] : [ { label: modelComparison.currentLabel, value: modelComparison.value } ]

				name:				"restrictedModelComparisonReference"
				label:				qsTr("Reference model")
				indexDefaultValue:	0
				source:				[ models, { values: modelsToReference } ]
			}
		}

		CheckBox
		{
			name:	"restrictedModelComparisonMatrix"
			label:	qsTr("Relative weights matrix")
		}
		
		CheckBox
		{
			name: "restrictedModelComparisonCoefficients"
			label: qsTr("Compare model coefficients")
			childrenOnSameRow: false
			
			CheckBox
			{
				name: "highlightEstimates"
				label: qsTr("Highlight differences with the unconstrained model")
				checked: true
			}
		}
	}


	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight * 0.5
		implicitWidth: jaspTheme.formWidth - 50
		AvailableVariablesList
		{
			name: "restrictedModelTerms"
			label: qsTr("Restricted Marginal Means")
			source: (type === "Ancova" ? [ { name: "modelTerms", discard: "covariates" } ] :
										 type === "RM-Anova" ? [ { name: "betweenModelTerms", discard: "covariates" }, { name: "withinModelTerms" } ] :
															   "modelTerms")
		}

		AssignedVariablesList { name: "restrictedModelMarginalMeansTerms"; label: qsTr("Terms") }
	}

	Group
	{
		title: qsTr("Uncertainty quantification")

		CIField
		{
			name:	"restrictedConfidenceIntervalLevel"
			label:	qsTr("Confidence intervals")
		}

		DropDown
		{
			name:				"restrictedMarginalMeansModelSE"
			id:					restrictedMarginalMeansModelSE
			label:				qsTr("Error calculation")
			indexDefaultValue:	0
			values:
			[
				{ label: qsTr("Standard"), value: "standard" },
				{ label: qsTr("Bootstrap"), value: "bootstrap"},
				{ label: qsTr("HC0"), value: "HC0" },
				{ label: qsTr("HC1"), value: "HC1" },
				{ label: qsTr("HC2"), value: "HC2" },
				{ label: qsTr("HC3"), value: "HC3" },
				{ label: qsTr("HC4"), value: "HC4" },
				{ label: qsTr("HC4m"), value: "HC4m" },
				{ label: qsTr("HC5"), value: "HC5" }
			]
		}

		IntegerField
		{
			name: "restrictedMarginalMeansBootstrappingReplicates"
			visible: restrictedMarginalMeansModelSE.value === "bootstrap"
			defaultValue: 1000
			fieldWidth: 50
			min: 100
			label: qsTr("From")
			afterLabel: qsTr("bootstraps")
		}
	}


}
