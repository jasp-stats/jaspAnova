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
import QtQuick.Layouts
import JASP
import JASP.Controls
import "../." as Common


Section
{
	property int analysis
	property alias source: marginalMeansTerms.source
	
	title:		qsTr("Order Restricted Hypotheses")
	columns:	2
	
	Text
	{
		Layout.columnSpan:	2
		text:				qsTr("Enter each restriction of one hypothesis on a new line, e.g., \nfactorLow == factorMid\nfactorMid < factorHigh\nwhere 'factor' is the factor (or covariate) name and 'Low'/'Mid'/'High' are the factor level names.\nClick on the 'plus' icon to add more hypotheses. \nClick the information icon for more examples.")
	}

	HelpButton
	{
		toolTip:			qsTr("Click to learn more about the syntax for order restrictions.")
		helpPage:			"goric/restriktorSyntax"
		Layout.columnSpan:	2
	}

	Group
	{
		columns:			2
		Layout.columnSpan:	2
		Group
		{
			title:	qsTr("Syntax settings")
			CheckBox
			{
				name:	"restrictedInterceptInclusion"
				label:	qsTr("Include intercept")
			}

			CheckBox
			{
				name:	"restrictedAvailableCoefficients"
				label:	qsTr("Show available coefficients")
			}
		}

		Group
		{
			title:		qsTr("Set for all models")
			CheckBox
			{
				name:	"restrictedModelSummaryForAllModels"
				id:		modelSummaryByDefault
				label:	qsTr("Model summary")
			}

			CheckBox
			{
				name:	"restrictedMarginalMeanForAllModels"
				id:		marginalMeansByDefault
				label:	qsTr("Marginal means")
			}

			CheckBox
			{
				name:		"restrictedInformedHypothesisTestForAllModels"
				id:			informedHypothesisTestByDefault
				label:		qsTr("Informed hypothesis tests")
				visible:	analysis !== Common.Type.Analysis.RMANOVA
			}
		}
	}

	TabView
	{
		id:					models
		name:				"restrictedModels"
		maximumItems:		10
		newItemName:		qsTr("Model 1")
		optionKey:			"name"
		Layout.columnSpan:	2

		content: Group
		{
			TextArea
			{
				name:				"syntax"
				width:				models.width
				textType:			JASP.TextTypeModel
				trim:				true
				applyScriptInfo:	qsTr("Ctrl + Enter to apply. Click on the blue button above for help on the restriction syntax")
			}

			Group
			{
				columns: 3
				CheckBox
				{
					name:		"summary"
					label:		qsTr("Summary for %1").arg(rowValue)
					checked:	modelSummaryByDefault.checked
				}

				CheckBox
				{
					name:		"marginalMean"
					label:		qsTr("Marginal means for %1").arg(rowValue)
					checked:	marginalMeansByDefault.checked
				}

				CheckBox
				{
					name:		"informedHypothesisTest"
					label:		qsTr("Informed hypothesis tests for %1").arg(rowValue)
					checked:	informedHypothesisTestByDefault.checked
					visible:	analysis !== Common.Type.Analysis.RMANOVA
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
				{ label: qsTr("Complement model"),		value: "complement"		},
				{ label: qsTr("Unconstrained model"),	value: "unconstrained"	},
				{ label: qsTr("None"),					value: "none"			}
			]
			property var comparisonValuesExclComplement: 
			[
				{ label: qsTr("Unconstrained model"),	value: "unconstrained"	},
				{ label: qsTr("None"),					value: "none"			}
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
			name:				"restrictedModelComparisonCoefficients"
			label:				qsTr("Compare model coefficients")
			childrenOnSameRow:	false
			
			CheckBox
			{
				name:		"restrictedModelComparisonCoefficientsHighlight"
				label:		qsTr("Highlight active restrictions")
				checked:	true
			}
		}
	}

	Group
	{
		title: qsTr("Uncertainty quantification")

		DropDown
		{
			name:				"restrictedHeterogeneityCorrection"
			label:				qsTr("Heterogeneity correction")
			visible:			analysis !== Common.Type.Analysis.RMANOVA
			indexDefaultValue:	0
			values:
			[
				{ label: qsTr("None"),	value: "none"			},
				{ label: qsTr("HC0"),	value: "huberWhite0"	},
				{ label: qsTr("HC1"),	value: "huberWhite1"	},
				{ label: qsTr("HC2"),	value: "huberWhite2"	},
				{ label: qsTr("HC3"),	value: "huberWhite3"	},
				{ label: qsTr("HC4"),	value: "huberWhite4"	},
				{ label: qsTr("HC4m"),	value: "huberWhite4m"	},
				{ label: qsTr("HC5"),	value: "huberWhite5"	}
			]
		}


		CheckBox
		{
			name:	"restrictedBootstrap"
			label:	qsTr("Bootstrapping")

			IntegerField
			{
				name:			"restrictedBootstrapSamples"
				defaultValue:	1000
				fieldWidth:		50
				min:			100
				label:			qsTr("Samples")
			}

			CIField
			{
				name:	"restrictedBootstrapCiLevel"
				label:	qsTr("Confidence intervals")
			}
		}
	}


	VariablesForm
	{
		preferredHeight:	jaspTheme.smallDefaultVariablesFormHeight
		Layout.columnSpan:	2
		AvailableVariablesList
		{
			name: "restrictedModelTerms"
			label: qsTr("Restricted Marginal Means")
			id: marginalMeansTerms
		}

		AssignedVariablesList { name: "restrictedMarginalMeanTerms"; label: qsTr("Terms") }
	}


}
