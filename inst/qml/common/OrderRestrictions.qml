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
	
	title: qsTr("Order Restrictions")
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
	
	Form
	{
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
					applyScriptInfo: qsTr("Ctrl + Enter to apply. Click on the blue button below for help on the restriction syntax")
				}
				
				Group
				{
					columns: 2
					
					Group
					{
						columns: 1
						
						CheckBox
						{
							name: "modelSummary"
							label: qsTr("Summary for ") + rowValue
							checked: false
						}
						
						CheckBox
						{
							name: "informedHypothesisTest"
							label: qsTr("Informed hypothesis test for ") + rowValue
							checked: false
							visible: type !== "RM-Anova"
							columns: 3
							
							CheckBox
							{
								name: "informedHypothesisTestGlobal"
								label: qsTr("Type global")
							}
							
							CheckBox
							{
								name: "informedHypothesisTestA"
								label: qsTr("Type A")
							}
							
							CheckBox
							{
								name: "informedHypothesisTestB"
								label: qsTr("Type B")
								checked: true
							}
						}
					}
				}
			}
		}
	}
	
	CheckBox
	{
		name: "includeIntercept"
		id: incInt
		label: qsTr("Include intercept")
		visible: type !== "RM-Anova"
		checked: true
	}
	
	Group
	{
		title: qsTr("Model comparison")
		columns: 1
		
		DropDown
		{
			property var comparisonValuesInclComplement: 
			[
				{ label: qsTr("Complement"), value: "complement"},
				{ label: qsTr("Unconstrained"), value: "unconstrained" },
				{ label: qsTr("None"), value: "none" }
			]
			property var comparisonValuesExclComplement: 
			[
				{ label: qsTr("Unconstrained"), value: "unconstrained" },
				{ label: qsTr("None"), value: "none" }
			]
			property var comparisonValues: (models.count > 1) ? comparisonValuesExclComplement : comparisonValuesInclComplement
			
			name: "restrictedModelComparison"
			label: qsTr("Compare against")
			indexDefaultValue: 0
			values: comparisonValues
			id: modelComparison
		}
		
		DropDown
		{
			property var modelsToReference: (modelComparison.value === "none") ? [] : [ modelComparison.currentLabel ]
			
			name: "restrictedModelComparisonReference"
			label: qsTr("Reference model for weight ratios")
			indexDefaultValue: 0
			source:  [ models, { values: modelsToReference } ]
		}
		
		CheckBox
		{
			name: "restrictedModelComparisonCoefficients"
			label: qsTr("Show model coefficients")
			childrenOnSameRow: true
			
			CheckBox
			{
				name: "highlightEstimates"
				label: qsTr("Highlight coefficients")
				checked: true
			}
		}
	}
	
	Group
	{
		title: qsTr("Uncertainty")
		columns: 1
		
		Group
		{
			columns: 2
			
			CIField
			{
				name: "restrictedConfidenceIntervalLevel"
				afterLabel: type === "RM-Anova" ? qsTr("% confidence intervals based on") : qsTr("% confidence intervals")
			}
			
			Group
			{
				columns: 2
				
				CheckBox
				{
					name: "restrictedConfidenceIntervalBootstrap"
					label: qsTr("based on")
					id: restrictedModelsBootstrap
					visible: type !== "RM-Anova"
				}
				
				IntegerField
				{
					name: "restrictedConfidenceIntervalBootstrapSamples"
					defaultValue: 1000
					min: 100
					afterLabel: qsTr("bootstraps")
				}
			}
		}
		
		DropDown
		{
			name: "restrictedModelHeteroskedasticity"
			label: qsTr("Huber-White correction for heteroskedasticity")
			indexDefaultValue: 0
			visible: type !== "RM-Anova"
			enabled: !restrictedModelsBootstrap.checked
			values:
			[
				{ label: qsTr("None"), value: "none" },
				{ label: qsTr("HC0"), value: "HC0" },
				{ label: qsTr("HC1"), value: "HC1" },
				{ label: qsTr("HC2"), value: "HC2" },
				{ label: qsTr("HC3"), value: "HC3" },
				{ label: qsTr("HC4"), value: "HC4" },
				{ label: qsTr("HC4m"), value: "HC4m" },
				{ label: qsTr("HC5"), value: "HC5" },
			]
		}
	}
	
	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight * 0.5
		AvailableVariablesList
		{
			name: "restrictedModelTerms"
			title: qsTr("Model Terms")
			source: (type === "Ancova" ? [ { name: "modelTerms", discard: "covariates" } ] :
										 type === "RM-Anova" ? [ { name: "betweenModelTerms", discard: "covariates" }, { name: "withinModelTerms" } ] :
															   "modelTerms")
		}
		
		AssignedVariablesList { name: "restrictedModelMarginalMeansTerm"; title: qsTr("Restricted Marginal Means"); singleVariable: true }
	}
	
	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight * 0.5
		AvailableVariablesList
		{
			property var modelsToPlot: (modelComparison.value === "none") ? [] : [ modelComparison.currentLabel ]
			
			name: "availableRestrictedModels"
			title: qsTr("Restricted Models")
			source: [ models, { values: modelsToPlot }]
		}
		
		AssignedVariablesList { name: "plotRestrictedModels"; title: qsTr("Plot Restricted Marginal Means") }
	}
}
