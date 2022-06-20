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
import "./common" as Common
import "./common/classical" as Classical

Form
{
	id: form
	property int analysis:	Common.Type.Analysis.ANOVA
	property int framework:	Common.Type.Framework.Classical
	
	Classical.InvisiblePlotSizes{}

	VariablesForm
	{
		preferredHeight: 400 * preferencesModel.uiScale
		AvailableVariablesList	{	name:	"allVariablesList" }
		AssignedVariablesList	{	name:	"dependent";		title: qsTr("Dependent Variable");	suggestedColumns: ["scale"];				singleVariable: true		}
		AssignedVariablesList	{	name:	"fixedFactors";		title: qsTr("Fixed Factors");		suggestedColumns: ["ordinal", "nominal"]								}
		AssignedVariablesList	{	name:	"randomFactors";	title: qsTr("Random Factors");		suggestedColumns: ["ordinal", "nominal"];	debug:	true				}
		AssignedVariablesList	{	name:	"wlsWeights";		title: qsTr("WLS Weights");			suggestedColumns: ["scale"];				singleVariable: true		}
	}
	
	Section
	{
		title: 		qsTr("Bar Plots")
		columns: 	1
		
		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList { name: "descriptivePlotsTwoVariables"; 	title: qsTr("Factors"); 		source: ["fixedFactors", "randomFactors"] }
			AssignedVariablesList { name: "plotTwoHorizontalAxis";			title: qsTr("Horizontal Axis"); singleVariable: true }
			AssignedVariablesList { name: "plotTwoSeparatePlots";			title: qsTr("Separate Plots");	singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
		}
		
		Group
		{
			title: 		qsTr("Display")
			columns: 	2
			
			CheckBox
			{
				name: 	"plotTwoErrorBars"
				label: 	qsTr("Display error bars")
				
				RadioButtonGroup
				{
					name: 	"errorBarTypeTwo"
					
					RadioButton
					{
						value: 				"confidenceInterval"
						label: 				qsTr("Confidence interval")
						checked: 			true
						childrenOnSameRow: 	true
						
						CIField { name: 	"confidenceIntervalIntervalTwo" }
					}
					RadioButton { value: 	"standardErrorTwo"; label: qsTr("Standard error") }
				}
			}
			CheckBox { name: "zeroFix"; label: qsTr("Fix horizontal axis to 0")	}
		}
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
}
