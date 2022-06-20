//
// Copyright (C) 2013-2022 University of Amsterdam
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
	property bool showLabel: 	false
	property alias source: 		descriptivePlotsTwoVariables.source

	title: qsTr("Bar Plots")

	VariablesForm
	{
		preferredHeight: 150 * preferencesModel.uiScale
		AvailableVariablesList { name: "descriptivePlotsTwoVariables"; 	title: qsTr("Factors"); 			id: descriptivePlotsTwoVariables }
		AssignedVariablesList { name: "plotTwoHorizontalAxis";			title: qsTr("Horizontal Axis"); 	singleVariable: true }
		AssignedVariablesList { name: "plotTwoSeparatePlots";			title: qsTr("Separate Plots");		singleVariable: true }
	}
	
	TextField
	{
		visible: 	showLabel
		name: 		"labelYAxisTwo" 
		label: 		qsTr("Label y-axis") 
		fieldWidth: 200
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
					label: 				qsTr("Credible interval")
					checked: 			true
					childrenOnSameRow: 	true
					
					CIField { name: 	"plotTwoCredibleIntervalInterval" }
				}
				RadioButton { value: 	"standardErrorTwo"; label: qsTr("Standard error") }
			}
		}
		CheckBox { name: "zeroFix"; label: qsTr("Fix horizontal axis to 0")	}
	}
}
