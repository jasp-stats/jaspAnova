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
	title: 						qsTr("Bar Plots")
	property alias source: 		availableTerms.source
	columns: 					1

	VariablesForm
	{
		preferredHeight: 150 * preferencesModel.uiScale
		AvailableVariablesList { name: "descriptivePlotsTwoVariables"; 	title: qsTr("Factors"); 			id: availableTerms }
		AssignedVariablesList { name: "plotTwoHorizontalAxis";			title: qsTr("Horizontal Axis"); 	singleVariable: true }
		AssignedVariablesList { name: "plotTwoSeparatePlots";			title: qsTr("Separate Plots");		singleVariable: true }
	}

	Group
	{
		title: 		qsTr("Display")
		
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
		CheckBox { name: "zeroFix"; 	label: qsTr("Fix horizontal axis to 0");	checked: true }
	}
}
