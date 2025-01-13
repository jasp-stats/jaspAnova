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

import QtQuick
import JASP.Controls
import JASP
import "./" as Common

Section
{
	title: 						qsTr("Bar Plots")
	property alias source: 		availableTerms.source
	property int framework:		Common.Type.Framework.Classical
	columns: 					1

	VariablesForm
	{
		preferredHeight: 150 * preferencesModel.uiScale
		AvailableVariablesList { name: "barPlotVariables"; 				title: qsTr("Factors"); 			id: availableTerms }
		AssignedVariablesList { name: "barPlotHorizontalAxis";			title: qsTr("Horizontal Axis"); 	singleVariable: true; allowedColumns: ["nominal"] }
		AssignedVariablesList { name: "barPlotSeparatePlots";			title: qsTr("Separate Plots");		singleVariable: true; allowedColumns: ["nominal"] }
	}

	Group
	{
		title: 		qsTr("Display")
		
		CheckBox
		{
			name: 	"barPlotErrorBars"
			label: 	qsTr("Error bars")
			
			RadioButtonGroup
			{
				name: 	"barPlotErrorBarType"
				
				RadioButton
				{
					value: 				"ci" 
					label: 				framework === Common.Type.Framework.Classical ? qsTr("Confidence interval") : qsTr("Credible interval")
					checked: 			true
					childrenOnSameRow: 	true
					
					CIField { name: 	"barPlotCiInterval" }
				}
				RadioButton { value: 	"se"; 	label: qsTr("Standard error") }
			}
		}
		CheckBox { name: "barPlotHorizontalZeroFix"; 		label: qsTr("Fix horizontal axis to 0");	checked: true }
	}
}
