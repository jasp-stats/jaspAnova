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
import "../." as Common

Section
{
	title:		qsTr("Descriptives Plots"); info: qsTr("To create a descriptive plot, select the independent variable to be placed on the horizontal axis. If there are more than one independent variable, they can be visualized in a single plot by placing the second variable in 'Separate Lines,' or visualized in separate plots by placing the variable in 'Separate Plots'.")
	columns:	1
	property alias source: availableTerms.source

	VariablesForm
	{
		preferredHeight: 150 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "descriptivePlotAvailableFactors";	title: qsTr("Factors"); info: qsTr("The independent variables included in the analysis.")	;id: availableTerms }
		AssignedVariablesList	{ name: "descriptivePlotHorizontalAxis";			title: qsTr("Horizontal Axis"); info: qsTr("Select the independent variable that should be displayed on the horizontal axis of the plot.")	;singleVariable: true }
		AssignedVariablesList	{ name: "descriptivePlotSeparateLines";			title: qsTr("Separate Lines"); info: qsTr("By placing an independent variable in this box, different lines corresponding to the different levels of the independent variable will be displayed.")	;singleVariable: true; allowedColumns: ["nominal"] }
		AssignedVariablesList	{ name: "descriptivePlotSeparatePlot";			title: qsTr("Separate Plots"); info: qsTr("By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.")	;singleVariable: true; allowedColumns: ["nominal"] }
	}

	Group
	{
		title: qsTr("Display")
		CheckBox
		{
			name: "descriptivePlotErrorBar"; label: qsTr("Error bars"); checked: true; info: qsTr("Display error bars in the plot. The error bars can represent either confidence intervals or standard errors.")
			RadioButtonGroup
			{
				name: "descriptivePlotErrorBarType"
				RadioButton
				{
					value:				"ci";
					label:				qsTr("Confidence interval")
					checked:			true
					childrenOnSameRow:	true
					CIField { name: "descriptivePlotCiLevel" }
				}
				RadioButton 
				{ 
					value: "se"
					label: qsTr("Standard error")
				}
				
				CheckBox 
				{ 
					name: "normalizeErrorBarsDescriptives"
					label: qsTr("Normalize error bars")
					checked:			true
					visible: analysis === Common.Type.Analysis.RMANOVA
				}
			}
		}
	}
}
