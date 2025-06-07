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
	title: 						qsTr("Bar Plots"); info: qsTr("To create a bar plot, select the independent variable to be placed on the horizontal axis. If there are more than one independent variable, the variables can be displayed in separate plots by selecting the other variable in the box Separate plots.")
	property alias source: 		availableTerms.source
	property int framework:		Common.Type.Framework.Classical
	columns: 					1

	VariablesForm
	{
		preferredHeight: 150 * preferencesModel.uiScale
		AvailableVariablesList { name: "barPlotVariables"; 				title: qsTr("Factors"); info: qsTr("The independent variables included in the analysis.") ;			id: availableTerms }
		AssignedVariablesList { name: "barPlotHorizontalAxis";			title: qsTr("Horizontal Axis"); info: qsTr("Select the independent variable that should be displayed on the horizontal axis of the plot.")	;singleVariable: true; allowedColumns: ["nominal"] }
		AssignedVariablesList { name: "barPlotSeparatePlots";			title: qsTr("Separate Plots"); info: qsTr("By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.")	;	singleVariable: true; allowedColumns: ["nominal"] }
	}

	Group
	{
		title: 		qsTr("Display")
		
		CheckBox
		{
			name: 	"barPlotErrorBars"
			label: 	qsTr("Error bars"); info: qsTr("By selecting this option, error bars will be displayed in the plot. The error bars can represent either confidence intervals or standard errors.")
			
			RadioButtonGroup
			{
				name: 	"barPlotErrorBarType"
				
				RadioButton
				{
					value: 				"ci" 
					label: 				framework === Common.Type.Framework.Classical ? qsTr("Confidence interval") : qsTr("Credible interval"); info: qsTr(" This option is selected by default. With this option, the error bars will represent confidence intervals of the mean of each level combination of the independent variables, or the credible interval in the case of Bayesian analyses. By default, the confidence/credible interval is set to 95%, but this can be changed into the desired percentage.")
					checked: 			true
					childrenOnSameRow: 	true
					
					CIField { name: 	"barPlotCiInterval" }
				}
				RadioButton { value: 	"se"; 	label: qsTr("Standard error"); info: qsTr("By selecting this option, the error bars will represent standard errors of the mean of each level combination of the independent variables.") }
			}
		}
		CheckBox { name: "barPlotHorizontalZeroFix"; 		label: qsTr("Fix horizontal axis to 0"); info: qsTr("Forces the graphs to show the default x-axis at y = 0.")	;checked: true }
	}
}
