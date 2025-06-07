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

Section
{
	property alias	source:				availableTerms.source
	property bool	enableHorizontal:	true
	property bool	enableYAxisLabel:	false
	property var	allowed:			(enableYAxisLabel) ? [] : ["nominal"]
	
	title: qsTr("Raincloud Plots")

	VariablesForm
	{
		preferredHeight: 150 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "rainCloudAvailableFactors";		title: qsTr("Factors"); info: qsTr("The independent variables included in the analysis.") ;id: availableTerms }
		AssignedVariablesList	{ name: "rainCloudHorizontalAxis";	title: qsTr("Horizontal Axis"); info: qsTr("Select the independent variable that should be displayed on the horizontal axis of the plot.") ;singleVariable: true; allowedColumns: allowed }
		AssignedVariablesList	{ name: "rainCloudSeparatePlots";	title: qsTr("Separate Plots"); info: qsTr("By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.")	;singleVariable: true; allowedColumns: allowed }
	}

	CheckBox
	{
		name: "rainCloudHorizontalDisplay";
		label: qsTr("Horizontal display"); info: qsTr("Changes the orientation of the raincloud difference plot so that the x-axis represents the dependent variable and the y-axis the difference between measures.")
		visible: enableHorizontal 
	}
	
	TextField
	{
		name: "rainCloudYAxisLabel";
		label: qsTr("Label y-axis");
		fieldWidth: 200;
		visible: enableYAxisLabel
	}
}
