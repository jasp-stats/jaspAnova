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
	property alias	source:				availableTerms.source
	property bool	enableHorizontal:	true
	property bool	enableYAxisLabel:	false
	property var	suggested:			(enableYAxisLabel) ? [] : ["ordinal", "nominal"]
	
	title: qsTr("Raincloud Plots")

	VariablesForm
	{
		preferredHeight: 150 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "rainCloudAvailableFactors";		title: qsTr("Factors"); id: availableTerms }
		AssignedVariablesList	{ name: "rainCloudHorizontalAxis";	title: qsTr("Horizontal Axis"); singleVariable: true; suggestedColumns: suggested }
		AssignedVariablesList	{ name: "rainCloudSeparatePlots";	title: qsTr("Separate Plots");	singleVariable: true; suggestedColumns: suggested }
	}

	CheckBox
	{
		name: "rainCloudPlotsHorizontalDisplay";
		label: qsTr("Horizontal display");
		visible: enableHorizontal 
	}
	
	TextField
	{
		name: "rainCloudPlotsLabelYAxis";
		label: qsTr("Label y-axis");
		fieldWidth: 200;
		visible: enableYAxisLabel
	}
}
