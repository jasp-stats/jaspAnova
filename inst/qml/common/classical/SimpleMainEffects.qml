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
	title:	qsTr("Simple Main Effects"); info: qsTr("The simple main effects represent the effect of one independent variable for each level of the other independent variable, by conducting an ANOVA for each subset of the data as specified by the moderator variables.")
	property alias source:	availableTerms.source

	VariablesForm
	{
		preferredHeight:	170 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "simpleMainEffectAvailableFactors";		title: qsTr("Factors")	; info: qsTr("This box contains all the independent variables included in the analysis.")			; id: availableTerms }
		AssignedVariablesList	{ name: "simpleMainEffectFactor";				title: qsTr("Simple Effect Factor") ; info: qsTr("In this box, select the independent variable to determine the effect of this variable, conditional on the levels of the moderator factor(s).") ; singleVariable: true }
		AssignedVariablesList	{ name: "simpleMainEffectModeratorFactorOne";	title: qsTr("Moderator Factor 1")	; info: qsTr(" In this box, select the independent variable that will represent the different levels.") ;singleVariable: true }
		AssignedVariablesList	{ name: "simpleMainEffectModeratorFactorTwo";	title: qsTr("Moderator Factor 2")	; info: qsTr(" In this box, selector an optional, additional independent variable.") ;singleVariable: true }
	}
}
