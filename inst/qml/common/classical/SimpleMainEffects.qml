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
		AssignedVariablesList	{ name: "simpleMainEffectFactor";				title: qsTr("Simple Effect Factor") ; info: qsTr("Select the independent variable to determine the effect of this variable, conditional on the levels of the moderator factor(s).") ; singleVariable: true }
		AssignedVariablesList	{ name: "simpleMainEffectModeratorFactorOne";	title: qsTr("Moderator Factor 1")	; info: qsTr("Select the independent variable that will represent the different levels.") ;singleVariable: true }
		AssignedVariablesList	{ name: "simpleMainEffectModeratorFactorTwo";	title: qsTr("Moderator Factor 2")	; info: qsTr("Select an optional, additional independent variable.") ;singleVariable: true }
	}

	CheckBox
	{
		name: "simpleEffectSizeEstimates";	label: qsTr("Effect size estimates"); info: qsTr("By selecting this option, the specific types of calculations to estimate the effect size of the simple main effects can be specified.")
		Group
		{
			columns: 2
			CheckBox { name: "simpleEffectSizePartialEtaSquared";	label: qsTr("partial η²"); checked: true; info: qsTr("Partial eta squared (η²p) is the proportion of variance accounted for by the simple main effect after excluding variance from the error term: SS_effect / (SS_effect + SS_error), derived from the F statistic. Computed using the effectsize R package.") }
			CheckBox { name: "simpleEffectSizePartialOmegaSquared";	label: qsTr("partial ω²"); info: qsTr("Partial omega squared (ω²p) is a less biased estimate of the proportion of variance accounted for by the simple main effect, compared to partial η², derived from the F statistic. Computed using the effectsize R package.") }
		}

		CheckBox
		{
			name: "simpleEffectSizeCi"; label: qsTr("Confidence intervals"); info: qsTr("Displays confidence intervals for the effect size. Set at 95% by default but can be changed into the desired percentage.")
			CIField {	name: "simpleEffectSizeCiLevel" }
			childrenOnSameRow: true
		}
	}
}
