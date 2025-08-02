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
	title:		qsTr("Marginal Means"); info: qsTr("When this option is selected, the mean for each level of the independent variable, adjusted for all the other variables in the model, is calculated.")
	columns:	1
	property alias source: availableTerms.source

	VariablesForm
	{
		preferredHeight:	jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "marginalMeanAvailableTerms"; id: availableTerms	}
		AssignedVariablesList {  name: "marginalMeanTerms"									}
	}

	CheckBox
	{
		name:				"marginalMeanBootstrap"
		id : marginalMeanBootstrap
		label:				qsTr("From"); info: qsTr("When this option is selected, the bootstrapped marginal means are calculated. By default, the number of replications is set to 1000. This can be changed into the desired number.")
		childrenOnSameRow:	true
		IntegerField
		{
			name:			"marginalMeanBootstrapSamples"
			defaultValue:	1000
			fieldWidth:		50
			min:			100
			afterLabel:		qsTr("bootstraps")
		}
	}

	CheckBox
	{
		name:	"marginalMeanComparedToZero";
		label:	qsTr("Compare marginal means to 0"); info: qsTr("By selecting this option, the adjusted means are compared to 0 and the confidence intervals of the adjusted means are calculated.")
	}

	DropDown
	{
		name:	"marginalMeanCiCorrection"
		label:	qsTr("Confidence interval adjustment")
		values:	[
			{ label:	qsTr("None"),	value:	"none", info: qsTr("When this option is selected, no adjustment will be applied.")},
			{ label:	"Bonferroni",	value:	"bonferroni", info: qsTr("Bonferroni correction of the confidence intervals.")},
			{ label:	"Šidák",		value:	"sidak", info: qsTr("Sidak correction of the confidence intervals.")}
		]
        enabled: !marginalMeanBootstrap.checked
		
	}
	
	CheckBox
	{
		isBound: false
		visible: analysis === Common.Type.Analysis.RMANOVA
		label: qsTr("Pool error term for follow-up tests")
        checked: analysis === Common.Type.Analysis.RMANOVA && poolErrorTermFollowup.checked
		onCheckedChanged: poolErrorTermFollowup.checked = checked
	}  

}
