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
	title:		qsTr("Marginal Means")
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
		name:				"marginalMeanBootstrap";
		label:				qsTr("From")
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
		label:	qsTr("Compare marginal means to 0")
		DropDown
		{
			name:	"marginalMeanCiCorrection"
			label:	qsTr("Confidence interval adjustment")
			values:	[
				{ label:	qsTr("None"),	value:	"none"},
				{ label:	"Bonferroni",	value:	"bonferroni"},
				{ label:	"Šidák",		value:	"sidak"}
			]
		}
	}

}
