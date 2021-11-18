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
	// defaults follow ANOVA + ANCOVA
	property	string	sectionTitle : qsTr("Model")
	property	var		variablesSource: ["fixedFactors", "randomFactors"]

	title: sectionTitle

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "components"; title: qsTr("Components"); source: variablesSource}
		ModelTermsList {}
	}

	DropDown
	{
		name: "modelSpaceType"
		indexDefaultValue: 0
		label: qsTr("Model space type")
		values: [
			{ label: qsTr("Type \u2161"),				value: "type 2"		},
			{ label: qsTr("Type \u2162"),				value: "type 3"		},
			{ label: qsTr("Type \u2161 + \u2162"),		value: "type 2 + 3"	}
		]
	}

}
