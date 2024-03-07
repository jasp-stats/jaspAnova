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
	title: qsTr("Nonparametrics")
	property alias source: availableTerms.source

	VariablesForm
	{
		preferredHeight:	170 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "kruskalWallisAvailableFactors";	title: qsTr("Kruskal-Wallis Test");	id: availableTerms	}
		AssignedVariablesList	{ name: "kruskalWallisFactors";				title: " "												}
	}

	CheckBox {
		name: "kruskalEffectSizeEstimates";	label: qsTr("Estimates of effect size")
		columns: 3
		CheckBox { name: "kruskalEpsilon"; label: qsTr("ε²"); checked: true	}
		CheckBox { name: "kruskalEta"; label: qsTr("η²")		}
		CIField {name: "kruskalCiLevel"; label: qsTr("Confidence") }
	}
}
