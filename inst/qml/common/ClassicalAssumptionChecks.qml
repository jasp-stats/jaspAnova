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

import QtQuick	2.15
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import "./" as Common


Section
{
	title: qsTr("Assumption Checks")

	Group
	{
		CheckBox { name: "homogeneityTests";	label: qsTr("Homogeneity tests")			}
		Group
		{
			title: qsTr("Homogeneity corrections")
			columns: 3
			CheckBox { name: "homogeneityNone";		label: qsTr("None")           ; checked: true }
			CheckBox { name: "homogeneityBrown";	label: qsTr("Brown-Forsythe") ; checked: false }
			CheckBox { name: "homogeneityWelch";	label: qsTr("Welch")          ; checked: false }
		}
		CheckBox { name: "qqPlot"; label: qsTr("Q-Q plot of residuals") }
	}
}
