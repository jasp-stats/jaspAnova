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
import JASP
import JASP.Controls
import "../." as Common


Section
{
	title:		qsTr("Assumption Checks")
	columns:	1
	property int analysis

	CheckBox { name: "homogeneityTests";	label: qsTr("Homogeneity tests")			}
	Loader
	{
		Component
		{
			id: homogeneityCorrections
			Group
			{
				title: qsTr("Homogeneity corrections")
				columns: 3
				CheckBox { name: "homogeneityCorrectionNone";		label: qsTr("None")           ; checked: true }
				CheckBox { name: "homogeneityCorrectionBrown";		label: qsTr("Brown-Forsythe") ; checked: false }
				CheckBox { name: "homogeneityCorrectionWelch";		label: qsTr("Welch")          ; checked: false }
			}
		}
		sourceComponent: analysis === Common.Type.Analysis.ANOVA ? homogeneityCorrections : undefined
	}
	CheckBox { name: "qqPlot"; label: qsTr("Q-Q plot of residuals") }
}
