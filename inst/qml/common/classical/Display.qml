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
import "../." as Common


Group
{
	property int analysis
	title: qsTr("Display")
	CheckBox { name: "descriptives";	label: qsTr("Descriptive statistics")	}
	CheckBox {
		name: "effectSizeEstimates";	label: qsTr("Estimates of effect size")
		columns: 3
		CheckBox { name: "effectSizeEtaSquared";		label: qsTr("η²"); checked: true	}
		CheckBox { name: "effectSizePartialEtaSquared";	label: qsTr("partial η²")		}
		Loader
		{
			Component
			{
				id: effectSizePartialGeneral
				CheckBox { name: "effectSizePartialEtaGeneral";	label: qsTr("general η²")	}
			}
			sourceComponent: effectSizePartialGeneral
			active: analysis === Common.Type.Analysis.RMANOVA
		}

		CheckBox { name: "effectSizeOmegaSquared";		label: qsTr("ω²")				}
	}
	CheckBox { name: "VovkSellkeMPR"; label: qsTr("Vovk-Sellke maximum p-ratio") }
}
