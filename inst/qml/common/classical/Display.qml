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


Group
{
	property int analysis
	title: qsTr("Display")
	CheckBox { name: "descriptives";	label: qsTr("Descriptive statistics")	}
	CheckBox {
		name: "effectSizeEstimates";	label: qsTr("Estimates of effect size")
		columns: 4
		CheckBox { name: "effectSizeEtaSquared";		label: qsTr("η²")	}
		CheckBox { name: "effectSizePartialEtaSquared";	label: qsTr("partial η²")	; checked: true	}
		Loader
		{
			Component
			{
				id: effectSizeGeneralEtaSquared
				CheckBox { name: "effectSizeGeneralEtaSquared";	label: qsTr("general η²")	}
			}
			sourceComponent: effectSizeGeneralEtaSquared
			active: analysis === Common.Type.Analysis.RMANOVA
		}

		CheckBox { name: "effectSizeOmegaSquared";		label: qsTr("ω²")				}			
		CheckBox
		{
			name: "effectSizeCi"; label: qsTr("Confidence intervals")
			CIField {	name: "effectSizeCiLevel" }
			childrenOnSameRow: true
			visible: analysis === Common.Type.Analysis.RMANOVA
		}
	}
	CheckBox { name: "vovkSellke"; label: qsTr("Vovk-Sellke maximum p-ratio") }
}
