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
import "./" as Classical


Section
{
	title: qsTr("Post Hoc Tests")
	property alias source: availableTerms.source

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList	{	name: "postHocAvailableTerms";	id: availableTerms }
		AssignedVariablesList	{	name: "postHocTerms" }
	}


	Group
	{
		title: qsTr("Type")
		CheckBox
		{
			name: "postHocTypeStandard";	label: qsTr("Standard"); checked: true
			Group
			{
				CheckBox
				{
					name: "postHocTypeStandardBootstrap"; label: qsTr("From")
					childrenOnSameRow: true
					IntegerField
					{
						name: "postHocTypeStandardBootstrapSamples"
						defaultValue: 1000
						fieldWidth: 50
						min: 100
						afterLabel: qsTr("bootstraps")
					}
				}
			}
			CheckBox { name: "postHocTypeStandardEffectSize";	label: qsTr("Effect size") }
			CheckBox { name: "postHocConditionalTable";	label: qsTr("Conditional comparisons for interactions") }
		}
		CheckBox { name: "postHocTypeGames";		label: qsTr("Games-Howell")				}
		CheckBox { name: "postHocTypeDunnet";		label: qsTr("Dunnett")					}
	}


	Group
	{
		title: qsTr("Correction")
		CheckBox { name: "postHocCorrectionTukey";			label: qsTr("Tukey"); checked: true	}
		CheckBox { name: "postHocCorrectionScheffe";		label: qsTr("Scheffé")				}
		CheckBox { name: "postHocCorrectionBonferroni";		label: qsTr("Bonferroni")			}
		CheckBox { name: "postHocCorrectionHolm";			label: qsTr("Holm")					}
		CheckBox { name: "postHocCorrectionSidak";			label: qsTr("Šidák")				}
	}

	Classical.PostHocDisplay{}
}
