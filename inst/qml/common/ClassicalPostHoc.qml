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
	title: qsTr("Post Hoc Tests")
	property alias source: availableTerms.source

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "postHocTestsAvailable"; id: availableTerms }
		AssignedVariablesList {  name: "postHocTestsVariables" }
	}


	Group
	{
		title: qsTr("Type")
		CheckBox
		{
			name: "postHocTestsTypeStandard";	label: qsTr("Standard"); checked: true
			Group
			{
				CheckBox
				{
					name: "postHocTestsBootstrapping"; label: qsTr("From")
					childrenOnSameRow: true
					IntegerField
					{
						name: "postHocTestsBootstrappingReplicates"
						defaultValue: 1000
						fieldWidth: 50
						min: 100
						afterLabel: qsTr("bootstraps")
					}
				}
			}
			CheckBox { name: "postHocTestEffectSize";	label: qsTr("Effect size") }
		}
		CheckBox { name: "postHocTestsTypeGames";		label: qsTr("Games-Howell")				}
		CheckBox { name: "postHocTestsTypeDunnett";		label: qsTr("Dunnett")					}
		CheckBox { name: "postHocTestsTypeDunn";		label: qsTr("Dunn")						}
	}


	Group
	{
		title: qsTr("Correction")
		CheckBox { name: "postHocTestsTukey";		label: qsTr("Tukey"); checked: true	}
		CheckBox { name: "postHocTestsScheffe";		label: qsTr("Scheffé")				}
		CheckBox { name: "postHocTestsBonferroni";	label: qsTr("Bonferroni")			}
		CheckBox { name: "postHocTestsHolm";		label: qsTr("Holm")					}
		CheckBox { name: "postHocTestsSidak";       label: qsTr("Šidák")                }
	}

	Common.ClassicalPostHocDisplay{}
}
