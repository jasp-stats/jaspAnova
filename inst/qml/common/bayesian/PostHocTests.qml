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

Section
{
	title: qsTr("Post Hoc Tests"); info: qsTr("To perform a post hoc test, drag the factor name to perform the post hoc test on to the right column.")
	property alias source: postHocTestsAvailable.source

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "postHocAvailableTerms"; id: postHocTestsAvailable }
		AssignedVariablesList {  name: "postHocTerms" }
	}

	Group
	{
		title: qsTr("Correction")
		CheckBox { name: "postHocNullControl"; label: qsTr("Null control"); info: qsTr("When selecting this option, the prior odds will be corrected for multiple testing. This option is selected by default. At the moment, no output will be generated for the post hoc test when this option is not selected.") ;checked: true }
	}
}
