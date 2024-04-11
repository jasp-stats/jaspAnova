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
import "../." as Common

Section
{
	title: qsTr("Contrasts")
	property int analysis
	property alias source: contrasts.source

	ContrastsList { id: contrasts }

	CheckBox
	{
		isBound: false
		label: qsTr("Pool error term for follow-up tests")
		checked: poolErrorTermFollowup.checked
		onCheckedChanged: poolErrorTermFollowup.checked = checked
		visible: analysis === Common.Type.Analysis.RMANOVA
	}
	CheckBox
	{
		name: "contrastCi"; label: qsTr("Confidence intervals")
		childrenOnSameRow: true
		CIField {	name: "contrastCiLevel" }
	}
	CheckBox
	{
		name: "contrastEffectSize"; label: qsTr("Effect size")
	}
}
