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
	// defaults follow ANOVA + ANCOVA
	property	string	sectionTitle : qsTr("Model")
	property	alias	source: components.source
	property    alias   modelTermsList: modelTermsList

	title: sectionTitle

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { id: components; name: "components"; title: qsTr("Components"); info: qsTr("All the independent variables that can be included in the model.")}
		ModelTermsList { id: modelTermsList}
	}

}
