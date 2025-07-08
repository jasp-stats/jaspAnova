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
	title: qsTr("Model")
	property alias source: components.source

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "components"; title: qsTr("Components"); info: qsTr("All the independent variables and covariates that can be included in the model.") ;id: components }
		AssignedVariablesList {  name: "modelTerms"; title: qsTr("Model Terms"); info: qsTr("The independent variables and covariates included in the model. By default, all the main effects and interaction effects of factor variables, and the main effects of covariates are included in the model.") ; listViewType: JASP.Interaction }
	}

	Classical.SumOfSquares{}
}
