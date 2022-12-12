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
	title: qsTr("Single Model Inference")
	property alias source: components2.source

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { id: components2; name: "components2"; title: qsTr("Components")}
		AssignedVariablesList
		{
			title: qsTr("Specific Model Terms")
			name: "singleModelTerms"
			listViewType: JASP.Interaction
		}
	}

	GridLayout
	{

		GroupBox
		{
			title: qsTr("Tables")
			CheckBox { label: qsTr("Estimates"); name: "singleModelEstimates"}
			CheckBox { label: qsTr("R\u00B2");   name: "singleModelCriTable" }
		}

		GroupBox
		{
			title: qsTr("Plots")
			CheckBox {
				label: qsTr("Marginal posteriors");		name: "singleModelPosteriorPlot"
				RadioButtonGroup
				{
					name: "singleModelGroupPosterior"
					RadioButton { value: "grouped";		label: qsTr("Group levels in single plot"); checked: true}
					RadioButton { value: "individual";	label: qsTr("Individual plot per level")				 }
				}
			}
			CheckBox { label: qsTr("Q-Q plot of residuals");	name: "singleModelQqPlot" }
			CheckBox { label: qsTr("Posterior R\u00B2") ;		name: "singleModelRsqPlot"}
		}

	}
}
