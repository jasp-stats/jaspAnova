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

Group
{
	property bool matchedModelsEnabled: true
	columns: 2

	BayesFactorType { }

	Group
	{
		title: qsTr("Tables")
		CheckBox
		{
			name: "effects"; label: qsTr("Effects");
			RadioButtonGroup
			{
				name: "effectsType"
				RadioButton { value: "allModels";		label: qsTr("Across all models");		checked: true					}
				RadioButton { value: "matchedModels";	label: qsTr("Across matched models");	enabled: matchedModelsEnabled	}
			}
		}
		CheckBox { name: "posteriorEstimates";	label: qsTr("Estimates") }
		CheckBox { name: "criTable";			label: qsTr("Model averaged R\u00B2") }
		CheckBox { name: "descriptives";		label: qsTr("Descriptives") }
		CIField {  name: "credibleInterval";	label: qsTr("Credible interval") }
	}

	RadioButtonGroup
	{
		title: qsTr("Order")
		name: "bayesFactorOrder"
		RadioButton { value: "bestModelTop"; label: qsTr("Compare to best model"); checked: true	}
		RadioButton { value: "nullModelTop"; label: qsTr("Compare to null model")					}
	}

	RadioButtonGroup
	{
		name: "modelsShown"
		title: qsTr("Limit No. Models Shown")
		RadioButton { value: "unlimited"; label: qsTr("No") }
		RadioButton {
			value: "limited"
			label: qsTr("Yes, show best")
			checked: true
			childrenOnSameRow: true
			IntegerField { name: "numModelsShown"; defaultValue: 10; min: 1}
		}
	}

	GroupBox
	{
		title: qsTr("Plots")
		CheckBox {
			label: qsTr("Model averaged posteriors");		name: "modelAveragedPosteriorPlot"
			RadioButtonGroup
			{
				name: "groupPosterior"
				RadioButton { value: "grouped";		label: qsTr("Group levels in single plot"); checked: true}
				RadioButton { value: "individual";	label: qsTr("Individual plot per level")				 }
			}
		}
		CheckBox { label: qsTr("Q-Q plot of residuals") ;		name: "qqPlot" }
		CheckBox { label: qsTr("Posterior R\u00B2") ;			name: "rsqPlot"}
	}

}
