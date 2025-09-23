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
	title: qsTr("Single Model Inference"); info: qsTr("Here, a single model can be specified to obtain information about the posterior of this specific model, including a table with the posterior summary and plots of the marginal posterior.")
	property alias source: components2.source

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { id: components2; name: "components2"; title: qsTr("Components"); info: qsTr("This box contains all the factors included in the model.")}
		AssignedVariablesList
		{
			title: qsTr("Specific Model Terms"); info: qsTr("Select the factors that should be included in the model.")
			name: "singleModelTerms"
			listViewType: JASP.Interaction
		}
	}

	GridLayout
	{

		Group
		{
			title: qsTr("Tables")
			CheckBox { label: qsTr("Estimates"); info: qsTr("A table with the posterior summary for the single model, specified in the assignment box, will be displayed. This table provides information about the single model posterior mean, the standard deviation, and the credible interval for each level of the fixed factors and their interaction included in the model. This is different from the estimate option in Output, since the estimates option provides the posterior summary averaged over all the models included in the analysis, while this option gives the posterior summary for the single specified model only.") ;name: "singleModelEstimates"}
			CheckBox { label: qsTr("R\u00B2"); info: qsTr("Displays a table with the mean and credible interval for the model R², meaning the proportion of variance in the outcome variable explained by the specified predictor.") ; name: "singleModelCriTable" }
		}

		Group
		{
			title: qsTr("Plots")
			CheckBox {
				label: qsTr("Marginal posteriors");	info: qsTr("By selecting this option, plots illustrating the posterior distribution of each fixed factor and interaction included in the single model will be generated.")	;name: "singleModelPosteriorPlot"
				RadioButtonGroup
				{
					name: "singleModelGroupPosterior"
					RadioButton { value: "grouped";		label: qsTr("Group levels in single plot"); info: qsTr("When this option is selected, one plot for each factor will be displayed. Therefore, the posterior distribution of each level of the factor will be shown in the same plot.") ;checked: true}
					RadioButton { value: "individual";	label: qsTr("Individual plot per level"); info: qsTr("When this option is selected, a plot for each level of the factors will be displayed. Therefore, the posterior distribution of each level of the factor will be shown in a different plot.")				 }
				}
			}
			CheckBox { label: qsTr("Q-Q plot of residuals"); info: qsTr("Checks the validity of the distributional assumption of the data set. Specifically, the plot shows whether the residuals are normally distributed. Systematic deviations from the straight line indicate that the residuals might not be normally distributed.")	;name: "singleModelQqPlot" }
			CheckBox { label: qsTr("Posterior R\u00B2") ; info: qsTr("By selecting this option, a plot of the posterior distribution of the R2 (i.e., explained variance) will be displayed.")	;	name: "singleModelRsqPlot"}
		}

	}
}
