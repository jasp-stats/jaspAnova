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
			name: "effects"; label: qsTr("Effects"); info: qsTr("By selecting this option, the inclusion probability of each component (i.e., model term) in the models will be calculated. The inclusion probability is the probability that a fixed factor is included in the model given the observed data. It also calculates the BF inclusion, which are the odds of obtaining the observed data under models with the predictor vs models without it.")
			RadioButtonGroup
			{
				name: "effectsType"
				RadioButton { value: "allModels";		label: qsTr("Across all models"); info: qsTr("When this option is selected, each model where the component is included will be used to estimate the effect (i.e., inclusion probability) of the component. When the option Effects is selected, this method is used by default.")	;	checked: true					}
				RadioButton { value: "matchedModels";	label: qsTr("Across matched models"); info: qsTr(" When this option is selected, only models with exactly that component will be included in the analysis. Therefore, interactions with the component are excluded. Compares models that include the component to equivalent models excluding the component.")	;enabled: matchedModelsEnabled	}
			}
		}
		CheckBox { name: "posteriorEstimates";	label: qsTr("Estimates"); info: qsTr("By selecting this option, a table with the model averaged posterior summary will be displayed. This table includes information about the model averaged posterior mean, the standard deviation, and the credible interval for each level of the fixed factors and their interactions.") }
		CheckBox { name: "criTable";			label: qsTr("Model averaged R\u00B2"); info: qsTr("Displays a table with the mean and credible interval of the averaged R², meaning the proportion of variance in the outcome variable explained by the predictors, which are based on the model averaged posterior distribution.") }
		CheckBox { name: "descriptives";		label: qsTr("Descriptives"); info: qsTr("When this option is selected, the mean, standard deviation, and the sample size will be displayed for each level combination of the independent variable. Moreover, it displays the credible interval, which refers to the interval in which the true value of the mean lies based on a certain probability. By default this is set to 95%.") }
		CIField {  name: "credibleInterval";	label: qsTr("Credible interval") }
	}

	RadioButtonGroup
	{
		title: qsTr("Order"); info: qsTr("Compares each model against the model selected here")
		name: "bayesFactorOrder"
		RadioButton { value: "bestModelTop"; label: qsTr("Compare to best model"); info: qsTr("All models are compared to the best model") ;checked: true	}
		RadioButton { value: "nullModelTop"; label: qsTr("Compare to null model"); info: qsTr("All models are compared to the null model")					}
	}

	RadioButtonGroup
	{
		name: "modelsShown"
		title: qsTr("Limit No. Models Shown"); info: qsTr("Gives the option to limit the number of models being displayed.")
		RadioButton { value: "unlimited"; label: qsTr("No"); info: qsTr("Select this so that there is no limit.") }
		RadioButton {
			value: "limited"
			label: qsTr("Yes, show best"); info: qsTr("Select this to limit the number of shown models, can be limited to a number selected by the user, set at 10 by default.")
			checked: true
			childrenOnSameRow: true
			IntegerField { name: "numModelsShown"; defaultValue: 10; min: 1}
		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox {
			label: qsTr("Model averaged posteriors"); info: qsTr("By selecting this option, plots illustrating the model averaged posterior distribution of each fixed factor and interaction will be displayed.")	;	name: "modelAveragedPosteriorPlot"
			RadioButtonGroup
			{
				name: "groupPosterior"
				RadioButton { value: "grouped";		label: qsTr("Group levels in single plot"); info: qsTr("When this option is selected, one plot for each factor will be displayed. Therefore, the posterior distribution of each level of the factor will be shown in the same plot.") ;checked: true}
				RadioButton { value: "individual";	label: qsTr("Individual plot per level"); info: qsTr("When this option is selected, a plot for each level of the factors will be displayed. Therefore, the posterior distribution of each level of the factor will be shown in a different plot.")				 }
			}
		}
		CheckBox { label: qsTr("Q-Q plot of residuals") ; info: qsTr("Checks the validity of the distributional assumption of the data set. Specifically, the plot shows whether the residuals are normally distributed. Systematic deviations from the straight line indicate that the residuals might not be normally distributed.")	;	name: "qqPlot" }
		CheckBox { label: qsTr("Posterior R\u00B2") ;	info: qsTr("By selecting this option, a plot of the posterior distribution of the R2 (i.e., explained variance) will be displayed.")	;	name: "rsqPlot"}
	}

}
