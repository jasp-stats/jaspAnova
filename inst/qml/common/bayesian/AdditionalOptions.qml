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
import QtQuick.Layouts
import JASP
import JASP.Controls
import "../." as Common

Section
{

				property int	analysis
				property var	covariates:				null
	readonly	property alias	marginalityEnforced:	fixedMarginality.checked

	title: qsTr("Additional Options")
	columns: 1

	Group
	{
		columns: 2
		Group
		{
			RadioButtonGroup
			{
				title: qsTr("Specify Prior on Coefficients"); info: qsTr("Prior: Here it is possible to set the prior distributions for the fixed and random effect sizes.")
				name: "priorSpecificationMode"
				RadioButton {	value: "acrossParameters";	label: qsTr("For fixed and random terms"); info: qsTr("Allows specifying the same prior for all the coefficients at once")	;		checked: true;	id: priorSpecificationAcrossParameters	}
				RadioButton	{	value: "perTerm";			label: qsTr("For each term individually");		info: qsTr("Allows specifying the prior of each coefficient individually")															}
			}
			Group
			{
				columns: 1
				title: qsTr("Coefficient Prior");  info: qsTr("Allows specifying the width of the fixed and random coefficient priors. Set at 0.5 and 1 by default respectively. They can be changed into the desired values.")
				DoubleField {													name: "cauchyPriorScaleFixedEffects";	label: qsTr("r scale fixed effects") ;defaultValue: 0.5;		max: 2;		inclusive: JASP.MaxOnly;	decimals: 3;	enabled: priorSpecificationAcrossParameters.checked	}
				DoubleField {													name: "cauchyPriorScaleRandomEffects";	label: qsTr("r scale random effects"); defaultValue: 1;		max: 2;		inclusive: JASP.MaxOnly;	decimals: 3;	enabled: priorSpecificationAcrossParameters.checked	}
				DoubleField { visible: analysis !== Common.Type.Analysis.ANOVA;	name: "cauchyPriorScaleCovariates";		label: qsTr("r scale covariates")	;	defaultValue: 0.354;	max: 2;		inclusive: JASP.MaxOnly;	decimals: 3;														}
			}
		}

		RadioButtonGroup
		{
			name: "integrationMethod"
			title: qsTr("Integration Method"); info: qsTr("Specify how the marginal likelihoods should be approximated.")
			RadioButton	{ value: "automatic";	label: qsTr("Automatic"); info: qsTr("Numerical integration is used to approximate the marginal likelihood")	;		checked: true	; id: integrationMethodAutomatic	}
			RadioButton	{ value: "laplace";		label: qsTr("Laplace approximation");	info: qsTr("Laplace approximation of the marginal likelihood. Works better for large sample sizes and very large models.")													}
		}

		RadioButtonGroup
		{
			enabled: integrationMethodAutomatic.checked
			name: "samplingMethodNumericAccuracy"
			title: qsTr("Numerical Accuracy")
			RadioButton { value: "auto";	label: qsTr("Auto"); info: qsTr("If this option is selected, 10000 steps will be used. This option is selected by default.") ; checked: true }
			RadioButton
			{
				value: "manual";	label: qsTr("Manual"); info: qsTr("If this option is selected, the number of steps can be manually specified. The number of steps is set to 10000 by default.")
				IntegerField
				{
					name: "samplesNumericAccuracy"
					label: qsTr("No. samples")
					defaultValue: 1e4
					fieldWidth: 50
					min: 100
					max: 1e7
				}
			}
		}

		RadioButtonGroup
		{
			name: "samplingMethodMCMC"
			title: qsTr("Posterior Samples"); info: qsTr("It is possible to set the number of Markov Chain Monte Carlo samples, used to approximate the posterior distribution and error %.")
			RadioButton { value: "auto";	label: qsTr("Auto"); info: qsTr("If this option is selected, 10000 samples will be used. This option is selected by default.") ;checked: true }
			RadioButton
			{
				value: "manual";	label: qsTr("Manual"); info: qsTr("If this option is selected, the number of samples can be specified manually. When selecting this option a sample size of 1000 is used by default.")
				IntegerField
				{
					name: "samplesMCMC"
					label: qsTr("No. samples")
					defaultValue: 1e3
					fieldWidth: 50
					min: 100
					max: 1e7
				}
			}
		}

		Group
		{
			CheckBox
			{
				visible: analysis === Common.Type.Analysis.RMANOVA
				name:	"legacyResults"
				label:	qsTr("Legacy results")
				info:	qsTr("When checked, the random slopes of repeated measures factors are omitted as in JASP <=0.16. Omitting the random slopes may yield completely different results from the frequentist ANOVA.")
			}

			CheckBox
			{
				name:		"hideNuisanceParameters"
				checked:	true
				label:		qsTr("Hide nuisance in model")
				info:		qsTr("When checked, the nuisance parameters common to all models are omitted from the model specification.")
			}
		}

		Group
		{
			title: qsTr("Enforce the Principle of Marginality"); info: qsTr("The principle of marginality states that higher-order interaction terms in a model should only be included if their corresponding lower-order terms (main effects and simpler interactions) are also included. When enforced, an interaction between A and B cannot be added to the model without also adding the main effects of A and B.")
			CheckBox	{	name: "enforcePrincipleOfMarginalityFixedEffects";	label: qsTr("For fixed effects"); info: qsTr("Enforces the principle of marginality for fixed effects(User specified)")	;checked: true;	id: fixedMarginality	}
			CheckBox	{	name: "enforcePrincipleOfMarginalityRandomSlopes";	label: qsTr("For random slopes"); info: qsTr("Enforces the principle of marginality for random slopes (See the 'Legacy results' options)")	;checked: false							}
		}

		SetSeed{}

		RadioButtonGroup
		{
			id: modelPrior
			name: "modelPrior"
			title: qsTr("Model Prior"); info: qsTr("prior distribution of the models")
			RadioButton { value: "uniform"; label: qsTr("Uniform"); info: qsTr("Assumes that all possible values are equally likely.") ;checked: true}
			RadioButton
			{
				value: "betaBinomial"; label: qsTr("Beta binomial"); info: qsTr("Default Beta(a = 1, b = 1).")
				childrenOnSameRow: true
				childrenArea.columnSpacing: 1
				DoubleField { name: "betaBinomialParameterA"; label: qsTr("a"); defaultValue: 1; inclusive: JASP.MaxOnly}
				DoubleField { name: "betaBinomialParameterB"; label: qsTr("b"); defaultValue: 1; inclusive: JASP.MaxOnly}
			}
			RadioButton
			{
				value: "Wilson"
				label: qsTr("Wilson"); info: qsTr("Default lambda = 1. Equivalent to a Beta binomial with a = 1 and b = lambda * p, where p is the number of predictors in the model.")
				childrenOnSameRow: true
				childrenArea.columnSpacing: 1
				DoubleField { name: "wilsonParameterLambda"; label: qsTr("λ"); defaultValue: 1; inclusive: JASP.None; min: 0}
			}
			RadioButton
			{
				value: "Castillo"
				label: qsTr("Castillo"); info: qsTr("Default u = 1. Equivalent to a Beta binomial with a = 1 and b = p^u, where p is the number of predictors in the model.")
				childrenOnSameRow: true
				childrenArea.columnSpacing: 1
				DoubleField { name: "castilloParameterU"; label: qsTr("u"); defaultValue: 1; inclusive: JASP.MinMax; min: 1}
			}
			RadioButton
			{
				value: "Bernoulli"; label: qsTr("Bernoulli"); info: qsTr("Default p = 0.5.")
				childrenOnSameRow: true
				DoubleField { name: "bernoulliParameter"; label: qsTr("p"); defaultValue: 0.5; max: 1; inclusive: JASP.None; decimals: 3 }
			}
			RadioButton
			{
				id: customPriorModelProbabilities
				value: "custom"
				label: qsTr("Custom"); info: qsTr("Allows the user to select the prior inclusion probability for the desired variable")
			}
		}
	}


	ColumnLayout
	{
		id: customPriorLayout
		property int space:		  4 * preferencesModel.uiScale
		property int prefWidth:	100 * preferencesModel.uiScale

		spacing:				0
		Layout.preferredWidth:	parent.width
		visible	:				customPriorModelProbabilities.checked || !priorSpecificationAcrossParameters.checked
		RowLayout
		{
			Row
			{
				Layout.leftMargin:		5   * preferencesModel.uiScale
				Layout.preferredWidth:	332 * preferencesModel.uiScale // perhaps @boutinb knows a better way to determine this value
				Label { text: qsTr("Term")}
			}
			Row
			{
				spacing:				customPriorLayout.space
				Layout.preferredWidth:	121 * preferencesModel.uiScale // perhaps @boutinb knows a better way to determine this value
				Label { text: qsTr("Prior incl. prob.");	visible: customPriorModelProbabilities.checked}
			}
			Row
			{
				spacing:				customPriorLayout.space
				Layout.preferredWidth:	customPriorLayout.prefWidth
				Label { text: qsTr("r-scale");				visible: !priorSpecificationAcrossParameters.checked}
			}
		}

		VariablesList
		{
			id					: customPriorSpecification
			name				: "customPriorSpecification"
			optionKey			: "components"
			source				: [ { name: "modelTerms", condition: "isNuisanceValue == false", conditionVariables: [{ name: "isNuisanceValue", component: "isNuisance", property: "checked"}] }]
			listViewType		: JASP.AssignedVariables
			draggable			: false
			preferredHeight		: jaspTheme.smallDefaultVariablesFormHeight

			rowComponent: RowLayout
			{
				Row
				{
					spacing:				customPriorLayout.space
					Layout.preferredWidth:	customPriorLayout.prefWidth
					DoubleField
					{
						name:			"inclusionProbability"
						min:			0
						max:			100
						defaultValue:	0.5
						inclusive:		JASP.None
						visible:		customPriorModelProbabilities.checked
					}
				}
				Row
				{
					spacing:				customPriorLayout.space
					Layout.preferredWidth:	customPriorLayout.prefWidth
					DoubleField
					{
						name:			"scaleFixedEffects"
						min:			0
						max:			100
						defaultValue:	0.5
						inclusive:		JASP.None
						visible:		!priorSpecificationAcrossParameters.checked && (covariates === null || !rowValue.split(INTERACTION_SEPARATOR).some(elt => { return covariates.columnsNames.includes(elt.trim())}))
					}
				}
			}
		}
	}
}

