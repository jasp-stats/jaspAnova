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

import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Section
{

	property int analysisType

	title: qsTr("Additional Options")
	columns: 1

	Group
	{
		columns: 2
		Group
		{
			columns: 1
			title: qsTr("Prior")
			DoubleField {																name: "priorFixedEffects";	label: qsTr("r scale fixed effects");  defaultValue: 0.5; max: 2; inclusive: JASP.MaxOnly; decimals: 3 }
			DoubleField {																name: "priorRandomEffects";	label: qsTr("r scale random effects"); defaultValue: 1;   max: 2; inclusive: JASP.MaxOnly; decimals: 3 }
			DoubleField { visible: analysisType !== AnalysisType.AnalysisType.BANOVA;	name: "priorCovariates";	label: qsTr("r scale covariates");     defaultValue: 0.354; max: 2; inclusive: JASP.MaxOnly; decimals: 3 }
		}

		RadioButtonGroup
		{
			name: "sampleModeNumAcc"
			title: qsTr("Numerical Accuracy")
			RadioButton { value: "auto";	label: qsTr("Auto"); checked: true }
			RadioButton
			{
				value: "manual";	label: qsTr("Manual")
				IntegerField
				{
					name: "fixedNumAcc"
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
			name: "sampleModeMCMC"
			title: qsTr("Posterior Samples")
			RadioButton { value: "auto";	label: qsTr("Auto"); checked: true }
			RadioButton
			{
				value: "manual";	label: qsTr("Manual")
				IntegerField
				{
					name: "fixedMCMCSamples"
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
				visible: analysisType === AnalysisType.AnalysisType.BRMANOVA
				name:	"legacy"
				label:	qsTr("Legacy results")
				info:	qsTr("When checked, the random slopes of repeated measures factors are omitted as in JASP <=0.16. Omitting the random slopes may yield completely different results from the frequentist ANOVA.")
			}

			CheckBox
			{
				name:		"hideNuisanceEffects"
				checked:	true
				label:		qsTr("Hide nuisance in model")
				info:		qsTr("When checked, the nuisance parameters common to all models are omitted from the model specification.")
			}
		}

		SetSeed{}

		RadioButtonGroup
		{
			id: modelPrior
			name: "modelPrior"
			title: qsTr("Model Prior")
			RadioButton { value: "uniform"; label: qsTr("Uniform"); checked: true}
			RadioButton
			{
				value: "beta.binomial"; label: qsTr("Beta binomial")
				childrenOnSameRow: true
				childrenArea.columnSpacing: 1
				DoubleField { name: "betaBinomialParamA"; label: qsTr("a"); defaultValue: 1; inclusive: JASP.MaxOnly}
				DoubleField { name: "betaBinomialParamB"; label: qsTr("b"); defaultValue: 1; inclusive: JASP.MaxOnly}
			}
			RadioButton
			{
				value: "Wilson"
				label: qsTr("Wilson")
				childrenOnSameRow: true
				childrenArea.columnSpacing: 1
				DoubleField { name: "wilsonParamLambda"; label: qsTr("λ"); defaultValue: 1; inclusive: JASP.None; min: 0}
			}
			RadioButton
			{
				value: "Castillo"
				label: qsTr("Castillo")
				childrenOnSameRow: true
				childrenArea.columnSpacing: 1
				DoubleField { name: "castilloParamU"; label: qsTr("u"); defaultValue: 1; inclusive: JASP.MinMax; min: 1}
			}
			RadioButton
			{
				value: "Bernoulli"; label: qsTr("Bernoulli")
				childrenOnSameRow: true
				DoubleField { name: "bernoulliParam"; label: qsTr("p"); defaultValue: 0.5; max: 1; inclusive: JASP.None; decimals: 3 }
			}
			RadioButton
			{
				id: customPriorModelProbabilities
				value: "custom"
				label: qsTr("Custom")
			}
		}
	}

	Group
	{
		title: qsTr("Enforce the principle of marginality for")
		CheckBox	{	name: "enforcePrincipleOfMarginalityFixedEffects";	label: qsTr("Fixed effects");	checked: true	}
		CheckBox	{	name: "enforcePrincipleOfMarginalityRandomSlopes";	label: qsTr("Random slopes");	checked: false	}
	}

	VariablesList
	{
		name				: "modelTermsCustomPrior"
		optionKey			: "components"
		source				: [ { name: "modelTerms", condition: "isNuisanceValue == false", conditionVariables: [{ name: "isNuisanceValue", component: "isNuisance", property: "checked"}] }]
		visible				: customPriorModelProbabilities.checked
		title				: qsTr("Model Term")
		rowComponentTitle	: qsTr("Prior inclusion probability")
		listViewType		: JASP.AssignedVariables
		draggable			: false

		rowComponent		: DoubleField
		{
			name:			"modelTermsCustomPrior2"
			min:			0
			max:			1
			defaultValue:	0.5
			inclusive:		JASP.None
		}
	}


}

