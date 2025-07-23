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
	title: qsTr("Post Hoc Tests"); info: qsTr("To perform a post hoc test, drag one or more factor names to the right box. Several options are available:")
	property alias source: availableTerms.source

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList	{	name: "postHocAvailableTerms";	id: availableTerms }
		AssignedVariablesList	{	name: "postHocTerms" }
	}


	Group
	{
		title: qsTr("Type"); info: qsTr("Different types of post hoc tests can be selected.")
		CheckBox
		{
			name: "postHocTypeStandard";	label: qsTr("Standard"); info: qsTr("Pairwise t-tests are performed. All the corrections can be applied to this method. When Tukey's p-value correction is selected, this is equivalent to Tukey's HSD. This option is selected by default.") ;checked: true
			Group
			{
				CheckBox
				{
					name: "postHocTypeStandardBootstrap"; label: qsTr("From"); info: qsTr("By selecting this option, the bootstrapped post hoc test is applied. By default, the number of replications is set to 1000. This can be changed into the desired number.")
					childrenOnSameRow: true
					IntegerField
					{
						name: "postHocTypeStandardBootstrapSamples"
						defaultValue: 1000
						fieldWidth: 50
						min: 100
						afterLabel: qsTr("bootstraps")
					}
				}
			}
			CheckBox { name: "postHocTypeStandardEffectSize";	label: qsTr("Effect size"); info: qsTr("By selecting this option, the effect size (i.e., the magnitude of the observed effect) will be displayed. The used measure for the effect size is Cohen's d. The effect size will only be displayed for the post hoc type Standard.") }
			CheckBox { name: "postHocConditionalTable";	label: qsTr("Conditional comparisons for interactions"); info: qsTr("Instead of pairwise comparisons for all possible combination of cells in the interaction, list pairwise comparisons conditional on each of the interaction terms. This provides as many tables as there are terms in the interaction effect.") }
		}
		CheckBox { name: "postHocTypeGames";		label: qsTr("Games-Howell"); info: qsTr("This method can be used when equal group variances cannot be assumed. The p-values are adjusted using the Tukey method.")				}
		CheckBox { name: "postHocTypeDunnet";		label: qsTr("Dunnett"); info: qsTr("With this method, all levels are compared to the first occurring level in the data. To change which level is used as the reference group, adjust the order of the level labels in the Variable Settings.")					}
	}


	Group
	{
		title: qsTr("Correction"); info: qsTr("To correct for multiple comparisons and avoid Type I errors, various methods are available for adjusting the p-value and confidence interval for mean differences. Note: confidence intervals for effect sizes can only be adjusted using the Bonferroni method.")
		CheckBox { name: "postHocCorrectionTukey";			label: qsTr("Tukey"); info: qsTr("Compare all possible pairs of group means. This correction can be used when the groups of the independent variable have an equal sample size and variance. This method is commonly used and is selected by default.") ;checked: true	}
		CheckBox { name: "postHocCorrectionScheffe";		label: qsTr("Scheffé"); info: qsTr("Adjusting significance levels in a linear regression, to account for multiple comparisons. This method is considered to be quite conservative.")				}
		CheckBox { name: "postHocCorrectionBonferroni";		label: qsTr("Bonferroni"); info: qsTr("This correction is considered conservative. The risk of Type I error is reduced, however the statistical power decreases as well.")			}
		CheckBox { name: "postHocCorrectionHolm";			label: qsTr("Holm")	; info: qsTr("This method is also called sequential Bonferroni, and considered less conservative than the Bonferroni method.")				}
		CheckBox { name: "postHocCorrectionSidak";			label: qsTr("Šidák"); info: qsTr("This method is considered less conservative than Bonferroni but still maintains statistical power. It is usually used if there is not a big amount of tests to be performed.")				}
	}

	Classical.PostHocDisplay{}
}
