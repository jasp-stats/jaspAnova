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

Section
{
	title: qsTr("Nonparametrics")
	property alias source: availableTerms.source

	VariablesForm
	{
		preferredHeight:	170 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "kruskalWallisAvailableFactors";	title: qsTr("Kruskal-Wallis Test"); info: qsTr("The Kruskal-Wallis test is a non-parametric ANOVA and can be used to compare two or more groups. This test is a rank-based one-way ANOVA. The Kruskal-Wallis test can be performed when one of the following assumptions is not met: normality of the dependent variable, no outliers, homogeneity of the variance between the groups. To perform the test, move the independent variables from the left column to the right column.")	;id: availableTerms	}
		AssignedVariablesList	{ name: "kruskalWallisFactors";				title: " "												}
	}

	CheckBox 
	{
		name: "kruskalEffectSizeEstimates";	label: qsTr("Estimates of effect size"); info: qsTr("request effect size estimates for the Kruskal-Wallis test: rank epsilon squared and rank eta squared, including their confidence interval. Based on the effectsize package.")
		columns: 3
		CheckBox { name: "kruskalEpsilon"; label: qsTr("ε²");checked: true	}
		CheckBox { name: "kruskalEta"; label: qsTr("η²")		}
		CIField {name: "kruskalCiLevel"; label: qsTr("Confidence") }
	}

	CheckBox 
	{ 
		name: "postHocTypeDunn"
		label: qsTr("Dunn's post hoc tests"); info: qsTr("This is a non-parametric follow-up test that can be used for testing small subsets of pairs. This post hoc test is a follow up for the Kruskal-Wallis test. The p-values are corrected with the Bonferroni and Holm methods.")						
	}

}
