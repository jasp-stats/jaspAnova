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
		AvailableVariablesList	{ name: "kruskalWallisAvailableFactors";	title: qsTr("Kruskal-Wallis Test"); info: qsTr("The Kruskal-Wallis test is a non-parametric, rank-based one-way ANOVA that can be used to compare two or more groups. This test is appropriate when the normality assumption is violated and/or when outliers are present. To conduct the test, move the independent variables from the left box to the right box.")	;id: availableTerms	}
		AssignedVariablesList	{ name: "kruskalWallisFactors";				title: " "												}
	}

	CheckBox 
	{
		name: "kruskalEffectSizeEstimates";	label: qsTr("Estimates of effect size"); info: qsTr("Request effect size estimates for the Kruskal-Wallis test: rank epsilon squared and rank eta squared, including their confidence interval. Based on the effectsize package.")
		columns: 3
		CheckBox { name: "kruskalEpsilon"; label: qsTr("ε²");checked: true	}
		CheckBox { name: "kruskalEta"; label: qsTr("η²")		}
		CIField {name: "kruskalCiLevel"; label: qsTr("Confidence") }
	}

	CheckBox 
	{ 
		name: "postHocTypeDunn"
		label: qsTr("Dunn's post hoc tests"); info: qsTr("This non-parametric follow-up test is used for pairwise comparisons after performing the Kruskal-Wallis test. The p-values are corrected using the Bonferroni and Holm methods.")						
	}

}
