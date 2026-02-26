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
import "../." as Common


Section
{
	title:		qsTr("Assumption Checks")
	columns:	1
	property int analysis

	CheckBox { name: "homogeneityTests";	label: qsTr("Homogeneity tests"); info: qsTr("By selecting this option, it will be checked whether the variance of the dependent variable is equal between the groups by performing Levene's test of equal variances.")			}
	Group
	{
		visible: analysis === Common.Type.Analysis.ANOVA
		height: visible ? implicitHeight : 0

		title: qsTr("Homogeneity corrections"); info: qsTr("If the assumption of homogeneity is not met, corrections can be selected.")
		columns: 3
		CheckBox { name: "homogeneityCorrectionNone";		label: qsTr("None")  ; info: qsTr("No homogeneity correction.")        ; checked: true }
		CheckBox { name: "homogeneityCorrectionBrown";		label: qsTr("Brown-Forsythe"); info: qsTr(" If the homogeneity assumption is not met, this correction could be used. This correction is only available for one-way ANOVA.") ; checked: false }
		CheckBox { name: "homogeneityCorrectionWelch";		label: qsTr("Welch")    ; info: qsTr("If the homogeneity assumption is not met, this correction could be used. This correction is only available for one-way ANOVA.")      ; checked: false }
	}
	CheckBox 
	{ 
		name: "qqPlot";		 	label: qsTr("Q-Q plot residuals"); info: qsTr("Displays Q-Q plot of the standardized residuals. The confidence band shows the expected range of residuals under normality; points outside the band suggest deviations from normality.") 
		CheckBox
		{
			name:               "qqPlotCi"
			label:              qsTr("Confidence interval")
			childrenOnSameRow:  true
			CIField{ name: "qqPlotCiLevel" }
		}		
	}
}
