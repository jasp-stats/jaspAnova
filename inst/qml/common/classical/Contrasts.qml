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
import "../." as Common

Section
{
	title: qsTr("Contrasts")
	info: qsTr("For each independent variable, you can select a specific contrast in the right column by clicking on 'none' and choosing the desired contrast.")
	property int analysis
	property alias source: contrasts.source
	ContrastsList { id: contrasts }

	CheckBox
	{
		isBound: false
		label: qsTr("Pool error term for follow-up tests"); info: qsTr("By selecting this option, the univariate linear model, rather than the multivariate model, will be used for follow-up tests (contrasts, post-hoc tests, marginal means). Caution: multivariate models (i.e., unpooled error terms) handle departures from sphericity better, since these models allow the standard errors to differ for each level of the repeated measure(s) factor(s).")
        checked: analysis === Common.Type.Analysis.RMANOVA && poolErrorTermFollowup.checked
		onCheckedChanged: poolErrorTermFollowup.checked = checked
		visible: analysis === Common.Type.Analysis.RMANOVA
	}
	CheckBox
	{
		name: "contrastCi"; label: qsTr("Confidence intervals"); info: qsTr("By selecting this option, confidence intervals for the estimated mean difference and effect size will be included. By default the confidence level is set to 95%. This can be changed into the desired percentage.")
		childrenOnSameRow: true
		CIField {	name: "contrastCiLevel" }
	}
	CheckBox
	{
		name: "contrastEffectSize"; label: qsTr("Effect size (Cohen's d)"); info: qsTr("Include standardized mean differences, based on the effectsize function in the emmeans package.")
	}
}
