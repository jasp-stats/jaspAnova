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


Group
{
	property int analysis
	title: qsTr("Display")
	CheckBox { name: "descriptives";	label: qsTr("Descriptive statistics"); info: qsTr("When this option is selected, the mean, standard deviation, and the sample size will be displayed for each level combination of the independent variables.")	}
	CheckBox {
		name: "effectSizeEstimates";	label: qsTr("Estimates of effect size"); info: qsTr("By selecting this option, the specific types of calculations to estimate the effect size can be specified.")
		Group
		{
			columns: 2
			CheckBox { name: "effectSizeOmegaSquared";		label: qsTr("ω²")	; info: qsTr("Omega squared (ω²) is a less biased estimate of the proportion of variance accounted for by the effect, compared to η² (Kroes & Finley, 2023). Computed using the effectsize R package.") ;checked: true			}
			CheckBox { name: "effectSizePartialOmegaSquared";		label: qsTr("partial ω²"); info: qsTr("Partial omega squared (ω²p) estimates the effect size after removing variance from other factors in the model. In repeated-measures designs, the denominator retains variance due to individual differences between subjects, following Kroes & Finley (2023). For single-factor designs, partial ω² equals standard ω². Computed using the effectsize R package.")		}
CheckBox { name: "effectSizeEtaSquared";		label: qsTr("η²"); info: qsTr("Eta squared (η²) is the proportion of total variance accounted for by the effect: SS_effect / SS_total. In repeated measures designs, SS_total includes between-subjects variability (Olejnik & Algina, 2003). Computed using the effectsize R package.")  }
			CheckBox { name: "effectSizePartialEtaSquared";	label: qsTr("partial η²"); info: qsTr("Partial eta squared (η²p) is the proportion of variance accounted for by the effect after excluding variance from other effects: SS_effect / (SS_effect + SS_error). In repeated measures designs, SS_error is the effect-specific error term. Computed using the effectsize R package.")}
		}

		CheckBox
		{
			name: "effectSizeGeneralEtaSquared"
			label: qsTr("general η²")
			visible: analysis === Common.Type.Analysis.RMANOVA
			info: qsTr("Generalized eta squared (η²G) includes variance from measured factors (e.g., subjects) in the denominator, but not from manipulated factors. This makes it comparable across between-subjects and within-subjects designs (Olejnik & Algina, 2003; Bakeman, 2005). Computed using the afex R package.")
		}


		CheckBox
		{
			name: "effectSizeCi"; label: qsTr("Confidence intervals"); info: qsTr("Displays confidence intervals for the effect size. Set at 95% by default but can be changed into the desired percentage.")
			CIField {	name: "effectSizeCiLevel" }
			childrenOnSameRow: true
			visible: analysis === Common.Type.Analysis.ANOVA ||  analysis === Common.Type.Analysis.ANCOVA

		}
	}
	CheckBox { name: "vovkSellke"; label: qsTr("Vovk-Sellke maximum p-ratio"); info: qsTr("Shows the maximum ratio of the likelihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0") }
}
