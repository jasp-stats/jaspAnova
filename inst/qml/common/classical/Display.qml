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
			CheckBox { name: "effectSizeOmegaSquared";		label: qsTr("ω²")	; info: qsTr("Omega squared is calculated as an estimate of the effect size. This is considered a less biased estimate of the effect size, compared to η2 . (Kroes & Finley, 2023).") ;checked: true			}
			CheckBox { name: "effectSizePartialOmegaSquared";		label: qsTr("partial ω²"); info: qsTr("Partial Omega squared is calculated as an estimate of the effect size. Partial ω2 measures the effect size of the predictor in the context of multiple factors or covariates, isolating its unique contribution.")		}
			CheckBox { name: "effectSizeEtaSquared";		label: qsTr("η²"); info: qsTr("Eta-squared is calculated as an estimate of the effect size. However, this method is considered to overestimate the population variance, making it hard to compare the effect of the same variable across different studies (Goss-Sampson, 2018; Kroes & Finley, 2023).")	}
			CheckBox { name: "effectSizePartialEtaSquared";	label: qsTr("partial η²"); info: qsTr("Partial eta-squared is calculated as an estimate of the effect size. Partial η2 measures the effect size of the predictor in the context of multiple factors or covariates, isolating its unique contribution.")}
		}

		CheckBox
		{
			name: "effectSizeGeneralEtaSquared"
			label: qsTr("general η²")
			visible: analysis === Common.Type.Analysis.RMANOVA
		}


		CheckBox
		{
			name: "effectSizeCi"; label: qsTr("Confidence intervals"); info: qsTr("Displays confidence inetrvals for the effect size. SEt at 95% by default but can be changed into the desired percentage.")
			CIField {	name: "effectSizeCiLevel" }
			childrenOnSameRow: true
			visible: analysis === Common.Type.Analysis.ANOVA ||  analysis === Common.Type.Analysis.ANCOVA

		}
	}
	CheckBox { name: "vovkSellke"; label: qsTr("Vovk-Sellke maximum p-ratio"); info: qsTr("Shows the maximum ratio of the lieklihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0") }
}
