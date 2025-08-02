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

Form
{
	info: qsTr("MANOVA allows the user to analyze the difference among groups when there are multiple dependent variables") + "\n" +
	"## " + qsTr("Assumptions") + "\n" +
	"- " + qsTr("The dependent variables are normally distributed for every group.") + "\n" +
	"- " + qsTr("The independent variables are categorical, the dependent variables are continuous.") + "\n" +
	"- " + qsTr("The population covariance matrices of each group are equal.") + "\n" +
	"- " + qsTr("The groups are independent.")

	Formula
	{
		lhs: "dependent"
		rhs: "modelTerms"
		userMustSpecify: "randomFactors"
	}
	
	IntegerField { visible: false; name: "plotWidthQQPlot"                      ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightQQPlot"                     ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotLegend"     ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotNoLegend"   ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotLegend"      ; defaultValue: 430 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotNoLegend"    ; defaultValue: 350 }
	
	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "dependent";		title: qsTr("Dependent Variables"); info: qsTr("The variable of interest. Also called the outcome variable.")	;allowedColumns: ["scale"]; singleVariable: false		}
		AssignedVariablesList { name: "fixedFactors";	title: qsTr("Fixed Factors"); info: qsTr("The variables that are manipulated/define the different groups. These are also called the independent variables.")	;	allowedColumns: ["nominal"]; minLevels: 2				}
		AssignedVariablesList { name: "randomFactors";	title: qsTr("Random Factors");		allowedColumns: ["nominal"]; minLevels: 2;	debug: true }
	}
	
	Section
	{
		title: qsTr("Model"); info: qsTr("Components and model terms")
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "components"; title: qsTr("Components"); info: qsTr("All the independent variables that can be included in the model.") ; source: ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  name: "modelTerms"; title: qsTr("Model Terms"); info: qsTr("The independent variables included in the model. By default all the main effects and interaction effects of the specified independent variables are included in the model.") ;listViewType: JASP.Interaction }
		}
		CheckBox { name: "includeIntercept";		label: qsTr("Include intercept"); info: qsTr("display the intercept term in the MANOVA and ANOVA tables.") ;checked: true }
		
	}
	
	
	Section
	{
		title: qsTr("Additional Options")	
		
		Group
		{
			title: qsTr("Test"); info: qsTr("Select the statistical test to be performed for the MANOVA and how the F-ratio is approximated.")
			CheckBox { name: "testPillai";		label: qsTr("Pillai"); info: qsTr("Pillai's trace. Produces a value between 0 and 1. The closer to 1, the more evidence there is of an effect of the independent variable on the dependent variable.") ;checked: true }
			CheckBox { name: "testWilks";		label: qsTr("Wilks"); info: qsTr("Wilks' lambda. This can be interpreted as the proportion of the variance in the outcomes that is not explained by an effect.") ;checked: false }
			CheckBox { name: "testHotellingLawley";	label: qsTr("Hotelling-Lawley"); info: qsTr("Hotelling-Lawley's trace. Measures multivariate separation between group means relative to within group variance. Meaning how well can two groups be distinguished taking all the dependent variables into account.") ;checked: false }
			CheckBox { name: "testRoy";		label: qsTr("Roy"); info: qsTr("Roy's largest root. Measures the largest separation between group means along the most discriminating direction in the multuvariate space.");checked: false }
		}
		
		Group
		{
			title: qsTr("Assumption Checks")
			CheckBox { name: "boxMTest";			label: qsTr("Homogeneity of covariance matrices"); info: qsTr(" Box's M-test for homogeneity of covariance matrices.")}
			CheckBox { name: "shapiroTest";			label: qsTr("Multivariate normality"); info: qsTr("Generalized Shapiro-Wilk test for multivariate normality.")		}
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox { name: "anovaTables"; label: qsTr("ANOVA tables"); info: qsTr("Outputs individual ANOVA tables per dependent variable.") }
			CheckBox { name: "vovkSellke"; label: qsTr("Vovk-Sellke maximum p-ratio"); info: qsTr("Shows the maximum ratio of the likelihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0") }
		}

	
	}
	
}
