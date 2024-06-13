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
import "./common" as Common
import "./common/classical" as Classical

Form
{
	id: form
	property int analysis:	Common.Type.Analysis.ANCOVA
	property int framework:	Common.Type.Framework.Classical

	Classical.InvisiblePlotSizes{}

	Formula
	{
		lhs: "dependent"
		rhs: "modelTerms"
		userMustSpecify: ["randomFactors", "covariates"]
	}

	VariablesForm
	{
		preferredHeight:	400 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";		title: qsTr("Dependent Variable");	allowedColumns: ["scale"];				singleVariable: true	}
		AssignedVariablesList	{ name: "fixedFactors";		title: qsTr("Fixed Factors");		allowedColumns: ["nominal"]							}
		AssignedVariablesList	{ name: "randomFactors";	title: qsTr("Random Factors");		allowedColumns: ["nominal"];	debug: true				}
		AssignedVariablesList	{ name: "covariates";		title: qsTr("Covariates");			allowedColumns: ["scale"]											}
		AssignedVariablesList	{ name: "wlsWeights";		title: qsTr("WLS Weights");			allowedColumns: ["scale"];				singleVariable: true	}
	}

	Classical.Display
	{
		analysis: form.analysis
	}

	Classical.Models
	{
		source: ["fixedFactors", "randomFactors", "covariates"]
	}

	Classical.AssumptionChecks
	{
		analysis: form.analysis
		CheckBox { name: "factorCovariateIndependenceCheck";	label: qsTr("Factor covariate independence check");	debug: true	}
	}

	Classical.Contrasts
	{
		analysis:	form.analysis
		source:		[{ name : "modelTerms", discard: "covariates" }]
	}

	Classical.OrderRestrictions
	{
		analysis:	form.analysis
		source:		[{ name: "modelTerms", discard: "covariates" }]
	}

	Classical.PostHoc
	{
		source: [{ name: "modelTerms", discard: "covariates" }]
	}

	Classical.DescriptivePlots
	{
		source: ["fixedFactors", "randomFactors", "covariates"]
	}

	Common.BarPlots
	{
		source: ["fixedFactors", "randomFactors", "covariates"]
		framework:	form.framework
	}
	
	Common.RainCloudPlots
	{
		source: ["fixedFactors", "randomFactors", "covariates"]
	}

	Classical.MarginalMeans
	{
		source: [{ name: "modelTerms", discard: "covariates" }]
	}

	Classical.SimpleMainEffects
	{
		source: ["fixedFactors", "randomFactors"]
	}

	Classical.Nonparametrics
	{
		source: ["fixedFactors", "randomFactors"]
	}

}
