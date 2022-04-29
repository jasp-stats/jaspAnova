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

import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import "./common" as ANOVA

Form
{

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"																							}
		AssignedVariablesList	{ name: "dependent";		title: qsTr("Dependent Variable");	suggestedColumns: ["scale"]; singleVariable: true	}
		AssignedVariablesList	{ name: "fixedFactors";		title: qsTr("Fixed Factors");		suggestedColumns: ["ordinal", "nominal"]			}
		AssignedVariablesList	{ name: "randomFactors";	title: qsTr("Random Factors");		suggestedColumns: ["ordinal", "nominal"]			}
	}

	ANOVA.DefaultOptions { matchedModelsEnabled: additionalOptions.marginalityEnforced	}

	ANOVA.ModelTerms { source: ["fixedFactors", "randomFactors"] }

	ANOVA.SingleModelInference { source: ["fixedFactors", "randomFactors"] }

	ANOVA.PostHocTests { source: "fixedFactors" }

	ANOVA.DescriptivesPlots { source: "fixedFactors" }

	ANOVA.RainCloudPlots { availableVariableSource: ["fixedFactors", "randomFactors"] }

	ANOVA.AdditionalOptions { analysisType: ANOVA.AnalysisType.AnalysisType.BANOVA; id: additionalOptions }

}
