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

DropDown
{
	name: "sumOfSquares"
	indexDefaultValue: 2
	label: qsTr("Sum of squares")
	info: qsTr(" There are different types of the sum of squares. The choice of the type is important when there are multiple factors and when the data are unbalanced. In an unbalanced design, the different levels of the independent variable do not contain an equal number of observations (e.g., one group contains more observations than another group). In this scenario, the sum of squares type can influence the results.")
	values: [
		{ label: qsTr("Type %1").arg("\u2160"), info: qsTr("Sequential sum of squares. It is the reduction of error when each factor of the model is added to the factors already included, preserving the order of factors in the model. The results depend on the order in which the factors are added to the model. This is important to consider when the model contains more than one factor.")	, value: "type1"},
		{ label: qsTr("Type %1").arg("\u2161"), info: qsTr("Hierarchical/partially sequential sum of squares. It is the reduction of error when each factor is added to the model that includes all the other factors, except the factors where the added factor is a part of, such as interactions containing that factor. Langsrud (2003) advises to apply this type for an ANOVA with unbalanced data.")	, value: "type2"},
		{ label: qsTr("Type %1").arg("\u2162"), info: qsTr("Partial sum of squares. It is the reduction of error when each factor is added to the model that includes all the other factors, including interactions with this factor. This type is often selected, because it takes interactions into account (Langsrud, 2003). This type is selected by default.")	, value: "type3"}
	]
}
