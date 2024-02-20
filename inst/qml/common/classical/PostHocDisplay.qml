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

Group
{
	title: qsTr("Display")
	CheckBox
	{
		name:				"postHocCi";
		label:				qsTr("Confidence intervals")
		childrenOnSameRow:	true
		CIField { name: "postHocCiLevel" }
	}
	CheckBox { name: "postHocSignificanceFlag";	label: qsTr("Flag significant comparisons") }
	CheckBox 
	{ 
		name: "postHocLetterTable"	
		label: qsTr("Letter-based grouping table")
		childrenOnSameRow:	true
		DoubleField 
		{ 
			name: "postHocLetterAlpha" 
			label: qsTr("α-level:")
			min: 0
          	max: 1
          	defaultValue: 0.05} 
	}

}
