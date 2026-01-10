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
import "./" as Classical


Section
{
	title: qsTr("Export")

	Group
	{
		CheckBox
		{
			id:							residualsSavedToData
			name:						"residualsSavedToData"
			info: qsTr("Save the residuals of the most complex model as a new column in the data file.")
			text:						qsTr("Append residuals to data")

			ComputedColumnField
			{
				name:					"residualsSavedToDataColumn"
				text:					qsTr("Column name")
				placeholderText:		qsTr("e.g., residuals")
				fieldWidth:				120
				enabled:				residualsSavedToData.checked
			}
			
			RadioButtonGroup
			{
				title: qsTr("Residuals type")
				name: "residualsSavedToDataType"
				RadioButton { value: "raw";			label: qsTr("Raw"); info: qsTr("Appends raw residuals, meaning the simple difference between observed values and predicted values. Selected by default.") ;checked: true	}
				RadioButton { value: "standard";	label: qsTr("Standardized"); info: qsTr("Divides residuals by estimate of standard error of the residuals and adjusts for the leverage of each data point. Use when there are assumption violations as it is more robust.")		}
				RadioButton { value: "student";		label: qsTr("Studentized"); info: qsTr("Divides residuals by estimate of the standard deviation of the residuals.")		}
			}
		}

		CheckBox
		{
			id:							predictionsSavedToData
			name:						"predictionsSavedToData"
			info: qsTr("Save the predictions of the most complex model as a new column in the data file.")
			text:						qsTr("Append predictions to data")

			ComputedColumnField
			{
				name:					"predictionsSavedToDataColumn"
				text:					qsTr("Column name")
				placeholderText:		qsTr("e.g., predictions")
				fieldWidth:				120
				enabled:				predictionsSavedToData.checked
			}
		}
	}
}
