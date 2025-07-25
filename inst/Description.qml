import QtQuick
import JASP.Module

Description
{
	name		: "jaspAnova"
	title		: qsTr("ANOVA")
	icon		: "analysis-classical-anova.svg"
	description	: qsTr("Evaluate the difference between multiple means")
	version			: "0.95.0"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	hasWrappers	: true


	GroupTitle
	{
		title:  qsTr("Classical");
		icon:	"analysis-classical-anova.svg"
	}

	Analysis { title: qsTr("ANOVA");					func:	"Anova"					}
	Analysis { title: qsTr("Repeated Measures ANOVA");	func:	"AnovaRepeatedMeasures"	}
	Analysis { title: qsTr("ANCOVA");					func:	"Ancova"				}
	Analysis { title: qsTr("MANOVA");					func:	"Manova"				}

	Separator{}

	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"analysis-bayesian-anova.svg"
	}

	Analysis
	{
		menu:	qsTr("ANOVA")
		title:	qsTr("Bayesian ANOVA")
		func:	"AnovaBayesian"
	}

	Analysis
	{
		menu:	qsTr("Repeated Measures ANOVA")
		title:	qsTr("Bayesian Repeated Measures ANOVA")
		func:	"AnovaRepeatedMeasuresBayesian"
	}

	Analysis
	{
		menu:	qsTr("ANCOVA")
		title:	qsTr("Bayesian ANCOVA")
		func:	"AncovaBayesian"
	}
}
