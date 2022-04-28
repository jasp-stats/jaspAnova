import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName: 		"Anova"
		fromVersion:		"0.16.2"
		toVersion:			"0.16.3"

		// Changes for better consistency within restrictions (every option is preceded by 'restricted') + between analyses (e.g. naming of bootstrapping options)
		ChangeRename {	from: "includeIntercept";								to: "restrictedIncludeIntercept"							}
		ChangeRename {	from: "highlightEstimates";								to: "restrictedModelComparisonHighlightCoefficients"		}
		ChangeRename {	from: "restrictedModelHeteroskedasticity";				to: "restrictedSE"											}
		ChangeRename {	from: "restrictedConfidenceIntervalBootstrap";			to: "restrictedBootstrapping"								}
		ChangeRename {	from: "restrictedConfidenceIntervalBootstrapSamples";	to: "restrictedBootstrappingReplicates"						}
		ChangeRename {	from: "restrictedConfidenceIntervalLevel";				to: "restrictedBootstrappingConfidenceIntervalLevel"		}
		ChangeRename {	from: "restrictedModelMarginalMeansTerm";				to: "restrictedModelMarginalMeansTerms"						}

		// Change of option value that is passed directly to the restriktor package: before it was 'none', but the package now uses 'standard'
		ChangeSetValue
		{
			name:		"restrictedSE"
			condition:	function(options) { return options["restrictedSE"] === "none"; }
			jsValue:	"standard"
		}
	}

	Upgrade
	{
		functionName: 		"Ancova"
		fromVersion:		"0.16.2"
		toVersion:			"0.17"

		// Changes for better consistency within restrictions (every option is preceded by 'restricted') + between analyses (e.g. naming of bootstrapping options)
		ChangeRename {	from: "includeIntercept";								to: "restrictedIncludeIntercept"							}
		ChangeRename {	from: "highlightEstimates";								to: "restrictedModelComparisonHighlightCoefficients"		}
		ChangeRename {	from: "restrictedModelHeteroskedasticity";				to: "restrictedSE"											}
		ChangeRename {	from: "restrictedConfidenceIntervalBootstrap";			to: "restrictedBootstrapping"								}
		ChangeRename {	from: "restrictedConfidenceIntervalBootstrapSamples";	to: "restrictedBootstrappingReplicates"						}
		ChangeRename {	from: "restrictedConfidenceIntervalLevel";				to: "restrictedBootstrappingConfidenceIntervalLevel"		}
		ChangeRename {	from: "restrictedModelMarginalMeansTerm";				to: "restrictedModelMarginalMeansTerms"						}

		// Change of option value that is passed directly to the restriktor package: before it was 'none', but the package now uses 'standard'
		ChangeSetValue
		{
			name:		"restrictedSE"
			condition:	function(options) { return options["restrictedSE"] === "none"; }
			jsValue:	"standard"
		}
	}

	Upgrade
	{
		functionName: 		"AnovaRepeatedMeasures"
		fromVersion:		"0.16.2"
		toVersion:			"0.17"

		// Changes for better consistency within restrictions (every option is preceded by 'restricted') + between analyses (e.g. naming of bootstrapping options)
		ChangeRename {	from: "includeIntercept";								to: "restrictedIncludeIntercept"							}
		ChangeRename {	from: "highlightEstimates";								to: "restrictedModelComparisonHighlightCoefficients"		}
		ChangeRename {	from: "restrictedModelHeteroskedasticity";				to: "restrictedSE"											}
		ChangeRename {	from: "restrictedConfidenceIntervalBootstrap";			to: "restrictedBootstrapping"								}
		ChangeRename {	from: "restrictedConfidenceIntervalBootstrapSamples";	to: "restrictedBootstrappingReplicates"						}
		ChangeRename {	from: "restrictedConfidenceIntervalLevel";				to: "restrictedBootstrappingConfidenceIntervalLevel"		}
		ChangeRename {	from: "restrictedModelMarginalMeansTerm";				to: "restrictedModelMarginalMeansTerms"						}

		// Change of option value that is passed directly to the restriktor package: before it was 'none', but the package now uses 'standard'
		ChangeSetValue
		{
			name:		"restrictedSE"
			condition:	function(options) { return options["restrictedSE"] === "none"; }
			jsValue:	"standard"
		}
	}
}
