import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	//Here an ugly workaround function because people forgot to actually set the module version to 0.16.3 for this upgrade. So the version might still be 0.15 and thus this would be applied twice.
	//Future authors DONT FORGET TO UPDATE THE VERSION NUMBER in DESCRIPTION and Description.qml!
	function shouldRename(name)
	{
		return function(options) { return options.hasOwnProperty(name); }
	}

	Upgrade
	{
		functionName: 		"Anova"
		fromVersion:		"0.16.2"
		toVersion:			"0.16.3"

		// Changes for better consistency within restrictions (every option is preceded by 'restricted') + between analyses (e.g. naming of bootstrapping options)
		ChangeRename {	condition: shouldRename("includeIntercept");									from: "includeIntercept";								to: "restrictedIncludeIntercept"							}
		ChangeRename {	condition: shouldRename("highlightEstimates");									from: "highlightEstimates";								to: "restrictedModelComparisonHighlightCoefficients"		}
		ChangeRename {	condition: shouldRename("restrictedModelHeteroskedasticity");					from: "restrictedModelHeteroskedasticity";				to: "restrictedSE"											}
		ChangeRename {	condition: shouldRename("restrictedConfidenceIntervalBootstrap");				from: "restrictedConfidenceIntervalBootstrap";			to: "restrictedBootstrapping"								}
		ChangeRename {	condition: shouldRename("restrictedConfidenceIntervalBootstrapSamples");		from: "restrictedConfidenceIntervalBootstrapSamples";	to: "restrictedBootstrappingReplicates"						}
		ChangeRename {	condition: shouldRename("restrictedConfidenceIntervalLevel");					from: "restrictedConfidenceIntervalLevel";				to: "restrictedBootstrappingConfidenceIntervalLevel"		}
		ChangeRename {	condition: shouldRename("restrictedModelMarginalMeansTerm");					from: "restrictedModelMarginalMeansTerm";				to: "restrictedModelMarginalMeansTerms"						}

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
		toVersion:			"0.16.3"

		// Changes for better consistency within restrictions (every option is preceded by 'restricted') + between analyses (e.g. naming of bootstrapping options)
		ChangeRename {	condition: shouldRename("includeIntercept");									from: "includeIntercept";								to: "restrictedIncludeIntercept"							}
		ChangeRename {	condition: shouldRename("highlightEstimates");									from: "highlightEstimates";								to: "restrictedModelComparisonHighlightCoefficients"		}
		ChangeRename {	condition: shouldRename("restrictedModelHeteroskedasticity");					from: "restrictedModelHeteroskedasticity";				to: "restrictedSE"											}
		ChangeRename {	condition: shouldRename("restrictedConfidenceIntervalBootstrap");				from: "restrictedConfidenceIntervalBootstrap";			to: "restrictedBootstrapping"								}
		ChangeRename {	condition: shouldRename("restrictedConfidenceIntervalBootstrapSamples");		from: "restrictedConfidenceIntervalBootstrapSamples";	to: "restrictedBootstrappingReplicates"						}
		ChangeRename {	condition: shouldRename("restrictedConfidenceIntervalLevel");					from: "restrictedConfidenceIntervalLevel";				to: "restrictedBootstrappingConfidenceIntervalLevel"		}
		ChangeRename {	condition: shouldRename("restrictedModelMarginalMeansTerm");					from: "restrictedModelMarginalMeansTerm";				to: "restrictedModelMarginalMeansTerms"						}

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
		toVersion:			"0.16.3"

		// Changes for better consistency within restrictions (every option is preceded by 'restricted') + between analyses (e.g. naming of bootstrapping options)
		ChangeRename {	condition: shouldRename("includeIntercept");									from: "includeIntercept";								to: "restrictedIncludeIntercept"							}
		ChangeRename {	condition: shouldRename("highlightEstimates");									from: "highlightEstimates";								to: "restrictedModelComparisonHighlightCoefficients"		}
		ChangeRename {	condition: shouldRename("restrictedModelHeteroskedasticity");					from: "restrictedModelHeteroskedasticity";				to: "restrictedSE"											}
		ChangeRename {	condition: shouldRename("restrictedConfidenceIntervalBootstrap");				from: "restrictedConfidenceIntervalBootstrap";			to: "restrictedBootstrapping"								}
		ChangeRename {	condition: shouldRename("restrictedConfidenceIntervalBootstrapSamples");		from: "restrictedConfidenceIntervalBootstrapSamples";	to: "restrictedBootstrappingReplicates"						}
		ChangeRename {	condition: shouldRename("restrictedConfidenceIntervalLevel");					from: "restrictedConfidenceIntervalLevel";				to: "restrictedBootstrappingConfidenceIntervalLevel"		}
		ChangeRename {	condition: shouldRename("restrictedModelMarginalMeansTerm");					from: "restrictedModelMarginalMeansTerm";				to: "restrictedModelMarginalMeansTerms"						}

		// Change of option value that is passed directly to the restriktor package: before it was 'none', but the package now uses 'standard'
		ChangeSetValue
		{
			name:		"restrictedSE"
			condition:	function(options) { return options["restrictedSE"] === "none"; }
			jsValue:	"standard"
		}
	}
}
