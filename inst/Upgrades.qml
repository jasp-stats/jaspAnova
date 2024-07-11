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
			jsonValue:	"standard"
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
			jsonValue:	"standard"
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
			jsonValue:	"standard"
		}
	}

	// Option renaming for syntax
	Upgrade
	{
		functionName:		"Anova"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		// Display.qml
		ChangeRename {	from: "VovkSellkeMPR";	to: "vovkSellke"	}

		// AssumptionChecks.qml
		ChangeRename {	from: "homogeneityNone";	to: "homogeneityCorrectionNone"		}
		ChangeRename {	from: "homogeneityBrown";	to: "homogeneityCorrectionBrown"	}
		ChangeRename {	from: "homogeneityWelch";	to: "homogeneityCorrectionWelch"	}

		// Contrasts.qml
		ChangeRename {	from: "confidenceIntervalsContrast";			to: "contrastCi"		}
		ChangeRename {	from: "confidenceIntervalIntervalContrast";		to: "contrastCiLevel"	}

		// OrderRestrictions.qml
		ChangeRename {	from: "restrictedIncludeIntercept";					to: "restrictedInterceptInclusion"					}
		ChangeRename {	from: "restrictedModelShowAvailableCoefficients";	to: "restrictedAvailableCoefficients"				}
		ChangeRename {	from: "restrictedModelSummaryByDefault";			to: "restrictedModelSummaryForAllModels"			}
		ChangeRename {	from: "restrictedMarginalMeansByDefault";			to: "restrictedMarginalMeanForAllModels"			}
		ChangeRename {	from: "restrictedInformedHypothesisTestByDefault";	to: "restrictedInformedHypothesisTestForAllModels"	}

		// restrictedModels is an array of option lists so we need to change the option names for each model
		ChangeJS
		{
			name: "restrictedModels"
			jsFunction: function(options)
			{
				let newModels = options["restrictedModels"].map(model => {
					let newModel = {};
					newModel.informedHypothesisTest	= model.informedHypothesisTest;
					newModel.marginalMean			= model.marginalMeans;
					newModel.summary				= model.modelSummary;
					newModel.name					= model.modelName;
					newModel.syntax					= model.restrictionSyntax;

					return newModel ;
				});

				return newModels;
			}
		}

		ChangeRename {	from: "restrictedModelComparisonHighlightCoefficients";		to: "restrictedModelComparisonCoefficientsHighlight"	}

		ChangeRename {	from: "restrictedSE";	to: "restrictedHeterogeneityCorrection"	}

		ChangeJS
		{
			name: "restrictedHeterogeneityCorrection"
			jsFunction: function(options)
			{
				switch(options["restrictedHeterogeneityCorrection"])
				{
					case "standard":	return "none";
					case "HC0":			return "huberWhite0";
					case "HC1":			return "huberWhite1";
					case "HC2":			return "huberWhite2";
					case "HC3":			return "huberWhite3";
					case "HC4":			return "huberWhite4";
					case "HC4m":		return "huberWhite4m";
					case "HC5":			return "huberWhite5";
					default:			return options["restrictedHeterogeneityCorrection"];
				}
			}
		}

		ChangeRename {	from: "restrictedBootstrapping";								to: "restrictedBootstrap"			}
		ChangeRename {	from: "restrictedBootstrappingReplicates";						to: "restrictedBootstrapSamples"	}
		ChangeRename {	from: "restrictedBootstrappingConfidenceIntervalLevel";			to: "restrictedBootstrapCiLevel"	}

		ChangeRename {	from: "restrictedModelMarginalMeansTerms";	to: "restrictedMarginalMeanTerms"	}

		// PostHoc.qml
		ChangeRename {	from: "postHocTestsVariables";					to: "postHocTerms"							}
		ChangeRename {	from: "postHocTestsTypeStandard";				to: "postHocTypeStandard"					}
		ChangeRename {	from: "postHocTestsBootstrapping";					to: "postHocTypeStandardBootstrap"			}
		ChangeRename {	from: "postHocTestsBootstrappingReplicates";	to: "postHocTypeStandardBootstrapSamples"	}
		ChangeRename {	from: "postHocTestEffectSize";					to: "postHocTypeStandardEffectSize"			}
		ChangeRename {	from: "postHocTestsTypeGames";					to: "postHocTypeGames"						}
		ChangeRename {	from: "postHocTestsTypeDunnett";				to: "postHocTypeDunnet"						}
		ChangeRename {	from: "postHocTestsTypeDunn";					to: "postHocTypeDunn"						}
		ChangeRename {	from: "postHocTestsTukey";						to: "postHocCorrectionTukey"				}
		ChangeRename {	from: "postHocTestsScheffe";					to: "postHocCorrectionScheffe"				}
		ChangeRename {	from: "postHocTestsBonferroni";					to: "postHocCorrectionBonferroni"			}
		ChangeRename {	from: "postHocTestsHolm";						to: "postHocCorrectionHolm"					}
		ChangeRename {	from: "postHocTestsSidak";						to: "postHocCorrectionSidak"				}

		// PostHocDiplay.qml
		ChangeRename {	from: "confidenceIntervalsPostHoc";			to: "postHocCi"					}
		ChangeRename {	from: "confidenceIntervalIntervalPostHoc";	to: "postHocCiLevel"			}
		ChangeRename {	from: "postHocFlagSignificant";				to: "postHocSignificanceFlag"	}

		// DescriptivePlots.qml
		ChangeRename {	from: "plotHorizontalAxis";			to: "descriptivePlotHorizontalAxis"		}
		ChangeRename {	from: "plotSeparateLines";			to: "descriptivePlotSeparateLines"		}
		ChangeRename {	from: "plotSeparatePlots";			to: "descriptivePlotSeparatePlot"		}
		ChangeRename {	from: "plotErrorBars";				to: "descriptivePlotErrorBar"			}
		ChangeRename {	from: "errorBarType";				to: "descriptivePlotErrorBarType"		}

		ChangeJS
		{
			name: "descriptivePlotErrorBarType"
			jsFunction: function(options)
			{
				switch(options["descriptivePlotErrorBarType"])
				{
					case "confidenceInterval":	return "ci";
					case "standardError":		return "se";
					default:					return options["descriptivePlotErrorBarType"];
				}
			}
		}

		ChangeRename {	from: "confidenceIntervalInterval";		to: "descriptivePlotCiLevel"	}

		// RainCloudPlots.qml
		ChangeRename {	from: "rainCloudPlotsHorizontalAxis";		to: "rainCloudHorizontalAxis"		}
		ChangeRename {	from: "rainCloudPlotsSeparatePlots";		to: "rainCloudSeparatePlots"		}
		ChangeRename {	from: "rainCloudPlotsHorizontalDisplay";	to: "rainCloudHorizontalDisplay"	}


		// MarginalMeans.qml
		ChangeRename {	from: "marginalMeansTerms";						to: "marginalMeanTerms"				}
		ChangeRename {	from: "marginalMeansBootstrapping";				to: "marginalMeanBootstrap"			}
		ChangeRename {	from: "marginalMeansBootstrappingReplicates";	to: "marginalMeanBootstrapSamples"	}
		ChangeRename {	from: "marginalMeansCompareMainEffects";		to: "marginalMeanComparedToZero"	}
		ChangeRename {	from: "marginalMeansCIAdjustment";				to: "marginalMeanCiCorrection"		}

		// SimpleMainEffects
		ChangeRename {	from: "simpleFactor";			to: "simpleMainEffectFactor"				}
		ChangeRename {	from: "moderatorFactorOne";		to: "simpleMainEffectModeratorFactorOne"	}
		ChangeRename {	from: "moderatorFactorTwo";		to: "simpleMainEffectModeratorFactorTwo"	}

		// Nonparametrics
		ChangeRename {	from: "kruskalVariablesAssigned";	to: "kruskalWallisFactors"			}
	}

	Upgrade
	{
		functionName:		"Ancova"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		// Display.qml
		ChangeRename {	from: "VovkSellkeMPR";	to: "vovkSellke"	}

		// AssumptionChecks.qml
		ChangeRename {	from: "factorCovariateIndependence";	to: "factorCovariateIndependenceCheck"	}

		// Contrasts.qml
		ChangeRename {	from: "confidenceIntervalsContrast";			to: "contrastCi"		}
		ChangeRename {	from: "confidenceIntervalIntervalContrast";		to: "contrastCiLevel"	}

		// OrderRestrictions.qml
		ChangeRename {	from: "restrictedIncludeIntercept";					to: "restrictedInterceptInclusion"					}
		ChangeRename {	from: "restrictedModelShowAvailableCoefficients";	to: "restrictedAvailableCoefficients"				}
		ChangeRename {	from: "restrictedModelSummaryByDefault";			to: "restrictedModelSummaryForAllModels"			}
		ChangeRename {	from: "restrictedMarginalMeansByDefault";			to: "restrictedMarginalMeanForAllModels"			}
		ChangeRename {	from: "restrictedInformedHypothesisTestByDefault";	to: "restrictedInformedHypothesisTestForAllModels"	}

		// restrictedModels is an array of option lists so we need to change the option names for each model
		ChangeJS
		{
			name: "restrictedModels"
			jsFunction: function(options)
			{
				let newModels = options["restrictedModels"].map(model => {
					let newModel = {};
					newModel.informedHypothesisTest	= model.informedHypothesisTest;
					newModel.marginalMean			= model.marginalMeans;
					newModel.summary				= model.modelSummary;
					newModel.name					= model.modelName;
					newModel.syntax					= model.restrictionSyntax;

					return newModel ;
				});

				return newModels;
			}
		}

		ChangeRename {	from: "restrictedModelComparisonHighlightCoefficients";		to: "restrictedModelComparisonCoefficientsHighlight"	}

		ChangeRename {	from: "restrictedSE";	to: "restrictedHeterogeneityCorrection"	}

		ChangeJS
		{
			name: "restrictedHeterogeneityCorrection"
			jsFunction: function(options)
			{
				switch(options["restrictedHeterogeneityCorrection"])
				{
					case "standard":	return "none";
					case "HC0":			return "huberWhite0";
					case "HC1":			return "huberWhite1";
					case "HC2":			return "huberWhite2";
					case "HC3":			return "huberWhite3";
					case "HC4":			return "huberWhite4";
					case "HC4m":		return "huberWhite4m";
					case "HC5":			return "huberWhite5";
					default:			return options["restrictedHeterogeneityCorrection"];
				}
			}
		}

		ChangeRename {	from: "restrictedBootstrapping";								to: "restrictedBootstrap"			}
		ChangeRename {	from: "restrictedBootstrappingReplicates";						to: "restrictedBootstrapSamples"	}
		ChangeRename {	from: "restrictedBootstrappingConfidenceIntervalLevel";			to: "restrictedBootstrapCiLevel"	}

		ChangeRename {	from: "restrictedModelMarginalMeansTerms";	to: "restrictedMarginalMeanTerms"	}

		// PostHoc.qml
		ChangeRename {	from: "postHocTestsVariables";					to: "postHocTerms"							}
		ChangeRename {	from: "postHocTestsTypeStandard";				to: "postHocTypeStandard"					}
		ChangeRename {	from: "postHocTestsBootstrapping";				to: "postHocTypeStandardBootstrap"			}
		ChangeRename {	from: "postHocTestsBootstrappingReplicates";	to: "postHocTypeStandardBootstrapSamples"	}
		ChangeRename {	from: "postHocTestEffectSize";					to: "postHocTypeStandardEffectSize"			}
		ChangeRename {	from: "postHocTestsTypeGames";					to: "postHocTypeGames"						}
		ChangeRename {	from: "postHocTestsTypeDunnett";				to: "postHocTypeDunnet"						}
		ChangeRename {	from: "postHocTestsTypeDunn";					to: "postHocTypeDunn"						}
		ChangeRename {	from: "postHocTestsTukey";						to: "postHocCorrectionTukey"				}
		ChangeRename {	from: "postHocTestsScheffe";					to: "postHocCorrectionScheffe"				}
		ChangeRename {	from: "postHocTestsBonferroni";					to: "postHocCorrectionBonferroni"			}
		ChangeRename {	from: "postHocTestsHolm";						to: "postHocCorrectionHolm"					}
		ChangeRename {	from: "postHocTestsSidak";						to: "postHocCorrectionSidak"				}

		// PostHocDiplay.qml
		ChangeRename {	from: "confidenceIntervalsPostHoc";			to: "postHocCi"					}
		ChangeRename {	from: "confidenceIntervalIntervalPostHoc";	to: "postHocCiLevel"			}
		ChangeRename {	from: "postHocFlagSignificant";				to: "postHocSignificanceFlag"	}

		// DescriptivePlots.qml
		ChangeRename {	from: "plotHorizontalAxis";			to: "descriptivePlotHorizontalAxis"		}
		ChangeRename {	from: "plotSeparateLines";			to: "descriptivePlotSeparateLines"		}
		ChangeRename {	from: "plotSeparatePlots";			to: "descriptivePlotSeparatePlot"		}
		ChangeRename {	from: "plotErrorBars";				to: "descriptivePlotErrorBar"			}
		ChangeRename {	from: "errorBarType";				to: "descriptivePlotErrorBarType"		}

		ChangeJS
		{
			name: "descriptivePlotErrorBarType"
			jsFunction: function(options)
			{
				switch(options["descriptivePlotErrorBarType"])
				{
					case "confidenceInterval":	return "ci";
					case "standardError":		return "se";
					default:					return options["descriptivePlotErrorBarType"];
				}
			}
		}

		ChangeRename {	from: "confidenceIntervalInterval";		to: "descriptivePlotCiLevel"	}

		// RainCloudPlots.qml
		ChangeRename {	from: "rainCloudPlotsHorizontalAxis";		to: "rainCloudHorizontalAxis"		}
		ChangeRename {	from: "rainCloudPlotsSeparatePlots";		to: "rainCloudSeparatePlots"		}
		ChangeRename {	from: "rainCloudPlotsHorizontalDisplay";	to: "rainCloudHorizontalDisplay"	}


		// MarginalMeans.qml
		ChangeRename {	from: "marginalMeansTerms";						to: "marginalMeanTerms"				}
		ChangeRename {	from: "marginalMeansBootstrapping";				to: "marginalMeanBootstrap"			}
		ChangeRename {	from: "marginalMeansBootstrappingReplicates";	to: "marginalMeanBootstrapSamples"	}
		ChangeRename {	from: "marginalMeansCompareMainEffects";		to: "marginalMeanComparedToZero"	}
		ChangeRename {	from: "marginalMeansCIAdjustment";				to: "marginalMeanCiCorrection"		}

		// SimpleMainEffects
		ChangeRename {	from: "simpleFactor";			to: "simpleMainEffectFactor"				}
		ChangeRename {	from: "moderatorFactorOne";		to: "simpleMainEffectModeratorFactorOne"	}
		ChangeRename {	from: "moderatorFactorTwo";		to: "simpleMainEffectModeratorFactorTwo"	}

		// Nonparametrics
		ChangeRename {	from: "kruskalVariablesAssigned";	to: "kruskalWallisFactors"			}
	}

	Upgrade
	{
		functionName:		"AnovaRepeatedMeasures"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		// Display.qml
		ChangeRename {	from: "VovkSellkeMPR";	to: "vovkSellke"	}
		ChangeRename {	from: "effectSizeGenEtaSquared";	to: "effectSizeGeneralEtaSquared"	}

		// Model section
		ChangeRename {	from: "useMultivariateModelFollowup";	to: "multivariateModelFollowup"	}

		// AssumptionChecks.qml
		ChangeRename {	from: "sphericityNone";					to: "sphericityCorrectionNone"				}
		ChangeRename {	from: "sphericityGreenhouseGeisser";	to: "sphericityCorrectionGreenhouseGeisser"	}
		ChangeRename {	from: "sphericityHuynhFeldt";			to: "sphericityCorrectionHuynhFeldt"		}


		// Contrasts.qml
		ChangeRename {	from: "contrastAssumeEqualVariance";			to: "contrastEqualVariance"		}
		ChangeRename {	from: "confidenceIntervalsContrast";			to: "contrastCi"				}
		ChangeRename {	from: "confidenceIntervalIntervalContrast";		to: "contrastCiLevel"			}

		// OrderRestrictions.qml
		ChangeRename {	from: "restrictedIncludeIntercept";					to: "restrictedInterceptInclusion"					}
		ChangeRename {	from: "restrictedModelShowAvailableCoefficients";	to: "restrictedAvailableCoefficients"				}
		ChangeRename {	from: "restrictedModelSummaryByDefault";			to: "restrictedModelSummaryForAllModels"			}
		ChangeRename {	from: "restrictedMarginalMeansByDefault";			to: "restrictedMarginalMeanForAllModels"			}
		ChangeRename {	from: "restrictedInformedHypothesisTestByDefault";	to: "restrictedInformedHypothesisTestForAllModels"	}

		// restrictedModels is an array of option lists so we need to change the option names for each model
		ChangeJS
		{
			name: "restrictedModels"
			jsFunction: function(options)
			{
				let newModels = options["restrictedModels"].map(model => {
					let newModel = {};
					newModel.informedHypothesisTest	= model.informedHypothesisTest;
					newModel.marginalMean			= model.marginalMeans;
					newModel.summary				= model.modelSummary;
					newModel.name					= model.modelName;
					newModel.syntax					= model.restrictionSyntax;

					return newModel ;
				});

				return newModels;
			}
		}

		ChangeRename {	from: "restrictedModelComparisonHighlightCoefficients";		to: "restrictedModelComparisonCoefficientsHighlight"	}

		ChangeRename {	from: "restrictedSE";	to: "restrictedHeterogeneityCorrection"	}

		ChangeJS
		{
			name: "restrictedHeterogeneityCorrection"
			jsFunction: function(options)
			{
				switch(options["restrictedHeterogeneityCorrection"])
				{
					case "standard":	return "none";
					case "HC0":			return "huberWhite0";
					case "HC1":			return "huberWhite1";
					case "HC2":			return "huberWhite2";
					case "HC3":			return "huberWhite3";
					case "HC4":			return "huberWhite4";
					case "HC4m":		return "huberWhite4m";
					case "HC5":			return "huberWhite5";
					default:			return options["restrictedHeterogeneityCorrection"];
				}
			}
		}

		ChangeRename {	from: "restrictedBootstrapping";								to: "restrictedBootstrap"			}
		ChangeRename {	from: "restrictedBootstrappingReplicates";						to: "restrictedBootstrapSamples"	}
		ChangeRename {	from: "restrictedBootstrappingConfidenceIntervalLevel";			to: "restrictedBootstrapCiLevel"	}

		ChangeRename {	from: "restrictedModelMarginalMeansTerms";	to: "restrictedMarginalMeanTerms"	}

		// PostHoc section
		ChangeRename {	from: "postHocTestsVariables";					to: "postHocTerms"							}
		ChangeRename {	from: "postHocTestEffectSize";					to: "postHocEffectSize"						}
		ChangeRename {	from: "postHocTestPooledError";					to: "postHocPooledError"					}

		ChangeRename {	from: "postHocTestsHolm";						to: "postHocCorrectionHolm"					}
		ChangeRename {	from: "postHocTestsBonferroni";					to: "postHocCorrectionBonferroni"			}
		ChangeRename {	from: "postHocTestsTukey";						to: "postHocCorrectionTukey"				}
		ChangeRename {	from: "postHocTestsScheffe";					to: "postHocCorrectionScheffe"				}

		// PostHocDiplay.qml
		ChangeRename {	from: "confidenceIntervalsPostHoc";			to: "postHocCi"					}
		ChangeRename {	from: "confidenceIntervalIntervalPostHoc";	to: "postHocCiLevel"			}
		ChangeRename {	from: "postHocFlagSignificant";				to: "postHocSignificanceFlag"	}

		// DescriptivePlots.qml
		ChangeRename {	from: "plotHorizontalAxis";			to: "descriptivePlotHorizontalAxis"		}
		ChangeRename {	from: "plotSeparateLines";			to: "descriptivePlotSeparateLines"		}
		ChangeRename {	from: "plotSeparatePlots";			to: "descriptivePlotSeparatePlot"		}
		ChangeRename {	from: "plotErrorBars";				to: "descriptivePlotErrorBar"			}
		ChangeRename {	from: "errorBarType";				to: "descriptivePlotErrorBarType"		}

		ChangeJS
		{
			name: "descriptivePlotErrorBarType"
			jsFunction: function(options)
			{
				switch(options["descriptivePlotErrorBarType"])
				{
					case "confidenceInterval":	return "ci";
					case "standardError":		return "se";
					default:					return options["descriptivePlotErrorBarType"];
				}
			}
		}

		ChangeRename {	from: "confidenceIntervalInterval";		to: "descriptivePlotCiLevel"	}

		ChangeRename {	from: "labelYAxis";					to: "descriptivePlotYAxisLabel"		}
		ChangeRename {	from: "usePooledStandErrorCI";		to: "descriptivePlotErrorBarPooled"	}


		// RainCloudPlots.qml
		ChangeRename {	from: "rainCloudPlotsHorizontalAxis";		to: "rainCloudHorizontalAxis"		}
		ChangeRename {	from: "rainCloudPlotsSeparatePlots";		to: "rainCloudSeparatePlots"		}
		ChangeRename {	from: "rainCloudPlotsLabelYAxis";			to: "rainCloudYAxisLabel"			}


		// MarginalMeans.qml
		ChangeRename {	from: "marginalMeansTerms";						to: "marginalMeanTerms"				}
		ChangeRename {	from: "marginalMeansBootstrapping";				to: "marginalMeanBootstrap"			}
		ChangeRename {	from: "marginalMeansBootstrappingReplicates";	to: "marginalMeanBootstrapSamples"	}
		ChangeRename {	from: "marginalMeansCompareMainEffects";		to: "marginalMeanComparedToZero"	}
		ChangeRename {	from: "marginalMeansCIAdjustment";				to: "marginalMeanCiCorrection"		}

		// SimpleMainEffects
		ChangeRename {	from: "simpleFactor";			to: "simpleMainEffectFactor"				}
		ChangeRename {	from: "moderatorFactorOne";		to: "simpleMainEffectModeratorFactorOne"	}
		ChangeRename {	from: "moderatorFactorTwo";		to: "simpleMainEffectModeratorFactorTwo"	}

		ChangeRename {	from: "poolErrorTermSimpleEffects";		to: "simpleMainEffectErrorTermPooled"	}


		// Nonparametrics
	}

	Upgrade
	{
		functionName:		"AnovaBayesian"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		// RainCloudPlots.qml
		ChangeRename {	from: "rainCloudPlotsHorizontalAxis";		to: "rainCloudHorizontalAxis"		}
		ChangeRename {	from: "rainCloudPlotsSeparatePlots";		to: "rainCloudSeparatePlots"		}
		ChangeRename {	from: "rainCloudPlotsHorizontalDisplay";	to: "rainCloudHorizontalDisplay"	}

		// DescriptivePlots.qml
		ChangeRename {	from: "plotHorizontalAxis";				to: "descriptivePlotHorizontalAxis"		}
		ChangeRename {	from: "plotSeparateLines";				to: "descriptivePlotSeparateLines"		}
		ChangeRename {	from: "plotSeparatePlots";				to: "descriptivePlotSeparatePlot"		}

		ChangeRename {	from: "plotCredibleInterval";			to: "descriptivePlotCi"					}
		ChangeRename {	from: "plotCredibleIntervalInterval";	to: "descriptivePlotCiLevel"			}

		// DefaultOptions.qml
		ChangeRename {	from: "posteriorPlot";					to: "modelAveragedPosteriorPlot"		}

		// PostHocTests.qml
		ChangeRename {	from: "postHocTestsVariables";			to: "postHocTerms"						}
		ChangeRename {	from: "postHocTestsNullControl";		to: "postHocNullControl"				}

		// SingleModelInference.qml
		ChangeRename {	from: "singleModelqqPlot";				to: "singleModelQqPlot"					}
		ChangeRename {	from: "singleModelrsqPlot";				to: "singleModelRsqPlot"				}

		// AdditionalOptions.qml
		ChangeRename {	from: "coefficientsPrior";				to: "priorSpecificationMode"			}
		ChangeJS
		{
			name: "priorSpecificationMode"
			jsFunction: function(options)
			{
				switch(options["priorSpecificationMode"])
				{
					case "rscalesAcrossParameters":	return "acrossParameters";
					case "rscalesPerTerm":			return "perTerm";
					default:						return options["priorSpecificationMode"];
				}
			}
		}

		ChangeRename {	from: "priorFixedEffects";				to: "cauchyPriorScaleFixedEffects"		}
		ChangeRename {	from: "priorRandomEffects";				to: "cauchyPriorScaleRandomEffects"		}
		ChangeRename {	from: "priorCovariates";				to: "cauchyPriorScaleCovariates"		}

		ChangeRename {	from: "sampleModeNumAcc";				to: "samplingMethodNumericAccuracy"		}
		ChangeRename {	from: "fixedNumAcc";					to: "samplesNumericAccuracy"			}
		ChangeRename {	from: "sampleModeMCMC";					to: "samplingMethodMCMC"				}
		ChangeRename {	from: "fixedMCMCSamples";				to: "samplesMCMC"						}
		ChangeRename {	from: "legacy";							to: "legacyResults"						}
		ChangeRename {	from: "hideNuisanceEffects";			to: "hideNuisanceParameters"			}

		ChangeJS
		{
			name: 		"modelPrior"
			jsFunction:	function(options)
			{
				switch(options["modelPrior"])
				{
					case "beta.binomial":		return "betaBinomial";
					default:					return options["modelPrior"];
				}
			}
		}
		ChangeRename {	from: "betaBinomialParamA";				to: "betaBinomialParameterA"			}
		ChangeRename {	from: "betaBinomialParamB";				to: "betaBinomialParameterB"			}
		ChangeRename {	from: "wilsonParamLambda";				to: "wilsonParameterLambda"				}
		ChangeRename {	from: "castilloParamU";					to: "castilloParameterU"				}
		ChangeRename {	from: "bernoulliParam";					to: "bernoulliParameter"				}
		ChangeRename {	from: "modelTermsCustomPrior";			to: "customPriorSpecification"			}
		ChangeJS
		{
			name:		"customPriorSpecification"
			jsFunction:	function(options)
			{
				let newSpecs = options["customPriorSpecification"].map(
					spec => {
						let newSpec = {};
						newSpec["components"] 			= spec["components"];
						newSpec["inclusionProbability"] = spec["priorIncl"];
						newSpec["scaleFixedEffects"] 	= spec["rscaleFixed"];
						return newSpec; 
					});

				return newSpecs;	
			}
		}
	}

	Upgrade
	{
		functionName:		"AncovaBayesian"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		// RainCloudPlots.qml
		ChangeRename {	from: "rainCloudPlotsHorizontalAxis";		to: "rainCloudHorizontalAxis"		}
		ChangeRename {	from: "rainCloudPlotsSeparatePlots";		to: "rainCloudSeparatePlots"		}
		ChangeRename {	from: "rainCloudPlotsHorizontalDisplay";	to: "rainCloudHorizontalDisplay"	}

		// DescriptivePlots.qml
		ChangeRename {	from: "plotHorizontalAxis";				to: "descriptivePlotHorizontalAxis"		}
		ChangeRename {	from: "plotSeparateLines";				to: "descriptivePlotSeparateLines"		}
		ChangeRename {	from: "plotSeparatePlots";				to: "descriptivePlotSeparatePlot"		}

		ChangeRename {	from: "plotCredibleInterval";			to: "descriptivePlotCi"					}
		ChangeRename {	from: "plotCredibleIntervalInterval";	to: "descriptivePlotCiLevel"			}

		// DefaultOptions.qml
		ChangeRename {	from: "posteriorPlot";					to: "modelAveragedPosteriorPlot"		}

		// PostHocTests.qml
		ChangeRename {	from: "postHocTestsVariables";			to: "postHocTerms"						}
		ChangeRename {	from: "postHocTestsNullControl";		to: "postHocNullControl"				}

		// SingleModelInference.qml
		ChangeRename {	from: "singleModelqqPlot";				to: "singleModelQqPlot"					}
		ChangeRename {	from: "singleModelrsqPlot";				to: "singleModelRsqPlot"				}

		// AdditionalOptions.qml
		ChangeRename {	from: "coefficientsPrior";				to: "priorSpecificationMode"			}
		ChangeJS
		{
			name: "priorSpecificationMode"
			jsFunction: function(options)
			{
				switch(options["priorSpecificationMode"])
				{
					case "rscalesAcrossParameters":	return "acrossParameters";
					case "rscalesPerTerm":			return "perTerm";
					default:						return options["priorSpecificationMode"];
				}
			}
		}

		ChangeRename {	from: "priorFixedEffects";				to: "cauchyPriorScaleFixedEffects"		}
		ChangeRename {	from: "priorRandomEffects";				to: "cauchyPriorScaleRandomEffects"		}
		ChangeRename {	from: "priorCovariates";				to: "cauchyPriorScaleCovariates"		}

		ChangeRename {	from: "sampleModeNumAcc";				to: "samplingMethodNumericAccuracy"		}
		ChangeRename {	from: "fixedNumAcc";					to: "samplesNumericAccuracy"			}
		ChangeRename {	from: "sampleModeMCMC";					to: "samplingMethodMCMC"				}
		ChangeRename {	from: "fixedMCMCSamples";				to: "samplesMCMC"						}
		ChangeRename {	from: "legacy";							to: "legacyResults"						}
		ChangeRename {	from: "hideNuisanceEffects";			to: "hideNuisanceParameters"			}

		ChangeJS
		{
			name: 		"modelPrior"
			jsFunction:	function(options)
			{
				switch(options["modelPrior"])
				{
					case "beta.binomial":		return "betaBinomial";
					default:					return options["modelPrior"];
				}
			}
		}
		ChangeRename {	from: "betaBinomialParamA";				to: "betaBinomialParameterA"			}
		ChangeRename {	from: "betaBinomialParamB";				to: "betaBinomialParameterB"			}
		ChangeRename {	from: "wilsonParamLambda";				to: "wilsonParameterLambda"				}
		ChangeRename {	from: "castilloParamU";					to: "castilloParameterU"				}
		ChangeRename {	from: "bernoulliParam";					to: "bernoulliParameter"				}
		ChangeRename {	from: "modelTermsCustomPrior";			to: "customPriorSpecification"			}
		ChangeJS
		{
			name:		"customPriorSpecification"
			jsFunction:	function(options)
			{
				let newSpecs = options["customPriorSpecification"].map(
					spec => {
						let newSpec = {};
						newSpec["components"] 			= spec["components"];
						newSpec["inclusionProbability"] = spec["priorIncl"];
						newSpec["scaleFixedEffects"] 	= spec["rscaleFixed"];
						return newSpec; 
					});

				return newSpecs;	
			}
		}
	}

	Upgrade
	{
		functionName:		"AnovaRepeatedMeasuresBayesian"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		// RainCloudPlots.qml
		ChangeRename {	from: "rainCloudPlotsHorizontalAxis";		to: "rainCloudHorizontalAxis"		}
		ChangeRename {	from: "rainCloudPlotsSeparatePlots";		to: "rainCloudSeparatePlots"		}
		ChangeRename {	from: "rainCloudPlotsLabelYAxis";			to: "rainCloudYAxisLabel"			}

		// DescriptivePlots.qml
		ChangeRename {	from: "plotHorizontalAxis";				to: "descriptivePlotHorizontalAxis"		}
		ChangeRename {	from: "plotSeparateLines";				to: "descriptivePlotSeparateLines"		}
		ChangeRename {	from: "plotSeparatePlots";				to: "descriptivePlotSeparatePlot"		}

		ChangeRename {	from: "plotCredibleInterval";			to: "descriptivePlotCi"					}
		ChangeRename {	from: "plotCredibleIntervalInterval";	to: "descriptivePlotCiLevel"			}

		ChangeRename {	from: "labelYAxis";						to: "descriptivePlotYAxisLabel"			}

		// DefaultOptions.qml
		ChangeRename {	from: "posteriorPlot";					to: "modelAveragedPosteriorPlot"		}

		// PostHocTests.qml
		ChangeRename {	from: "postHocTestsVariables";			to: "postHocTerms"						}
		ChangeRename {	from: "postHocTestsNullControl";		to: "postHocNullControl"				}


		// SingleModelInference.qml
		ChangeRename {	from: "singleModelqqPlot";				to: "singleModelQqPlot"					}
		ChangeRename {	from: "singleModelrsqPlot";				to: "singleModelRsqPlot"				}

		// AdditionalOptions.qml
		ChangeRename {	from: "coefficientsPrior";				to: "priorSpecificationMode"			}
		ChangeJS
		{
			name: "priorSpecificationMode"
			jsFunction: function(options)
			{
				switch(options["priorSpecificationMode"])
				{
					case "rscalesAcrossParameters":	return "acrossParameters";
					case "rscalesPerTerm":			return "perTerm";
					default:						return options["priorSpecificationMode"];
				}
			}
		}
		
		ChangeRename {	from: "priorFixedEffects";				to: "cauchyPriorScaleFixedEffects"		}
		ChangeRename {	from: "priorRandomEffects";				to: "cauchyPriorScaleRandomEffects"		}
		ChangeRename {	from: "priorCovariates";				to: "cauchyPriorScaleCovariates"		}

		ChangeRename {	from: "sampleModeNumAcc";				to: "samplingMethodNumericAccuracy"		}
		ChangeRename {	from: "fixedNumAcc";					to: "samplesNumericAccuracy"			}
		ChangeRename {	from: "sampleModeMCMC";					to: "samplingMethodMCMC"				}
		ChangeRename {	from: "fixedMCMCSamples";				to: "samplesMCMC"						}
		ChangeRename {	from: "legacy";							to: "legacyResults"						}
		ChangeRename {	from: "hideNuisanceEffects";			to: "hideNuisanceParameters"			}

		ChangeJS
		{
			name: 		"modelPrior"
			jsFunction:	function(options)
			{
				switch(options["modelPrior"])
				{
					case "beta.binomial":		return "betaBinomial";
					default:					return options["modelPrior"];
				}
			}
		}
		ChangeRename {	from: "betaBinomialParamA";				to: "betaBinomialParameterA"			}
		ChangeRename {	from: "betaBinomialParamB";				to: "betaBinomialParameterB"			}
		ChangeRename {	from: "wilsonParamLambda";				to: "wilsonParameterLambda"				}
		ChangeRename {	from: "castilloParamU";					to: "castilloParameterU"				}
		ChangeRename {	from: "bernoulliParam";					to: "bernoulliParameter"				}
		ChangeRename {	from: "modelTermsCustomPrior";			to: "customPriorSpecification"			}
		ChangeJS
		{
			name:		"customPriorSpecification"
			jsFunction:	function(options)
			{
				let newSpecs = options["customPriorSpecification"].map(
					spec => {
						let newSpec = {};
						newSpec["components"] 			= spec["components"];
						newSpec["inclusionProbability"] = spec["priorIncl"];
						newSpec["scaleFixedEffects"] 	= spec["rscaleFixed"];
						return newSpec; 
					});

				return newSpecs;	
			}
		}
	}

	Upgrade
	{
		functionName:		"Manova"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		// DescriptivePlots.qml
		ChangeRename {	from: "includeAnovaTables";				to: "anovaTables"	}
		ChangeRename {	from: "VovkSellkeMPR";					to: "vovkSellke"	}

	}

	Upgrade
	{
		functionName:		"AnovaRepeatedMeasures"
		fromVersion:		"0.18.3"
		toVersion:			"0.19.0"
		msg:				"From JASP 0.19 onward, the RM ANOVA uses the unpooled error terms for follow-up tests as the default, to better handle possible violations of sphericity. If you were previously using the pooled error term, this means your post hoc, contrast, and marginal mean results can slightly differ when refreshing the results. In order to use the pooled error term, you can tick the checkbox labelled 'Pool error term for follow-up tests'."
	}

}
