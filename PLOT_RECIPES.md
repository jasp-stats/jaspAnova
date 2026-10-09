# Plot recipes

Migrated the local frequentist Q-Q plot and Bayesian model-averaged/single-model Q-Q, R², grouped/ungrouped posterior plots, plus the common frequentist/Bayesian descriptive and bar plots. Posterior sampling, interval calculation and summary computation remain in the analysis. Recipes store only numeric summaries, density curves, labels and drawing settings; drawing helpers construct ggplot mappings and layers. Summary drawing is shared across frequentist/Bayesian ANOVA, ANCOVA and repeated-measures ANOVA; grouped posterior drawing is shared across the Bayesian analyses.

`renv.lock` pins recipe-capable jaspBase, jaspGraphs and jaspTools. Other dependency pins remain unchanged.

## Deferred shared plots

- Common frequentist/Bayesian rainclouds in `.BANOVArainCloudPlots` call `jaspTTests::.descriptivesPlotsRainCloudFill`. The currently locked helper builds ggplots and generates jitter. Prefer pinning a released recipe-capable jaspTTests revision, then keep this call unchanged so its preparation captures jitter before redraw. Alternatively extract point/density preparation into a shared helper and store its plain output in a drawing recipe. Do not wrap the existing random drawing helper directly.
- Covariate descriptive scatter plots in `.BANOVAdescriptivesPlots` delegate to `jaspDescriptives::.descriptivesScatterPlots`. Update that dependency to a validated recipe migration separately, and verify the shared container helper supports recipes. No duplicate plotting implementation is introduced here.

## Validation

Use R-4.5.2. `test-plotRecipes.R` checks recipes emitted by posterior producers, serialization, deterministic redraws and unchanged RNG state, summary factors/interval data, and edited axis titles. The producer test mocks only native plot allocation; existing analysis tests exercise actual native rendering and name decoding. Compare existing snapshots without accepting changed references.

The migration was validated with recipe-capable jaspBase `b187388a`, jaspGraphs `288d2751` and jaspTools `1a109435`, using the module's original remaining dependencies. All 31 focused recipe assertions and 11 exact SVG comparisons against the original builders passed. A real classical ANOVA Q-Q rendering smoke test passed.

The existing full suite and an independently installed master baseline each reported 230 passes, 3 failures, 4 errors, 13 warnings and 1 skip. Every test outcome and diagnostic matched after normalizing timing and temporary paths. The three failures involve delegated jaspDescriptives scatter formula snapshots; the errors concern the factor-order table, Bayesian model-comparison/prior tests and repeated-measures Bayesian setup. Existing visual fallbacks also matched the baseline. No snapshots were accepted or changed.

Grouped posterior axis labels are prepared while names remain encoded, before recipe decoding. A regression test checks Unicode names and interaction labels through jaspBase’s decoded materialization path.
