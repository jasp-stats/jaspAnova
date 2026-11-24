# jaspAnova Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspAnova (development version)

## Changed
* The Contrasts effect size info text and the help files for ANOVA, ANCOVA, and Repeated Measures ANOVA now describe how Cohen's d is standardized, and the Contrasts output section documents the Cohen's d and its confidence interval columns.
* Normality assumption in the help and info text now refers to the residuals being normally distributed instead of the dependent variable, for Repeated Measures ANOVA (frequentist and Bayesian) and MANOVA (multivariate normality of residuals). Also corrected the Kruskal-Wallis description and Q-Q plot interpretation text in ANOVA and ANCOVA to refer to residuals.
* All analyses in the module now use data preloaded by JASP, so variables arrive with the type set in the variable lists instead of being converted in R. Rows with a missing value on any analysis variable are still excluded listwise, and factor levels left empty by that exclusion are dropped. Repeated measures cells that are ordinal or nominal with text labels now give an error asking to set them to Scale, because JASP passes their labels instead of their values ([PR #546](https://github.com/jasp-stats/jaspAnova/pull/546)).

## Fixed
* ANOVA and ANCOVA now correctly apply the selected WLS Weights variable when its column type is included in the analysis options. Previously, weights were silently ignored; refresh affected saved analyses to obtain weighted results ([PR #589](https://github.com/jasp-stats/jaspAnova/pull/589)).
* Cohen's d for contrasts in ANOVA, ANCOVA, and Repeated Measures ANOVA is now invariant to the scale of the contrast weights: weights are rescaled so their absolute values sum to 2 before the effect size is computed, so e.g. `(1, 0, 1, -2, 0)` and `(0.5, 0, 0.5, -1, 0)` yield the same d. A footnote on the contrast table states this. This also changes d for the built-in deviation and polynomial contrasts ([Issue #4488](https://github.com/jasp-stats/jasp-issues/issues/4488)).
* Repeated Measures ANOVA showed η², partial η², ω², and partial ω² in the wrong rows of the Within Subjects Effects table when the design mixed 2-level and 3+-level factors (e.g., 2 × 3 × 2). Each effect size is now matched to its own term ([Issue #4704](https://github.com/jasp-stats/jasp-issues/issues/4704)).

## Added
* Partial η² and partial ω² (with optional CIs) as effect size estimates for the Simple Main Effects table in ANOVA, ANCOVA, and Repeated Measures ANOVA, matching SPSS's simple effects effect size.

---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
