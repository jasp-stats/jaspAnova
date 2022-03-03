# Syntax for Creating Order Restrictions

### Order Restrictions

A special syntax is necessary to specify order restrictions in the General Linear Model. Order restrictions mean that effects in a model are required to be equal or unequal. For example, when performing an ANOVA with three groups, the following order constraint could be applied:

μ<sub>Low</sub> \< μ<sub>Med</sub> \< μ<sub>High</sub>.

This is an example for an *inequality constraint* and restricts the mean of the first group to be smaller than the mean of the second group, which must be smaller than the mean of the third group. The null-model for the ANOVA could also be expressed as an *equality constraint*:

μ<sub>Low</sub> = μ<sub>Med</sub> = μ<sub>High</sub>,

which means that all three groups must have the same mean.

### Basic Syntax

To specify an order constraint, the (in-) equality between two coefficients must be expressed using `=`/`==`, `<`, or `>` operators. Coefficients can be accessed by their name in the data set. For instance:

`variable1 == variable2`,

indicates that the coefficient (effect) for `variable1` is equal to the coefficient for `variable2`. Coefficients can also be constrained to a constant, e.g., `variable1 == 0`.

The constraints are set up as relations between two coefficients and each relation is written on a new line. Alternatively, they can also be put on the same line and separated with a semicolon (i.e., `;`). It is possible to specify multiple relations simultaneously, e.g. `variable1 < variable2 = variable3` is the same as writing `variable1 < variable2; variable2 = variable3`.

### Available coefficients

Coefficients that are available in the syntax depend on the model and the data. Tick "Show available coefficients" to display the list of coefficients that are available.

#### Define custom coefficients

It is possible to define a custom parameter using `:=` operator and then subsequently use it further in the model. For example `newPar := variable1 + variable2; newPar = 0` defines a new parameter called `newPar`, which is the sum of coefficients associated with predictors `variable1` and `variable2`. The second statement sets `newPar` to zero, which effectively implements a sum-to-zero constraint of the two coefficients.

#### Factors

Factor predictors are represented by multiple coefficients, depending on the number of levels in the factor. They can be accessed by the factor variable name immediately followed by the respective level:

`factorLow == factorHigh`.

Factors are dummy coded.

#### Intercept

When the intercept is included in the model, it can be accessed using the `.Intercept.` keyword. The reference level of the first factor in the model would then correspond to this `.Intercept.` coefficient.

#### Example

Consider a factor called `factor` with three levels: `Low`, `Med`, `High`. We will construct the inequality constraint μ<sub>Low</sub> \< μ<sub>Med</sub> \< μ<sub>High</sub>.

If the intercept is included in the model, the reference category (`Low`) is estimated as the `.Intercept.`. The factors are dummy coded, so the mean of the `Med` category is equal to `.Intercept. + factorMed`, and the mean of the `High` category is equal to `.Intercept. + factorHigh`. That is, `factorMed` and `factorHigh` here refer to the difference between the respective category and the reference category. The restriction could be implemented as follows:

`.Intercept. < .Intercept. + factorMed`

`.Intercept. + factorMed < .Intercept. + factorHigh`.

The syntax could be simplified as:

`factorMed > 0`

`factorHigh > factorMed`.

Alternatively, we could define new parameters that correspond to the means of the three categories, and then specify the constraint in terms of those:

`low  := .Intercept.`

`med  := .Intercept. + factorMed`

`high := .Intercept. + factorHigh`

`low < med < high`.


In case that the intercept is not included in the model, there are three coefficients `factorLow`, `factorMed`, `factorHigh`, which already correspond to the means of the three categories. The following syntax would yield the same results:

`factorLow < factorMed`

`factorMed < factorHigh`.

### Advanced Syntax

#### Interactions

Constraints on interaction effects can be specified using `variable1.variable2` to refer to an interaction term between `variable1` and `variable2`.

In case of interaction between factors, one needs to take into account that the factors are dummy coded, and whether or not intercept is included in the model. For example, if we have the first predictor `Group` with levels `Control` and `Experimental`, and another predictor `Fitness` with level `Low` and `High`. If we include the intercept in the model, the `.Intercept` coefficient then correspond to the first level of each factor (in this case `Control` Group and `Low` Fitness). We can define the means of the four cells in the design as:

`ControlLow := .Intercept.`

`ControlHigh := .Intercept. + FitnessHigh`

`ExperimentalLow := .Intercept. + GroupExperimental`

`ExperimentalHigh := .Intercept. + FithessHigh + GroupExperimental + FitnessHigh.GroupExperimental`,

Which makes it easier to make complex predictions in terms of the means of the cells in the design instead of the coefficients in the model.

When intercept is not included, defining the corresponding group means would be:

`ControlLow := GroupControl`

`ControlHigh := GroupControl + FitnessHigh`

`ExperimentalLow := GroupExperimental`

`ExperimentalHigh := GroupExperimental + FitnessHigh + GroupExperimenal.FitnessHigh`.

#### Repeated Measures

Constraints on coefficients of within-subject factors can be specified similar to between-subject factors. To access the coefficient associates with a repeated measures cell, use the variable name that is included in the "Repeated Measures Cells" field.

If both within- and between-subjects terms are in the model, their coefficients can also only be accessed through the dot operator `.`; the within-subject terms preceed those that are between-subjects, e.g.

`withinSubjectsLevel1.betweenFactorLevel1`.

Including or excluding the intercept has the same implications for the between-subject terms (and covariates) as in AN(C)OVA. There is no intercept for the within-subject terms. 


### Summary

The syntax for specifying order constraints contains the following elements: - `=`/`==` for equality constraints - `<` and `>` for inequality constraints - `.Intercept.` to access the intercept - `variable1.variable2` to access interaction terms - `:=` to define new parameters - Interaction syntax if both within- and between-subject terms are included

### Examples

For further information and examples, see: <https://restriktor.org/tutorial/syntax.html>
