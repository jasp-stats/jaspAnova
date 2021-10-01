Syntax for Creating Order Restrictions
===

### Order Restrictions
A special syntax is necessary to specify order restrictions in the General Linear Model. Order restrictions mean that effects in a model are required to be equal or unequal. For example, when performing an ANOVA with three groups, the following order constraint could be applied:

&mu;<sub>Low</sub> < &mu;<sub>Med</sub> < &mu;<sub>High</sub>.

This is an example for an *inequality constraint* and restricts the mean of the first group to be smaller than the mean of the second group, which must be smaller than the mean of the third group. The null-model for the ANOVA could also be expressed as an *equality constraint*:

&mu;<sub>Low</sub> = &mu;<sub>Med</sub> = &mu;<sub>High</sub>,

which means that all three groups must have the same mean.

### Basic Syntax
To specify an order constraint, the (in-) equality between two coefficients must be expressed using `==`, `<`, or `>` operators. Coefficients can be accessed by their name in the data set. For instance:  

`variable1 == variable2`,  

indicates that the coefficient (effect) for `variable1` is equal to the coefficient for `variable2`. Coefficients can also be constrained to a constant, e.g., `variable1 == 0`.

#### Factors
Factor variables can have several coeffcients (usually k-1, with k being the number of levels). They can be accessed by the factor variable name immediately followed by the respective level:  

`factorLow == factorHigh`.  

#### Intercept
The intercept can be accessed using `.Intercept.`.

#### Example
If dummy coding was used, the syntax for the inequality constraint shown in the beginning could look like:  

`.Intercept. < .Intercept. + factorMed`  
`.Intercept. + factorMed < .Intercept. + factorHigh`.  

The constraint is split up in relations between two coefficients and each relation is written on a new line. Alternatively, they can also be put on the same line and separated with a semicolon (i.e., `;`).
The above equality constraint could be simplified as:  

`factor2 == 0; factor3 == 0`,  

which means that level `Med` and `High` do not differ from level `Low` (the intercept).

### Advanced Syntax
#### Interactions
Constraints on interaction effects can be specified using `variable1.variable2` to refer to an interaction term between `variable1` and `variable2`.

#### Defining New Parameters
New parameters can be defined as a function of the existing parameters in the model using the `:=` (assign) operator. For example, a constraint on the difference between two factor levels could be specified as:  

`diff := factorMed - factorHigh`  
`diff > 0`.

#### Repeated Measures
Constraints on coefficients of within-subject factors can be specified similar to between-subject factors, e.g.:

`withinLevel1 == withinLevel2`.  

Note that there is no intercept in the model when only within-subject factors are included. If two interacting within-subject factors are specified, only their interaction coefficients can be accessed, e.g.:

`within1Level1.within2Level1 == within1Level2.within2Level2`.  

Here `within1Level1 == within2Level2` would result in an error. If both within- and between-subjects terms are in the model, their coefficients can also only be accessed through interactions:

`betweenLevel2.withinLevel1 == betweenLevel2.withinLevel2`.  

Note that between-subject terms always have to be specified before within-subject terms. The first level of between subject factors is the intercept and can be accessed as `.Intercept.`:

`.Intercept..withinLevel1 == .Intercept..withinLevel2`.

``

### Summary
The syntax for specifying order constraints contains the following elements:
- `==` for equality constraints
- `<` and `>` for inequality constraints
- `.Intercept.` to access the intercept
- `variable1.variable2` to access interaction terms
- `:=` to define new parameters
- Interaction syntax if both within- and between-subject terms are included

### Examples
For further information and examples, see: https://restriktor.org/tutorial/syntax.html
