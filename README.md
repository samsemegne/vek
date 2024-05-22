# Vek
***Predicate Helper Functions for Testing Atomic Vectors in R***

## Installation
```
install.packages("vek")
```

## About
All functions take a single argument `x` and check whether it's of the target
type of base-R atomic vector, returning `TRUE` or `FALSE`. Some additionally
check for value. Classes that extend any base-R atomic vector return `FALSE`.
Vectors that carry any attributes other than 'names' return `FALSE`.

Function names may include a suffix that encodes what additional conditions
are evaluated. A select combination of these conditions is provided for each
type. Naming scheme:

* n: no 'names' attribute
* x: no `NA` (type specific)
* y: no `NaN`
* z: no `Inf` or `-Inf`
* b: no blank characters, i.e. `""`
* 1: is of length 1

For example:
* `is_dbl_vec(x)` evaluates whether `x` is a base-R typeof double atomic vector,
of any length.
* `is_dbl_vec_xz(x)` will additionaly evaluate whether `x` contains no `NA`
nor `Inf` values. Note, `NaN` values are still allowed here, which is dissimilar
to behavior from base-R functions like `is.na(x)` or `anyNA(x)`, wherein both
`NA` and `NaN` values yield `TRUE`.
* `is_num_vec_xyz1(x)` effectively evaluates whether `x` is a single real
number.

Supported types: logical, integer, double, numeric, and character.