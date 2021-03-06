[![Build Status](https://travis-ci.org/mb706/typer.svg?branch=master)](https://travis-ci.org/mb706/typer)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/typer)](https://CRAN.R-project.org/package=typer)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/typer)](https://cran.rstudio.com/web/packages/typer/index.html)


# TypeR: Type Annotations for R

## What is this?

When writing an R package, it may be important to check that the types of certain values are as expected. This is especially true for user-facing functions, since otherwise giving wrongly typed arguments can lead to cryptic error messages. `typer` makes these checks automatic, using short and legible type annotations in declared functions.

## Usage

An example function with type annotations is

```r
fun = function(a = NULL : numeric(1) [[ a > 0 ]] | NULL) { a }
```
This function accepts one argument that may either be `NULL` or a numeric length 1 vector with a positive value. To compile a single function so that it uses its type decorators, use `compileFunction()`.

```r
library("typer")
compfun = compileFunction(fun, name = "compfun")

compfun()
#> NULL

compfun(1)
#> [1] 1

compfun("a")
#> Error in compfun("a"): Argument 'a' must be of format 'numeric(1)' and the expression
#>       a > 0
#>     must evaluate to TRUE
#>   or
#>     'a' must be of format 'NULL'

compfun(-1)
#> Error in compfun(-1): Argument 'a' must be of format 'numeric(1)' and the expression
#>       a > 0
#>     must evaluate to TRUE
#>   or
#>     'a' must be of format 'NULL'
```

`compileFunction()` is mostly useful for interactive development; to use type decorators in packages, the `compileTypes()` function should be used: It compiles all functions declared in its current environment. It should be called *after* all other functions have been created, e.g. in a file called `zzz.R`. The code generated by `typer` uses the `checkmate` package, so a package using `typer` should declare `checkmate` in its `DESCRIPTION` file in the `Imports` section.

The full description of annotation possibilities can be read in the help page of `compileTypes()`:

```r
?compileTypes
```

## Installation

`typer` is currently only on GitHub, so using the `devtools` package, do

```r
devtools::install_github("mb706/typer")
```

## Future Changes

`typer` is currently only a small tool which may or may not undergo future changes. Possibilities are, in order of decreasing likelihood:

* Syntax refinement
* Decoration of return values (possibly using dummy argument names `RETURNTYPE`)
* Static / interactive switching between slow checking and fast not-checking mode
* Grouping of functions between front-end (always checked) and back-end (only checked during debug), possibly using dummy argument `TYPERGROUP`
* Recognizing more special classes / modes
* Compatibility with the `types` package
* Using a different backend than `checkmate`
* Class structure definitions
* Type inference

## License

This project is licensed under the terms of the [GNU AGPL v3](https://www.gnu.org/licenses/agpl-3.0.html).
