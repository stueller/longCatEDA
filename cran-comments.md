## Resubmission of an archived package

longCatEDA was archived on 2024-04-20 because the problems in the CRAN
check results for version 0.31 were not corrected in time. This is a
resubmission (version 1.0.0) by the same maintainer, with those problems
fixed and the package code rewritten.

The problems reported on 2024-01-22, and their fixes:

* **Examples ERROR** (r-devel-linux-x86_64-debian-clang): the `sorter()`
  example called `library(ggplot2)`, and ggplot2 was not available on the
  check machine. ggplot2 is still only suggested. Every use of it in the
  examples is now guarded with `requireNamespace("ggplot2", quietly = TRUE)`,
  the tests that need it use `testthat::skip_if_not_installed("ggplot2")`,
  and the new `autoplot()` methods are registered only when ggplot2 is
  installed (delayed S3 registration). The examples pass with only the
  Depends and Imports packages available (`_R_CHECK_DEPENDS_ONLY_=true`).
* **Rd contents NOTE** (r-devel flavors): "Argument items with no
  description in Rd file 'norpt.Rd': 'alist'". `norpt()` was an internal
  helper and has been replaced by an unexported function, so `norpt.Rd` no
  longer exists. All documentation is now generated with roxygen2, and
  every argument is documented.
* **Package dependencies NOTE** ("Package suggested but not available for
  checking: 'ggplot2'"): with the changes above, the package checks cleanly
  without ggplot2.

Other changes, listed in full in NEWS.md:

* Plotting now uses methods for the generic `plot()` (`plot.longCat()`,
  `plot.longCont()`) instead of the functions `longCatPlot()` and
  `longContPlot()`, which remain as deprecated wrappers.
* Objects are S3 classes with validators, `print()`, `summary()`,
  `as.data.frame()` and `[` methods.
* New features: case weights, side-by-side plots of two variables, and
  conversion from long-format data.
* The methods, MASS, RColorBrewer and colorspace packages are no longer
  used.
* A testthat test suite has been added.

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  New submission. Package was archived on CRAN.

  This is the resubmission of the archived package explained above.

* checking top-level files ... NOTE
  Files 'README.md' or 'NEWS.md' cannot be checked without 'pandoc' being
  installed.

  This comes from the local check machine, which does not have pandoc.

## Test environments

* Local: Windows 11, R 4.6.1, `R CMD check --as-cran`.
* Local: Windows 11, R 4.6.1, with only the Depends and Imports packages
  available (`_R_CHECK_DEPENDS_ONLY_=true`, tests not run because testthat
  is suggested).

## Reverse dependencies

There are no reverse dependencies on CRAN.
