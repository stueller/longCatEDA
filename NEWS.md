# longCatEDA 1.0.0

This release modernizes the package code for resubmission to CRAN.

## Breaking changes

* `longCat` objects are now plain S3 objects. The previous S4 class
  definition, whose slots did not match the objects actually returned, has
  been removed. Fields are still accessed with `$` (e.g., `lc$y`).
* `longCat()`, `sorter()`, and `longCatPlot()` now check objects with the
  new `validate_longCat()`. Hand-edited objects that are internally
  inconsistent now raise an error that lists every problem. In particular,
  setting `lc$sorted <- TRUE` to plot unsorted data no longer works; use
  `longCatPlot(lc, sort = FALSE)` instead.
* `longCat()` now errors (rather than warns) when `tLabels` does not have one
  label per column of `y`.
* The methods package is no longer attached when longCatEDA is loaded.

## New features

* New `validate_longCat()` checks the structure of a `longCat` object.
* New `print()` method gives a compact description of a `longCat` object.
* `summary()` now returns a `summary.longCat` object reporting state, group,
  and event frequencies.

## Bug fixes

* `longCat()` stores character input (e.g., a matrix of `"0"`/`"1"`) as a
  numeric matrix; previously `y` stayed a character matrix.
* Examples no longer fail when the suggested ggplot2 package is not installed
  (#8), and no longer use MASS.

## Infrastructure

* Added unit tests (testthat).
* graphics, grDevices, and stats are now declared in `Imports`.

# longCatEDA 0.32

* Bug fix for event `pch` options.

# longCatEDA 0.31

* Checks for monotonically increasing `times`.

# longCatEDA 0.30

* Updated handling of factor `y` and `group` inputs, with automatic
  extraction of levels for `Labels` and `groupLabels` and conversion of `y`
  and `group` to numeric.
* Corrections to x-axis tick labeling.
* Added multivariate time series data examples in the `longCatPlot()`
  documentation.
* Added an example for implementing the horizontal line plot in ggplot2 after
  sorting data using `sorter()`; see `example(sorter)`.

# longCatEDA 0.21

* Added `alignTime()` to create new `times` matrices aligned at the nth state
  in `y` or the nth event in `events`.
* `sorter()` rewritten for more efficient handling of grouped data.

# longCatEDA 0.20

* Added options to superimpose `events` over top of states that occur at
  `event.times`.
* Simplified the options for the `times` variable, deprecating the `endt`
  option.

# longCatEDA 0.17

* Added options for handling the legend outside the margins and suppressing
  the legend above the plot.
* Added handling for factor input (which is converted to numeric integers).
* Default y-axis label moved closer to the plot unless there is grouping.
* Default for `groupBuffer` in `longCatPlot()` changed to 0.
* Changed default handling of y-axis label to include sample size.
* Removed restriction to 9 categories (a warning is issued).
* Added options for controlling the length of terminal line segments with the
  `endt` option of `longCat()`. This also allows for individually varying
  times of observations with known end times (i.e., the `times` matrix has one
  more column than the data matrix).
* Expanded the examples to illustrate the use of the RColorBrewer and
  colorspace packages.

# longCatEDA 0.16

* Corrected a bug in missing data handling for the group variable in version
  0.13.
* Added error message for attempts to sort a sorted `lc` object; a code
  example is printed for the correct approach to multiple sorts.
* Added error message for missing values on `customSort` input to
  `sorter()`.
* Added funder information to the author field in DESCRIPTION.
* Added a not-run example for `longCatPlot()` showing how to speed up plot
  production for larger data sets.

# longCatEDA 0.15

* Explored automatic selection of `lwd`, with the option to weight `lwd` via
  the sampling weights. The minimum `lwd` was too small to make this useful;
  0.15 was not released.

# longCatEDA 0.14

* Explored using the seriation package instead of or with `sorter()`.
  seriation was far too slow; 0.14 was not released.

# longCatEDA 0.13

* Added utility for saving sorting order and ID variables.

# longCatEDA 0.12

* Removed restriction to 9 categories (not released; changes not carried
  into 0.13).

# longCatEDA 0.11

* Original version.
