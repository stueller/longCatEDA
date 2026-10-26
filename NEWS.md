# longCatEDA 1.0.0

This release modernizes the package code for resubmission to CRAN.

## Breaking changes

* Plots are now methods for the generic `plot()`: `plot(lc)` replaces
  `longCatPlot(lc)`, and `plot(longCont(y, times))` replaces
  `longContPlot(y, times)`. `longCatPlot()` and `longContPlot()` still work
  but are deprecated: they warn and will be removed in a future release.
* Growth curves (`plot()` of a `longCont` object, formerly
  `longContPlot()`) label the x-axis "Time" and leave the y-axis
  unlabelled by default, rather than showing the internal names `txx` and
  `y[1, ]`.
* `longCat` objects are now plain S3 objects. The previous S4 class
  definition, whose slots did not match the objects actually returned, has
  been removed. Fields are still accessed with `$` (e.g., `lc$y`).
* `longCat()`, `sorter()`, and `plot.longCat()` now check objects with the
  new `validate_longCat()`. Hand-edited objects that are internally
  inconsistent now raise an error that lists every problem. In particular,
  setting `lc$sorted <- TRUE` to plot unsorted data no longer works; use
  `plot(lc, sort = FALSE)` instead.
* `longCat()` now errors (rather than warns) when `tLabels` does not have one
  label per column of `y`, when `Labels` does not have one label per state,
  or when `eventLabels` does not have one label per distinct event.
  Previously a wrong number of `Labels` gave a warning and a mislabelled
  legend.
* `longCat()` now sets default `Labels` to the original state values
  (e.g., `0` and `1`), not their codes (`1` and `2`). Previously, when a
  state such as 4 never occurred, state 5 was labelled `4`.
* `longCat()` now sets default `eventLabels` in the order of the event
  codes (sorted), not the order in which events first appear; previously
  labels could be attached to the wrong events.
* `longCat()` stores events as numeric codes. Text events used to keep
  text codes, so they were plotted as the characters `"1"`, `"2"`, ...
  rather than plotting symbols.
* The methods package is no longer attached when longCatEDA is loaded.
* `levelCheck()` and `lunique()` are no longer exported. They were internal
  helpers for `longCat()`, and `lunique()` miscounted matrices and data
  frames (it counted the cells of unique rows rather than unique values).
* `norpt()` is no longer exported; it was a helper for
  `makePatterns(igrpt = TRUE)`.
* `makePatterns()` now returns a plain vector with one pattern per row,
  as documented, instead of a one-column matrix.
* `getN()` and `clean.events()` are no longer exported. `getN()` was a
  helper for `alignTime()` (which used its own, different copy), and
  `clean.events()` a helper for `plot.longCat()`; its documentation also
  described it incorrectly.
* `alignTime()` now always returns numeric matrices (data frame input used
  to give data frame output), and `aligned.event.times` is `NULL` when
  there are no events (previously `numeric(0)`).
* `sort1()` has been removed. It was a helper for `sorter()`, which now
  orders all cases at once instead of looping over groups.
* `sorter()` now returns its sorted components (`y.sorted`,
  `times.sorted`, `group.sorted`, `events.sorted`, `event.times.sorted`)
  as matrices; with groups they used to be data frames. `order.y.sorted`
  is still a data frame.
* `sorter()` now leaves cases with a missing `group` out of the sorted
  data, as its warning always said. Previously each such case added a row
  of `NA`s to every group.
* `sorter()` now sets default `groupLabels` to the group values (or factor
  levels present), so groups are labelled in plots; a wrong number of
  `groupLabels` is an error.
* `plot.longCat()`'s x-axis now covers the actual time range, and
  `groupBuffer` is the proportion of that range kept free for group labels
  (it used to be scaled oddly, `0.25 * ceiling(groupBuffer * max(times))`
  time units). Axis ticks are not drawn in the group-label margin.
* `plot.longCat()` places the default y-axis label close to the lines unless
  there are groups, as intended in version 0.17 (it was always placed as if
  there were groups once the data had been sorted).
* `plot(lc, event.pch = )` now gives the plotting character of each
  event code (in `eventLabels` order). It used to be matched to the codes
  present, so after `which.events` removed an event the characters shifted.
* `colChoose()` now returns exactly `nfactors` colors. It previously
  returned 3, 5, or 9 colors depending on `nfactors` (only the first
  `nfactors` were used).
* `mindur` now does what Tueller, Van Dorn, and Bobashev (2016) describe:
  in `sorter()` and `makePatterns()`, a state observed for less than
  `mindur` (`times[, j + 1] - times[, j] < mindur`) is ignored for sorting.
  Previously every cell within `mindur` of a case's first time was ignored,
  whatever its duration. For `makePatterns()`, `times` now gives the
  interval boundaries, with `ncol(dat) + 1` columns as in `longCat()`, and
  may be a vector shared by all rows.
* `sorter()` now compares cases' state sequences column by column instead
  of sorting on the concatenated patterns from `makePatterns()`. The order
  is unchanged for data with states 1 to 9 and up to 15 time points. Beyond
  that, the old patterns gave wrong orders: states of 10 or more were split
  into digits (rows `(1, 12)` and `(11, 2)` both gave `"112"`), and numeric
  patterns keep only about 15 significant digits, so cases that differed
  only after the 15th time point were treated as ties. See `?sorter` for the
  ordering rules.

## New features

* New `longCont` class for continuous longitudinal data: `longCont()`
  creates it, `validate_longCont()` checks it, and it has `print()`,
  `summary()`, and `plot()` methods. `longCont()` accepts the interval
  times given to `longCat()`, so the same `times` work for both.
* `plot()` of a `longCont` object gains `col`, `lty`, and `lwd` (recycled
  across cases), and `jog` may be a number giving the size of the jog.
* New `as.data.frame()` methods return `longCat` and `longCont` objects
  in long format (one row per case and time point) for use with ggplot2,
  dplyr, and other tools. For `longCat` objects, `what = "events"` gives
  the events, and sorted objects come out in plot order with the plot row
  and group.
* New `as.longCat()` and `as.longCont()` generics build objects from
  long-format data frames (one row per case and time, or per interval with
  `stop`), with events from a long data frame of events. They reverse
  `as.data.frame()`, and keep the order of factor levels.
* Checking that each case's times do not decrease is vectorized, which
  speeds up `longCat()`, `sorter()`, and `plot()` for large data
  (for 200,000 cases at 10 time points, it took 0.9 seconds per call).
* New `validate_longCat()` checks the structure of a `longCat` object.
* `sorter()` implements the secondary sort described by Tueller, Van Dorn,
  and Bobashev (2016): with individually varying times, cases with the same
  sequence of states are ordered by the time spent in each state. The new
  argument `sortDurations` controls this (by default it is on when times
  vary across cases).
* `plot.longCat()` gains `lty`, a line type for each state, used in the
  plot and the legend (#3), and `xlim` and `ylim` arguments. `event.col`
  may now give one color per event.
* `plot.longCat()`'s default y-axis label shows the number of cases with
  thousands separators (#2), counting only the cases plotted.
* `plot.longCat()` draws all lines with one call instead of one call per
  cell, about 20 times faster for large data.
* `colChoose()` documents all of its schemes (including `0` to `3`,
  `"oldheat"`, and `"coldheat"`) and defaults to `colScheme = "heat"`.
* Added a package help page, `?longCatEDA`.
* New `print()` method gives a compact description of a `longCat` object.
* `summary()` now returns a `summary.longCat` object reporting state, group,
  and event frequencies.

## Bug fixes

* Growth curves (formerly `longContPlot()`) honour a user `xlim`, which was
  always replaced by the range of the times; widen the y-axis to fit
  jogged lines, which were clipped; plot each case at its own times when
  `times` is a matrix with `ncol(y) + 1` columns (this failed with "'x'
  and 'y' lengths differ"); and give a clear error for `times` of the
  wrong length (previously "incorrect number of dimensions").
* `longCat()` stores character input (e.g., a matrix of `"0"`/`"1"`) as a
  numeric matrix; previously `y` stayed a character matrix.
* `longCat()` gives a state the same code in every column. Factor columns
  were coded column by column, so a state could have different codes (and
  colors) in different columns; now all columns follow the order of the
  factor levels. Text states that look like numbers are ordered
  numerically (`"9"` before `"10"`). See `?longCat` for the ordering rules.
* `longCat()` warns that `tLabels` are ignored when times vary across cases
  (they were dropped silently), and gives clear errors when `events` or
  `event.times` is missing, when `y` has no observed values, or when an
  argument has the wrong shape.
* `longCat()` now warns whenever `y` has more than 9 distinct states in
  total, the number of colors in the built-in color schemes. Previously it
  only warned when a single column had 10 or more, so data with, e.g., 5
  states per column but 10 overall had state 10 silently left undrawn. The
  warning message, which was garbled, has been rewritten.
* `colChoose()` now always returns one valid color per state:
  * `colScheme = "gray"` returned `NA` colors for 2 to 5 states (and for
    1 to 8 states with `reverse = TRUE`), so those states were not drawn.
  * For more than 9 states, every scheme is now extended (built-in
    generators produce more colors; fixed schemes are interpolated).
    Previously states above 9 got `NA` and were not drawn.
  * An unknown `colScheme` now gives an error listing the valid schemes
    instead of "object 'cols' not found"; `nfactors` and `reverse` are
    checked.
  For 1 to 9 states the colors are unchanged.
* `sorter(igrpt = TRUE)` and `makePatterns(igrpt = TRUE)` no longer lose
  data around missing values. The value following an `NA` was dropped (a row
  `c(1, NA, 2, 2)` sorted as if it were `1`), a leading `NA` displaced the
  first run, and rows of length 1 caused an error. Runs of repeated values
  are now collapsed with each missing value ending a run, and missing values
  are then dropped, so `c(1, 1, NA, 1, 2)` sorts as `c(1, 1, 2)`.
* `makePatterns(igrpt = TRUE)` returned a single pattern for the whole
  matrix when `dat` had one column, so such data were not sorted. Runs are
  now collapsed for all rows at once, which also makes `igrpt = TRUE` about
  ten times faster on large data.
* `sorter(whichColumns = )` with a single column no longer errors.
* `sorter()` and `plot.longCat()` no longer fail when `events` has one
  column (#6), or when a group has a single case.
* A `ggap` given to `sorter()` is now used; it was always replaced by 0.05
  (#1, which had regressed).
* `sorter()` checks `whichColumns`, `customSort`, `group`, `ggap`, and its
  logical arguments, with clear messages.
* `plot.longCat()` no longer prints a stray "1" over the plot title (a value
  was passed to `title()` unnamed).
* `plot(lc, ylim = )` works; it failed with "formal argument "ylim"
  matched by multiple actual arguments". The function also no longer looks
  up `ylim` and `cex.axis` in the user's workspace.
* `plot.longCat()` plots times that are not whole numbers correctly (e.g.,
  times from 0 to 0.6 were drawn on an axis from 0 to 1).
* `plot(lc, n.events = )` equal to the number of event columns no
  longer fails, and `which.events` and `n.events` now also work with a
  single column of events (they were silently ignored).
* `plot.longCat()` checks `cols`, `legendBuffer`, `groupBuffer`, `texclude`,
  `which.events`, and `n.events`.
* `makePatterns()` now errors when `mindur` is given without `times`
  (previously `mindur` was ignored silently unless `times` was a matrix),
  and checks its arguments.
* `alignTime()` accepts `times` as a vector shared by all cases (as
  `longCat()` does), checks its arguments with clear messages (e.g.,
  aligning at an event without `events` failed with "dim(X) must have a
  positive length"), and documents that cases without the nth occurrence
  are aligned at their last observed time.
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
