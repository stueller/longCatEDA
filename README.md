longCatEDA
==========

R package for exploratory data analysis (EDA) with longitudinal or time-series categorical data (ordinal or nominal)

Using ggplot2
-------------

`plot()` draws horizontal line plots with base graphics. When the
[ggplot2](https://ggplot2.tidyverse.org) package is installed, longCatEDA
also works with ggplot2; ggplot2 is suggested, not required.

**A complete plot.** `autoplot()` returns the plot of `plot()` as a
`ggplot` object, to change with further layers, scales, and themes. It
takes the same options for colors, events, alignment, and `texclude`.
Groups are drawn as facets.

```r
library(longCatEDA)
library(ggplot2)

lc <- longCat(example3, Labels = c("Home", "School", "Work", "Jail", "Other"))
s <- sorter(lc, group = rep(c("Treatment", "Control"), 50))
autoplot(s, colScheme = "topo") + theme_minimal()

# aligned at each person's first time in jail
autoplot(lc, which.state = 4, nth.state = 1, xlab = "Days since jail")
```

**Two variables of the same people.** As with `plot(lc1, lc2)`, the cases
are in the same order in both panels, so each person's row lines up.

```r
set.seed(3)
use <- longCat(pmax(pmin(example3 + matrix(sample(-1:1, 600, TRUE), 100), 5), 1))
autoplot(s, use, y_args = list(colScheme = "topo"),
         titles = c("Living situation", "Drug use"))
```

**Your own plots.** `ggplot()` takes a `longCat` object as data: the
cases are sorted as `plot()` sorts them, and each case's plot row (`row`)
and line or band (`y`, `ymin`, `ymax`) are added. `scale_colour_longcat()`
and `scale_fill_longcat()` give the colors of `plot()`.

```r
ggplot(lc, aes(x = start, xend = stop, y = y, yend = y, colour = state)) +
  geom_segment() +
  scale_colour_longcat()
```

**Long-format data.** Data with one row per person and time can be sorted
and aligned without reshaping. `sorter()` and `alignTime()` keep every
column and add the plot row and aligned times.

```r
long <- data.frame(pid = rep(1:100, each = 6), wave = rep(1:6, 100),
                   status = c(t(example3)),
                   site = rep(c("North", "South"), each = 300))
d <- sorter(long, id = "pid", time = "wave", state = "status", group = "site")
ggplot(d, aes(x = wave, xend = wave + 1, y = row, yend = row,
              colour = factor(status))) +
  geom_segment() +
  scale_colour_longcat(name = "Status") +
  facet_grid(site ~ ., scales = "free_y", space = "free_y")

d <- alignTime(d, id = "pid", time = "wave", state = "status",
               which.state = 4, nth.state = 1)
head(d)   # with the column aligned_time
```

`as.data.frame()` gives the long data of a `longCat` object, and
`as.longCat()` builds one from long data.
