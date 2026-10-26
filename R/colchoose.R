#' Choose colors for the states in a horizontal line plot
#'
#' Returns one color per state from a built-in color scheme. [longCatPlot()]
#' calls `colChoose()` unless colors are supplied with its `cols` argument.
#'
#' Most schemes are defined by 9 colors. For 5 or fewer states, colors are
#' spread across the 9 (positions 1, 5, 9 for up to 3 states and 1, 3, 5, 7,
#' 9 for 4 or 5) so neighboring states contrast; for 6 to 9 states the first
#' colors are used. For more than 9 states, the scheme is extended to as many
#' colors as there are states (by interpolating between the 9 colors for the
#' fixed schemes), although so many states are hard to tell apart in a plot.
#'
#' @param colScheme The color scheme, one of:
#'   * `"heat"` (the default): [heat.colors()], from light to dark.
#'   * `"gray"`: grays from black to light gray, `gray(seq(0, 0.9, length.out
#'     = nfactors))`; not spread.
#'   * `"rainbow"`, `"terrain"`, `"topo"`, `"cm"`: [rainbow()],
#'     [terrain.colors()], [topo.colors()], and [cm.colors()].
#'   * `"oldheat"`: a manual heat spectrum from purple to dark red.
#'   * `"coldheat"`: a manual heat spectrum from dark blue to red.
#'   * `0`: colors 1 to 8 of the current [palette()] followed by
#'     `"darkgreen"`; not spread.
#'   * `1`: red, orange, gold, green, light blue, dark blue, violet, black,
#'     pink.
#'   * `2`: pairs of dark and light orange, olive, orchid, and blue, then
#'     black.
#'   * `3`: pairs of dark and light green, pink, brown, and red, then black.
#'
#'   The numbered schemes may also be given as strings (e.g. `"1"`).
#' @param nfactors The number of states (`lc$nfactors` for a [longCat()]
#'   object).
#' @param reverse Logical. If `TRUE`, the scheme is reversed before colors are
#'   chosen. Default is `FALSE`.
#'
#' @return A character vector of `nfactors` colors.
#'
#' @examples
#' colChoose("heat", 5)
#' colChoose("gray", 3, reverse = TRUE)
#'
#' # compare the schemes for 5 states
#' schemes <- c("heat", "gray", "rainbow", "terrain", "topo", "cm",
#'              "oldheat", "coldheat", "0", "1", "2", "3")
#' op <- par(mar = c(1, 6, 1, 1))
#' plot(NULL, xlim = c(0, 5), ylim = c(0, length(schemes)), axes = FALSE,
#'      xlab = "", ylab = "")
#' for (i in seq_along(schemes)) {
#'   rect(0:4, i - 1, 1:5, i - 0.1, col = colChoose(schemes[i], 5),
#'        border = NA)
#' }
#' axis(2, at = seq_along(schemes) - 0.55, labels = schemes, las = 1,
#'      tick = FALSE)
#' par(op)
#'
#' # use a scheme in a plot
#' times <- c(1, 100, 200, 300, 400, 500, 600)
#' longCatPlot(longCat(example3, times), colScheme = "gray", lwd = 2,
#'             main = "colScheme = 'gray'")
#'
#' @seealso [longCatPlot()], whose `cols` argument accepts any colors.
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016).
#'   Visualization of categorical longitudinal and times series data (Report
#'   No. MR-0033-1602). Research Triangle Park, NC: RTI Press.
#'   \doi{10.3768/rtipress.2016.mr.0033.1602}
#' @author Stephen J. Tueller
#' @md
#' @export
colChoose <- function(colScheme = "heat", nfactors, reverse = FALSE) {
  scheme <- match_color_scheme(colScheme)
  if (!is_whole_scalar(nfactors) || nfactors < 1) {
    stop("`nfactors` must be a single positive integer.", call. = FALSE)
  }
  if (!is_flag(reverse)) stop("`reverse` must be TRUE or FALSE.", call. = FALSE)
  n <- as.integer(nfactors)
  spec <- longcat_color_schemes[[scheme]]

  if (!spec$base9) {
    cols <- spec$colors(n)
    return(if (reverse) rev(cols) else cols)
  }

  cols <- spec$colors(max(longcat_max_states, n))
  if (reverse) cols <- rev(cols)
  if (spec$spread && n <= 5L) {
    cols <- cols[if (n <= 3L) c(1, 5, 9) else c(1, 3, 5, 7, 9)]
  }
  cols[seq_len(n)]
}

# ---- scheme table -----------------------------------------------------------

# A scheme defined by fixed colors, extended by interpolation when more are
# needed.
fixed_palette <- function(base) {
  force(base)
  function(k) {
    if (k == length(base)) base else grDevices::colorRampPalette(base)(k)
  }
}

# Each scheme gives `colors(k)`, a function returning k colors. `base9`
# schemes are defined by 9 colors (more only when there are more than 9
# states); `spread` schemes spread their 9 colors over 5 or fewer states.
longcat_color_schemes <- list(
  heat = list(colors = function(k) rev(heat.colors(k, alpha = 1)),
              base9 = TRUE, spread = TRUE),
  gray = list(colors = function(k) gray(seq(0, 0.9, length.out = k)),
              base9 = FALSE, spread = FALSE),
  rainbow = list(colors = function(k) rainbow(k),
                 base9 = TRUE, spread = TRUE),
  terrain = list(colors = function(k) terrain.colors(k, alpha = 1),
                 base9 = TRUE, spread = TRUE),
  topo = list(colors = function(k) topo.colors(k, alpha = 1),
              base9 = TRUE, spread = TRUE),
  cm = list(colors = function(k) cm.colors(k, alpha = 1),
            base9 = TRUE, spread = TRUE),
  oldheat = list(colors = fixed_palette(c(
    "purple4", "royalblue", "paleturquoise3", "palegreen", "yellow",
    "orange", "orangered", "maroon", "red4")),
    base9 = TRUE, spread = TRUE),
  coldheat = list(colors = fixed_palette(c(
    "blue4", "dodgerblue2", "green4", "darkseagreen3", "yellow4", "tan1",
    "orange", "orangered", "red2")),
    base9 = TRUE, spread = TRUE),
  "0" = list(colors = fixed_palette(c(as.character(1:8), "darkgreen")),
             base9 = TRUE, spread = FALSE),
  "1" = list(colors = fixed_palette(c(
    "darkred", "darkorange", "darkgoldenrod2", "darkgreen", "lightblue",
    "darkblue", "blueviolet", "black", "hotpink")),
    base9 = TRUE, spread = TRUE),
  "2" = list(colors = fixed_palette(c(
    "orange4", "orange", "olivedrab", "olivedrab1", "mediumorchid4",
    "mediumorchid1", "royalblue4", "royalblue1", "black")),
    base9 = TRUE, spread = TRUE),
  "3" = list(colors = fixed_palette(c(
    "forestgreen", "green", "deeppink4", "hotpink1", "chocolate4",
    "darkorange", "red4", "orangered", "black")),
    base9 = TRUE, spread = TRUE)
)

match_color_scheme <- function(colScheme) {
  if (is.numeric(colScheme) && length(colScheme) == 1L && !is.na(colScheme)) {
    colScheme <- as.character(colScheme)
  }
  if (!is.character(colScheme) || length(colScheme) != 1L ||
      !colScheme %in% names(longcat_color_schemes)) {
    stop("`colScheme` must be one of: ",
         paste0('"', names(longcat_color_schemes), '"', collapse = ", "), ".",
         call. = FALSE)
  }
  colScheme
}
