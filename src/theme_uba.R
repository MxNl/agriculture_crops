# ggplot2 theme, colour scales and figure frame following the corporate design
# of the Umweltbundesamt (UBA), "Corporate Design des Umweltbundesamtes" (2019):
# https://www.umweltbundesamt.de/system/files/medien/421/dokumente/2019_uba_manual_screen.pdf
# colours p. 20, fonts p. 22-23, hatching p. 26, charts p. 80-81.
#
# usage: source("src/theme_uba.R")
#   plot + theme_uba()                      # charts with hatched panel
#   plot + theme_uba_map()                  # maps
#   uba_figure(plot, number = 1, source = "Quelle: ...") |> ggsave(...)
# the hatching is a grid pattern: save with ragg::agg_png, svg or cairo_pdf.

library(ggplot2)
library(dplyr)
library(purrr)
library(grid)

# colours ----------------------------------------------------------------------

# six colour pairs (light / dark), hex derived from the RGB values of the
# manual. The manual's hex for UBA Blau (#0b90d5) does not match its RGB
# (0/155/213 = #009BD5); the RGB value is used here.
uba_colours <- c(
  gruen         = "#5EAD35",
  dunkelgruen   = "#007626",
  blau          = "#009BD5",
  dunkelblau    = "#005F85",
  flieder       = "#9D579A",
  dunkelflieder = "#622F63",
  fuchsia       = "#CE1F5E",
  dunkelfuchsia = "#83053C",
  ocker         = "#FABB00",
  dunkelocker   = "#D78400",
  hellgrau      = "#F0F1F1",
  dunkelgrau    = "#4B4B4D"
)

uba_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols)) uba_colours else unname(uba_colours[cols])
}

# categorical order starts like the example chart in the manual (p. 81)
uba_order_d <- c(
  "gruen", "dunkelblau", "blau", "dunkelgruen", "ocker",
  "fuchsia", "flieder", "dunkelocker", "dunkelfuchsia", "dunkelflieder"
)

# palettes ---------------------------------------------------------------------

uba_pal_d <- function(reverse = FALSE) {
  cols <- uba_cols(uba_order_d)
  if (reverse) cols <- rev(cols)
  function(n) {
    if (n > length(cols)) {
      warning("UBA discrete palette has ", length(cols), " colours, ", n, " needed")
    }
    cols[seq_len(n)]
  }
}

# tint of a UBA colour, e.g. 20 % coverage as used for page backgrounds
uba_tint <- function(colour, coverage = 0.2) {
  grDevices::colorRampPalette(c("white", uba_cols(colour)))(101)[coverage * 100 + 1]
}

# sequential: 20 % tint -> light tone -> dark tone of one colour pair
uba_pal_c <- function(pair = "gruen", reverse = FALSE) {
  cols <- c(uba_tint(pair), uba_cols(pair, paste0("dunkel", pair)))
  if (reverse) rev(cols) else cols
}

uba_pal_div <- function(low = "dunkelblau", mid = "hellgrau", high = "dunkelfuchsia") {
  uba_cols(low, mid, high)
}

# scales -----------------------------------------------------------------------

# German number format: 1.234,5
label_number_de <- function(...) {
  scales::label_number(big.mark = ".", decimal.mark = ",", ...)
}

scale_colour_uba_d <- function(..., reverse = FALSE, aesthetics = "colour") {
  discrete_scale(aesthetics, palette = uba_pal_d(reverse), ...)
}

scale_fill_uba_d <- function(..., reverse = FALSE) {
  scale_colour_uba_d(..., reverse = reverse, aesthetics = "fill")
}

scale_colour_uba_c <- function(..., pair = "gruen", reverse = FALSE,
                               aesthetics = "colour") {
  scale_colour_gradientn(
    ..., colours = uba_pal_c(pair, reverse), aesthetics = aesthetics
  )
}

scale_fill_uba_c <- function(..., pair = "gruen", reverse = FALSE) {
  scale_colour_uba_c(..., pair = pair, reverse = reverse, aesthetics = "fill")
}

scale_colour_uba_div <- function(..., low = "dunkelblau", mid = "hellgrau",
                                 high = "dunkelfuchsia", midpoint = 0,
                                 aesthetics = "colour") {
  cols <- uba_pal_div(low, mid, high)
  scale_colour_gradient2(
    ..., low = cols[1], mid = cols[2], high = cols[3],
    midpoint = midpoint, aesthetics = aesthetics
  )
}

scale_fill_uba_div <- function(..., low = "dunkelblau", mid = "hellgrau",
                               high = "dunkelfuchsia", midpoint = 0) {
  scale_colour_uba_div(
    ..., low = low, mid = mid, high = high, midpoint = midpoint,
    aesthetics = "fill"
  )
}

# fonts ------------------------------------------------------------------------

# house fonts Meta Offc / Meta Serif Offc, official substitutes Calibri /
# Cambria. Fira Sans was derived from FF Meta and is the closest free match.
uba_font_chain <- c("Meta Offc", "Meta Office", "FF Meta", "Fira Sans",
                    "Calibri", "Carlito")
uba_font_serif_chain <- c("Meta Serif Offc", "Meta Serif Office",
                          "FF Meta Serif", "Cambria", "Caladea", "Noto Serif")

uba_pick_font <- function(chain, fallback) {
  installed <- unique(systemfonts::system_fonts()$family)
  chain |> keep(\(f) f %in% installed) |> first(default = fallback)
}

uba_font <- local({
  font <- NULL
  function() font <<- font %||% uba_pick_font(uba_font_chain, "sans")
})

uba_font_serif <- local({
  font <- NULL
  function() font <<- font %||% uba_pick_font(uba_font_serif_chain, "serif")
})

# hatching ---------------------------------------------------------------------

# UBA hatching: thin light-grey lines repeated at 135° (p. 26, p. 81)
uba_hatch <- function(spacing = 1.2, colour = "grey80", linewidth = 0.35,
                      background = "white") {
  # the tile grob is drawn in the coordinates of the panel, so the tile is
  # anchored at the panel's lower-left corner and drawn in absolute units
  tile <- unit(spacing, "mm")
  # diagonal plus two corner pieces so the lines join across tiles
  lines <- segmentsGrob(
    x0 = spacing * c(0, -0.5, 0.5), y0 = spacing * c(0, 0.5, -0.5),
    x1 = spacing * c(1, 0.5, 1.5), y1 = spacing * c(1, 1.5, 0.5),
    default.units = "mm",
    gp = gpar(col = colour, lwd = linewidth * .pt, lineend = "square")
  )
  background <- rectGrob(
    x = 0, y = 0, width = tile, height = tile, just = c("left", "bottom"),
    gp = gpar(col = NA, fill = background)
  )
  pattern(
    grobTree(background, lines),
    x = 0, y = 0, width = tile, height = tile, just = c("left", "bottom"),
    extend = "repeat"
  )
}

# themes -----------------------------------------------------------------------

# base_size: 9 pt as legend and subline in the manual; title 10 pt,
# axis text 8 pt, source/notes 6 pt
theme_uba <- function(base_size = 9, base_family = uba_font(),
                      serif_family = uba_font_serif(), hatch = TRUE) {
  ink <- "black"
  text_col <- uba_cols("dunkelgrau")

  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(colour = ink),
      plot.title = element_text(
        face = "bold", size = rel(10 / 9), hjust = 0,
        margin = margin(b = base_size / 3)
      ),
      plot.subtitle = element_text(
        size = rel(1), hjust = 0, margin = margin(b = base_size)
      ),
      plot.caption = element_text(
        family = serif_family, size = rel(6 / 9), colour = text_col,
        hjust = 1, margin = margin(t = base_size)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(base_size, base_size, base_size, base_size),

      panel.background = element_rect(
        fill = if (hatch) uba_hatch() else "white", colour = NA
      ),
      panel.grid.major.y = element_line(
        colour = "grey30", linewidth = 0.25, linetype = "22"
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      axis.text = element_text(size = rel(8 / 9), colour = ink),
      axis.title = element_text(size = rel(8 / 9), colour = ink),
      axis.line.x = element_line(colour = ink, linewidth = 0.35),
      axis.ticks.x = element_line(colour = ink, linewidth = 0.35),
      axis.ticks.length.x = unit(base_size / 3, "pt"),

      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.title = element_text(size = rel(1)),
      legend.text = element_text(size = rel(1)),
      legend.key.size = unit(base_size, "pt"),
      legend.key.spacing.x = unit(base_size * 2, "pt"),

      strip.text = element_text(face = "bold", hjust = 0, size = rel(1)),

      # UBA colours as default palettes, no scale_*_uba_*() needed
      palette.colour.discrete = uba_pal_d(),
      palette.fill.discrete = uba_pal_d(),
      palette.colour.continuous = uba_pal_c(),
      palette.fill.continuous = uba_pal_c(),
      geom = element_geom(
        ink = ink, paper = "white", accent = uba_cols("gruen"),
        family = base_family
      )
    )
}

theme_uba_map <- function(base_size = 9, base_family = uba_font(),
                          serif_family = uba_font_serif()) {
  theme_uba(base_size, base_family, serif_family, hatch = FALSE) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "right",
      legend.direction = "vertical"
    )
}

# figure frame -----------------------------------------------------------------

# frame of a UBA chart (p. 80): "Abbildung n", 1 pt rule, headline and
# subline, 0.5 pt rule, chart, 0.5 pt rule, notes (left) and source (right),
# 1 pt closing rule. Title, subtitle and caption are taken from the plot.
uba_figure <- function(plot, number = NULL, source = NULL, note = NULL,
                       label = "Abbildung", base_size = 9,
                       base_family = uba_font(),
                       serif_family = uba_font_serif()) {
  source <- source %||% plot$labels$caption
  pad <- unit(base_size / 2, "pt")

  text_row <- function(label, size, x = 0, hjust = 0, family = base_family,
                       face = "plain") {
    if (is.null(label)) return(NULL)
    textGrob(
      label, x = x, hjust = hjust,
      gp = gpar(fontsize = size, fontfamily = family, fontface = face)
    )
  }
  rule <- function(pt) {
    linesGrob(
      x = c(0, 1), y = 0.5,
      gp = gpar(lwd = pt * 72.27 / 96, col = "black", lineend = "butt")
    )
  }

  footer <- list(
    text_row(note, 6),
    text_row(source, 6, x = 1, hjust = 1, family = serif_family)
  ) |> compact()

  # measure text on a ragg device, the default null device knows no system fonts
  if (dev.cur() == 1) {
    ragg::agg_capture()
    on.exit(dev.off(), add = TRUE)
  }
  chart <- ggplotGrob(
    plot +
      labs(title = NULL, subtitle = NULL, caption = NULL) +
      theme(plot.margin = margin(base_size, 0, base_size / 2, 0))
  )

  rows <- list(
    list(grob = text_row(if (!is.null(number)) paste(label, number), 8),
         height = unit(1.5, "lines") * 8 / base_size),
    list(grob = rule(1), height = pad),
    list(grob = text_row(plot$labels$title, 10, face = "bold"),
         height = unit(1.4, "lines") * 10 / base_size),
    list(grob = text_row(plot$labels$subtitle, 9),
         height = unit(1.4, "lines")),
    list(grob = rule(0.5), height = pad),
    list(grob = chart, height = unit(1, "null")),
    list(grob = if (length(footer) > 0) rule(0.5), height = pad),
    list(grob = if (length(footer) > 0) do.call(grobTree, footer),
         height = unit(1.4, "lines") * 6 / base_size),
    list(grob = rule(1), height = pad)
  ) |>
    keep(\(row) !is.null(row$grob))

  figure <- gtable::gtable_col(
    "uba_figure",
    grobs = map(rows, "grob"),
    heights = do.call(unit.c, map(rows, "height")),
    width = unit(1, "null")
  )
  figure <- gtable::gtable_add_padding(figure, unit(base_size, "pt"))
  figure <- gtable::gtable_add_grob(
    figure, rectGrob(gp = gpar(fill = "white", col = NA)),
    t = 1, l = 1, b = nrow(figure), r = ncol(figure), z = -Inf, name = "bg"
  )
  class(figure) <- c("uba_figure", class(figure))
  figure
}

print.uba_figure <- function(x, ...) {
  grid.newpage()
  grid.draw(x)
  invisible(x)
}

plot.uba_figure <- print.uba_figure

# demo -------------------------------------------------------------------------
# furans <- tidyr::expand_grid(
#   homolog = c("TCDF", "PeCDF", "HxCDF", "HpCDF", "OCDF"),
#   station = c("Hünfelden", "Erfurt", "Frankfurt-Griesheim", "Duisburg-Meiderich")
# ) |>
#   mutate(across(c(homolog, station), forcats::fct_inorder),
#          value = runif(n(), 0, 1.8))
#
# p <- ggplot(furans, aes(homolog, value, fill = station)) +
#   geom_col(position = position_dodge(0.8), width = 0.75) +
#   scale_y_continuous(labels = label_number_de(accuracy = 0.1),
#                      expand = expansion(c(0, 0.05))) +
#   labs(title = "Homologenprofil verschiedener Furane",
#        subtitle = "Luftmessstationen unterschiedlicher Standortkategorien, 1994",
#        x = NULL, y = "PCDF in pg/m³", fill = NULL,
#        caption = "Quelle: Umweltbundesamt") +
#   theme_uba()
#
# uba_figure(p, number = 7) |>
#   ggsave(filename = "plots/uba_theme_demo.png", width = 16, height = 10,
#          units = "cm", dpi = 300, device = ragg::agg_png)
