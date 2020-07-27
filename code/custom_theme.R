## GGPLOT THEME
if ("extrafont" %in% rownames(installed.packages())) {
  library(extrafont)
  # extrafont::font_import()
  extrafont::loadfonts(quiet = TRUE)
  base <- "GT America"
} else {
  base <- "Arial"
}

theme_custom <- function(base_size = 14, base_family = base) {
  half_line <- base_size / 2

  theme(
    line = element_line(color = "black", size = 1, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "white", size = 0.4, linetype = 1),
    text = element_text(
      family = base_family, face = "plain", color = "black", size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE
    ),
    axis.line = element_blank(),
    axis.line.x = NULL,
    axis.line.y = element_line(color = "#efecea", size = 1),
    axis.text = element_text(size = base_size * 0.9, color = "#3C3532"),
    axis.text.x = element_text(margin = margin(t = 0.6 * half_line / 2), vjust = 1),
    axis.text.x.top = element_text(margin = margin(b = 0.6 * half_line / 2), vjust = 0),
    axis.text.y = element_text(margin = margin(r = 0.6 * half_line / 2), hjust = 1),
    axis.text.y.right = element_text(margin = margin(l = 0.6 * half_line / 2), hjust = 0),
    axis.ticks = element_line(colour = NA, size = 0.3),
    axis.ticks.length = unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = element_text(margin = unit(c(3.5, 0, 0, 0), "mm"), vjust = 1, size = base_size),
    axis.title.x.top = element_text(margin = margin(b = half_line), vjust = 0),
    axis.title.y = element_text(angle = 90, margin = unit(c(0, 3.5, 0, 0), "mm"), vjust = 1, size = base_size),
    axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0),
    legend.background = element_rect(colour = NA),
    legend.spacing = unit(0.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.9)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0, size = rel(1)),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(0.4, "cm"),
    panel.background = element_rect(fill = NA, colour = NA),
    panel.border = element_rect(colour = NA, fill = NA, size = rel(5)),
    # panel.grid = element_blank(),
    panel.grid.major.x = element_line(colour = "transparent"),
    panel.grid.major.y = element_line(colour = "#efecea", size = 0.5),
    panel.grid.minor = element_line(colour = "transparent"),
    # panel.spacing = unit(base_size/2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    panel.spacing = unit(1.5, "lines"),

    strip.background = element_rect(fill = "#efecea", colour = "white"),
    strip.text = element_text(colour = "black", size = base_size * 0.6, face = "bold"),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(size = 14, hjust = 0, vjust = 1, face = "bold", margin = margin(b = half_line * 1.2)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = base_size * 1.2, color = "#999999", hjust = 0, vjust = 1, margin = margin(half_line * 0.9, half_line * 0.9, 20, half_line * 0.9)),
    plot.caption = element_text(size = rel(0.9), color = "#999999", hjust = 0),
    plot.caption.position = "plot",
    plot.margin = margin(0, 0, 0, 0), complete = T,
    plot.tag = element_text(size = rel(1.5), face = "bold", hjust = 0.5, vjust = 0.5),
    plot.tag.position = "topleft"
  )
}

theme_set(theme_custom()) # Can change to font of choice. Normally would use GT America at the ACLU. 
theme_update(
  plot.title = element_text(size = 14, face = "bold", hjust = 0.01),
  plot.subtitle = element_text(color = "black", size = 11),
  plot.caption = element_text(color = "#999999", hjust = 0.00, size = 10),
  strip.background = element_rect(fill = "transparent", colour = "white"),
  axis.text.x = element_text(size = 10)
)
