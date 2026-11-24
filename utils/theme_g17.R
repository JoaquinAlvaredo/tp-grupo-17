ROJO <- "#A01E1E"; GRIS <- "#A9A9A9"; GRIS_OSC <- "#4D4D4D"; GRIS_CLARO <- "#E6E6E6"

theme_g17 <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(plot.title.position = "plot", plot.caption.position = "plot",
          plot.title = element_text(face = "bold", size = 14.5, lineheight = 1.05, margin = margin(b = 4)),
          plot.subtitle = element_text(size = 10, colour = GRIS_OSC, lineheight = 1.15, margin = margin(b = 10)),
          plot.caption = element_text(size = 7.8, colour = "#666666", hjust = 0, lineheight = 1.15, margin = margin(t = 10)),
          axis.title = element_text(size = 9.5, colour = GRIS_OSC),
          axis.text = element_text(size = 9.5, colour = "#222222"),
          panel.grid.minor = element_blank(),
          plot.margin = margin(12, 16, 10, 12),
          plot.background = element_rect(fill = "white", colour = NA))
}
