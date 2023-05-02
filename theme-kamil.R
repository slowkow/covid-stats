#' @importFrom ggplot2 theme_classic theme element_rect element_line
#'   element_blank element_text
#' @importFrom grid unit
theme_kamil <- theme_classic() +
theme(
  panel.spacing    = unit(2, "lines"),
  panel.border     = element_rect(linewidth = 0.5, fill = NA),
  axis.ticks       = element_line(linewidth = 0.4),
  axis.line        = element_blank(),
  plot.title       = element_text(size = 16),
  plot.title.position = "plot",
  plot.subtitle    = element_text(size = 16),
  plot.caption     = element_text(size = 16),
  strip.background = element_blank(),
  strip.text       = element_text(size = 16),
  legend.text      = element_text(size = 16),
  legend.title     = element_text(size = 16),
  axis.text        = element_text(size = 16),
  axis.title       = element_text(size = 16)
)

scientific_10 <- function(x) {
  ifelse(x < 0.01,
    gsub("e", "%*%10^", scales::scientific_format(digits = 1)(x)),
    signif(x, 1)
  )
}

