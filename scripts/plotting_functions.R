# Plotting helper functions -------

#' Figure Theme
#' 
#' Theme for figures with frequently used formatting instructions.
#' @param legend whether to show the legend
#' @param grid whether to show the grid
#' @param plot_margin margins for c(top, right, bottom, left) in mm
#' @param text_size font size for all text on plot 
#' @param axis_text_size font size for the axes' text (define only if different from text_size)
#' @param axis_x_rotated whether to rotate the x axis labels
theme_figure <- function(legend = TRUE, grid = TRUE, plot_margin = c(1, 1, 1, 1), 
                         text_size = 20, axis_text_size = NULL, axis_x_rotate = 0) {
  the_theme <- theme_bw() + 
    theme(text = element_text(size = text_size),
          plot.background = element_blank(), panel.background = element_blank(),
          panel.border = element_rect(color="black", size=1), 
          strip.background = element_rect(color="black", linetype = 1),
          plot.margin = unit(plot_margin, "mm")
    )
  # adjust grid
  if(!grid)
    the_theme <- the_theme + theme(panel.grid = element_blank())
  else
    the_theme <- the_theme + theme(panel.grid.minor = element_blank())
  # adjust legend
  if (!legend)
    the_theme <- the_theme + theme(legend.position = "none")
  # overwrite axis text size if provided
  if (!is.null(axis_text_size))
    the_theme <- the_theme + 
      theme(axis.text = element_text(size = axis_text_size)) 
  # axis rotation
  if (axis_x_rotate != 0) {
    the_theme <- the_theme + 
      theme(axis.text.x = element_text(angle = axis_x_rotate, vjust = 0.5, hjust = 1))
  }
  return(the_theme)
}

#' Format number with significant digits
#' 
#' This is useful for including numbers on plots as text elements.
#' 
#' @param signif number of significant digits
#' @param sci_notation cutoff when to start using scientific notation
#' @param sci_as_latex scientific notiation as latex
#' @param include_plus whether to include plus sign
#' @return textual representation of the number matching the parameter settings
format_with_signif <- function(x, signif = 2, sci_notation = 1e-5, sci_as_latex = FALSE, include_plus = FALSE) {
  if(signif <= 0) stop("this function only supports numbers with at least 1 significant digit")
  x <- base::signif(x, digits = signif)
  n_decimals = ifelse(x == 0, 0, log10(abs(x)))
  n_decimals = ifelse(
    n_decimals < 0,
    -floor(n_decimals) + signif - 1L,
    -ceiling(n_decimals) + signif
  )
  plus <- if(include_plus) "+" else ""
  pow <- if(sci_as_latex) "\\\\cdot{}10^{-%.0f}" else "e-%.0f"
  ifelse(
    abs(x) > sci_notation,
    sprintf(sprintf("%%%s.%0.ff", plus, n_decimals), x),
    sprintf(sprintf("%%%s.%0.ff%s", plus, signif - 1L, pow), 
            x * 10^(n_decimals - signif + 1L), (n_decimals - signif + 1L))
  )
}

#' Latex labeller
#' 
#' Latex labeller for ggplot that will interpret latex equations correctly (i.e. anything between $$). 
#' Works for both the \code{labels} parameter of discrete ggplot2 scales as well as the \code{labeller} of facets.
latex_labeller <- function(labels, ...) {
  
  require("dplyr")
  require("tidyr")
  require("purrr")
  require("latex2exp")
  
  # figure out if we're in a scale or facet labeller
  facet_labels <- is(labels, "data.frame")
  if (!facet_labels) labels <- tibble(..x.. = as.character(labels))
  
  # gather labels
  labels <- labels %>% 
    # add position info
    mutate(pos = row_number()) %>% 
    # gather labels
    mutate_if(is.factor, as.character) %>% 
    gather(var, val, -pos) %>% as_tibble() 
  
  # convert latex to expression
  labels <- labels %>% 
    mutate(
      val = map(val, ~latex2exp::TeX(.x))
    )
  
  # spread data frame again
  labels <- labels %>% 
    filter(!is.na(pos)) %>% 
    spread(var, val) %>% 
    select(-pos)
  
  # return appropriate value for labeller
  if (facet_labels) return(labels)
  else return(labels$..x..)
}
class(latex_labeller) <- c("function", "labeller")
