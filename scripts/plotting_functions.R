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

#' Parse scientific notation formatter
#' @example 
#' p + scale_y_continuous(labels = parsed_sci_format(signif = 3))
parsed_sci_format <- function(signif = 2, include_plus = FALSE) {
  require("latex2exp")
  function(x) {
    latex2exp::TeX(format_with_signif(x, signif = signif, always_sci = TRUE, sci_as_latex = TRUE, include_plus = include_plus))
  }
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

#' @param pb_* bottom subplot parameters
#' @param pt_* top subplot parameters
plot_with_y_gap <- function(
  pb, pb_ylim = NULL, pb_breaks = waiver(), pb_labels = labels, pb_gap_marker_offset = 0.02, pb_gap_marker_width = 0.5, pb_relative_size = 1,
  pt = pb, pt_ylim = NULL, pt_breaks = waiver(), pt_labels = labels, pt_gap_marker_offset = 0, pt_gap_marker_width = 0.5, pt_relative_size = 1,
  ylab = pb$labels$y, xlab = pb$labels$x, labels = waiver()
) {
  force(xlab)
  pb$labels$x <- NULL
  pt <- pt + coord_cartesian(ylim = pt_ylim) + 
    scale_y_continuous(NULL, expand = c(0, 0), breaks = pt_breaks, labels = pt_labels, sec.axis = sec_axis(~ .)) +
    theme(
      panel.border = element_blank(),
      plot.margin = margin(b = -3, t = 3, r = 3),
      axis.line.x.top = element_line(color = 'black'),
      axis.line.y.left = element_y_axis_with_gap(bottom_marker = pt_gap_marker_offset, width = pt_gap_marker_width),
      axis.line.y.right = element_line(color = 'black'),
      axis.ticks.y.right = element_blank(),
      axis.text.y.right = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  # add sec axis for x on top plot
  position_scales <- map_lgl(pt$scales$scales, is, "ScaleContinuousPosition")
  x_aesthetics <- map_lgl(pt$scales$scales, ~any(.x$aesthetics == "x"))
  x_idx <- which(position_scales & x_aesthetics)
  if (length(x_idx) > 0) 
    pt$scales$scales[[x_idx[1]]]$secondary.axis <- sec_axis(~ .)
  else
    pt <- pt + scale_x_continuous(sec.axis = sec_axis(~ .))
  
  pb <- pb + coord_cartesian(ylim = pb_ylim) + 
    scale_y_continuous(NULL, expand = c(0, 0), pb_breaks, labels = pb_labels, sec.axis = sec_axis(~ .)) +
    theme(
      panel.border = element_blank(),
      plot.margin = margin(t = -3, b = 3, r = 3),
      axis.line.x.bottom = element_line(color = 'black'),
      axis.line.y.left = element_y_axis_with_gap(top_marker = pb_gap_marker_offset, width = pb_gap_marker_width),
      axis.line.y.right = element_line(color = 'black'),
      axis.ticks.y.right = element_blank(),
      axis.text.y.right = element_blank(),
      axis.title.x.top = element_blank(),
      axis.text.x.top = element_blank(),
      axis.ticks.x.top = element_blank()
    )
  
  # combined plots
  p <- cowplot::plot_grid(
    pt, pb, ncol = 1, align = "v", rel_heights = c(pt_relative_size, pb_relative_size)
  )
  
  # add axis labels
  y_label_grob <- NULL
  x_label_grob <- NULL
  if (!is.null(ylab)) {
    y_label_grob <- grid::textGrob(ylab, gp = element_render(pb$theme, "axis.text.y")$children[[1]]$gp, rot=90)
  } 
  if (!is.null(xlab)) {
    x_label_grob <- grid::textGrob(xlab, gp = element_render(pb$theme, "axis.text.x")$children[[1]]$gp)
  }
  gridExtra::grid.arrange(gridExtra::arrangeGrob(p, left = y_label_grob, bottom = x_label_grob))
}


#' function to use y axis with gap in theme
#' @param bottom_marker offset of the gap marker at the bottom of the scale (if NULL, no bottom marker is drawn)
#' @param top_marker offset of the gap marker at the top of the scale (if NULL, no top marker is drawn)
#' @param width of markers (relative)
element_y_axis_with_gap <- function(bottom_marker = NULL, top_marker = NULL, width = 0.5) {
  structure(
    list(bottom_marker = bottom_marker, top_marker = top_marker, width = width), 
    class = c("element_y_axis_with_gap","element_blank", "element") 
  )
}

# actual axis structure (don't call this directly)
element_grob.element_y_axis_with_gap <- function(element, ...)  {
  width <- if (!is.null(element$width)) element$width else 0.5
  if (!is.null(element$bottom_marker)) {
    bottom_offset <- element$bottom_marker
    bottom_grob <- grid::segmentsGrob(x0 = 1 - width, y0 = bottom_offset, x1 = 1 + width, y1 = bottom_offset)
  } else {
    bottom_offset <- 0
    bottom_grob <- NULL
  }
  if (!is.null(element$top_marker)) {
    top_offset <- element$top_marker
    top_grob <- grid::segmentsGrob(x0 = 1 - width, y0 = 1 - top_offset, x1 = 1 + width, y1 = 1 - top_offset)
  } else {
    top_offset <- 0
    top_grob <- NULL
  }
  
  grid::gList(
    grid::segmentsGrob(x0 = 1, y0 = bottom_offset, x1 = 1, y1 = 1 - top_offset),
    bottom_grob,
    top_grob
  )
}

