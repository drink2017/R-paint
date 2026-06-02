library(ggplot2)
library(extrafont)
library(showtext)

setwd("E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/evaluation/sensitivity")

source("../../../../MyR/Line_Point.R")

font_add("Arial", "C:/Windows/Fonts/arial.ttf")
showtext_auto()
windowsFonts(Arial = windowsFont("Arial"))

# Fill in the two data columns below before running the script.
plot_data <- data.frame(
  x = c(32, 64, 128, 256),
  compression_ratio = c(51.0, 51.1, 50.8, 49.1),
  duration = c(621, 608, 575, 492)
)

plot_compression_duration_dual_axis <- function(
  data,
  export_path = "./",
  export_name = "Web.pdf",
  x_label = "Depth coefficient A (bytes)",
  left_y_label = "DRR",
  right_y_label = "Duration (s)",
  left_y_lim = NULL,
  right_y_lim = NULL,
  left_y_breaks = NULL,
  right_y_breaks = NULL,
  colors = c("Compression Ratio" = "#AD0626", "Duration (s)" = "#2C3359"),
  shapes = c("Compression Ratio" = 23, "Duration (s)" = 24),
  line_size = 2.8,
  point_size = 9,
  stroke_size = 4.5,
  axis_text_size = 42,
  x_title_size = 48,
  y_title_size = 45,
  legend_text_size = 32,
  show_legend = FALSE,
  plot_width = 10,
  plot_height = 5
) {
  required_cols <- c("x", "compression_ratio", "duration")
  if (!all(required_cols %in% names(data))) {
    stop("data must contain columns: x, compression_ratio, duration")
  }

  if (anyNA(data[required_cols])) {
    stop("Please fill compression_ratio and duration values before plotting.")
  }

  if (is.null(left_y_lim)) {
    left_y_lim <- range(data$compression_ratio, na.rm = TRUE)
    left_lower_pad <- diff(left_y_lim) * 0.08
    left_upper_pad <- diff(left_y_lim) * 0.20
    if (left_lower_pad == 0) left_lower_pad <- max(abs(left_y_lim[1]) * 0.08, 1)
    if (left_upper_pad == 0) left_upper_pad <- max(abs(left_y_lim[2]) * 0.20, 1)
    left_y_lim <- c(left_y_lim[1] - left_lower_pad, left_y_lim[2] + left_upper_pad)
  }

  if (is.null(right_y_lim)) {
    right_y_lim <- range(data$duration, na.rm = TRUE)
    right_lower_pad <- diff(right_y_lim) * 0.08
    right_upper_pad <- diff(right_y_lim) * 0.20
    if (right_lower_pad == 0) right_lower_pad <- max(abs(right_y_lim[1]) * 0.08, 1)
    if (right_upper_pad == 0) right_upper_pad <- max(abs(right_y_lim[2]) * 0.20, 1)
    right_y_lim <- c(right_y_lim[1] - right_lower_pad, right_y_lim[2] + right_upper_pad)
  }

  left_span <- diff(left_y_lim)
  right_span <- diff(right_y_lim)
  if (left_span == 0 || right_span == 0) {
    stop("left_y_lim and right_y_lim must have non-zero ranges.")
  }

  scale_factor <- left_span / right_span
  to_left_axis <- function(y) (y - right_y_lim[1]) * scale_factor + left_y_lim[1]
  to_right_axis <- function(y) (y - left_y_lim[1]) / scale_factor + right_y_lim[1]

  compression_df <- data.frame(
    x = data$x,
    value = data$compression_ratio,
    metric = "Compression Ratio"
  )
  duration_df <- data.frame(
    x = data$x,
    value = to_left_axis(data$duration),
    metric = "Duration (s)"
  )
  long_data <- rbind(compression_df, duration_df)

  y_scale_args <- list(
    name = left_y_label,
    limits = left_y_lim,
    sec.axis = sec_axis(
      transform = to_right_axis,
      name = right_y_label
    ),
    expand = expansion(mult = c(0.01, 0.03))
  )
  if (!is.null(left_y_breaks)) {
    y_scale_args$breaks <- left_y_breaks
  }
  if (!is.null(right_y_breaks)) {
    y_scale_args$sec.axis <- sec_axis(
      transform = to_right_axis,
      name = right_y_label,
      breaks = right_y_breaks
    )
  }

  p <- ggplot(long_data, aes(x = x, y = value, color = metric, shape = metric)) +
    geom_line(aes(group = metric), size = line_size) +
    geom_point(size = point_size, stroke = stroke_size) +
    scale_color_manual(values = colors, name = NULL) +
    scale_shape_manual(values = shapes, name = NULL) +
    scale_x_continuous(
      trans = "log2",
      breaks = data$x,
      labels = data$x,
      expand = expansion(mult = c(0.03, 0.06))
    ) +
    do.call(scale_y_continuous, y_scale_args) +
    labs(x = x_label) +
    theme_classic() +
    theme(
      text = element_text(family = "Arial", color = "black"),
      axis.text = element_text(size = axis_text_size, color = "black"),
      axis.text.x = element_text(size = axis_text_size, color = "black", margin = margin(t = 5)),
      axis.text.y.left = element_text(size = axis_text_size, color = colors["Compression Ratio"], margin = margin(r = 5)),
      axis.text.y.right = element_text(size = axis_text_size, color = colors["Duration (s)"], margin = margin(l = 5)),
      axis.title.x = element_text(size = x_title_size, color = "black"),
      axis.title.y.left = element_text(size = y_title_size, color = colors["Compression Ratio"], hjust = 0.5),
      axis.title.y.right = element_text(size = y_title_size, color = colors["Duration (s)"], hjust = 0.5),
      axis.line = element_line(linewidth = 1),
      axis.ticks = element_line(linewidth = 1),
      axis.ticks.length = unit(0.2, "cm"),
      plot.margin = margin(t = 22, r = 15, b = 10, l = 10, unit = "pt"),
      legend.position = if (show_legend) "top" else "none",
      legend.text = element_text(size = legend_text_size)
    )

  ggsave(file.path(export_path, export_name), plot = p, width = plot_width, height = plot_height)
  return(p)
}

plot_compression_duration_dual_axis(
  data = plot_data,
  export_path = "./",
  export_name = "Log.pdf",
  x_label = "Depth coefficient A (bytes)",
  left_y_label = "DRR",
  right_y_label = "Duration (s)",
  left_y_lim = NULL,
  right_y_lim = NULL,
  left_y_breaks = NULL,
  right_y_breaks = NULL
)
