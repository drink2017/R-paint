library(ggplot2)
library(grid)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_arg <- args[startsWith(args, file_arg)]

  if (length(script_arg) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", script_arg[1]))))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    context <- rstudioapi::getActiveDocumentContext()
    if (nzchar(context$path)) {
      return(dirname(normalizePath(context$path)))
    }
  }

  normalizePath(getwd())
}

export_dir <- get_script_dir()

# Fill in the four rows below. Each row is one group, and the two columns are:
# Mdelta, Mdelta-NoGroup.
plot_data <- data.frame(
  group = c("Docker", "Linux", "Web-32", "Log"),
  Mdelta = c(429, 2766, 3119, 491),           # TODO: replace with your data
  `Mdelta-NoGroup` = c(817, 3717, 20826, 348), # TODO: replace with your data
  check.names = FALSE
)

method_labels <- c("Mdelta", "Mdelta-NoGroup")
method_colors <- c(
  "Mdelta" = "#A61D24",
  "Mdelta-NoGroup" = "#78B0B8"
)

create_three_method_barplot <- function(
    data,
    group_col = "group",
    method_labels = c("Mdelta", "Mdelta-NoGroup"),
    fill_colors = c("Mdelta" = "#A61D24", "Mdelta-NoGroup" = "#78B0B8"),
    x_label = "",
    y_label = "Value",
    axis_text_size = 50,
    axis_title_size = 50,
    legend_text_size = 50,
    show_data_labels = FALSE,
    data_label_size = 10,
    y_max_multiplier = 1.20,
    bar_width = 0.72
) {
  long_df <- reshape(
    data,
    varying = method_labels,
    v.names = "value",
    timevar = "method",
    times = method_labels,
    direction = "long"
  )
  long_df[[group_col]] <- factor(long_df[[group_col]], levels = data[[group_col]])
  long_df$method <- factor(long_df$method, levels = method_labels)

  finite_values <- long_df$value[is.finite(long_df$value)]
  if (length(finite_values) == 0) {
    stop("No finite values found in plot_data. Fill in the data before plotting.")
  }
  value_max <- max(c(0, finite_values), na.rm = TRUE)
  y_max <- if (value_max > 0) value_max * y_max_multiplier else 1

  p <- ggplot(long_df, aes(x = .data[[group_col]], y = value, fill = method)) +
    geom_col(
      position = position_dodge(width = bar_width),
      width = bar_width,
      color = "black",
      linewidth = 0.3
    ) +
    scale_fill_manual(
      name = NULL,
      values = fill_colors,
      breaks = method_labels,
      labels = method_labels,
      guide = guide_legend(
        nrow = 1,
        byrow = TRUE,
        keyheight = unit(18, "pt"),
        keywidth = unit(18, "pt")
      )
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.12)),
      limits = c(0, y_max)
    ) +
    labs(x = x_label, y = paste(" ", y_label, " ")) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = axis_text_size, color = "black"),
      axis.text.y = element_text(size = axis_text_size, color = "black"),
      axis.title.x = element_text(size = axis_title_size, color = "black"),
      axis.title.y = element_text(size = axis_title_size, color = "black", hjust = 0.5),
      legend.position = c(0.03, 1.01),
      legend.justification = c(0, 1),
      legend.direction = "horizontal",
      legend.text = element_text(size = legend_text_size, color = "black"),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.spacing.x = unit(10, "pt"),
      plot.margin = margin(t = 18, r = 12, b = 12, l = 12, unit = "pt")
    )

  if (show_data_labels) {
    p <- p + geom_text(
      aes(label = round(value, 2)),
      position = position_dodge(width = bar_width),
      vjust = -0.25,
      size = data_label_size
    )
  }

  p
}

p <- create_three_method_barplot(
  data = plot_data,
  method_labels = method_labels,
  fill_colors = method_colors,
  x_label = "",
  y_label = "Duration (s)",
  show_data_labels = FALSE # Change to TRUE if labels above bars are needed.
)

output_file <- file.path(export_dir, "overall.pdf")
message("Writing plot to: ", output_file)

cairo_pdf(
  output_file,
  width = 12,
  height = 5
)
print(p)
dev.off()
