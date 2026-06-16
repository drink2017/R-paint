args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grepl("^--file=", args)]
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = FALSE))
} else if (!is.null(sys.frames()[[1]]$ofile)) {
  dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = FALSE))
} else {
  getwd()
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required but not installed.", call. = FALSE)
}

input_files <- c("web.txt", "windows.txt", "glibc.txt", "linux.txt")
input_paths <- file.path(script_dir, input_files)

if (!all(file.exists(input_paths))) {
  fallback_dir <- file.path(getwd(), "Paper", "hotstorage26", "challenge", "3", "1")
  fallback_paths <- file.path(fallback_dir, input_files)
  if (all(file.exists(fallback_paths))) {
    script_dir <- normalizePath(fallback_dir, winslash = "/", mustWork = TRUE)
    input_paths <- fallback_paths
  } else {
    stop("Could not locate the input txt files for challenge/3/1.", call. = FALSE)
  }
}

df_web <- read.table(input_paths[1], header = TRUE, sep = "\t")
df_windows <- read.table(input_paths[2], header = TRUE, sep = "\t")
df_glibc <- read.table(input_paths[3], header = TRUE, sep = "\t")
df_linux <- read.table(input_paths[4], header = TRUE, sep = "\t")

series_order <- c("Glibc", "Linux", "Log", "Web")
legend_labels <- c(
  Glibc = "Glibc",
  Linux = "Linux",
  Log = "Log",
  Web = "Web"
)
line_colors <- c(
  Glibc = "#E8B15E",
  Linux = "#78B0B8",
  Log = "#B196C1",
  Web = "#A61D24"
)

build_series_df <- function(distances, values, label) {
  series_df <- data.frame(
    x = c(1, as.numeric(distances)),
    y = c(0, as.numeric(values)),
    series = label,
    stringsAsFactors = FALSE
  )
  series_df <- series_df[is.finite(series_df$x) & is.finite(series_df$y), , drop = FALSE]
  series_df$x <- pmax(series_df$x, 1)
  series_df$y[nrow(series_df)] <- 1
  series_df
}

plot_df <- rbind(
  build_series_df(df_glibc[[1]], df_glibc[[3]], "Glibc"),
  build_series_df(df_linux[[1]], df_linux[[3]], "Linux"),
  build_series_df(df_windows[[1]], df_windows[[3]], "Log"),
  build_series_df(df_web[[1]], df_web[[3]], "Web")
)

plot_df$series <- factor(plot_df$series, levels = series_order)

x_range <- range(plot_df$x, na.rm = TRUE)
min_pow <- ceiling(log2(x_range[1]))
max_pow <- floor(log2(x_range[2]))
x_breaks <- 2^(min_pow:max_pow)
x_breaks <- x_breaks[x_breaks >= x_range[1] & x_breaks <= x_range[2]]

# Unicode superscript conversion
superscript_digits <- c(
  "0" = "\u2070", "1" = "\u00b9", "2" = "\u00b2", "3" = "\u00b3",
  "4" = "\u2074", "5" = "\u2075", "6" = "\u2076", "7" = "\u2077",
  "8" = "\u2078", "9" = "\u2079"
)
to_superscript <- function(n) {
  chars <- strsplit(as.character(n), "", fixed = TRUE)[[1]]
  paste0(superscript_digits[chars], collapse = "")
}

x_labels <- character(length(x_breaks))
x_labels[] <- ""
if (length(x_breaks) > 0) {
  label_idx <- seq(1, length(x_breaks), by = 4)
  exponents <- as.integer(log2(x_breaks[label_idx]))
  x_labels[label_idx] <- paste0("2", sapply(exponents, to_superscript))
}

p <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = x, y = y, color = series)
) +
  ggplot2::geom_line(linewidth = 2.8, lineend = "round") +
  ggplot2::scale_color_manual(
    values = line_colors,
    breaks = series_order,
    labels = legend_labels[series_order],
    guide = ggplot2::guide_legend(
      nrow = 2,
      ncol = 2,
      byrow = TRUE,
      keyheight = grid::unit(18, "pt"),
      keywidth = grid::unit(18, "pt"),
      label.theme = ggplot2::element_text(margin = ggplot2::margin(r = 100, unit = "pt"))
    )
  ) +
  ggplot2::scale_x_continuous(
    trans = "log2",
    breaks = x_breaks,
    labels = x_labels,
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = c("", "0.2", "0.4", "0.6", "0.8", "1.0"),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::coord_cartesian(xlim = x_range, ylim = c(0, 1)) +
  ggplot2::labs(
    x = "Reuse Distance",
    y = "CDF",
    color = NULL
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.margin = ggplot2::margin(t = 14, r = 20, b = 20, l = 10, unit = "pt"),
    axis.text = ggplot2::element_text(size = 54, color = "black"),
    axis.title.x = ggplot2::element_text(size = 54),
    axis.title.y = ggplot2::element_text(size = 54, margin = ggplot2::margin(r = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.box.just = "left",
    legend.direction = "horizontal",
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.key.height = grid::unit(18, "pt"),
    legend.spacing.x = grid::unit(80, "pt"),
    legend.spacing.y = grid::unit(0, "pt"),
    legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    legend.box.margin = ggplot2::margin(t = -12, r = 0, b = 0, l = -16, unit = "pt"),
    legend.box.spacing = grid::unit(0, "pt"),
    legend.text = ggplot2::element_text(size = 52, color = "black")
  )

# Extract legend grob and build layout matching process
get_legend_grob <- function(plot) {
  plot_grob <- ggplot2::ggplotGrob(plot)
  legend_index <- which(vapply(plot_grob$grobs, function(grob) grob$name, character(1)) == "guide-box")
  if (length(legend_index) == 0) {
    return(grid::nullGrob())
  }
  plot_grob$grobs[[legend_index[1]]]
}

legend_grob <- get_legend_grob(p)
legend_area_height <- grid::unit(1.80, "in")
legend_y_offset <- grid::unit(0.00, "in")

main_plot <- p + ggplot2::theme(
  legend.position = "none",
  plot.margin = ggplot2::margin(t = 0, r = 20, b = 12, l = 10, unit = "pt")
)

cairo_pdf(file.path(script_dir, "reuse.pdf"), width = 12, height = 9)
grid::grid.newpage()

plot_layout <- grid::grid.layout(
  nrow = 2,
  ncol = 1,
  heights = grid::unit.c(legend_area_height, grid::unit(1, "null"))
)
grid::pushViewport(grid::viewport(layout = plot_layout))

grid::pushViewport(grid::viewport(
  layout.pos.row = 1,
  layout.pos.col = 1
))
grid::pushViewport(grid::viewport(
  x = grid::unit(0.50, "npc"),
  y = grid::unit(0.90, "npc") + legend_y_offset,
  width = sum(legend_grob$widths),
  height = sum(legend_grob$heights),
  just = c("center", "top")
))
grid::grid.draw(legend_grob)
grid::popViewport(2)

print(main_plot, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
grid::popViewport()
dev.off()