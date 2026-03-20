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

min_len <- min(
  length(df_web[[3]]),
  length(df_windows[[3]]),
  length(df_glibc[[3]]),
  length(df_linux[[3]])
)

series_order <- c("Glibc", "Linux", "Log", "Web*")
line_colors <- c(
  Glibc = "#E8B15E",
  Linux = "#78B0B8",
  Log = "#B196C1",
  `Web*` = "#A61D24"
)

build_series_df <- function(values, label) {
  data.frame(
    x = seq_along(values),
    y = values,
    series = label,
    stringsAsFactors = FALSE
  )
}

plot_df <- rbind(
  build_series_df(c(0, as.numeric(df_glibc[[3]])[1:min_len]), "Glibc"),
  build_series_df(c(0, as.numeric(df_linux[[3]])[1:min_len]), "Linux"),
  build_series_df(c(0, as.numeric(df_windows[[3]])[1:min_len]), "Log"),
  build_series_df(c(0, as.numeric(df_web[[3]])[1:min_len]), "Web*")
)

plot_df$series <- factor(plot_df$series, levels = series_order)

x_range <- range(plot_df$x, na.rm = TRUE)
min_pow <- ceiling(log2(x_range[1]))
max_pow <- floor(log2(x_range[2]))
x_breaks <- 2^(min_pow:max_pow)
x_breaks <- x_breaks[x_breaks >= x_range[1] & x_breaks <= x_range[2]]

x_labels <- rep("", length(x_breaks))
if (length(x_breaks) > 0) {
  label_idx <- seq(1, length(x_breaks), by = 4)
  x_labels[label_idx] <- as.character(x_breaks[label_idx])
}

p <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = x, y = y, color = series)
) +
  ggplot2::geom_line(linewidth = 2.8, lineend = "round") +
  ggplot2::scale_color_manual(
    values = line_colors,
    breaks = series_order,
    guide = ggplot2::guide_legend(
      ncol = 1,
      byrow = TRUE,
      keyheight = grid::unit(22, "pt")
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
    labels = seq(0, 1, by = 0.2),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::coord_cartesian(xlim = x_range, ylim = c(0, 1)) +
  ggplot2::labs(
    x = "Reuse Distance",
    y = "CDF",
    color = NULL
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
    axis.text = ggplot2::element_text(size = 30, color = "black"),
    axis.title.x = ggplot2::element_text(size = 30, margin = ggplot2::margin(t = 12)),
    axis.title.y = ggplot2::element_text(size = 30, margin = ggplot2::margin(r = 12)),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.key.height = grid::unit(28, "pt"),
    legend.spacing.y = grid::unit(28, "pt"),
    legend.text = ggplot2::element_text(size = 30, color = "black")
  )

ggplot2::ggsave(
  filename = file.path(script_dir, "reuse.pdf"),
  plot = p,
  width = 10,
  height = 5
)
