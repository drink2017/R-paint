library(ggplot2)
library(extrafont)
library(showtext)

font_add("Arial", "C:/Windows/Fonts/arial.ttf")
showtext_auto()
windowsFonts(Arial = windowsFont("Arial"))

root_dir <- "E:/Users/drinkwater/Desktop/R-script-template"
input_dir <- file.path(
  root_dir,
  "Paper/hotstorage26/evaluation/restore"
)
export_dir <- input_dir

read_method_pair <- function(dataset_name) {
  design_path <- file.path(input_dir, paste0("design3_", dataset_name, ".csv"))
  odess_path <- file.path(input_dir, paste0("odess_", dataset_name, ".csv"))

  design_df <- read.csv(design_path, stringsAsFactors = FALSE)
  odess_df <- read.csv(odess_path, stringsAsFactors = FALSE)

  if (nrow(design_df) != nrow(odess_df)) {
    stop(sprintf("Row count mismatch for %s", dataset_name))
  }
  if (!all(design_df$backup_index == odess_df$backup_index)) {
    stop(sprintf("backup_index mismatch for %s", dataset_name))
  }

  data.frame(
    backup_index = design_df$backup_index,
    design3 = design_df$restore_time_seconds,
    odess = odess_df$restore_time_seconds
  )
}

make_x_breaks <- function(x_values) {
  x_min <- min(x_values, na.rm = TRUE)
  x_max <- max(x_values, na.rm = TRUE)
  span <- x_max - x_min

  if (span <= 120) {
    step <- 20
  } else if (span <= 300) {
    step <- 50
  } else if (span <= 500) {
    step <- 75
  } else {
    step <- 100
  }

  pretty_breaks <- pretty(c(x_min, x_max), n = 7)
  step_breaks <- seq(ceiling(x_min / step) * step, x_max, by = step)
  breaks <- unique(c(x_min, pretty_breaks, step_breaks, x_max))
  breaks <- breaks[breaks >= x_min & breaks <= x_max]
  breaks <- sort(unique(as.integer(breaks)))

  if (length(breaks) >= 2) {
    last_gap <- breaks[length(breaks)] - breaks[length(breaks) - 1]
    if (last_gap <= step * 0.6) {
      breaks <- breaks[-(length(breaks) - 1)]
    }
  }

  breaks
}

plot_one_dataset <- function(dataset_name, export_name) {
  wide_df <- read_method_pair(dataset_name)
  long_df <- rbind(
    data.frame(
      backup_index = wide_df$backup_index,
      method = "design3",
      value = wide_df$design3
    ),
    data.frame(
      backup_index = wide_df$backup_index,
      method = "odess",
      value = wide_df$odess
    )
  )

  x_breaks <- make_x_breaks(wide_df$backup_index)

  p <- ggplot(long_df, aes(x = backup_index, y = value, color = method)) +
    geom_line(size = 2.8) +
    scale_color_manual(
      values = c(design3 = "#AD0626", odess = "#2C3359"),
      labels = c(design3 = "Mdelta", odess = "Odess")
    ) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 42, color = "black"),
      text = element_text(family = "Arial"),
      axis.title.x = element_text(size = 48),
      axis.title.y = element_text(size = 45),
      legend.position = "top",
      legend.text = element_text(size = 32),
      plot.margin = margin(t = 15, r = 15, b = 10, l = 40, unit = "pt"),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.text.y = element_text(margin = margin(r = 5))
    ) +
    labs(x = "Backup Index", y = "Restore Time (s)", color = NULL) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_breaks,
      expand = expansion(add = c(0.02 * diff(range(wide_df$backup_index)), 0.04 * diff(range(wide_df$backup_index))))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    guides(color = guide_legend(title = NULL))

  ggsave(
    filename = file.path(export_dir, export_name),
    plot = p,
    width = 10,
    height = 5
  )

  invisible(p)
}

plot_one_dataset("linux", "restore_time_linux.pdf")
plot_one_dataset("docker_gitlab", "restore_time_docker_gitlab.pdf")
plot_one_dataset("logs", "restore_time_logs.pdf")
plot_one_dataset("WEB", "restore_time_WEB.pdf")
