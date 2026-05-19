# ============================================================
# CCDF of Candidate Set Sizes (Challenge 1: Search-Space Explosion)
#
# Complementary CDF with log-scale y-axis.  Unlike a CDF where
# all curves converge to 1 and overlap, the CCDF separates the
# tail: a curve that stays high at large x means that dataset
# has many super-features with huge candidate sets.
# ============================================================

# ---- Libraries ----
library(ggplot2)
library(scales)

# ---- Font: Arial (same as all existing figures) ----
library(showtext)
font_add("Arial", "C:/Windows/Fonts/arial.ttf")
showtext_auto()

# ---- Paths ----
base_dir    <- "C:/Users/YP/Desktop/work/R-paint/Paper/hotstorage26/challenge/set"
export_dir  <- base_dir
export_name <- "set_size_ccdf.pdf"

# ---- Dataset list (must match CSV file names without .csv) ----
csv_names     <- c("Docker", "Linux", "Log", "Web")
display_names <- c("Docker", "Linux", "Log", "Web*")

# ---- Color palette (same as plot_line_comparison_log2) ----
colors <- c("#A61D24", "#B196C1", "#E8B15E", "#78B0B8")
names(colors) <- display_names

# ---- Visual parameters ----
line_size        <- 2.8
axis_text_size   <- 42
x_title_size     <- 48
y_title_size     <- 48
legend_text_size <- 48
plot_width       <- 14
plot_height      <- 7           # taller, ~2:1 aspect ratio
n_sample         <- 300

# ============================================================
# Build CCDF data (≥ x), downsampled for smooth curves
# ============================================================
plot_data <- data.frame()

for (i in seq_along(csv_names)) {
  fp <- file.path(base_dir, paste0(csv_names[i], ".csv"))
  df <- read.csv(fp, header = TRUE)

  sizes     <- sort(df$set_size)
  n         <- length(sizes)

  # CCDF: fraction of super-features with set size  ≥  x
  # Prepending (1, 1) so the curve starts at 10^0
  log2_x    <- c(0, log2(sizes))
  ccdf_raw  <- c(1, (n - seq_len(n) + 1) / n)

  # Evenly-spaced grid in log2 space, from 0 (= log2(1))
  log2_grid <- seq(0, max(log2_x), length.out = n_sample)
  grid_x    <- 2^log2_grid

  grid_y <- approx(log2_x, ccdf_raw, xout = log2_grid,
                   method = "linear", rule = 2)$y
  grid_y <- pmax(grid_y, 1e-5)       # avoid log10(0)

  temp <- data.frame(
    size    = grid_x,
    ccdf    = grid_y,
    dataset = display_names[i],
    stringsAsFactors = FALSE
  )
  plot_data <- rbind(plot_data, temp)
}

plot_data$dataset <- factor(plot_data$dataset, levels = display_names)

# ============================================================
# Axis setup
# ============================================================
x_min <- 1
x_max <- max(plot_data$size)

# x: log2, powers of 2
min_pow  <- 0
max_pow  <- ceiling(log2(x_max))
x_breaks <- 2^(min_pow:max_pow)

x_labels <- sapply(seq_along(x_breaks), function(i) {
  p <- log2(x_breaks[i])
  if (p %% 2 == 0) {
    bquote(2^.(as.integer(p)))
  } else {
    ""
  }
})

# y: log10, 10^0 down to 10^-4
y_breaks <- 10^(0:-4)
y_labels <- sapply(0:-4, function(p) {
  if (p == 0) {
    bquote(10^0)
  } else {
    bquote(10^.(p))
  }
})

y_min <- min(plot_data$ccdf)

# ============================================================
# Enforce that every dataset starts at (1, 1) — proportion ≥ 1
# is always 1.0 by definition
# ============================================================
anchor <- do.call(rbind, lapply(display_names, function(ds) {
  data.frame(size = 1, ccdf = 1, dataset = ds, stringsAsFactors = FALSE)
}))
anchor$dataset <- factor(anchor$dataset, levels = display_names)
plot_data <- rbind(anchor, plot_data)

# Deduplicate: if a dataset already has size==1 rows (from grid),
# keep only the anchor
plot_data <- plot_data[!duplicated(plot_data[, c("size", "dataset")]), ]
plot_data <- plot_data[order(plot_data$dataset, plot_data$size), ]

# ============================================================
# Plot
# ============================================================
p <- ggplot(plot_data, aes(x = size, y = ccdf, color = dataset)) +
  geom_line(size = line_size) +
  scale_color_manual(values = colors) +

  # Use limits directly (not coord_cartesian) so edges are exact
  scale_x_continuous(
    trans   = "log2",
    breaks  = x_breaks,
    labels  = x_labels,
    limits  = c(x_min, x_max),
    expand  = c(0, 0)
  ) +
  scale_y_continuous(
    trans   = "log10",
    breaks  = y_breaks,
    labels  = y_labels,
    limits  = c(y_min, 1),
    expand  = c(0, 0)
  ) +

  labs(x = "Candidate Set Size", y = "Proportion") +

  theme_classic() +
  theme(
    text          = element_text(family = "Arial"),
    axis.text     = element_text(size = axis_text_size, color = "black"),
    axis.title.x  = element_text(size = x_title_size),
    axis.title.y  = element_text(size = y_title_size, hjust = 0.5),
    axis.text.x   = element_text(size = axis_text_size, color = "black",
                                 margin = margin(t = 5)),
    axis.text.y   = element_text(size = axis_text_size, color = "black",
                                 margin = margin(r = 5)),
    legend.text   = element_text(size = legend_text_size),
    legend.title  = element_blank(),
    legend.position   = c(0.82, 0.90),          # top-right
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.key.size   = unit(1.8, "cm"),
    legend.margin     = margin(4, 6, 4, 6),
    plot.margin       = margin(t = 20, r = 30, b = 10, l = 10, unit = "pt")
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

# ============================================================
# Export
# ============================================================
cairo_pdf(file.path(export_dir, export_name),
          width = plot_width, height = plot_height)
print(p)
dev.off()

message("Done → ", file.path(export_dir, export_name))
