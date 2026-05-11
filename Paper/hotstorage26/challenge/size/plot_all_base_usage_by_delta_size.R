args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
  setwd(dirname(script_path))
}

library(ggplot2)

datasets <- data.frame(
  Dataset = c("Glibc", "Linux", "Web*", "Log"),
  File = c(
    "glibc/C1_M14_glibc_R0_T64_delta_base_usage_summary.csv",
    "linux/C1_M14_linux_R0_T64_delta_base_usage_summary.csv",
    "web/C1_M14_WEB-3_R0_T64_delta_base_usage_summary.csv",
    "log/C1_M14_WindowsLog_R0_T64_delta_base_usage_summary.csv"
  ),
  stringsAsFactors = FALSE
)

dataset_colors <- c(
  "Glibc" = "#AD0626",
  "Linux" = "#2C6DB2",
  "Web*" = "#2E8B57",
  "Log" = "#E08A00"
)

missing_files <- datasets$File[!file.exists(datasets$File)]
if (length(missing_files) > 0) {
  stop("Missing summary file(s): ", paste(missing_files, collapse = ", "))
}

summary_list <- lapply(seq_len(nrow(datasets)), function(i) {
  df <- read.csv(datasets$File[i], header = TRUE)
  required_cols <- c("Bucket", "BaseRatio")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ", datasets$File[i], ": ",
      paste(missing_cols, collapse = ", ")
    )
  }
  df$Dataset <- datasets$Dataset[i]
  df
})

summary_df <- do.call(rbind, summary_list)
bucket_labels <- c("P0-P50", "P50-P75", "P75-P90", "P90-P95", "P95-P99", "P99+")
summary_df$Bucket <- factor(summary_df$Bucket, levels = bucket_labels)
summary_df$Dataset <- factor(summary_df$Dataset, levels = datasets$Dataset)
summary_df <- summary_df[order(summary_df$Dataset, summary_df$Bucket), ]

output_file <- "C1_M14_all_datasets_delta_base_usage.pdf"

p <- ggplot(summary_df, aes(x = Bucket, y = BaseRatio, fill = Dataset)) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.68,
    color = NA
  ) +
  scale_fill_manual(
    name = NULL,
    values = dataset_colors,
    breaks = names(dataset_colors),
    guide = guide_legend(
      nrow = 1,
      byrow = TRUE,
      keyheight = grid::unit(18, "pt"),
      keywidth = grid::unit(18, "pt")
    )
  ) +
  scale_y_continuous(
    limits = c(0, max(summary_df$BaseRatio) * 1.18),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Encoded delta size percentile",
    y = "Reuse fraction"
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 32, color = "black", angle = 25, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 32, color = "black"),
    axis.title.x = element_text(size = 32, color = "black", margin = margin(t = 10)),
    axis.title.y = element_text(size = 32, color = "black", margin = margin(r = 6)),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    legend.position = c(0.03, 1.01),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 32, color = "black"),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.spacing.x = grid::unit(8, "pt"),
    plot.margin = margin(t = 18, r = 12, b = 34, l = 30, unit = "pt")
  )

cairo_pdf(output_file, width = 12, height = 5)
print(p)
dev.off()

message("Wrote ", output_file)
