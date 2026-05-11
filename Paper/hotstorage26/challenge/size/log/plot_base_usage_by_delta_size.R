args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  setwd(script_dir)
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  active_path <- rstudioapi::getActiveDocumentContext()$path
  if (!is.null(active_path) && nzchar(active_path)) {
    setwd(dirname(active_path))
  }
}

source("../../../../../MyR/Bar.R")

csv_file <- "C1_M14_WindowsLog_R0_T64_delta_base_usage.csv"
output_file <- "C1_M14_WindowsLog_R0_T64_delta_base_usage.pdf"

df <- read.csv(csv_file, header = TRUE)

required_cols <- c("ChunkID", "BaseChunkID", "EncodedDeltaSize")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

df <- df[!is.na(df$ChunkID) & !is.na(df$BaseChunkID) & !is.na(df$EncodedDeltaSize), ]

bucket_labels <- c("P0-P50", "P50-P75", "P75-P90", "P90-P95", "P95-P99", "P99+")
bucket_breaks <- c(0, 0.50, 0.75, 0.90, 0.95, 0.99, 1.00)

encoded_size_percentile <- rank(df$EncodedDeltaSize, ties.method = "first") / nrow(df)
df$size_bucket <- cut(
  encoded_size_percentile,
  breaks = bucket_breaks,
  labels = bucket_labels,
  include.lowest = TRUE,
  right = TRUE
)

df$used_as_base <- df$ChunkID %in% df$BaseChunkID

base_ratio <- tapply(df$used_as_base, df$size_bucket, mean)
base_ratio <- base_ratio[bucket_labels]
base_ratio[is.na(base_ratio)] <- 0

summary_df <- data.frame(
  Bucket = bucket_labels,
  BaseRatio = as.numeric(base_ratio),
  stringsAsFactors = FALSE
)
write.csv(summary_df, "C1_M14_WindowsLog_R0_T64_delta_base_usage_summary.csv", row.names = FALSE)

create_single_barplot(
  data = summary_df$BaseRatio,
  labels = summary_df$Bucket,
  fill_color = "#AD0626",
  fill_name = "Base usage",
  x_label = "Encoded delta size percentile",
  y_label = "Fraction later reused as base",
  export_name = output_file,
  export_path = "./",
  width = 10,
  height = 5,
  show_legend = FALSE,
  show_data_labels = TRUE,
  axis_text_size = 20,
  label_text_size = 22,
  y_max_multiplier = 1.15,
  bar_width = 0.55
)
