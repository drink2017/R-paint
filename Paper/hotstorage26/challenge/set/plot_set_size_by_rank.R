args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
  setwd(dirname(script_path))
}

library(ggplot2)

dataset_order <- c("glibc", "linux", "web", "log")
dataset_labels <- c(
  glibc = "Glibc",
  linux = "Linux",
  web = "Web*",
  log = "Log"
)
dataset_colors <- c(
  glibc = "#AD0626",
  linux = "#2C6DB2",
  web = "#2E8B57",
  log = "#E08A00"
)

output_dir <- "figures"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

read_rank_set_size <- function(file) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    df <- data.table::fread(
      file,
      select = c("rank", "set_size"),
      showProgress = FALSE,
      data.table = FALSE
    )
  } else {
    df <- read.csv(
      file,
      header = TRUE,
      colClasses = c(rank = "numeric", superfeature = "NULL", set_size = "numeric")
    )
  }

  required_cols <- c("rank", "set_size")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in ", file, ": ", paste(missing_cols, collapse = ", "))
  }

  df <- df[is.finite(df$rank) & is.finite(df$set_size), required_cols]
  df <- df[seq_len(min(5000, nrow(df))), ]
  df
}

pretty_dataset_name <- function(dataset_id) {
  if (dataset_id %in% names(dataset_labels)) {
    dataset_labels[[dataset_id]]
  } else {
    paste0(toupper(substr(dataset_id, 1, 1)), substr(dataset_id, 2, nchar(dataset_id)))
  }
}

find_dataset_files <- function() {
  dirs <- list.dirs(".", full.names = FALSE, recursive = FALSE)
  dirs <- dirs[dirs != output_dir]
  dirs <- dirs[file.info(dirs)$isdir]

  dataset_files <- lapply(dirs, function(dataset_id) {
    csv_files <- list.files(dataset_id, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
      return(NULL)
    }

    preferred <- grep("sf_size_dist", csv_files, value = TRUE)
    data.frame(
      DatasetID = dataset_id,
      File = if (length(preferred) > 0) preferred[1] else csv_files[1],
      stringsAsFactors = FALSE
    )
  })

  dataset_files <- do.call(rbind, dataset_files)
  if (is.null(dataset_files) || nrow(dataset_files) == 0) {
    stop("No CSV files found in dataset subdirectories.")
  }

  dataset_files$Order <- match(dataset_files$DatasetID, dataset_order)
  dataset_files$Order[is.na(dataset_files$Order)] <- length(dataset_order) + seq_len(sum(is.na(dataset_files$Order)))
  dataset_files <- dataset_files[order(dataset_files$Order, dataset_files$DatasetID), ]
  dataset_files$Order <- NULL
  rownames(dataset_files) <- NULL
  dataset_files
}

make_plot <- function(df, dataset_id, dataset_name) {
  point_color <- if (dataset_id %in% names(dataset_colors)) dataset_colors[[dataset_id]] else "#2C6DB2"

  ggplot(df, aes(x = rank, y = set_size)) +
    geom_point(
      color = point_color,
      alpha = 0.28,
      size = 0.42,
      stroke = 0
    ) +
    scale_x_continuous(
      labels = scales::label_number(big.mark = ","),
      expand = expansion(mult = c(0.01, 0.03))
    ) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ","),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      x = "Rank",
      y = "Set size",
      title = dataset_name
    ) +
    theme_classic(base_family = "Arial") +
    theme(
      plot.title = element_text(size = 34, color = "black", hjust = 0.5, margin = margin(b = 8)),
      axis.text.x = element_text(size = 30, color = "black"),
      axis.text.y = element_text(size = 30, color = "black"),
      axis.title.x = element_text(size = 34, color = "black", margin = margin(t = 10)),
      axis.title.y = element_text(size = 34, color = "black", margin = margin(r = 8)),
      axis.line = element_line(color = "black", linewidth = 0.6),
      axis.ticks = element_line(color = "black", linewidth = 0.6),
      axis.ticks.length = grid::unit(6, "pt"),
      plot.margin = margin(t = 16, r = 18, b = 18, l = 22, unit = "pt")
    )
}

dataset_files <- find_dataset_files()

for (i in seq_len(nrow(dataset_files))) {
  dataset_id <- dataset_files$DatasetID[i]
  dataset_name <- pretty_dataset_name(dataset_id)
  input_file <- dataset_files$File[i]
  output_file <- file.path(output_dir, paste0("set_size_by_rank_", dataset_id, ".pdf"))

  message("Reading ", input_file)
  df <- read_rank_set_size(input_file)

  message("Plotting ", dataset_name, " -> ", output_file)
  p <- make_plot(df, dataset_id, dataset_name)

  cairo_pdf(output_file, width = 8.6, height = 5.2)
  print(p)
  dev.off()
}

message("Done. Wrote ", nrow(dataset_files), " figure(s) to ", output_dir)
