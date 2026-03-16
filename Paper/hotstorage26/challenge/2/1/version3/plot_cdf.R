find_line_no_point_script <- function(start_dir, max_depth = 10) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)

  for (i in seq_len(max_depth + 1)) {
    candidate <- file.path(current_dir, "MyR", "Line_NoPoint.R")
    if (file.exists(candidate)) {
      return(candidate)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      break
    }
    current_dir <- parent_dir
  }

  NA_character_
}

plot_superfeature_hit_cdf <- function(txt_path, pdf_path, title = NULL) {
  if (!file.exists(txt_path)) {
    stop(sprintf("Input file does not exist: %s", txt_path), call. = FALSE)
  }

  df <- tryCatch(
    read.csv(
      txt_path,
      header = FALSE,
      col.names = c("super_feature_id", "hit_count"),
      sep = ",",
      stringsAsFactors = FALSE
    ),
    error = function(e) {
      stop(
        paste0(
          "Failed to read input file as a two-column comma-separated file: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  if (ncol(df) != 2) {
    stop("Input file must contain exactly two columns.", call. = FALSE)
  }

  if (nrow(df) == 0) {
    stop("Input file is empty.", call. = FALSE)
  }

  hit_count <- suppressWarnings(as.numeric(df$hit_count))
  if (any(is.na(hit_count))) {
    stop("Column 2 (hit_count) must be numeric for all rows.", call. = FALSE)
  }

  df$hit_count <- hit_count

  total_hits <- sum(df$hit_count)
  if (!is.finite(total_hits) || total_hits <= 0) {
    stop("Total hit_count must be greater than 0.", call. = FALSE)
  }

  # Merge duplicated features first to avoid double-counting.
  df <- aggregate(hit_count ~ super_feature_id, data = df, FUN = sum)
  df <- df[order(df$hit_count, decreasing = TRUE), , drop = FALSE]

  feature_frac <- seq_len(nrow(df)) / nrow(df)
  hit_frac <- cumsum(df$hit_count) / sum(df$hit_count)
  cdf_df <- data.frame(
    feature_frac = c(0, feature_frac),
    hit_frac = c(0, hit_frac)
  )

  if (!exists("plot_line_comparison_xcdf", mode = "function")) {
    line_script <- find_line_no_point_script(dirname(txt_path))
    if (is.na(line_script)) {
      stop("Cannot locate MyR/Line_NoPoint.R from txt_path.", call. = FALSE)
    }
    source(line_script)
  }

  output_dir <- dirname(pdf_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  export_path <- if (output_dir %in% c(".", "")) {
    "./"
  } else {
    paste0(normalizePath(output_dir, winslash = "/", mustWork = FALSE), "/")
  }

  plot_data <- data.frame(cdf = cdf_df$hit_frac)
  plot_width <- 11
  plot_height <- 6.5

  p <- plot_line_comparison_xcdf(
    data = plot_data,
    export_path = export_path,
    export_name = basename(pdf_path),
    x_lim = c(0, 1),
    y_lim = c(0, 1),
    line_size = 2.8,
    axis_text_size = 30,
    x_title_size = 34,
    y_title_size = 34,
    x_breaks = seq(0, 1, 0.2),
    y_breaks = seq(0, 1, 0.2),
    x_label = "Candidate Set",
    y_label = "Proportion",
    plot_width = plot_width,
    plot_height = plot_height,
    x_expand = c(0, 0),
    y_expand = c(0, 0)
  )

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.", call. = FALSE)
  }

  p_final <- p +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 20, r = 30, b = 35, l = 40, unit = "pt"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 12))
    )

  if (!is.null(title) && nzchar(title)) {
    p_final <- p_final + ggplot2::ggtitle(title)
  }

  ggplot2::ggsave(
    pdf_path,
    plot = p_final,
    width = plot_width,
    height = plot_height
  )

  invisible(cdf_df)
}

args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grepl("^--file=", args)]
script_dir <- if (length(script_arg) > 0) {
  dirname(
    normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = FALSE)
  )
} else if (!is.null(sys.frames()[[1]]$ofile)) {
  dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = FALSE))
} else {
  getwd()
}

input_txt <- file.path(script_dir, "linux.txt")
output_pdf <- file.path(script_dir, "linux_cdf.pdf")
plot_superfeature_hit_cdf(input_txt, output_pdf)
