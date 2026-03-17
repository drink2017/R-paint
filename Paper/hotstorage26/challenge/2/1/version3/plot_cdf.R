  read_superfeature_hit_cdf <- function(txt_path) {                                                           
    if (!file.exists(txt_path)) {                                                                             
      stop(sprintf("Input file does not exist: %s", txt_path), call. = FALSE)                                 
    }                                                                                                         
                                                                                                              
    df <- tryCatch(                                                                                           
      read.csv(                                                                                               
        txt_path,                                                                                             
        header = FALSE,                                                                                       
        col.names = c("super_feature_id", "hit_count"),                                                       
        sep = ",",                                                                                            
        colClasses = c("character", "numeric"),                                                               
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
      stop(sprintf("Input file is empty: %s", txt_path), call. = FALSE)                                       
    }                                                                                                         
                                                                                                              
    if (any(is.na(df$hit_count))) {                                                                           
      stop(                                                                                                   
        sprintf("Column 2 (hit_count) must be numeric for all rows: %s", txt_path),                           
        call. = FALSE                                                                                         
      )                                                                                                       
    }                                                                                                         
                                                                                                              
    df <- aggregate(hit_count ~ super_feature_id, data = df, FUN = sum)                                       
    df <- df[order(df$hit_count, decreasing = TRUE), , drop = FALSE]                                          
                                                                                                              
    total_hits <- sum(df$hit_count)                                                                           
    if (!is.finite(total_hits) || total_hits <= 0) {                                                          
      stop(sprintf("Total hit_count must be greater than 0: %s", txt_path), call. = FALSE)                    
    }                                                                                                         
                                                                                                              
    data.frame(                                                                                               
      feature_frac = c(0, seq_len(nrow(df)) / nrow(df)),                                                      
      hit_frac = c(0, cumsum(df$hit_count) / total_hits),                                                     
      series = tools::file_path_sans_ext(basename(txt_path)),                                                 
      stringsAsFactors = FALSE                                                                                
    )                                                                                                         
  }                                                                                                           
                                                                                                              
  plot_superfeature_hit_cdfs <- function(txt_paths, pdf_path, title = NULL) {                                 
    if (length(txt_paths) == 0) {                                                                             
      stop("No txt files were found to plot.", call. = FALSE)                                                 
    }                                                                                                         
                                                                                                              
    if (!requireNamespace("ggplot2", quietly = TRUE)) {                                                       
      stop("Package 'ggplot2' is required but not installed.", call. = FALSE)                                 
    }                                                                                                         
                                                                                                              
    cdf_list <- lapply(txt_paths, read_superfeature_hit_cdf)                                                  
    plot_df <- do.call(rbind, cdf_list)                                                                       
    series_levels <- vapply(cdf_list, function(x) x$series[[1]], character(1))                                
    plot_df$series <- factor(plot_df$series, levels = series_levels)                                          
                                                                                                              
    base_colors <- c("#E8B15E", "#78B0B8", "#B196C1", "#A61D24")                                              
    if (length(series_levels) > length(base_colors)) {                                                        
      extra_colors <- grDevices::hcl.colors(                                                                  
        length(series_levels) - length(base_colors),                                                          
        palette = "Dark 3"                                                                                    
      )                                                                                                       
      base_colors <- c(base_colors, extra_colors)                                                             
    }                                                                                                         
    line_colors <- stats::setNames(base_colors[seq_along(series_levels)], series_levels)                      
                                                                                                              
    output_dir <- dirname(pdf_path)                                                                           
    if (!dir.exists(output_dir)) {                                                                            
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)                                          
    }                                                                                                         
                                                                                                              
    plot_width <- 11                                                                                          
    plot_height <- 6.5                                                                                        
                                                                                                              
    p <- ggplot2::ggplot(                                                                                     
      plot_df,                                                                                                
      ggplot2::aes(x = feature_frac, y = hit_frac, color = series)                                            
    ) +                                                                                                       
      ggplot2::geom_line(size = 2.8, lineend = "round") +                                                     
      ggplot2::scale_color_manual(
        values = line_colors,
        guide = ggplot2::guide_legend(
          ncol = 1,
          byrow = TRUE,
          keyheight = grid::unit(22, "pt")
        )
      ) +                                                     
      ggplot2::scale_x_continuous(                                                                            
        limits = c(0, 1),                                                                                     
        breaks = seq(0, 1, 0.2),                                                                              
        expand = ggplot2::expansion(mult = c(0, 0.04))                                                        
      ) +                                                                                                     
      ggplot2::scale_y_continuous(                                                                            
        limits = c(0, 1),                                                                                     
        breaks = seq(0.2, 1, 0.2),                                                                            
        expand = ggplot2::expansion(mult = c(0.01, 0))                                                        
      ) +                                                                                                     
      ggplot2::labs(                                                                                          
        x = "Candidate Set",                                                                                  
        y = "Proportion",                                                                                     
        color = NULL                                                                                          
      ) +                                                                                                     
      ggplot2::theme_classic() +                                                                              
      ggplot2::theme(                                                                                         
        plot.margin = ggplot2::margin(t = 20, r = 20, b = 35, l = 40, unit = "pt"),                           
        axis.text = ggplot2::element_text(size = 34, color = "black"),                                        
        axis.title.x = ggplot2::element_text(size = 34, margin = ggplot2::margin(t = 12)),                    
        axis.title.y = ggplot2::element_text(size = 34, margin = ggplot2::margin(r = 12)),                    
        legend.position = c(0.80, 0.80),                                                                      
        legend.justification = c(0, 1),                                                                       
        legend.background = ggplot2::element_blank(),                                                         
        legend.key = ggplot2::element_blank(),                                                                
        legend.key.height = grid::unit(28, "pt"),                                     
        legend.spacing.y = grid::unit(28, "pt"),  
        legend.text = ggplot2::element_text(size = 34, color = "black")                                                             
      )                                                                                                       
                                                                                                              
    if (!is.null(title) && nzchar(title)) {                                                                   
      p <- p + ggplot2::ggtitle(title)                                                                        
    }                                                                                                         
                                                                                                              
    ggplot2::ggsave(                                                                                          
      filename = pdf_path,                                                                                    
      plot = p,                                                                                               
      width = plot_width,                                                                                     
      height = plot_height                                                                                    
    )                                                                                                         
                                                                                                              
    invisible(plot_df)                                                                                        
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
                                                                                                              
  input_txts <- sort(list.files(script_dir, pattern = "\\.txt$", full.names = TRUE))                          
  output_pdf <- file.path(script_dir, "all_cdf.pdf")                                                          
                                                                                                              
  plot_superfeature_hit_cdfs(input_txts, output_pdf) 
