
create_comparison_barplot <- function(
    data1, 
    data2, 
    labels, 
    fill_colors = c("#AD0626", "#B79AD1"),
    fill_names = c("Group 1", "Group 2"),
    x_label = "Workloads",
    y_label = "Value",
    export_name = NULL,
    export_path = "./",
    width = 12,
    height = 6,
    bar_width = 0.35,
    text_size = 13,
    axis_text_size = 42,
    legend_text_size = 30,
    show_legend = FALSE,
    legend_position = c(0.85, 0.85),
    show_data_labels = TRUE,
    use_arial = TRUE,
    y_max_multiplier = 1.35 # 新增参数：y轴上限倍数
) {
  # 加载必要的包
  library(ggplot2)
  
  # 字体设置
  if(use_arial) {
    # 尝试使用showtext包处理字体
    if(requireNamespace("showtext", quietly = TRUE)) {
      library(showtext)
      
      # 检查字体是否已加载
      if(requireNamespace("sysfonts", quietly = TRUE)) {
        library(sysfonts)
        
        # 检查Arial是否已添加
        if(!("Arial" %in% sysfonts::font_families())) {
          tryCatch({
            sysfonts::font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
          }, error = function(e) {
            warning("无法加载Arial字体: ", e$message)
          })
        }
        showtext::showtext_auto()
      } else {
        warning("sysfonts包不可用，无法加载Arial字体")
      }
    } else if(requireNamespace("extrafont", quietly = TRUE)) {
      # 尝试使用extrafont包
      library(extrafont)
      tryCatch({
        windowsFonts(Arial = windowsFont("Arial"))
      }, error = function(e) {
        warning("无法通过extrafont加载Arial字体: ", e$message)
      })
    } else {
      warning("showtext和extrafont包均不可用，使用系统默认字体")
    }
  }
  
  # 数据准备
  n_bars <- length(data1)
  x_positions <- 0:(n_bars-1)
  x1_positions <- x_positions - 0.5 * bar_width
  x2_positions <- x_positions + 0.5 * bar_width
  y_max <- max(c(data1, data2)) * y_max_multiplier
  # 绘图
  p <- ggplot() + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, y_max)) +  # 设置y轴上限
    # 柱状图
    geom_col(aes(x = x1_positions, y = data1, fill = fill_names[1]), width = bar_width, 
             color = "black", size = 0.5) +
    geom_col(aes(x = x2_positions, y = data2, fill = fill_names[2]), width = bar_width, 
             color = "black", size = 0.5) +
    # 填充颜色
    scale_fill_manual(name = "", values = setNames(fill_colors, fill_names)) +
    # 主题设置
    theme_classic()
  
  # 添加字体设置（仅当Arial可用时）
  if(use_arial) {
    p <- p + theme(text = element_text(family = "Arial"))
  }
  
  # 其他主题设置
  p <- p + theme(
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_text_size),
    axis.title.y = element_text(size = axis_text_size, hjust = 0.5)
  ) +
    # 标签设置
    labs(y = paste(" ", y_label, " "), x = x_label) +
    # x轴刻度设置
    scale_x_continuous(breaks = x_positions, labels = labels)
  
  # 添加数据标签(如果需要)
  if(show_data_labels) {
    p <- p +
      geom_text(aes(x = x2_positions, y = data2), hjust = 0,
                vjust = 0.5, label = round(data2, 2), angle = 90, 
                size = text_size, nudge_y = max(data2) * 0.05) +
      geom_text(aes(x = x1_positions, y = data1), hjust = 0,
                vjust = 0.5, label = round(data1, 2), angle = 90, 
                size = text_size, nudge_y = max(data1) * 0.05)
  }
  
  # 图例设置
  if(show_legend) {
    p <- p + theme(
      legend.position = legend_position,
      legend.text = element_text(size = legend_text_size),
      legend.margin = margin(b = 10)
    )
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # 保存文件(如果提供了文件名)
  if(!is.null(export_name)) {
    ggsave(paste0(export_path, export_name), plot = p, width = width, height = height)
  }
  
  return(p)
}


create_barplot_with_ci <- function(
    data_list,                # list，每个元素为一组数据向量
    labels,                   # x轴标签，长度等于组数
    fill_colors = NULL,       # 柱子颜色，长度等于组数
    fill_names = NULL,        # 图例名称，长度等于组数
    x_label = NULL,
    y_label = "Value",
    export_name = NULL,
    export_path = "./",
    width = 12,
    height = 6,
    bar_width = 0.5,
    text_size = 13,
    axis_text_size = 42,
    legend_text_size = 30,
    show_legend = FALSE,
    legend_position = c(0.85, 0.85),
    show_data_labels = TRUE,
    use_arial = TRUE,
    y_max_multiplier = 1.35
) {
  library(ggplot2)
  # 计算均值和置信区间
  summary_df <- data.frame(
    group = factor(labels, levels = labels),
    mean = sapply(data_list, mean, na.rm = TRUE),
    n = sapply(data_list, function(x) sum(!is.na(x))),
    sd = sapply(data_list, sd, na.rm = TRUE)
  )
  summary_df$se <- summary_df$sd / sqrt(summary_df$n)
  summary_df$ci95 <- summary_df$se * qt(0.975, df = summary_df$n - 1)
  summary_df$ymin <- summary_df$mean - summary_df$ci95
  summary_df$ymax <- summary_df$mean + summary_df$ci95

  # 颜色和图例
  if (is.null(fill_colors)) {
    fill_colors <- scales::hue_pal()(length(labels))
  }
  if (is.null(fill_names)) {
    fill_names <- labels
  }
  fill_map <- setNames(fill_colors, fill_names)

  y_max <- max(summary_df$ymax, na.rm = TRUE) * y_max_multiplier

  # 字体设置
  if(use_arial) {
    if(requireNamespace("showtext", quietly = TRUE)) {
      library(showtext)
      if(requireNamespace("sysfonts", quietly = TRUE)) {
        library(sysfonts)
        if(!("Arial" %in% sysfonts::font_families())) {
          tryCatch({
            sysfonts::font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
          }, error = function(e) {
            warning("无法加载Arial字体: ", e$message)
          })
        }
        showtext::showtext_auto()
      }
    } else if(requireNamespace("extrafont", quietly = TRUE)) {
      library(extrafont)
      tryCatch({
        windowsFonts(Arial = windowsFont("Arial"))
      }, error = function(e) {
        warning("无法通过extrafont加载Arial字体: ", e$message)
      })
    }
  }

  p <- ggplot(summary_df, aes(x = group, y = mean, fill = group)) +
    geom_col(width = bar_width, color = "black", size = 0.5) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = bar_width * 0.4, size = 1) +
    scale_fill_manual(values = fill_colors, labels = fill_names, name = "") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, y_max)) +
    labs(x = x_label, y = paste(" ", y_label, " ")) +
    theme_classic()

  if(use_arial) {
    p <- p + theme(text = element_text(family = "Arial"))
  }
  p <- p + theme(
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_text_size),
    axis.title.y = element_text(size = axis_text_size, hjust = 0.5)
  )

  if(show_data_labels) {
    p <- p + geom_text(aes(label = round(mean, 2), y = mean), 
                       vjust = -0.7, size = text_size)
  }

  if(show_legend) {
    p <- p + theme(
      legend.position = legend_position,
      legend.text = element_text(size = legend_text_size),
      legend.margin = margin(b = 10)
    )
  } else {
    p <- p + theme(legend.position = "none")
  }

  if(!is.null(export_name)) {
    ggsave(paste0(export_path, export_name), plot = p, width = width, height = height)
  }
  return(p)
}


# 修改后的函数：创建分组柱状图（每组4个柱子，8个分组）
create_grouped_barplot_with_ci <- function(
    data_matrix,              # 8x4的数据列表矩阵，data_matrix[[i]][[j]]为第i组第j个柱子的数据向量
    group_labels,             # 8个分组的标签
    bar_labels,               # 4个柱子的标签
    fill_colors = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12"),  # 4个柱子的颜色
    x_label = "Groups",
    y_label = "Value",
    export_name = NULL,
    export_path = "./",
    width = 16,
    height = 8,
    bar_width = 0.8,
    text_size = 10,
    x_axis_text_size = 14,    # 修改：x轴刻度文字大小
    y_axis_text_size = 14,    # 新增：y轴刻度文字大小
    x_label_size = 14,        # 新增：x轴标签字体大小
    y_label_size = 14,        # 新增：y轴标签字体大小
    legend_text_size = 12,
    show_legend = TRUE,
    legend_position = "top",
    show_data_labels = FALSE,
    use_arial = TRUE,
    y_max_multiplier = 1.2,
    dodge_width = 0.9,
    group_spacing = 1.0,      # 新增：分组间隔控制，1.0为默认间隔，值越大间隔越大
    x_text_angle = 45,         # x轴文字角度，0为横着，45为斜着
    remove_x_axis_space = FALSE,  # 是否完全移除x轴标签空间
    y_axis_margin = 0.5       # 新增：y轴左侧边距调整，值越小y轴越靠近第一个柱子
) {
  library(ggplot2)
  library(dplyr)
  
  # 数据验证
  if(length(data_matrix) != 8) {
    stop("data_matrix必须包含8个分组")
  }
  if(any(sapply(data_matrix, length) != 4)) {
    stop("每个分组必须包含4个柱子的数据")
  }
  
  # 计算每个柱子的统计信息
  summary_list <- list()
  for(i in 1:8) {
    for(j in 1:4) {
      data_vec <- data_matrix[[i]][[j]]
      n <- sum(!is.na(data_vec))
      mean_val <- mean(data_vec, na.rm = TRUE)
      sd_val <- sd(data_vec, na.rm = TRUE)
      se <- sd_val / sqrt(n)
      ci95 <- se * qt(0.975, df = n - 1)
      
      summary_list[[length(summary_list) + 1]] <- data.frame(
        group = group_labels[i],
        bar = bar_labels[j],
        mean = mean_val,
        ymin = mean_val - ci95,
        ymax = mean_val + ci95,
        n = n
      )
    }
  }

# 合并数据
  summary_df <- do.call(rbind, summary_list)
  summary_df$group <- factor(summary_df$group, levels = group_labels)
  summary_df$bar <- factor(summary_df$bar, levels = bar_labels)
  
  # 创建分组位置，考虑间隔控制
  # 将分组位置按照group_spacing进行调整
  group_positions <- (0:(length(group_labels)-1)) * group_spacing
  names(group_positions) <- group_labels
  summary_df$group_pos <- group_positions[as.character(summary_df$group)]
  
  # 计算y轴上限
  y_max <- max(summary_df$ymax, na.rm = TRUE) * y_max_multiplier
  
  # 字体设置
  if(use_arial) {
    if(requireNamespace("showtext", quietly = TRUE)) {
      library(showtext)
      if(requireNamespace("sysfonts", quietly = TRUE)) {
        library(sysfonts)
        if(!("Arial" %in% sysfonts::font_families())) {
          tryCatch({
            sysfonts::font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
          }, error = function(e) {
            warning("无法加载Arial字体: ", e$message)
          })
        }
        showtext::showtext_auto()
      }
    } else if(requireNamespace("extrafont", quietly = TRUE)) {
      library(extrafont)
      tryCatch({
        windowsFonts(Arial = windowsFont("Arial"))
      }, error = function(e) {
        warning("无法通过extrafont加载Arial字体: ", e$message)
      })
    }
  }
  
  # 创建图形 - 使用group_pos而不是group来控制x轴位置
  p <- ggplot(summary_df, aes(x = group_pos, y = mean, fill = bar)) +
    geom_col(position = position_dodge(width = dodge_width), 
             width = bar_width, color = "black", size = 0.3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), 
                  position = position_dodge(width = dodge_width),
                  width = 0.2, size = 0.5) +
    scale_fill_manual(values = setNames(fill_colors, bar_labels), 
                      name = "", labels = bar_labels) +
    # 修改：调整y轴的expand参数来减少y轴与第一个柱子的距离
    scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0, 0)), 
                       limits = c(0, y_max)) +
    # 设置x轴刻度位置和标签，并调整expand来减少左右边距
    scale_x_continuous(breaks = group_positions, labels = group_labels,
                       expand = expansion(mult = c(y_axis_margin, 0.05), add = c(0, 0))) +
    labs(x = x_label, y = paste(" ", y_label, " ")) +
    theme_classic()
  
  # 字体应用
  if(use_arial) {
    p <- p + theme(text = element_text(family = "Arial"))
  }
  
  # 主题设置 - 使用独立的字体大小参数，并修复x轴标签对齐问题
  if(remove_x_axis_space) {
    # 完全移除x轴标签和标题的空间
    p <- p + theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = y_axis_text_size, color = "black"),
      axis.title.y = element_text(size = y_label_size, hjust = 0.5),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")  # 减少底部边距
    )
  } else {
    # 修改：调整x轴文字的hjust和vjust来确保对齐
    hjust_val <- if(x_text_angle == 0) 0.5 else if(x_text_angle == 45) 1 else 0.5
    vjust_val <- if(x_text_angle == 0) 0.5 else if(x_text_angle == 45) 1 else 0.5
    
    p <- p + theme(
      axis.text.x = element_text(size = x_axis_text_size, color = "black", 
                                angle = x_text_angle, hjust = hjust_val, vjust = vjust_val),
      axis.text.y = element_text(size = y_axis_text_size, color = "black"),
      axis.title.x = element_text(size = x_label_size),
      axis.title.y = element_text(size = y_label_size, hjust = 0.5)
    )
  }
  
  # 数据标签
  if(show_data_labels) {
    p <- p + geom_text(aes(label = round(mean, 2)), 
                       position = position_dodge(width = dodge_width),
                       vjust = -0.5, size = text_size)
  }
  
  # 图例设置
  if(show_legend) {
    p <- p + theme(
      legend.position = legend_position,
      legend.text = element_text(size = legend_text_size),
      legend.margin = margin(b = 10)
    )
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # 保存文件
  if(!is.null(export_name)) {
    ggsave(paste0(export_path, export_name), plot = p, width = width, height = height, dpi = 300)
  }
  
  return(p)
}

create_grouped_two_with_stacked_right <- function(
  left_list,
  right_parts_list,
  group_labels = NULL,
  component_labels = c("C1", "C2", "C3"),
  left_fill = "#2E2B54",
  component_fills = c("#A61D24", "#78B0B8", "#B196C1"),
  x_label = "Datasets",
  y_label = "Time(s)",
  show_legend = TRUE,
  show_data_labels = FALSE,
  # 标签控制 —— 默认为水平居中显示在柱顶上方
  data_label_size = 4,
  data_label_angle = 0,
  data_label_hjust = 0.5,
  data_label_vjust = -0.2,
  label_margin_frac = 0.04,   # 标签与柱顶间距占总体最大值比例
  # 文字与布局
  axis_text_size = 12,
  axis_title_size = 14,
  legend_text_size = 10,
  use_arial = FALSE,
  # y轴/绘图导出
  y_max_multiplier = 1.25,
  export_name = NULL,
  export_path = "./",
  width = 12,
  height = 6,
  dpi = 300,
  # 左右偏移与柱宽
  left_right_offset = 0.23,
  bar_width = 0.22,
  # 可选的 y-break 支持（需 ggbreak 包）
  use_y_break = FALSE,
  break_start = NULL,
  break_end = NULL,
  break_scales = 0.6,
  ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("需要 ggplot2 包，请先 install.packages('ggplot2')")
  library(ggplot2)
  library(scales)

  # =========================================================================
  # 核心修复：自定义文本图层，自动过滤掉超出当前切分面板范围的文本标签
  # =========================================================================
  GeomTextOOB <- ggproto("GeomTextOOB", GeomText,
    draw_panel = function(data, panel_params, coord, ...) {
      # 将数据坐标转换为面板内的标准化坐标 (0 到 1)
      coords <- coord$transform(data, panel_params)
      # 过滤掉落在面板外部的文本（允许上下 10% 溢出以防正常标签被切掉）
      keep <- coords$y >= -0.1 & coords$y <= 1.1
      data <- data[keep, , drop = FALSE]
      if (nrow(data) == 0) return(grid::nullGrob())
      GeomText$draw_panel(data, panel_params, coord, ...)
    }
  )
  
  geom_text_oob <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    if (!missing(nudge_x) || !missing(nudge_y)) {
      if (!missing(position)) stop("You must specify either `position` or `nudge_x`/`nudge_y`.")
      position <- position_nudge(nudge_x, nudge_y)
    }
    layer(
      data = data, mapping = mapping, stat = stat, geom = GeomTextOOB, position = position,
      show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(parse = parse, check_overlap = check_overlap, na.rm = na.rm, ...)
    )
  }
  # =========================================================================

  if (use_arial) {
    if (requireNamespace("showtext", quietly = TRUE)) {
      library(showtext)
      if (requireNamespace("sysfonts", quietly = TRUE)) {
        library(sysfonts)
        if (!("Arial" %in% sysfonts::font_families())) {
          tryCatch({ sysfonts::font_add("Arial", "C:/Windows/Fonts/arial.ttf") }, error = function(e) {})
        }
        showtext::showtext_auto()
      }
    } else if (requireNamespace("extrafont", quietly = TRUE)) {
      library(extrafont)
      tryCatch({ windowsFonts(Arial = windowsFont("Arial")) }, error = function(e) {})
    }
  }

  group_count <- length(left_list)
  if (group_count < 1) stop("left_list 必须至少包含一组")
  if (length(right_parts_list) != group_count) stop("right_parts_list 长度须等于 left_list（组数）")
  if (any(sapply(right_parts_list, length) != length(component_labels))) {
    stop("right_parts_list 的每个元素必须包含与 component_labels 一致数量的部分")
  }
  if (is.null(group_labels)) group_labels <- paste0("G", seq_len(group_count))

  # 计算统计量
  long_rows <- list()
  total_stats <- data.frame(group = character(0), total_mean = numeric(0), ymin = numeric(0), ymax = numeric(0), stringsAsFactors = FALSE)

  for (i in seq_len(group_count)) {
    lv <- left_list[[i]]
    nL <- sum(!is.na(lv))
    meanL <- if (nL > 0) mean(lv, na.rm = TRUE) else NA
    sdL <- if (nL > 1) sd(lv, na.rm = TRUE) else NA
    seL <- if (!is.na(sdL) && nL > 1) sdL / sqrt(nL) else NA
    ciL <- if (!is.na(seL) && nL > 1) seL * qt(0.975, df = nL - 1) else 0

    parts <- right_parts_list[[i]]
    means_parts <- sapply(parts, function(x) if (sum(!is.na(x)) > 0) mean(x, na.rm = TRUE) else 0)

    len_parts <- sapply(parts, function(x) length(na.omit(x)))
    same_len <- length(unique(len_parts)) == 1 && len_parts[1] > 0

    if (same_len && len_parts[1] > 1) {
      total_vec <- Reduce(`+`, parts)
      nT <- sum(!is.na(total_vec))
      meanT <- mean(total_vec, na.rm = TRUE)
      sdT <- sd(total_vec, na.rm = TRUE)
      seT <- sdT / sqrt(nT)
      ciT <- seT * qt(0.975, df = nT - 1)
    } else {
      meanT <- sum(means_parts)
      ciT <- 0
      if (!same_len) warning(sprintf("第 %d 组右侧部分样本长度不一致或样本数不足，无法计算总和的置信区间；将 CI 设为 0。", i))
    }
    yminT <- meanT - ciT
    ymaxT <- meanT + ciT

    long_rows[[length(long_rows) + 1]] <- data.frame(group = group_labels[i], type = "Left", component = "Left", value = meanL, ymin = meanL - ciL, ymax = meanL + ciL, stringsAsFactors = FALSE)
    for (j in seq_along(component_labels)) {
      long_rows[[length(long_rows) + 1]] <- data.frame(group = group_labels[i], type = "Right", component = component_labels[j], value = means_parts[j], ymin = NA, ymax = NA, stringsAsFactors = FALSE)
    }
    total_stats <- rbind(total_stats, data.frame(group = group_labels[i], total_mean = meanT, ymin = yminT, ymax = ymaxT, stringsAsFactors = FALSE))
  }

  plot_df <- do.call(rbind, long_rows)
  plot_df$group <- factor(plot_df$group, levels = group_labels)
  plot_df$type <- factor(plot_df$type, levels = c("Left", "Right"))
  plot_df$component <- factor(plot_df$component, levels = c("Left", component_labels))
  plot_df$group_pos <- as.numeric(plot_df$group)

  # 颜色映射
  fill_values <- c(left_fill, component_fills)
  names(fill_values) <- c("Left", component_labels)

  base_max <- max(c(plot_df$value, total_stats$ymax), na.rm = TRUE)

  # 强制左右两个柱子紧贴（中心间距 = bar_width）
  left_right_offset <- bar_width / 2

  # 估算标签需要的额外空间：根据最大数字的字符长度增加少量偏移
  all_label_vals <- c(plot_df$value, total_stats$total_mean)
  # 只保留有限数值
  all_label_vals <- all_label_vals[is.finite(all_label_vals) & !is.na(all_label_vals)]
  if (length(all_label_vals) == 0) {
    max_digits <- 0
  } else {
    # 四舍五入到整数后计算字符长度（不含千分位分隔符）
    max_digits <- max(nchar(as.character(round(all_label_vals, 0))), na.rm = TRUE)
  }

  # 基础偏移（原来的比例） + 基于位数的额外偏移
  # extra_digit_frac: 每超出 base_digit_threshold 位，额外加 base_max * extra_digit_frac
  base_digit_threshold <- 3
  extra_digit_frac <- 0.065   # 每多一位额外增加 base_max 的 1%（可调整）
  extra_digits <- pmax(0, max_digits - base_digit_threshold)
  digit_extra <- if (is.finite(base_max) && base_max > 0) base_max * extra_digit_frac * extra_digits else 0

  label_offset <- if (is.finite(base_max) && base_max > 0) base_max * label_margin_frac + digit_extra else 1

  # y 上限在原来基础上增加标签空间，避免被裁剪
  # 把预留空间略放大以保证被切掉的风险更小
  y_limit_max <- if (is.finite(base_max)) base_max * y_max_multiplier + label_offset * 1.5 else NULL

  # 绘图主体
  p <- ggplot() +
    scale_x_continuous(breaks = seq_along(group_labels), labels = group_labels) +
    scale_fill_manual(name = "", values = fill_values) +
    labs(x = x_label, y = paste(" ", y_label, " ")) +
    theme_classic()

  # 右侧堆叠条与左侧单条
  p <- p + geom_col(data = subset(plot_df, type == "Right"), aes(x = group_pos + left_right_offset, y = value, fill = component), stat = "identity", width = bar_width, color = "black")
  p <- p + geom_col(data = subset(plot_df, type == "Left"), aes(x = group_pos - left_right_offset, y = value, fill = component), stat = "identity", width = bar_width, color = "black")

  # 字体支持
  if (use_arial && requireNamespace("showtext", quietly = TRUE) && requireNamespace("sysfonts", quietly = TRUE)) {
    library(showtext); library(sysfonts)
    if (!("Arial" %in% sysfonts::font_families())) tryCatch(sysfonts::font_add("Arial", "C:/Windows/Fonts/arial.ttf"), error = function(e) {})
    showtext::showtext_auto()
    p <- p + theme(text = element_text(family = "Arial"))
  }

  # y 轴设置（含为标签预留空间）
  if (!is.null(y_limit_max)) {
    p <- p + scale_y_continuous(expand = c(0, 0), limits = c(0, y_limit_max))
  } else {
    p <- p + scale_y_continuous(expand = c(0, 0))
  }

  if (use_y_break && requireNamespace("ggbreak", quietly = TRUE) && !is.null(break_start) && !is.null(break_end)) {
    p <- p + ggbreak::scale_y_break(breaks = c(break_start, break_end), scales = break_scales)
  }

  # 主题大小
  p <- p + theme(
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size, margin = margin(t = 6)),
    axis.title.y = element_text(size = axis_title_size, hjust = 0.5, margin = margin(r = 6)),
    legend.text = element_text(size = legend_text_size),
    legend.position = if (show_legend) "top" else "none",
    plot.margin = margin(t = 8, r = 8, b = 6, l = 6)
  )

  # 数据标签：放在柱顶上方并水平居中
  if (show_data_labels) {
    # 左侧：y = value + offset
    left_labels_df <- subset(plot_df, type == "Left" & !is.na(value))
    if (nrow(left_labels_df) > 0) {
      left_labels_df$ypos <- left_labels_df$value + label_offset
      p <- p + geom_text(data = left_labels_df, aes(x = group_pos - left_right_offset, y = ypos, label = round(value, 2)), angle = data_label_angle, size = data_label_size, hjust = data_label_hjust, vjust = data_label_vjust)
    }

    # 右侧总和：y = total_mean + offset
    total_stats <- total_stats[!is.na(total_stats$total_mean), , drop = FALSE]
    if (nrow(total_stats) > 0) {
      total_stats$ypos <- total_stats$total_mean + label_offset
      p <- p + geom_text(data = total_stats, aes(x = as.numeric(factor(group, levels = group_labels)) + left_right_offset, y = ypos, label = round(total_mean, 2)), angle = data_label_angle, size = data_label_size, hjust = data_label_hjust, vjust = data_label_vjust)
      # 如果有 CI，则绘制 errorbar（在总和上方）
      if (any(!is.na(total_stats$ymin))) {
        p <- p + geom_errorbar(data = total_stats, aes(x = as.numeric(factor(group, levels = group_labels)) + left_right_offset, ymin = ymin, ymax = ymax), width = bar_width * 0.25, size = 0.6)
      }
    }
  }

  # 保存
  if (!is.null(export_name)) {
    dir.create(export_path, showWarnings = FALSE, recursive = TRUE)
    allowed <- c("dpi", "units", "device", "bg", "scale", "limitsize")
    dots <- list(...)
    ggsave_args <- dots[names(dots) %in% allowed]
    base_args <- list(filename = file.path(export_path, export_name), plot = p, width = width, height = height, dpi = dpi)
    do.call(ggsave, c(base_args, ggsave_args))
  }

  return(p)
}

create_single_barplot <- function(
    data,
    labels,
    fill_color = "#AD0626",
    fill_name = "Value",
    x_label = "",
    y_label = "",
    export_name = NULL,
    export_path = "./",
    width = 8,
    height = 5,
    show_legend = FALSE,
    show_data_labels = FALSE,
    axis_text_size = 40, 
    label_text_size = 40, 
    y_max_multiplier = 1.35,
    bar_width = 0.35,
    y_breaks = NULL   # 新增参数：y轴刻度
) {
    library(ggplot2)
    library(scales)

    n <- length(data)
    df <- data.frame(
        label = factor(labels, levels = labels),
        value = data * 100,
        group = fill_name
    )

    y_max <- max(df$value, na.rm = TRUE) * y_max_multiplier

    # 自动或手动设置y轴刻度
    if (is.null(y_breaks)) {
        y_breaks_use <- pretty(c(0, y_max), n = 5)
    } else {
        y_breaks_use <- y_breaks
    }

    p <- ggplot(df, aes(x = label, y = value, fill = group)) +
        geom_col(width = bar_width, color = "black") +
        scale_fill_manual(values = setNames(fill_color, fill_name)) +
        scale_y_continuous(
            expand = c(0, 0),
            limits = c(0, y_max),
            breaks = y_breaks_use,
            labels = percent_format(scale = 1)
        ) +
        scale_x_discrete(labels = labels) +
        labs(x = x_label, y = y_label) +
        theme_classic() +
        theme(
            axis.text.x = element_text(size = axis_text_size),
            axis.text.y = element_text(size = axis_text_size),
            axis.title.x = element_text(size = label_text_size),
            axis.title.y = element_text(size = label_text_size),
            legend.position = if (show_legend) "top" else "none"
        )

    if (show_data_labels) {
        p <- p + geom_text(aes(label = paste0(round(value, 1), "%")), vjust = -0.5, size = axis_text_size * 0.28)
    }

    if (!is.null(export_name)) {
        ggsave(filename = file.path(export_path, export_name), plot = p, width = width, height = height)
    } else {
        print(p)
    }
}