
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
    x_text_angle = 0,         # x轴文字角度，0为横着，45为斜着
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

# 每组两个柱子：左侧单一柱（mean + CI），右侧为三部分堆叠（堆叠部分显示 mean，顶部显示总和的 95% CI）
create_grouped_two_with_stacked_right <- function(
  left_list,          # list: 每组左侧柱子的样本向量，长度 = 组数
  right_parts_list,   # list: 每组的右侧堆叠部分，形如 list(list(part1_vec, part2_vec, part3_vec), ...)，长度 = 组数
  group_labels = NULL,
  component_labels = c("C1","C2","C3"),
  left_fill = "#6BAED6",
  component_fills = c("#E41A1C","#4DAF4A","#984EA3"),
  x_label = "Group",
  y_label = "Value",
  show_legend = TRUE,
  show_data_labels = FALSE,
  use_arial = FALSE,
  y_max_multiplier = 1.1,
  export_name = NULL,
  export_path = "./",
  width = 10,
  height = 6,
  dpi = 300,
  normalize_to_left = TRUE,
  ...                 # 额外给 ggsave 的参数（谨慎使用，见说明）
) {
  if(!requireNamespace("ggplot2", quietly = TRUE)) stop("需要 ggplot2 包，请先 install.packages('ggplot2')")
  library(ggplot2)

  # 基本检查
  group_count <- length(left_list)
  if(group_count < 1) stop("left_list 必须至少包含一组")
  if(length(right_parts_list) != group_count) stop("right_parts_list 长度须等于 left_list（组数）")

  # 每组应该包含三个部分
  parts_per_group <- sapply(right_parts_list, length)
  if(any(parts_per_group != 3)) stop("right_parts_list 的每个元素必须包含 3 个部分的向量")

  # 生成 group_labels
  if(is.null(group_labels)) group_labels <- paste0("G", seq_len(group_count))

  # 计算统计量：左侧 mean/ci；右侧每部分 mean，及右侧总和的 mean/ci（需要同一组内三部分样本长度相同）
  long_rows <- list()
  total_stats <- data.frame(group = character(0), total_mean = numeric(0), ymin = numeric(0), ymax = numeric(0), stringsAsFactors = FALSE)
  for(i in seq_len(group_count)) {
    # left
    lv <- left_list[[i]]
    nL <- sum(!is.na(lv))
    meanL <- if(nL>0) mean(lv, na.rm = TRUE) else NA
    sdL <- if(nL>1) sd(lv, na.rm = TRUE) else NA
    seL <- if(!is.na(sdL) && nL>1) sdL / sqrt(nL) else NA
    ciL <- if(!is.na(seL) && nL>1) seL * qt(0.975, df = nL - 1) else 0

    # right parts
    p1 <- right_parts_list[[i]][[1]]
    p2 <- right_parts_list[[i]][[2]]
    p3 <- right_parts_list[[i]][[3]]

    # 检查右侧三部分是否为同长度（若不是，无法逐样本求总和 CI）
    len_parts <- c(length(na.omit(p1)), length(na.omit(p2)), length(na.omit(p3)))
    same_len <- length(unique(len_parts)) == 1

    mean_p1 <- if(sum(!is.na(p1))>0) mean(p1, na.rm = TRUE) else 0
    mean_p2 <- if(sum(!is.na(p2))>0) mean(p2, na.rm = TRUE) else 0
    mean_p3 <- if(sum(!is.na(p3))>0) mean(p3, na.rm = TRUE) else 0

    # 计算右侧总和的 mean & CI：优选对逐样本相加计算 CI（需三部分长度相同）
    if(same_len && len_parts[1] > 1) {
      total_vec <- (p1 + p2 + p3)
      nT <- sum(!is.na(total_vec))
      meanT <- mean(total_vec, na.rm = TRUE)
      sdT <- sd(total_vec, na.rm = TRUE)
      seT <- sdT / sqrt(nT)
      ciT <- seT * qt(0.975, df = nT - 1)
    } else {
      # 退化策略：将总 mean 近似为三部分 mean 之和，CI 设为 0（并发出警告）
      meanT <- mean_p1 + mean_p2 + mean_p3
      ciT <- 0
      warning(sprintf("第 %d 组右侧三部分样本长度不一致或样本数不足，无法计算总和的置信区间；将 CI 设为 0。", i))
    }

    yminT <- meanT - ciT
    ymaxT <- meanT + ciT

    # 构建用于绘图的分组长表（两列位置：左为 type = 'L', 右为 'R' 的堆叠行）
    # 左侧单一柱（用 bar 为 "Left"）
    long_rows[[length(long_rows)+1]] <- data.frame(
      group = group_labels[i],
      type = "Left",
      component = "Left",
      value = meanL,
      ymin = meanL - ciL,
      ymax = meanL + ciL,
      stringsAsFactors = FALSE
    )
    # 右侧三部分（作为堆叠，每个 component 一行）
    long_rows[[length(long_rows)+1]] <- data.frame(group = group_labels[i], type = "Right", component = component_labels[1], value = mean_p1, ymin = NA, ymax = NA, stringsAsFactors = FALSE)
    long_rows[[length(long_rows)+1]] <- data.frame(group = group_labels[i], type = "Right", component = component_labels[2], value = mean_p2, ymin = NA, ymax = NA, stringsAsFactors = FALSE)
    long_rows[[length(long_rows)+1]] <- data.frame(group = group_labels[i], type = "Right", component = component_labels[3], value = mean_p3, ymin = NA, ymax = NA, stringsAsFactors = FALSE)

    total_stats <- rbind(total_stats, data.frame(group = group_labels[i], total_mean = meanT, ymin = yminT, ymax = ymaxT, stringsAsFactors = FALSE))
  }

  plot_df <- do.call(rbind, long_rows)
  plot_df$group <- factor(plot_df$group, levels = group_labels)
  plot_df$type <- factor(plot_df$type, levels = c("Left","Right"))
  plot_df$component <- factor(plot_df$component, levels = c("Left", component_labels))

  # x positions: for each group, two x values (1..G), use dodge offset within group for left/right
  plot_df$group_pos <- as.numeric(plot_df$group)
  # build stacked position by ggplot stacking (no manual cumulative necessary)

  # 归一化：使左边柱子都为1
  if(normalize_to_left) {
    for(i in seq_len(group_count)) {
      # 找到这一组的左边值
      left_idx <- which(plot_df$group == group_labels[i] & plot_df$type == "Left")
      if(length(left_idx) > 0) {
        scale_factor <- plot_df$value[left_idx[1]]
        
        if(scale_factor != 0 && !is.na(scale_factor)) {
          # 缩放 plot_df
          group_mask <- plot_df$group == group_labels[i]
          plot_df$value[group_mask] <- plot_df$value[group_mask] / scale_factor
          plot_df$ymin[group_mask] <- plot_df$ymin[group_mask] / scale_factor
          plot_df$ymax[group_mask] <- plot_df$ymax[group_mask] / scale_factor
          
          # 缩放 total_stats
          total_mask <- total_stats$group == group_labels[i]
          total_stats$total_mean[total_mask] <- total_stats$total_mean[total_mask] / scale_factor
          total_stats$ymin[total_mask] <- total_stats$ymin[total_mask] / scale_factor
          total_stats$ymax[total_mask] <- total_stats$ymax[total_mask] / scale_factor
        }
      }
    }
  }

  # 绘图
  p <- ggplot() +
    # 右侧堆叠条（只取 type == "Right"）
    geom_col(data = subset(plot_df, type == "Right"), 
             aes(x = group_pos + 0.25, y = value, fill = component),
             stat = "identity", width = 0.45, color = "black") +
    # 左侧单一条
    geom_col(data = subset(plot_df, type == "Left"),
             aes(x = group_pos - 0.25, y = value, fill = component),
             stat = "identity", width = 0.45, color = "black") +
    scale_x_continuous(breaks = seq_along(group_labels), labels = group_labels) +
    labs(x = x_label, y = y_label)

  # 颜色映射：左使用 left_fill（component == "Left"），右三部分使用 component_fills
  # 创建填充向量，按 component factor levels 顺序设值
  fill_values <- c(left_fill, component_fills)
  names(fill_values) <- c("Left", component_labels)
  p <- p + scale_fill_manual(values = fill_values, name = "")

  # 主题与文字
  if(use_arial && requireNamespace("showtext", quietly = TRUE) && requireNamespace("sysfonts", quietly = TRUE)) {
    sysfonts::font_add("Arial", "C:/Windows/Fonts/arial.ttf")
    showtext::showtext_auto()
    p <- p + theme_classic() + theme(text = element_text(family = "Arial"))
  } else {
    p <- p + theme_classic()
  }
  p <- p + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = if(show_legend) "top" else "none")

  # 数据标签（若需要，仅显示数值）
  if(show_data_labels) {
    # 左标签
    p <- p + geom_text(data = subset(plot_df, type == "Left"), aes(x = group_pos - 0.25, y = value, label = round(value,2)), vjust = -0.5, size = 3)
    # 右顶部标签（显示总和）
    p <- p + geom_text(data = total_stats, aes(x = as.numeric(factor(group, levels = group_labels)) + 0.25, y = total_mean, label = round(total_mean,2)), vjust = -0.5, size = 3)
  }

  # y 轴上限放大
  y_top <- max(c(plot_df$value, total_stats$ymax), na.rm = TRUE)
  if(!is.na(y_top) && is.finite(y_top)) p <- p + scale_y_continuous(expand = expansion(mult = c(0,0.02)), limits = c(0, y_top * y_max_multiplier))

  # 保存
  if(!is.null(export_name)) {
    dir.create(export_path, showWarnings = FALSE, recursive = TRUE)
    dots <- list(...)
    # 允许的 ggsave 参数（可按需扩展）
    allowed <- c("dpi","units","device","bg","scale","limitsize")
    ggsave_args <- dots[names(dots) %in% allowed]
    base_args <- list(filename = file.path(export_path, export_name), plot = p, width = width, height = height, dpi = dpi)
    final_args <- c(base_args, ggsave_args)
    do.call(ggsave, final_args)
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