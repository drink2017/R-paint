library(here)
library(readxl)
create_grouped_barplot_with_ci <- function(
    data_matrix,              # 8x5的数据列表矩阵
    group_labels,             # 8个分组的标签
    bar_labels,               # 5个柱子的标签
    fill_colors = c("#F2BE5C", "#1d84c9", "#B79AD1", "#75B8BF", "#FF5809"),  # 5个柱子的颜色
    x_label = "Groups",
    y_label = "Value",
    export_name = NULL,
    export_path = "./",
    width = 16,
    height = 8,
    bar_width = 0.8,
    text_size = 10,
    x_axis_text_size = 14,
    y_axis_text_size = 14,
    x_label_size = 14,
    y_label_size = 14,
    legend_text_size = 12,
    show_legend = TRUE,
    legend_position = "top",
    show_data_labels = FALSE,
    use_arial = TRUE,
    y_max_multiplier = 1.2,
    dodge_width = 0.9,
    group_spacing = 1.0,
    x_text_angle = 0,
    remove_x_axis_space = FALSE,
    y_axis_margin = 0.5
) {
  library(ggplot2)
  library(dplyr)
  
  # 数据验证
  if(length(data_matrix) != 8) {
    stop("data_matrix必须包含8个分组")
  }
  if(any(sapply(data_matrix, length) != 5)) {
    stop("每个分组必须包含5个柱子的数据")
  }
  
  # 计算每个柱子的统计信息
  summary_list <- list()
  for(i in 1:8) {
    for(j in 1:5) {
      data_vec <- data_matrix[[i]][[j]]
      n <- sum(!is.na(data_vec))
      mean_val <- mean(data_vec, na.rm = TRUE)
      sd_val <- sd(data_vec, na.rm = TRUE)
      se <- sd_val / sqrt(n)
      # 95%置信区间计算（修正版本）
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
  
  # 创建分组位置
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
  
  # 创建图形
  p <- ggplot(summary_df, aes(x = group_pos, y = mean, fill = bar)) +
    geom_col(position = position_dodge(width = dodge_width), 
             width = bar_width, color = "black", size = 0.3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), 
                  position = position_dodge(width = dodge_width),
                  width = 0.2, size = 0.5) +
    scale_fill_manual(values = setNames(fill_colors, bar_labels), 
                      name = "", labels = bar_labels) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0, 0)), 
                       limits = c(0, y_max)) +
    scale_x_continuous(breaks = group_positions, labels = group_labels,
                       expand = expansion(mult = c(y_axis_margin, 0.05), add = c(0, 0))) +
    labs(x = x_label, y = paste(" ", y_label, " ")) +
    theme_classic()
  
  # 字体应用
  if(use_arial) {
    p <- p + theme(text = element_text(family = "Arial"))
  }
  
  # 主题设置
  if(remove_x_axis_space) {
    p <- p + theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = y_axis_text_size, color = "black"),
      axis.title.y = element_text(size = y_label_size, hjust = 0.5),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    )
  } else {
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



# 读取Excel数据
excel_path <- here("Paper/DSN/Restore.xlsx")
compression_data <- read_excel(excel_path)

# 获取列名
column_names <- colnames(compression_data)
num_columns <- length(column_names)

# 参数设置
GROUP_SIZE <- 5  # 每组的方法数量（TL, ML, MO, FineTAR, zstd）
INTERVAL <- 8    # 数据集数量

# 确保输出文件夹存在
plot_dir <- here("Paper/DSN/RestoreBar")
if(!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# 设定方法的顺序和颜色（包含5个方法）
desired_order <- c("TL", "ML", "MO", "FineTAR", "zstd")
fill_colors <- c("#F2BE5C", "#1d84c9", "#B79AD1", "#75B8BF", "#FF5809")  # 5种颜色

# 准备分组数据矩阵（先按原始顺序收集所有数据）
all_data_matrix <- list()
all_group_labels <- c()

# 为8个数据集收集数据（按原始顺序）
for(i in 1:INTERVAL) {
  # 计算当前组包含的列索引
  col_indices <- seq(from = i, to = num_columns, by = INTERVAL)
  
  # 如果最终选择的列数超过GROUP_SIZE，只取前GROUP_SIZE个
  if(length(col_indices) > GROUP_SIZE) {
    col_indices <- col_indices[1:GROUP_SIZE]
  }
  
  # 提取当前组的列名和系统名称
  current_col_names <- column_names[col_indices]
  system_names <- gsub("SA_BiSearch_", "", current_col_names)
  
  # 获取数据集名称
  dataset_parts <- strsplit(system_names[length(system_names)], "_")[[1]]
  dataset_name <- dataset_parts[length(dataset_parts)]
  all_group_labels[i] <- dataset_name
  
  # 创建当前组的数据列表
  group_data <- list()
  
  # 提取每列数据
  for(j in 1:length(col_indices)) {
    col_idx <- col_indices[j]
    col_data <- compression_data[[col_idx]]
    valid_data <- col_data[!is.na(col_data)]
    
    if(length(valid_data) > 0) {
      group_data[[j]] <- valid_data
    } else {
      # 如果没有有效数据，创建一个空向量
      group_data[[j]] <- numeric(0)
    }
  }
  
  # 按照desired_order重新排列数据
  original_order <- c("TL", "ML", "MO", "FineTAR", "zstd")
  order_index <- match(desired_order, original_order)
  
  # 重新排列数据
  reordered_data <- group_data[order_index[1:5]]  # 现在包括5个方法
  all_data_matrix[[i]] <- reordered_data
  
  cat("收集数据集", i, ":", dataset_name, "\n")
}

# ================================
# 自定义分组（数据集）顺序和显示名称
# ================================

# 首先打印所有数据集名称，方便您选择顺序
cat("所有数据集名称：", paste(all_group_labels, collapse = ", "), "\n")

# 自定义数据集显示顺序（用于数据匹配的原始名称）
custom_group_order <- c(
    "linux",
    "WEB",
    "automake",
    "coreutils", 
    "gcc",
    "react",
    "netty",
    "Cpython"
)

# 自定义显示名称（用于图表显示的美化名称）
display_names <- c(
    "Linux",           # linux -> Linux
    "Web",             # WEB保持不变
    "Automake",        # automake -> Automake
    "Coreutils",       # coreutils -> Coreutils
    "Gcc",             # gcc -> Gcc
    "React",           # react -> React
    "Netty",           # netty -> Netty
    "CPython"          # Cpython -> CPython
)

# 验证自定义顺序和显示名称长度是否匹配
if(length(custom_group_order) != length(display_names)) {
  stop("custom_group_order 和 display_names 的长度必须相同")
}

# 如果您不确定数据集名称，可以使用原始顺序
if(length(custom_group_order) != length(all_group_labels) || 
   !all(custom_group_order %in% all_group_labels)) {
  warning("自定义顺序与实际数据集不匹配，使用原始顺序")
  custom_group_order <- all_group_labels
  display_names <- all_group_labels  # 使用原始名称作为显示名称
}

# 按照自定义顺序重新排列数据
data_matrix <- list()
group_labels <- c()        # 用于显示的美化标签

for(i in 1:length(custom_group_order)) {
  target_dataset <- custom_group_order[i]
  original_index <- which(all_group_labels == target_dataset)
  
  if(length(original_index) > 0) {
    data_matrix[[i]] <- all_data_matrix[[original_index]]
    group_labels[i] <- display_names[i]  # 使用美化后的显示名称
    cat("重排序：位置", i, "-> 数据集", target_dataset, "-> 显示为", display_names[i], "\n")
  } else {
    warning(paste("找不到数据集:", target_dataset))
  }
}

# 设置柱子标签（包含5个方法）
bar_labels <- desired_order[1:5]
fill_colors_5 <- fill_colors[1:5]  # 使用全部5种颜色

# 创建分组柱状图
p <- create_grouped_barplot_with_ci(
  data_matrix = data_matrix,
  group_labels = group_labels,
  bar_labels = bar_labels,
  fill_colors = fill_colors_5,  # 使用5种颜色
  x_label = "",
  y_label = "Speed (MiB/s)",
  export_name = "restore_with_zstd.pdf",
  export_path = plot_dir,
  width = 24,  # 增加宽度以容纳更多柱子
  height = 8,
  bar_width = 0.6,  # 减小柱子宽度以适应更多柱子
  text_size = 40,
  x_axis_text_size = 40,
  y_axis_text_size = 40,
  x_label_size = 40,
  y_label_size = 50,
  legend_text_size = 40,
  show_legend = FALSE,
  show_data_labels = FALSE,
  use_arial = TRUE,
  y_max_multiplier = 1.15,
  dodge_width = 0.6,  # 减小dodge_width以适应更多柱子
  group_spacing = 0.8,  
  x_text_angle = 20,
  y_axis_margin = 0.02
)
cat("已生成分组柱状图（包含zstd）: grouped_restore_comparison_with_zstd.pdf\n")
print(p)