library(here)
library(readxl)
source(here("MyR", "Bar.R"))

# 读取Excel数据
excel_path <- here("Paper/开源mtar_longnameFineTAR/Restore.xlsx")
compression_data <- read_excel(excel_path)

# 获取列名
column_names <- colnames(compression_data)
num_columns <- length(column_names)

# 参数设置
GROUP_SIZE <- 4  # 每组的方法数量（TL, ML, MO, FineTAR）
INTERVAL <- 8    # 数据集数量

# 确保输出文件夹存在
plot_dir <- here("Paper/开源mtar_longnameFineTAR/RestoreBar")
if(!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# 设定方法的顺序和颜色
desired_order <- c("TL", "ML", "MO", "FineTAR")
fill_colors <- c("#F2BE5C", "#B79AD1", "#75B8BF", "#FF5809")

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
  original_order <- c("TL", "ML", "MO", "FineTAR")
  order_index <- match(desired_order, original_order)
  
  # 重新排列数据
  reordered_data <- group_data[order_index[1:4]]
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

# 设置柱子标签
bar_labels <- desired_order[1:4]
fill_colors_4 <- fill_colors[1:4]

# 创建分组柱状图
p <- create_grouped_barplot_with_ci(
  data_matrix = data_matrix,
  group_labels = group_labels,
  bar_labels = bar_labels,
  fill_colors = fill_colors_4,
  x_label = "",
  y_label = "Speed (MiB/s)",
  export_name = "grouped_performance_comparison.pdf",
  export_path = plot_dir,
  width = 20,
  height = 8,
  bar_width = 0.7,
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
  dodge_width = 0.7,
  group_spacing = 0.8,  
  x_text_angle = 20,
  y_axis_margin = 0.02  # 添加这个参数来减少y轴边距，值越小y轴越靠近柱子
)
cat("已生成分组柱状图（自定义分组顺序）: grouped_performance_comparison.pdf\n")
print(p)

