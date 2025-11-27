library(here)

#' @title 绘制DRR和ERR关系图 (简化版)
#' @description 创建一个单Y轴的折线图，用于展示DRR和ERR随阈值变化的关系。
#' @param data 数据框，必须包含以下列：`Threshold`, `OCR`, `ERR`。
#' @param dataset_name 数据集名称，用于图表标题和导出文件名，例如 "Linux"。
#' @param export_path 导出路径。
#' @param export_format 导出文件格式，例如 "pdf" 或 "png"。
#' @param colors 包含两个元素的颜色向量，分别对应 DRR, ERR。
#' @param shapes 包含两个元素的形状向量，分别对应 DRR, ERR。
#' @param line_size 线条粗细。
#' @param point_size 点的大小。
#' @param stroke_size 点边框的粗细。
#' @param axis_text_size 坐标轴文本大小。
#' @param title_size 标题大小。
#' @param legend_text_size 图例文本大小。
#' @param plot_width, plot_height 图形宽高。
#' @param x_label X轴标签，如果为NULL则隐藏。
#' @param primary_breaks Y轴的刻度，如果为NULL则自动生成。
#' @return 返回一个ggplot对象。
plot_tradeoff_dual_y_axis <- function(
  data,
  dataset_name,
  export_path = "./",
  export_format = "pdf",
  colors = c("OCR" = "#AD0626", "ERR" = "#2C3359"),  # 只保留两种颜色
  shapes = c("OCR" = 23, "ERR" = 24),  # 只保留两种形状
  line_size = 1.5,
  point_size = 9,
  stroke_size = 4.5,
  axis_text_size = 46,
  title_size = 48,
  legend_text_size = 44,
  plot_width = 12,
  plot_height = 6,
  x_label = NULL,
  primary_breaks = NULL     # Y轴刻度，NULL表示自动
) {
  # 1. 加载库和设置字体
  library(ggplot2)
  library(extrafont)
  library(showtext)
  library(scales)

  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))

  # 2. 数据校验
  required_cols <- c("Threshold", "OCR", "ERR")
  if (!all(required_cols %in% names(data))) {
    stop(paste("Data must contain the following columns:", paste(required_cols, collapse = ", ")))
  }

  # 3. 创建绘图
  p <- ggplot(data, aes(x = Threshold)) +
    # DRR and ERR lines and points
    geom_line(aes(y = OCR, color = "OCR"), linewidth = line_size) +
    geom_point(aes(y = OCR, color = "OCR", shape = "OCR"), size = point_size, stroke = stroke_size) +
    geom_line(aes(y = ERR, color = "ERR"), linewidth = line_size) +
    geom_point(aes(y = ERR, color = "ERR", shape = "ERR"), size = point_size, stroke = stroke_size) +
    
    # --- 坐标轴 ---
    scale_x_log10(
      breaks = data$Threshold,
      labels = function(x) {
        ifelse(x >= 1024*1024, paste0(x / (1024*1024), "M"), paste0(x / 1024, "K"))
      }
    )

  # 4. 构建 scale_y_continuous，根据是否指定了 primary_breaks
  if (is.null(primary_breaks)) {
    # Y轴自动生成刻度
    p <- p + scale_y_continuous(
      name = "Ratio",  # 修改Y轴标签
      expand = expansion(mult = c(0.05, 0.15))
    )
  } else {
    # Y轴手动指定刻度
    p <- p + scale_y_continuous(
      name = "Ratio",  # 修改Y轴标签
      expand = expansion(mult = c(0.05, 0.15)),
      breaks = primary_breaks
    )
  }

  p <- p +
    # --- 样式和标签 ---
    scale_color_manual(
      name = NULL, 
      values = colors, 
      labels = c("OCR" = "DRR", "ERR" = "ERR")  # 只保留两个标签
    ) +
    scale_shape_manual(
      name = NULL, 
      values = shapes,
      labels = c("OCR" = "DRR", "ERR" = "ERR")  # 只保留两个标签
    ) +
    labs(
      x = x_label
    ) +
    
    # --- 主题 ---
    theme_classic() +
    theme(
      text = element_text(family = "Arial", color = "black"),
      axis.text = element_text(size = axis_text_size, color = "black"),
      axis.text.x = element_text(angle = 30,  hjust = 0.8, vjust = 0.8),
      axis.title = element_text(size = title_size),
      axis.line = element_line(linewidth = 1),
      axis.ticks = element_line(linewidth = 1),
      axis.ticks.length = unit(0.2, "cm"),
      legend.position = "none"
    )

  # 5. 保存文件
  export_name <- paste0(dataset_name, "_B.", export_format)
  ggsave(file.path(export_path, export_name), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

# ===================================================================
# 数据设置
# ===================================================================

# 定义阈值 (X轴)，与您的数据列数对应
thresholds <- 2^(13:22)

# --- Linux 数据 ---
linux_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为DRR)
  OCR = c(70.2765, 74.0984, 78.6333, 82.4209, 84.1419, 84.5195, 84.5115, 84.1376, 84.1067, 84.0508),
  # 来自 "ERR" 表
  ERR = c(52.0646, 54.9674, 59.0644, 63.2713, 66.1475, 67.388, 67.9531, 68.1214, 68.583, 68.8177)
)

# --- WEB 数据 ---
web_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为DRR)
  OCR = c(215.914, 215.966, 216.144, 243.503, 246.768, 247.494, 247.713, 246.275, 246.275, 246.275),
  # 来自 "ERR" 表
  ERR = c(114.98, 115.272, 115.46, 129.554, 213.988, 219.744, 220.266, 219.344, 219.344, 219.344)
)

# --- Automake 数据 ---
automake_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(21.4821, 21.7177, 22.0653, 22.3008, 22.2431, 22.9165, 23.2725, 23.795, 23.795, 23.795),
  # 来自 "ERR" 表
  ERR = c(18.1946, 18.404, 18.7219, 18.9543, 19.0553, 19.7498, 20.1448, 20.6275, 20.6275, 20.6275)
)

# --- GCC 数据 ---
gcc_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(30.3584, 30.7101, 31.1284, 31.6794, 32.0315, 32.2956, 31.9986, 32.2289, 34.1496, 35.6264),
  # 来自 "ERR" 表
  ERR = c(24.7629, 25.1049, 25.5269, 26.0723, 26.5106, 26.9266, 26.9671, 27.3375, 28.9674, 30.1301)
)

# --- Coreutils 数据 ---
coreutils_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(15.5121, 16.0816, 17.0408, 17.3659, 17.8328, 17.9858, 18.2084, 18.0975, 18.0975, 18.3285),
  # 来自 "ERR" 表
  ERR = c(14.0255, 14.5413, 15.4233, 15.7331, 16.2046, 16.3637, 16.6512, 16.6048, 16.6048, 16.8708)
)

# --- CPython 数据 ---
cpython_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(57.7488, 58.9216, 60.8271, 63.7146, 65.0477, 65.132, 65.1658, 64.3747, 60.7219, 61.0874),
  # 来自 "ERR" 表
  ERR = c(45.5177, 46.6675, 48.7129, 52.0518, 54.8235, 56.3094, 57.0699, 56.9102, 54.7085, 55.3143)
)

# --- Netty 数据 ---
netty_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(53.616, 57.2619, 59.6742, 62.1817, 63.0132, 63.229, 63.2686, 62.9603, 62.9603, 62.9603),
  # 来自 "ERR" 表
  ERR = c(39.8212, 42.5141, 44.7885, 46.9705, 47.829, 48.2077, 48.2402, 48.0755, 48.0755, 48.0755)
)

# --- React 数据 ---
react_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(26.5783, 27.1474, 27.777, 28.394, 28.6832, 28.9481, 28.7434, 27.5845, 27.5996, 27.5996),
  # 来自 "ERR" 表
  ERR = c(23.5372, 24.038, 24.618, 25.2089, 25.5486, 25.9503, 26.012, 26.0869, 26.2755, 26.2755)
)

# ===================================================================
# 生成图表
# ===================================================================

# --- 为 "linux" 数据集生成图表 ---
tryCatch({
  print("Processing 'linux' dataset...")
  plot_tradeoff_dual_y_axis(
    data = linux_plot_data,
    dataset_name = "Linux",
    export_path = here("Paper", "impact"),
    export_format = "pdf"
  )
  print("'Linux' plot generated successfully.")
}, error = function(e) {
  print(paste("Failed to generate 'Linux' plot:", e$message))
})

# --- 为 "WEB" 数据集生成图表 ---
tryCatch({
  print("Processing 'WEB' dataset...")
  plot_tradeoff_dual_y_axis(
    data = web_plot_data,
    dataset_name = "WEB",
    export_path = here("Paper", "impact"),
    export_format = "pdf"
  )
  print("'WEB' plot generated successfully.")
}, error = function(e) {
  print(paste("Failed to generate 'WEB' plot:", e$message))
})

# --- 为 "automake" 数据集生成图表 ---
tryCatch({
  print("Processing 'automake' dataset...")
  plot_tradeoff_dual_y_axis(
    data = automake_plot_data,
    dataset_name = "Automake",
    export_path = here("Paper", "impact"),
    export_format = "pdf"
  )
  print("'Automake' plot generated successfully.")
}, error = function(e) {
  print(paste("Failed to generate 'Automake' plot:", e$message))
})

# --- 为 "gcc" 数据集生成图表 ---
tryCatch({
  print("Processing 'gcc' dataset...")
  plot_tradeoff_dual_y_axis(
    data = gcc_plot_data,
    dataset_name = "GCC",
    export_path = here("Paper", "impact"),
    export_format = "pdf"
  )
  print("'GCC' plot generated successfully.")
}, error = function(e) {
  print(paste("Failed to generate 'GCC' plot:", e$message))
})

# --- 为 "coreutils" 数据集生成图表 ---
tryCatch({
  print("Processing 'coreutils' dataset...")
  plot_tradeoff_dual_y_axis(
    data = coreutils_plot_data,
    dataset_name = "Coreutils",
    export_path = here("Paper", "impact"),
    export_format = "pdf"
  )
  print("'Coreutils' plot generated successfully.")
}, error = function(e) {
  print(paste("Failed to generate 'Coreutils' plot:", e$message))
})

# --- 为 "cpython" 数据集生成图表 ---
tryCatch({
  print("Processing 'cpython' dataset...")
  plot_tradeoff_dual_y_axis(
    data = cpython_plot_data,
    dataset_name = "CPython",
    export_path = here("Paper", "impact"),
    export_format = "pdf"
  )
  print("'CPython' plot generated successfully.")
}, error = function(e) {
  print(paste("Failed to generate 'CPython' plot:", e$message))
})

# --- 为 "netty" 数据集生成图表 ---
tryCatch({
  print("Processing 'netty' dataset...")
  plot_tradeoff_dual_y_axis(
    data = netty_plot_data,
    dataset_name = "Netty",
    export_path = here("Paper", "impact"),
    export_format = "pdf"
  )
  print("'Netty' plot generated successfully.")
}, error = function(e) {
  print(paste("Failed to generate 'Netty' plot:", e$message))
})

# --- 为 "react" 数据集生成图表 ---
tryCatch({
  print("Processing 'react' dataset...")
  plot_tradeoff_dual_y_axis(
    data = react_plot_data,
    dataset_name = "React",
    export_path = here("Paper", "impact"),
    export_format = "pdf"
  )
  print("'React' plot generated successfully.")
}, error = function(e) {
  print(paste("Failed to generate 'React' plot:", e$message))
})