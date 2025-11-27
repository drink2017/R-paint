#' @title 绘制权衡关系双Y轴图 (已更新)
#' @description 创建一个双Y轴的折线图，用于展示OCR/ERR与Recipe Size随阈值变化的权衡关系。
#' @param data 数据框，必须包含以下列：`Threshold`, `OCR`, `ERR`, `RecipeSize`。
#' @param dataset_name 数据集名称，用于图表标题和导出文件名，例如 "Linux"。
#' @param export_path 导出路径。
#' @param export_format 导出文件格式，例如 "pdf" 或 "png"。
#' @param colors 包含三个元素的颜色向量，分别对应 OCR, ERR, Recipe Size。
#' @param shapes 包含三个元素的形状向量，分别对应 OCR, ERR, Recipe Size。
#' @param line_size 线条粗细。
#' @param point_size 点的大小。
#' @param stroke_size 点边框的粗细。
#' @param axis_text_size 坐标轴文本大小。
#' @param title_size 标题大小。
#' @param legend_text_size 图例文本大小。
#' @param plot_width, plot_height 图形宽高。
#' @param x_label X轴标签，如果为NULL则隐藏。
#' @param primary_breaks 左Y轴的刻度，如果为NULL则自动生成。
#' @param secondary_breaks 右Y轴的刻度，如果为NULL则自动生成。
#' @return 返回一个ggplot对象。
plot_tradeoff_dual_y_axis <- function(
  data,
  dataset_name,
  export_path = "./",
  export_format = "pdf",
  colors = c("OCR" = "#AD0626", "ERR" = "#2C3359", "RecipeSize" = "#1B9E9E"),
  shapes = c("OCR" = 23, "ERR" = 24, "RecipeSize" = 22),
  line_size = 1.5,
  point_size = 9,
  stroke_size = 4.5,
  axis_text_size = 42,
  title_size = 48,
  legend_text_size = 32,
  plot_width = 12,
  plot_height = 6,
  x_label = NULL,
  primary_breaks = NULL,     # 左Y轴刻度，NULL表示自动
  secondary_breaks = NULL    # 右Y轴刻度，NULL表示自动
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
  required_cols <- c("Threshold", "OCR", "ERR", "RecipeSize")
  if (!all(required_cols %in% names(data))) {
    stop(paste("Data must contain the following columns:", paste(required_cols, collapse = ", ")))
  }

  # 3. 计算缩放因子
  primary_range <- range(c(data$OCR, data$ERR), na.rm = TRUE)
  secondary_range <- range(data$RecipeSize, na.rm = TRUE)
  
  if (diff(secondary_range) == 0) {
    scaling_factor <- 1
  } else {
    scaling_factor <- diff(primary_range) / diff(secondary_range)
  }
  
  format_bytes_manual <- function(b) {
    sapply(b, function(x) {
      if (is.na(x)) return(NA)
      if (x >= 1e9) {
        label <- scales::number(x / 1e9, accuracy = 0.01, trim = TRUE)
        paste0(label, "G")
      } else if (x >= 1e6) {
        # 对于 MB 范围，智能调整精度
        mb_value <- x / 1e6
        if (mb_value >= 100) {
          # >= 100 MB: 精度为 1
          label <- scales::number(mb_value, accuracy = 1, trim = TRUE)
        } else if (mb_value >= 10) {
          # 10-100 MB: 先用 0.01 精度计算，如果 0.01 位没有数字则使用 0.1
          label_0_01 <- scales::number(mb_value, accuracy = 0.01, trim = TRUE)
          # 检查第二位小数是否为 0
          rounded_0_1 <- round(mb_value, 1)
          if (abs(mb_value - rounded_0_1) < 0.001) {
            # 0.01 位是 0，使用 0.1 精度
            label <- scales::number(mb_value, accuracy = 0.1, trim = TRUE)
          } else {
            # 0.01 位有数字，使用 0.01 精度
            label <- label_0_01
          }
        } else {
          # < 10 MB: 先用 0.01 精度，如果 0.01 位没有数字则使用 0.1
          label_0_01 <- scales::number(mb_value, accuracy = 0.01, trim = TRUE)
          rounded_0_1 <- round(mb_value, 1)
          if (abs(mb_value - rounded_0_1) < 0.001) {
            # 0.01 位是 0，使用 0.1 精度
            label <- scales::number(mb_value, accuracy = 0.1, trim = TRUE)
          } else {
            # 0.01 位有数字，使用 0.01 精度
            label <- label_0_01
          }
        }
        paste0(label, "M")
      } else if (x >= 1e3) {
        label <- scales::number(x / 1e3, accuracy = 1, trim = TRUE)
        paste0(label, "K")
      } else {
        paste0(x, "B")
      }
    })
  }

  # 4. 创建绘图
  p <- ggplot(data, aes(x = Threshold)) +
    # OCR and ERR lines and points
    geom_line(aes(y = OCR, color = "OCR"), linewidth = line_size) +
    geom_point(aes(y = OCR, color = "OCR", shape = "OCR"), size = point_size, stroke = stroke_size) +
    geom_line(aes(y = ERR, color = "ERR"), linewidth = line_size) +
    geom_point(aes(y = ERR, color = "ERR", shape = "ERR"), size = point_size, stroke = stroke_size) +
    
    # Recipe Size line and points (on secondary axis)
    geom_line(aes(y = (RecipeSize - secondary_range[1]) * scaling_factor + primary_range[1], color = "RecipeSize"), linewidth = line_size) +
    geom_point(aes(y = (RecipeSize - secondary_range[1]) * scaling_factor + primary_range[1], color = "RecipeSize", shape = "RecipeSize"), size = point_size, stroke = stroke_size) +
    
    # --- 坐标轴 ---
    scale_x_log10(
      breaks = data$Threshold,
      labels = function(x) {
        ifelse(x >= 1024*1024, paste0(x / (1024*1024), "M"), paste0(x / 1024, "K"))
      }
    )

  # 5. 构建 sec_axis，根据是否指定了 secondary_breaks
  if (is.null(secondary_breaks)) {
    # 右Y轴自动生成刻度
    sec_axis_spec <- sec_axis(
      transform = ~ (.- primary_range[1]) / scaling_factor + secondary_range[1],
      name = "Recipe Size",
      labels = format_bytes_manual
    )
  } else {
    # 右Y轴手动指定刻度
    sec_axis_spec <- sec_axis(
      transform = ~ (.- primary_range[1]) / scaling_factor + secondary_range[1],
      name = "Recipe Size",
      labels = format_bytes_manual,
      breaks = secondary_breaks
    )
  }

  # 6. 构建 scale_y_continuous，根据是否指定了 primary_breaks
  if (is.null(primary_breaks)) {
    # 左Y轴自动生成刻度
    p <- p + scale_y_continuous(
      name = "DRR & ERR",
      expand = expansion(mult = c(0.05, 0.15)),
      sec.axis = sec_axis_spec
    )
  } else {
    # 左Y轴手动指定刻度
    p <- p + scale_y_continuous(
      name = "DRR & ERR",
      expand = expansion(mult = c(0.05, 0.15)),
      breaks = primary_breaks,
      sec.axis = sec_axis_spec
    )
  }

  p <- p +
    # --- 样式和标签 ---
    scale_color_manual(
      name = NULL, 
      values = colors, 
      labels = c("OCR" = "DRR", "ERR" = "ERR", "RecipeSize" = "Recipe Size")
    ) +
    scale_shape_manual(
      name = NULL, 
      values = shapes,
      labels = c("OCR" = "DRR", "ERR" = "ERR", "RecipeSize" = "Recipe Size")
    ) +
    labs(
      x = x_label
    ) +
    
    # --- 主题 ---
    theme_classic() +
    theme(
      text = element_text(family = "Arial", color = "black"),
      axis.text = element_text(size = axis_text_size, color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = title_size),
      axis.line = element_line(linewidth = 1),
      axis.ticks = element_line(linewidth = 1),
      axis.ticks.length = unit(0.2, "cm"),
      legend.position = "none"
    )

  # 7. 保存文件
  export_name <- paste0(dataset_name, "_tradeoff_plot.", export_format)
  ggsave(file.path(export_path, export_name), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}
# # ===================================================================
# # 1. 环境设置
# # ===================================================================
# # 检查并安装必要的包
# packages <- c("here")
# for (pkg in packages) {
#   if (!require(pkg, character.only = TRUE)) {
#     install.packages(pkg)
#     library(pkg, character.only = TRUE)
#   }
# }

# # 加载绘图函数
# # 使用 here() 确保能从项目根目录正确找到文件
# source(here("MyR", "Line_Point.R"))


# ===================================================================
# 2. 手动输入数据
# ===================================================================

# 定义阈值 (X轴)，与您的数据列数对应
thresholds <- 2^(13:22)
mib_to_bytes <- 1024 * 1024 # <--- 添加这行定义
# --- Linux 数据 ---
# 请将 impact-B-recipe.xlsx, impact-B-drr.xlsx, impact-B-ERR.xlsx 中
# 'linux' 对应行的数据分别粘贴到下面的 c() 中。
# 注意：数据点之间用英文逗号 , 分隔。
linux_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "Recipe Overhead(MiB)" 表, 单位已转换为 Bytes
  RecipeSize = c(1010.11, 953.209, 855.063, 745.213, 656.112, 610.406, 585.14, 567.088, 546.151, 534.455) * mib_to_bytes,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为DRR)
  OCR = c(70.2765, 74.0984, 78.6333, 82.4209, 84.1419, 84.5195, 84.5115, 84.1376, 84.1067, 84.0508),
  # 来自 "ERR" 表
  ERR = c(52.0646, 54.9674, 59.0644, 63.2713, 66.1475, 67.388, 67.9531, 68.1214, 68.583, 68.8177)
)

# --- WEB 数据 ---
web_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "Recipe Overhead(MiB)" 表, 单位已转换为 Bytes
  RecipeSize = c(1153.69, 1147.75, 1144.82, 1024.97, 176.156, 144.79, 142.74, 141.467, 141.467, 141.467) * mib_to_bytes,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为DRR)
  OCR = c(215.914, 215.966, 216.144, 243.503, 246.768, 247.494, 247.713, 246.275, 246.275, 246.275),
  # 来自 "ERR" 表
  ERR = c(114.98, 115.272, 115.46, 129.554, 213.988, 219.744, 220.266, 219.344, 219.344, 219.344)
)


# ===================================================================
# 3. 生成图表
# ===================================================================

# --- 为 "linux" 数据集生成图表 ---
tryCatch({
  print("Processing 'linux' dataset...")
  plot_tradeoff_dual_y_axis(
    data = linux_plot_data,
    dataset_name = "Linux",
    export_path = here("Paper", "impact"), # 将图表保存在当前目录
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
    export_path = here("Paper", "impact"), # 将图表保存在当前目录
    export_format = "pdf"
  )
  print("'WEB' plot generated successfully.")
}, error = function(e) {
  print(paste("Failed to generate 'WEB' plot:", e$message))
})


# --- Automake 数据 ---
automake_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "Recipe Overhead(MiB)" 表, 单位已转换为 Bytes
  RecipeSize = c(4.0434, 3.9856, 3.89075, 3.806, 3.6156, 3.36362, 3.20712, 3.10233, 3.10233, 3.10233) * mib_to_bytes,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(21.4821, 21.7177, 22.0653, 22.3008, 22.2431, 22.9165, 23.2725, 23.795, 23.795, 23.795),
  # 来自 "ERR" 表
  ERR = c(18.1946, 18.404, 18.7219, 18.9543, 19.0553, 19.7498, 20.1448, 20.6275, 20.6275, 20.6275)
)

# --- GCC 数据 ---
gcc_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "Recipe Overhead(MiB)" 表, 单位已转换为 Bytes
  RecipeSize = c(313.342, 306.063, 296.759, 285.787, 273.7, 259.914, 245.467, 233.714, 220.534, 215.555) * mib_to_bytes,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(30.3584, 30.7101, 31.1284, 31.6794, 32.0315, 32.2956, 31.9986, 32.2289, 34.1496, 35.6264),
  # 来自 "ERR" 表
  ERR = c(24.7629, 25.1049, 25.5269, 26.0723, 26.5106, 26.9266, 26.9671, 27.3375, 28.9674, 30.1301)
)



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


# --- Coreutils 数据 ---
coreutils_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "Recipe Overhead(MiB)" 表, 单位已转换为 Bytes
  RecipeSize = c(1.5701, 1.51346, 1.41415, 1.3732, 1.29459, 1.26642, 1.18011, 1.14136, 1.14136, 1.08316) * mib_to_bytes,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(15.5121, 16.0816, 17.0408, 17.3659, 17.8328, 17.9858, 18.2084, 18.0975, 18.0975, 18.3285),
  # 来自 "ERR" 表
  ERR = c(14.0255, 14.5413, 15.4233, 15.7331, 16.2046, 16.3637, 16.6512, 16.6048, 16.6048, 16.8708)
)

# --- CPython 数据 ---
cpython_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "Recipe Overhead(MiB)" 表, 单位已转换为 Bytes
  RecipeSize = c(45.8129, 43.8772, 40.2529, 34.6237, 28.2277, 23.6845, 21.4331, 20.0605, 17.8222, 16.8217) * mib_to_bytes,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(57.7488, 58.9216, 60.8271, 63.7146, 65.0477, 65.132, 65.1658, 64.3747, 60.7219, 61.0874),
  # 来自 "ERR" 表
  ERR = c(45.5177, 46.6675, 48.7129, 52.0518, 54.8235, 56.3094, 57.0699, 56.9102, 54.7085, 55.3143)
)

# --- Netty 数据 ---
netty_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "Recipe Overhead(MiB)" 表, 单位已转换为 Bytes
  RecipeSize = c(14.0028, 13.1292, 12.0706, 11.2872, 10.9189, 10.6804, 10.6715, 10.6577, 10.6577, 10.6577) * mib_to_bytes,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(53.616, 57.2619, 59.6742, 62.1817, 63.0132, 63.229, 63.2686, 62.9603, 62.9603, 62.9603),
  # 来自 "ERR" 表
  ERR = c(39.8212, 42.5141, 44.7885, 46.9705, 47.829, 48.2077, 48.2402, 48.0755, 48.0755, 48.0755)
)

# --- React 数据 ---
react_plot_data <- data.frame(
  Threshold = thresholds,
  # 来自 "Recipe Overhead(MiB)" 表, 单位已转换为 Bytes
  RecipeSize = c(13.4846, 13.2174, 12.8146, 12.3434, 11.8652, 11.0696, 10.1333, 5.77325, 5.06491, 5.06491) * mib_to_bytes,
  # 来自 "drr" 表, 作为 OCR (将在图中显示为 DRR)
  OCR = c(26.5783, 27.1474, 27.777, 28.394, 28.6832, 28.9481, 28.7434, 27.5845, 27.5996, 27.5996),
  # 来自 "ERR" 表
  ERR = c(23.5372, 24.038, 24.618, 25.2089, 25.5486, 25.9503, 26.012, 26.0869, 26.2755, 26.2755)
)


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





# DRR

# | Dataset   | B_8192  | B_16384 | B_32768 | B_65536 | B_131072 | B_262144 | B_524288 | B_1048576 | B_2097152 | B_4194304 |
# | --------- | ------- | ------- | ------- | ------- | -------- | -------- | -------- | --------- | --------- | --------- |
# | automake  | 21.4821 | 21.7177 | 22.0653 | 22.3008 | 22.2431  | 22.9165  | 23.2725  | 23.795    | 23.795    | 23.795    |
# | coreutils | 15.5121 | 16.0816 | 17.0408 | 17.3659 | 17.8328  | 17.9858  | 18.2084  | 18.0975   | 18.0975   | 18.3285   |
# | Cpython   | 57.7488 | 58.9216 | 60.8271 | 63.7146 | 65.0477  | 65.132   | 65.1658  | 64.3747   | 60.7219   | 61.0874   |
# | linux     | 70.2765 | 74.0984 | 78.6333 | 82.4209 | 84.1419  | 84.5195  | 84.5115  | 84.1376   | 84.1067   | 84.0508   |
# | netty     | 53.616  | 57.2619 | 59.6742 | 62.1817 | 63.0132  | 63.229   | 63.2686  | 62.9603   | 62.9603   | 62.9603   |
# | react     | 26.5783 | 27.1474 | 27.777  | 28.394  | 28.6832  | 28.9481  | 28.7434  | 27.5845   | 27.5996   | 27.5996   |
# | gcc       | 30.3584 | 30.7101 | 31.1284 | 31.6794 | 32.0315  | 32.2956  | 31.9986  | 32.2289   | 34.1496   | 35.6264   |
# | WEB       | 215.914 | 215.966 | 216.144 | 243.503 | 246.768  | 247.494  | 247.713  | 246.275   | 246.275   | 246.275   |
# ERR

# | Dataset   | B_8192  | B_16384 | B_32768 | B_65536 | B_131072 | B_262144 | B_524288 | B_1048576 | B_2097152 | B_4194304 |
# | --------- | ------- | ------- | ------- | ------- | -------- | -------- | -------- | --------- | --------- | --------- |
# | automake  | 18.1946 | 18.404  | 18.7219 | 18.9543 | 19.0553  | 19.7498  | 20.1448  | 20.6275   | 20.6275   | 20.6275   |
# | coreutils | 14.0255 | 14.5413 | 15.4233 | 15.7331 | 16.2046  | 16.3637  | 16.6512  | 16.6048   | 16.6048   | 16.8708   |
# | Cpython   | 45.5177 | 46.6675 | 48.7129 | 52.0518 | 54.8235  | 56.3094  | 57.0699  | 56.9102   | 54.7085   | 55.3143   |
# | linux     | 52.0646 | 54.9674 | 59.0644 | 63.2713 | 66.1475  | 67.388   | 67.9531  | 68.1214   | 68.583    | 68.8177   |
# | netty     | 39.8212 | 42.5141 | 44.7885 | 46.9705 | 47.829   | 48.2077  | 48.2402  | 48.0755   | 48.0755   | 48.0755   |
# | react     | 23.5372 | 24.038  | 24.618  | 25.2089 | 25.5486  | 25.9503  | 26.012   | 26.0869   | 26.2755   | 26.2755   |
# | gcc       | 24.7629 | 25.1049 | 25.5269 | 26.0723 | 26.5106  | 26.9266  | 26.9671  | 27.3375   | 28.9674   | 30.1301   |
# | WEB       | 114.98  | 115.272 | 115.46  | 129.554 | 213.988  | 219.744  | 220.266  | 219.344   | 219.344   | 219.344   |
# recipe

# | Dataset   | B_8192  | B_16384 | B_32768 | B_65536 | B_131072 | B_262144 | B_524288 | B_1048576 | B_2097152 | B_4194304 |
# | --------- | ------- | ------- | ------- | ------- | -------- | -------- | -------- | --------- | --------- | --------- |
# | automake  | 4.0434  | 3.9856  | 3.89075 | 3.806   | 3.6156   | 3.36362  | 3.20712  | 3.10233   | 3.10233   | 3.10233   |
# | coreutils | 1.5701  | 1.51346 | 1.41415 | 1.3732  | 1.29459  | 1.26642  | 1.18011  | 1.14136   | 1.14136   | 1.08316   |
# | Cpython   | 45.8129 | 43.8772 | 40.2529 | 34.6237 | 28.2277  | 23.6845  | 21.4331  | 20.0605   | 17.8222   | 16.8217   |
# | linux     | 1010.11 | 953.209 | 855.063 | 745.213 | 656.112  | 610.406  | 585.14   | 567.088   | 546.151   | 534.455   |
# | netty     | 14.0028 | 13.1292 | 12.0706 | 11.2872 | 10.9189  | 10.6804  | 10.6715  | 10.6577   | 10.6577   | 10.6577   |
# | react     | 13.4846 | 13.2174 | 12.8146 | 12.3434 | 11.8652  | 11.0696  | 10.1333  | 5.77325   | 5.06491   | 5.06491   |
# | gcc       | 313.342 | 306.063 | 296.759 | 285.787 | 273.7    | 259.914  | 245.467  | 233.714   | 220.534   | 215.555   |
# | WEB       | 1153.69 | 1147.75 | 1144.82 | 1024.97 | 176.156  | 144.79   | 142.74   | 141.467   | 141.467   | 141.467   |
