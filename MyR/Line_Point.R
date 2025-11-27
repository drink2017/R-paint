plot_line_with_points <- function(
  data,                       # 数据框，第一列为x轴数据，其余列为不同线条的y值
  column_names = NULL,        # 列名(用于图例)，如果为NULL则使用数据框的列名
  colors = NULL,              # 颜色列表，如果为NULL则自动生成
  shapes = NULL,              # 形状列表，如果为NULL则自动生成 
  export_path = "./",         # 导出路径
  export_name = "line_plot.pdf", # 导出文件名
  x_lim = NULL,               # x轴范围，如果为NULL则自动确定
  y_lim = NULL,               # y轴范围，如果为NULL则自动确定
  line_size = 3,              # 线条粗细
  point_size = 9,             # 点大小
  stroke_size = 4.5,          # 点边框粗细
  axis_text_size = 42,        # 坐标轴文本大小
  x_title_size = 48,          # x轴标题大小
  y_title_size = 41,          # y轴标题大小
  x_label = "X",              # x轴标签
  y_label = "Y",              # y轴标签
  x_breaks = NULL,            # x轴刻度，如果为NULL则自动生成
  y_breaks = NULL,            # y轴刻度，如果为NULL则自动生成
  plot_width = 10,            # 导出图形宽度
  plot_height = 5,            # 导出图形高度
  show_legend = FALSE,        # 默认不显示图例
  legend_position = "right",  # 图例位置(如果显示)
  legend_text_size = 32       # 图例文本大小(如果显示)
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 检查数据输入
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # 确定列数(第一列为x轴，其余为y轴数据)
  n_series <- ncol(data) - 1
  if (n_series < 1) {
    stop("Data frame must have at least two columns: x values and at least one y series")
  }
  
  # 设置列名（用于图例）
  if (is.null(column_names)) {
    column_names <- colnames(data)[-1]  # 排除第一列(x轴)的名称
  } else if (length(column_names) != n_series) {
    warning("column_names length does not match number of data series. Using data frame column names.")
    column_names <- colnames(data)[-1]
  }
  
  # 设置颜色
  if (is.null(colors)) {
    # 默认颜色方案
    default_colors <- c("#AD0626", "#2C3359", "#B79AD1", "#75B8BF", "#F2BE5C")
    # 如果系列多于默认颜色，则循环使用
    colors <- rep(default_colors, length.out = n_series)
  } else if (length(colors) < n_series) {
    warning("Not enough colors provided. Recycling colors.")
    colors <- rep(colors, length.out = n_series)
  }
  
  # 设置形状
  if (is.null(shapes)) {
    # 默认形状列表：三角形、菱形、正方形、圆形等
    default_shapes <- c(23, 24, 22, 21, 25)
    shapes <- rep(default_shapes, length.out = n_series)
  } else if (length(shapes) < n_series) {
    warning("Not enough shapes provided. Recycling shapes.")
    shapes <- rep(shapes, length.out = n_series)
  }
  
  # 设置x轴范围
  if (is.null(x_lim)) {
    x_range <- range(data[, 1])
    x_margin <- (x_range[2] - x_range[1]) * 0.05
    x_lim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  }
  
  # 设置y轴范围
  if (is.null(y_lim)) {
    y_values <- unlist(data[, -1])  # 提取所有y值
    y_range <- range(y_values)
    y_margin <- (y_range[2] - y_range[1]) * 0.05
    y_lim <- c(max(0, y_range[1] - y_margin), y_range[2] + y_margin)  # 确保下限不小于0
  }
  
  # 设置x轴刻度
  if (is.null(x_breaks)) {
    x_breaks <- pretty(data[, 1], n = 5)
  }
  
  # 设置y轴刻度
  if (is.null(y_breaks)) {
    y_breaks <- pretty(unlist(data[, -1]), n = 5)
  }
  
  # 将数据转换为长格式，用于ggplot2
  plot_data <- reshape2::melt(data, id.vars = names(data)[1], 
                             measure.vars = names(data)[-1],
                             variable.name = "series", value.name = "value")
  
  # 如果提供了自定义列名，更新系列名称
  if (!is.null(column_names)) {
    levels(plot_data$series) <- column_names
  }
  
  # 创建基本绘图
  p <- ggplot(plot_data, aes_string(x = names(data)[1], y = "value", 
                                   group = "series", color = "series", shape = "series")) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    geom_line(size = line_size) +
    geom_point(size = point_size, stroke = stroke_size) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    theme_classic() +
    theme(axis.text = element_text(size = axis_text_size, color = "black"),
          text = element_text(family = "Arial"),
          axis.title.x = element_text(size = x_title_size),
          axis.title.y = element_text(size = y_title_size, hjust = 1.1)) +
    labs(y = y_label, x = x_label) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(breaks = y_breaks, labels = y_breaks)
  
  # 处理图例
  if (show_legend) {
    p <- p + 
      guides(color = guide_legend(title = NULL),
             shape = guide_legend(title = NULL)) +
      theme(legend.position = legend_position,
            legend.text = element_text(size = legend_text_size))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

plot_line_with_selected_points <- function(
  data,                       # 数据框，第一列为x轴数据，其余列为不同线条的y值
  column_names = NULL,        # 列名(用于图例)，如果为NULL则使用数据框的列名
  colors = NULL,              # 颜色列表，如果为NULL则自动生成
  shapes = NULL,              # 形状列表，如果为NULL则自动生成 
  num_points = 10,            # 每条线上均匀显示的点数
  export_path = "./",         # 导出路径
  export_name = "line_plot.pdf", # 导出文件名
  x_lim = NULL,               # x轴范围，如果为NULL则自动确定
  y_lim = NULL,               # y轴范围，如果为NULL则自动确定
  line_size = 3,              # 线条粗细
  point_size = 9,             # 点大小
  stroke_size = 4.5,          # 点边框粗细
  axis_text_size = 42,        # 坐标轴文本大小
  x_title_size = 48,          # x轴标题大小
  y_title_size = 41,          # y轴标题大小
  x_label = "X",              # x轴标签
  y_label = "Y",              # y轴标签
  x_breaks = NULL,            # x轴刻度，如果为NULL则自动生成
  y_breaks = NULL,            # y轴刻度，如果为NULL则自动生成
  plot_width = 10,            # 导出图形宽度
  plot_height = 5,            # 导出图形高度
  show_legend = FALSE,        # 默认不显示图例
  legend_position = "right",  # 图例位置(如果显示)
  legend_text_size = 32,      # 图例文本大小(如果显示)
  include_endpoints = TRUE    # 是否始终包含端点
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  library(reshape2)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 检查数据输入
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # 确定列数(第一列为x轴，其余为y轴数据)
  n_series <- ncol(data) - 1
  if (n_series < 1) {
    stop("Data frame must have at least two columns: x values and at least one y series")
  }
  
  # 设置列名（用于图例）
  if (is.null(column_names)) {
    column_names <- colnames(data)[-1]  # 排除第一列(x轴)的名称
  } else if (length(column_names) != n_series) {
    warning("column_names length does not match number of data series. Using data frame column names.")
    column_names <- colnames(data)[-1]
  }
  
  # 设置颜色
  if (is.null(colors)) {
    # 默认颜色方案
    default_colors <- c("#AD0626", "#2C3359", "#B79AD1", "#75B8BF", "#F2BE5C")
    # 如果系列多于默认颜色，则循环使用
    colors <- rep(default_colors, length.out = n_series)
  } else if (length(colors) < n_series) {
    warning("Not enough colors provided. Recycling colors.")
    colors <- rep(colors, length.out = n_series)
  }
  
  # 设置形状
  if (is.null(shapes)) {
    # 默认形状列表：三角形、菱形、正方形、圆形等
    default_shapes <- c(23, 24, 22, 21, 25)
    shapes <- rep(default_shapes, length.out = n_series)
  } else if (length(shapes) < n_series) {
    warning("Not enough shapes provided. Recycling shapes.")
    shapes <- rep(shapes, length.out = n_series)
  }
  
  # 设置x轴范围
  if (is.null(x_lim)) {
    x_range <- range(data[, 1])
    x_margin <- (x_range[2] - x_range[1]) * 0.05
    x_lim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  }
  
  # 设置y轴范围
  if (is.null(y_lim)) {
    y_values <- unlist(data[, -1])  # 提取所有y值
    y_range <- range(y_values)
    y_margin <- (y_range[2] - y_range[1]) * 0.05
    y_lim <- c(max(0, y_range[1] - y_margin), y_range[2] + y_margin)  # 确保下限不小于0
  }
  
  # 设置x轴刻度
  if (is.null(x_breaks)) {
    x_breaks <- pretty(data[, 1], n = 5)
  }
  
  # 设置y轴刻度
  if (is.null(y_breaks)) {
    y_breaks <- pretty(unlist(data[, -1]), n = 5)
  }
  
  # 将数据转换为长格式，用于ggplot2
  plot_data <- reshape2::melt(data, id.vars = names(data)[1], 
                             measure.vars = names(data)[-1],
                             variable.name = "series", value.name = "value")
  
  # 如果提供了自定义列名，更新系列名称
  if (!is.null(column_names)) {
    levels(plot_data$series) <- column_names
  }
  
  # 创建均匀选择点的函数
  select_points <- function(df, n) {
    # 分割数据按系列
    series_data <- split(df, df$series)
    result <- data.frame()
    
    for (s in names(series_data)) {
      s_data <- series_data[[s]]
      total_points <- nrow(s_data)
      
      # 处理点数不足的情况
      if (total_points <= n) {
        selected <- s_data
      } else {
        # 根据包含端点设置的策略选择点
        if (include_endpoints) {
          if (n >= 2) {
            # 始终包括第一个和最后一个点，其余均匀分布
            interval <- (total_points - 1) / (n - 1)
            indices <- round(seq(1, total_points, by = interval))
            # 确保不超出范围
            indices <- indices[indices <= total_points]
            selected <- s_data[indices, ]
          } else if (n == 1) {
            # 如果只需一个点，取中间点
            selected <- s_data[ceiling(total_points/2), ]
          } else {
            # 如果n为0，不选择任何点
            selected <- s_data[0, ]
          }
        } else {
          # 均匀选择n个点
          interval <- total_points / n
          indices <- round(seq(1, total_points, by = interval))
          # 确保不超出范围
          indices <- indices[indices <= total_points]
          selected <- s_data[indices, ]
        }
      }
      
      result <- rbind(result, selected)
    }
    
    return(result)
  }
  
  # 选择要显示点的数据行
  points_data <- select_points(plot_data, num_points)
  
  # 创建基本绘图
  p <- ggplot() +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    # 绘制所有线
    geom_line(data = plot_data, 
              aes_string(x = names(data)[1], y = "value", 
                        group = "series", color = "series"),
              size = line_size) +
    # 只在选定的点上绘制符号
    geom_point(data = points_data, 
              aes_string(x = names(data)[1], y = "value", 
                        group = "series", color = "series", shape = "series"),
              size = point_size, stroke = stroke_size) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    theme_classic() +
    theme(axis.text = element_text(size = axis_text_size, color = "black"),
          text = element_text(family = "Arial"),
          axis.title.x = element_text(size = x_title_size),
          axis.title.y = element_text(size = y_title_size, hjust = 1.1)) +
    labs(y = y_label, x = x_label) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(breaks = y_breaks, labels = y_breaks)
  
  # 处理图例
  if (show_legend) {
    p <- p + 
      guides(color = guide_legend(title = NULL),
             shape = guide_legend(title = NULL)) +
      theme(legend.position = legend_position,
            legend.text = element_text(size = legend_text_size))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

plot_with_sampled_points <- function(
  data,                       # 数据框，第一列为x轴数据，其余列为不同线条的y值
  column_names = NULL,        # 列名(用于图例)，如果为NULL则使用数据框的列名
  colors = NULL,              # 颜色列表，如果为NULL则自动生成
  shapes = NULL,              # 形状列表，如果为NULL则自动生成 
  sample_size = 10,           # 每条线均匀抽样的点数
  export_path = "./",         # 导出路径
  export_name = "line_plot.pdf", # 导出文件名
  x_lim = NULL,               # x轴范围，如果为NULL则自动确定
  y_lim = NULL,               # y轴范围，如果为NULL则自动确定
  line_size = 3,              # 线条粗细
  point_size = 9,             # 点大小
  stroke_size = 4.5,          # 点边框粗细
  axis_text_size = 42,        # 坐标轴文本大小
  x_title_size = 48,          # x轴标题大小
  y_title_size = 41,          # y轴标题大小
  x_label = "X",              # x轴标签
  y_label = "Y",              # y轴标签
  x_breaks = NULL,            # x轴刻度，如果为NULL则自动生成
  y_breaks = NULL,            # y轴刻度，如果为NULL则自动生成
  plot_width = 10,            # 导出图形宽度
  plot_height = 5,            # 导出图形高度
  show_legend = FALSE,        # 默认不显示图例
  legend_position = "right",  # 图例位置(如果显示)
  legend_text_size = 32,      # 图例文本大小(如果显示)
  include_endpoints = TRUE    # 是否始终包含端点
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  library(reshape2)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 检查数据输入
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # 确定列数(第一列为x轴，其余为y轴数据)
  n_series <- ncol(data) - 1
  if (n_series < 1) {
    stop("Data frame must have at least two columns: x values and at least one y series")
  }
  
  # 设置列名（用于图例）
  if (is.null(column_names)) {
    column_names <- colnames(data)[-1]  # 排除第一列(x轴)的名称
  } else if (length(column_names) != n_series) {
    warning("column_names length does not match number of data series. Using data frame column names.")
    column_names <- colnames(data)[-1]
  }
  
  # 设置颜色
  if (is.null(colors)) {
    # 默认颜色方案
    default_colors <- c("#AD0626", "#2C3359", "#B79AD1", "#75B8BF", "#F2BE5C")
    # 如果系列多于默认颜色，则循环使用
    colors <- rep(default_colors, length.out = n_series)
  } else if (length(colors) < n_series) {
    warning("Not enough colors provided. Recycling colors.")
    colors <- rep(colors, length.out = n_series)
  }
  
  # 设置形状
  if (is.null(shapes)) {
    # 默认形状列表：三角形、菱形、正方形、圆形等
    default_shapes <- c(23, 24, 22, 21, 25)
    shapes <- rep(default_shapes, length.out = n_series)
  } else if (length(shapes) < n_series) {
    warning("Not enough shapes provided. Recycling shapes.")
    shapes <- rep(shapes, length.out = n_series)
  }
  
  # 对数据进行均匀抽样的函数
  sample_data <- function(df, n, include_ends = TRUE) {
    x_col <- names(df)[1]
    result <- data.frame()
    
    # 对每个Y系列进行抽样
    for (i in 2:ncol(df)) {
      y_col <- names(df)[i]
      temp_df <- data.frame(x = df[[x_col]], y = df[[y_col]], series = y_col)
      
      total_points <- nrow(temp_df)
      
      # 处理点数不足的情况
      if (total_points <= n) {
        selected <- temp_df
      } else {
        # 根据包含端点设置的策略选择点
        if (include_ends && n >= 2) {
          # 始终包括第一个和最后一个点，其余均匀分布
          interval <- (total_points - 1) / (n - 1)
          indices <- round(seq(1, total_points, by = interval))
          # 确保不超出范围且无重复
          indices <- unique(indices[indices <= total_points])
          selected <- temp_df[indices, ]
        } else {
          # 均匀选择n个点
          interval <- total_points / n
          indices <- round(seq(interval/2, total_points, by = interval))
          # 确保不超出范围
          indices <- indices[indices <= total_points]
          selected <- temp_df[indices, ]
        }
      }
      
      result <- rbind(result, selected)
    }
    
    return(result)
  }
  
  # 抽样数据
  sampled_data <- sample_data(data, sample_size, include_endpoints)
  
  # 将系列名称转换为因子并赋予自定义名称
  if (!is.null(column_names)) {
    # 创建映射
    name_map <- setNames(column_names, colnames(data)[-1])
    # 应用映射
    sampled_data$series <- factor(sampled_data$series, 
                                levels = colnames(data)[-1], 
                                labels = name_map[colnames(data)[-1]])
  } else {
    sampled_data$series <- factor(sampled_data$series, levels = colnames(data)[-1])
  }
  
  # 设置x轴范围
  if (is.null(x_lim)) {
    x_range <- range(sampled_data$x)
    x_margin <- (x_range[2] - x_range[1]) * 0.05
    x_lim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  }
  
  # 设置y轴范围
  if (is.null(y_lim)) {
    y_range <- range(sampled_data$y)
    y_margin <- (y_range[2] - y_range[1]) * 0.05
    y_lim <- c(max(0, y_range[1] - y_margin), y_range[2] + y_margin)  # 确保下限不小于0
  }
  
  # 设置x轴刻度
  if (is.null(x_breaks)) {
    x_breaks <- pretty(sampled_data$x, n = 5)
  }
  
  # 设置y轴刻度
  if (is.null(y_breaks)) {
    y_breaks <- pretty(sampled_data$y, n = 5)
  }
  
  # 创建绘图
  p <- ggplot(sampled_data, aes(x = x, y = y, group = series, color = series, shape = series)) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    # 绘制线条 - 仅使用抽样点
    geom_line(size = line_size) +
    # 绘制点 - 仅使用抽样点
    geom_point(size = point_size, stroke = stroke_size) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    theme_classic() +
    theme(axis.text = element_text(size = axis_text_size, color = "black"),
          text = element_text(family = "Arial"),
          axis.title.x = element_text(size = x_title_size),
          axis.title.y = element_text(size = y_title_size, hjust = 0.5)) +
    labs(y = y_label, x = x_label) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(breaks = y_breaks, labels = y_breaks)
  
  # 处理图例
  if (show_legend) {
    p <- p + 
      guides(color = guide_legend(title = NULL),
             shape = guide_legend(title = NULL)) +
      theme(legend.position = legend_position,
            legend.text = element_text(size = legend_text_size))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}


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
#' @return 返回一个ggplot对象。
plot_tradeoff_dual_y_axis <- function(
  data,
  dataset_name, # 移除默认值 ""，使其成为必需参数
  export_path = "./",
  export_format = "pdf",
  colors = c("OCR" = "#AD0626", "ERR" = "#2C3359", "RecipeSize" = "#75B8BF"),
  shapes = c("OCR" = 23, "ERR" = 24, "RecipeSize" = 22),
  line_size = 1.5,
  point_size = 9,
  stroke_size = 4.5,
  axis_text_size = 42,
  title_size = 48,
  legend_text_size = 32,
  plot_width = 12,
  plot_height = 8
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
        label <- scales::number(x / 1e6, accuracy = 1, trim = TRUE)
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
    ) +
    scale_y_continuous(
      name = "DRR & ERR", # 移除百分号
      expand = expansion(mult = c(0.05, 0.1)),
      sec.axis = sec_axis(
        transform = ~ (.- primary_range[1]) / scaling_factor + secondary_range[1],
        name = "Recipe Size",
        labels = format_bytes_manual
      )
    ) +
    
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
      x = "Large File Threshold"
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
      legend.position = "none" # 移除图例
    )

  # 5. 保存文件
  export_name <- paste0(dataset_name, "_tradeoff_plot.", export_format)
  ggsave(file.path(export_path, export_name), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}