plot_line_comparison <- function(
  data,
  export_path = "./",
  export_name = "line_comparison.pdf",
  x_lim = NULL,
  y_lim = NULL,
  line_size = 2.8,
  axis_text_size = 42,
  x_title_size = 48,
  y_title_size = 45,
  x_breaks = NULL,
  y_breaks = NULL,
  x_label = "X_Value",
  y_label = "Y_Value",
  plot_width = 10,
  plot_height = 5,
  range_extension = 1.0,  # 控制轴范围的扩展系数
  x_expand_left = 0,      # 新参数：x轴左侧扩展量
  x_expand_right = 0,   # 新参数：x轴右侧扩展量
  y_expand_bottom = 0,    # 新参数：y轴底部扩展量
  y_expand_top = 0.1,     # 新参数：y轴顶部扩展量
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C","#FF5809")
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 数据预处理 - 确保数据格式正确
  if (!is.data.frame(data)) {
    stop("数据必须是数据框")
  }
  
  # 创建长格式数据框用于ggplot
  plot_data <- data.frame()
  col_count <- 0
  
  # 处理每列数据
  for (col_name in names(data)) {
    col_count <- col_count + 1
    if (col_count > length(colors)) break # 确保不超过可用颜色数
    
    # 提取当前列
    col_data <- data[[col_name]]
    
    # 添加到主数据框
    temp_df <- data.frame(
      x = 1:length(col_data),
      y = col_data,
      group = factor(col_count)
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  
  # 检查是否有有效数据
  if (nrow(plot_data) == 0) {
    stop("没有有效的数据可供绘图")
  }
  
  # 创建基本图表
  p <- ggplot(plot_data, aes(x = x, y = y, color = group)) +
       geom_line(size = line_size) +
       scale_color_manual(values = colors[1:col_count], guide = "none")
  
  # 基本主题设置
  p <- p + 
    # 白底，没有上边框和右边框
    theme_classic() +
    # 设置坐标轴上字体大小
    theme(axis.text = element_text(size = axis_text_size, color = "black")) +
    # 设置字体样式
    theme(text = element_text(family = "Arial")) +
    # 设置label字体大小
    theme(axis.title.x = element_text(size = x_title_size)) +
    theme(axis.title.y = element_text(size = y_title_size)) +
    # ylabel位置
    theme(axis.title.y = element_text(hjust = 0.5)) +
    # 设置label内容
    labs(y = y_label, x = x_label)
  
  # 确定x轴和y轴的范围
  if(is.null(x_lim)) {
    x_range <- range(plot_data$x, na.rm = TRUE)
    # 计算扩展范围
    x_range_width <- diff(x_range)
    x_range <- c(x_range[1], x_range[2] + x_range_width * (range_extension - 1))
  } else {
    x_range <- x_lim
  }
  
  if(is.null(y_lim)) {
    y_range <- range(plot_data$y, na.rm = TRUE)
    # 计算扩展范围
    y_range_width <- diff(y_range)
    y_range <- c(y_range[1], y_range[2] + y_range_width * (range_extension - 1))
  } else {
    y_range <- y_lim
  }
  
  # 调整x刻度，使用add参数来控制扩展
if(is.null(x_breaks)) {
  # 使用自动计算的刻度
  p <- p + scale_x_continuous(
    expand = expansion(add = c(x_expand_left * diff(x_range), 
                             max(0.02, x_expand_right) * diff(x_range))),
    labels = function(breaks) {
      # 给最后一个标签添加额外的空格，使其向左移动
      labels <- as.character(breaks)
      if(length(labels) > 0) {
        labels[length(labels)] <- paste0(labels[length(labels)], "    ")
      }
      return(labels)
    }
  )
} else {
  # 使用自定义刻度，并且确保最后一个刻度标签不会溢出
  p <- p + scale_x_continuous(
    breaks = x_breaks,
    labels = function(breaks) {
      labels <- as.character(breaks)
      if(length(labels) > 0) {
        labels[length(labels)] <- paste0(labels[length(labels)], "  ")
      }
      return(labels)
    },
    expand = expansion(add = c(x_expand_left * diff(x_range), 
                             max(0.02, x_expand_right) * diff(x_range)))
  )
}
  
  # 调整y刻度，使用add参数来控制扩展
  if(is.null(y_breaks)) {
    # 如果没有提供y_breaks，让ggplot2自动处理
    p <- p + scale_y_continuous(
      expand = expansion(add = c(y_expand_bottom * diff(y_range), 
                                y_expand_top * diff(y_range)))
    )
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    if(max(y_breaks) > y_range[2] * 0.95) {
      y_breaks <- y_breaks[y_breaks < max(y_breaks)]
    }
    p <- p + scale_y_continuous(
      breaks = y_breaks, 
      labels = y_breaks,
      expand = expansion(add = c(y_expand_bottom * diff(y_range), 
                                y_expand_top * diff(y_range)))
    )
  }
  
  # 应用坐标范围
  if (!is.null(x_lim) || !is.null(y_lim) || range_extension > 1) {
    p <- p + coord_cartesian(xlim = x_range, ylim = y_range)
  }
p <- p + theme(
  # 增加合适的边距
  plot.margin = margin(t = 15, r = 15, b = 10, l = 10, unit = "pt"),
  
  # 确保坐标轴文本有足够空间
  axis.text.x = element_text(
    size = axis_text_size, 
    color = "black",
    margin = margin(t = 5)
  ),
  axis.text.y = element_text(
    size = axis_text_size, 
    color = "black",
    margin = margin(r = 5)
  )
)


  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

# 同样修改 plot_line_comparison_xcdf 函数
plot_line_comparison_xcdf <- function(
  data,
  export_path = "./",
  export_name = "line_comparison_cdf.pdf",
  x_lim = NULL,
  y_lim = NULL,
  line_size = 2.8,
  axis_text_size = 42,
  x_title_size = 48,
  y_title_size = 45,
  x_breaks = NULL,
  y_breaks = NULL,
  x_label = "Cumulative Progress",
  y_label = "Value",
  plot_width = 10,
  plot_height = 5,
  range_extension = 1.1,  # 新参数：控制轴范围的扩展系数
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C")
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 转换输入，确保是列表形式
  if (is.data.frame(data)) {
    data_list <- as.list(data)
  } else if (is.list(data)) {
    data_list <- data
  } else {
    stop("Data must be a data frame or list")
  }
  
  # 创建一个空的数据框来存储转换后的数据
  plot_data <- data.frame()
  
  # 处理每列数据，并归一化x轴
  col_count <- 0
  for (col_name in names(data_list)) {
    col_count <- col_count + 1
    if (col_count > length(colors)) break # 确保不超过可用颜色数
    
    # 提取当前列的非NA值
    col_data <- data_list[[col_name]][!is.na(data_list[[col_name]])]
    if (length(col_data) == 0) next # 跳过空列
    
    # 创建归一化的x值（从0到1）
    x_norm <- seq(0, 1, length.out = length(col_data))
    
    # 添加到主数据框
    temp_df <- data.frame(
      x = x_norm,
      y = col_data,
      group = factor(col_count)
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  
  # 如果没有有效数据，返回错误
  if (nrow(plot_data) == 0) {
    stop("No valid data found to plot")
  }
  
  # 创建基本图表
  p <- ggplot(plot_data, aes(x = x, y = y, color = group)) +
       geom_line(size = line_size) +
       scale_color_manual(values = colors[1:col_count], guide = "none")
  
  # 基本主题设置
  p <- p + 
    # 白底，没有上边框和右边框
    theme_classic() +
    # 设置坐标轴上字体大小
    theme(axis.text = element_text(size = axis_text_size, color = "black")) +
    # 设置字体样式
    theme(text = element_text(family = "Arial")) +
    # 设置label字体大小
    theme(axis.title.x = element_text(size = x_title_size)) +
    theme(axis.title.y = element_text(size = y_title_size)) +
    # ylabel位置
    theme(axis.title.y = element_text(hjust = 0.5)) +
    # 设置label内容
    labs(y = y_label, x = x_label)
  
  # 默认x轴范围为0-1，考虑扩展系数
  if(is.null(x_lim)) {
    x_range <- c(0, 1)
  } else {
    x_range <- x_lim
  }
  
  # y轴范围，考虑扩展系数
  if(is.null(y_lim)) {
    y_range <- range(plot_data$y, na.rm = TRUE)
    # 计算扩展范围
    y_range_width <- diff(y_range)
    y_range <- c(y_range[1], y_range[2] + y_range_width * (range_extension - 1))
  } else {
    y_range <- y_lim
  }
  
  # 调整x刻度，确保最大值显示完全
  if(is.null(x_breaks)) {
    # 如果没有提供x_breaks，默认使用0, 0.25, 0.5, 0.75, 1.0
    p <- p + scale_x_continuous(
      breaks = seq(0, 1, 0.25),
      labels = seq(0, 1, 0.25),
      expand = expansion(mult = c(0.02, 0.08))
    )
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    if(max(x_breaks) > x_range[2] * 0.95) {
      x_breaks <- x_breaks[x_breaks < max(x_breaks)]
    }
    p <- p + scale_x_continuous(
      breaks = x_breaks, 
      labels = x_breaks,
      expand = expansion(mult = c(0.02, 0.08))
    )
  }
  
  # 调整y刻度，确保最大值显示完全
  if(is.null(y_breaks)) {
    # 如果没有提供y_breaks，让ggplot2自动处理
    p <- p + scale_y_continuous(expand = expansion(mult = c(0.02, 0.08)))
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    # if(max(y_breaks) > y_range[2] * 0.95) {
    #   y_breaks <- y_breaks[y_breaks < max(y_breaks)]
    # }
    p <- p + scale_y_continuous(
      breaks = y_breaks, 
      labels = y_breaks,
      expand = expansion(mult = c(0.02, 0.08))
    )
  }
  
  # 应用坐标范围
  if (!is.null(x_lim) || !is.null(y_lim) || range_extension > 1) {
    p <- p + coord_cartesian(xlim = x_range, ylim = y_range)
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

plot_line_comparison_xxcdf <- function(
    data,
    export_path = "./",
    export_name = "line_comparison_cdf.pdf",
    x_lim = NULL,
    y_lim = NULL,
    line_size = 2.8,
    axis_text_size = 42,
    x_title_size = 48,
    y_title_size = 45,
    x_breaks = NULL,
    y_breaks = NULL,
    x_label = "Cumulative Progress",
    y_label = "Value",
    plot_width = 10,
    plot_height = 5,
    range_extension = 1.15,  # 增加默认扩展系数
    colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C")
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 转换输入，确保是列表形式
  if (is.data.frame(data)) {
    data_list <- as.list(data)
  } else if (is.list(data)) {
    data_list <- data
  } else {
    stop("Data must be a data frame or list")
  }
  
  # 创建一个空的数据框来存储转换后的数据
  plot_data <- data.frame()
  
  # 处理每列数据，并归一化x轴
  col_count <- 0
  for (col_name in names(data_list)) {
    col_count <- col_count + 1
    if (col_count > length(colors)) break
    
    # 提取当前列的非NA值
    col_data <- data_list[[col_name]][!is.na(data_list[[col_name]])]
    if (length(col_data) == 0) next
    
    # 创建归一化的x值（从0到1）
    x_norm <- seq(0, 1, length.out = length(col_data))
    
    # 添加到主数据框
    temp_df <- data.frame(
      x = x_norm,
      y = col_data,
      group = factor(col_count)
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  
  # 如果没有有效数据，返回错误
  if (nrow(plot_data) == 0) {
    stop("No valid data found to plot")
  }
  
  # 计算y轴的真实范围
  y_data_range <- range(plot_data$y, na.rm = TRUE)
  
  # 创建基本图表
  p <- ggplot(plot_data, aes(x = x, y = y, color = group)) +
    geom_line(size = line_size) +
    scale_color_manual(values = colors[1:col_count], guide = "none")
  
  # 基本主题设置
  p <- p + 
    theme_classic() +
    theme(axis.text = element_text(size = axis_text_size, color = "black")) +
    theme(text = element_text(family = "Arial")) +
    theme(axis.title.x = element_text(size = x_title_size)) +
    theme(axis.title.y = element_text(size = y_title_size)) +
    theme(axis.title.y = element_text(hjust = 0.5)) +
    labs(y = y_label, x = x_label)
  
  # X轴设置
  if(is.null(x_breaks)) {
    p <- p + scale_x_continuous(
      breaks = seq(0, 1, 0.25),
      labels = seq(0, 1, 0.25),
      expand = expansion(mult = c(0.02, 0.05))  # 减少右侧扩展
    )
  } else {
    p <- p + scale_x_continuous(
      breaks = x_breaks, 
      labels = x_breaks,
      expand = expansion(mult = c(0.02, 0.05))
    )
  }
  
  # Y轴设置 - 关键修改
  if(is.null(y_lim)) {
    # 计算扩展后的y轴范围
    y_range_width <- diff(y_data_range)
    y_max_extended <- y_data_range[2] + y_range_width * (range_extension - 1)
    
    if(is.null(y_breaks)) {
      p <- p + scale_y_continuous(
        expand = expansion(mult = c(0.02, 0.1)),  # 给顶部更多空间
        limits = c(y_data_range[1], y_max_extended)  # 明确设置limits
      )
    } else {
      # 过滤掉超出扩展范围的刻度
      valid_y_breaks <- y_breaks[y_breaks <= y_max_extended * 0.95]
      p <- p + scale_y_continuous(
        breaks = valid_y_breaks,
        labels = valid_y_breaks,
        expand = expansion(mult = c(0.02, 0.1)),
        limits = c(y_data_range[1], y_max_extended)
      )
    }
  } else {
    # 用户指定了y_lim
    if(is.null(y_breaks)) {
      p <- p + scale_y_continuous(
        expand = expansion(mult = c(0.02, 0.1)),
        limits = y_lim
      )
    } else {
      valid_y_breaks <- y_breaks[y_breaks >= y_lim[1] & y_breaks <= y_lim[2]]
      p <- p + scale_y_continuous(
        breaks = valid_y_breaks,
        labels = valid_y_breaks,
        expand = expansion(mult = c(0.02, 0.1)),
        limits = y_lim
      )
    }
  }
  
  # 如果用户指定了x_lim，应用它
  if (!is.null(x_lim)) {
    p <- p + coord_cartesian(xlim = x_lim)
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}
create_legend_pdf <- function(
  series_names,
  export_path = "./",
  export_name = "legend.pdf",
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C"),
  legend_title = NULL,
  legend_position = "horizontal", # "horizontal" 或 "vertical"
  text_size = 36,
  title_size = 40,
  plot_width = 10,
  plot_height = 3
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 尝试加载cowplot
  if(!requireNamespace("cowplot", quietly = TRUE)) {
    install.packages("cowplot")
    library(cowplot)
  } else {
    library(cowplot)
  }
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 确保series_names长度不超过可用颜色数量
  if(length(series_names) > length(colors)) {
    warning("提供的系列名称数量超过可用颜色数量，将只使用前", length(colors), "个系列")
    series_names <- series_names[1:length(colors)]
  }
  
  # 创建一个更可靠的数据框用于生成图例
  dummy_df <- data.frame(
    x = 1:length(series_names),
    y = 1:length(series_names),
    group = factor(series_names, levels = series_names)
  )
  
  # 创建一个包含实际线条的图（而不是使用点）
  p <- ggplot(dummy_df, aes(x, y, color = group)) +
       geom_line(size = 2) +  # 使用实际可见的线条
       scale_color_manual(
         values = colors[1:length(series_names)],
         name = legend_title,
         guide = guide_legend(
           title.position = "top",
           title.hjust = 0.5,
           nrow = if(legend_position == "horizontal") 1 else length(series_names)
         )
       ) +
       theme_void() +
       theme(
         text = element_text(family = "Arial"),
         legend.text = element_text(size = text_size),
         legend.title = element_text(size = title_size),
         legend.position = "bottom",  # 确保图例在可见位置
         legend.box.margin = margin(0, 0, 0, 0)
       )
  
  # 为了确保生成正确的图例，先保存完整图形
  temp_plot_path <- paste(export_path, "temp_plot.pdf", sep = "")
  ggsave(temp_plot_path, plot = p, width = plot_width, height = plot_height)
  
  # 创建一个只有图例的图
  legend_only <- p + theme(legend.position = "bottom") +
                   guides(color = guide_legend(nrow = if(legend_position == "horizontal") 1 else length(series_names)))
  
  # 直接导出图例
  ggsave(paste(export_path, export_name, sep = ""), 
         plot = legend_only, 
         width = plot_width, 
         height = plot_height)
  
  # 删除临时文件
  if(file.exists(temp_plot_path)) {
    file.remove(temp_plot_path)
  }
  
  return(legend_only)
}

# 同样修改 plot_line_comparison_xcdf 函数
plot_line_comparison_xcdf <- function(
  data,
  export_path = "./",
  export_name = "line_comparison_cdf.pdf",
  x_lim = NULL,
  y_lim = NULL,
  line_size = 2.8,
  axis_text_size = 42,
  x_title_size = 48,
  y_title_size = 45,
  x_breaks = NULL,
  y_breaks = NULL,
  x_label = "Cumulative Progress",
  y_label = "Value",
  plot_width = 10,
  plot_height = 5,
  range_extension = 1.1,  # 控制轴范围的扩展系数
  x_expand = c(0, 0),     # 新参数：x轴扩展系数，默认为不扩展
  y_expand = c(0, 0),     # 新参数：y轴扩展系数，默认为不扩展
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C")
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 转换输入，确保是列表形式
  if (is.data.frame(data)) {
    data_list <- as.list(data)
  } else if (is.list(data)) {
    data_list <- data
  } else {
    stop("Data must be a data frame or list")
  }
  
  # 创建一个空的数据框来存储转换后的数据
  plot_data <- data.frame()
  
  # 处理每列数据，并归一化x轴
  col_count <- 0
  for (col_name in names(data_list)) {
    col_count <- col_count + 1
    if (col_count > length(colors)) break # 确保不超过可用颜色数
    
    # 提取当前列的非NA值
    col_data <- data_list[[col_name]][!is.na(data_list[[col_name]])]
    if (length(col_data) == 0) next # 跳过空列
    
    # 创建归一化的x值（从0到1）
    x_norm <- seq(0, 1, length.out = length(col_data))
    
    # 添加到主数据框
    temp_df <- data.frame(
      x = x_norm,
      y = col_data,
      group = factor(col_count)
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  
  # 如果没有有效数据，返回错误
  if (nrow(plot_data) == 0) {
    stop("No valid data found to plot")
  }
  
  # 创建基本图表
  p <- ggplot(plot_data, aes(x = x, y = y, color = group)) +
       geom_line(size = line_size) +
       scale_color_manual(values = colors[1:col_count], guide = "none")
  
  # 基本主题设置
  p <- p + 
    # 白底，没有上边框和右边框
    theme_classic() +
    # 设置坐标轴上字体大小
    theme(axis.text = element_text(size = axis_text_size, color = "black")) +
    # 设置字体样式
    theme(text = element_text(family = "Arial")) +
    # 设置label字体大小
    theme(axis.title.x = element_text(size = x_title_size)) +
    theme(axis.title.y = element_text(size = y_title_size)) +
    # ylabel位置
    theme(axis.title.y = element_text(hjust = 0.5)) +
    # 设置label内容
    labs(y = y_label, x = x_label)
  
  # 默认x轴范围为0-1，考虑扩展系数
  if(is.null(x_lim)) {
    x_range <- c(0, 1)
  } else {
    x_range <- x_lim
  }
  
  # y轴范围，考虑扩展系数
  if(is.null(y_lim)) {
    y_range <- range(plot_data$y, na.rm = TRUE)
    # 计算扩展范围
    y_range_width <- diff(y_range)
    y_range <- c(y_range[1], y_range[2] + y_range_width * (range_extension - 1))
  } else {
    y_range <- y_lim
  }
  
  if(is.null(x_breaks)) {
    # 如果没有提供x_breaks，默认使用0, 0.25, 0.5, 0.75, 1.0
    p <- p + scale_x_continuous(
      breaks = seq(0, 1, 0.25),
      labels = seq(0, 1, 0.25),
      expand = expansion(mult = x_expand)  # 使用自定义的扩展参数
    )
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    # if(max(x_breaks) > x_range[2] * 0.95) {
    #   x_breaks <- x_breaks[x_breaks < max(x_breaks)]
    # }
    p <- p + scale_x_continuous(
      breaks = x_breaks, 
      labels = x_breaks,
      expand = expansion(mult = x_expand)  # 使用自定义的扩展参数
    )
  }
  
  # 调整y刻度，确保最大值显示完全，并且根据参数控制点是否贴合在轴上
  if(is.null(y_breaks)) {
    # 如果没有提供y_breaks，让ggplot2自动处理刻度
    p <- p + scale_y_continuous(expand = expansion(mult = y_expand))  # 使用自定义的扩展参数
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    # if(max(y_breaks) > y_range[2] * 0.95) {
    #   y_breaks <- y_breaks[y_breaks < max(y_breaks)]
    # }
    p <- p + scale_y_continuous(
      breaks = y_breaks, 
      labels = y_breaks,
      expand = expansion(mult = y_expand)  # 使用自定义的扩展参数
    )
  }
  
  # 应用坐标范围
  if (!is.null(x_lim) || !is.null(y_lim) || range_extension > 1) {
    p <- p + coord_cartesian(xlim = x_range, ylim = y_range)
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

plot_line_comparison_log2 <- function(
  data,
  export_path = "./",
  export_name = "line_comparison_log2.pdf",
  x_lim = NULL,
  y_lim = NULL,
  line_size = 2.8,
  axis_text_size = 42,
  x_title_size = 48,
  y_title_size = 45,
  x_breaks = NULL,
  y_breaks = NULL,
  x_label = "log2(X_Value)",
  y_label = "Y_Value",
  plot_width = 10,
  plot_height = 5,
  range_extension = 1.0,
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C","#FF5809"),
  x_labels = NULL   # 新增参数
) {
  library(ggplot2)
  library(extrafont)
  library(showtext)
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))

  if (!is.data.frame(data)) stop("数据必须是数据框")

  plot_data <- data.frame()
  col_count <- 0
  for (col_name in names(data)) {
    col_count <- col_count + 1
    if (col_count > length(colors)) break
    col_data <- data[[col_name]]
    temp_df <- data.frame(
      x = 1:length(col_data),
      y = col_data,
      group = factor(col_count)
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  if (nrow(plot_data) == 0) stop("没有有效的数据可供绘图")

  # x轴必须大于0才能做log2变换
  plot_data <- plot_data[plot_data$x > 0, ]

  # 自动确定x轴范围
  if(is.null(x_lim)) {
    x_range <- range(plot_data$x, na.rm = TRUE)
    x_range_width <- diff(log2(x_range))
    x_range <- c(x_range[1], 2^(log2(x_range[2]) + x_range_width * (range_extension - 1)))
  } else {
    x_range <- x_lim
  }
  if(is.null(y_lim)) {
    y_range <- range(plot_data$y, na.rm = TRUE)
    y_range_width <- diff(y_range)
    y_range <- c(y_range[1], y_range[2] + y_range_width * (range_extension - 1))
  } else {
    y_range <- y_lim
  }

  # 自动生成2的幂次刻度
  if (is.null(x_breaks)) {
    min_pow <- ceiling(log2(x_range[1]))
    max_pow <- floor(log2(x_range[2]))
    x_breaks <- 2^(min_pow:max_pow)
    # 保证x_range边界也在breaks里
    if (!x_range[1] %in% x_breaks) x_breaks <- sort(unique(c(x_range[1], x_breaks)))
    if (!x_range[2] %in% x_breaks) x_breaks <- sort(unique(c(x_breaks, x_range[2])))
  }

  # 如果最后一个break等于x_range[2]，则移除
  if (length(x_breaks) > 0 && abs(x_breaks[length(x_breaks)] - x_range[2]) < 1e-8) {
    x_breaks <- x_breaks[-length(x_breaks)]
  }

  # x轴标签为整数
  n <- 2
  if (is.null(x_labels)) {
    x_labels <- as.character(x_breaks)
    if (length(x_labels) > 0) {
      x_labels[seq_along(x_labels) %% n != 1] <- ""  # 只显示第1、1+n、1+2n...个
    }
  }

  p <- ggplot(plot_data, aes(x = x, y = y, color = group)) +
    geom_line(size = line_size) +
    scale_color_manual(values = colors[1:col_count], guide = "none") +
    theme_classic() +
    theme(
      axis.text = element_text(size = axis_text_size, color = "black"),
      text = element_text(family = "Arial"),
      axis.title.x = element_text(size = x_title_size),
      axis.title.y = element_text(size = y_title_size, hjust = 0.5),
      plot.margin = margin(t = 15, r = 15, b = 10, l = 10, unit = "pt"),
      axis.text.x = element_text(size = axis_text_size, color = "black", margin = margin(t = 5), angle = 0),
      axis.text.y = element_text(size = axis_text_size, color = "black", margin = margin(r = 5))
    ) +
    labs(y = y_label, x = x_label) +
    scale_x_continuous(
      trans = "log2",
      breaks = x_breaks,
      labels = x_labels,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      labels = y_breaks,
      expand = c(0, 0)
    ) +
    coord_cartesian(xlim = x_range, ylim = y_range)

  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  return(p)
}