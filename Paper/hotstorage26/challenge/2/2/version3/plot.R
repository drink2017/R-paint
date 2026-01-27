# 设置工作目录
setwd("E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/2/2/version3")

# 加载绘图函数
source("E:/Users/drinkwater/Desktop/R-script-template/MyR/Line_NoPoint.R")

draw_pdf <- function(txt_path, pdf_name) {
  df <- read.table(txt_path, header=TRUE, sep="\t")
  x <- as.numeric(df[[1]])
  y <- as.numeric(df[[3]])
  plot_data <- data.frame(x = x, y = y)
  # 计算x_breaks
  min_pow <- ceiling(log2(min(x)))
  max_pow <- floor(log2(max(x)))
  x_breaks <- 2^(min_pow:max_pow)
  # 只显示每隔4个刻度的标签
  x_labels <- rep("", length(x_breaks))
  x_labels[seq(1, length(x_breaks), by = 4)] <- as.character(x_breaks[seq(1, length(x_breaks), by = 4)])
  y_breaks <- seq(0, 1, by = 0.2)  # 根据你的数据范围自定义
  plot_line_comparison_log2(
    data = data.frame(y = y),
    export_path = "./",
    export_name = pdf_name,
    x_label = "distance",
    y_label = "CDF",
    x_breaks = x_breaks,
    # x_labels = x_labels,
    y_breaks = y_breaks   # 传递y_breaks参数
  )
}

draw_pdf("web.txt", "web.pdf")
draw_pdf("windows.txt", "windows.pdf")
draw_pdf("glibc.txt", "glibc.pdf")
draw_pdf("linux.txt", "linux.pdf")