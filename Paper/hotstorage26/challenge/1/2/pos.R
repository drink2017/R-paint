setwd("C:/Users/YP/Desktop/work/R-paint/Paper/hotstorage26/challenge/1/2")

# 加载自定义绘图函数
source("../../../../../MyR/Line_NoPoint.R")

# 文件名列表
files <- c("windowsLog.txt", "glibc.txt", "linux.txt", "web.txt")
pdfs  <- c("windowsLog.pdf", "glibc.pdf", "linux.pdf", "web.pdf")

# 循环处理每个文件
for (i in seq_along(files)) {
  # 读取数据
  data <- read.table(files[i], header = TRUE, sep = "\t")
  
  # 构建数据框，第一列+1为x轴，第三列为y轴
  df <- data.frame(
    x = data[[1]] + 1,
    y = data[[3]]
  )
  # 在最前面加上(0, 0)这个点
  df <- rbind(data.frame(x = 0, y = 0), df)
  # 转换为行名为x、列名为y的data.frame
  df_for_func <- data.frame(row.names = df$x, cdf = df$y)
  
  # 绘图
  plot_line_comparison(
    data = df_for_func,
    x_label = "Reverse Position",
    y_label = "CDF",
    export_path = "./",
    export_name = pdfs[i],
    y_breaks = seq(0, 1, 0.2),
    y_lim = c(0, 1)
  )
}