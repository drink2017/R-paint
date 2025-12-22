setwd("C:/Users/YP/Desktop/work/R-paint/Paper/hotstorage26/challenge/2/1/")

# 加载绘图函数
source("C:\\Users\\YP\\Desktop\\work\\R-paint\\MyR\\Line_NoPoint.R")

# 文件名列表
files <- c("windowsLog.txt", "glibc.txt", "linux.txt", "web.txt")
pdfs  <- c("windowsLog.pdf", "glibc.pdf", "linux.pdf", "web.pdf")

# 循环处理每个文件
for (i in seq_along(files)) {
  # 读取数据
  data <- read.table(files[i], header = TRUE)
  
  # 只取第一列和第三列
  df <- data.frame(
    x = data[[1]],
    y = data[[3]]
  )
  # 在最前面加上(0, 0)这个点
  df <- rbind(data.frame(x = 0, y = 0), df)
  # 转换为行名为x、列名为y的data.frame
  df_for_func <- data.frame(row.names = df$x, cdf = df$y)
  
  # 绘图
  plot_line_comparison(
    data = df_for_func,
    x_label = "Candidate Set Size",
    y_label = "CDF",
    export_path = "./",
    export_name = pdfs[i],
    y_lim = c(0, 1)
  )
}