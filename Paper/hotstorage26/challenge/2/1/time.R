# 设置当前工作路径
setwd("E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/2/1/")

# 加载绘图函数
source("E:/Users/drinkwater/Desktop/R-script-template/MyR/Line_Point.R")

# 定义文件名和导出名
files <- c("glibc.txt", "linux.txt", "web.txt", "windowsLog.txt")
export_names <- c("glibc.pdf", "linux.pdf", "web.pdf", "windowsLog.pdf")

# 循环处理每个文件
for (i in seq_along(files)) {
  # 读取数据
  data <- read.table(files[i], header = TRUE)
  
  # 只取第一列和第三列
  df <- data.frame(
    x = data[[1]],
    y = data[[3]]
  )
  
  # 画折线图
  plot_line_comparison(
    data = data.frame(df$y), # 只画y，x轴用默认序号
    x_label = "vector size",
    y_label = "cdf",
    export_path = getwd(),
    export_name = export_names[i],
    y_lim = c(0, 1)
  )
}