# 设置工作路径
setwd("C:\\Users\\YP\\Desktop\\work\\R-paint\\Paper\\hotstorage26\\challenge\\2\\1\\version2")

# 加载自定义Bar绘图函数
source("../../../../../../MyR/Bar.R")

# 文件名列表
csv_files <- c("windowsLog.csv")
pdf_files <- c("windowsLog.pdf")

for (i in seq_along(csv_files)) {
  # 读取数据
  df <- read.csv(csv_files[i], header = TRUE)
  # 按HitRatio降序排列
  df <- df[order(-df$HitRatio), ]
  # 取前N个
  N <- 10
  labels <- as.character(1:N)
  data1 <- df$HitRatio[1:N]

  create_single_barplot(
    data = data1,
    labels = labels,
    y_breaks = c(0.0, 0.1, 0.2, 0.3, 0.4),
    fill_color = "#AD0626",
    fill_name = "HitRatio",
    x_label = "Candidate Set",
    y_label = "Proportion",
    export_name = pdf_files[i],
    export_path = "./",
    width = 10,
    height = 5,
    show_legend = FALSE,
    show_data_labels = FALSE,
    y_max_multiplier = 1.1
  )
}