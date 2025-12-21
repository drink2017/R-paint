# 加载绘图函数
source("E:/Users/drinkwater/Desktop/R-script-template/MyR/Line_Point.R")

# 读取数据
data <- read.table("e:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/2/1/glibc.txt", header = TRUE)

# 只取第一列和第三列
df <- data.frame(
  x = data[[1]],
  y = data[[3]]
)

# 画折线图（x轴为vector_size，y轴为cdf）
plot_line_comparison(
  data = data.frame(df$y), # 只画y，x轴用默认序号
  x_label = "vector size",
  y_label = "cdf",
  export_path = "E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/2/1/",
  export_name = "glibc.pdf",
  y_lim = c(0, 1)
)