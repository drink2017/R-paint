# 设置工作路径
setwd("E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/2/1/version2")

# 加载自定义Bar绘图函数
source("E:/Users/drinkwater/Desktop/R-script-template/MyR/Bar.R")

# 读取数据
df <- read.csv("web.csv", header = TRUE)

# 按HitRatio降序排列
df <- df[order(-df$HitRatio), ]

# 取前N个（如前20个）可视化
N <- 20
labels <- as.character(1:N)  # 用编号作为x轴标签
data1 <- df$HitRatio[1:N]

# 用自定义函数画图
create_comparison_barplot(
  data1 = data1,
  data2 = rep(0, N), # 只画一组，另一组为0
  labels = labels,
  fill_colors = c("#AD0626", "#B79AD1"),
  fill_names = c("HitRatio", ""),
  x_label = "Super Feature",
  y_label = "Hit Ratio",
  export_name = "web.pdf",
  export_path = "./",
  width = 10,
  height = 5,
  show_legend = FALSE,
  show_data_labels = FALSE,
  axis_text_size = 10
)