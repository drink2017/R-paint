# 加载自定义绘图函数
source("E:/Users/drinkwater/Desktop/R-script-template/MyR/Line_Point.R")

# 读取数据（注意路径和分隔符）
data <- read.table("e:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/1/2/glibc.txt", header = TRUE, sep = "\t")

# 构建数据框，第一列为x轴，第三列为y轴
df <- data.frame(
  x = data[[1]],
  y = data[[3]]
)

# 或者调用自定义函数（x轴为实际reverse_pos，y轴为cdf）
# 需要将数据转换为行名为x、列名为y的data.frame
df_for_func <- data.frame(row.names = df$x, cdf = df$y)

plot_line_comparison(
  data = df_for_func,
  x_label = "reverse pos",
  y_label = "cdf",
  export_path = "E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/1/2/",
  export_name = "glibc.pdf",
  y_breaks = seq(0, 1, 0.2),
  y_lim = c(0, 1) 
)