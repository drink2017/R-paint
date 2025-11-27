# 加载模板函数
source("MyR/Line_NoPoint.R")

# 读取两个 CSV 文件
data1 <- read.csv("motivationDCEdata/linux_allGreedy.csv", header = TRUE)
data2 <- read.csv("motivationDCEdata/Odess_linux.csv", header = TRUE)

# 假设第一列为 x，第二列为 y
x_vals <- data1[[1]]  # 假设两个文件 x 轴完全一致
y1 <- data1[[2]]
y2 <- data2[[2]]

# 合并为一个数据框，每列为一条线
data_df <- data.frame(
  Line1 = y1,
  Line2 = y2
)
rownames(data_df) <- x_vals  # 设置 x 轴标签

# 无点折线图
plot_line_comparison(
  data = data_df,
  x_label = "version",
  y_label = "DCE",
  export_name = "line_no_point.pdf"
)