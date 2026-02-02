# 设置工作目录
setwd("C:/Users/YP/Desktop/work/R-paint/Paper/hotstorage26/challenge/3/amplification")

# 加载绘图函数
source("C:/Users/YP/Desktop/work/R-paint/MyR/Line_NoPoint.R")

# 读取数据
glibc_allGreedy <- scan("glibc_allGreedy.txt")
glibc_odess     <- scan("glibc_odess.txt")
linux_allGreedy <- scan("linux_allGreedy.txt")
linux_odess     <- scan("linux_odess.txt")

# 组合成数据框
glibc_df <- data.frame(
  allGreedy = glibc_allGreedy,
  odess = glibc_odess
)
linux_df <- data.frame(
  allGreedy = linux_allGreedy,
  odess = linux_odess
)

# 绘制glibc图
plot_line_comparison(
  data = glibc_df,
  export_path = "./",
  export_name = "glibc_line.pdf",
  x_label = "Backup",
  y_label = "Read Amp.",
  colors = c("#AD0626", "#B79AD1")
)

# 绘制linux图
plot_line_comparison(
  data = linux_df,
  export_path = "./",
  export_name = "linux_line.pdf",
  x_label = "Backup",
  y_label = "Amplification",
  colors = c("#AD0626", "#B79AD1")
)