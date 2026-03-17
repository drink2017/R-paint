# 设置工作目录
setwd("C:/Users/YP/Desktop/work/R-paint/Paper/hotstorage26/challenge/3/1")

# 加载绘图函数
source("C:/Users/YP/Desktop/work/R-paint/MyR/Line_NoPoint.R")

# 读取四个数据文件
df_web <- read.table("web.txt", header=TRUE, sep="\t")
df_windows <- read.table("windows.txt", header=TRUE, sep="\t")
df_glibc <- read.table("glibc.txt", header=TRUE, sep="\t")
df_linux <- read.table("linux.txt", header=TRUE, sep="\t")

# 取所有数据的最小长度
min_len <- min(
  length(df_web[[3]]),
  length(df_windows[[3]]),
  length(df_glibc[[3]]),
  length(df_linux[[3]])
)

# 在每组数据前加一个0，确保从原点开始
web <- c(0, as.numeric(df_web[[3]])[1:min_len])
windows <- c(0, as.numeric(df_windows[[3]])[1:min_len])
glibc <- c(0, as.numeric(df_glibc[[3]])[1:min_len])
linux <- c(0, as.numeric(df_linux[[3]])[1:min_len])

# 合并为数据框
plot_data <- data.frame(
  web = web,
  windows = windows,
  glibc = glibc,
  linux = linux
)

# 画图
plot_line_comparison_log2(
  data = plot_data,
  export_path = "./",
  export_name = "reuse.pdf",
  x_label = "Reuse Distance",
  y_label = "CDF",
  y_breaks = seq(0, 1, by = 0.2),   # 明确指定纵轴刻度
  axis_text_size = 30,
  x_title_size = 30,
  y_title_size = 30
)