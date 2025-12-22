# 设置当前工作路径
setwd("E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/2/2/version2")

# 载入绘图包
library(ggplot2)

# 定义文件列表和标题
files <- c("windowsLog.csv", "glibc.csv", "linux.csv", "web.csv")
titles <- c("windowsLog", "glibc", "linux", "web")

# 循环处理每个文件
for (i in seq_along(files)) {
  # 读取数据
  data <- read.csv(files[i], header = TRUE)
  
  # 绘制点状图
  p <- ggplot(data, aes(x = AccessIndex, y = base_chunk_ID)) +
    geom_point(size = 1.5, color = "#AD0626") +
    theme_classic() +
    labs(x = "Access Index", y = "Basechunk ID", title = titles[i])
  
  # 保存为PDF
  ggsave(paste0(titles[i], ".pdf"), plot = p, width = 8, height = 4)
}