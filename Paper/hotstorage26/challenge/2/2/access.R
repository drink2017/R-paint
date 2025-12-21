# 载入绘图包
library(ggplot2)

# 读取数据
data <- read.csv("e:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/2/2/windowsLog.csv", header = TRUE)

# 绘制点状图
p <- ggplot(data, aes(x = AccessIndex, y = SF_ID)) +
  geom_point(size = 1.5, color = "#AD0626") +
  theme_classic() +
  labs(x = "access index", y = "basechunk ID", title = "windowsLog")

# 显示图形
print(p)

# 保存为PDF
ggsave("E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/2/2/windowsLog.pdf", plot = p, width = 8, height = 4)