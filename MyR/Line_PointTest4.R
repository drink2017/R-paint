if (!require("here")) install.packages("here")
library("here")

# 使用 here() 来构建相对于项目根目录的正确路径
source(here("MyR", "Line_Point.R"))

linux_data <- data.frame(
    Threshold = 2^(13:22), # 8K, 16K, ..., 4M
    OCR = 0.95 - log(2^(13:22)) * 0.03 + rnorm(10, 0, 0.01),
    ERR = 0.01 + log(2^(13:22)) * 0.005 + rnorm(10, 0, 0.002),
    RecipeSize = 1.5e9 - (2^(13:22)) * 100 + rnorm(10, 0, 5e6)
)
linux_data$OCR[linux_data$OCR > 1] <- 0.99 # 确保比率不超过1

# 示例：为 "Web" 数据集创建模拟数据
web_data <- data.frame(
    Threshold = 2^(13:22),
    OCR = 0.85 - log(2^(13:22)) * 0.025 + rnorm(10, 0, 0.015),
    ERR = 0.02 + log(2^(13:22)) * 0.006 + rnorm(10, 0, 0.003),
    RecipeSize = 2.5e9 - (2^(13:22)) * 150 + rnorm(10, 0, 8e6)
)
web_data$OCR[web_data$OCR > 1] <- 0.98

    plot_tradeoff_dual_y_axis(
    data = linux_data,
    dataset_name = "Linux",
    export_path = "./"
)

# 为 Web 数据集生成图表
plot_tradeoff_dual_y_axis(
    data = web_data,
    # dataset_name = "Web",
    export_path = "./"
)