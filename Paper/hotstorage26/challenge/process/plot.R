# 先 source 保存了函数的文件
source("C:/Users/YP/Desktop/work/R-paint/MyR/Bar.R")

set.seed(123)
# 模拟数据：4 组
left_list <- list(rnorm(20, 5, 1), rnorm(18, 6, 1.1), rnorm(22, 4.5, 0.9), rnorm(16, 6.2, 1.0))
right_parts_list <- list(
  list(rnorm(20,1,0.2), rnorm(20,2,0.3), rnorm(20,1.5,0.25)),
  list(rnorm(18,1.2,0.25), rnorm(18,2.1,0.3), rnorm(18,1.6,0.3)),
  list(rnorm(22,0.8,0.2), rnorm(22,1.8,0.25), rnorm(22,1.4,0.2)),
  list(rnorm(16,1.5,0.3), rnorm(16,2.2,0.35), rnorm(16,1.7,0.25))
)

p <- create_grouped_two_with_stacked_right(
  left_list = left_list,
  right_parts_list = right_parts_list,
  group_labels = c("G1","G2","G3","G4"),
  component_labels = c("p1","p2","p3"),
  left_fill = "#92C5DE",
  component_fills = c("#FB8072","#80B1D3","#BEBADA"),
  show_data_labels = TRUE,
  export_name = "group2_stacked_right.png",
  export_path = "C:/Users/YP/Desktop/work/R-paint/outputs",
  width = 8, height = 5, dpi = 300
)
print(p)