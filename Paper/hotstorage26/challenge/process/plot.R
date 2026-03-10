source("E:/Users/drinkwater/Desktop/R-script-template/MyR/Bar.R")

set.seed(123)
left_list <- list(c(64), c(1698), c(31), c(89))
right_parts_list <- list(
  list(411,175,76),
  list(5250,22701,1236),
  list(37502,17469,1176),
  list(9822,8772,1292)
)

p <- create_grouped_two_with_stacked_right(
  left_list = left_list,
  right_parts_list = right_parts_list,
  group_labels = c("G1","G2","G3","G4"),
  component_labels = c("p1","p2","p3"),
  left_fill = "#92C5DE",
  component_fills = c("#FB8072","#80B1D3","#BEBADA"),
  show_data_labels = TRUE,
  show_legend = FALSE,  # 添加这一行
  normalize_to_left = TRUE,  # 启用归一化
  export_name = "group2_stacked_right.png",
  export_path = "E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/process",
  width = 8, height = 5, dpi = 300
)

print(p)

ggsave(
  filename = "group2_stacked_right.png",
  plot = p,
  path = "E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/process",
  width = 8, height = 5, dpi = 300
)

