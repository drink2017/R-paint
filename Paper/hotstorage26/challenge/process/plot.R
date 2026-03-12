source("C:/Users/YP/Desktop/work/R-paint/MyR/Bar.R")

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
  group_labels = c("Glibc","Linux","Web","Win.Log"),
  component_labels = c("p1","p2","p3"),
  show_data_labels = TRUE,
  data_label_size = 10,
  data_label_angle = 90,       # 改为竖排
  data_label_hjust = 0.5,      # 水平居中
  data_label_vjust = 0.5,      # 竖直微调（可试 -0.5 / 0.5 看视觉效果）
  label_margin_frac = 0.05,
  axis_text_size = 12,
  axis_title_size = 14,
  show_legend = FALSE,
  bar_width = 0.40
)

# 覆盖主题：刻度文字 10pt，轴标题 14pt，图例文字 10pt
p <- p + theme(
  axis.text.x = element_text(size = 32),
  axis.text.y = element_text(size = 32),
  axis.title.x = element_text(size = 32),
  axis.title.y = element_text(size = 32),
  legend.text = element_text(size = 10)
)

# 保存为 PDF（建议使用 cairo_pdf 以保证字体/向量输出）
cairo_pdf("C:/Users/YP/Desktop/work/R-paint/Paper/hotstorage26/challenge/process/process.pdf", width = 12, height = 6)
print(p)
dev.off()

