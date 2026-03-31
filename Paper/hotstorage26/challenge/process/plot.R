source("E:/Users/drinkwater/Desktop/R-script-template/MyR/Bar.R")

legend_breaks <- c("p1", "Left", "p2", "p3")
legend_labels <- c(
  "Trial delta compression",
  "Real",
  "Base chunk reconstruction",
  "Others"
)
legend_values <- c(
  Left = "#2E2B54",
  p1 = "#A61D24",
  p2 = "#78B0B8",
  p3 = "#B196C1"
)

left_list <- list(c(64), c(1698), c(31), c(89))
right_parts_list <- list(
  list(c(410), c(185), c(74)),
  list(c(5269), c(26213), c(1126)),
  list(c(37552), c(18254), c(772)),
  list(c(9793), c(9491), c(713))
)

p <- create_grouped_two_with_stacked_right(
  left_list = left_list,
  right_parts_list = right_parts_list,
  group_labels = c("Glibc","Linux","Web*","Log"),
  component_labels = c("p1","p2","p3"),
  show_data_labels = TRUE,
  data_label_size = 10,
  data_label_angle = 0,       # 改为竖排
  data_label_hjust = 0.5,      # 水平居中
  data_label_vjust = 0.5,      # 竖直微调（可试 -0.5 / 0.5 看视觉效果）
  label_margin_frac = 0.05,
  axis_text_size = 12,
  axis_title_size = 14,
  show_legend = TRUE,
  legend_text_size = 32,
  bar_width = 0.40
)

# 定义科学计数法标签函数
fancy_scientific <- function(x) {
  sapply(x, function(val) {
    if (is.na(val) || val == 0) {
      return(expression(0))
    }
    exponent  <- floor(log10(abs(val)))
    mantissa  <- round(val / 10^exponent, 2)
    if (mantissa == 1) {
      parse(text = paste0("10^", exponent))[[1]]
    } else {
      parse(text = paste0(mantissa, " %*% 10^", exponent))[[1]]
    }
  })
}

p <- p + scale_y_continuous(
  labels = fancy_scientific,
  expand = expansion(mult = c(0, 0.12))
)

p <- p + scale_fill_manual(
  name = NULL,
  values = legend_values,
  breaks = legend_breaks,
  labels = legend_labels,
  guide = guide_legend(
    ncol = 1,
    byrow = TRUE,
    keyheight = grid::unit(18, "pt"),
    keywidth = grid::unit(18, "pt")
  )
)

# 覆盖主题：刻度文字 10pt，轴标题 14pt，图例文字 10pt
p <- p + theme(
  axis.text.x = element_text(size = 32),
  axis.text.y = element_text(size = 32),
  axis.title.x = element_text(size = 32),
  axis.title.y = element_text(size = 32),
  legend.position = c(0.03, 1.01),
  legend.justification = c(0, 1),
  legend.direction = "vertical",
  legend.text = element_text(size = 32, color = "black"),
  legend.title = element_blank(),
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.spacing.y = grid::unit(8, "pt"),
  plot.margin = margin(t = 18, r = 12, b = 12, l = 12, unit = "pt")
)

# 保存为 PDF（建议使用 cairo_pdf 以保证字体/向量输出）
cairo_pdf("E:/Users/drinkwater/Desktop/R-script-template/Paper/hotstorage26/challenge/process/process.pdf", width = 12, height = 5)
print(p)
dev.off()

