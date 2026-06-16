source("C:/Users/YP/Desktop/work/R-paint/MyR/Bar.R")

args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grepl("^--file=", args)]
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = FALSE))
} else if (!is.null(sys.frames()[[1]]$ofile)) {
  dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = FALSE))
} else {
  getwd()
}

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

left_list <- list(c(3944), c(1698), c(31), c(313))
right_parts_list <- list(
  list(c(5559), c(5533), c(2052)),
  list(c(5269), c(26213), c(1126)),
  list(c(37552), c(18254), c(772)),
  list(c(15568), c(20148), c(906))
)

p <- create_grouped_two_with_stacked_right(
  left_list = left_list,
  right_parts_list = right_parts_list,
  group_labels = c("Docker","Linux","Web","Log"),
  component_labels = c("p1","p2","p3"),
  x_label = NULL,
  y_label = "Time (s)",
  show_data_labels = TRUE,
  data_label_size = 19,
  data_label_angle = 0,       # 改为竖排
  data_label_hjust = 0,
  data_label_vjust = 0.5,
  label_margin_frac = 0.00,
  data_label_voffset = 0.03,
  axis_text_size = 12,
  axis_title_size = 14,
  show_legend = TRUE,
  legend_text_size = 32,
  bar_width = 0.40,
  swap_pair_positions = TRUE
)

# 定义科学计数法标签函数
fancy_scientific <- function(x) {
  superscript_digits <- c(
    "0" = "\u2070", "1" = "\u00b9", "2" = "\u00b2", "3" = "\u00b3",
    "4" = "\u2074", "5" = "\u2075", "6" = "\u2076", "7" = "\u2077",
    "8" = "\u2078", "9" = "\u2079", "-" = "\u207b"
  )
  to_superscript <- function(value) {
    chars <- strsplit(as.character(value), "", fixed = TRUE)[[1]]
    paste0(superscript_digits[chars], collapse = "")
  }

  sapply(x, function(val) {
    if (is.na(val) || val == 0) {
      return("0")
    }
    exponent  <- floor(log10(abs(val)))
    mantissa  <- round(val / 10^exponent, 2)
    exponent_label <- to_superscript(exponent)
    if (mantissa == 1) {
      paste0("10", exponent_label)
    } else {
      paste0(mantissa, "\u00d710", exponent_label)
    }
  })
}

p <- p + scale_y_continuous(
  labels = fancy_scientific,
  breaks = c(20000, 40000, 60000),
  expand = expansion(mult = c(0, 0.30))
)

p <- p + coord_flip(clip = "off")

p <- p + scale_fill_manual(
  name = NULL,
  values = legend_values,
  breaks = legend_breaks,
  labels = legend_labels,
  guide = guide_legend(
    nrow = 2,
    byrow = TRUE,
    keyheight = grid::unit(18, "pt"),
    keywidth = grid::unit(18, "pt")
  )
)

# 覆盖主题：刻度文字 10pt，轴标题 14pt，图例文字 10pt
p <- p + theme(
  axis.text.x = element_text(size = 54),
  axis.text.y = element_text(size = 54),
  axis.title.x = element_text(size = 54),
  axis.title.y = element_text(size = 54),
  legend.position = "top",
  legend.justification = "left",
  legend.box.just = "left",
  legend.direction = "horizontal",
  legend.text = element_text(size = 52, color = "black"),
  legend.title = element_blank(),
  legend.background = element_blank(),
  legend.key = element_blank(),

  legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
  legend.box.spacing = grid::unit(0, "pt"),

  legend.spacing.y = grid::unit(8, "pt"),
  plot.margin = margin(t = 6, r = 12, b = 12, l = 12, unit = "pt")
)

# 保存为 PDF（建议使用 cairo_pdf 以保证字体/向量输出）
get_legend_grob <- function(plot) {
  plot_grob <- ggplot2::ggplotGrob(plot)
  legend_index <- which(vapply(plot_grob$grobs, function(grob) grob$name, character(1)) == "guide-box")
  if (length(legend_index) == 0) {
    return(grid::nullGrob())
  }
  plot_grob$grobs[[legend_index[1]]]
}

legend_grob <- get_legend_grob(p)
legend_area_height <- grid::unit(1.55, "in")
legend_y_offset <- grid::unit(-0.08, "in")

main_plot <- p + theme(
  legend.position = "none",
  plot.margin = margin(t = 0, r = 36, b = 12, l = 12, unit = "pt")
)

cairo_pdf(file.path(script_dir, "time_overhead.pdf"), width = 12, height = 9)
grid::grid.newpage()

plot_layout <- grid::grid.layout(
  nrow = 2,
  ncol = 1,
  heights = grid::unit.c(legend_area_height, grid::unit(1, "null"))
)
grid::pushViewport(grid::viewport(layout = plot_layout))

grid::pushViewport(grid::viewport(
  layout.pos.row = 1,
  layout.pos.col = 1
))
grid::pushViewport(grid::viewport(
  x = grid::unit(0.02, "npc"),
  y = grid::unit(1, "npc") + legend_y_offset,
  width = sum(legend_grob$widths),
  height = sum(legend_grob$heights),
  just = c("left", "top")
))
grid::grid.draw(legend_grob)
grid::popViewport(2)

print(main_plot, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
grid::popViewport()
dev.off()