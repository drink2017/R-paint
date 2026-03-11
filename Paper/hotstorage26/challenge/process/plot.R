source("C:\\Users\\YP\\Desktop\\work\\R-paint\\MyR\\Bar.R")

left_list <- list(c(64), c(1698), c(31), c(89))
right_parts_list <- list(
  list(c(411), c(175), c(76)),
  list(c(5250), c(22701), c(1236)),
  list(c(37502), c(17469), c(1176)),
  list(c(9822), c(8772), c(1292))
)

p <- create_grouped_two_with_stacked_right(
  left_list = left_list,
  right_parts_list = right_parts_list,
  group_labels = c("Glibc","Linux","Web","Win.Log"),
  component_labels = c("p1","p2","p3"),
  left_fill = "#6BAED6",
  component_fills = c("#E41A1C","#4DAF4A","#984EA3"),
  show_data_labels = TRUE,
  show_legend = FALSE,
  use_y_break = TRUE,
  break_start = 2500,
  break_end = 8000,
  break_scales = 0.5,
  left_right_offset = 0.14,
  export_name = "group2_stacked_right_break.pdf",
  export_path = "C:\\Users\\YP\\Desktop\\work\\R-paint\\Paper\\hotstorage26\\challenge\\process\\",
  width = 8,
  height = 5,
  device = cairo_pdf,
  axis_text_size = 16,
  text_size = 5
)

print(p)

