# 设置当前工作路径
setwd("C:/Users/YP/Desktop/work/R-paint/Paper/hotstorage26/challenge/2/2/version2")

library(ggplot2)

# 可选：手动设置y轴刻度（如不需要手动设置，设为NULL即可）
# y_breaks <- NULL
y_breaks <- c(0, 200, 400, 600) # 示例：手动设置

files <- c("linux.csv")

for (i in seq_along(files)) {
  data <- read.csv(files[i], header = TRUE)
  y_max <- max(data$base_chunk_ID, na.rm = TRUE) * 1.1

  # 自动或手动设置y轴刻度
  if (is.null(y_breaks)) {
    y_breaks_use <- pretty(c(0, y_max), n = 5)
  } else {
    y_breaks_use <- y_breaks
  }

  p <- ggplot(data, aes(x = AccessIndex, y = base_chunk_ID)) +
    geom_point(size = 1.5, color = "#AD0626") +
    theme_classic() +
    labs(x = "Access Index", y = "Chunk ID") +
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = y_breaks_use,
      expand = expansion(mult = c(0, 0.05))
    ) +
    theme(
      axis.text.x = element_text(size = 40),
      axis.text.y = element_text(size = 40),
      axis.title.x = element_text(size = 40),
      axis.title.y = element_text(size = 40)
    )

  ggsave(paste0(tools::file_path_sans_ext(files[i]), ".pdf"), plot = p, width = 8, height = 4)
}