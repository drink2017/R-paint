# 安装并加载必要的包
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("reshape2")) install.packages("reshape2")
library(openxlsx)
library(reshape2)

# 读取日志文件（相对路径）
file_path <- "Paper/impact/ImpactchunkInfoLog -B.txt"
if (!file.exists(file_path)) stop(paste("日志文件不存在：", file_path))
lines <- readLines(file_path)

# 初始化数据框
results <- data.frame(
  Dataset = character(),
  B_Parameter = integer(),
  Recipe_Overhead = numeric(),
  stringsAsFactors = FALSE
)

# 临时变量
current_dataset <- NA
current_b_param <- NA
current_overhead <- NA

# 遍历每一行，提取 -i 数据集、-B 参数以及 Recipe Overhead 值
for (i in seq_along(lines)) {
  line <- lines[i]
  
  # 提取 -i /mnt/dataset2/... 的数据集名
  if (grepl("-i /mnt/dataset2/", line)) {
    ds_match <- regmatches(line, regexpr("/mnt/dataset2/[^ ]+", line))
    if (length(ds_match) > 0) {
      current_dataset <- gsub(".*/", "", ds_match)
    }
    # 提取 -B 参数
    b_match <- regmatches(line, regexpr("-B [0-9]+", line))
    if (length(b_match) > 0) {
      current_b_param <- as.integer(sub("-B ", "", b_match))
    }
  }
  
  # 提取 Recipe Overhead: 后的数值（提取第一个数字）
  if (grepl("Recipe Overhead:", line, ignore.case = TRUE)) {
    num_match <- sub(".*Recipe Overhead:\\s*([0-9]+\\.?[0-9]*).*", "\\1", line, ignore.case = TRUE)
    if (nzchar(num_match) && grepl("[0-9]", num_match)) {
      current_overhead <- as.numeric(num_match)
      
      # 当三项都可用时写入结果
      if (!is.na(current_dataset) && !is.na(current_b_param) && !is.na(current_overhead)) {
        results <- rbind(results, data.frame(
          Dataset = current_dataset,
          B_Parameter = current_b_param,
          Recipe_Overhead = current_overhead,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# 打印结果摘要
print(results)
cat(paste("\n共提取记录数:", nrow(results), "\n"))

# 如果没有数据则退出
if (nrow(results) == 0) {
  cat("未发现 Recipe Overhead 数据，脚本结束。\n")
  quit(save = "no", status = 0)
}

# 创建数据透视表（行 = Dataset，列 = B 参数，值 = Recipe_Overhead 的平均值）
pivot_table <- dcast(results, Dataset ~ B_Parameter, value.var = "Recipe_Overhead", fun.aggregate = mean)

# 为列名添加前缀
colnames(pivot_table)[-1] <- paste0("B_", colnames(pivot_table)[-1])

# 保存到 Excel，包含透视表和原始数据
wb <- createWorkbook()
addWorksheet(wb, "Pivot Table")
addWorksheet(wb, "Raw Data")

writeData(wb, "Pivot Table", pivot_table)
writeData(wb, "Raw Data", results)

# 样式（与现有脚本保持一致）
headerStyle <- createStyle(fontColour = "#FFFFFF", fgFill = "#4F81BD",
                           halign = "center", valign = "center",
                           textDecoration = "bold", border = "TopBottomLeftRight")
dataStyle <- createStyle(halign = "center", valign = "center", border = "TopBottomLeftRight")
datasetStyle <- createStyle(halign = "left", valign = "center", border = "TopBottomLeftRight", textDecoration = "bold")

addStyle(wb, "Pivot Table", headerStyle, rows = 1, cols = 1:ncol(pivot_table), gridExpand = TRUE)
addStyle(wb, "Pivot Table", datasetStyle, rows = 2:(nrow(pivot_table)+1), cols = 1, gridExpand = TRUE)
if (ncol(pivot_table) >= 2) {
  addStyle(wb, "Pivot Table", dataStyle, rows = 2:(nrow(pivot_table)+1), cols = 2:ncol(pivot_table), gridExpand = TRUE, stack = TRUE)
}
addStyle(wb, "Raw Data", headerStyle, rows = 1, cols = 1:3, gridExpand = TRUE)

# 列宽与冻结窗格
setColWidths(wb, "Pivot Table", cols = 1, widths = 30)
if (ncol(pivot_table) >= 2) setColWidths(wb, "Pivot Table", cols = 2:ncol(pivot_table), widths = 15)
setColWidths(wb, "Raw Data", cols = 1:3, widths = c(25, 15, 20))

freezePane(wb, "Pivot Table", firstRow = TRUE)
freezePane(wb, "Raw Data", firstRow = TRUE)

addFilter(wb, "Pivot Table", rows = 1, cols = 1:ncol(pivot_table))
addFilter(wb, "Raw Data", rows = 1, cols = 1:3)

output_file_full <- "Paper/impact/Recipe_Overhead_results.xlsx"
saveWorkbook(wb, output_file_full, overwrite = TRUE)
cat(paste("\nRecipe Overhead 结果已保存到:", output_file_full, "\n"))

# 简要统计
cat("\n统计信息:\n")
cat(paste("- 数据集数量:", length(unique(results$Dataset)), "\n"))
cat(paste("- B 参数数量:", length(unique(results$B_Parameter)), "\n"))
cat(paste("- B 参数值:", paste(sort(unique(results$B_Parameter)), collapse = ", "), "\n"))