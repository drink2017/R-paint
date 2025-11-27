# 安装并加载必要的包
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("reshape2")) install.packages("reshape2")
library(openxlsx)
library(reshape2)

# 读取文本文件
file_path <- "Paper/impact/ImpactchunkInfoLog -B.txt"
lines <- readLines(file_path)

# 初始化数据框
results <- data.frame(
  Dataset = character(),
  B_Parameter = integer(),
  Overall_Compression_Ratio = numeric(),
  stringsAsFactors = FALSE
)

# 临时变量
current_dataset <- NA
current_b_param <- NA
current_ratio <- NA

# 遍历每一行
for (i in 1:length(lines)) {
  line <- lines[i]
  
  # 提取-i参数（数据集名称）
  if (grepl("-i /mnt/dataset2/", line)) {
    # 提取数据集名称
    dataset_match <- regmatches(line, regexpr("/mnt/dataset2/[^ ]+", line))
    if (length(dataset_match) > 0) {
      # 移除路径前缀，只保留数据集名称
      current_dataset <- gsub(".*/", "", dataset_match)
    }
    
    # 提取-B参数
    b_match <- regmatches(line, regexpr("-B [0-9]+", line))
    if (length(b_match) > 0) {
      current_b_param <- as.integer(sub("-B ", "", b_match))
    }
  }
  
  # 提取Overall Compression Ratio
  if (grepl("^Overall Compression Ratio:", line)) {
    ratio_match <- regmatches(line, regexpr("[0-9]+\\.?[0-9]*$", line))
    if (length(ratio_match) > 0) {
      current_ratio <- as.numeric(ratio_match)
      
      # 当三个值都收集到后，添加到结果中
      if (!is.na(current_dataset) && !is.na(current_b_param) && !is.na(current_ratio)) {
        results <- rbind(results, data.frame(
          Dataset = current_dataset,
          B_Parameter = current_b_param,
          Overall_Compression_Ratio = current_ratio,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# 查看结果
print(results)
print(paste("总共提取了", nrow(results), "条记录"))

# 创建数据透视表：数据集为行，B参数为列
pivot_table <- dcast(results, Dataset ~ B_Parameter, value.var = "Overall_Compression_Ratio")

# 为列名添加"B_"前缀，使其更清晰
colnames(pivot_table)[-1] <- paste0("B_", colnames(pivot_table)[-1])

# 查看透视表
print(pivot_table)

# 保存包含两个工作表的Excel文件
wb <- createWorkbook()
addWorksheet(wb, "Pivot Table")
addWorksheet(wb, "Raw Data")

# 先写入透视表（主要数据）
writeData(wb, "Pivot Table", pivot_table)
writeData(wb, "Raw Data", results)

# 添加表头样式
headerStyle <- createStyle(
  fontColour = "#FFFFFF", 
  fgFill = "#4F81BD",
  halign = "center", 
  valign = "center", 
  textDecoration = "bold", 
  border = "TopBottomLeftRight"
)

# 添加数据单元格样式
dataStyle <- createStyle(
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

# 数据集名称列样式（左对齐）
datasetStyle <- createStyle(
  halign = "left",
  valign = "center",
  border = "TopBottomLeftRight",
  textDecoration = "bold"
)

# 应用样式到透视表
addStyle(wb, "Pivot Table", headerStyle, rows = 1, cols = 1:ncol(pivot_table), gridExpand = TRUE)
addStyle(wb, "Pivot Table", datasetStyle, rows = 2:(nrow(pivot_table)+1), cols = 1, gridExpand = TRUE)
addStyle(wb, "Pivot Table", dataStyle, rows = 2:(nrow(pivot_table)+1), cols = 2:ncol(pivot_table), gridExpand = TRUE, stack = TRUE)

# 应用样式到原始数据
addStyle(wb, "Raw Data", headerStyle, rows = 1, cols = 1:3, gridExpand = TRUE)

# 设置列宽
setColWidths(wb, "Pivot Table", cols = 1, widths = 30)  # Dataset列宽
setColWidths(wb, "Pivot Table", cols = 2:ncol(pivot_table), widths = 15)  # B参数列宽
setColWidths(wb, "Raw Data", cols = 1:3, widths = c(25, 15, 25))

# 冻结首行（表头）
freezePane(wb, "Pivot Table", firstRow = TRUE)
freezePane(wb, "Raw Data", firstRow = TRUE)

# 添加筛选功能
addFilter(wb, "Pivot Table", rows = 1, cols = 1:ncol(pivot_table))
addFilter(wb, "Raw Data", rows = 1, cols = 1:3)

# 保存工作簿
output_file_full <- "Paper/impact/impact-B-OCR.xlsx"
saveWorkbook(wb, output_file_full, overwrite = TRUE)
cat(paste("\n完整结果已保存到:", output_file_full, "\n"))

# 输出统计信息
cat("\n数据集统计:\n")
cat(paste("- 数据集数量:", length(unique(results$Dataset)), "\n"))
cat(paste("- B参数数量:", length(unique(results$B_Parameter)), "\n"))
cat(paste("- B参数值:", paste(sort(unique(results$B_Parameter)), collapse = ", "), "\n"))