# R脚本 - 处理zstd压缩/解压吞吐量数据
library(openxlsx)

# 设置工作目录为脚本所在目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat(sprintf("工作目录: %s\n\n", getwd()))

# 提取吞吐量
extract_throughput <- function(file_path) {
  if (!file.exists(file_path)) {
    cat(sprintf("  ✗ 文件不存在: %s\n", file_path))
    return(numeric(0))
  }
  
  lines <- readLines(file_path, warn = FALSE)
  header_idx <- grep("^filename,", lines)[1]
  if (is.na(header_idx)) return(numeric(0))
  
  data_lines <- lines[(header_idx + 1):length(lines)]
  data_lines <- grep("^Total_|^Mean_|^Std_|^95%|^$", data_lines, 
                     value = TRUE, invert = TRUE)
  
  if (length(data_lines) == 0) return(numeric(0))
  
  values <- sapply(strsplit(data_lines, ","), function(x) as.numeric(x[length(x)]))
  return(values[!is.na(values)])
}

# 数据集
datasets <- c("automake", "coreutils", "cpython", "gcc", 
              "linux", "netty", "react", "web")

# 处理压缩数据
cat("========================================\n")
cat("处理压缩数据 (Throughput.xlsx)\n")
cat("========================================\n")

compress_data <- lapply(datasets, function(d) {
  file_name <- paste0(d, "_compress_throughput.log")
  cat(sprintf("  读取 %s\n", file_name))
  extract_throughput(file_name)
})
names(compress_data) <- paste0("zstd_", datasets)

max_len <- max(sapply(compress_data, length))
compress_df <- as.data.frame(lapply(compress_data, function(x) {
  c(x, rep(NA, max_len - length(x)))
}))

write.xlsx(compress_df, "Throughput.xlsx", rowNames = FALSE)
cat(sprintf("\n✓ Throughput.xlsx: %d 列 × %d 行\n", ncol(compress_df), nrow(compress_df)))

# 处理解压数据
cat("========================================\n")
cat("处理解压数据 (Restore.xlsx)\n")
cat("========================================\n")

decompress_data <- lapply(datasets, function(d) {
  file_name <- paste0(d, "_decompress_throughput.log")
  cat(sprintf("  读取 %s\n", file_name))
  extract_throughput(file_name)
})
names(decompress_data) <- paste0("zstd_", datasets)

max_len <- max(sapply(decompress_data, length))
decompress_df <- as.data.frame(lapply(decompress_data, function(x) {
  c(x, rep(NA, max_len - length(x)))
}))

write.xlsx(decompress_df, "Restore.xlsx", rowNames = FALSE)
cat(sprintf("\n✓ Restore.xlsx: %d 列 × %d 行\n\n", ncol(decompress_df), nrow(decompress_df)))

cat("========================================\n")
cat("完成！\n")
cat("========================================\n")