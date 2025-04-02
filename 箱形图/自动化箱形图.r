# 加载必要的包
library(ggplot2)
library(ggthemes)
library(showtext)
library(tidyr)
library(dplyr)

# 设置中文字体
font_add("SimSun", "/System/Library/Fonts/Supplemental/Songti.ttc")
showtext_auto()

# 获取用户输入实验设计类型
cat("请选择实验设计类型：\n1: 被试内变量\n2: 被试间变量\n")
design_type <- as.numeric(readline())

if(design_type == 1) {
  # 被试内设计
  cat("请输入需要比较的变量数量：\n")
  var_count <- as.numeric(readline())
  
  # 创建空列表存储数据
  all_data <- list()
  var_names <- character(var_count)
  max_length <- 0
  
  for(i in 1:var_count) {
    cat(sprintf("请输入第%d个变量的名称：\n", i))
    var_name <- trimws(readline())
    if(nchar(var_name) == 0) {  # 若用户未输入名称，自动赋默认名称
      var_name <- paste0("Var", i)
    }
    var_names[i] <- var_name
    
    cat(sprintf("请输入第%d个变量的数据（可用逗号、空格或回车分隔）：\n", i))
    input <- readline()
    if(grepl(",", input)) {
      data <- as.numeric(trimws(unlist(strsplit(input, ","))))
    } else if(grepl(" ", input)) {
      data <- as.numeric(trimws(unlist(strsplit(input, " "))))
    } else {
      data <- as.numeric(trimws(input))
    }
    all_data[[ var_name ]] <- data
    max_length <- max(max_length, length(data))
  }
  
  for(name in var_names) {
    length(all_data[[name]]) <- max_length
  }
  
  df <- as.data.frame(all_data)
  
  df_long <- tidyr::pivot_longer(
    df,
    cols = everything(),
    names_to = "Variable",
    values_to = "Value",
    values_drop_na = TRUE
  )
  
  df_long$Variable <- factor(df_long$Variable, levels = var_names)
  
  cat("请输入X轴标签：\n")
  x_label <- readline()
  cat("请输入Y轴标签：\n")
  y_label <- readline()
  
  p <- ggplot(df_long, aes(x = Variable, y = Value, fill = Variable)) +
    geom_boxplot(alpha = 0.7) +
    stat_boxplot(geom = "errorbar", width = 0.2)
  
} else if(design_type == 2) {
  # 被试间设计
  cat("请输入X轴标签：\n")
  x_label <- readline()
  cat("请输入X轴分组数据（用逗号分隔）：\n")
  x_input <- readline()
  x_data <- unlist(strsplit(x_input, ","))
  
  cat("请输入Y轴标签：\n")
  y_label <- readline()
  cat("请输入Y轴数值数据（用逗号分隔）：\n")
  y_input <- readline()
  y_data <- as.numeric(unlist(strsplit(y_input, ",")))
  
  df <- data.frame(
    Group = x_data,
    Value = y_data
  )
  
  p <- ggplot(df, aes(x = Group, y = Value, fill = Group)) +
    geom_boxplot(alpha = 0.7) +
    stat_boxplot(geom = "errorbar", width = 0.2)
}

# 添加通用的主题设置
p <- p +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "SimSun"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "plain"),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(x = x_label, y = y_label) +
  scale_fill_brewer(palette = "Set2")

# 自动调整Y轴范围
y_range <- layer_scales(p)$y$range$range
y_breaks <- pretty(y_range, n = 8)
p <- p + scale_y_continuous(
  limits = range(y_breaks),
  breaks = y_breaks,
  expand = expansion(mult = c(0.05, 0.05))
)

# 显示图形
print(p)

# 保存图片
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
file_name <- paste0("boxplot_", current_time, ".png")
ggsave(file_name, plot = p, width = 6, height = 5, dpi = 300)
cat("图片已保存为", file_name, "\n")
