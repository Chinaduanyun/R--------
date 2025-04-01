# 加载必要的包
library(ggplot2)
library(ggthemes)
library(showtext)
library(ggpmisc) # 用于添加回归方程

# 设置中文字体
font_add("SimSun", "/System/Library/Fonts/Supplemental/Songti.ttc")
showtext_auto()

# 获取用户输入
# 获取X轴标题
cat("请输入X轴标题：\n")
x_title <- readline()

cat("请输入X轴数据（英文逗号分隔）：\n")
x_input <- readline()
x_data <- as.numeric(unlist(strsplit(x_input, ",")))


# 获取Y轴标题
cat("请输入Y轴标题：\n")
y_title <- readline()

cat("请输入Y轴数据（英文逗号分隔）：\n")
y_input <- readline()
y_data <- as.numeric(unlist(strsplit(y_input, ",")))

# 获取图表标题（可选）
cat("请输入图表标题（可选，直接按Enter跳过）：\n")
plot_title <- readline()

# 创建数据框
data <- data.frame(
  x = x_data,
  y = y_data
)

# 计算数据范围用于自动调整坐标轴
x_range <- range(data$x, na.rm = TRUE)
y_range <- range(data$y, na.rm = TRUE)

# 自动计算刻度间隔的优化版本
calculate_breaks <- function(range) {
  span <- diff(range)
  if (span <= 0) return(range)  # 防止除零错误
  
  # 计算数量级
  power <- floor(log10(span))
  magnitude <- 10^power
  
  # 根据数量级确定合适的间隔
  normalized_span <- span / magnitude
  
  if (normalized_span <= 1.5) {
    by <- 0.2 * magnitude
  } else if (normalized_span <= 3) {
    by <- 0.5 * magnitude
  } else if (normalized_span <= 7) {
    by <- 1 * magnitude
  } else {
    by <- 2 * magnitude
  }
  
  # 确保刻度从合适的值开始
  start <- floor(range[1] / by) * by
  end <- ceiling(range[2] / by) * by
  
  seq(start, end, by = by)
}



x_breaks <- calculate_breaks(x_range)
y_breaks <- calculate_breaks(y_range)

# 计算回归模型用于获取方程参数
model <- lm(y ~ x, data = data)

# 创建基本图形
p <- ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 3, shape = 21, fill = "steelblue", color = "white", stroke = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", fill = "lightgray", alpha = 0.2,
              fullrange = TRUE) + # fullrange使回归线延伸至边界
  
  # 添加回归方程（显示在图的左上角）
stat_poly_eq(
  formula = y ~ x,
  aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "*\", \"*")),
  parse = TRUE,
  size = 4,
  label.x = 0.05,  # 左侧5%位置
  label.y = 0.95,   # 顶部95%位置
  coef.digits = 2,  # 系数保留2位小数
  rr.digits = 2,    # R²保留2位小数
  small.r = FALSE,  # 不使用科学计数法显示小数字
  small.p = FALSE   # 同上
) +
  
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "SimSun"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 16, face = "plain"),
    axis.text = element_text(size = 10, color = "#000000"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  
  scale_y_continuous(
    limits = c(min(y_breaks), max(y_breaks)),
    breaks = y_breaks,
    expand = expansion(mult = c(0, 0.10)) # 顶部留5%空间给方程
  ) +
  
  scale_x_continuous(
    limits = c(min(x_breaks), max(x_breaks)),
    breaks = x_breaks,
    expand = expansion(mult = c(0, 0.10)) # 右侧留5%空间
  )

# 添加标题和轴标签
if (nchar(plot_title) > 0) {
  p <- p + labs(
    x = x_title,
    y = y_title,
    title = plot_title
  )
} else {
  p <- p + labs(
    x = x_title,
    y = y_title
  )
}

# 显示图形
print(p)

# 获取当前时间并格式化为字符串
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")

# 构造文件名
file_name <- paste0("pic_", current_time, ".png")

# 保存图片
ggsave(file_name, plot = p, width = 6, height = 5, dpi = 300)

# 打印保存信息
cat("图片已保存为", file_name, "\n")
