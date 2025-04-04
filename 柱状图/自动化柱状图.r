
# 加载必要的包
library(ggplot2)
library(ggthemes)
library(showtext)
library(dplyr)

# 设置中文字体
font_add("SimSun", "/System/Library/Fonts/Supplemental/Songti.ttc")
showtext_auto()

# 获取实验设计类型
cat("请输入实验设计类型（1:被试内设计, 2:被试间设计）：\n")
design_type <- as.numeric(readline())

if(design_type == 1) {
    # 获取变量数量
    cat("请输入变量数量：\n")
    var_count <- as.numeric(readline())
    
    # 创建空列表存储数据和标签
    all_data <- list()
    var_labels <- character(var_count)
    
    # 添加数据处理函数
    process_input_data <- function(input_str) {
        # 首先按回车分割
        lines <- unlist(strsplit(input_str, "\n"))
        # 合并所有行
        combined <- paste(lines, collapse = " ")
        # 将连续的多个空格替换为单个空格
        cleaned <- gsub("\\s+", " ", combined)
        # 将逗号和空格都替换为单个空格
        cleaned <- gsub(",", " ", cleaned)
        # 分割字符串
        values <- unlist(strsplit(cleaned, " "))
        # 移除空字符串
        values <- values[values != ""]
        # 转换为数值
        as.numeric(values)
    }
    
    # 修改数据输入部分
    for(i in 1:var_count) {
        cat(sprintf("请输入第%d个变量的标签名称：\n", i))
        var_labels[i] <- readline()
        cat(sprintf("请输入第%d个变量的数据（可用英文逗号、空格或回车分隔）：\n", i))
        input_data <- readLines(n = 1)
        all_data[[i]] <- process_input_data(input_data)
    }
    
    # 获取轴标签
    cat("请输入X轴标签：\n")
    x_label <- readline()
    cat("请输入Y轴标签：\n")
    y_label <- readline()
    # 修改y轴标签为竖排显示：每个字符一行
    y_label <- paste(strsplit(y_label, "")[[1]], collapse="\n")
    # 数据标签选项
    cat("请选择数据标签显示方式:\n0:不显示\n1:只显示均值\n2:只显示标准差\n3:均值和标准差\n")
    label_type <- as.numeric(readline())
    
    # 误差棒选项
    cat("请选择误差棒类型：\n")
    cat("1: ±1.96SD\n2: ±2.58SD\n3: ±3SD\n4: ±2SD\n5: ±1SD\n6: 不显示\n")
    error_type <- as.numeric(readline())
    # 新增：选择是否需要增加显著性标志
    cat("请选择是否需要增加显著性标志（0: 不增加, 1: 增加）：\n")
    sig_choice <- as.numeric(readline())
    
    # Y轴最大值（可选）
    cat("请输入Y轴最大值（直接按Enter则自动确定）：\n")
    y_max_input <- readline()
    
    # 数据处理
    df_list <- list()
    for(i in 1:var_count) {
        means <- mean(all_data[[i]], na.rm = TRUE)
        sds <- sd(all_data[[i]], na.rm = TRUE)
        df_list[[i]] <- data.frame(
            condition = i,
            mean = means,
            sd = sds,
            label_name = var_labels[i]
        )
    }
    plot_data <- do.call(rbind, df_list)
    
    # 计算误差棒
    error_multiplier <- switch(error_type,
        1.96,  # ±1.96SD
        2.58,  # ±2.58SD
        3,     # ±3SD
        2,     # ±2SD
        1,     # ±1SD
        0      # 不显示
    )
    
    # 创建标签
    plot_data$label <- switch(label_type + 1,
        "",  # 0: 不显示
        sprintf("%.2f", plot_data$mean),  # 1: 只显示均值
        sprintf("SD=%.2f", plot_data$sd),  # 2: 只显示标准差
        sprintf("%.2f\nSD=%.2f", plot_data$mean, plot_data$sd)  # 3: 均值和标准差
    )
    
    # 设置Y轴范围
    if(nchar(y_max_input) > 0) {
        y_max <- as.numeric(y_max_input)
    } else {
        y_max <- max(plot_data$mean + error_multiplier * plot_data$sd, na.rm = TRUE) * 1.1
    }
    
    # 显著性检验结果存储
    sig_results <- NULL
    
    # 创建图形
    p <- ggplot(plot_data, aes(x = label_name, y = mean)) +
        geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +  # 修改：柱宽0.5
        {if(error_type != 6) geom_errorbar(
            aes(ymin = mean - error_multiplier * sd,
                ymax = mean + error_multiplier * sd),
            width = 0.2
        )} +
        geom_text(aes(label = label), vjust = -0.5) +
        scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +  # 确保柱子从0开始
        labs(x = x_label, y = y_label) +
        theme_classic(base_size = 12) +
        theme(
            text = element_text(family = "SimSun"),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12),
            axis.title.y = element_text(lineheight = 0.8, 
                                        angle = 0,  # 竖直显示
                                        vjust = 0.5),
            axis.title.x = element_text(vjust = -0.5),  # 调整X轴标签位置
            axis.text.x = element_text(hjust = 0.5)  # 确保X轴文字居中对齐
        )
    
 # 显著性标记部分
    if(sig_choice == 1) {
        # 进行两两配对t检验并存储显著结果
        for(i in 1:(var_count-1)) {
            for(j in (i+1):var_count) {
                # 配对t检验前验证数据
                if(length(all_data[[i]]) != length(all_data[[j]])) {
                    warning(paste("跳过比较", var_labels[i], "和", var_labels[j],
                                "因为数据长度不一致(需要相同长度进行配对检验)"))
                    next
                }
                if(sd(all_data[[i]]) == 0 || sd(all_data[[j]]) == 0) {
                    warning(paste("跳过比较", var_labels[i], "和", var_labels[j],
                                "因为至少有一组数据是恒量"))
                    next
                }
                tryCatch({
                    test_result <- t.test(all_data[[i]], all_data[[j]], paired = TRUE)
                    p_val <- test_result$p.value
                }, error = function(e) {
                    warning(paste("比较", var_labels[i], "和", var_labels[j],
                                "时出错:", e$message))
                    return(NULL)
                })
                if(is.null(p_val)) next
                
                if(p_val < 0.05) {  # 只处理显著结果
                    # 确定显著性标记
                    sig_stars <- ifelse(p_val < 0.001, "***",
                                      ifelse(p_val < 0.01, "**",
                                            ifelse(p_val < 0.05, "*", "")))
                    
                    # 计算标记位置
                    y_base <- max(c(
                        plot_data$mean[i] + error_multiplier * plot_data$sd[i],
                        plot_data$mean[j] + error_multiplier * plot_data$sd[j]
                    )) * 1.1
                    y_line <- y_base + 0.05 * y_max
                    y_text <- y_line + 0.02 * y_max
                    
                    # 添加到图形
                    p <- p +
                        # 左侧竖线
                        annotate("segment",
                               x = i, xend = i,
                               y = y_base, yend = y_line,
                               size = 0.5) +
                        # 右侧竖线
                        annotate("segment",
                               x = j, xend = j,
                               y = y_base, yend = y_line,
                               size = 0.5) +
                        # 横线
                        annotate("segment",
                               x = i, xend = j,
                               y = y_line, yend = y_line,
                               size = 0.5) +
                        # 星号标记
                        annotate("text",
                               x = mean(c(i, j)),
                               y = y_text,
                               label = sig_stars,
                               size = 4)
                }
            }
        }
    }
    
    # 显示图形
    print(p)
    
    # 保存图形
    current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_name <- paste0("barplot_", current_time, ".png")
    ggsave(file_name, plot = p, width = 6, height = 6, dpi = 300)
    cat("图片已保存为", file_name, "\n")

} else if(design_type == 2) {
    cat("被试间设计功能尚未开发\n")
}
