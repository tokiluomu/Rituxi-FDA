# 加载所需的库
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(readxl)

# 加载数据
TRA_CVARD <- read_excel("TRA_CANADA.xlsx")
TRA_JADER <- read_excel("TRA_JADER.xlsx")
TRA_FAERS <- read_excel("TRA_FAERS.xlsx")

# 添加数据来源列
TRA_CVARD$Source <- "CVARD"
TRA_JADER$Source <- "JADER"
TRA_FAERS$Source <- "FAERS"

# 为每个数据库的数据添加来源前缀
TRA_CVARD$pt_name_en <- paste("CVARD:", TRA_CVARD$pt_name_en)
TRA_JADER$pt_name_en <- paste("JADER:", TRA_JADER$pt_name_en)
TRA_FAERS$pt_name_en <- paste("FAERS:", TRA_FAERS$pt_name_en)

# 合并数据
all_data <- rbind(
  TRA_CVARD[, c(1, 6:11)],
  TRA_JADER[, c(1, 6:11)],
  TRA_FAERS[, c(1, 6:11)]
)

all_data_value <- rbind(
  TRA_CVARD[, c(1:5,11)],
  TRA_JADER[, c(1:5,11)],
  TRA_FAERS[, c(1:5,11)]
)

colnames(all_data_value) <- c("pt_name_en", "ROR", "PRR", "MGPS", "BCPNN", "Source")

# 数据转换为长格式
data_long <- melt(all_data, id.vars = c("pt_name_en", "Source"))           # 信号数据
data_value_long <- melt(all_data_value, id.vars = c("pt_name_en", "Source")) # 值数据

# 修改列名
colnames(data_long) <- c("pt_name_en", "Source", "variable", "value")
colnames(data_value_long) <- c("pt_name_en", "Source", "variable", "value")

# 将`value`转换为因子以便使用离散调色板
data_long$value <- as.factor(data_long$value)

# 绘制热图
ggplot(data_long, aes(x = variable, y = pt_name_en, fill = interaction(Source, value))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c(
    "CVARD.0" = "#BEE9F5", "CVARD.1" = "#457B9D",
    "JADER.0" = "#ADF5D7", "JADER.1" = "#2A9D8E",
    "FAERS.0" = "#FCBEB0", "FAERS.1" = "#E73847"),
    labels = c("JADER: Negative", "JADER: Positive", 
               "FAERS: Negative", "FAERS: Positive", 
               "CVARD: Negative", "CVARD: Positive"),
    name = "Signal",
    breaks = c("JADER.0", "JADER.1", 
               "FAERS.0", "FAERS.1", 
               "CVARD.0", "CVARD.1"))+
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold")) +  
  labs(x = "Signal Algorithm", y = "PT Name") +
  geom_text(data = data_value_long, aes(label = value), color = "black", size = 3, na.rm = TRUE)

# 保存图像
ggsave('TRA_combined_heatmap.png', device = "png", dpi = 600, width = 13, height = 10, units = "in")
