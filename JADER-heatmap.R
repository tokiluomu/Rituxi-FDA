# 加载所需的库
library(ggplot2)
library(reshape2)

JADER <- read_excel("JADER.xlsx")

data_value <- JADER[,1:5]
data <- JADER[, c(1, 6:10)]


colnames(data_value) <- c("pt_name_en", "ROR", "PRR", "MGPS", "BCPNN")

# 将数据转换为长格式
data_long <- melt(data, id.vars = "pt_name_en")            # 信号数据
data_value_long <- melt(data_value, id.vars = "pt_name_en")  # 值数据

colnames(data_long) <- c("pt_name_en", "variable", "value")
colnames(data_value_long) <- c("pt_name_en", "variable", "value")

# 将`value`转换为因子，以便使用离散调色板
data_long$value <- as.factor(data_long$value)

# 绘制热图
ggplot(data_long, aes(x = variable, y = pt_name_en, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "#ADF5D7", "1" = "#2A9D8E"), 
                    labels = c("Negative", "Positive"), 
                    name = "Signal") +
  theme_minimal() +
  theme(axis.text.x = element_text( hjust = 1,face = "bold"),axis.text.y = element_text(face = "bold")) +  
  labs(x = "Signal Algorithm", y = "PT Name") +
  geom_text(data = data_value_long, aes(label = value), color = "black", size = 3, na.rm = TRUE)

ggsave('JADER-heatmap.png', device = "png", dpi = 600,width=13, height=10,units = "in")
