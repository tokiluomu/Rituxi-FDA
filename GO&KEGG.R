library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(showtext)

# 载入数据
data <- read.xlsx("GSE31729-1.xlsx")



gene_data <- read.xlsx("gene list.xlsx")

# 将每行以 "/" 分割，合并成一个大向量
gene_list <- unlist(strsplit(gene_data[[1]], "/"))

# 去重并排序
unique_genes <- sort(unique(gene_list))

# 拆分多基因符号（确保名称正确）
data <- separate_rows(data, Gene.symbol, sep = "///")
data <- na.omit(data)

# 统一字段名为 log2FoldChange 和 pval，避免后续引用混乱
colnames(data)[which(names(data) == "logFC")] <- "log2FoldChange"
colnames(data)[which(names(data) == "P.Value")] <- "pval"

# 添加上下调标签
data <- data %>%
  mutate(label = case_when(
    pval < 0.05 & log2FoldChange > 1 ~ "UP",
    pval < 0.05 & log2FoldChange < -1 ~ "DOWN",
    TRUE ~ "STABLE"
  ))

# 去除重复基因，保留最显著的记录
data <- data %>%
  group_by(Gene.symbol) %>%
  slice_min(order_by = pval, with_ties = FALSE) %>%
  ungroup()

# 提取上下调前5
up_genes <- data %>% filter(label == "UP")
down_genes <- data %>% filter(label == "DOWN")

top5_up <- up_genes %>% slice_max(order_by = log2FoldChange, n = 5)
top5_down <- down_genes %>% slice_min(order_by = log2FoldChange, n = 5)

label_genes <- bind_rows(top5_up, top5_down)
# 筛选 data 中 gene.symbol 与 unique_genes 匹配的行
label_genes <- data[data$Gene.symbol %in% unique_genes, ]

# 确保有一个列名叫 "id" 用于标签显示（这里用 Gene.symbol 来命名）
label_genes$id <- label_genes$Gene.symbol

# 绘图并保存

png("volcano.tiff", units = "in", width = 7, height = 6, res = 600)
showtext_begin()

p1 <- ggplot(data, aes(x = log2FoldChange, y = -log10(pval), colour = label)) +
  geom_point(alpha = 0.65, size = 2) +
  scale_color_manual(values = c("DOWN" = "#0C4276", "STABLE" = "#d2dae2", "UP" = "#E73847")) +
  xlim(c(-3.5, 2)) + ylim(c(0, 5)) +
  geom_vline(xintercept = c(-1, 1), lty = 4, col = "black", lwd = 0.8) +
  geom_hline(yintercept = -log10(0.05), lty = 4, col = "black", lwd = 0.8) +
  labs(x = "log2FC", y = "-log10(pval)") +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 50, family = "serif"),
    axis.text = element_text(size = 50, family = "serif"),
    axis.title = element_text(size = 50, family = "serif")
  ) +
  geom_label_repel(
    data = label_genes,
    aes(log2FoldChange, -log10(pval), label = id),
    size = 15, fill = "#A8DADB", max.overlaps = 100, force = 100, color = "black", family = "serif"
  )

print(p1)

showtext_end()
dev.off()













#从这里要清除环境变量

#GO KEGG
library(openxlsx)
library(tidyr)
library(dplyr)


data <- read.xlsx("GSE31729-1.xlsx")

DEG_data <- data[data$P.Value < 0.05 & abs(data$logFC) > 1, ]
DEG_data <- separate_rows(DEG_data, Gene.symbol, sep = "///")

# 第一步：去除含 NA 的行
DEG_data <- na.omit(DEG_data)
DEG_data <- DEG_data %>%
  group_by(Gene.symbol) %>%
  slice_min(order_by = P.Value, with_ties = FALSE) %>%
  ungroup()






library(clusterProfiler)
library(org.Hs.eg.db)
library(enrichplot)
library(ggplot2)


# 提取基因列表
gene_symbols <- DEG_data$Gene.symbol

# 转换为 ENTREZ ID（KEGG 需要用）
gene_df <- bitr(gene_symbols,
                fromType = "SYMBOL",
                toType = "ENTREZID",
                OrgDb = org.Hs.eg.db)


ego <- enrichGO(gene         = gene_df$ENTREZID,
                OrgDb        = org.Hs.eg.db,
                keyType      = "ENTREZID",
                ont          = "ALL",   # 可改为 "BP", "MF", 或 "CC"
                pAdjustMethod = "BH",
                pvalueCutoff  = 0.05,
                qvalueCutoff  = 0.2,
                readable     = TRUE)
# 转成数据框
go_df <- as.data.frame(ego)

# 只保留常用列（可选）
go_clean <- go_df

# 加个标记
go_clean$Type <- "GO"





ekegg <- enrichKEGG(gene         = gene_df$ENTREZID,
                    organism     = "hsa",   # 人类是 "hsa"
                    pvalueCutoff = 0.05)

# 转换 KEGG 结果中的 ENTREZ ID 为 SYMBOL，便于阅读
ekegg <- setReadable(ekegg, OrgDb = org.Hs.eg.db, keyType = "ENTREZID")


kegg_df <- as.data.frame(ekegg)

# 只保留常用列
kegg_clean <- kegg_df

# 加个标记列补充分类（无 GO 分类，用 NA）
kegg_clean$ONTOLOGY <- NA
kegg_clean$Type <- "KEGG"




# GO KEGG绘图（压缩横轴版本）

library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggprism)
library(stringr)
library(tibble)
library(ggfun)
library(gground)
library(GO.db)

# 自定义颜色
pal <- c('#457B9D','#2A9D8E','#fbb05b','#E66F51')

# 合并 GO 和 KEGG 结果
data <- bind_rows(go_clean, kegg_clean)

# 处理数据
data <- data %>%
  mutate(ONTOLOGY = ifelse(is.na(ONTOLOGY), "KEGG", ONTOLOGY)) %>%
  filter(!is.na(p.adjust)) %>%
  transmute(
    ONTOLOGY = ONTOLOGY,
    Description = Description,
    Pvalue = pvalue,
    Count = Count,
    geneID = geneID
  )

# 找出需要标注的描述项（即在多个ONTOLOGY中重复出现的）
dup_descriptions <- data %>%
  distinct(Description, ONTOLOGY) %>%
  count(Description) %>%
  filter(n > 1) %>%
  pull(Description)

# 替换：仅在需要加括号的描述项中，才拼接 ONTOLOGY
data <- data %>%
  mutate(
    Description = if_else(
      Description %in% dup_descriptions,
      paste0(Description, " (", ONTOLOGY, ")"),
      Description
    )
  )


# 每个分类取前5显著项
use_pathway <- data %>%
  group_by(ONTOLOGY) %>%
  top_n(5, wt = -Pvalue) %>%
  group_by(Pvalue) %>%
  top_n(1, wt = Count) %>%
  ungroup() %>%
  mutate(ONTOLOGY = factor(ONTOLOGY, levels = rev(c("BP", "CC", "MF", "KEGG")))) %>%
  arrange(ONTOLOGY, Pvalue) %>%
  mutate(Description = factor(Description, levels = unique(Description))) %>%
  tibble::rowid_to_column("index")



# 添加压缩后的坐标
use_pathway <- use_pathway %>%
  mutate(logP = -log10(Pvalue),
         compressed_logP = log2(logP + 1))  # 避免log(0)

# 左侧分类标签和基因数量点图的宽度
width <- 1.3
# x 轴长度
xaxis_max <- max(log2(-log10(use_pathway$Pvalue)))+1 
# 左侧分类标签数据
rect.data <- group_by(use_pathway, ONTOLOGY) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  mutate(
    xmin = -3 * width,
    xmax = -2 * width,
    ymax = cumsum(n),
    ymin = lag(ymax, default = 0) + 0.6,
    ymax = ymax + 0.4
  )

tiff("GOKEGG.tiff", units = "in", width = 10, height = 9, res = 600)
use_pathway %>%
  ggplot(aes(log2(-log10(Pvalue)) + 1, y = index, fill = ONTOLOGY)) +
  geom_round_col(
    aes(y = Description), width = 0.6, alpha = 0.8
  ) +
  geom_text(
    aes(x = 0.05, label = Description),
    hjust = 0, size = 5
  ) +
  geom_text(
    aes(x = 0.1, label = geneID, colour = ONTOLOGY), 
    hjust = 0, vjust = 2.6, size = 3.5, fontface = 'italic', 
    show.legend = FALSE
  ) +
  # 基因数量
  geom_point(
    aes(x = -width, size = Count),
    shape = 21
  ) +
  geom_text(
    aes(x = -width, label = Count)
  ) +
  scale_size_continuous(name = 'Count', range = c(5, 16)) +
  # 分类标签
  geom_round_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = ONTOLOGY),
    data = rect.data,
    radius = unit(2, 'mm'),
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = ONTOLOGY),
    data = rect.data,
    inherit.aes = FALSE
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = xaxis_max, yend = 0),
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  labs(y = NULL) +
  scale_fill_manual(name = 'Category', values = pal) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(
    breaks = seq(0, xaxis_max, 2), 
    expand = expansion(c(0, 0))
  ) +
  theme_prism() +
  theme(
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_text()
  )
dev.off()