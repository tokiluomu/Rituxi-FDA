library(dplyr)
library(stringr)
library(ggplot2)

pt_list <- c(
  "Bladder Cancer", "Pituitary Tumour", "Cholesteatoma", "Lung Neoplasm",
  "Myelodysplastic Syndrome Transformation", "Skin Cancer Metastatic",
  "Neuroendocrine Tumour", "Mutism", "Reading Disorder",
  "Attention Deficit Hyperactivity Disorder"
)

pt_list_upper <- toupper(pt_list)

all_data <- data.frame()

# 循环每个PT，提取年龄数据并添加标签
for(pt in pt_list_upper){
  df_filtered <- RESULT_IMMUNE %>%
    filter(
      rituximab_user == 1,
      immune_kidney == TRUE,
      str_detect(toupper(AE), pt)
    )
  
  n_total <- nrow(df_filtered)
  n_na <- sum(is.na(df_filtered$AGE))
  
  # AGE 非空
  df_age <- df_filtered %>%
    filter(!is.na(AGE)) %>%
    mutate(
      PT_label = paste0(pt, " (Total: ", n_total, ", Missing AGE: ", n_na, ")")
    )
  
  all_data <- bind_rows(all_data, df_age)
}

# 将年龄按5岁分箱
all_data <- all_data %>%
  mutate(AGE_BIN = cut(AGE, breaks = seq(0, max(AGE, na.rm = TRUE) + 5, by = 5),
                       right = FALSE, include.lowest = TRUE))

# 统计每个PT每个年龄区间数量
age_count <- all_data %>%
  group_by(PT_label, AGE_BIN) %>%
  summarise(Count = n(), .groups = "drop")

# 绘图
p <- ggplot(age_count, aes(x = AGE_BIN, y = Count, color = PT_label, group = PT_label)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(size = 1.2) +
  geom_text(aes(label = Count), vjust = -0.5, size = 3, show.legend = FALSE) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Age Distribution of Selected PTs in IMM Patients on Rituximab",
    subtitle = "Colored by PT",
    x = "Age (years, 5-year bins)",
    y = "Number of Patients",
    color = "PT (Total / Missing AGE)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  ) +
  guides(color = guide_legend(nrow = 4, byrow = TRUE)) +  # 图例两行
  expand_limits(y = 0)

ggsave(filename = "IMM_PT_Age_Distribution.png",plot = p,width = 12,height = 8,dpi = 600)
