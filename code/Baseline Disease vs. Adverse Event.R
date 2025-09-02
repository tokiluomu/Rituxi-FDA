library(dplyr)
library(openxlsx)

ther <- read.csv("FAERS-IMM/ther.csv")

ther <- ther %>%
  filter(DRUG_SEQ == 1 & !is.na(START_DT) & !is.na(END_DT)) %>%
  group_by(primaryid) %>%
  slice(1) %>%  # 保留每组的第一条记录
  ungroup()

parse_date_flexible <- function(date_str) {
  date_str <- as.character(date_str)
  n <- nchar(date_str)
  if (n == 8) {
    as.Date(date_str, format = "%Y%m%d")
  } else if (n == 6) {
    as.Date(paste0(date_str, "15"), format = "%Y%m%d")  # 中间日
  } else if (n == 4) {
    as.Date(paste0(date_str, "0615"), format = "%Y%m%d")  # 年中
  } else {
    NA
  }
}


convert_dur_to_days <- function(dur, dur_cod) {
  if (is.na(dur) | is.na(dur_cod)) return(NA)
  
  dur_cod <- toupper(dur_cod)
  days <- switch(dur_cod,
                 "YR" = ceiling(as.numeric(dur) * 360),
                 "WK" = ceiling(as.numeric(dur) * 7),
                 "DAY" = ceiling(as.numeric(dur)),
                 "MIN" = ceiling(as.numeric(dur) / 1440),
                 NA)
  return(days)
}


ther_duration <- ther %>%
  rowwise() %>%
  mutate(
    start_date = parse_date_flexible(START_DT),
    end_date = parse_date_flexible(END_DT),
    duration_day = case_when(
      # 情况1：两个都是完整日期
      !is.na(start_date) & !is.na(end_date) ~ as.integer(difftime(end_date, start_date, units = "days")) + 1,
      
      # 情况2：至少一方不是完整日期，但有 DUR + DUR_COD
      (is.na(start_date) | is.na(end_date)) & !is.na(DUR) & !is.na(DUR_COD) ~ convert_dur_to_days(DUR, DUR_COD),
      
      # 情况3：两边都不完整，也没 DUR，用中间值估计
      TRUE ~ {
        sdate <- parse_date_flexible(START_DT)
        edate <- parse_date_flexible(END_DT)
        
        # 如果中间值解析后 end <= start，则让 end = start
        if (is.na(sdate)) sdate <- parse_date_flexible(START_DT)
        if (is.na(edate)) edate <- parse_date_flexible(END_DT)
        
        if (is.na(sdate)) sdate <- as.Date("1900-01-01")  # fallback 防止崩溃
        if (is.na(edate)) edate <- as.Date("1900-01-01")
        
        delta <- as.integer(difftime(edate, sdate, units = "days")) + 1
        if (delta <= 0) 1 else delta
      }
    )
  ) %>%
  ungroup()


demo <- read.csv("FAERS-IMM/demo.csv")

ALL_info_data <- merge(ther_duration, demo, by = "primaryid", all.x = TRUE)


write.xlsx(ALL_info_data,"筛选有治疗时间患者的DUR.xlsx")





# 读取indi数据
indi <- read.csv("FAERS-IMM/indi.csv", sep = ",", stringsAsFactors = FALSE)

# 使用 dplyr 和 tidyr
library(dplyr)
library(tidyr)

# 合并相同 primaryid 的 INDI_PT 字段
indi_combined <- indi %>%
  group_by(primaryid) %>%
  summarise(INDI_PT_merged = paste(INDI_PT, collapse = "; "))  # 可换成 "," 或其他分隔符

# 假设 ALL_info_unique 已经在环境中，包含 primaryid 字段
# 如果还没加载，请使用 read.csv 加载 ALL_info_unique 对应文件

# 合并两个数据框
merged_ALL_info_data <- ALL_info_data %>%
  left_join(indi_combined, by = "primaryid")






# 1. 假设 reac 已经读取进来了（reac.csv）
# 建议读取时也用 sep="\t" 并指定不自动改列名：
reac <- read.csv("FAERS-IMM/reac.csv", sep = ",", stringsAsFactors = FALSE, check.names = FALSE)

# 2. 清理列名（建议总是加上）
names(reac) <- trimws(names(reac))

# 3. 整合相同 primaryid 的 PT 列
reac_combined <- reac %>%
  group_by(primaryid) %>%
  summarise(PT_merged = paste(PT, collapse = "; "))

# 4. 合并进之前的 merged_ALL_info_data
final_merged_data <- merged_ALL_info_data %>%
  left_join(reac_combined, by = "primaryid")

final_merged_data <- final_merged_data %>%
  filter(!is.na(EVENT_DT) & EVENT_DT != "")

ext <- read.xlsx("IMM-详细-大于720天治疗时长PT.xlsx")

names(ext) <- trimws(names(ext))
names(final_merged_data) <- trimws(names(final_merged_data))
ext$primaryid <- as.character(ext$primaryid)
final_merged_data$primaryid <- as.character(final_merged_data$primaryid)
all_ids_present <- ext$primaryid %in% final_merged_data$primaryid

# 将 duration_day 转换为数值类型（如尚未）
final_merged_data$duration_day <- as.numeric(final_merged_data$duration_day)

# 替换 duration_day < 1 为 1
final_merged_data$duration_day[final_merged_data$duration_day < 1] <- 1


final_merged_data$primaryid <- as.character(final_merged_data$primaryid)
valid_ids <- as.character(ext$primaryid[all_ids_present])


# 1. 拆分为两部分：duration_day <= 720 全保留，>720 做筛选
part_1 <- final_merged_data %>% 
  filter(duration_day <= 720)

part_2 <- final_merged_data %>% 
  filter(duration_day > 720 & primaryid %in% valid_ids)

# 2. 合并两部分为最终数据
final_data <- bind_rows(part_1, part_2)


# 读取 drug.csv（注意\t分隔）
drug <- read.csv("FAERS-IMM/drug.csv", sep = ",", stringsAsFactors = FALSE, check.names = FALSE)

# 清理列名（可选但推荐）
names(drug) <- trimws(names(drug))

# 第一步：按 primaryid 和 DRUGNAME 去重
drug_dedup <- drug %>%
  distinct(primaryid, DRUGNAME, .keep_all = TRUE)

# 第二步：按 primaryid 合并 DRUGNAME（使用 ; 分隔）
drug_combined <- drug_dedup %>%
  group_by(primaryid) %>%
  summarise(DRUGNAME_merged = paste(unique(DRUGNAME), collapse = "; "))

drug_combined$primaryid <- as.character(drug_combined$primaryid)

final_data_with_drug <- final_data %>%
  left_join(drug_combined, by = "primaryid")


final_data_with_drug <- final_data_with_drug %>%
  mutate(
    INDI_count = ifelse(!is.na(INDI_PT_merged),
                        sapply(strsplit(INDI_PT_merged, ";"), length),
                        0),
    DRUG_count = ifelse(!is.na(DRUGNAME_merged),
                        sapply(strsplit(DRUGNAME_merged, ";"), length),
                        0),
    PT_count = ifelse(!is.na(PT_merged),
                      sapply(strsplit(PT_merged, ";"), length),
                      0)
  )








library(dplyr)

final_data_with_drug <- final_data_with_drug %>%
  mutate(
    DUR_group = ifelse(duration_day > 720, "长期用药", "短期用药")
  )
summary_stats <- final_data_with_drug %>%
  group_by(DUR_group) %>%
  summarise(
    Avg_INDI = mean(INDI_count, na.rm = TRUE),
    Med_INDI = median(INDI_count, na.rm = TRUE),
    Avg_DRUG = mean(DRUG_count, na.rm = TRUE),
    Med_DRUG = median(DRUG_count, na.rm = TRUE),
    Count = n()
  )




library(dplyr)
library(tidyr)

# 创建 duration 分组
final_data_with_drug <- final_data_with_drug %>%
  mutate(DUR_group = ifelse(duration_day > 720, "Long-Term", "Short-Term"))

# 将 INDI_count 和 DRUG_count 转为长格式变量
plot_data <- final_data_with_drug %>%
  select(DUR_group, INDI_count, DRUG_count) %>%
  pivot_longer(cols = c(INDI_count, DRUG_count),
               names_to = "Count_Type", values_to = "Count_Value") %>%
  mutate(Count_Type = recode(Count_Type,
                             "INDI_count" = "Baseline Disease Count ",
                             "DRUG_count" = "Medication Count"))


library(ggplot2)
library(ggpubr)

ggplot(plot_data, aes(x = DUR_group, y = Count_Value, fill = DUR_group)) +
  # 加 whisker 横线
  stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 0.6, color = "black") +
  
  # 主体箱线图
  geom_boxplot(width = 0.5,
               outlier.shape = NA,
               fatten = 1.2,
               notch = FALSE,
               coef = 1.5,
               color = "black",
               alpha = 0.7,
               varwidth = FALSE,
               na.rm = TRUE) +
  
  # 抖动点
  geom_jitter(width = 0.2, alpha = 0.3, color = "gray30") +
  
  # 分面与显著性注释
  facet_wrap(~ Count_Type, scales = "free_y") +
  stat_compare_means(method = "wilcox.test", label = "p.format", size = 5) +
  
  # 调色与标签
  scale_fill_manual(values = c("Short-Term" = "#3C8DBC", "Long-Term" = "#E69F00")) +
  labs(title = "Baseline Disease and Medication Counts by Treatment Duration",
       x = "Treatment Duration Group",
       y = "Count") +
  
  # 美化主题
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 15),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "gray85"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

ggsave("duration_vs_baseline_medication.png", width = 7, height = 10, dpi = 600)
ggsave("duration_vs_baseline_medication.tiff", width = 7, height = 10, dpi = 600)





final_data_with_drug <- final_data_with_drug %>%
  mutate(
    duration_flex_group = case_when(
      duration_day <= 30    ~ "0–30 days",
      duration_day <= 60    ~ "31–60 days",
      duration_day <= 90    ~ "61–90 days",
      duration_day <= 180   ~ "91–180 days",
      duration_day <= 270   ~ "181–270 days",
      duration_day <= 360   ~ "271–360 days",
      duration_day <= 540   ~ "361–540 days",
      duration_day <= 720   ~ "541–720 days",
      duration_day <= 900   ~ "721–900 days",
      duration_day <= 1080  ~ "901–1080 days",
      duration_day <= 1800  ~ "1081–1800 days",
      duration_day <= 2520  ~ "1801–2520 days",
      duration_day > 2520   ~ ">2520 days",
    )
  )

final_data_with_drug$duration_flex_group <- factor(
  final_data_with_drug$duration_flex_group,
  levels = c(
    "0–30 days", "31–60 days", "61–90 days", "91–180 days",
    "181–270 days", "271–360 days", "361–540 days", "541–720 days",
    "721–900 days", "901–1080 days", "1081–1800 days", "1801–2520 days", ">2520 days"
  ),
  ordered = TRUE
)


ggplot(final_data_with_drug, aes(x = duration_flex_group, y = DRUG_count)) +
  geom_boxplot(fill = "#A0CBE8", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "black") +
  labs(
    title = "Drug Count by Treatment Duration Group",
    x = "Treatment Duration (Days)",
    y = "Number of Medications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("Drug counts.png", width = 7, height = 7, dpi = 600)


ggplot(final_data_with_drug, aes(x = duration_flex_group, y = INDI_count)) +
  geom_boxplot(fill = "#D55E00", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "black") +
  labs(
    title = "Baseline Disease Count by Treatment Duration",
    x = "Duration Group", 
    y = "Number of Baseline Diseases"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("Baseline Disease.png", width = 7, height = 7, dpi = 600)


ggplot(final_data_with_drug, aes(x = duration_flex_group, y = PT_count)) +
  geom_boxplot(fill = "#66A61E", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "black") +
  labs(
    title = "Adverse Event Count by Treatment Duration",
    x = "Duration Group", 
    y = "Number of Adverse Event"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("Adverse Event.png", width = 7, height = 7, dpi = 600)



cor_test_result <- cor.test(final_data_with_drug$INDI_count, final_data_with_drug$PT_count, method = "spearman")

# 计算相关性
cor_result <- cor.test(final_data_with_drug$INDI_count, final_data_with_drug$PT_count, method = "spearman")
rho <- round(cor_result$estimate, 3)
pval <- signif(cor_result$p.value, 3)
label_text <- paste0("Spearman rho = ", rho, "\n", "p = ", pval)

# 绘图
ggplot(final_data_with_drug, aes(x = INDI_count, y = PT_count)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.4, color = "#3C8DBC") +
  geom_smooth(method = "lm", se = TRUE, color = "#E69F00", size = 1.2, linetype = "dashed") +
  geom_text(aes(label = ifelse(PT_count > 10 | INDI_count > 5, paste0("(", INDI_count, ",", PT_count, ")"), "")),
            size = 3, vjust = -0.8, color = "black", check_overlap = TRUE) +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5,
           label = label_text, size = 5, fontface = "bold", color = "gray20") +
  labs(
    title = "Baseline Disease Count vs. Adverse Event Count",
    x = "Number of Baseline Diseases",
    y = "Number of Adverse Events (PT_count)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

ggsave("Baseline Disease Count vs. Adverse Event Count.png", width = 7, height = 7, dpi = 600)





final_data_with_drug <- final_data_with_drug %>%
  mutate(
    INDI_bin = case_when(
      INDI_count <= 1  ~ "0–1",
      INDI_count <= 3  ~ "2–3",
      INDI_count <= 5  ~ "4–5",
      INDI_count <= 7  ~ "6–7",
      INDI_count <= 9  ~ "8–9",
      INDI_count <= 11 ~ "10–11",
      INDI_count <= 13 ~ "12–13",
      INDI_count <= 15 ~ "14–15",
      INDI_count <= 17 ~ "16–17",
      INDI_count <= 19 ~ "18–19",
      INDI_count <= 21 ~ "20–21",
      INDI_count <= 23 ~ "22–23",
      INDI_count <= 25 ~ "24–25",
      INDI_count <= 27 ~ "26–27",
      INDI_count <= 29 ~ "28–29",
      INDI_count <= 31 ~ "30–31",
      INDI_count >= 32 ~ "32+",
      TRUE ~ NA_character_
    )
  )

# 固定顺序
final_data_with_drug$INDI_bin <- factor(
  final_data_with_drug$INDI_bin,
  levels = c("0–1", "2–3", "4–5", "6–7", "8–9", "10–11", "12–13", "14–15",
             "16–17", "18–19", "20–21", "22–23", "24–25", "26–27", "28–29", "30–31", "32+"),
  ordered = TRUE
)

ggplot(final_data_with_drug, aes(x = INDI_bin, y = PT_count, fill = INDI_bin)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, color = "black") +
  geom_jitter(width = 0.25, alpha = 0.2, color = "gray40") +
  scale_fill_manual(values = rep(c("#F0E442"), length.out = 17)) +
  labs(
    title = "Adverse Event Count by Baseline Disease Group",
    x = "Baseline Disease Count Group",
    y = "Adverse Event Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA)
  )

ggsave("Baseline Disease vs. Adverse Event boxpolt.png", width = 7, height = 7, dpi = 600)


library(openxlsx)
write.xlsx(final_data_with_drug,"All.xlsx")
