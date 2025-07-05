str(DEMO)
DEMO_filtered <- DEMO[DEMO$FDA_DT %/% 100 <= 202406, ]


library(dplyr)

# ① DRUG 里先“行去重”——只保留 (primaryid, DRUGNAME) 的唯一组合
DRUG_unique <- DRUG %>% 
  distinct(primaryid, DRUGNAME)

# ② 对同一 primaryid 的多个 DRUGNAME 合并为一行，
#    用 ; 或 , 连接都可以，自行替换分隔符
DRUG_agg <- DRUG_unique %>% 
  group_by(primaryid) %>% 
  summarise(
    DRUGNAME = paste(DRUGNAME, collapse = "; "),   # <- 换成 "," 也行
    .groups = "drop"
  )

# ③ 以 DEMO_filtered 为主表做左连接，匹配不上填 NA
RESULT <- DEMO_filtered %>% 
  left_join(DRUG_agg, by = "primaryid")




keep_objs <- c("RESULT")
rm(list = setdiff(ls(), keep_objs))  
gc()   



### 1. 处理 INDI ----------------------------------------------------------

# 去掉 (primaryid, INDI_PT) 的重复组合
INDI_unique <- INDI %>% 
  distinct(primaryid, INDI_PT)

# 同一 primaryid 把多个 INDI_PT 串成一条，用 ; 分隔
INDI_agg <- INDI_unique %>% 
  group_by(primaryid) %>% 
  summarise(
    INDI_PT = paste(INDI_PT, collapse = "; "),
    .groups = "drop"
  )

### 2. 与之前的结果合并 ----------------------------------------------------

# 如果你已经按上一条指引得到了 RESULT（DEMO_filtered + DRUGNAME）
RESULT <- RESULT %>% 
  left_join(INDI_agg, by = "primaryid")


keep_objs <- c("RESULT")
rm(list = setdiff(ls(), keep_objs))  
gc()   





### 1. 预处理 REAC --------------------------------------------------------
colnames(REAC)[1] <- "primaryid"

REAC_agg <- REAC %>% 
  distinct(primaryid, pt) %>%         # (primaryid, PT) 去重
  group_by(primaryid) %>% 
  summarise(
    AE = paste(pt, collapse = "; "),  # 把 PT 串成一个字段，命名为 AE
    .groups = "drop"
  )

### 2. 合并到总表 (假设之前的总表叫 FINAL) -------------------------------
RESULT <- RESULT %>%                   # 如果你之前只是 DEMO_filtered，也可以换成 DEMO_filtered
  left_join(REAC_agg, by = "primaryid")


keep_objs <- c("RESULT")
rm(list = setdiff(ls(), keep_objs))  
gc()   




ther <- THER


library(data.table)

# 1. 过滤 & 去重 ----------------------------------------------------------
setDT(ther)                                  # 转 data.table
ther <- ther[DRUG_SEQ == 1 &
               !is.na(START_DT) & !is.na(END_DT)]
ther <- ther[order(primaryid)]
ther <- ther[, .SD[1], by = primaryid]       # 每个 primaryid 只保留第一条

# 2. 向量化日期解析 --------------------------------------------------------
parse_date_vec <- function(x) {
  x <- as.character(x)
  n <- nchar(x)
  fifelse(n == 8, as.IDate(x, "%Y%m%d"),
          fifelse(n == 6, as.IDate(paste0(x, "15"), "%Y%m%d"),
                  fifelse(n == 4, as.IDate(paste0(x, "0615"), "%Y%m%d"),
                          as.IDate(NA_character_))))
}

ther[, `:=`(
  start_date = parse_date_vec(START_DT),
  end_date   = parse_date_vec(END_DT)
)]

# 3. 向量化 duration 计算 --------------------------------------------------
convert_dur_to_days <- function(dur, cod) {
  # 向量化实现
  cod <- toupper(cod)
  days <- fifelse(cod == "YR",  ceiling(as.numeric(dur) * 360),
                  fifelse(cod == "WK",  ceiling(as.numeric(dur) * 7),
                          fifelse(cod == "DAY", ceiling(as.numeric(dur)),
                                  fifelse(cod == "MIN", ceiling(as.numeric(dur) / 1440), NA_real_))))
  days
}

ther[, duration_day :=
       fifelse(!is.na(start_date) & !is.na(end_date),
               as.integer(end_date - start_date) + 1,
               
               fifelse((is.na(start_date) | is.na(end_date)) & !is.na(DUR) & !is.na(DUR_COD),
                       convert_dur_to_days(DUR, DUR_COD),
                       
                       {                       # fallback：两端模糊 → 取差值，非正则置 1
                         delta <- as.integer(end_date - start_date) + 1
                         fifelse(delta <= 0, 1L, delta)
                       }))]

# 结果即 ther，data.table 自动多线程
# ① 处理 duration_day
ther_dur <- ther %>% 
  mutate(
    duration_day = if_else(is.na(duration_day) | duration_day < 1, 1L, duration_day)
  ) %>% 
  select(primaryid, duration_day)           # ② 只保留两列

# ③ 左连接到 RESULT
RESULT <- RESULT %>%
  left_join(ther_dur, by = "primaryid")




library(dplyr)
library(lubridate)


parse_date_flexible <- function(date_vec) {
  date_vec <- as.character(date_vec)
  n        <- nchar(date_vec)
  
  out <- rep(NA_character_, length(date_vec))   # 预填 NA
  
  idx8 <- !is.na(n) & n == 8
  idx6 <- !is.na(n) & n == 6
  idx4 <- !is.na(n) & n == 4
  
  out[idx8] <- date_vec[idx8]
  out[idx6] <- paste0(date_vec[idx6], "15")     # YYYYMM → YYYYMM15
  out[idx4] <- paste0(date_vec[idx4], "0615")   # YYYY   → YYYY0615
  
  as.Date(out, format = "%Y%m%d")
}


library(data.table)

# a) 转成 data.table
setDT(ther)
setDT(RESULT)

# b) 解析日期（向量化函数依旧可用）
ther[, start_date := parse_date_flexible(START_DT)]
RESULT[, event_date := parse_date_flexible(EVENT_DT)]

# c) 最早事件
result_event <- RESULT[!is.na(event_date),
                       .(event_date = min(event_date)),   # data.table 语法
                       by = primaryid]

# d) join & 计算 onset
ther_onset <- result_event[ther, on = "primaryid"]      # left join
ther_onset[, onset_day := as.integer(event_date - start_date) + 1]

onset_map <- ther_onset[, .(primaryid, onset_day)]

## 把 onset_day 写回 RESULT（按引用更新，内存友好）
RESULT[onset_map, onset_day := i.onset_day, on = "primaryid"]


## 1️⃣ 删掉 event_date 列
RESULT[, event_date := NULL]

## 2️⃣ 把 onset_day 中 < 1 的值改成 1
RESULT[onset_day < 1, onset_day := 1L]   # 加 L 明确设为整数

keep_objs <- c("RESULT")
rm(list = setdiff(ls(), keep_objs))  
gc()   




library(data.table)
library(stringi)

setDT(RESULT)

## 可显式指定线程；设 0 则用全部核心
Sys.setenv(STRINGI_NUM_THREADS = parallel::detectCores()) 

count_fast <- function(x) {
  # 先把缺失/字面 "NA" 变空串，避免多计
  x2 <- ifelse(is.na(x) | toupper(trimws(x)) == "NA", "", x)
  
  # fast: ";" 个数 + 1 = token 数；空串计 0
  n <- stri_count_fixed(x2, ";") + 1L
  n[x2 == ""] <- 0L
  n
}

RESULT[, `:=`(
  drug_n    = count_fast(DRUGNAME),
  indi_pt_n = count_fast(INDI_PT),
  ae_n      = count_fast(AE)
)]



SE <- read.csv("PT查询.csv")
drug <- read.csv("drug.csv", sep = ",", stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
library(stringr)
library(parallel)

## ---------- 1. 准备肿瘤 & 精神疾病 PT ----------
tumor_pts <- SE %>%
  filter(soc_name_en == "NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS)") %>%
  pull(PT) %>% str_to_upper() %>% unique()

psych_pts <- SE %>%
  filter(soc_name_en == "PSYCHIATRIC DISORDERS") %>%
  pull(PT) %>% str_to_upper() %>% unique()

## ---------- 2. 切块 ----------
RESULT <- RESULT %>% mutate(row_id = row_number())
ncore <- detectCores(logical = FALSE)
chunks <- split(RESULT, (seq_len(nrow(RESULT)) - 1) %% ncore)

## ---------- 3. 并行计算 ----------
cl <- makeCluster(ncore)

clusterExport(cl, varlist = c("tumor_pts", "psych_pts"), envir = environment())
clusterEvalQ(cl, {
  library(dplyr)
  library(tidyr)
  library(stringr)
})

annot_list <- parLapply(cl, chunks, function(df_chunk) {
  df_chunk %>%
    separate_rows(AE, sep = ";\\s*") %>%
    mutate(AE = str_trim(str_to_upper(AE))) %>%
    mutate(
      is_tumor = AE %in% tumor_pts,
      is_psych = AE %in% psych_pts
    ) %>%
    group_by(primaryid, row_id) %>%
    summarise(
      has_tumor = any(is_tumor, na.rm = TRUE),
      has_psych = any(is_psych, na.rm = TRUE),
      .groups = "drop"
    )
})

stopCluster(cl)

annot <- bind_rows(annot_list)

## ---------- 4. 合并 ----------
RESULT <- RESULT %>%
  left_join(annot, by = c("primaryid", "row_id")) %>%
  select(-row_id)





# 先把drug的primaryid提取出来
ritux_ids <- unique(drug$primaryid)

# 在RESULT中标记
RESULT_tagged <- RESULT %>%
  mutate(rituximab_user = primaryid %in% ritux_ids)

RESULT <- RESULT %>%
  mutate(rituximab_user = as.integer(primaryid %in% ritux_ids))


library(openxlsx)
write.xlsx(RESULT[1:5, ], "RESULT_top5.xlsx")

# 加载必要的R包
library(dplyr)
library(ggplot2)
library(tableone)
library(MatchIt)
library(broom)
library(forestplot)

# 确保关键变量为正确类型
RESULT$rituximab_user <- as.factor(RESULT$rituximab_user)
RESULT$has_tumor <- as.factor(RESULT$has_tumor)
RESULT$has_psych <- as.factor(RESULT$has_psych)
RESULT$SEX <- as.factor(RESULT$SEX)
RESULT$REPORTER_COUNTRY <- as.factor(RESULT$REPORTER_COUNTRY)




# 可选：处理缺失值
RESULT <- RESULT %>% filter(!is.na(has_tumor), !is.na(has_psych), !is.na(rituximab_user))

# 3. 单因素分析
# 3.1 肿瘤 vs. rituximab
tumor_table <- table(RESULT$has_tumor, RESULT$rituximab_user)
tumor_test <- chisq.test(tumor_table)

# 3.2 精神疾病 vs. rituximab
psych_table <- table(RESULT$has_psych, RESULT$rituximab_user)
psych_test <- chisq.test(psych_table)

print(tumor_table)
print(tumor_test)
print(psych_table)
print(psych_test)





# 4. 多因素Logistic回归
# 4.1 has_tumor模型
model_tumor <- glm(has_tumor ~ rituximab_user + AGE + SEX + drug_n + ae_n + REPORTER_COUNTRY, 
                   family = binomial(link = "logit"), data = RESULT)
summary(model_tumor)

# 4.2 has_psych模型
model_psych <- glm(has_psych ~ rituximab_user + AGE + SEX + drug_n + ae_n + REPORTER_COUNTRY, 
                   family = binomial(link = "logit"), data = RESULT)
summary(model_psych)

# 5. 结果提取与可视化
# 5.1 OR值计算
tumor_or <- tidy(model_tumor, exponentiate = TRUE, conf.int = TRUE)
psych_or <- tidy(model_psych, exponentiate = TRUE, conf.int = TRUE)

print(tumor_or)
print(psych_or)
