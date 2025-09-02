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


##读取IMM的那个Rdata

# 1️⃣ 提取 RESULT_IMMUNE 的 primaryid
imm_ids <- unique(RESULT_IMMUNE$primaryid)

# 2️⃣ 在 RESULT 中新增 IMM 列
RESULT <- RESULT %>%
  mutate(IMM = as.integer(primaryid %in% imm_ids))



# ============================
# 简化版脚本：只做交互检验（单一先验）
# ============================

# ---- 包 ----
library(dplyr)
library(brms)
library(posterior)
library(openxlsx)

# ---- 1) 数据准备与基本校验 ----
req_cols <- c("IMM","duration_day","onset_day","rituximab_user","has_tumor","has_psych",
              "AGE","SEX","drug_n","indi_pt_n")
missing_cols <- setdiff(req_cols, names(RESULT))
if(length(missing_cols)>0) stop("RESULT 缺少列: ", paste(missing_cols, collapse=", "))

# 只分析 IMM==1
RESULT <- RESULT %>% filter(IMM == 1)

# rituximab_user 和 SEX 设为因子
RESULT$rituximab_user <- as.factor(RESULT$rituximab_user)
RESULT$SEX <- as.factor(RESULT$SEX)

# 转化结局为 0/1
to_binary01 <- function(x){
  if(is.logical(x)) return(as.integer(x))
  xc <- as.character(x)
  xc[is.na(xc)] <- NA
  res <- ifelse(toupper(xc) %in% c("TRUE","T","1","YES","Y"), 1,
                ifelse(toupper(xc) %in% c("FALSE","F","0","NO","N"), 0, NA))
  return(as.integer(res))
}
RESULT$event_tumor <- to_binary01(RESULT$has_tumor)
RESULT$event_psych <- to_binary01(RESULT$has_psych)

# 处理缺失 duration_day
RESULT <- RESULT %>%
  mutate(duration_missing = is.na(duration_day),
         duration_day_filled = ifelse(duration_missing, 0, duration_day))

message("N (IMM==1): ", nrow(RESULT),
        "  tumor events: ", sum(RESULT$event_tumor==1, na.rm=TRUE),
        "  psych events: ", sum(RESULT$event_psych==1, na.rm=TRUE),
        "  duration missing: ", sum(RESULT$duration_missing))

# ---- 2) Strata3（把缺失单独作为一层） ----
RESULT <- RESULT %>%
  mutate(Strata3 = case_when(
    duration_missing ~ "Missing",
    duration_day_filled <= 360  ~ "<=360",
    duration_day_filled <= 720  ~ "361_720",
    duration_day_filled > 720   ~ ">720",
    TRUE ~ NA_character_
  ))
RESULT$Strata3 <- factor(RESULT$Strata3, levels = c("<=360","361_720",">720","Missing"))

# ---- 3) 单一先验交互检验 ----
prior_single <- c(prior(normal(0,2), class="b"), prior(cauchy(0,2), class="Intercept"))

interaction_results <- list()

# Tumor
fit_inter_t <- brm(
  formula = event_tumor ~ rituximab_user*Strata3 + AGE + SEX + drug_n + indi_pt_n + duration_missing,
  data = RESULT,
  family = bernoulli(link="logit"),
  prior = prior_single,
  chains=4, iter=2000, warmup=1000, cores=4, seed=123,
  control=list(adapt_delta=0.95)
)
post_it <- as_draws_df(fit_inter_t)
for(param in c("b_rituximab_user1:Strata3361_720","b_rituximab_user1:Strata3>720","b_rituximab_user1:Strata3Missing")){
  if(param %in% names(post_it)){
    or_it <- exp(post_it[[param]])
    interaction_results[[paste0(param,"_tumor")]] <- data.frame(
      param = param, outcome="tumor",
      median_OR = median(or_it), CI_lower=quantile(or_it,0.025), CI_upper=quantile(or_it,0.975)
    )
  }
}

# Psych
fit_inter_p <- brm(
  formula = event_psych ~ rituximab_user*Strata3 + AGE + SEX + drug_n + indi_pt_n + duration_missing,
  data = RESULT,
  family = bernoulli(link="logit"),
  prior = prior_single,
  chains=4, iter=2000, warmup=1000, cores=4, seed=123,
  control=list(adapt_delta=0.95)
)
post_ip <- as_draws_df(fit_inter_p)
for(param in c("b_rituximab_user1:Strata3361_720","b_rituximab_user1:Strata3>720","b_rituximab_user1:Strata3Missing")){
  if(param %in% names(post_ip)){
    or_ip <- exp(post_ip[[param]])
    interaction_results[[paste0(param,"_psych")]] <- data.frame(
      param = param, outcome="psych",
      median_OR = median(or_ip), CI_lower=quantile(or_ip,0.025), CI_upper=quantile(or_ip,0.975)
    )
  }
}

interaction_summary <- do.call(rbind, interaction_results)

# ---- 4) 保存 Excel ----
wb <- createWorkbook()
addWorksheet(wb,"Interaction_terms_OR")
writeData(wb,"Interaction_terms_OR",interaction_summary)
saveWorkbook(wb,"Interaction_Only_SinglePrior.xlsx", overwrite=TRUE)

message("交互检验完成，查看 Excel: Interaction_Only_SinglePrior.xlsx")
