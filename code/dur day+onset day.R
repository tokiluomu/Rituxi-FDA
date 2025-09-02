####治疗与诱发时间作图
library(openxlsx)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(cowplot)
library(extrafont)#字体
library(dplyr)
library(tools)
library(broom)
library(stringr)
library(foreach)
library(doParallel)
#抗雄
#以下代码的前提建立在ther文件中不存在同一id同一药物不在同一天开始治疗的情况下（即以下情况会被过度去重：同一id在同一天施用同一药物，但在不同天停止治疗）
#该种情况可在zhiliao_fil与zhiliao_fil_pt去重前的重复中检出
###############################################################抗雄

##将ther与drug表根据id与drugseq合并，并取PS（默认PS导致不良反应）
read.csv("ther.csv")->ther
read.csv("drug.csv")->drug
zhiliao<-merge(ther,drug,by=c("primaryid","DRUG_SEQ"))
zhiliao<-zhiliao[which(zhiliao$ROLE_COD=="PS"),]
#######保证开始和结束治疗的时间完整计算治疗持续时间（当天服用为0天）
zhiliao_fil<-zhiliao[which(nchar(zhiliao$START_DT)+nchar(zhiliao$END_DT)==16),]
zhiliao_fil$DUR<-as.numeric(as.Date(as.character(zhiliao_fil$END_DT),format = "%Y%m%d")-as.Date(as.character(zhiliao_fil$START_DT),format = "%Y%m%d"))
##导出有效治疗时间数据（有负值也舍去，负值代表未用该药时不良反应就已发生）
zhiliao_fil<-zhiliao_fil[,c(1,3,4,5,9)]
zhiliao_fil<-zhiliao_fil[which(zhiliao_fil$DUR>=0),]
#****检查是否相等，不相等便取出同一时间服用同一药的个例对两条记录取平均值
duplicates1 <- zhiliao_fil[duplicated(paste(zhiliao_fil$primaryid,zhiliao_fil$START_DT,zhiliao_fil$DUR)), ]
duplicates2 <- zhiliao_fil[duplicated(paste(zhiliao_fil$primaryid,zhiliao_fil$START_DT)), ]
#zhiliao_fil<- zhiliao_fil %>% distinct(primaryid,START_DT,.keep_all = T)#对此两项去重保证没有完全相同的数据（但保留同人同季度的数据）

#取出zhiliao_fil数据框primaryid与START_DT列相同但END_DT不同的列
# 对 primaryid 和 START_DT 相同且 END_DT 不同的行，计算 END_DT 和 DUR 的均值
chongfu <- zhiliao_fil %>%
  group_by(primaryid, START_DT) %>%
  filter(n_distinct(END_DT) > 1) %>%
  summarize(
    END_DT = mean(END_DT),
    DUR = mean(DUR),
    DRUGNAME = first(DRUGNAME)  # 保留第一个出现的药物名，或根据需求调整
  ) %>%
  ungroup()
#****重复id：94132462，101564273，7357363，用药开始时间存在重复，但一个当天结束，另有服用更多天的情况
zhiliao_fil <- zhiliao_fil[!(zhiliao_fil$primaryid %in% chongfu$primaryid & zhiliao_fil$START_DT %in% chongfu$START_DT),]
zhiliao_fil<-rbind(zhiliao_fil,chongfu)
#****检查是否相等，此时应该相同
duplicates1 <- zhiliao_fil[duplicated(paste(zhiliao_fil$primaryid,zhiliao_fil$START_DT,zhiliao_fil$DUR)), ]
duplicates2 <- zhiliao_fil[duplicated(paste(zhiliao_fil$primaryid,zhiliao_fil$START_DT)), ]
zhiliao_fil<- zhiliao_fil %>% distinct(primaryid,START_DT,.keep_all = T)


##对数据框zhiliao_fil根据primaryid列去重，保留START_DT更大的行（本次这个不算重复）
#zhiliao_fil <- zhiliao_fil %>%
#  group_by(primaryid) %>%
#  slice_max(order_by = START_DT, with_ties = FALSE) %>%
#  ungroup()
write.xlsx(zhiliao_fil,"治疗持续时间.xlsx")                            
##结合reac文件对应pt
reac<-read.csv("reac.csv")
reac_fil<-reac[which(reac$primaryid %in% zhiliao_fil$primaryid),]
reac_fil <- reac_fil[reac_fil$PT != "", ]
zhiliao_fil_pt<-merge(zhiliao_fil,reac_fil,by="primaryid",all = T)
zhiliao_fil_pt <- na.omit(zhiliao_fil_pt)
###检查合并前后reac数据量不一致原因（同一种被视为PS的药物有多条用药时间记录，算最新的）
#reac_ji<-reac_fil %>% group_by(primaryid) %>% summarise(count_1 = n())
#reac_fil_ji<-zhiliao_fil_pt %>% group_by(primaryid) %>% summarise(count_2 = n())
#reac_jiancha<-merge(reac_ji,reac_fil_ji,by="primaryid",all = T)
#reac_jiancha_cuo<-reac_jiancha[which(reac_jiancha$count_1 != reac_jiancha$count_2),]
zhiliao_fil_pt<-zhiliao_fil_pt[,c(1:6)]
#****检查是否相等，不相等便在去重条件上加一条DUR（在前次两重复比较的时候便去重，这里也应该相等）
duplicates1 <- zhiliao_fil_pt[duplicated(paste(zhiliao_fil_pt$primaryid, zhiliao_fil_pt$PT,zhiliao_fil_pt$START_DT,zhiliao_fil_pt$DUR)), ]
duplicates2 <- zhiliao_fil_pt[duplicated(paste(zhiliao_fil_pt$primaryid, zhiliao_fil_pt$PT,zhiliao_fil_pt$START_DT)), ]
zhiliao_fil_pt<- zhiliao_fil_pt %>% distinct(primaryid,PT,START_DT,.keep_all = T)#对此三项去重保证没有完全相同的数据（但保留同人同季度的数据）
###检查zhiliao_fil_pt的重复
#duplicates <- zhiliao_fil_pt[duplicated(paste(zhiliao_fil_pt$primaryid, zhiliao_fil_pt$PT)), ]
###########zhiliao_fil_pt<- zhiliao_fil_pt %>% distinct(primaryid,PT,.keep_all = T)
####原则上应该把同一id同一pt不同治疗时间的数据算作不重复的两条，但是这里对治疗数据去重但未对诱发数据去重（本次都不算重复）

write.xlsx(zhiliao_fil_pt,"治疗持续时间(PT).xlsx")
read.xlsx("治疗持续时间(PT).xlsx")->zhiliao_fil_pt

zhiliao_fil_pt_combined <- zhiliao_fil_pt %>%
  mutate(PT = toTitleCase(tolower(PT))) %>%
  group_by(primaryid, START_DT, END_DT, DUR, DRUGNAME) %>%
  summarize(PT = paste(PT, collapse = ";")) %>%
  ungroup()

indi<-read.csv("indi.csv")

# 步骤 1 和 2: 只保留 primaryid 和 INDI_PT 列，并进行去重
indi_unique <- indi %>%
  select(primaryid, INDI_PT) %>%
  distinct()
# 步骤 3: 将相同 primaryid 的 INDI_PT 值合并在一起，首字母大写
indi_combined <- indi_unique %>%
  mutate(INDI_PT = toTitleCase(tolower(INDI_PT))) %>%
  group_by(primaryid) %>%
  summarize(INDI_PT = paste(unique(INDI_PT), collapse = ";")) %>%
  ungroup()
# 步骤 4: 与 zhiliao_fil_pt_combined 数据框合并
final_result <- zhiliao_fil_pt_combined %>%
  left_join(indi_combined, by = "primaryid")

demo <- read.csv("demo.csv")
demo_filtered <- demo %>%
  select(primaryid, FDA_DT, EVENT_DT, AGE, AGE_COD, SEX)
demo_filtered_unique <- demo_filtered %>%
  distinct(primaryid, .keep_all = TRUE)
final_merged_data <- demo_filtered_unique %>%
  inner_join(final_result, by = "primaryid")

youfa <- read.xlsx("不良反应诱发时间(每个PT的不良反应诱发时间).xlsx")
# 确保两个数据框的 primaryid 列类型一致
final_merged_data <- final_merged_data %>%
  mutate(primaryid = as.character(primaryid)) # 将 primaryid 转为字符类型

youfa <- youfa %>%
  mutate(primaryid = as.character(primaryid)) # 确保两者类型一致

# 进行左连接
final_merged_data <- final_merged_data %>%
  left_join(youfa %>% select(primaryid, TIME) %>% rename(youfa_time = TIME), 
            by = "primaryid")


write.xlsx(final_merged_data,"治疗+诱发时间-ALL.xlsx")


# 将 PT 列的每个值转换为首字母大写其他字母小写的格式
zhiliao_fil_pt <- zhiliao_fil_pt %>%
  mutate(PT = str_to_title(tolower(PT)))


zhiliao_fil_pt <- zhiliao_fil_pt %>%
  mutate(primaryid = as.character(primaryid)) # 将 primaryid 转为字符类型

youfa <- youfa %>%
  mutate(primaryid = as.character(primaryid)) # 确保两者类型一致

zhiliao_fil_pt <- zhiliao_fil_pt %>%
  left_join(
    youfa %>%
      select(primaryid, TIME) %>%
      rename(youfa_time = TIME),
    by = "primaryid",
    relationship = "many-to-many" # 声明多对多关系
  )
#去重
zhiliao_fil_pt <- zhiliao_fil_pt %>% 
  distinct()


demo_deduplicated <- demo_filtered %>%
  distinct(across(-primaryid), .keep_all = TRUE)

# 筛选只保留在 demo_deduplicated 中存在的 primaryid
zhiliao_fil_pt_filtered <- zhiliao_fil_pt %>%
  filter(primaryid %in% demo_deduplicated$primaryid)

pt_duration_summary <- zhiliao_fil_pt_filtered %>%
  group_by(PT) %>%
  summarize(
    mean_duration = mean(DUR, na.rm = TRUE),          # DUR 的均值
    mean_youfa_time = mean(youfa_time, na.rm = TRUE), # youfa_time 的均值
    count = n(),                                      # 每个 PT 的数量
    primaryid = paste(unique(primaryid), collapse = ";") # 去重后的 primaryid 列表
  ) %>%
  ungroup()  # 解除分组

ptname <- read.xlsx("不良反应信号(PT).xlsx")

# Step 1: 将 ptname 的 PT 列首字母大写，其他小写
ptname$PT <- stringr::str_to_title(ptname$PT)

# Step 2: 进行匹配，将 ptname 的 pt_name_cn 和 soc_name_cn 列添加到 pt_duration_summary
# 添加 pt_name_cn 和 soc_name_cn 列
pt_duration_summary <- pt_duration_summary %>%
  left_join(ptname %>% select(PT, pt_name_cn, soc_name_cn), by = "PT")






write.xlsx(pt_duration_summary,"PT平均治疗+诱发时间.xlsx")


