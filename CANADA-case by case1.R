#case-by-case病例分析



library(openxlsx)
library(dplyr)
library(tools)


reac<-read.csv("CANADA-IMM-reac.csv")
reac[reac == ""] <- NA

teding_PT<-c("Anxiety")#疾病名称
teding_PT<-toupper(teding_PT)
reac_teding<-reac[reac$pt %in% teding_PT,]
teding_id<-unique(reac_teding$primaryid)
##特定人口学信息（如果年龄是mon单位还得换算）
demo<-read.csv("CANADA-IMM-demo.csv")
demo_teding<-demo[demo$primaryid %in% teding_id,]

##特定用药信息
drug<-read.csv("CANADA-IMM-drug.csv")
drug_teding<-drug[drug$primaryid %in% teding_id,]
drug_teding_unique <- drug_teding %>%
  distinct(primaryid, drugname, .keep_all = TRUE)
##特定适应症信息
indi<-read.csv("CANADA-IMM-indi.csv")
indi_teding<-indi[indi$primaryid %in% teding_id,]
indi_teding_unique <- indi_teding %>%
  distinct(primaryid, indi, .keep_all = TRUE)
indi_teding_unique$indi<-tolower(indi_teding_unique$indi)
indi_teding_unique$indi<-toTitleCase(indi_teding_unique$indi)
indi_teding1 <- indi_teding_unique %>%
  group_by(primaryid) %>%                   # 按 primaryid 分组
  summarise(indi_fenlei = paste(indi, collapse = "; ")) # 将每组 drug 列按 "; " 分隔连接成字符串
#reac_teding1 <- reac_teding %>%
#  group_by(primaryid) %>%                   # 按 primaryid 分组
#  summarise(pt_fenlei = paste(PT, collapse = "; ")) %>% arrange(pt_fenlei) # 将每组 pt 列按 "; " 分隔连接成字符串
drug_teding1 <- drug_teding_unique %>%
  group_by(primaryid) %>%                   # 按 primaryid 分组
  summarise(drug_fenlei = paste(drugname, collapse = "; ")) # 将每组 drug 列按 "; " 分隔连接成字符串
case_by_case<-merge(demo_teding,drug_teding1,by="primaryid")
case_by_case<-merge(case_by_case,reac_teding,by="primaryid",all = T)
case_by_case<-case_by_case[,c(1,7,8,9,17,21)]
case_by_case<-case_by_case %>% arrange(pt)
case_by_case1<-merge(case_by_case,indi_teding1,by="primaryid",all = T)
case_by_case1<-case_by_case1 %>% arrange(pt)
case_by_case1$pt<-tolower(case_by_case1$pt)
case_by_case1$pt<-toTitleCase(case_by_case1$pt)
case_by_case1 <- case_by_case1 %>%
  select(1, 6, 2:5, 7:ncol(case_by_case1))
write.xlsx(case_by_case1,"CANADA-IMM-焦虑Anxiety.xlsx")
