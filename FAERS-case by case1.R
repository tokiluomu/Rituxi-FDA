#case-by-case病例分析



library(openxlsx)
library(dplyr)
library(tools)


reac<-read.csv("FAERS-IMM-reac.csv")
reac[reac == ""] <- NA
reac<-na.omit(reac)
teding_PT<-c("MUTISM")#疾病名称
teding_PT<-toupper(teding_PT)
reac_teding<-reac[reac$PT %in% teding_PT,]
teding_id<-unique(reac_teding$primaryid)
##特定人口学信息（如果年龄是mon单位还得换算）
demo<-read.csv("FAERS-IMM-demo.csv")
demo_teding<-demo[demo$primaryid %in% teding_id,]
demo_teding$AGE<-ifelse(demo_teding$AGE_COD=="MON",
                        round(demo_teding$AGE/12),demo_teding$AGE)
demo_teding$AGE<-ifelse(demo_teding$AGE_COD=="WK",
                        round(demo_teding$AGE/52),demo_teding$AGE)
##特定用药信息
drug<-read.csv("FAERS-IMM-drug.csv")
drug_teding<-drug[drug$primaryid %in% teding_id,]
drug_teding_unique <- drug_teding %>%
  distinct(primaryid, DRUGNAME, .keep_all = TRUE)
##特定适应症信息
indi<-read.csv("FAERS-IMM-indi.csv")
indi_teding<-indi[indi$primaryid %in% teding_id,]
indi_teding_unique <- indi_teding %>%
  distinct(primaryid, INDI_PT, .keep_all = TRUE)
indi_teding_unique$INDI_PT<-tolower(indi_teding_unique$INDI_PT)
indi_teding_unique$INDI_PT<-toTitleCase(indi_teding_unique$INDI_PT)
indi_teding1 <- indi_teding_unique %>%
  group_by(primaryid) %>%                   # 按 primaryid 分组
  summarise(indi_fenlei = paste(INDI_PT, collapse = "; ")) # 将每组 drug 列按 "; " 分隔连接成字符串
#reac_teding1 <- reac_teding %>%
#  group_by(primaryid) %>%                   # 按 primaryid 分组
#  summarise(pt_fenlei = paste(PT, collapse = "; ")) %>% arrange(pt_fenlei) # 将每组 pt 列按 "; " 分隔连接成字符串
drug_teding1 <- drug_teding_unique %>%
  group_by(primaryid) %>%                   # 按 primaryid 分组
  summarise(drug_fenlei = paste(DRUGNAME, collapse = "; ")) # 将每组 drug 列按 "; " 分隔连接成字符串
case_by_case<-merge(demo_teding,drug_teding1,by="primaryid")
case_by_case<-merge(case_by_case,reac_teding,by="primaryid",all = T)
case_by_case<-case_by_case[,c(15,1,5,9,13:14)]
case_by_case<-case_by_case %>% arrange(PT)
case_by_case1<-merge(case_by_case,indi_teding1,by="primaryid",all = T)
case_by_case1<-case_by_case1 %>% arrange(PT)
case_by_case1$PT<-tolower(case_by_case1$PT)
case_by_case1$PT<-toTitleCase(case_by_case1$PT)
write.xlsx(case_by_case1,"FAERS-IMM-缄默症 MUTISM.xlsx")
