####治疗与诱发时间作图
library(openxlsx)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(cowplot)
library(extrafont)#字体
library(dplyr)
#抗雄
#以下代码的前提建立在ther文件中不存在同一id同一药物不在同一天开始治疗的情况下（即以下情况会被过度去重：同一id在同一天施用同一药物，但在不同天停止治疗）
#该种情况可在zhiliao_fil与zhiliao_fil_pt去重前的重复中检出
###############################################################抗雄

##将ther与drug表根据id与drugseq合并，并取PS（默认PS导致不良反应）
read.csv("FAERS-IMM-ther.csv")->ther
read.csv("FAERS-IMM-drug.csv")->drug
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
write.xlsx(zhiliao_fil,"FAERS-IMM-治疗持续时间.xlsx")                            
##结合reac文件对应pt
reac<-read.csv("FAERS-IMM-reac.csv")
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

write.xlsx(zhiliao_fil_pt,"FAERS-IMM-治疗持续时间(PT).xlsx")
read.xlsx("FAERS-IMM-治疗持续时间(PT).xlsx")->zhiliao_fil_pt

##计算每个pt治疗时长平均值
pt_mean_zhiliao <- zhiliao_fil_pt %>%
  filter(PT != "") %>%           # 移除 PT 列中为空字符串的行
  group_by(PT) %>%
  summarise(mean_zhiliao = mean(DUR),count_zhiliao=n()) %>% arrange(desc(count_zhiliao))

###读取诱发时间数据并合并
youfa<-read.xlsx("FAERS-IMM-不良反应诱发时间(每个PT的不良反应诱发时间).xlsx")
#将诱发里的起始时间转换为年月日
youfa$START_DT<-as.numeric(format(as.Date(youfa$START_DT,origin = "1899-12-30"),"%Y%m%d"))
###检查youfa的重复
duplicates <- youfa[duplicated(paste(youfa$primaryid, youfa$PT,youfa$START_DT,youfa$EVENT_DT,youfa$TIME)), ]#四项筛选重复的过程中，最后重复七次的原因是ther，reac，drug文件分别重复两次（同一数据不同季度上报两次），相乘为8，重复不为7次多是reac文件重复报道而其余两个文件没有
#****检查是否相等，不相等便在去重条件上加一条DUR（针对诱发，只要demo文件没有重复且event_dt有多个，则duplicates1与duplicates2一定相等）
duplicates1 <- youfa[duplicated(paste(youfa$primaryid, youfa$PT,youfa$START_DT,youfa$TIME)), ]
duplicates2 <- youfa[duplicated(paste(youfa$primaryid, youfa$PT,youfa$START_DT)), ]
youfa<-youfa %>% distinct(primaryid,PT,START_DT,.keep_all = T)#,EVENT_DT,TIME
zhi_you<-merge(zhiliao_fil_pt,youfa,by=c("PT","primaryid","START_DT"))#若前面去重前重复数均相等，这里便能顺利合并，若有重复会出现merge时相乘
zhi_you<-na.omit(zhi_you)#合并治疗与诱发原始数据
a<-nrow(youfa)+nrow(zhiliao_fil_pt)-nrow(zhi_you)
pt_mean_youfa <- youfa %>%
  filter(PT != "") %>%           # 移除 PT 列中为空字符串的行
  group_by(PT) %>%
  summarise(mean_youfa = mean(TIME),count_youfa=n()) %>% arrange(desc(count_youfa))
pt_cn_mean<-merge(pt_mean_zhiliao,pt_mean_youfa,by="PT",all = T) %>% arrange(desc(count_youfa))
###添加中文
pt_cn_mean<-merge(pt_cn_mean,reac[,c(1,3)],by="PT")
pt_cn_mean<-pt_cn_mean[!duplicated(pt_cn_mean$PT),] %>% arrange(desc(count_youfa))
###接下来可以对照阳性再筛选
read.xlsx("FAERS-IMM-不良反应信号(PT).xlsx")->ror_yang#该文件只包含阳性pt
pt_cn_mean$yang<-ifelse(pt_cn_mean$PT %in% ror_yang$PT,"阳","阴")
write.xlsx(pt_cn_mean,"FAERS-IMM-治疗与诱发时间.xlsx")

##分别取在top20ror里与诱发时间数据量排top20的pt（最后选取top20的AE作图）
pt_cn_mean<-read.xlsx("FAERS-IMM-治疗与诱发时间.xlsx")
top20AE<-read.xlsx("FAERS-IMM-不良反应信号(PT).xlsx")
pt_cn_mean_top20AE<-pt_cn_mean[pt_cn_mean$PT %in% top20AE$PT,]
pt_cn_mean_top20youfa<-pt_cn_mean[which(pt_cn_mean$yang=="阳"),]
pt_cn_mean_top20youfa<-pt_cn_mean_top20youfa[1:20,]#有些不够20个
pt_cn_mean_top20youfa<-pt_cn_mean_top20youfa[!grepl("NA",rownames(pt_cn_mean_top20youfa)),]
write.xlsx(pt_cn_mean_top20AE,"FAERS-IMM-治疗与诱发时间(top20AE).xlsx")
write.xlsx(pt_cn_mean_top20youfa,"FAERS-IMM-治疗与诱发时间(top20youfa).xlsx")


####治疗与诱发时间作图


##读取AE数排名前二十的pt作图
read.xlsx("FAERS-IMM-治疗与诱发时间(top20AE).xlsx")->pt_cn_mean_top20AE
top20AE<-read.xlsx("FAERS-IMM-不良反应信号(PT).xlsx")
pt_cn_mean_top20AE<-merge(top20AE,pt_cn_mean_top20AE,by="PT")
pt_cn_mean_top20AE<-pt_cn_mean_top20AE %>% arrange(a)#a代表目标药物目标pt
pt_cn_mean_top20AE<-pt_cn_mean_top20AE[complete.cases(pt_cn_mean_top20AE[, 28:31]), ]#只保留诱发与治疗时间都有数据的行
##pt转首字母大写
capitalize_first_word <- function(phrase) {
  sub("^(\\w)(\\w*)", "\\U\\1\\L\\2", phrase, perl = TRUE)
}
pt_cn_mean_top20AE$PT<-tolower(pt_cn_mean_top20AE$PT)
pt_cn_mean_top20AE$PT<-capitalize_first_word(pt_cn_mean_top20AE$PT)
top20AE$PT<-tolower(top20AE$PT)
top20AE$PT<-capitalize_first_word(top20AE$PT)
top20AE<-top20AE[which(top20AE$PT %in% pt_cn_mean_top20AE$PT),]
##ggplot2默认按照字母顺序或因子水平排序。要确保柱子按照数据框中的顺序排列，需要将 x 变量设置为一个有序因子。
desired_order <- rev(top20AE$PT)#因为绘图反转坐标轴，所以得反转pt排列顺序
pt_cn_mean_top20AE$PT <- factor(pt_cn_mean_top20AE$PT, levels = desired_order)

#pt_cn_mean_top20youfa$PT<-factor(pt_cn_mean_top20youfa$PT,levels = pt_cn_mean_top20youfa$PT)
##AE数排名
p1 <- ggplot(pt_cn_mean_top20AE, aes(x = PT)) +
  geom_bar(aes(y = mean_zhiliao, fill = "Duration of therapy"), stat = "identity", width = 0.2) + # 柱状图
  geom_point(aes(y = mean_youfa, color = "Time to onset"), size = 2, shape = 18) +   # 散点图
  scale_fill_manual(name="",values = c("Duration of therapy" = "#C5C5C5")) +
  scale_color_manual(name="",values = c("Time to onset" = "#000000")) +
  coord_flip(clip = "off") +                                                                    # 反转坐标轴
  labs(x = "PT", y = "Time (days)", title = "Average Time-to-onset and Treatment Duration of PTs Associated with Rituximab") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman",size = 12), legend.position = "right",legend.spacing.y = unit(-0.5, 'cm'),# 调整图例项之间的垂直间距，因为相当于画了两张图
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),# 添加四周边框
        plot.title = element_text(hjust = 0.5),  # 标题居中 
        #axis.text.x = element_text(margin = margin(2,1,0,1,'cm'))  
  )+
  #scale_x_discrete(expand = expansion(mult = c(0.5, 0.5))) + # 去除 x 轴的额外空白
  scale_y_continuous(expand = expansion(add = c(0, 0)),limits = c(0, 2000)) # 去除 y 轴的额外空白
p1
ggsave('FAERS-IMM-Average Time-to-onset and Treatment Duration of PTs Associated with Rituximab(ALL-AE)1.png',p1,dpi=600,width=8,height=12,bg="white")
