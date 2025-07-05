library(openxlsx)
library(dplyr)
library(stringr)

data <- read.xlsx("PT平均治疗+诱发时间.xlsx")
other <- read.xlsx("说明书不良反应.xlsx")

filtered_data <- data %>%
  filter(mean_duration > 720, count > 3) %>%
  filter(!PT %in% other$PT)


PT_positive <- read.xlsx("利妥昔单抗对所有疾病的PT阳性.xlsx")
PT_positive$PT <- str_to_title(PT_positive$PT)
# 筛选 filtered_data 中 PT 与 PT_positive 中 PT 匹配的行
matched_data <- filtered_data %>%
  filter(PT %in% PT_positive$PT)

write.xlsx(matched_data,"治疗大于720天排除说明书的所有阳性PT.xlsx")
