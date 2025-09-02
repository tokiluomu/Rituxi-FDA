library(openxlsx)
library(dplyr)

PT <- read.xlsx("三阳PT.xlsx")
IME <- read.xlsx("IME.xlsx")

PT <- PT %>%
  mutate(IME = ifelse(tolower(pt_name_en) %in% tolower(IME$PT.Name), TRUE, FALSE))

write.xlsx(PT,"IME标注后.xlsx")
