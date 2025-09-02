library(easyFAERS)
  
# 读取药物名称和疾病名称
drug_names <- readLines("/Users/wangyida/Desktop/drug/JXY/drug_name.txt")
illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-Congenital.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/FAERS-CON")
tryCatch({
  AFun(
    drug = drug_names,  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    drugEffect = c('PS')  # 药物效果参数
  )
})

drug_names <- readLines("/Users/wangyida/Desktop/drug/JXY/drug_name.txt")
illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-immunity.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/FAERS-IMM")
tryCatch({
  AFun(
    drug = drug_names,  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    drugEffect = c('PS')  # 药物效果参数
  )
})

drug_names <- readLines("/Users/wangyida/Desktop/drug/JXY/drug_name.txt")
illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-transplant.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/FAERS-TRA")
tryCatch({
  AFun(
    drug = drug_names,  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    drugEffect = c('PS')  # 药物效果参数
  )
})

drug_names <- readLines("/Users/wangyida/Desktop/drug/JXY/drug_name.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/FAERS-drugtime")
tryCatch({
  AFun(
    drug = drug_names,  # 直接将所有药物名称传入
    path = folder_path,  # 动态路径
    drugEffect = c('PS')  # 药物效果参数
  )
})

library(easyJADER)
illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-Congenital.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/JADER-CON")
tryCatch({
  AFun(
    drug = c("ＲＩＴＵＸＩＭＡＢ","T_RITUXIMAB","リツキシマブ（遺伝子組換え）［後続１］","リツキシマブ（遺伝子組換え）［後続２］","Rituximab bs","リツキシマブバイオシミラー1","リツキシマブ（遺伝子組換え）","リツキシマブ"),  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    drugEffect = c('PS')  # 药物效果参数
  )
})

illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-immunity.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/JADER-IMM")
tryCatch({
  AFun(
    drug = c("ＲＩＴＵＸＩＭＡＢ","T_RITUXIMAB","リツキシマブ（遺伝子組換え）［後続１］","リツキシマブ（遺伝子組換え）［後続２］","Rituximab bs","リツキシマブバイオシミラー1","リツキシマブ（遺伝子組換え）","リツキシマブ"),  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    drugeffect = c('PS')  # 药物效果参数
  )
})

illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-transplant.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/JADER-TRA")
tryCatch({
  AFun(
    drug = c("ＲＩＴＵＸＩＭＡＢ","T_RITUXIMAB","リツキシマブ（遺伝子組換え）［後続１］","リツキシマブ（遺伝子組換え）［後続２］","Rituximab bs","リツキシマブバイオシミラー1","リツキシマブ（遺伝子組換え）","リツキシマブ"),  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    drugeffect = c('PS')  # 药物效果参数
  )
})

library(easyCANADA)
illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-Congenital.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/CANADA-CON")
tryCatch({
  AFun(
    drug = c("RITUXIMAB(GENETICAL RECOMBINATION)","RITUXIMAB PVVR","RITUXIMAB I-131","RITUXIMAB BIOSIMILAR 2","RITUXIMAB ARRX","RITUXIMAB ABBS","RITUXIMAB","RITTUXIMAB","PF-05280586 (RITUXIMAB BIOSIMILAR)","MK-5180","GP2013 (RITUXIMAB BIOSIMILAR)","GP2013","BI695500","BI 695500","131I-RITUXIMAB","RITUXIMAB GP2013","RUXIENCE (RITUXIMAB)","RUXIENCE [RITUXIMAB PVVR]","RITUXIMABE","VORHYALURONIDASE ALFA"),  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    drugeffect = c('Suspect'),  # 药物效果参数
    year=c(19900101,20241031)
  )
})

illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-immunity.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/CANADA-IMM")
tryCatch({
  AFun(
    drug = c("RITUXIMAB(GENETICAL RECOMBINATION)","RITUXIMAB PVVR","RITUXIMAB I-131","RITUXIMAB BIOSIMILAR 2","RITUXIMAB ARRX","RITUXIMAB ABBS","RITUXIMAB","RITTUXIMAB","PF-05280586 (RITUXIMAB BIOSIMILAR)","MK-5180","GP2013 (RITUXIMAB BIOSIMILAR)","GP2013","BI695500","BI 695500","131I-RITUXIMAB","RITUXIMAB GP2013","RUXIENCE (RITUXIMAB)","RUXIENCE [RITUXIMAB PVVR]","RITUXIMABE","VORHYALURONIDASE ALFA"),  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    drugeffect = c('Suspect'),  # 药物效果参数
    year=c(19900101,20241031)
  )
})

illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-transplant.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/CANADA-TRA")
tryCatch({
  AFun(
    drug = c("RITUXIMAB(GENETICAL RECOMBINATION)","RITUXIMAB PVVR","RITUXIMAB I-131","RITUXIMAB BIOSIMILAR 2","RITUXIMAB ARRX","RITUXIMAB ABBS","RITUXIMAB","RITTUXIMAB","PF-05280586 (RITUXIMAB BIOSIMILAR)","MK-5180","GP2013 (RITUXIMAB BIOSIMILAR)","GP2013","BI695500","BI 695500","131I-RITUXIMAB","RITUXIMAB GP2013","RUXIENCE (RITUXIMAB)","RUXIENCE [RITUXIMAB PVVR]","RITUXIMABE","VORHYALURONIDASE ALFA"),  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    drugeffect = c('Suspect'),  # 药物效果参数
    year=c(19900101,20241031)
  )
})

library(easyFAERS)
  
# 读取药物名称和疾病名称
drug_names <- readLines("/Users/wangyida/Desktop/drug/JXY/drug_name.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/FAERS-720")
tryCatch({
  AFun(
    drug = drug_names,  # 直接将所有药物名称传入
    path = folder_path,  # 动态路径
    primaryid=T,
    drugEffect = c('PS')  # 药物效果参数
  )
})

drug_names <- readLines("/Users/wangyida/Desktop/drug/JXY/drug_name.txt")
folder_path <- file.path("/Users/wangyida/Desktop/drug/JXY/FAERS-720-imm")
illness_names <- readLines("/Users/wangyida/Desktop/drug/JXY/illness-immunity.txt")
tryCatch({
  AFun(
    drug = drug_names,  # 直接将所有药物名称传入
    illnesses = illness_names,  # 当前的疾病
    path = folder_path,  # 动态路径
    primaryid=T,
    drugEffect = c('PS')  # 药物效果参数
  )
})
