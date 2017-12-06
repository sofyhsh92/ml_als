##PRO-ACT data
##script 16

setwd("C:/Users/user/Documents/ml_als")
data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.allforms <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)
rm(data.allforms_leaderboard)
rm(data.allforms_training)
rm(data.allforms_training2)
rm(data.allforms_validation)
unique(data.allforms$form_name)

library(dplyr)
p.lab_test <- filter(data.allforms, form_name == "Lab Test")
p.lab_test$form_name <- NULL
p.lab_test$feature_name <- droplevels(p.lab_test$feature_name)
p.lab_test$feature_delta <- as.numeric(as.character(p.lab_test$feature_delta))
p.lab_test$feature_value <- as.numeric(as.character(p.lab_test$feature_value))


##lab test
sort(unique((p.lab_test$feature_name)))
#ALP
p.lab.alp <- p.lab_test %>%
  filter(feature_name == "Alkaline Phosphatase") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(alp = mean(feature_value)) %>%
  select(SubjectID, alp)
colnames(p.lab.alp) <- c("subject_id", "alp")

#chloride
p.lab.chloride <- p.lab_test %>%
  filter(feature_name == "Chloride") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(chloride = mean(feature_value)) %>%
  select(SubjectID, chloride)
colnames(p.lab.chloride) <- c("subject_id", "chloride")


#creatinine
p.lab.creatinine <- p.lab_test %>%
  filter(feature_name == "Creatinine") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(creatinine = mean(feature_value)) %>%
  select(SubjectID, creatinine)
colnames(p.lab.creatinine) <- c("subject_id", "creatinine")

#ast
p.lab.ast <- p.lab_test %>%
  filter(feature_name == "AST(SGOT)") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(ast = mean(feature_value)) %>%
  select(SubjectID, ast)
colnames(p.lab.ast) <- c("subject_id", "ast")

#neutrophils
p.lab.neutrophils <- p.lab_test %>%
  filter(feature_name == "Neutrophils") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(neutrophils = mean(feature_value)) %>%
  select(SubjectID, neutrophils)
colnames(p.lab.neutrophils) <- c("subject_id", "neutrophils")

#protein
p.lab.protein <- p.lab_test %>%
  filter(feature_name == "Protein") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(protein = mean(feature_value)) %>%
  select(SubjectID, protein)
colnames(p.lab.protein) <- c("subject_id", "protein")

#calcium
p.lab.calcium <- p.lab_test %>%
  filter(feature_name == "Calcium") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(calcium = mean(feature_value)) %>%
  select(SubjectID, calcium)
colnames(p.lab.calcium) <- c("subject_id", "calcium")

#glucose
p.lab.glucose <- p.lab_test %>%
  filter(feature_name == "Glucose") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(glucose = mean(feature_value)) %>%
  select(SubjectID, glucose)
colnames(p.lab.glucose) <- c("subject_id", "glucose")

#bun
p.lab.bun <- p.lab_test %>%
  filter(feature_name == "Blood Urea Nitrogen (BUN)") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(bun = mean(feature_value)) %>%
  select(SubjectID, bun)
colnames(p.lab.bun) <- c("subject_id", "bun")

#bicarb
p.lab.bicarb <- p.lab_test %>%
  filter(feature_name == "Bicarbonate") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(bicarb = mean(feature_value)) %>%
  select(SubjectID, bicarb)
colnames(p.lab.bicarb) <- c("subject_id", "bicarb")

#bilirubin_tot
p.lab.bilirubin_tot <- p.lab_test %>%
  filter(feature_name == "Bilirubin (Total)") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(bilirubin_tot = mean(feature_value)) %>%
  select(SubjectID, bilirubin_tot)
colnames(p.lab.bilirubin_tot) <- c("subject_id", "bilirubin_tot")

#phosphorus
p.lab.phosphorus <- p.lab_test %>%
  filter(feature_name == "Phosphorus") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(phosphorus = mean(feature_value)) %>%
  select(SubjectID, phosphorus)
colnames(p.lab.phosphorus) <- c("subject_id", "phosphorus")

#alt
p.lab.alt <- p.lab_test %>%
  filter(feature_name == "ALT(SGPT)") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(alt = mean(feature_value)) %>%
  select(SubjectID, alt)
colnames(p.lab.alt) <- c("subject_id", "alt")

#triglycerides
p.lab.triglycerides <- p.lab_test %>%
  filter(feature_name == "Triglycerides") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(triglycerides = mean(feature_value)) %>%
  select(SubjectID, triglycerides)
colnames(p.lab.triglycerides) <- c("subject_id", "triglycerides")

#hematocrit
p.lab.hematocrit <- p.lab_test %>%
  filter(feature_name == "Hematocrit") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(hematocrit = mean(feature_value)) %>%
  select(SubjectID, hematocrit)
colnames(p.lab.hematocrit) <- c("subject_id", "hematocrit")

#ck
p.lab.ck <- p.lab_test %>%
  filter(feature_name == "CK") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(ck = mean(feature_value)) %>%
  select(SubjectID, ck)
colnames(p.lab.ck) <- c("subject_id", "ck")

#eosinophils
p.lab.eosinophils <- p.lab_test %>%
  filter(feature_name == "Eosinophils") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(eosinophils = mean(feature_value)) %>%
  select(SubjectID, eosinophils)
colnames(p.lab.eosinophils) <- c("subject_id", "eosinophils")

#lymphocytes
p.lab.lymphocytes <- p.lab_test %>%
  filter(feature_name == "Lymphocytes") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(lymphocytes = mean(feature_value)) %>%
  select(SubjectID, lymphocytes)
colnames(p.lab.lymphocytes) <- c("subject_id", "lymphocytes")

#albumin
p.lab.albumin <- p.lab_test %>%
  filter(feature_name == "Albumin") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(albumin = mean(feature_value)) %>%
  select(SubjectID, albumin)
colnames(p.lab.albumin) <- c("subject_id", "albumin")

#wbc
p.lab.wbc <- p.lab_test %>%
  filter(feature_name == "White Blood Cell (WBC)") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(wbc = mean(feature_value)) %>%
  select(SubjectID, wbc)
colnames(p.lab.wbc) <- c("subject_id", "wbc")

#rbc
p.lab.rbc <- p.lab_test %>%
  filter(feature_name == "Red Blood Cells (RBC)") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(rbc = mean(feature_value)) %>%
  select(SubjectID, rbc)
colnames(p.lab.rbc) <- c("subject_id", "rbc")

#basophils
p.lab.basophils <- p.lab_test %>%
  filter(feature_name == "Basophils") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(basophils = mean(feature_value)) %>%
  select(SubjectID, basophils)
colnames(p.lab.basophils) <- c("subject_id", "basophils")

#hba1c
p.lab.hba1c <- p.lab_test %>%
  filter(feature_name == "HbA1c (Glycated Hemoglobin)") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(hba1c = mean(feature_value)) %>%
  select(SubjectID, hba1c)
colnames(p.lab.hba1c) <- c("subject_id", "hba1c")

#platelets
p.lab.platelets <- p.lab_test %>%
  filter(feature_name == "Platelets") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(platelets = mean(feature_value)) %>%
  select(SubjectID, platelets)
colnames(p.lab.platelets) <- c("subject_id", "platelets")

#cholesterol
p.lab.cholesterol <- p.lab_test %>%
  filter(feature_name == "Total Cholesterol") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(cholesterol = mean(feature_value)) %>%
  select(SubjectID, cholesterol)
colnames(p.lab.cholesterol) <- c("subject_id", "cholesterol")

#sodium
p.lab.sodium <- p.lab_test %>%
  filter(feature_name == "Sodium") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(sodium = mean(feature_value)) %>%
  select(SubjectID, sodium)
colnames(p.lab.sodium) <- c("subject_id", "sodium")

#monocytes
p.lab.monocytes <- p.lab_test %>%
  filter(feature_name == "Monocytes") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(monocytes = mean(feature_value)) %>%
  select(SubjectID, monocytes)
colnames(p.lab.monocytes) <- c("subject_id", "monocytes")

#ggt
p.lab.ggt <- p.lab_test %>%
  filter(feature_name == "Gamma-glutamyltransferase") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(ggt = mean(feature_value)) %>%
  select(SubjectID, ggt)
colnames(p.lab.ggt) <- c("subject_id", "ggt")

#hemoglobin
p.lab.hemoglobin <- p.lab_test %>%
  filter(feature_name == "Hemoglobin") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(hemoglobin = mean(feature_value)) %>%
  select(SubjectID, hemoglobin)
colnames(p.lab.hemoglobin) <- c("subject_id", "hemoglobin")

#urine_glucose
p.lab.urine_glucose <- p.lab_test %>%
  filter(feature_name == "Urine Glucose") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(urine_glucose = mean(feature_value)) %>%
  select(SubjectID, urine_glucose)
colnames(p.lab.urine_glucose) <- c("subject_id", "urine_glucose")

#urine_protein
p.lab.urine_protein <- p.lab_test %>%
  filter(feature_name == "Urine Protein") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(urine_protein = mean(feature_value)) %>%
  select(SubjectID, urine_protein)
colnames(p.lab.urine_protein) <- c("subject_id", "urine_protein")

#urine_ph
p.lab.urine_ph <- p.lab_test %>%
  filter(feature_name == "Urine Ph") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(urine_ph = mean(feature_value)) %>%
  select(SubjectID, urine_ph)
colnames(p.lab.urine_ph) <- c("subject_id", "urine_ph")

#uric_acid
p.lab.uric_acid <- p.lab_test %>%
  filter(feature_name == "Uric Acid") %>%
  filter(feature_delta <= 90 & feature_delta >=0) %>%
  group_by(SubjectID) %>%
  summarise(uric_acid = mean(feature_value)) %>%
  select(SubjectID, uric_acid)
colnames(p.lab.uric_acid) <- c("subject_id", "uric_acid")

p.lab <- alsfrs_label %>% 
  left_join(p.lab.albumin, by = "subject_id") %>%
  left_join(p.lab.alp, by = "subject_id") %>%
  left_join(p.lab.alt, by = "subject_id") %>%
  left_join(p.lab.ast, by = "subject_id") %>%
  left_join(p.lab.basophils, by = "subject_id") %>%
  left_join(p.lab.bicarb, by = "subject_id") %>%
  left_join(p.lab.bilirubin_tot, by = "subject_id") %>%
  left_join(p.lab.bun, by = "subject_id") %>%
  left_join(p.lab.calcium, by = "subject_id") %>%
  left_join(p.lab.chloride, by = "subject_id") %>%
  left_join(p.lab.cholesterol, by = "subject_id") %>%
  left_join(p.lab.ck, by = "subject_id") %>%
  left_join(p.lab.creatinine, by = "subject_id") %>%
  left_join(p.lab.eosinophils, by = "subject_id") %>%
  left_join(p.lab.ggt, by = "subject_id") %>%
  left_join(p.lab.glucose, by = "subject_id") %>%
  left_join(p.lab.hba1c, by = "subject_id") %>%
  left_join(p.lab.hematocrit, by = "subject_id") %>%
  left_join(p.lab.hemoglobin, by = "subject_id") %>%
  left_join(p.lab.lymphocytes, by = "subject_id") %>%
  left_join(p.lab.monocytes, by = "subject_id") %>%
  left_join(p.lab.neutrophils, by = "subject_id") %>%
  left_join(p.lab.phosphorus, by = "subject_id") %>%
  left_join(p.lab.platelets, by = "subject_id") %>%
  left_join(p.lab.protein, by = "subject_id") %>%
  left_join(p.lab.rbc, by = "subject_id") %>%  
  left_join(p.lab.sodium, by = "subject_id") %>%
  left_join(p.lab.triglycerides, by = "subject_id") %>%
  left_join(p.lab.uric_acid, by = "subject_id") %>%
  left_join(p.lab.urine_glucose, by = "subject_id") %>%
  left_join(p.lab.urine_ph, by = "subject_id") %>%
  left_join(p.lab.urine_protein, by = "subject_id") %>%
  left_join(p.lab.wbc, by = "subject_id") %>%
  select(-label)

#after observation, drop some values
p.lab$urine_glucose <- NULL
p.lab$urine_protein <- NULL
  
rm(p.lab.albumin)
rm(p.lab.alp)
rm(p.lab.alt)
rm(p.lab.ast)
rm(p.lab.basophils)
rm(p.lab.bicarb)
rm(p.lab.bilirubin_tot)
rm(p.lab.bun)
rm(p.lab.calcium)
rm(p.lab.chloride)
rm(p.lab.cholesterol)
rm(p.lab.ck)
rm(p.lab.creatinine)
rm(p.lab.eosinophils)
rm(p.lab.ggt)
rm(p.lab.glucose)
rm(p.lab.hba1c)
rm(p.lab.hematocrit)
rm(p.lab.hemoglobin)
rm(p.lab.lymphocytes)
rm(p.lab.monocytes)
rm(p.lab.neutrophils)
rm(p.lab.phosphorus)
rm(p.lab.platelets)
rm(p.lab.protein)
rm(p.lab.rbc)
rm(p.lab.sodium)
rm(p.lab.triglycerides)
rm(p.lab.uric_acid)
rm(p.lab.urine_glucose)
rm(p.lab.urine_ph)
rm(p.lab.urine_protein)
rm(p.lab.wbc)
rm(p.lab_test)
