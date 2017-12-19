## script 18
### ALL IN ONE ###

setwd("C:/Users/user/Documents/ml_als")
library(dplyr)
library(tidyr)

## new data (length : 18 months, ALSFRS_Delta 0 ~ 548 // data points >= 6 // last point alsfrs_delta >= 180)
alsfrs.ori <- read.csv("alsfrs.ori.csv")
colnames(alsfrs.ori) <- alsfrs.ori[1, ]
alsfrs <- cbind(subject_id = alsfrs.ori[, 1], alsfrs.ori[, 13:561])
rm(alsfrs.ori)
#deleting "X" from each subject_id  
alsfrs$subject_id <- as.character(alsfrs$subject_id)
for (i in 2:nrow(alsfrs)) {
  alsfrs$subject_id[i] <- sub("^X", "", alsfrs$subject_id[i])
  
}
#data points >= 6
c <- rep(TRUE, nrow(alsfrs))
for(i in 2:nrow(alsfrs)) {
  if(sum(!is.na(alsfrs[i, ])) <= 6) {
    c[i] <- FALSE
  }
}
alsfrs <- alsfrs[c, ]

#last point alsfrs_delta >= 180
c <- rep(TRUE, nrow(alsfrs))
als180 <- cbind(subject_id = alsfrs[, 1], alsfrs[, 182:550])
for(i in 2:nrow(alsfrs)) {
  if(sum(!is.na(als180[i, ])) <= 1 ) {
    c[i] <- FALSE
  }
}
rm(als180)
alsfrs <- alsfrs[c, ]
rm(i)
rm(c)

rownames(alsfrs) <- alsfrs[, 1]
alsfrs[, 1] <- NULL

#error due to all same ALSFRS
h <- rep(NA, nrow(alsfrs))
h[1] <- 1
for(i in 2:nrow(alsfrs)) {
  if (!(sum(!duplicated(alsfrs[i, ][!is.na(alsfrs[i, ])])) == 1)) {
    h[i] <- i
  }
}
h <- h[!is.na(h)]

alsfrs <- alsfrs[h, ]

#calculation of relative errors of Weibull and linear
#omit i = 330, 2121, 4032, 4140, 4480 (singular gradient)
alsfrs <- alsfrs[-c(330, 2121, 4032, 4140, 4480), ]
b <- 0
rel.err.diff <- 0
library(minpack.lm)
for (i in 2:nrow(alsfrs)) {
  
  x <- alsfrs[1, ]
  y <- alsfrs[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  fit.w <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 125))
  rmse.w <- sqrt( fit.w$m$deviance() / length(x) )
  fit.l <- lm(y ~ x)
  res.l <- summary(fit.l)$residuals
  rmse.l <- sqrt(sum(res.l^2)/length(x))
  rel.err.w <- rmse.w / sd(y)
  rel.err.l <- rmse.l / sd(y)
  rel.err.diff[i] <- rel.err.l - rel.err.w
  b[i] <- as.numeric(fit.w$m$getPars()[3])
  
}

b_and_rel.err <- data.frame(cbind(subject_id = as.numeric(rownames(alsfrs)[-1]), b = b[-1], rel.err.diff = rel.err.diff[-1]))
quantile(rel.err.diff, 0.7)
plot(density(b_and_rel.err[,3]), main = "relative error difference (linear - Weibull)", xlab = "")
abline(v=0.06926119, col = "orange")

#labeling based on density plot of relative error difference
label <- rep("E", nrow(alsfrs))
label[1] <- NA

for(i in 2:nrow(alsfrs)) {
  if (rel.err.diff[i] > 0.06926119) {
    label[i] <- "W"
  }
}

table(label)

b_and_rel.err$label <- NULL
b_and_rel.err <- mutate(b_and_rel.err, label = label[-1])
plot(density(filter(b_and_rel.err, label != "E")$b), xlim = c(-1, 15), main = "density plot of b", xlab="")
abline(v=1.55, col = "orange")

for(i in 1:nrow(b_and_rel.err)){
  if(b_and_rel.err$label[i] == "W" & b_and_rel.err$b[i] > 1.55){
    b_and_rel.err$label[i] <- "W_concave"
  } else if (b_and_rel.err$label[i] == "W" & b_and_rel.err$b[i] <= 1.55){
    b_and_rel.err$label[i] <- "W_convex"
  }
}

table(b_and_rel.err$label)

alsfrs_label <- b_and_rel.err[, c(1, 4)]

#go to script 15 to see graphs

rm(alsfrs)
rm(b_and_rel.err)
rm(fit.l)
rm(fit.w)
rm(b)
rm(h)
rm(i)
rm(label)
rm(rel.err.l)
rm(rel.err.w)
rm(rel.err.diff)
rm(res.l)
rm(rmse.l)
rm(rmse.w)
rm(x)
rm(y)
#______________________________________________________________________________________________________

######creating data for machine learning

###AlsHistory (Thanks to JKW)
alshistory<-read.csv("AlsHistory_r.csv")

# OnsetSite 변수 추가 과정.
for (i in 1:length(alshistory$subject_id))
{
  if (alshistory$Site_of_Onset[i]=="Onset: Bulbar"){
    alshistory$Onsetsite[i]="Bulbar"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Limb"){
    alshistory$Onsetsite[i]="Limb"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Limb and Bulbar"){
    alshistory$Onsetsite[i]="Limb and Bulbar"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Other"){
    alshistory$Onsetsite[i]="Other"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Spine"){
    alshistory$Onsetsite[i]="Limb"
  }
  if (!is.na(alshistory$Site_of_Onset___Bulbar[i])) {
    if (alshistory$Site_of_Onset___Bulbar[i]==1) {
      alshistory$Onsetsite[i]="Bulbar"
    }
  }
  if (!is.na(alshistory$Site_of_Onset___Limb[i])) {
    if (alshistory$Site_of_Onset___Limb[i]==1) {
      alshistory$Onsetsite[i]="Limb"
    }
  }
  if (!is.na(alshistory$Site_of_Onset___Bulbar[i]) & !is.na(alshistory$Site_of_Onset___Limb[i])){
    if (alshistory$Site_of_Onset___Bulbar[i]==1 & alshistory$Site_of_Onset___Limb[i]==1) {
      alshistory$Onsetsite[i]="Limb and Bulbar"
    }
  }
  
}
alshistory$Onsetsite <- factor(alshistory$Onsetsite)


# DiagnosisDelta >0인 것들은 오류일테니 NA처리
alshistory$Diagnosis_Delta[alshistory$Diagnosis_Delta>0]=NA

# Make 'Mode' function
Mode <- function(x){
  x <- x[!is.na(x)]
  uqx <- unique(x)
  uqx[which.max(tabulate(match(x,uqx)))]
}

alshistory <- summarize(group_by(alshistory, subject_id), Onsetsite = Mode(Onsetsite), Onset_Delta = Mode(Onset_Delta),Diagnosis_Delta = Mode(Diagnosis_Delta))

alshistory <- alshistory %>%
  mutate(onset_to_diagnosis = Diagnosis_Delta - Onset_Delta) %>%
  select(subject_id, Onsetsite, Onset_Delta, onset_to_diagnosis)
rm(Mode)
#done (alshistory : Onsetsite, Onset_Delta, onset_to_diagnosis \\ NA remains in Onset_Delta, onset_to_diagnosis)

###ALSFRS
alsfrs <- read.csv("alsfrs.csv")
#merge Q5a and Q5b
for(i in 1:nrow(alsfrs)) {
  if(is.na(alsfrs$Q5a_Cutting_without_Gastrostomy[i])) {
    alsfrs$Q5a_Cutting_without_Gastrostomy[i] <- alsfrs$Q5b_Cutting_with_Gastrostomy[i]
  }
}
#drop Q5b
alsfrs$Q5b_Cutting_with_Gastrostomy <- NULL
#merge Q10 and R1
for(i in 1:nrow(alsfrs)) {
  if(is.na(alsfrs$Q10_Respiratory[i])) {
    alsfrs$Q10_Respiratory[i] <- alsfrs$R_1_Dyspnea[i]
  }
}
#drop everything except Q1~Q10 and ALSFRS_Delta
alsfrs[ , 13:19] <- NULL

#filter for 0~90 ALSFRS_Delta
alsfrs90 <- filter(alsfrs, ALSFRS_Delta <= 90 & ALSFRS_Delta >= 0)
alsfrs90 <- na.omit(alsfrs90)
colnames(alsfrs90) <- c("subject_id", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "ALSFRS_Delta")
alsfrs90 <- alsfrs90 %>%
  group_by(subject_id) %>%
  mutate(Q1_diff = diff(range(Q1)), Q1_max = max(Q1), Q2_diff = diff(range(Q2)), Q2_max = max(Q2), Q3_diff = diff(range(Q3)), Q3_max = max(Q3), Q4_diff = diff(range(Q4)), Q4_max = max(Q4), Q5_diff = diff(range(Q5)), Q5_max = max(Q5), Q6_diff = diff(range(Q6)), Q6_max = max(Q6), Q7_diff = diff(range(Q7)), Q7_max = max(Q7), Q8_diff = diff(range(Q8)), Q8_max = max(Q8), Q9_diff = diff(range(Q9)), Q9_max = max(Q9), Q10_diff = diff(range(Q10)), Q10_max = max(Q10)) %>%
  mutate(range_alsfrs_delta = diff(range(ALSFRS_Delta))) %>%
  mutate(alsfrs_total = Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10) %>%
  left_join(alshistory, by = "subject_id") %>%
  mutate(preslope = (alsfrs_total - 39) * 30 / (-Onset_Delta + ALSFRS_Delta) ) %>%  #alshistory must be calculated in advance
  mutate(movement = (Q8 <=1 | Q6 <=1),
         swallowing = (Q3 <=1),
         communicating = (Q1 <= 1 | Q4 <= 1),
         breathing = (Q10 <= 1)) %>%
  mutate(mitos = movement + swallowing + communicating + breathing)

alsfrs_last <- alsfrs90 %>%
  group_by(subject_id) %>%
  mutate(reverse_rank = rank(-ALSFRS_Delta)) %>%
  filter(reverse_rank == 1) %>%
  select(subject_id, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, alsfrs_total)
colnames(alsfrs_last) <- c("subject_id", "Q1_last", "Q2_last", "Q3_last", "Q4_last", "Q5_last", "Q6_last", "Q7_last", "Q8_last", "Q9_last", "Q10_last", "alsfrs_total_last")

alsfrs_first <- alsfrs90 %>%
  group_by(subject_id) %>%
  mutate(rank = rank(ALSFRS_Delta)) %>%
  filter(rank == 1) %>%
  select(subject_id, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, alsfrs_total)
colnames(alsfrs_first) <- c("subject_id", "Q1_first", "Q2_first", "Q3_first", "Q4_first", "Q5_first", "Q6_first", "Q7_first", "Q8_first", "Q9_first", "Q10_first", "alsfrs_total_first")

alsfrs90 <- alsfrs90 %>%
  mutate(rank = rank(ALSFRS_Delta)) %>%
  filter(rank == 1) %>%
  left_join(alsfrs_last, by = "subject_id") %>%
  left_join(alsfrs_first, by = "subject_id") %>%
  mutate(alsfrs_slope = (alsfrs_total_last - alsfrs_total_first) / range_alsfrs_delta, 
         Q1_slope = (Q1_last - Q1_first) / range_alsfrs_delta, 
         Q2_slope = (Q2_last - Q2_first) / range_alsfrs_delta,
         Q3_slope = (Q3_last - Q3_first) / range_alsfrs_delta,
         Q4_slope = (Q4_last - Q4_first) / range_alsfrs_delta,
         Q5_slope = (Q5_last - Q5_first) / range_alsfrs_delta,
         Q6_slope = (Q6_last - Q6_first) / range_alsfrs_delta,
         Q7_slope = (Q7_last - Q7_first) / range_alsfrs_delta,
         Q8_slope = (Q8_last - Q8_first) / range_alsfrs_delta,
         Q9_slope = (Q9_last - Q9_first) / range_alsfrs_delta,
         Q10_slope = (Q10_last - Q10_first) / range_alsfrs_delta
         ) %>%
  select(subject_id, Q1_max, Q1_diff, Q2_max, Q2_diff, Q3_max, Q3_diff, Q4_max, Q4_diff, Q5_max, Q5_diff, Q6_max, Q6_diff, Q7_max, Q7_diff, Q8_max, Q8_diff, Q9_max, Q9_diff, Q10_max, Q10_diff, Q1_slope, Q2_slope, Q3_slope, Q4_slope, Q5_slope, Q6_slope, Q7_slope, Q8_slope, Q9_slope, Q10_slope, alsfrs_total, alsfrs_slope, preslope, mitos)
rm(alsfrs)
rm(alsfrs_first)
rm(alsfrs_last)
rm(i)
#done(alsfrs : Q1_max ~ Q10_max, Q1_diff ~ Q10_diff, Q1_slope ~ Q10_slope, alsfrs_slope, alsfrs_total, preslope, mitos)

###demographics
#PROACT data
data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.allforms <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)
rm(data.allforms_leaderboard)
rm(data.allforms_training)
rm(data.allforms_training2)
rm(data.allforms_validation)

p.demographics <- filter(data.allforms, form_name == "Demographic")
p.demographics$form_name <- NULL
p.demographics$feature_name <- droplevels(p.demographics$feature_name)
unique(p.demographics$feature_name)
p.demographics$feature_unit <- droplevels(p.demographics$feature_unit)
p.demographics$feature_unit <- NULL
p.demographics$feature_delta <- NULL
p.demographics <- p.demographics %>%
  spread(feature_name, feature_value)
colnames(p.demographics) <- c("subject_id", "age", "gender", "race")
p.demographics$age <- as.numeric(as.character(p.demographics$age))
p.demographics$gender <- as.factor(as.character(p.demographics$gender))
p.demographics$gender <- droplevels(p.demographics$gender)
levels(p.demographics$gender)
p.demographics$race <- as.factor(as.character(p.demographics$race))
p.demographics$race <- droplevels(p.demographics$race)
levels(p.demographics$race)
#done(p.demographics: age, gender, race)

###FamilyHx
p.familyhx <- filter(data.allforms, form_name == "FamilyHx")
p.familyhx$form_name <- NULL
p.familyhx$feature_name <- droplevels(p.familyhx$feature_name)
unique(p.familyhx$feature_name)
p.familyhx$feature_name <- NULL
p.familyhx$feature_unit <- NULL
p.familyhx$feature_delta <- NULL
p.familyhx$feature_value <- as.factor(as.character(p.familyhx$feature_value))
p.familyhx$feature_value <- droplevels(p.familyhx$feature_value)
levels(p.familyhx$feature_value)
colnames(p.familyhx) <- c("subject_id", "familyhx")
#done(p.familyhx: familyhx) <- too little data (omit)
rm(p.familyhx)

###vitalsigns 
vitalsigns <- read.csv("VitalSigns.csv")
vital <- filter(vitalsigns, Vital_Signs_Delta >= 0 & Vital_Signs_Delta <= 90)
#weight to kg, height to cm (thanks to JKW)
vital$Weight[vital$Weight_Units=="Pounds"] <- vital$Weight[vital$Weight_Units=="Pounds"] * 0.453592
vital$Height[vital$Height_Units=="Inches"] <- vital$Height[vital$Height_Units=="Inches"] * 2.54
#diastolic, systolic pressure
for (i in 1:nrow(vital)) {
  if(!is.na(vital$Supine_BP_Diastolic[i])) {
    vital$Blood_Pressure_Diastolic[i] <- (vital$Supine_BP_Diastolic[i] + vital$Standing_BP_Diastolic[i]) / 2
    vital$Blood_Pressure_Systolic[i] <- (vital$Supine_BP_Systolic[i] + vital$Standing_BP_Systolic[i]) / 2
    vital$Pulse[i] <- ( vital$Supine_Pulse[i] + vital$Standing_Pulse[i] ) / 2
  }
}
vital_pressure <- select(vital, subject_id, Blood_Pressure_Diastolic, Blood_Pressure_Systolic, Pulse)
vital_pressure <- vital_pressure %>%
  group_by(subject_id) %>%
  summarize(diastolic_p = mean(Blood_Pressure_Diastolic, na.rm=TRUE),
            systolic_p = mean(Blood_Pressure_Systolic, na.rm=TRUE),
            heart_rate = mean(Pulse, na.rm=TRUE))

#weight
vital_weight <- vital %>%
  select(subject_id, Weight, Vital_Signs_Delta) %>%
  filter(Vital_Signs_Delta >=0 & Vital_Signs_Delta <=90) %>%
  na.omit() %>%
  group_by(subject_id) %>%
  mutate(n = n()) %>%
  filter(!(n == 1)) %>%
  mutate(rank = rank(Vital_Signs_Delta)) %>%
  mutate(reverse_rank = rank(-Vital_Signs_Delta))
vital_initial_weight <- vital_weight %>%
  filter(rank == 1) %>%
  mutate(initial_weight = Weight) %>%
  select(subject_id, initial_weight)
vital_last_weight <- vital_weight %>%
  filter(reverse_rank == 1) %>%
  mutate(last_weight = Weight) %>%
  select(subject_id, last_weight)
vital_two_weights <- left_join(vital_initial_weight, vital_last_weight, by ="subject_id")
rm(vital_initial_weight)
rm(vital_last_weight)
vital_weight <- vital_two_weights %>%
  mutate(weight_difference_ratio = (initial_weight - last_weight) / initial_weight) %>%
  select(subject_id, initial_weight, weight_difference_ratio)
rm(vital_two_weights)

#height
vital_height <- vitalsigns %>%
  select(subject_id, Height) %>%
  group_by(subject_id) %>%
  summarize(height = mean(Height, na.rm=TRUE)) %>%
  filter(!is.nan(height))

#respiratory rate
vital_respiratory_rate <- vitalsigns %>%
  select(subject_id, Respiratory_Rate) %>%
  group_by(subject_id) %>%
  summarize(respiratory_rate = mean(Respiratory_Rate, na.rm=TRUE)) %>%
  filter(!is.nan(respiratory_rate))

#temperature
vital_temperature <- vitalsigns %>%
  select(subject_id, Temperature) %>%
  group_by(subject_id) %>%
  summarize(temperature = mean(Temperature, na.rm=TRUE)) %>%
  filter(!is.nan(temperature))

rm(vital)
rm(vitalsigns)
rm(i)
vitals <- vital_height %>%
  full_join(vital_pressure, by = "subject_id") %>%
  full_join(vital_respiratory_rate, by = "subject_id") %>%
  full_join(vital_temperature, by = "subject_id") %>%
  full_join(vital_weight, by = "subject_id")
rm(vital_height)
rm(vital_pressure)
rm(vital_respiratory_rate)
rm(vital_temperature)
rm(vital_weight)
#done(vitals : height, initial_weight, weight_difference_ratio, diastolic_p, systolic_p, heart_rate, respiratory_rate, temperature)


###Lab Test
p.lab_test <- filter(data.allforms, form_name == "Lab Test")
p.lab_test$form_name <- NULL
p.lab_test$feature_name <- droplevels(p.lab_test$feature_name)
p.lab_test$feature_delta <- as.numeric(as.character(p.lab_test$feature_delta))
p.lab_test$feature_value <- as.numeric(as.character(p.lab_test$feature_value))
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

p.lab <- p.lab.albumin %>%
  full_join(p.lab.alp, by = "subject_id") %>%
  full_join(p.lab.alt, by = "subject_id") %>%
  full_join(p.lab.ast, by = "subject_id") %>%
  full_join(p.lab.basophils, by = "subject_id") %>%
  full_join(p.lab.bicarb, by = "subject_id") %>%
  full_join(p.lab.bilirubin_tot, by = "subject_id") %>%
  full_join(p.lab.bun, by = "subject_id") %>%
  full_join(p.lab.calcium, by = "subject_id") %>%
  full_join(p.lab.chloride, by = "subject_id") %>%
  full_join(p.lab.cholesterol, by = "subject_id") %>%
  full_join(p.lab.ck, by = "subject_id") %>%
  full_join(p.lab.creatinine, by = "subject_id") %>%
  full_join(p.lab.eosinophils, by = "subject_id") %>%
  full_join(p.lab.ggt, by = "subject_id") %>%
  full_join(p.lab.glucose, by = "subject_id") %>%
  full_join(p.lab.hba1c, by = "subject_id") %>%
  full_join(p.lab.hematocrit, by = "subject_id") %>%
  full_join(p.lab.hemoglobin, by = "subject_id") %>%
  full_join(p.lab.lymphocytes, by = "subject_id") %>%
  full_join(p.lab.monocytes, by = "subject_id") %>%
  full_join(p.lab.neutrophils, by = "subject_id") %>%
  full_join(p.lab.phosphorus, by = "subject_id") %>%
  full_join(p.lab.platelets, by = "subject_id") %>%
  full_join(p.lab.protein, by = "subject_id") %>%
  full_join(p.lab.rbc, by = "subject_id") %>%  
  full_join(p.lab.sodium, by = "subject_id") %>%
  full_join(p.lab.triglycerides, by = "subject_id") %>%
  full_join(p.lab.uric_acid, by = "subject_id") %>%
  full_join(p.lab.urine_glucose, by = "subject_id") %>%
  full_join(p.lab.urine_ph, by = "subject_id") %>%
  full_join(p.lab.urine_protein, by = "subject_id") %>%
  full_join(p.lab.wbc, by = "subject_id")

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
#done(lab tests: albumin, ALP, AST, basophils, bicarbonate, total bilirubin, BUN, calcium, chloride, total cholesterol, CK, eosinophils, GGT, glucose, HbA1c, hematocrit, hemoglobin, lymphocytes, monocytes, neutrophils, phosphorus, platelets, protein, RBC, sodium, triglycerides, uric_acid, urine_pH, WBC)

###FVC
p.fvc <- filter(data.allforms, form_name == "FVC")
p.fvc$form_name <- NULL
p.fvc$feature_name <- droplevels(p.fvc$feature_name)
unique(p.fvc$feature_name)
p.fvc$feature_unit <- droplevels(p.fvc$feature_unit)
unique(p.fvc$feature_unit)
p.fvc$feature_unit <- NULL
p.fvc$feature_delta <- as.numeric(as.character(p.fvc$feature_delta))
p.fvc$feature_value <- as.numeric(as.character(p.fvc$feature_value))
#done(p.fvc : FVC)

p.fvc <- p.fvc[!duplicated(p.fvc[, -3]), ]
p.s.fvc <- spread(p.fvc, feature_name, feature_value)
p.fvc <- p.s.fvc %>%
  filter(feature_delta <= 90 & feature_delta >= 0) %>%
  select(SubjectID, fvc_percent) %>%
  group_by(SubjectID) %>%
  summarise(fvc = mean(fvc_percent))
p.fvc <- p.fvc[complete.cases(p.fvc), ]
colnames(p.fvc) <- c("subject_id", "fvc")
rm(p.s.fvc)

###Riluzole
p.riluzole <- filter(data.allforms, form_name == "Riluzole")
p.riluzole$form_name <- NULL
p.riluzole$feature_name <- NULL
p.riluzole$feature_unit <- NULL
p.riluzole$feature_delta <- NULL
colnames(p.riluzole) <- c("subject_id", "riluzole")
#done(p.riluzone: riluzole)
rm(data.allforms)

complete_data <- alsfrs_label %>%
  left_join(alsfrs90, by = "subject_id") %>%
  left_join(alshistory, by = "subject_id") %>%
  left_join(p.demographics, by = "subject_id") %>%
  left_join(p.riluzole, by = "subject_id") %>%
  left_join(vitals, by = "subject_id") %>%
  left_join(p.fvc, by = "subject_id") %>%
  left_join(p.lab, by = "subject_id")

rm(alsfrs_label)
rm(alsfrs90)
rm(alshistory)
rm(p.demographics)
rm(p.riluzole)
rm(vitals)
rm(p.fvc)
rm(p.lab)

write.csv(complete_data, "complete_data.csv")
#______________________________________________________________________________________________________
###Machine learning Classification
setwd("C:/Users/user/Documents/ml_als")
library(dplyr)
complete_data <- read.csv("complete_data.csv")
complete_data[, 1] <- NULL

complete_data$Q1_max <- NULL
complete_data$Q2_max <- NULL
complete_data$Q3_max <- NULL
complete_data$Q4_max <- NULL
complete_data$Q5_max <- NULL
complete_data$Q6_max <- NULL
complete_data$Q7_max <- NULL
complete_data$Q8_max <- NULL
complete_data$Q9_max <- NULL
complete_data$Q10_max <- NULL
complete_data$Q1_diff <- NULL
complete_data$Q2_diff <- NULL
complete_data$Q3_diff <- NULL
complete_data$Q4_diff <- NULL
complete_data$Q5_diff <- NULL
complete_data$Q6_diff <- NULL
complete_data$Q7_diff <- NULL
complete_data$Q8_diff <- NULL
complete_data$Q9_diff <- NULL
complete_data$Q10_diff <- NULL


library(caret)

complete_data_conti <- complete_data[, -c(17, 21, 22, 23)]
complete_data_conti_imp <- predict(preProcess(complete_data_conti[, -1], method=c("medianImpute")), complete_data_conti[, -1])
complete_data_imp <- left_join(mutate(complete_data_conti_imp, subject_id = complete_data_conti[, 1]), complete_data[, c(1, 17, 21, 22, 23)], by = "subject_id")
imputed_omitted_data <- na.omit(complete_data_imp)
imputed_omitted_data$subject_id <- NULL


#Random Forest
model1 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "ranger",
  tuneLength = 10,
  importance = "impurity",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model1
plot(model1)
plot(varImp(model1), 20)


#Gradient Boosting 1
xgb_grid_1 = expand.grid(
  nrounds = 100,
  eta = c(0.1, 0.01, 0.001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = c(0, 5, 10),
  colsample_bytree = c(0.5, 0.9),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.5, 0.9)
)

xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

model2 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "xgbTree",
  importance = TRUE,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1
)

model2
plot(model2)
plot(varImp(model2), 20)

#Gradient Boosting 2
xgb_grid_1 = expand.grid(
  nrounds = 1000,
  eta = 0.1,
  max_depth = 2,
  gamma = 0,
  colsample_bytree = 0.9,
  min_child_weight = 1,
  subsample = 0.5
)

xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

model3 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "xgbTree",
  importance = TRUE,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1
)

model3
plot(model3)
plot(varImp(model3), 20)

#Support Vector Machine (Linear Kernel)
model4 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "svmLinear2",
  importance = TRUE,
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model4
plot(model4)
varImp(model4)

#Support Vector Machine (Radial Kernel)
model5 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "svmRadialSigma",
  importance = TRUE,
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model5
plot(model5)
varImp(model5)

#Linear Discriminant Analysis
model6 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "lda2",
  importance = TRUE,
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model6
plot(model6)
varImp(model6)

#Quadratic Discriminant Analysis
model7 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "qda",
  importance = TRUE,
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model7
plot(model7)
varImp(model7)
