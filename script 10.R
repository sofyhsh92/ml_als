#script 10

setwd("C:/Users/user/Documents/ml_als")

######creating data for machine learning
###ALSFRS
alsfrs_data_mm <- read.csv("alsfrs.csv")
#merge Q5a and Q5b
for(i in 1:nrow(alsfrs_data_mm)) {
  if(is.na(alsfrs_data_mm$Q5a_Cutting_without_Gastrostomy[i])) {
    alsfrs_data_mm$Q5a_Cutting_without_Gastrostomy[i] <- alsfrs_data_mm$Q5b_Cutting_with_Gastrostomy[i]
  }
}
#drop Q5b
alsfrs_data_mm$Q5b_Cutting_with_Gastrostomy <- NULL
#merge Q10 and R1
for(i in 1:nrow(alsfrs_data_mm)) {
  if(is.na(alsfrs_data_mm$Q10_Respiratory[i])) {
    alsfrs_data_mm$Q10_Respiratory[i] <- alsfrs_data_mm$R_1_Dyspnea[i]
  }
}
#drop everything except Q1~Q10 and ALSFRS_Delta
alsfrs_data_mm[ , 13:19] <- NULL

#filter for 0~90 ALSFRS_Delta
library(dplyr)
alsfrs_data_mm <- filter(alsfrs_data_mm, ALSFRS_Delta <= 90 & ALSFRS_Delta >= 0)
alsfrs_data_mm <- na.omit(alsfrs_data_mm)
length(unique(alsfrs_data_mm$subject_id))
#6506 patients
colnames(alsfrs_data_mm) <- c("subject_id", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "ALSFRS_Delta")
alsfrs_data_mm <- alsfrs_data_mm %>%
  group_by(subject_id) %>%
  mutate(Q1_min = min(Q1), Q1_max = max(Q1), Q2_min = min(Q2), Q2_max = max(Q2), Q3_min = min(Q3), Q3_max = max(Q3), Q4_min = min(Q4), Q4_max = max(Q4), Q5_min = min(Q5), Q5_max = max(Q5), Q6_min = min(Q6), Q6_max = max(Q6), Q7_min = min(Q7), Q7_max = max(Q7), Q8_min = min(Q8), Q8_max = max(Q8), Q9_min = min(Q9), Q9_max = max(Q9), Q10_min = min(Q10), Q10_max = max(Q10)) %>%
  select(subject_id, Q1_min, Q1_max, Q2_min, Q2_max, Q3_min, Q3_max, Q4_min, Q4_max, Q5_min, Q5_max, Q6_min, Q6_max, Q7_min, Q7_max, Q8_min, Q8_max, Q9_min, Q9_max, Q10_min, Q10_max)
alsfrs_data_mm <- alsfrs_data_mm[!duplicated(alsfrs_data_mm), ]
#done(alsfrs : Q1min ~ Q10min, Q1max ~ Q10max)
write.csv(alsfrs_data_mm, "alsfrs_ml.csv")


###Riluzole
riluzole <- read.csv("Riluzole.csv")
riluzole <- select(riluzole, subject_id, Subject_used_Riluzole)
colnames(riluzole) <- c("subject_id", "riluzole")
#done(riluzole : riluzole)
write.csv(riluzole, "riluzole_ml.csv")

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

alshistory <- summarize(group_by(alshistory,subject_id),Onsetsite=Mode(Onsetsite),Onset_Delta=Mode(Onset_Delta),Diagnosis_Delta=Mode(Diagnosis_Delta))

alshistory <- alshistory %>%
  mutate(onset_to_diagnosis = Diagnosis_Delta - Onset_Delta) %>%
  select(subject_id, Onsetsite, Onset_Delta, onset_to_diagnosis)
#done (alshistory : Onsetsite, Onset_Delta, Diagnosis_Delta \\ NA remains in Onset_Delta, Diagnosis Delta)
write.csv(alshistory, "alshistory_ml.csv")


###demographics
demographics <- read.csv("demographics.csv")
for (i in 1:nrow(demographics)) {
  if (!is.na(demographics$Date_of_Birth[i])) {
    demographics$Age[i] <- -demographics$Date_of_Birth[i] / 365.25
  }
}
demographics <- select(demographics, subject_id, Age, Sex)
demographics <- na.omit(demographics)
demographics$Sex <- droplevels(demographics$Sex)
#done (demographics : Age, Sex)
write.csv(demographics, "demographics_ml.csv")

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

#done(vitalsigns : height, initial_weight, weight_difference_ratio, diastolic_p, systolic_p, heart_rate, respiratory_rate, temperature)
rm(vital)
write.csv(vital_height, "vital_height_ml.csv")
write.csv(vital_pressure, "vital_pressure_ml.csv")
write.csv(vital_respiratory_rate, "vital_respiratory_rate_ml.csv")
write.csv(vital_temperature, "vital_temperature_ml.csv")
write.csv(vital_weight, "vital_weight_ml.csv")

###FVC (later)
fvc <- read.csv("Fvc.csv")
exp <- select(fvc, subject_id, Subject_Normal)
exp <- na.omit(exp)
length(unique(exp$subject_id))

###Lab (later)