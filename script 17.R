###script 17
##FVC

p.fvc <- filter(data.allforms, form_name == "FVC")
p.fvc$form_name <- NULL
p.fvc$feature_name <- droplevels(p.fvc$feature_name)
unique(p.fvc$feature_name)
p.fvc$feature_unit <- droplevels(p.fvc$feature_unit)
unique(p.fvc$feature_unit)
p.fvc$feature_unit <- NULL
p.fvc$feature_delta <- as.numeric(as.character(p.fvc$feature_delta))
p.fvc$feature_value <- as.numeric(as.character(p.fvc$feature_value))

library(tidyr)
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

##Riluzole
p.riluzole <- filter(data.allforms, form_name == "Riluzole")
p.riluzole$form_name <- NULL
p.riluzole$feature_name <- NULL
p.riluzole$feature_unit <- NULL
p.riluzole$feature_delta <- NULL
colnames(p.riluzole) <- c("subject_id", "riluzole")

