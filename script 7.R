## script 7 for classification

lab <- read.csv("Labs.csv")
length(unique(lab$Test_Name))
#224

length(unique(lab$subject_id))
#8342

min.delta <- rep(0, length(unique(lab$subject_id)))

for (i in unique(lab$subject_id)) {
  
    min.delta[i] <- min( filter(lab, subject_id == i)$Laboratory_Delta )
    
}

k <- data.frame(student_id = unique(lab$subject_id))
k <- k %>%
  mutate(rank = rank(student_id))


#patient.rank <- function(i) {
#  return(filter(k, student_id == i)[1, 2])
#}


lab.start <- data.frame(lab[1,])
lab.start <- d[-1, ]

sample <- data.frame(lab[1,])
sample <- d[-1, ]

for (i in unique(lab$subject_id)) {
  sample <- filter(lab, subject_id == i & Laboratory_Delta == min.delta[i])
  lab.start <- bind_rows(lab.start, sample)
}

# got lab.start

#error due to duplicates (Absolute Basophil Count)
not.dup.index <- !duplicated(lab.start[ , c(1,2)])
sum(!not.dup.index)
# 21151 rows are duplicated. 21151 / 276791 = 0.076 (7.6%)

lab.start2 <- lab.start[not.dup.index, ]

lab.start.tidy <- spread(lab.start2[, c("subject_id", "Test_Name", "Test_Result")], key = Test_Name, value = Test_Result)

##-------------------------------------------------------

alsfrs.w.label <- mutate(alsfrs.w, label = label)
alsfrs.w.label <- select(alsfrs.w.label, label)

w.label.row.name <- row.names(alsfrs.w)
  
for (i in 2:nrow(alsfrs.w)) {
  w.label.row.name[i] <- sub("^X", "", row.names(alsfrs.w)[i])
  
}

row.names(alsfrs.w.label) <- w.label.row.name

alsfrs.w.label[2] <- alsfrs.w.label[1]
w.label.row.name[1] <- 0
alsfrs.w.label[1] <- as.numeric(w.label.row.name)
alsfrs.w.label <- alsfrs.w.label[-1, ]
colnames(alsfrs.w.label) <- c("subject_id", "label")

youmeiyouna <- na.omit(lab.start.tidy)

lab.in.join <- inner_join(lab.start.tidy, alsfrs.w.label, by = "subject_id")
#created data for machine learning

install.packages("randomForest")
library(randomForest)
lab.in.join2 <- lab.in.join[-1]
RFmodel <- randomForest(label ~ ., data = lab.in.join2)
#fail
