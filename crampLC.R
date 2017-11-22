#install package "readxl"
setwd("C:/Users/user/Desktop/ml_als")
install.packages("readxl")
library(readxl)
excel_sheets("crampLC.xlsx")

#screen
screen <- read_excel("crampLC.xlsx", sheet = "screen")
nrow(screen)
table(screen$`screen_pass   yes                no`)
table(screen$sreen_fail_cause)
barplot(table(screen$`sex        male       female`), main = "sex")
screen$`DM     yes  no    uk` <- factor(screen$`DM     yes  no    uk`, levels = c("yes", "no"))
barplot(table(screen$`DM     yes  no    uk`), main = "DM")
screen$`ascites        normol         abn              na` <- factor(screen$`ascites        normol         abn              na`, levels = c("normal", "abn"))
barplot(table(screen$`ascites        normol         abn              na`), main = "ascites")
screen$`diuretic        yes              no               na` <- factor(screen$`diuretic        yes              no               na`, levels = c("yes", "no"))
barplot(table(screen$`diuretic        yes              no               na`), main = "diuretics")
barplot(table(screen$`LC_cause    alc              hbv              hcv             pbc              psc               others`), main = "LC cause")
plot(density(as.numeric(screen$`LC_dur         years`[-c(8, 14)])), main = "LC duration (years)")

#close
close <- read_excel("crampLC.xlsx", sheet = "close")
barplot(table(close$`close          complete    early            death          withdr          fuloss         violate         others`), main = "close")
table(close$`close          complete    early            death          withdr          fuloss         violate         others`)

#runin
runin <- read_excel("crampLC.xlsx", sheet = "runin")
colnames(runin) <- c("pt_no", "date_ri", "dropout_ri", "dropout_ri_cause", "freq_ri_1w", "freq_ri_2w", "freq_ri_3w", "freq_ri_4w", "freq_ri_tot", "pain_ri_1w", "pain_ri_2w", "pain_ri_3w", "pain_ri_4w", "pain_ri_tot", "freq_sl_ri_1w", "freq_sl_ri_2w", "freq_sl_ri_3w", "freq_sl_ri_4w", "freq_sl_ri_tot", "ldqol_ri", "bil_ri", "alb_ri", "ptime_ri", "ascites_ri", "enceph_ri", "cpscore_ri", "cps_ri", "cbc_ri", "chem_ri", "coag_ri")
table(runin$dropout_ri)

#visit3
visit3 <- read_excel("crampLC.xlsx", sheet = "visit3")
colnames(visit3) <- c("pt_no", "date", "dropout", "dropout_cause", "freq", "pain", "freq_sl", "compl", "se", "d", "tx", "remark")
table(visit3$dropout)
table(visit3$dropout_cause)

#visit4
visit4 <- read_excel("crampLC.xlsx", sheet = "visit4")
colnames(visit4) <- c("pt_no", "date", "dose", "dropout", "dropout_cause", "freq_1w", "freq_2w", "freq_3w", "freq_4w", "freq_tot", "pain_1w", "pain_2w", "pain_3w", "pain_4w", "pain_tot", "freq_sl_1w", "freq_sl_2w", "freq_sl_3w", "freq_sl_4w", "freq_sl_tot", "compl", "ldqol", "cbc_ri", "chem_ri", "coag_ri", "bil", "alb", "ptime", "ascites", "enceph", "cpscore", "cps",  "se", "cgi", "d", "tx", "remark")
table(visit4$dropout)
table(visit4$dropout_cause)

#outcome1
library(dplyr)
runin.outcome <- select(runin, c(pt_no, freq_ri_tot))
visit4.outcome <- select(visit4, c(pt_no, freq_tot))
outcome1 <- left_join(runin.outcome, visit4.outcome, by = "pt_no" )
outcome1 <- na.omit(outcome1)
outcome1$freq_ri_tot <- as.numeric(outcome1$freq_ri_tot)
outcome1$freq_tot <- as.numeric(outcome1$freq_tot)
outcome1 <- na.omit(outcome1)
outcome1 <- mutate(outcome1, outcome1 = (freq_ri_tot - freq_tot)/freq_ri_tot)
boxplot(outcome1$outcome1, main = "outcome1")
outcome1.nooutlier <- filter(outcome1, outcome1 > -2)
outcome1.outlier <- filter(outcome1, outcome1 <= -2)
outcome1.outlier
nrow(outcome1.nooutlier)
hist(outcome1.nooutlier$outcome1, main = "histogram of outcome1 without outliers", xlab = "outcome1", breaks = seq(-1, 1, 0.2))

#outcome2
#1. ratio of outcome1 >= 0.5
nrow(filter(outcome1, outcome1 >= 0.5)) / nrow(outcome1)
#2. average pain difference
visit4$pain_1w <- as.numeric(visit4$pain_1w)
visit4$pain_2w <- as.numeric(visit4$pain_2w)
visit4$pain_3w <- as.numeric(visit4$pain_3w)
visit4$pain_4w <- as.numeric(visit4$pain_4w)
visit4$freq_tot <- as.numeric(visit4$freq_tot)
visit4.a.p.d <- visit4 %>%
  select(c(pt_no, pain_1w, pain_2w, pain_3w, pain_4w, freq_tot)) %>%
  na.omit() %>%
  mutate(v.a.p.d = (pain_1w + pain_2w + pain_3w + pain_4w ) / freq_tot )
runin$pain_ri_1w <- as.numeric(runin$pain_ri_1w)
runin$pain_ri_2w <- as.numeric(runin$pain_ri_2w)
runin$pain_ri_3w <- as.numeric(runin$pain_ri_3w)
runin$pain_ri_4w <- as.numeric(runin$pain_ri_4w)
runin$freq_ri_tot <- as.numeric(runin$freq_ri_tot)
runin.a.p.d <- runin %>%
  select(c(pt_no, pain_ri_1w, pain_ri_2w, pain_ri_3w, pain_ri_4w, freq_ri_tot)) %>%
  na.omit() %>%
  mutate(r.a.p.d = (pain_ri_1w + pain_ri_2w + pain_ri_3w + pain_ri_4w ) / freq_ri_tot )
a.p.d <- left_join(visit4.a.p.d, runin.a.p.d, by = "pt_no")
a.p.d <- a.p.d[complete.cases(a.p.d),]
a.p.d <- mutate(a.p.d, a.p.d = v.a.p.d - r.a.p.d)
hist(a.p.d$a.p.d, main = "average pain difference", xlab = "average pain difference", breaks = seq(-4, 6, 0.5) )

#new
screen.new <- select(screen, c(`pt_no                                                     S-`, age, `sex        male       female`, `diuretic        yes              no               na`, `LC_cause    alc              hbv              hcv             pbc              psc               others`, `LC_dur         years`))
colnames(screen.new) <- c("pt_no", "age", "sex", "diuretics", "LCcause", "LCdur")
runin.new <- select(runin, c(pt_no, dropout_ri, freq_ri_tot, cpscore_ri, pain_ri_1w, pain_ri_2w, pain_ri_3w, pain_ri_4w, freq_sl_ri_tot))
runin.sixty <- left_join(screen.new, runin.new, by = "pt_no")
runin.sixty.incl <- filter(runin.sixty, dropout_ri == "incl")
runin.sixty.incl <- mutate(runin.sixty.incl, average_pain = (pain_ri_1w + pain_ri_2w + pain_ri_3w + pain_ri_4w)/freq_ri_tot )
runin.sixty.incl$sex <- factor(runin.sixty.incl$sex)
runin.sixty.incl$LCcause <- factor(runin.sixty.incl$LCcause)
runin.sixty.incl$diuretics <- factor(runin.sixty.incl$diuretics)
runin.sixty.incl$LCdur <- as.numeric(runin.sixty.incl$LCdur)
runin.sixty.incl$cpscore_ri <- factor(runin.sixty.incl$cpscore_ri)
runin.sixty.incl$freq_sl_ri_tot <- as.numeric(runin.sixty.incl$freq_sl_ri_tot)
summary(select(runin.sixty.incl, c(age, sex, cpscore_ri, freq_ri_tot, average_pain, freq_sl_ri_tot, diuretics, LCcause, LCdur)))
hist(runin.sixty.incl$age, main = "age")
hist(runin.sixty.incl$freq_ri_tot, main = "frequency")
boxplot(runin.sixty.incl$freq_ri_tot, main = "frequency")
hist(runin.sixty.incl$freq_sl_ri_tot, main = "frequency while sleeping")
boxplot(runin.sixty.incl$freq_sl_ri_tot, main = "frequency while sleeping")
hist(runin.sixty.incl$average_pain, main = "average pain")
hist(runin.sixty.incl$LCdur, main = "LC duration", xlab = "years")
