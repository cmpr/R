library(foreign)
library(dplyr)
library(stringr)

##merge demographics 2 and 3
read.spss("demographics-phase2.sav", to.data.frame = TRUE) -> dem_2
read.spss("demographics-phase3.sav", to.data.frame = TRUE) -> dem_3
rbind(dem_2, dem_3) -> dem_comb
names(dem_comb) <- c("patientid", "date", "round", "language", "session", "weeknumber", "gender", "age", "race_1", "race_2", "latino", "origin", "religion", "attendance_religion", "marital", "education", "occupation", "living", "income", "occ_status", "past_employment", "employment_limit", "employment_limit2", "work_performance", "social_function", "date_event")
lapply(dem_comb$patientid, as.character) -> dem_comb$patientid
sapply(dem_comb$patientid, str_trim) -> dem_comb$patientid
dem_comb[26, 1] <- "N1040"
dem_comb <- dem_comb[-c(52, 57, 58, 60, 63, 64),]

##merge weeks and sessions attended
read.csv("patient-session-weeks.csv") -> patient_weeks
select(patient_weeks, patientid, weeks, sessions) -> weeks_sessions
lapply(weeks_sessions$patientid, as.character) -> weeks_sessions$patientid
sapply(weeks_sessions$patientid, str_trim) -> weeks_sessions$patientid
left_join(dem_comb, weeks_sessions, by = "patientid") -> dem_final

##description of combined sample
mean(dem_final$weeks, na.rm = TRUE)
med_mod_q(dem_final$weeks)
freqtb(dem_final$weeks)
mean(dem_final$sessions, na.rm = TRUE)
med_mod_q(dem_final$sessions)
freqtb(dem_final$sessions)
boxplot(dem_final$weeks, dem_final$sessions, main="Number of Weeks and Sessions Attended by All Patients", ylab="Number of Weeks/Sessions", names=c("Weeks", "Sessions"))

##description of Phase 1&2 sample
phase2_weeks <- dem_final[dem_final$round == "First and Second rounds",]$weeks
phase2_sessions <- dem_final[dem_final$round == "First and Second rounds",]$sessions 
mean(phase2_weeks, na.rm = TRUE)
med_mod_q(phase2_weeks)
freqtb(phase2_weeks)
mean(phase2_sessions, na.rm=TRUE)
med_mod_q(phase2_sessions)
freqtb(phase2_sessions)
boxplot(phase2_Weeks, phase2_sessions, main="Number of Weeks and Sessions Attended by Phase 1&2 Patients", yalb="Number of Weeks/Sessions", names=c("Weeks", "Sessions"))

##description of Phase 3 Sample
phase3_weeks <- dem_final[dem_final$round == "Third Round",]$weeks
phase3_sessions <- dem_final[dem_final$round == "Third Round",]$sessions
mean(phase3_weeks, na.rm = TRUE)
med_mod_q(phase3_weeks)
freqtb(phase3_weeks)
mean(phase3_sessions, na.rm=TRUE)
med_mod_q(phase3_sessions)
freqtb(phase3_sessions)
boxplot(phase3_Weeks, phase3_sessions, main="Number of Weeks and Sessions Attended by Phase 3 Patients", yalb="Number of Weeks/Sessions", names=c("Weeks", "Sessions"))

## gender and weeks/sessions t-test
filter(dem_final, gender == "female") -> female
filter(dem_final, gender == "male") -> male
t.test(female$weeks, male$weeks)
t.test(female$sessions, male$sessions)

## race and weeks/sessions t-test
filter(dem_final, race_1 == "White" & is.na(race_2) == TRUE) -> white
filter(dem_final, race_1 != "White" | is.na(race_2) == FALSE ) -> non_white
t.test(white$weeks, non_white$weeks)
t.test(white$sessions, non_white$sessions)

##age and weeks/sessions linear regression
linreg_weeks <- lm(dem_final$weeks ~ dem_final$age)
summary(linreg_weeks)
plot(dem_final$age, dem_final$weeks)
linreg_sessions <- lm(dem_final$sessions ~ dem_final$age)
summary(linreg_sessions)
plot(dem_final$age, dem_final$sessions)

##occupation status and weeks/sessions ANOVA
occ_weeks_anova <- aov(dem_final$weeks ~ dem_final$occ_status)
ls(occ_weeks_anova)
summary(occ_weeks_anova)
occ_sessions_anova <- aov(dem_final$sessions ~ dem_final$occ_status)
ls(occ_sessions_anova)
summary(occ_sessions_anova)

##ethnicity and weeks/sessions t-test
filter(dem_final, latino == "yes") -> latino
filter(dem_final, latino == "no") -> non_latino
t.test(latino$weeks, non_latino$weeks)
t.test(latino$sessions, non_latino$sessions)