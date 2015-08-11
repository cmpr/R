library(foreign)
library(xlsx)
library(stringr)

read.spss("demographics-phase3.sav", to.data.frame=TRUE) -> dem_3
read.spss("Patient Demographics_posttreatment_070915_AL.sav", to.data.frame=TRUE) -> dempt_3
names(dem_3) <- c("patientid", "date", "round", "language", "session", "weeknumber", "gender", "age", "race_1", "race_2", "latino", "origin", "religion", "attendance_religion", "marital", "education", "occupation", "living", "income", "occ_status", "past_employment", "employment_limit", "employment_limit2", "work_performance", "social_function", "date_event")
names(dempt_3) <- c("patientid", "date", "round", "language", "session", "weeknumber", "gender", "age", "race_1", "race_2", "latino", "origin", "religion", "attendance_religion", "marital", "education", "occupation", "living", "income", "occ_status", "past_employment", "employment_limit", "employment_limit2", "work_performance", "social_function", "date_event")
lapply(dem_3$patientid, as.character) -> dem_3$patientid
sapply(dem_3$patientid, str_trim) -> dem_3$patientid
lapply(dempt_3$patientid, as.character) -> dempt_3$patientid
sapply(dempt_3$patientid, str_trim) -> dempt_3$patientid

##replace NA with posttreatment data in columns from origin to employment limitation
dempt_3$patientid -> dempt_3_id 
dem_3[which(dem_3$patientid %in% dempt_3_id),] -> dem_3_nd ## isolate patients who did not drop out and had posttreatment data collected
for (x in 12:22) {
  dem_3_nd[,x][is.na(dem_3_nd[,x])] <- dempt_3[,x][is.na(dem_3_nd[,x])]  
} ## replace any cells with NA in columns 13-22 with posttreatment data for patients who did not dropout 
dem_3[which(dem_3$patientid %in% dem_3_nd$patientid),] <- dem_3_nd ##overwrite phase 3 dataset with the new data for patients who did not drop out

##combine levels
levels(dem_3$religion) <- c("Atheist", "Non-Catholic Christian", "Other", "Non-Catholic Christian", "Non-Catholic Christian", "Other", "Catholic", "Other", "Non-Catholic Christian", "Other", "Other", "Other", "Non-Catholic Christian", "Non-Catholic Christian", "Other", "Non-Catholic Christian", "Non-Catholic Christian", "Other", "Other")
dem_3$attendance_religion <- factor(dem_3$attendance_religion) ## change from numeric to factor
levels(dem_3$attendance_religion) <- c("Never", "Once a week or more", "Once a week or more", "Once a month", "Every few months", "Never")
levels(dem_3$education) <- c("College grad or more", "College grad or more", "Partial college", "High school grad", "Partial high school or less", "Partial high school or less", "Partial high school or less", "Partial high school or less")

##Mean age
mean(dem_3$age, na.rm=TRUE)
sd(dem_3$age, na.rm=TRUE)

##Frequency tables
sapply(dem_3[,-c(1:6, 26)], freqtb) -> dem_3_output ## exclude columns 1-6
write.xlsx(dem_3_output[1], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/gender.xlsx")
write.xlsx(dem_3_output[3], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/race_1.xlsx")
write.xlsx(dem_3_output[4], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/race_2.xlsx")
write.xlsx(dem_3_output[5], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/latino.xlsx")
write.xlsx(dem_3_output[6], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/origin.xlsx")
write.xlsx(dem_3_output[7], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/religion.xlsx")
write.xlsx(dem_3_output[8], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/attendance_religion.xlsx")
write.xlsx(dem_3_output[9], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/marital.xlsx")
write.xlsx(dem_3_output[10], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/education.xlsx")
write.xlsx(dem_3_output[11], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/occupation.xlsx")
write.xlsx(dem_3_output[12], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/living.xlsx")
write.xlsx(dem_3_output[13], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/income.xlsx")
write.xlsx(dem_3_output[14], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/occ_status.xlsx")
write.xlsx(dem_3_output[15], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/past_employment.xlsx")
write.xlsx(dem_3_output[16], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/employment_limit.xlsx")
write.xlsx(dem_3_output[17], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/employment_limit2.xlsx")
write.xlsx(dem_3_output[18], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/work_performance.xlsx")
write.xlsx(dem_3_output[19], "C:/Users/cz378/My Documents/Phase 3 Patient Demographics/social_function.xlsx")