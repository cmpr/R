library(foreign)
library(xlsx)
library(stringr)

read.spss("demographic-phase2.sav", to.data.frame=TRUE) -> dem_2
read.spss("Patient Demographics_posttreatment.sav", to.data.frame=TRUE) -> dempt_2
names(dem_2) <- c("patientid", "date", "round", "language", "session", "weeknumber", "gender", "age", "race_1", "race_2", "latino", "origin", "religion", "attendance_religion", "marital", "education", "occupation", "living", "income", "occ_status", "past_employment", "employment_limit", "employment_limit2", "work_performance", "social_function", "date_event")
names(dempt_2) <- c("patientid", "date", "round", "language", "session", "weeknumber", "gender", "age", "race_1", "race_2", "latino", "origin", "religion", "attendance_religion", "marital", "education", "occupation", "living", "income", "occ_status", "past_employment", "employment_limit", "employment_limit2", "work_performance", "social_function", "date_event")
lapply(dem_2$patientid, as.character) -> dem_2$patientid
sapply(dem_2$patientid, str_trim) -> dem_2$patientid
lapply(dempt_2$patientid, as.character) -> dempt_2$patientid
sapply(dempt_2$patientid, str_trim) -> dempt_2$patientid
dem_2[26, 1] <- "N1040" ## fix typo 'n1040'

##replace NA with posttreatment data in columns from religion to employment limitation
dempt_2$patientid -> dempt_2_id 
dem_2[which(dem_2$patientid %in% dempt_2_id),] -> dem_2_nd ## isolate patients who did not drop out and had posttreatment data collected
for (x in 13:22) {
  dem_2_nd[,x][is.na(dem_2_nd[,x])] <- dempt_2[,x][is.na(dem_2_nd[,x])]  
} ## replace any cells with NA in columns 13-22 with posttreatment data for patients who did not dropout 
dem_2[which(dem_2$patientid %in% dem_2_nd$patientid),] <- dem_2_nd ##overwrite phase 2 dataset with the new data for patients who did not drop out

##Combine levels
levels(dem_2$religion) <- c("Atheist", "Non-Catholic Christian", "Other", "Non-Catholic Christian", "Non-Catholic Christian", "Other", "Catholic", "Other", "Non-Catholic Christian", "Other", "Other", "Other", "Non-Catholic Christian", "Non-Catholic Christian", "Other", "Non-Catholic Christian", "Non-Catholic Christian", "Other", "Other")
dem_2$attendance_religion <- factor(dem_2$attendance_religion) ## change from numeric to factor
levels(dem_2$attendance_religion) <- c("Never", "Once a week or more", "Once a week or more", "Once a month", "Every few months", "Never")
levels(dem_2$education) <- c("College grad or more", "College grad or more", "Partial college", "High school grad", "Partial high school or less", "Partial high school or less", "Partial high school or less", "Partial high school or less")

##Mean age
mean(dem_2$age, na.rm=TRUE)
sd(dem_2$age, na.rm=TRUE)

##Frequency tables
sapply(dem_2[,-c(1:6, 26)], freqtb) -> dem_output ## exclude columns 1-6

##Export excel files
write.xlsx(dem_output[1], "C:/Users/cz378/My Documents/gender.xlsx")
write.xlsx(dem_output[2], "C:/Users/cz378/My Documents/age.xlsx")
write.xlsx(dem_output[3], "C:/Users/cz378/My Documents/race_1.xlsx")
write.xlsx(dem_output[4], "C:/Users/cz378/My Documents/race_2.xlsx")
write.xlsx(dem_output[5], "C:/Users/cz378/My Documents/latino.xlsx")
write.xlsx(dem_output[6], "C:/Users/cz378/My Documents/origin.xlsx")
write.xlsx(dem_output[7], "C:/Users/cz378/My Documents/religion.xlsx")
write.xlsx(dem_output[8], "C:/Users/cz378/My Documents/attendance_religion.xlsx")
write.xlsx(dem_output[9], "C:/Users/cz378/My Documents/marital.xlsx")
write.xlsx(dem_output[10], "C:/Users/cz378/My Documents/education.xlsx")
write.xlsx(dem_output[11], "C:/Users/cz378/My Documents/occupation.xlsx")
write.xlsx(dem_output[12], "C:/Users/cz378/My Documents/living.xlsx")
write.xlsx(dem_output[13], "C:/Users/cz378/My Documents/income.xlsx")
write.xlsx(dem_output[14], "C:/Users/cz378/My Documents/occ_status.xlsx")
write.xlsx(dem_output[15], "C:/Users/cz378/My Documents/past_employment.xlsx")
write.xlsx(dem_output[16], "C:/Users/cz378/My Documents/employment_limit.xlsx")
write.xlsx(dem_output[17], "C:/Users/cz378/My Documents/employment_limit2.xlsx")
write.xlsx(dem_output[18], "C:/Users/cz378/My Documents/work_performance.xlsx")
write.xlsx(dem_output[19], "C:/Users/cz378/My Documents/social_function.xlsx")