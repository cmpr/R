library(ggplot2)
library(foreign)
read.csv("survey_job.csv") -> survey_job
read.csv("survey_ders.csv") -> survey_ders
read.csv("survey_proqol.csv") -> survey_proqol
read.csv("survey_lec.csv") -> survey_lec

##descriptive statistics of staff demographic
survey_job[, c(3,4,5)] <- lapply(survey_job[,c(3,4,5)], factor)
levels(survey_job$jobarea) <- c("Education", "Employment", "Life Skills/TR", "Other")
levels(survey_job$employed) <- c("Less than 6 months", "6 months to 12 months", "13 months to 5 years", "More than 5 years")
levels(survey_job$pasttraining) <- c("No", "Yes")
survey_jobstats <- lapply(survey_job[,c(3,4,5)], freqtb) ##frequency and percentage of observationss in each category

##DERS Scoring
survey_ders_rev <- survey_ders ##reverse scoring
survey_ders_rev <- na.omit(survey_ders_rev) ##remove observations with any NAs
revders <- survey_ders_rev[, c(2,3,7,8,9,11,18,21,23,25,35)]
revders <- sapply(revders, rev_score) ##use rev_score function ('rev_score.R')
survey_ders_rev[, c(2,3,7,8,9,11,18,21,23,25,35)] <- revders 
##total score distribution
ders_scores <- rowSums(survey_ders_rev[,2:36], na.rm=TRUE) ## find sum of 36 items in each row for total DERS score, dropping the NAs
mean(ders_scores)
sd(ders_scores)
qplot(ders_scores, geom="histogram", binwidth=10, main="Histogram for DERS Scores", xlab="DERS Scores", fill=I("blue"), col=I("red"), alpha=I(.2))

##ProQOL Scoring
survey_proqol_rev <- survey_proqol ##reverse scoring
survey_proqol_rev <- na.omit(survey_proqol_rev) ##remove NAs
revproqol <- survey_proqol_rev[, c(2,5,16,18,30)]
revproqol <- sapply(revproqol, rev_score) ##use rev_score function ('rev_score.R')
revproqol[revproqol==6] <- NA ##rev_score function outputs NA as 6; convert them back to NA
survey_proqol_rev[, c(2,5,16,18,30)] <- revproqol
compassion <- rowSums(survey_proqol_rev[,c(4,7,13,17,19,21,23,25,28,31)], na.rm=TRUE) ##compassion satisfaction component
burnout <- rowSums(survey_proqol_rev[,c(2,5,9,11,16,18,20,22,27,30)], na.rm=TRUE) ## burnout component
sts <- rowSums(survey_proqol_rev[,c(3,6,8,10,12,14,15,24,26,29)], na.rm=TRUE) ## secondary traumatic stress component
proqol_scores <- rowSums(survey_ders_rev[,2:31], na.rm=TRUE) ## total score
##compassion score distribution
mean(compassion)
sd(compassion)
qplot(compassion, geom="histogram", binwidth=5, main="Histogram for ProQOL Compassion Satisfaction", xlab="ProQol Compassion Satisfaction", fill=I("blue"), col=I("red"), alpha=I(.2))
##burnout score distribution
mean(burnout)
sd(burnout)
qplot(burnout, geom="histogram", binwidth=5, main="Histogram for ProQOL Burnout", xlab="ProQol Burnout", fill=I("blue"), col=I("red"), alpha=I(.2))
##sts score distribution
mean(sts)
sd(sts)
qplot(sts, geom="histogram", binwidth=5, main="Histogram for ProQOL Secondary Traumatic Stress", xlab="ProQol STS", fill=I("blue"), col=I("red"), alpha=I(.2))
##total score distribution
mean(proqol_scores)
sd(proqol_scores)
qplot(proqol_scores, geom="histogram", binwidth=5, main="Histogram for ProQOL Total Scores", xlab="ProQol Total Scores", fill=I("blue"), col=I("red"), alpha=I(.2))

##Life Events Checklist
##find number of people who experienced each traumatic event
survey_lec_nn <- na.omit(survey_lec) ##remove NAs
happened <- survey_lec_nn[,grep("a", names(survey_lec_nn))] ##identify "lec_*a" - "happened to me" variables
happened <- colSums(happened) ##find number of observations who experienced each event
happened_percentage<- happened*100/nrow(survey_lec_nn) ##find percentages
happened_freq <- cbind(happened, happened_percentage) ##combine frequency and percentage tables
row.names(happened_freq) <- c("Natural disaster", "Fire or explosion", "Transportation accident","Serious accident","Exposure to toxic substance","Physical assault","Assault with a weapon","Sexual assault","Other unwanted sexual exp","Exposure to war zone","Captivity","Life-threatning illness","Severe human suffering","Sudden violent death","Sudden accidental death","Serious harm you cause to someone else","Other")
happened_freq
##find number of people who witnessed each traumatic event
witnessed <-survey_lec_nn[,grep("b", names(survey_lec_nn))] ##identify "lec_*b" - "witnessed" variables
witnessed <- colSums(witnessed) ##find number of observations who observed each event
witnessed_percentage<- witnessed*100/nrow(survey_lec_nn) ##find percentages
witnessed_freq <- cbind(witnessed, witnessed_percentage) ##combine frequency and percentage tables
witnessed_freq <- witnessed_freq[-c(1),] ## remove observation row
row.names(witnessed_freq) <- c("Natural disaster", "Fire or explosion", "Transportation accident","Serious accident","Exposure to toxic substance","Physical assault","Assault with a weapon","Sexual assault","Other unwanted sexual exp","Exposure to war zone","Captivity","Life-threatning illness","Severe human suffering","Sudden violent death","Sudden accidental death","Serious harm you cause to someone else","Other")
witnessed_freq
