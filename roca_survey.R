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
survey_ders_rev <- na.omit(survey_ders) ##remove observations with any NAs
numberders <- dim(survey_ders_rev)[1] ## number of observations included in DERS analysis
revders <- survey_ders_rev[, c(2,3,7,8,9,11,18,21,23,25,35)]
revders <- sapply(revders, rev_score) ##use rev_score function ('rev_score.R')
survey_ders_rev[, c(2,3,7,8,9,11,18,21,23,25,35)] <- revders 

##NONACCEPT
nonaccept <- rowSums(survey_ders_rev[,c(26,22,13,12,30,24)]) ## NONACCEPT components
mean(nonaccept)
sd(nonaccept)
qplot(nonaccept, geom="histogram", binwidth=1, main="Histogram for DERS NONACCEPT", xlab="DERS NONACCEPT", fill=I("blue"), col=I("red"), alpha=I(.2))

##GOALS
goals <- rowSums(survey_ders_rev[,c(27,19,14,34,21)]) ## GOAL components
mean(goals)
sd(goals)
qplot(goals, geom="histogram", binwidth=1, main="Histogram for DERS GOALS", xlab="DERS GOALS", fill=I("blue"), col=I("red"), alpha=I(.2))

##IMPULSE
impulse <- rowSums(survey_ders_rev[,c(33,28,15,20,4,25)])
mean(impulse)
sd(impulse)
qplot(impulse, geom="histogram", binwidth=1, main="Histogram for DERS IMPULSE", xlab="DERS IMPULSE", fill=I("blue"), col=I("red"), alpha=I(.2))

##AWARE
aware <- rowSums(survey_ders_rev[,c(7,3,11,18,9,35)])
mean(aware)
sd(aware)
qplot(aware, geom="histogram", binwidth=1, main="Histogram for DERS AWARE", xlab="DERS AWARE", fill=I("blue"), col=I("red"), alpha=I(.2))

##STRATEGIES
strategies <- rowSums(survey_ders_rev[,c(17,16,32,36,29,23,37,31)])
mean(strategies)
sd(strategies)
qplot(strategies, geom="histogram", binwidth=1, main="Histogram for DERS STRATEGIES", xlab="DERS STRATEGIES", fill=I("blue"), col=I("red"), alpha=I(.2))

##CLARITY
clarity <- rowSums(survey_ders_rev[,c(6,5,10,8,2)])
mean(clarity)
sd(clarity)
qplot(clarity, geom="histogram", binwidth=1, main="Histogram for DERS CLARITY", xlab="DERS CLARITY", fill=I("blue"), col=I("red"), alpha=I(.2))

##total score distribution
ders_scores <- rowSums(survey_ders_rev[,2:36]) ## find sum of 36 items in each row for total DERS score, dropping the NAs
mean(ders_scores)
sd(ders_scores)
qplot(ders_scores, geom="histogram", binwidth=10, main="Histogram for DERS Scores", xlab="DERS Scores", fill=I("blue"), col=I("red"), alpha=I(.2))

##ProQOL Scoring
survey_proqol_rev <- na.omit(survey_proqol) ##remove NAs
numberproqol <- dim(survey_proqol_rev)[1] ## number of observations included in PROQOL analysis
revproqol <- survey_proqol_rev[, c(2,5,16,18,30)] ## items to reverse score
revproqol <- sapply(revproqol, rev_score) ##use rev_score function ('rev_score.R')
revproqol[revproqol==6] <- NA ##rev_score function outputs NA as 6; convert them back to NA
survey_proqol_rev[, c(2,5,16,18,30)] <- revproqol
proqol_scores <- rowSums(survey_proqol_rev[,2:31]) ## total score

##compassion score distribution
compassion <- rowSums(survey_proqol_rev[,c(4,7,13,17,19,21,23,25,28,31)], na.rm=TRUE) ##compassion satisfaction component
mean(compassion)
sd(compassion)
qplot(compassion, geom="histogram", binwidth=5, main="Histogram for ProQOL Compassion Satisfaction", xlab="ProQol Compassion Satisfaction", fill=I("green"), col=I("red"), alpha=I(.2))

##burnout score distribution
burnout <- rowSums(survey_proqol_rev[,c(2,5,9,11,16,18,20,22,27,30)], na.rm=TRUE) ## burnout component
mean(burnout)
sd(burnout)
qplot(burnout, geom="histogram", binwidth=5, main="Histogram for ProQOL Burnout", xlab="ProQol Burnout", fill=I("green"), col=I("red"), alpha=I(.2))

##sts score distribution
sts <- rowSums(survey_proqol_rev[,c(3,6,8,10,12,14,15,24,26,29)], na.rm=TRUE) ## secondary traumatic stress component
mean(sts)
sd(sts)
qplot(sts, geom="histogram", binwidth=5, main="Histogram for ProQOL Secondary Traumatic Stress", xlab="ProQol STS", fill=I("green"), col=I("red"), alpha=I(.2))

##total score distribution
proqol_scores <- rowSums(survey_proqol_rev[,2:31]) ## total score
mean(proqol_scores)
sd(proqol_scores)
qplot(proqol_scores, geom="histogram", binwidth=5, main="Histogram for ProQOL Total Scores", xlab="ProQol Total Scores", fill=I("green"), col=I("red"), alpha=I(.2))

##Life Events Checklist
survey_lec_rev <- na.omit(survey_lec) ##remove NAs
numberlec <- dim(survey_lec_rev)[1] ## number of observations included in LEC analysis

##find number of people who experienced each traumatic event
happened <- survey_lec_rev[,grep("a", names(survey_lec_rev))] ##identify "lec_*a" - "happened to me" variables
happened <- colSums(happened) ##find number of observations who experienced each event
happened_percentage<- happened*100/nrow(survey_lec_rev) ##find percentages
happened_freq <- cbind(happened, happened_percentage) ##combine frequency and percentage tables
row.names(happened_freq) <- c("Natural disaster", "Fire or explosion", "Transportation accident","Serious accident","Exposure to toxic substance","Physical assault","Assault with a weapon","Sexual assault","Other unwanted sexual exp","Exposure to war zone","Captivity","Life-threatning illness","Severe human suffering","Sudden violent death","Sudden accidental death","Serious harm you cause to someone else","Other")
happened_freq

##find number of people who witnessed each traumatic event
witnessed <-survey_lec_rev[,grep("b", names(survey_lec_rev))] ##identify "lec_*b" - "witnessed" variables
witnessed <- colSums(witnessed) ##find number of observations who observed each event
witnessed_percentage<- witnessed*100/nrow(survey_lec_rev) ##find percentages
witnessed_freq <- cbind(witnessed, witnessed_percentage) ##combine frequency and percentage tables
witnessed_freq <- witnessed_freq[-c(1),] ## remove observation row
row.names(witnessed_freq) <- c("Natural disaster", "Fire or explosion", "Transportation accident","Serious accident","Exposure to toxic substance","Physical assault","Assault with a weapon","Sexual assault","Other unwanted sexual exp","Exposure to war zone","Captivity","Life-threatning illness","Severe human suffering","Sudden violent death","Sudden accidental death","Serious harm you cause to someone else","Other")
witnessed_freq

##find number of people who experienced each event as part of job
partofjob <- survey_lec_rev[,grep("d", names(survey_lec_rev))] ##identify "lec_*d" - "as part of job" variables
partofjob <- colSums(partofjob) ##find number of observations who experience as part of job
partofjob_percentage <- partofjob*100/nrow(survey_lec_rev) ##find percentages
partofjob_freq <- cbind(partofjob, partofjob_percentage) ##combine frequency and percentage tables
row.names(partofjob_freq) <- c("Natural disaster", "Fire or explosion", "Transportation accident","Serious accident","Exposure to toxic substance","Physical assault","Assault with a weapon","Sexual assault","Other unwanted sexual exp","Exposure to war zone","Captivity","Life-threatning illness","Severe human suffering","Sudden violent death","Sudden accidental death","Serious harm you cause to someone else","Other")
partofjob_freq
