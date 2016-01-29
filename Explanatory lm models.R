# Comparing different enrollment responses across lm models
setwd("~/Junior 1st sem/STAT 399 V2/Analysis")
library(hexbin)
library(car)
library(randomForest)
library(DAAG)
df<-read.csv('FinaldfV5.csv')
df$Core=as.factor(df$Core)
df$grad.crosslisting.=as.factor(df$grad.crosslisting.)
df$Section..=as.factor(df$Section..)
df$Year=as.factor(df$Year)
df<-subset(df,df$Enrollment>=5)
df<-subset(df,df$Max.Enroll>0)
df<-df[!df$Section..==413&!df$Section..==12&!df$Section..==13&!df$Section..==17&!df$Section..==410&!df$Section..==19,]
#models for explanation and understanding
# Max Enroll on Enrollment. vice versa I mean
cor(df[,unlist(lapply(df, is.numeric))])
EnrollmentOnMax<-lm(df$Enrollment~df$Max.Enroll)
summary(EnrollmentOnMax)
plot(EnrollmentOnMax)
LogEnrollmentOnMax<-lm(log(df$Enrollment)~df$Max.Enroll)
LogEnrollmentOnMax<-lm(log(df$Enrollment)~(df$Max.Enroll)^2)
summary(LogEnrollmentOnMax)
plot(LogEnrollmentOnMax)
EnrollmentOnCourseQuality<-lm(df$Enrollment~df$CourseQuality)
summary(EnrollmentOnCourseQuality)
EnrollmentOnInstructorQuality<-lm(df$Enrollment~df$InstructorQuality)
summary(EnrollmentOnInstructorQuality)
EnrollmentOnPrimaryRatings<-lm(df$Enrollment~df$CourseQuality+df$CourseDifficulty+df$InstructorQuality)
summary(EnrollmentOnPrimaryRatings)
#### All the below don't have max enrollment. Well the predictions do
# regular Enrollments on Core. A benchmark
EnrollmentOnCore<-lm(df$Enrollment~df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
summary(EnrollmentOnCore) # r2 of .42, adj of .415, kinda nonlinear, terribly heteroskedastic, our baseline to do better
par(mfrow=c(2,2))
plot(EnrollmentOnCore) # non linearity, terrible heteroskedasticity as y hat increases. Apply log transform 
leveragePlots(EnrollmentOnCore) # Once we changed the 2002A to df$Year and df$fall.spring it worked idk
vif(EnrollmentOnCore)
plot(hexbin(fitted(EnrollmentOnCore),resid(LogEnrollmentOnCore),xbins=40))
durbinWatsonTest(EnrollmentOnCore)
# Log Enrollments on Core
LogEnrollmentOnCore<-lm(log(df$Enrollment)~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
summary(LogEnrollmentOnCore) #.5036
plot(LogEnrollmentOnCore)
leveragePlots(LogEnrollmentOnCore)
vif(LogEnrollmentOnCore)
plot(hexbin(fitted(LogEnrollmentOnCore),resid(LogEnrollmentOnCore),xbins=40))
durbinWatsonTest(LogEnrollmentOnCore)
a<-fitted(LogEnrollmentOnCore)
r2ComparableLogEnrollmentOnCore<-1-(sum((exp(a)-df$Enrollment)^2)
                                    /sum((df$Enrollment-mean(df$Enrollment))^2)) #.388
# Percent Enrollments on Core
PercentEnrollmentOnCore<-lm(df$PercentEnrollment~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
summary(PercentEnrollmentOnCore) #r2 .50, adj r squared .49. Other assumptions seems to be met, not bad.
plot(PercentEnrollmentOnCore)
leveragePlots(PercentEnrollmentOnCore)
vif(PercentEnrollmentOnCore)
durbinWatsonTest(PercentEnrollmentOnCore)
a<-fitted(PercentEnrollmentOnCore)*as.vector(df$Max.Enroll)
r2ComparablePercentEnrollmentOnCore<-1-(sum((a-df$Enrollment)^2)
                                    /sum((df$Enrollment-mean(df$Enrollment))^2)) 
# Standardized Enrollment Deviations on Core
df$StandardizedEnrollmentDeviations<-(df$Enrollment-df$Average_Enrollment)/sqrt(df$NumberOfObservations)
StandardizedEnrollmentDeviationsOnCore<-lm(df$StandardizedEnrollmentDeviations~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
summary(StandardizedEnrollmentDeviationsOnCore) #r2 .50, adj r squared .49. Other assumptions seems to be met, not bad.
plot(StandardizedEnrollmentDeviationsOnCore)
leveragePlots(StandardizedEnrollmentDeviationsOnCore)
durbinWatsonTest(StandardizedEnrollmentDeviationsOnCore)
a<-(fitted(StandardizedEnrollmentDeviationsOnCore)*as.vector(sqrt(df$NumberOfObservations)))
    +as.vector(df$Average_Enrollment)
r2ComparableStandardizedEnrollmentDeviationsOnCore<-1-(sum((a-df$Enrollment)^2)
                                        /sum((df$Enrollment-mean(df$Enrollment))^2)) 

#can't do it! Negative values. And our interpretability is fucked anyway. 
LogStandardizedEnrollmentDeviationsOnCore<-lm(log(df$StandardizedEnrollmentDeviations)~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
summary(LogStandardizedEnrollmentDeviationsOnCore) 
plot(LogStandardizedEnrollmentDeviationsOnCore)

# go back for the first 2 and put max enrollment in

########################################################################
# Out of sample performance OLS
#OOS_r2_each_semester<-c(,OOS_r2_each_semester,summary(score)$r.squared)
OLS_OOS_mse_each_semester<-c()
OLS_OOS_rmse_each_semester<-c()
for (Year in levels(df$Year)){
  #for (Semester in levels(df$Fall.Spring)){
    df_temp<-df[!df$Year==Year,]#&]!df$Fall.Spring==Semester,] #create training data
    procedure_temp<-lm(Enrollment~Subject+Section..+Core+grad.crosslisting.+CourseQuality+
                         InstructorQuality+CourseDifficulty,data=df_temp) # train 
    holdout<-df[df$Year==Year,]#&df$Fall.Spring==Semester,] # create hold out data
    score<-predict(procedure_temp,newdata=holdout) # fitteds from test on hold out
    mse<-sum((score-holdout$Enrollment)^2)/nrow(holdout)
    rmse<-sqrt(mse)
    OLS_OOS_mse_each_semester<-c(OLS_OOS_mse_each_semester,mse) # this is just mse each semester in sample. Need proper dfe
    OLS_OOS_rmse_each_semester<-c(OLS_OOS_rmse_each_semester,rmse) # rmse each semester
  }
#}
# Out of sample performance OLS Percent
OLS_Percent_OOS_mse_each_semester<-c()
OLS_Percent_OOS_rmse_each_semester<-c()
for (Year in levels(df$Year)){
  #for (Semester in levels(df$Fall.Spring)){
    df_temp<-df[!df$Year==Year,]#&!df$Fall.Spring==Semester,] #create training data
    procedure_temp<-lm(PercentEnrollment~Fall.Spring+Subject+Section..+Core+grad.crosslisting.+CourseQuality+
                         InstructorQuality+CourseDifficulty,data=df_temp) # train 
    holdout<-df[df$Year==Year,]#&df$Fall.Spring==Semester,] # create hold out data
    score<-predict(procedure_temp,newdata=holdout) # fitteds from test on hold out
    a<-score*as.vector(holdout$Max.Enroll) # transform fitteds back
    mse<-sum((a-holdout$Enrollment)^2)/nrow(holdout) # compare transformed fitteds to regular to get mse
    rmse<-sqrt(mse)
    OLS_Percent_OOS_mse_each_semester<-c(OLS_Percent_OOS_mse_each_semester,mse) # this is just mse each semester in sample. Need proper dfe
    OLS_Percent_OOS_rmse_each_semester<-c(OLS_Percent_OOS_rmse_each_semester,rmse) # rmse each semester
  }
#}
# Out of sample performance OLS standardized
OLS_Standardized_OOS_mse_each_semester<-c()
OLS_Standardized_OOS_rmse_each_semester<-c()
for (Year in levels(df$Year)){
  #for (Semester in levels(df$Fall.Spring)){
    df_temp<-df[!df$Year==Year,]#&!df$Fall.Spring==Semester,] #create training data
    procedure_temp<-lm(StandardizedEnrollmentDeviations~Max.Enroll+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+CourseQuality+
                         InstructorQuality+CourseDifficulty,data=df_temp) # train 
    holdout<-df[df$Year==Year,]#&df$Fall.Spring==Semester,] # create hold out data
    score<-predict(procedure_temp,newdata=holdout) # fitteds from test on hold out
    a<-score*as.vector(sqrt(holdout$NumberOfObservations))+as.vector(holdout$Average_Enrollment) # transform fitteds back
    mse<-sum((a-holdout$Enrollment)^2)/nrow(holdout) # compare transformed fitteds to regular to get mse
    rmse<-sqrt(mse)
    OLS_Standardized_OOS_mse_each_semester<-c(OLS_Standardized_OOS_mse_each_semester,mse) # this is just mse each semester in sample. Need proper dfe
    OLS_Standardized_OOS_rmse_each_semester<-c(OLS_Standardized_OOS_rmse_each_semester,rmse) # rmse each semester
  }

#}
# Out of sample performance rf Percent
RF_Percent_OOS_mse_each_semester<-c()
RF_Percent_OOS_rmse_each_semester<-c()
for (Year in levels(df$Year)){
  #for (Semester in levels(df$Fall.Spring)){
    df_temp<-df[!df$Year==Year,]#&!df$Fall.Spring==Semester,] #create training data
    procedure_temp<-randomForest(PercentEnrollment~Fall.Spring+Subject+Section..+Core+grad.crosslisting.+CourseQuality+
                         InstructorQuality+CourseDifficulty,data=df_temp,importance=T) # train 
    holdout<-df[df$Year==Year,]#&df$Fall.Spring==Semester,] # create hold out data
    score<-predict(procedure_temp,newdata=holdout) # fitteds from test on hold out
    a<-score*as.vector(holdout$Max.Enroll) # transform fitteds back
    mse<-sum((a-holdout$Enrollment)^2)/nrow(holdout) # compare transformed fitteds to regular to get mse
    rmse<-sqrt(mse)
    RF_Percent_OOS_mse_each_semester<-c(RF_Percent_OOS_mse_each_semester,mse) # this is just mse each semester in sample. Need proper dfe
    RF_Percent_OOS_rmse_each_semester<-c(RF_Percent_OOS_rmse_each_semester,rmse) # rmse each semester
  }
#}
RFwithMaxEnroll_Percent_OOS_mse_each_semester<-c()
RFwithMaxEnroll_Percent_OOS_rmse_each_semester<-c()
for (Year in levels(df$Year)){
  #for (Semester in levels(df$Fall.Spring)){
  df_temp<-df[!df$Year==Year,]#&!df$Fall.Spring==Semester,] #create training data
  procedure_temp<-randomForest(PercentEnrollment~Max.Enroll+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+CourseQuality+
                                 InstructorQuality+CourseDifficulty,data=df_temp,importance=T) # train 
  holdout<-df[df$Year==Year,]#&df$Fall.Spring==Semester,] # create hold out data
  score<-predict(procedure_temp,newdata=holdout) # fitteds from test on hold out
  a<-score*as.vector(holdout$Max.Enroll) # transform fitteds back
  mse<-sum((a-holdout$Enrollment)^2)/nrow(holdout) # compare transformed fitteds to regular to get mse
  rmse<-sqrt(mse)
  RFwithMaxEnroll_Percent_OOS_mse_each_semester<-c(RFwithMaxEnroll_Percent_OOS_mse_each_semester,mse) # this is just mse each semester in sample. Need proper dfe
  RFwithMaxEnroll_Percent_OOS_rmse_each_semester<-c(RFwithMaxEnroll_Percent_OOS_rmse_each_semester,rmse) # rmse each semester
}
# out of sample RF standardized.
df$EnrollmentStandardUnits<-((df$Enrollment-df$Average_Enrollment)/df$StandardDeviationEnrollment)
RF_StandardUnits_OOS_mse_each_semester<-c()
RF_StandardUnits_OOS_rmse_each_semester<-c()
for (Year in levels(df$Year)){
  
  df_temp<-df[!df$Year==Year,]
  df_temp<-na.omit(df_temp)
  procedure_temp<-randomForest(EnrollmentStandardUnits~Fall.Spring+Subject+Section..
                               +Core+grad.crosslisting.+CourseQuality+
                                 InstructorQuality+CourseDifficulty,data=df_temp,importance=T) # train 
  holdout<-na.omit(df[df$Year==Year,]) # create hold out data
  score<-predict(procedure_temp,newdata=holdout) # fitteds from test on hold out
  a<-score*as.vector(holdout$Standard_Deviation_Enrollment)+as.vector(holdout$Average_Enrollment) # transform fitteds back
  mse<-sum((a-holdout$Enrollment)^2)/nrow(holdout) # compare transformed fitteds to regular to get mse
  rmse<-sqrt(mse)
  RF_StandardUnits_OOS_mse_each_semester<-c(RF_StandardUnits_OOS_mse_each_semester,mse) #  mse each semester 
  RF_StandardUnits_OOS_rmse_each_semester<-c(RF_StandardUnits_OOS_rmse_each_semester,rmse) # rmse each semester
}
#}
varImpPlot(procedure_temp)
partialPlot(procedure_temp,x.var='InstructorQuality',pred.data=holdout)
partialPlot(procedure_temp,x.var='CourseQuality',pred.data=holdout)
partialPlot(procedure_temp,x.var='CourseDifficulty',pred.data=holdout)
partialPlot(procedure_temp,x.var='Max.Enroll',pred.data=holdout)
partialPlot(procedure_temp,x.var='Subject',pred.data=holdout)
partialPlot(procedure_temp,x.var='Core',pred.data=holdout)
partialPlot(procedure_temp,x.var='Section',pred.data=holdout)
partialPlot(procedure_temp,x.var='Fall.Spring',pred.data=holdout)
partialPlot(procedure_temp,x.var='grad.crosslisting.',pred.data=holdout)

audrey<-c('Max.Enroll','Fall.Spring','Subject','Section..','Core','grad.crosslisting.','CourseQuality',
  'InstructorQuality','CourseDifficulty')
for (var in audrey){
  partialPlot(procedure_temp,x.var=var,pred.data = holdout)
}
