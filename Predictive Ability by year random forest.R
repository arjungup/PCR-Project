# Predictive ability by year. Our own hacky, but more natural, cross-folding.
# How to generate metrics http://stackoverflow.com/questions/16548882/how-to-know-if-a-regression-model-generated-by-random-forests-is-good-mse-and

##########################################################################
#df$year is not a predictor in any of the following
#Predictive ability without max enrollment, althought you should probably go back and use that
year2002RfEnrollmentOnAll<-randomForest(Enrollment~Fall.Spring+Subject+Section..+Core+grad.crosslisting.+CourseQuality+InstructorQuality+CourseDifficulty
                                +AmountLearned+WorkRequired+ReadingsValue+CommAbility+InstructorAccess+StimulateInterest+RecommendMajor+RecommendNonMajor,data=df[df$Year!=2002,],importance=T)
2002RfEnrollmentOnAll #MSE of 332, % of variance explained is 73.7. ntree=3000 it's 331.53 and var exp 73.77
predict(2002RfEnrollmentOnAll) #give this new data and it will predict values apparently
varImpPlot(2002RfEnrollmentOnAll,sort=TRUE,main='Variable Importance Plot')
Rfestimated<-predict(2002RfEnrollmentOnAll,df[df$Year==2002,])
plot(df[df$Year==2002,]$Enrollment, Rfestimated)
diagnosticlm<-lm(df[df$Year==2002,]$Enrollment~Rfestimated)
summary(diagnosticlm)
mse<-sum((Rfestimated-df[df$Year==2002,]$Enrollment)^2)/length(df[df$Year==2002,]$Enrollment)
rmse<-(sum((Rfestimated-df[df$Year==2002,]$Enrollment)^2)/length(df[df$Year==2002,]$Enrollment))^(1/2)
RMSE <- (sum((Rfestimated-df[df$Year==2002,]$Enrollment^2)/length(df[df$Year==2002,]$Enrollment)^(1/2)))

year2003RfEnrollmentOnAll<-randomForest(Enrollment~Fall.Spring+Subject+Section..+Core+grad.crosslisting.+CourseQuality+InstructorQuality+CourseDifficulty
                                    +AmountLearned+WorkRequired+ReadingsValue+CommAbility+InstructorAccess+StimulateInterest+RecommendMajor+RecommendNonMajor,data=df[df$Year!=2003,],importance=T)
year2003RfEnrollmentOnAll #MSE of 332, % of variance explained is 73.7. ntree=3000 it's 331.53 and var exp 73.77
predict(year2003RfEnrollmentOnAll) #give this new data and it will predict values apparently
varImpPlot(year2003RfEnrollmentOnAll,sort=TRUE,main='Variable Importance Plot')
Rfestimated<-predict(year2003RfEnrollmentOnAll,df[df$Year==2003,])
plot(df[df$Year==2003,]$Enrollment, Rfestimated)
diagnosticlm<-lm(df[df$Year==2003,]$Enrollment~Rfestimated)
summary(diagnosticlm)
mse<-sum((Rfestimated-df[df$Year==2003,]$Enrollment)^2)/length(df[df$Year==2003,]$Enrollment)
rmse<-(sum((Rfestimated-df[df$Year==2003,]$Enrollment)^2)/length(df[df$Year==2003,]$Enrollment))^(1/2)
RMSE <- (sum((Rfestimated-df[df$Year==2003,]$Enrollment^2)/length(df[df$Year==2003,]$Enrollment)^(1/2)))

RandomForestByYear<-function(standardRf,year){
  str(year)+RfEnrollmentOnAll<-randomForest(Enrollment~Fall.Spring+Subject+Section..+Core+grad.crosslisting.+CourseQuality+InstructorQuality+CourseDifficulty
                                          +AmountLearned+WorkRequired+ReadingsValue+CommAbility+InstructorAccess+StimulateInterest+RecommendMajor+RecommendNonMajor,data=df[df$Year!=2003,],importance=T)
  year2003RfEnrollmentOnAll #MSE of 332, % of variance explained is 73.7. ntree=3000 it's 331.53 and var exp 73.77
  predict(year2003RfEnrollmentOnAll) #give this new data and it will predict values apparently
  varImpPlot(year2003RfEnrollmentOnAll,sort=TRUE,main='Variable Importance Plot')
  Rfestimated<-predict(year2003RfEnrollmentOnAll,df[df$Year==2003,])
  plot(df[df$Year==2003,]$Enrollment, Rfestimated)
  diagnosticlm<-lm(df[df$Year==2003,]$Enrollment~Rfestimated)
  summary(diagnosticlm)
  mse<-sum((Rfestimated-df[df$Year==2003,]$Enrollment)^2)/length(df[df$Year==2003,]$Enrollment)
  rmse<-(sum((Rfestimated-df[df$Year==2003,]$Enrollment)^2)/length(df[df$Year==2003,]$Enrollment))^(1/2)
  RMSE <- (sum((Rfestimated-df[df$Year==2003,]$Enrollment^2)/length(df[df$Year==2003,]$Enrollment)^(1/2)))
}
mse<-data.frame()
for (year in levels(df$Year)){
  df_sub<-df[!df$Year=Year,]
  data=df_sub
  error
  mse()
}