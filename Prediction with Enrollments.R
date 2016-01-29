# Random Forests on Enrollments
setwd("~/Junior 1st sem/STAT 399 V2/Analysis")
library(randomForest)
library(gbm)
library(DAAG)
df<-read.csv('FinaldfV5.csv')
jim<-subset(df,df$Section..%in% c(12,13))
bill<-df[!df$Section..%in%c(12,13),]
summary(df)
df$Core=as.factor(df$Core)
df$grad.crosslisting.=as.factor(df$grad.crosslisting.)
df$Section..=as.factor(df$Section..)
df$Year=as.factor(df$Year)
df<-subset(df,df$Enrollment>=5)
df<-subset(df,df$Max.Enroll>0)
# Create response of (Course Enrollments-historical mean of course enrollments)/sqrt(n)

# log on core
RfLogEnrollmentOnCore<-randomForest(log(df$Enrollment)~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty,importance=T,ntree=500)
RfLogEnrollmentOnCore # MSE=.206, % var explained=62
varImpPlot(RfLogEnrollmentOnCore,sort=TRUE,main='Variable Importance Plot')
partialPlot(RfLogEnrollmentOnCore,x.var="CourseQuality")
# log on all
RfLogEnrollmentOnAll<-randomForest(log(df$Enrollment)~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty
+df$AmountLearned+df$WorkRequired+df$ReadingsValue+df$CommAbility+df$InstructorAccess+df$StimulateInterest+df$RecommendMajor+df$RecommendNonMajor,importance=T)
# regular on core
RfEnrollmentOnCore<-randomForest(df$Enrollment~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty,importance=T)
summary(RfEnrollmentOnCore)
RfEnrollmentOnCore #MSE of 375, % of variance explained is 70. When ntree=3000 (recommended in Berk's book) got an MSE of 375 and var explained of 70.26. Good.
plot(RfEnrollmentOnCore)
predict(RfEnrollmentOnCore) #give this new data and it will predict values apparently
varImpPlot(RfEnrollmentOnCore,sort=TRUE,main='Variable Importance Plot')
partialPlot(RfEnrollmentOnCore,x.var="CourseQuality")
# MSE changes, but this variance explained this doesn't. So it'd be really cool to use that
imp<-importance(RfEnrollmentOnCore)
impvar <- rownames(imp)
for (i in seq_along(impvar)) {
  partialPlot(RfEnrollmentOnCore,df, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
} # the for loop and that code from here: http://www.inside-r.org/packages/cran/randomForest/docs/partialPlot

# regullar on all
RfEnrollmentOnAll<-randomForest(df$Enrollment~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty
+df$AmountLearned+df$WorkRequired+df$ReadingsValue+df$CommAbility+df$InstructorAccess+df$StimulateInterest+df$RecommendMajor+df$RecommendNonMajor,importance=T)
summary(RfEnrollmentOnAll)
RfEnrollmentOnAll #MSE of 332, % of variance explained is 73.7. ntree=3000 it's 331.53 and var exp 73.77
plot(RfEnrollmentOnAll)
predict(RfEnrollmentOnAll) #give this new data and it will predict values apparently
varImpPlot(RfEnrollmentOnAll,sort=TRUE,main='Variable Importance Plot')

RfPercentEnrollmentOnCore<-randomForest(df$PercentEnrollment~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty,importance=T)
summary(RfPercentEnrollmentOnCore)
RfPercentEnrollmentOnCore #MSE of .067, % of variance explained is 25.5. 
plot(RfPercentEnrollmentOnCore)
predict(RfPercentEnrollmentOnCore) #give this new data and it will predict values apparently
varImpPlot(RfPercentEnrollmentOnCore,sort=TRUE,main='Variable Importance Plot')

######################################################################### OLS
# from "Just Working with Enrollment as response'
LogEnrollmentOnCore<-lm(log(df$Enrollment)~df$Year+df$Fall.Spring+df$Subject+df$Section..
              +df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
LogEnrollmentOnCore<-lm(log(Enrollment)~Year+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+
                          CourseQuality+InstructorQuality+CourseDifficulty,data=df[!df$Section..%in%c(12,13),])
datatmp<-df[!df$Section..%in%c(12,13,17,19,304,413,410,601),]
summary(LogEnrollmentOnCore)
cv.lm(df=datatmp,LogEnrollmentOnCore,m=2) # doesn't work, dumb piece of shit

CVlm(df=datatmp,form.lm=formula(log(df$Enrollment)~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty),
      m=3,dots =FALSE, seed=29,  plotit = c("Observed","Residual"))

CVlm(df=datatmp,form.lm=formula(log(Enrollment)~Year+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+
                             CourseQuality+InstructorQuality+CourseDifficulty,data=df),m=3)
######################################################################### Onto SGB
SGbEnrollmentOnCore<-gbm(df$Enrollment~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty,
          distribution='gaussian',interaction.depth=1,
          shrinkage=.1,bag.fraction=.5,n.trees=100,cv.folds=3)

SGbEnrollmentOnCore<-gbm(Enrollment~Year+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+CourseQuality+InstructorQuality+CourseDifficulty,
                         data=df,distribution='gaussian',interaction.depth=1,
                         shrinkage=.1,bag.fraction=.5,n.cores=4,n.trees=100,cv.folds=3)
best.iter <- gbm.perf(SGbEnrollmentOnCore,method="cv") # Find the number of trees needed
print(best.iter) 

plot(SGbEnrollmentOnCore$fit,working$avrain)

lm1<-lm(working$avrain~out1$fit) # Least Squares fit
summary(lm1) # Use residual distribution and RMSE as fit measure
abline(lm1) # Linear overlay --- Maybe tune or change profile.
library(mgcv) # GAM overlay --- Maybe more insight?
temp1<-data.frame(working$avrain,out1$fit) # New data frame
summary(temp1)
gam1<-gam(working$avrain~s(out1$fit)) # GAM in mgcv
summary(gam1)
plot(gam1,residuals=T,rug=T,se=T,cex=5)