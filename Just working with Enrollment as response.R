# Just dealing with Enrollments on whatever
# about error diagnostics http://www.statmethods.net/stats/rdiagnostics.html
# overview of regression techniques http://blog.minitab.com/blog/statistics-and-quality-data-analysis/giving-thanks-for-the-regression-menu-v2
# some really nifty looking tools and tips http://www.statmethods.net/stats/regression.html
setwd("~/Junior 1st sem/STAT 399 V2/Analysis")
install.packages('gvlma')
library(gvlma)
library(hexbin)
library(car)
library(gam)
library(mgcv)
df<-read.csv('FinaldfV5.csv')
summary(df)
df$Core=as.factor(df$Core)
df$grad.crosslisting.=as.factor(df$grad.crosslisting.)
df$Section..=as.factor(df$Section..)
df$Year=as.factor(df$Year)
jim<-subset(df,df$Enrollment<=5)
df<-subset(df,df$Enrollment>=5)
df<-subset(df,df$Max.Enroll>0)
##################################### Look at the data
# Scatter plot
pairs(~df$Enrollment+df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty,
      data=df,main="test")
# very useless, just blobs. Hexbin!
hbin1<-hexbin(df$CourseQuality,df$Enrollment,xbins=50)
plot(hbin1)
hbin2<-hexbin(df$InstructorQuality,df$Enrollment,xbins=50)
plot(hbin2)
hbin3<-hexbin(df$CourseDifficulty,df$Enrollment,xbins=50)
plot(hbin3)
hbin4<-hexbin(df$Fall.Spring,df$Enrollment,xbins=50)
plot(hbin4)
############################ End looking at data

####################################### Some Analysis
####################################### Using raw enrollment as response in OLS
##### straight up reg on core
EnrollmentOnCore<-lm(df$Enrollment~df$Max.Enroll+df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
summary(EnrollmentOnCore)
#diagnoistics
plot(EnrollmentOnCore) # non linearity, terrible heteroskedasticity as y hat increases. Apply log transform 
leveragePlots(EnrollmentOnCore) # Once we changed the 2002A to df$Year and df$fall.spring it worked idk
outlierTest(EnrollmentOnCore) # don't really know how to interpret this
qqPlot((EnrollmentOnCore), main="QQ Plot") # not this either
av.Plots(EnrollmentOnCore) # need another package or something
influencePlot(EnrollmentOnCore,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
vif(EnrollmentOnCore)
plot(hexbin(fitted(EnrollmentOnCore),resid(LogEnrollmentOnCore),xbins=40))
#### straight up reg on all
EnrollmentOnAll<-lm(df$Enrollment~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty
                    +df$AmountLearned+df$WorkRequired+df$ReadingsValue+df$CommAbility+df$InstructorAccess+df$StimulateInterest+df$RecommendMajor+df$RecommendNonMajor)
summary(EnrollmentOnAll)
plot(EnrollmentOnAll)
#### log response on core
LogEnrollmentOnCore<-lm(log(df$Enrollment)~df$Max.Enroll+df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
summary(LogEnrollmentOnCore) #r2 .50, adj r squared .49. Other assumptions seems to be met, not bad.
vif(EnrollmentOnAll)
# diagnostics
plot(LogEnrollmentOnCore)
leveragePlots(LogEnrollmentOnCore)
vif(LogEnrollmentOnCore)
plot(cooks.distance(LogEnrollmentOnCore))
plot(hexbin(fitted(LogEnrollmentOnCore),resid(LogEnrollmentOnCore),xbins=40))
plot(hexbin(c(),rstandard(LogEnrollmentOnCore)))
qqPlot(LogEnrollmentOnCore, main="QQ Plot")
ncvTest(LogEnrollmentOnCore)
spreadLevelPlot(LogEnrollmentOnCore)
anova(LogEnrollmentOnCore)
influence(LogEnrollmentOnCore)
durbinWatsonTest(LogEnrollmentOnCore) #quite autocorrelated
res = LogEnrollmentOnCore$residuals 
n = length(LogEnrollmentOnCore$residuals) 
lag1 = lm(res[-n] ~ res[-1]) 
summary(lag1)
lag2=lm(res[-n] ~ res[-2])
summary(lag2)
summary(gvlma(LogEnrollmentOnCore))
#### log response on all
LogEnrollmentOnAll<-lm(log(df$Enrollment)~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty
                    +df$AmountLearned+df$WorkRequired+df$ReadingsValue+df$CommAbility+df$InstructorAccess+df$StimulateInterest+df$RecommendMajor+df$RecommendNonMajor)
summary(LogEnrollmentOnAll) # r2 only up to .55 with all the extra junk. Multicollinearity apparent in lots of negative slopes
plot(LogEnrollmentOnAll)
leveragePlots(LogEnrollmentOnAll)
vif(LogEnrollmentOnAll)
plot(cooks.distance(LogEnrollmentOnAll))

######################################## End using raw enrollment as predictor

######################################## Looking at Percent Enrollment
PercentEnrollmentOnCore<-lm(df$PercentEnrollment~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
summary(PercentEnrollmentOnCore) #r2 .50, adj r squared .49. Other assumptions seems to be met, not bad.
# diagnostics
plot(PercentEnrollmentOnCore)
leveragePlots(PercentEnrollmentOnCore)
vif(PercentEnrollmentOnCore)
#### Log Percent Enrollment on Core
LogPercentEnrollmentOnCore<-lm(log(df$PercentEnrollment)~df$Year+df$Fall.Spring+df$Subject+df$Section..+df$Core+df$grad.crosslisting.+df$CourseQuality+df$InstructorQuality+df$CourseDifficulty)
summary(LogPercentEnrollmentOnCore) #r2 .50, adj r squared .49. Other assumptions seems to be met, not bad.
# diagnostics
plot(LogPercentEnrollmentOnCore)
leveragePlots(LogPercentEnrollmentOnCore)
vif(LogPercentEnrollmentOnCore)

####################################### End running OLS
####################################### semi-parametric section. Only considering Log Enrollment~Core now. Results of multicollinearity response will determine rest
LogEnrollmentOnCoreInGAM<-gam(log(Enrollment)~Year+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+CourseQuality+InstructorQuality+CourseDifficulty,data=df) 
summary(LogEnrollmentOnCoreInGAM) # AIC 5777
plot(LogEnrollmentOnCoreInGAM,se=T,rugplot=T)

LogSmootherdfdefault<-gam(log(Enrollment)~Year+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+s(CourseQuality)+s(InstructorQuality)+s(CourseDifficulty),data=df) #can't do df$ version for some reason
par(mfrow=c(2,3))
plot(LogSmootherdfdefault,se=T, rugplot=T)
summary(LogSmootherdfdefault) #AIC 5746

LogSmootherdf3<-gam(log(Enrollment)~Year+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+s(CourseQuality,df=3)+s(InstructorQuality,df=3)+s(CourseDifficulty,df=3),data=df) #can't do df$ version for some reason
summary(LogSmootherdf3) #AIC 5750
plot(LogSmootherdf3,se=T,rugplot=T)

LogSmootherdf2<-gam(log(Enrollment)~Year+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+s(CourseQuality,df=2)+s(InstructorQuality,df=2)+s(CourseDifficulty,df=2),data=df) #can't do df$ version for some reason
summary(LogSmootherdf2) #AIC 5757
plot(LogSmootherdf2,se=T,rugplot=T)


LogSmootherdf6<-gam(log(Enrollment)~Year+Fall.Spring+Subject+Section..+Core+grad.crosslisting.+s(CourseQuality,df=6)+s(InstructorQuality,df=6)+s(CourseDifficulty,df=6),data=df) #can't do df$ version for some reason
summary(LogSmootherdf6) #AIC 5744
plot(LogSmootherdf6,se=T,rugplot=T)

##At the suggestion of Andrew:
df$QualityPerDifficulty=df$CourseQuality/df$CourseDifficulty
df$AverageQuality<-(df$CourseQuality+df$InstructorQuality)/2
df$DifferenceQuality<-df$CourseQuality-df$InstructorQuality

EnrollmentOnCoreAndPCA<-lm(df$Enrollment~df$Year+df$Fall.Spring+df$Subject+df$Section..
   +df$Core+df$grad.crosslisting+df$CourseDifficulty
   +df$QualityPerDifficulty+df$AverageQuality+df$DifferenceQuality)
summary(EnrollmentOnCoreAndPCA)
PCAtest<-lm(df$Enrollment~df$AverageQuality+df$DifferenceQuality)
summary(PCAtest)
plot(df$AverageQuality,df$Enrollment)

jim<-subset(df,df$PercentEnrollment>1.4)
dfConstrainedPercentages<-subset(df,df$PercentEnrollment<=1.4)
plot(dfConstrainedPercentages$AverageQuality,dfConstrainedPercentages$PercentEnrollment)
plot(dfConstrainedPercentages$DifferenceQuality,dfConstrainedPercentages$PercentEnrollment)
plot()

# PCA 
newdf<-data.frame(df$InstructorQuality,df$CourseQuality)
QualityAndInstructorPCA<-prcomp(newdf,center=TRUE,scale=FALSE)
