# CHECKING WORKING DIRECTORY 
getwd()

#SETTING WORK DIRECTORY
setwd("P:\\Regression analysis\\Project\\")

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY
library(xlsx)
library(stats)
library(car)
library(foreign)
library(leaps)
library(MASS)
library(mixlm)
library(ggpubr)


options(max.print=1000000)  # Specifying the number of output rows displayed on the console


pdf("Rplot1.pdf")  # Output the plots in the program onto a PDF file


#READING AND CREATING DATA 
admit=readxl::read_xlsx("Admission_Predict_Ver1.1.xlsx",1)
admit


# MULTIPLE PLOTS
pairs(~ Chance_of_Admit + GRE_Score + TOEFL_Score + University_Rating + SOP + LOR + CGPA + Research, data=admit, cex.labels=2, cex.axis=1,cex=1)


#SETTING TO SEND OUTPUTS TO A TEXT FILE AND ON SCREEN
sink("Project_output.txt", append=TRUE, split=TRUE)


#SETTING UP THE MLR MODEL
mdl1 = lm( Chance_of_Admit ~ GRE_Score + TOEFL_Score + University_Rating + SOP + LOR + CGPA + Research, data=admit)
summary(mdl1)  # SUMMARY CALCULATION OF THE MODEL
anova(mdl1) # ANOVA TABLE



coef=mdl1$coefficients # COEFFICIENTS OF THE MODEL
coef # PRINTING THE COEFFICIENTS 



# Boxplots of all the variables with Chance of admit
boxplot(admit$Chance_of_Admit ~ admit$GRE_Score,main="Chance of admit vs GRE scores",xlab="GRE score",ylab="Chance of admit")
boxplot(admit$Chance_of_Admit ~ admit$TOEFL_Score,main="Chance of admit vs TOEFL scores",xlab="TOEFL score",ylab="Chance of admit")
boxplot(admit$Chance_of_Admit ~ admit$Research,main="Chance of admit vs Research",xlab="Research",ylab="Chance of admit")
boxplot(admit$Chance_of_Admit ~ admit$CGPA,main="Chance of admit vs CGPA",xlab="CGPA",ylab="Chance of admit")
boxplot(admit$Chance_of_Admit ~ admit$SOP,main="Chance of admit vs SOP",xlab="SOP",ylab="Chance of admit")
boxplot(admit$Chance_of_Admit ~ admit$LOR,main="Chance of admit vs LOR",xlab="LOR",ylab="Chance of admit")
boxplot(admit$Chance_of_Admit ~ admit$University_Rating,main="Chance of admit vs University rating",xlab="University rating",ylab="Chance of admit")


# Calculating Confidence intervals of admission for the best, average and the worst student applications
conf95_best=predict(mdl1,newdata = data.frame(GRE_Score=340,TOEFL_Score=120,University_Rating=4,SOP=4.5,LOR=4,CGPA=9.91,Research=1), interval="confidence", level=0.95)
conf95_best

conf95_avg=predict(mdl1,newdata = data.frame(GRE_Score=310,TOEFL_Score=105,University_Rating=3,SOP=3,LOR=3,CGPA=7.5,Research=0), interval="confidence", level=0.95)
conf95_avg

conf95_worst=predict(mdl1,newdata = data.frame(GRE_Score=290,TOEFL_Score=92,University_Rating=1,SOP=1,LOR=1,CGPA=6.8,Research=0), interval="confidence", level=0.95)
conf95_worst




# COMPUTING PREDICTED VALUES AND PLOTING AGAINST OBSERVED 
obsy=admit$Chance_of_Admit  # OBSERVED VALUES
obsy

yhat_y=cbind(predict(mdl1), admit$Chance_of_Admit) # COMBINING PREDICTED AND OBSERVED 

head(yhat_y) # PRINTING PART OF IT

plot(admit$Chance_of_Admit, predict(mdl1), pch = 20, type = 'p', las = 1,xlab="Observed", ylab="Predicted", main = 'Observed vs Predicted')
abline(0,1)


# FINDING STANDARD ERROR 
sigmahat= summary(mdl1)$sigma
sigmahat


#Normality plot
cat("Normality of Residuals or Normal probability \n\n")
plot(mdl1, which =2, col="blue", main="Normal probability Plot")
cat("This plot indicates that our data is partially normal with huge deviations at the end. Data has a heavy-tailed distribution as Above the line is lower percentile and below the line is higher percentile. This implied that non-linearity might exist")

#Q-Q plot with STUDENTIZED RESIDUAL 
rstud_mdl1 = stdres(mdl1)
qqnorm(rstud_mdl1, ylab="Studentized Residuals", xlab="Normal Scores", main="Q-Q Plot with Studentized Residual ") 
qqline(rstud_mdl1)

#Plotting residual values vs fitted values
cat("Residual vs Fitted Values - Plot \n\n")
plot(mdl1, which = 1)



#Influence analysis
#Measures of Influence
#Cooks distance 

#Plotting Cook'D of each point to identify outliers
plot(mdl1, which = 4)


#A LOOK AT THE ALL DIAGNOSTICS BY POINTS

Lev_hii=hatvalues(mdl1)             	## LEVERAGE POINTS 
CookD=cooks.distance(mdl1)  		## COOKS DISTANCE 
Dffit=dffits(mdl1)        		## DFFITS
Residual=residuals(mdl1) 			## RESIDUAL
Stand_Res=stdres(mdl1)			## STANDARDIZED RESIDUAL
Student_Res=studres(mdl1)   			## STUDENTIZED RESIDUAL 
R_Student = rstudent(mdl1) 			## COMPUTING R-Student
Y_Value=admit$Chance_of_Admit
Dfbetas=dfbeta(mdl1)				## COMPUTING DFBETAS

allres=cbind(admit$Chance_of_Admit,Residual,Stand_Res,Student_Res,R_Student,Lev_hii,CookD,Dffit,Y_Value,Dfbetas)
allres


#Transformation
plot(admit)
#We can see from the plots that relationship between x2 and y is not linear
#but resembles a sqrt relation.
#modb8_tr=lm(y~x1+sqrt(x2),dataB8)
#summary(modb8_tr)



############ COMPUTING FOR ALL MODELS ############

tmp =regsubsets(Chance_of_Admit ~ GRE_Score + TOEFL_Score + University_Rating + SOP + LOR + CGPA + Research,data=admit,nbest=10,really.big=T, intercept=T)
names(summary(tmp))

almdl=summary(tmp)[[1]]
RSQ=summary(tmp)[[2]]
SSE=summary(tmp)[[3]]
adjR2=summary(tmp)[[4]]
Cp=summary(tmp)[[5]]
BIC=summary(tmp)[[6]]

fnl=cbind(almdl,SSE,RSQ,adjR2,Cp,BIC)[order(-adjR2),]
fnl

plot(tmp,scale="adjr2")
plot(tmp,scale="Cp")

# Calculating the PRESS Statistic value for the 2 best models from the above table
mdl2_temp = lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + LOR + CGPA + Research,data = admit)
mdl3_temp = lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + LOR + CGPA + University_Rating + Research,data = admit)

PRESS(mdl2_temp)
PRESS(mdl3_temp)


#Displaying the coefficients of all the variables in the original model
allvr=lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + University_Rating + SOP + LOR + CGPA + Research , data=admit)
allvr

#Computing the best model after dropping the insignificant variables from mdl1 usinf Forwards selection, backward selection and the stepwise selection methods
forward(allvr, alpha = 0.05, full = TRUE)
backward(allvr, alpha = 0.05, full = TRUE, hierarchy = TRUE)
stepWise(allvr, alpha.enter = 0.05, alpha.remove = 0.05, full = TRUE)



#The final model doesn't contain the variables SOP and University_Rating, as these are insignificant variables
mdl2 = lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + LOR + CGPA + Research,data = admit)
summary(mdl2)
anova(mdl2)


# FINDING STANDARD ERROR 
sigmahat_new= summary(mdl2)$sigma
sigmahat_new

# Determining the correlation of each factor with Chance_of_Admit
cor(admit$GRE_Score,admit$Chance_of_Admit)
cor(admit$TOEFL_Score,admit$Chance_of_Admit)
cor(admit$LOR,admit$Chance_of_Admit)
cor(admit$CGPA,admit$Chance_of_Admit)
cor(admit$Research,admit$Chance_of_Admit)

sink() #Closing the file after storing all the output values


dev.off() #Closing the PDF file after storing all the plots