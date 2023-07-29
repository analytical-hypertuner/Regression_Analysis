df<-read.table("D:/PG-DAI/IIMA/Regression Analysis/Assignment2/senic.txt",header=TRUE)
#attach(df)
head(df)
#CR=data.frame(crime,education,urbanization,income)
#head(CR)
Average_Stay=df$Y
Age=df$X1
InfectionRisk=df$X2
Number_of_nurses=df$X9
AvailableFacilitiesServices=df$X10
MedicalSchoolAffiliation=df$X6

CR=data.frame(Average_Stay=df$Y,
              Age=df$X1,
              InfectionRisk=df$X2,
              Number_of_nurses=df$X9,
              AvailableFacilitiesServices=df$X10,
              MedicalSchoolAffiliation=df$X6
)
#replacing 2 with 0 which means No Medical Affiliation

CR$MedicalSchoolAffiliation[CR$MedicalSchoolAffiliation == 2] <- 0
head(CR)
pairs(CR)

#Q1
#Multiple Linear Regression Model
fit= lm(Average_Stay~Age+InfectionRisk+Number_of_nurses+AvailableFacilitiesServices+MedicalSchoolAffiliation,data = CR)
summary(fit)
#Y Intercept Coefficient
#1.2861417865 

Y_intercept= coef(fit)[1]
Y_intercept
#All the coeffients
coef(fit)

#Age: 0.0935290510 
#Infection Risk: 0.6496195059
#Number of nurses: 0.0003592619 
#Available Facilities Services: 0.0081970696                 
#Medical School Affiliation: 0.9201113300


#Q2 Hypothesis Testing
#Null Hypothesis: Slope of Age <=0
#Alternate Hypothesis Slope of Age > 0
# Sample Slope of Age

Age_BHat= coef(fit)[2]
Age_BHat


#Null Hypothesis Slope of Age Age_Beta<=0, Alternate Hypothesis Age_Beta > 0

# Standard Error of slope of Age

Age_SE_BHat=summary(fit)$coefficients[2, "Std. Error"]
Age_SE_BHat

# T-statistic

T_obs=Age_BHat/Age_SE_BHat
T_obs

#T_obs= 2.804 , which follows a t distribution with 107 degrees of freedom (as sample size is 113). The above value
#implies that the sample proportion Age_BHat falls 2.804 standard errors above the null value, 0.


###################################################


#Q3 95% Confidence Interval


# Degree of Freedom = 113-5-1= 107

dof=107


#P-Value

p_val=pt(T_obs,107)
p_val

#For our example, we have a '>' alternative. Hence the p-value will be one tailed, the area beyond t_obs = 2.804535 ,under a t107 distribution.
# With significance level .05 and p value = 0.9970071, we can conclude there is strong evidence failing to reject the Null Hypothesis, that is Beta <=0 is false, and we do not have sufficient evidence to support the Alternative Hypothesis


# Significance level (Alpha)
alpha =0.05

# Critical t-value
t_value =qt(1 - alpha / 2, dof)

# Print the t-value
print(t_value)



#Upper limit of Confidence Interval

CI_upper=Age_BHat+(t_value*Age_SE_BHat)
CI_upper

#Lower limit of Confidence Interval

CI_lower=Age_BHat-(t_value*Age_SE_BHat)
CI_lower

#The above interval implies that, the average length of stay will increase by at least 0.02741812 and
#at most 0.15964 for every 1 year increase in age with a 95 percentage confidence. In addition, since the
#confidence interval does not contain 0 and only contains positive values, we can infer with 95 percentage
#confidence that in the average length of stay has a positive linear association with Age
###################################################


#Q4 BootStrap Confidence Interval

# Set seed for reproducibility
set.seed(91)

sample_coef_intercept <- NULL
sample_coef_x <- NULL

for (i in 1:1000) {
  # Creating a resampled dataset from the sample data
  sample_d = CR[sample(1:nrow(df), nrow(df), replace = TRUE), ]
  # Running the regression on these data
  model_bootstrap <- lm(Average_Stay ~ Age + InfectionRisk + Number_of_nurses 
                        + AvailableFacilitiesServices + MedicalSchoolAffiliation, data = sample_d)
  
  # Saving the coefficients
  sample_coef_intercept <- c(sample_coef_intercept, model_bootstrap$coefficients[1])
  sample_coef_x <- c(sample_coef_x, model_bootstrap$coefficients[3])
}

# Rest of the code for calculating means and confidence intervals remains unchanged.

coefs <- rbind(sample_coef_intercept, sample_coef_x)

summary(model_bootstrap)


#Let us calculate the means of the y-intercept and slope of the bootstrap samples:
  
mean(sample_coef_intercept)

mean(sample_coef_x)

#and finally the 95% confidence intervals:
  
quantile(sample_coef_intercept,c(.025,.975))
quantile(sample_coef_x,c(.025,.975))

#We can also superimpose the 1000 bootstrapped regression fits on the original data pattern along with the least squares model fitted to the original data:


plot(Average_Stay ~ InfectionRisk,col = "gray",xlab = "Infection Risk",ylab = "Stay",pch=19)

abline(coef(model_bootstrap)[1], coef(model_bootstrap)[3], col = "red")

apply(coefs, 2, abline, col = rgb(1, 0, 0, 0.03))

#############################################

#Q5 Residual Diagnostic
#Standardised Residual
rstandard(fit)
fitted(fit)
#Residual Plot
plot(fitted(fit),rstandard(fit))
lines(c(7,15),c(0,0))

#quantile quantile plot
qqnorm(rstandard(fit))



#####
#####
#####
#####
#############################################

#Q6
anova_table = anova(fit)

SSReg = sum(anova_table["Sum Sq"][1:nrow(anova_table)-1,1])
SSReg

SSRes = anova_table["Sum Sq"][nrow(anova_table),1]
SSRes

Total = SSReg+SSRes

R_squared = SSReg/Total

library(regclass)

VIF(fit)

#Answer: R Square is 0.3633 , which means the all the above predictors taken together explain 36 percentage
#of the total variation in the Average Length of stay in the Hospital.
#VIF:
#  Age:1.025474
#Infection_Risk:1.225792
#Number_of_nurses:2.968698
#Available_facilities_services:2.730019
#Medical_school_affiliation :1.579376
#From the above IF of each predictor we can say everything is less than 3, but Number_of_nurses, and
#Available_facilities_services, has a high VIF as well as high P-value, which shows Multicollinearity.
###########################################

#Q7
#Considering the High VIF value of Number_of_nurses we fit the model removing the predictor

fit1= lm(Average_Stay~Age+InfectionRisk+AvailableFacilitiesServices+MedicalSchoolAffiliation,data = CR)
summary(fit1)
VIF(fit1)

#Considering the High P-Value value of Available_facilities_services we fit the model removing the predictor

fit2= lm(Average_Stay~Age+InfectionRisk+MedicalSchoolAffiliation,data = CR)
summary(fit2)
VIF(fit2)

###########################################

#Q8

#K-Fold  Cross Validation

#install.packages("tidyverse")

library(tidyverse)

#install.packages("caret")

library(caret)

data_ctrl <- trainControl(method = "cv",number=5)

model_caret <- train(Average_Stay ~ Age+InfectionRisk+MedicalSchoolAffiliation, data = CR,trControl = data_ctrl,method = "lm")

print(model_caret)


#Repeated K-Fold Cross Validation


data_ctrl <- trainControl(method = "repeatedcv",number=5,repeats=3)

model_caret <- train(Average_Stay ~ Age+InfectionRisk+MedicalSchoolAffiliation, data = CR,trControl = data_ctrl,method = "lm")

print(model_caret)

# Leave One Out Cross Validation
data_ctrl <- trainControl(method = "LOOCV")
model_caret <- train(Average_Stay ~ Age+InfectionRisk+MedicalSchoolAffiliation, data = CR,trControl = data_ctrl,method = "lm")
print(model_caret)




## Assessment of predictive ability

#We have seen in class that R2
#for the least squares model fitted to the original sample data is 75%. Since all
#the three estimated prediction errors reported above are close to those generated by the LOOCV, 5-fold CV
#and repeated 5-fold CV, we conclude that the fitted regression model has good predictive ability




#install.packages("Metrics")

library(Metrics)

fit<-lm(Average_Stay ~ Age+InfectionRisk+MedicalSchoolAffiliation)

predicted<-predict(fit)

rmse(Age+InfectionRisk+MedicalSchoolAffiliation,predicted)

mae(Age+InfectionRisk+MedicalSchoolAffiliation,predicted)


#We have seen in class that R2
#for the least squares model fitted to the original sample data is 36%. Since all
#the three estimated prediction errors reported above are similar to those generated by the LOOCV, 5-fold CV
#and repeated 5-fold CV, we conclude that the fitted regression model has good predictive ability.


