df<-read.csv("D:/PG-DAI/IIMA/Regression Analysis/R/EXAM II/cars.csv",header=TRUE)
attach(df)
head(df)

#1

Y=df$Price
X=df$Mileage

#Regressing
fit<-lm(Y~X)
summary(fit)

#Least squares fitted model: Price = 24,760 + -0.1725*(Mileage)

#The coefficient of Mileage is -1.725e-01, thus suggesting that 
#with -1.725e-01 increase in mileage, the car price goes up by 1 unit.

#Q2


Y_intercept = coef(fit)[1]
B_hat = coef(fit)[2]
standard_errors = summary(fit)$coefficients[, "Std. Error"]



#t-statistic
t_obs = B_hat/standard_errors[2]
t_obs

dof <-802

alpha <- 0.05

t_value <- qt(1 - alpha / 2, dof)


lower_conf_interval = B_hat - t_value*(standard_errors[2])
lower_conf_interval


upper_conf_interval = B_hat + t_value*(standard_errors[2])
upper_conf_interval


#(-0.2552534, -0.08978756), Since the confidence interval does not have 0 in it,
# We can say that Mileage has a significant negative linear effect on Price of cars.

#Thus we are 95% confident that for 1 percentage point increase in Mileage,
#The estimated price of a car would decrease by at least -0.08978756 and atmost by -0.2552534 

#Q3

(cor(X,Y))^2
# 2.05% of the variation in price of Cars is explained by the Mileage.

# wrt to ques 2,there is a weak negative linear relationship between the independent and dependent variables,
#but the model's predictive ability is limited due to the low R-squared value.

#Since it only explains ~2% of variation, other predictors are needed to explain the variation in price better.


#Q4

st.resid=rstandard(fit) ##standardised residuals##
plot(X,st.resid,xlab="Mileage",ylab="Standardised residuals",pch=19)
lines(c(0,50000),c(0,0))

# Since the plot is slightly skewed towards the right.
#We can say that the Normality and Homoscedasticity assumtions are not valid.

#Quantile-Quantile Plot
qqnorm(st.resid,xlab="Standardised residuals") ##Quantile-quantile plot of standardised residuals##
#Since the pattern is not linear, we conclude that the normality assumption does not hold.




#PART B

#Q1Regress Price on all the variables except “Make”, “Model”, “Trim” and “Type”. 
#Write down the fitted least squares model. Interpret the slope coefficients of “Cruise” and “Cylinder”

head(df)

CarData=data.frame(Price=df$Price,
              Mileage=df$Mileage,
              Cylinder=df$Cylinder,
              Liter=df$Liter,
              Doors=df$Doors,
              Cruise=df$Cruise,
              Sound=df$Sound,
              Leather=df$Leather)


fit2= lm(Price~Mileage+Cylinder+Liter+Doors+Cruise+Sound+Leather)
summary(fit2)

#Fitted MR Model

#Price = 6759-0.1698*(Mileage)+3792*(Cylinder)-787.2*(Liter)-1543*(Doors)+6289*(Cruise)-1994*(Sound)+3349*(Leather)

#Intrepretation of Cruise
#Controlling for other predictors,
#Price increases by 6289 for 1 unit increase in the Cruise


#Intrepretation of Cylinder
#Controlling for other predictors,
#Price increases by 3792 for 1 unit increase in the Cylinder


#Q2Interpret the coefficient of determination of the fitted regression model. 
#Does the newly added variables (Cylinder onwards) significantly 
#improve the predictive ability of the model compared to only Mileage ?

#The value of Multiple R-squared:0.4463, tells us that 
#the model is now able to explain 44.63% of the variance in the data.

#Q3 Starting with the above model, determine the optimal model by using backward elimination. 
#Which variable/s did you remove and why ?
library(regclass)

VIF(fit2)

#Backward Elimination
#Considering the threshold of VIF as 3.
#Since Liter has VIF of 13.518746, there is a case of multicollinearity we will remove it and refit the model.
fit3= lm(Price~Mileage+Cylinder+Doors+Cruise+Sound+Leather)
summary(fit3)
#Optimal Model
#Price = 7323 -0.1705*(Mileage)+3200*(Cylinder)-1463*(Doors)+6206*(Cruise)-2024*(Sound)+3327*(Leather)

VIF(fit3)
#Since now the VIF of all the predictors are under 3 we can say that this model has no multicollinearity now.

#Q4Based on the variance inflation factors of the predictors, do you detect any multicollinearity in the data ? 
#If so, how would you deal with it as part of your search for the optimal model ?

#Ans. Yes there was multicollinearity  in the data between cylinder and Litre.
# We removed Liter with VIF of 13.518746 and refit the data.

#Q5 Carry out residual diagnostics on the optimal model determined above and comment on the validity of the assumptions. 
#Can you detect any influential observations from the above plot ? If so, 
#how many such points are there ? On what basis do you deem them influential ?

std.resid.mm = rstandard(fit3)
#Residual Plots for verifying Linearity and Homoscedasticity
plot(fitted(fit3), std.resid.mm,xlab = "Fitted Values", ylab = "Standardized Residuals", pch=19)
lines(c(0,40000), c(0,0))

##histogram of standardised residuals
hist(st.resid,xlab="Standardised residuals") 

#QQ Plot
qqnorm(rstandard(fit3))
# Since the plot seems to be increasing exponentially, we can say that the dataset is not linear.

#Mileage
plot (Mileage, std.resid.mm,xlab = "Mileage", ylab = "Standardized Residuals", pch=19)
lines(c(0,50000),c(0,0))


#Cylinder
plot (Cylinder, std.resid.mm,xlab = "Cylinder", ylab = "Standardized Residuals", pch=19)
lines(c(0,50000),c(0,0))

#Doors
plot (Doors, std.resid.mm,xlab = "Doors", ylab = "Standardized Residuals", pch=19)
lines(c(0,50000),c(0,0))

#Cruise
plot (Cruise, std.resid.mm,xlab = "Cruise", ylab = "Standardized Residuals", pch=19)
lines(c(0,50000),c(0,0))

#Sound
plot (Sound, std.resid.mm,xlab = "Sound", ylab = "Standardized Residuals", pch=19)
lines(c(0,50000),c(0,0))

#Leather
plot (Sound, std.resid.mm,xlab = "Leather", ylab = "Standardized Residuals", pch=19)
lines(c(0,50000),c(0,0))


#Linearity :- Holds False, Since the standardized residuals when plotted against the fitted values of the model
#and the predictors do not tend to be randomly scattered above and below 0 having a funnel pattern.
#Hence the straight line fit (linearity assumption) is not appropriate for the model.

#Normality :- Holds False, The histogram do not show an approximate bell-shaped curve, and the
#histogram seems to be slightly skewed to the right. The Normal Probability plot shows a linear plot
#Hence the error distribution cannot be assumed to be linear, and the normality assumption is not satisfied.

#Homoscedasticity:- Holds True, Since the standardized residuals do not have almost constant spread with increase in
#the predictors taken for the model. The error variance do not seem to be constant with Price. Hence the assumption of
#constant error variance for fitting the linear regression model to the data is not satisfied. 

#Also there are presence of about 8 influential points in the data.



#Q6 In case any assumption/s have been violated, please suggest an appropriate transformation to remedy the situation. 
#Re-do the residual diagnostic plots for the transformed model and comment on the validity of the transformation. 
#Write down the fitted regression model on the transformed data.(Hint: you may try out the logarithm transformation on Price)


#Doing log transformation on Price
Price1 = log(Price)
fit4= lm(Price1~Mileage+Cylinder+Doors+Cruise+Sound+Leather)
summary(fit4)

std.resid.mm = rstandard(fit4)
#Residual Plots for verifying Linearity and Homoscedasticity
plot(fitted(fit4), std.resid.mm,xlab = "Fitted Values", ylab = "Standardized Residuals", pch=19)
lines(c(0,40000), c(0,0))

##histogram of standardised residuals
hist(st.resid,xlab="Standardised residuals") 

#QQ Plot
qqnorm(rstandard(fit4))

#Linearity :- Holds True, Since the standardized residuals when plotted against the fitted values of the model
#and the predictors X1, X2, X9,X10,X6 tend to be randomly scattered above and below 0 having no pattern.
#Hence the straight line fit (linearity assumption) is appropriate for the model.

#Normality :- Holds True, The histogram and boxplot show an approximate bell-shaped curve, but the
#histogram seems to be slightly skewed to the right. The Normal Probability plot shows a linear plot
#Hence the error distribution can be assumed to be linear, and the normality assumption is satisfied.

#Homoscedasticity:- Holds True, Since the standardized residuals have almost constant spread with increase in
#the predictors taken for the model. The error variance seems to be constant with X. Hence the assumption of
#constant error variance for fitting the linear regression model to the data is satisfied. 


#Q7 ] Include the variable “Make” in the transformed model arrived in (6).
#How many dummy variables do you need to do so ? Write the resulting fitted model.
#Do you notice any improvement in the model in terms of its predictive ability ? (You can just include as.factor(Make) in the lm() function).

fit5= lm(Price1~Mileage+Cylinder+Doors+Cruise+Sound+Leather+Make,as.factor(Make) )
summary(fit5)

#Since there are 5 Make we need 5 dummy variable.
#The R2 of the model improved significantly to 0.9012, 
#that a huge spike in the predictive ability of model.


# Fitted Model:


#Price = 9.216 + -0.000008015*(Mileage)+ 0.1816*(Cylinder) -0.06165*(Doors)+ 0.02271*(Cruise)+0.003874*(Sound)+0.02881*(Leather)+0.3449*(MakeCadillac) -0.1550*(MakeChevrolet)+ -0.09597*(MakePontiac)+0.6783*(MakeSAAB)-0.1237*(MakeSaturn)


#Q8What is the predicted difference in retail price between a SAAB car and the car you assign as your baseline ? What is the predicted difference in retail price between 
#a Chevrolet and a Pontiac ?

#Considering Cadillac as baseline predicted difference in retail price between a SAAB car and cadillac

#Considering Cadillac as baseline, SAAB car is expected to be priced approximately 0.6783 units higher than a Cadillac

#A Chevrolet car is expected to be priced approximately 0.05903 units lower than a Pontiac


#Q9
# Replace “Cylinder” by “Liter” in the model derived in (7) 
#(keeping the other variables as they are) and re-evaluate the model parameters. 
#Which model would you prefer i.e the one with “Liter” or the one having “Cylinder” ? Justify.

fit6= lm(Price1~Mileage+Liter+Doors+Cruise+Sound+Leather+Make,as.factor(Make) )
summary(fit6)
summary(fit5)


# The model with Cylinder instead of Liter 
#performs better based on the following factors:
  
#Lower residual standard error (more accurate predictions).
#Higher multiple R-squared and adjusted R-squared (explains more variance in the data).
#Higher F-statistic (better overall fit).


#Q10
#Re-do the regression diagnostic plots for the model you choose in (9) above and 
#comment on the validity of the assumptions.


std.resid.mm = rstandard(fit6)
#Residual Plots for verifying Linearity and Homoscedasticity
plot(fitted(fit6), std.resid.mm,xlab = "Fitted Values", ylab = "Standardized Residuals", pch=19)
lines(c(0,40000), c(0,0))

##histogram of standardised residuals
hist(st.resid,xlab="Standardised residuals") 

#QQ Plot
qqnorm(rstandard(fit6))


#Linearity :- Holds True, Since the standardized residuals when plotted against the fitted values of the model
#and the predictors X1, X2, X9,X10,X6 tend to be randomly scattered above and below 0 having no pattern.
#Hence the straight line fit (linearity assumption) is appropriate for the model.

#Normality :- Holds False, The histogram show an approximate bell-shaped curve, but the
#histogram seems to be slightly skewed to the right. The Normal Probability plot shows a linear plot
#Hence the error distribution cannot be assumed to be linear, and the normality assumption is not satisfied.

#Homoscedasticity:- Holds True, Since the standardized residuals have almost constant spread with increase in
#the predictors taken for the model. The error variance seems to be constant with X. Hence the assumption of
#constant error variance for fitting the linear regression model to the data is satisfied. 



#Q11
# Perform any one of the cross-validation procedures to verify the predictive
#ability of the optimal model chosen above. Discuss your findings in brief.

library(tidyverse)

library(caret)

CarData1=data.frame(Price=df$Price,
                   Mileage=df$Mileage,
                   Cylinder=df$Cylinder,
                   Liter=df$Liter,
                   Doors=df$Doors,
                   Cruise=df$Cruise,
                   Sound=df$Sound,
                   Leather=df$Leather,
                   Make=df$Make )
data_ctrl1 <- trainControl(method = "cv", number = 5)

model_caret <- train(Price1~Mileage+Liter+Doors+Cruise+Sound+Leather+Make, data = CarData1, trControl = data_ctrl1,method = "lm")

print(model_caret)