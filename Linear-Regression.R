#Linear Regression (Please run Setup in advance)



#--------------------------Linear regression with all parameters---------------------
lr.all <- lm(linear.model.all, data=df)

#Summary
summary(lr.all)

#MSE
mse.all = mse_func(df$bmi, lr.all$fitted.values)
print(mse.all)

#co linearity check
car::vif(lr.all) #No co linearity

#confidence intervals
confint(lr.all)

#autoplot

autoplot( lr.all, which = 1:4 ) + theme_minimal()

#------------------------Excluding Height and Weight-------------------------------
lr.noWH <- lm(linear.model.noWH, data=df)


summary(lr.noWH)

#MSE
mse.noWH = mse_func(df$bmi, lr.noWH$fitted.values)
print(mse.noWH)

car::vif(lr.noWH) 
confint(lr.noWH)


autoplot(lr.noWH, which = 1:4 ) + theme_minimal()


#--------------------------Only significant predictors------------------------------
lr.drop <- lm(linear.model.drop, data=df)


summary(lr.drop)

#MSE
mse.drop = mse_func(df$bmi, lr.drop$fitted.values)
print(mse.drop)

car::vif(lr.drop) 

confint(lr.drop)


autoplot(lr.drop, which = 1:4 ) + theme_minimal()

#--------------------------------Quadratic function----------------------------------
lr.quadratic <- lm(poly2.model.noWH, data=df)


summary(lr.quadratic)

#MSE
mse.quadratic = mse_func(df$bmi, lr.quadratic$fitted.values)
print(mse.quadratic)

car::vif(lr.quadratic) 

confint(lr.quadratic)

autoplot(lr.quadratic, which = 1:4 ) + theme_minimal()



#----------------------------------Cubic function----------------------------------
lr.cubic <- lm(poly3.model.noWH, data=df)
summary(lr.cubic)

#MSE
mse.cubic = mse_func(df$bmi, lr.cubic$fitted.values)
print(mse.cubic)

car::vif(lr.cubic) 

confint(lr.cubic)

autoplot(lr.cubic, which = 1:4 ) + theme_minimal()



#-------------------------------Quartic function---------------------------------
lr.quartic <- lm(poly4.model.noWH, data=df)
summary(lr.quartic)

#MSE
mse.quartic = mse_func(df$bmi, lr.quartic$fitted.values)
print(mse.quartic)

car::vif(lr.quartic) 

confint(lr.quartic)

autoplot(lr.quartic, which = 1:4 ) + theme_minimal()




#--------------------------------------Log--------------------------------------
lr.log <- lm(log.model.noWH, data=df)
summary(lr.log)

#MSE for Training
mse.log = mse_func(df$bmi, lr.log$fitted.values)
print(mse.log)

car::vif(lr.log)

confint(lr.log)

autoplot(lr.log, which = 1:4 ) + theme_minimal()

anova(lr.noWH, lr.quadratic)
anova(lr.quadratic,lr.cubic)
anova(lr.cubic,lr.quartic)
anova(lr.quartic,lr.log)
anova(lr.drop,lr.noWH,lr.quadratic,lr.cubic,lr.quartic,lr.log)



