#Removing HIGH Leverage points with high residuals

#------------------------------No Width and Height model-----------------------------
lr.noWH <- lm(linear.model.noWH, data=df)

summary(lr.noWH)

#MSE DIFFERENCE
mse.noWH = mse_func(df$bmi, lr.noWH$fitted.values)
print(mse.noWH)

car::vif(lr.noWH) 
confint(lr.noWH)
dev.new()
autoplot(lr.noWH, which = 1:4 ) + theme_minimal()


HighLeverage <- cooks.distance(lr.noWH) > (4/nrow(df))
LargeResiduals <- rstudent(lr.noWH) > 3
df1 <- na.omit(df[!HighLeverage & !LargeResiduals,])
lr.noWH.df1 <-lm(linear.model.noWH, data = df1)  
summary(lr.noWH.df1)
mse.noWH.bis = mse_func(df1$bmi, lr.noWH.df1$fitted.values)
print(mse.noWH.bis-mse.noWH)

car::vif(lr.noWH.df1) 
confint(lr.noWH.df1)
dev.new()
autoplot(lr.noWH.df1, which = 1:4 ) + theme_minimal()


#----------------------------------Quadratic model-----------------------------------
lr.quadratic <- lm(poly2.model.noWH, data=df)

summary(lr.quadratic)

#MSE
mse.quadratic = mse_func(df$bmi, lr.quadratic$fitted.values)
print(mse.quadratic)

car::vif(lr.quadratic) 

confint(lr.quadratic)

dev.new()
autoplot(lr.quadratic, which = 1:4 ) + theme_minimal()


HighLeverage <- cooks.distance(lr.quadratic) > (4/nrow(df))
LargeResiduals <- rstudent(lr.quadratic) > 3
df2 <- na.omit(df[!HighLeverage & !LargeResiduals,])
lr.quadratic.df2 <- lm(poly2.model.noWH, data=df2)

summary(lr.quadratic.df2)

#MSE DIFFERENCE
mse.quadratic.bis = mse_func(df2$bmi, lr.quadratic.df2$fitted.values)
print(mse.quadratic.bis-mse.quadratic)

car::vif(lr.quadratic.df2) 

confint(lr.quadratic.df2)

dev.new()
autoplot(lr.quadratic.df2, which = 1:4 ) + theme_minimal()
