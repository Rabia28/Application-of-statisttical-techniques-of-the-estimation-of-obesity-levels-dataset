#Resampling methods: Validation Set, K-Cross Validation, Bootstrap

#Random seed for replicability
set.seed(582)

#model.matrix needed for expanding factor

#Splitting Data Set in Half for Validation
split_df <- initial_split( df ) 
train_df <- training( split_df )
test_df <- testing( split_df )

#----------------------------------------VALIDATION------------------------------------
MSE_vector=rep(0,7)

#Full model
lr.all <- lm(linear.model.all, data=train_df)

#Estimated MSE on Validation Set
MSE = mean((test_df$bmi-predict(lr.all,test_df))^2)
print(MSE)
MSE_vector[1]=MSE

#No Width & Height model
lr.noWH <- lm(linear.model.noWH, data=train_df)

#Estimated MSE on Validation Set
MSE = mean((test_df$bmi-predict(lr.noWH,test_df))^2)
print(MSE)
MSE_vector[2]=MSE

#Only Significant Variables Model
lr.drop <- lm(linear.model.drop, data=train_df)

#Estimated MSE on Validation Set
MSE = mean((test_df$bmi-predict(lr.drop,test_df))^2)
print(MSE)
MSE_vector[3]=MSE

#Quadratic Model
lr.quadratic <- lm(poly2.model.noWH, data=train_df)

#Estimated MSE on Validation Set
MSE = mean((test_df$bmi-predict(lr.quadratic,test_df))^2)
print(MSE)
MSE_vector[4]=MSE

#Cubic Model
lr.cubic <- lm(poly3.model.noWH, data=train_df)

#Estimated MSE on Validation Set
MSE = mean((test_df$bmi-predict(lr.cubic,test_df))^2)
print(MSE)
MSE_vector[5]=MSE

#Quartic Model
lr.quartic <- lm(poly4.model.noWH, data=train_df)

#Estimated MSE on Validation Set
MSE = mean((test_df$bmi-predict(lr.quartic,test_df))^2)
print(MSE)
MSE_vector[6]=MSE


#Log Model
lr.log <- lm(log.model.noWH, data=train_df)

#Estimated MSE on Validation Set
MSE = mean((test_df$bmi-predict(lr.log,test_df))^2)
print(MSE)
MSE_vector[7]=MSE

print(MSE_vector)


#------------------------------Leave One Out Cross Validation-------------------------

cv.error=rep(0,7)
for (i in 1:7){
  glm.fit=glm(models[[i]],data=df)
  cv.error[i]=cv.glm(df,glm.fit)$delta [1]
}

print(cv.error)


#-------------------------------K-fold Cross Validation-------------------------------

set.seed(1389)

k=10 #the dataset will be splitted in 10 parts

cv.error.10=rep(0,7)
for (i in 1:7){
  glm.fit=glm(models[[i]],data=df)
  cv.error.10[i]=cv.glm(df ,glm.fit,K=k)$delta [1]
}

print(cv.error.10)

#--------------------- Bootstrap for SE(B) computation ---------------------------- 

set.seed(1389)
#full model
boot.fn = function(data, index)+
  return(coef(lm(bmi~., data=data, subset=index)))

b = boot(df ,boot.fn ,1000)
b.coef = summary(b)$bootSE

sum = summary(lm(linear.model.all, data = df))
coef = as.numeric(sum$coefficients[ ,2])

cat("\nFull model - Difference between Std.errors:\n",coef - b.coef,"\n")

#NOWH model
set.seed(1389)
boot.fn = function(data, index)+
  return(coef(lm(bmi~.-Weight-Height, data=data, subset=index)))

b = boot(df ,boot.fn ,1000)
b.coef = summary(b)$bootSE

sum = summary(lm(linear.model.noWH, data = df))
coef = as.numeric(sum$coefficients[ ,2])

cat("\nNo Weigth-Height model - Difference between Std.errors:\n",coef - b.coef,"\n")

#quadratic
set.seed(1389)
boot.fn = function(data, index)+
  return(coef(lm(bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                 +eats_snacksno+eats_snacksFrequently+eats_snacksSometimes+SMOKEyes+counts_caloriesyes
                 +drinks_alcoholno+drinks_alcoholSometimes+drinks_alcoholFrequently+method_transWalking
                 +method_transBike+method_transMotorbike+method_transPublic_Transportation
                 +poly(Age,2)+poly(num_meals,2)+poly(time_using_tech,2)
                 +poly(eats_veggies,2)+poly(drinks_water,2)+poly(exercises_often,2), data=data, subset=index)))

b = boot(df ,boot.fn ,1000)
b.coef = summary(b)$bootSE

sum = summary(lm(poly2.model.noWH, data = df))
coef = as.numeric(sum$coefficients[ ,2])

cat("\nquadratic model - Difference between Std.errors:\n",coef - b.coef,"\n")

#cubic
set.seed(1389)
boot.fn = function(data, index)+
  return(coef(lm(bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                 +eats_snacksno+eats_snacksFrequently+eats_snacksSometimes+SMOKEyes+counts_caloriesyes
                 +drinks_alcoholno+drinks_alcoholSometimes+drinks_alcoholFrequently+method_transWalking
                 +method_transBike+method_transMotorbike+method_transPublic_Transportation
                 +poly(Age,3)+poly(num_meals,3)+poly(time_using_tech,3)
                 +poly(eats_veggies,3)+poly(drinks_water,3)+poly(exercises_often,3), data=data, subset=index)))

b = boot(df ,boot.fn ,1000)
b.coef = summary(b)$bootSE

sum = summary(lm(poly3.model.noWH, data = df))
coef = as.numeric(sum$coefficients[ ,2])

cat("\ncubic model - Difference between Std.errors:\n",coef - b.coef,"\n")

#quartic model
set.seed(1389)
boot.fn = function(data, index)+
  return(coef(lm(bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                 +eats_snacksno+eats_snacksFrequently+eats_snacksSometimes+SMOKEyes+counts_caloriesyes
                 +drinks_alcoholno+drinks_alcoholSometimes+drinks_alcoholFrequently+method_transWalking
                 +method_transBike+method_transMotorbike+method_transPublic_Transportation
                 +poly(Age,4)+poly(num_meals,4)+poly(time_using_tech,4)
                 +poly(eats_veggies,4)+poly(drinks_water,4)+poly(exercises_often,4), data=data, subset=index)))

b = boot(df ,boot.fn ,1000)
b.coef = summary(b)$bootSE

sum = summary(lm(poly4.model.noWH, data = df))
coef = as.numeric(sum$coefficients[ ,2])

cat("\nquartic model - Difference between Std.errors:\n",coef - b.coef,"\n")

#Log Model
set.seed(1389)
boot.fn = function(data, index)+
  return(coef(lm(bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                 +eats_snacksno+eats_snacksFrequently+eats_snacksSometimes+SMOKEyes+counts_caloriesyes
                 +drinks_alcoholno+drinks_alcoholSometimes+drinks_alcoholFrequently+method_transWalking
                 +method_transBike+method_transMotorbike+method_transPublic_Transportation
                 +log(eats_veggies)+log(drinks_water)+log(exercises_often+1)+log(Age)+log(num_meals)
                 +log(time_using_tech+1), data=data, subset=index)))

b = boot(df ,boot.fn ,1000)
b.coef = summary(b)$bootSE

sum = summary(lm(log.model.noWH, data = df))
coef = as.numeric(sum$coefficients[ ,2])

cat("\nLog model - Difference between Std.errors:\n",coef - b.coef,"\n")
