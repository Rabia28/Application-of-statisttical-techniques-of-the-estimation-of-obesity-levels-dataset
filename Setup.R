if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggfortify")) install.packages("ggfortify")
if(!require("tidymodels")) install.packages("tidymodels")
if(!require("psych")) install.packages("psych")
if(!require("ISLR")) install.packages("ISLR")
if(!require("boot")) install.packages("boot")
if(!require("leaps")) install.packages("leaps")
if(!require("plotmo")) install.packages("plotmo")
if(!require("pls")) install.packages("pls")

library( tidyverse ) #tidyverse to model our data & other functions
library( ggplot2 ) #basic plotting of our lin. model fit + data
library( ggfortify ) #plot diagnostics of our lin. model
library( tidymodels )
#library (GGally)
library(psych)
library(ISLR)
library(boot)
library(plotmo)
library(glmnet)
library(leaps)
library(pls)

df = read.csv("C:/Users/ida_d/Desktop/project/SDAProject/Dataset/dataset_preprocessed.csv")
df <- df[,-1]
attach(df)

#simple function to evaluate the MSE
mse_func=function(actual,predicted)
{
  mean( (actual-predicted)^2 )
}



#()
#(Gender+family_history_with_overweight+eats_high_calor_food+eats_veggies+eats_snacks+SMOKE+drinks_water+counts_calories+exercises_often+drinks_alcohol+method_trans+poly(Age,2)+poly(num_meals,2)+poly(time_using_tech,2))

linear.model.all <- (bmi~.)

linear.model.noWH <- (bmi~.-Weight-Height)

linear.model.drop <- ((bmi~.-Weight-Height-SMOKEyes-drinks_alcoholno-drinks_alcoholSometimes
                       -drinks_alcoholFrequently-counts_caloriesyes-num_meals-GenderMale))


poly2.model.noWH <- (bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                     +eats_snacksno+eats_snacksFrequently+eats_snacksSometimes+SMOKEyes+counts_caloriesyes
                     +drinks_alcoholno+drinks_alcoholSometimes+drinks_alcoholFrequently+method_transWalking
                     +method_transBike+method_transMotorbike+method_transPublic_Transportation
                     +poly(Age,2)+poly(num_meals,2)+poly(time_using_tech,2)
                     +poly(eats_veggies,2)+poly(drinks_water,2)+poly(exercises_often,2))

poly3.model.noWH <- (bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                     +eats_snacksno+eats_snacksFrequently+eats_snacksSometimes+SMOKEyes+counts_caloriesyes
                     +drinks_alcoholno+drinks_alcoholSometimes+drinks_alcoholFrequently+method_transWalking
                     +method_transBike+method_transMotorbike+method_transPublic_Transportation
                     +poly(Age,3)+poly(num_meals,3)+poly(time_using_tech,3)
                     +poly(eats_veggies,3)+poly(drinks_water,3)+poly(exercises_often,3))

poly4.model.noWH <- (bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                     +eats_snacksno+eats_snacksFrequently+eats_snacksSometimes+SMOKEyes+counts_caloriesyes
                     +drinks_alcoholno+drinks_alcoholSometimes+drinks_alcoholFrequently+method_transWalking
                     +method_transBike+method_transMotorbike+method_transPublic_Transportation
                     +poly(Age,4)+poly(num_meals,4)+poly(time_using_tech,4)
                     +poly(eats_veggies,4)+poly(drinks_water,4)+poly(exercises_often,4))



log.model.noWH <- (bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                   +eats_snacksno+eats_snacksFrequently+eats_snacksSometimes+SMOKEyes+counts_caloriesyes
                   +drinks_alcoholno+drinks_alcoholSometimes+drinks_alcoholFrequently+method_transWalking
                   +method_transBike+method_transMotorbike+method_transPublic_Transportation
                   +log(eats_veggies)+log(drinks_water)+log(exercises_often+1)+log(Age)+log(num_meals)
                   +log(time_using_tech+1))

models = c(linear.model.all,linear.model.noWH,linear.model.drop,poly2.model.noWH,poly3.model.noWH,poly4.model.noWH,log.model.noWH)

#models for PCA/PCR
linear.model.all.PCR <- (bmi~.-drinks_alcoholno-drinks_alcoholSometimes-drinks_alcoholFrequently-eats_snacksno
                     -eats_snacksFrequently-eats_snacksSometimes-method_transWalking
                     -method_transBike-method_transMotorbike-method_transPublic_Transportation)

linear.model.noWH.PCR <- (bmi~.-Weight-Height-drinks_alcoholno-drinks_alcoholSometimes-drinks_alcoholFrequently-eats_snacksno
                      -eats_snacksFrequently-eats_snacksSometimes-method_transWalking
                      -method_transBike-method_transMotorbike-method_transPublic_Transportation)


poly2.model.noWH.PCR <- (bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                         +SMOKEyes+counts_caloriesyes+poly(Age,2)+poly(num_meals,2)+poly(time_using_tech,2)
                         +poly(eats_veggies,2)+poly(drinks_water,2)+poly(exercises_often,2))

poly3.model.noWH.PCR <- (bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                     +SMOKEyes+counts_caloriesyes+poly(Age,3)+poly(num_meals,3)+poly(time_using_tech,3)
                     +poly(eats_veggies,3)+poly(drinks_water,3)+poly(exercises_often,3))

poly4.model.noWH.PCR <- (bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                         +SMOKEyes+counts_caloriesyes+poly(Age,4)+poly(num_meals,4)+poly(time_using_tech,4)
                         +poly(eats_veggies,4)+poly(drinks_water,4)+poly(exercises_often,4))



log.model.noWH.PCR <- (bmi~GenderMale+family_history_with_overweightyes+eats_high_calor_foodyes
                   +SMOKEyes+counts_caloriesyes+log(eats_veggies)+log(drinks_water)
                   +log(exercises_often+1)+log(Age)+log(num_meals)+log(time_using_tech+1))
