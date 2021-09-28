#Plotting of variables against BMI values
dev.new()
par(mfrow=c(3,1))
plot(Height, bmi,main="bmi-regressors trend",xlab="Height", ylab="bmi")
plot(Weight, bmi, xlab="weight", ylab="bmi")
plot(factor(SMOKE), bmi, xlab="SMOKE", ylab="bmi")


dev.new()
par(mfrow=c(3,2))
plot(factor(Gender), bmi,main="bmi-regressors trend", xlab="Gender", ylab="bmi")
plot(Age, bmi, xlab="Age", ylab="bmi")
plot(factor(family_history_with_overweight), bmi, xlab="family_history_with_overweight", ylab="bmi")
plot(factor(eats_high_calor_food), bmi, xlab="eats_high_calor_food", ylab="bmi")
plot(eats_veggies, bmi, xlab="eats veggies", ylab="bmi")
plot(num_meals,bmi, xlab = "Number of Meals", ylab="bmi")

dev.new()
par(mfrow=c(4,2))
plot(factor(eats_snacks),bmi, xlab = "eats snacks", ylab = "bmi")
plot(factor(SMOKE),bmi, xlab = "smoke", ylab= "bmi")
plot(drinks_water, bmi, xlab = "drinks water", ylab = "bmi")
plot(factor(counts_calories), bmi, xlab = "counts calories", ylab="bmi")
plot(exercises_often, bmi, xlab = "exercises often", ylab="bmi")
plot(time_using_tech, bmi, xlab="time spent using tech", ylab="bmi")
plot(factor(drinks_alcohol),bmi, xlab ="drinks alcohol", ylab="bmi")
plot(factor(method_trans),bmi, xlab="method_trans", ylab="bmi")


multi.hist(df$num_meals)

#ggpairs(df)


ggplot( train_obs, aes( y = bmi, x = factor( eats_high_calor_food ) ) ) +
  geom_boxplot() +
  ggtitle( 'BMI ~ Eats High Calorie Foods' ) +
  xlab( 'Eats High Calorie Foods' )