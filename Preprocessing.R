df <- read.csv( "D:/Uni/SDA/SDAProject/SDAProject/Dataset/ObesityDataSet_raw_and_data_sinthetic.csv" ) #read data in as an r dataframe

df <- df %>% rename( eats_high_calor_food = FAVC, eats_veggies = FCVC, 
                             num_meals = NCP, eats_snacks = CAEC, drinks_water = CH2O, 
                             counts_calories = SCC, exercises_often = FAF, 
                             time_using_tech = TUE, drinks_alcohol = CALC, 
                             method_trans = MTRANS, weight_category = NObeyesdad ) %>%
mutate( bmi = Weight / (Height^2) )

df <- df[,-17]

df <-as.data.frame( model.matrix(~Gender+Age+Height+Weight+family_history_with_overweight
                                 +eats_high_calor_food+eats_veggies+num_meals+eats_snacks
                                 +SMOKE+drinks_water+counts_calories+exercises_often
                                 +time_using_tech+drinks_alcohol+method_trans+bmi,data=df))

df<-df[,-1]
write.csv(df, file = "./Dataset/dataset_preprocessed.csv")


