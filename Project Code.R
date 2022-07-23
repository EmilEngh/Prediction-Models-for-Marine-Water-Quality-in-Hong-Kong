library(readxl)
library(GGally)
library(ggfortify)
library(ISLR)
library(MASS)
library(tidyverse)
library(kknn)
library(discrim)
library(klaR)
library(tidymodels)
library(leaps)
library(gam) 
library(rpart)
library(baguette)
library(xgboost)
library(lubridate)
# Work directory 
setwd("C:/Users/Bruker/Documents/University stuff/Semester 3/Machine Learning/Project")
# Functions 
# for uploading and structuring the climatological data
upload <- function(folder, parameter, year){
  table <- read_excel(paste0(folder, parameter," ", year, ".xlsx")) %>% mutate(Year = year)
  table <- table %>% rename("Okt" = Oct, "Mai" = May, "Des" = Dec)
  table <- table %>% pivot_longer(cols = !c(Day, Year), names_to = "Month", values_to = "col_name") 
  table <- table %>% unite(Dates, Year, Month, Day, sep="/") %>% mutate(Dates = as.Date(Dates, "%Y/%b/%d"))
  table <- table %>% mutate(Dates = as.Date(Dates, "%Y/%b/%d"))
}

# Datasets
# water quality
files <- list.files(path = "WQ data", pattern="*.csv")
WaterQ <- list()
for (i in 1:length(files)) {
  WaterQ[[i]] <- read_csv(paste0(getwd(),"/WQ data/", files[i]))
}
WaterQ <- map(WaterQ, as_tibble) %>% 
  bind_rows() %>% 
  transmute(
  Water_zone = Water_zone,
  Station = Station,
  Sample_no = Sample_no,
  Depth = Depth,
  Dates = as.Date(Dates), 
  # Replacing the E. Coli variable with its logarithmic value
  lnEcoli = log(E_coli),
  lnBiochem_ox_demand = log(`5-day_biochemical_oxygen_demand`), 
  lnAmmonia_nitrogen = log(Ammonia_nitrogen), 
  lnChlorophyll_a = log(`Chlorophyll-a`),
  lnDissolved_ox = log(dissolved_oxygen),
  lnDissolved_ox1 = log(dissolved_oxygen_1),
  lnNitrate = log(Nitrate),
  lnNitrite = log(Nitrite), 
  lnOrthophosphate_phosphorus = log(Orthophosphate_phosphorus), 
  lnPhaeo_pigments = log(`Phaeo-pigments`),
  lnSilica = log(Silica), 
  lnSuspended_solids = log(Suspended_solids), 
  lnTot_inorganic_nitrogen = log(Total_inorganic_nitrogen),
  lnKjeldahl_nitrogen = log(Kjeldahl_nitrogen), 
  lnNitrogen = log(Nitrogen),
  lnPhosphorus = log(Phosphorus), 
  lnTurbidity = log(Turbidity), 
  lnUnionised_ammonia = log(Unionised_ammonia), 
  lnVolatile_suspended_solids = log(Volatile_suspended_solids), 
  Temperature = Temperature,
  pH = pH,
  )  %>% 
  filter(Depth == "Surface Water")
# climatological data
# daily wind speed
wind_speed_folder <- paste0(getwd(), "/Daily mean wind speed (km per h) - Wagland Island/")
wind_speed <- list()
wind_speed[[1]] <- upload(wind_speed_folder, "Daily mean wind speed", "2004")
wind_speed[[2]] <- upload(wind_speed_folder, "Daily mean wind speed", "2005")
wind_speed[[3]] <- upload(wind_speed_folder, "Daily mean wind speed", "2006")
wind_speed[[4]] <- upload(wind_speed_folder, "Daily mean wind speed", "2007")
wind_speed[[5]] <- upload(wind_speed_folder, "Daily mean wind speed", "2008")
wind_speed[[6]] <- upload(wind_speed_folder, "Daily mean wind speed", "2009")
wind_speed[[7]] <- upload(wind_speed_folder, "Daily mean wind speed", "2010")
wind_speed[[8]] <- upload(wind_speed_folder, "Daily mean wind speed", "2011")
wind_speed[[9]] <- upload(wind_speed_folder, "Daily mean wind speed", "2012")
wind_speed[[10]] <- upload(wind_speed_folder, "Daily mean wind speed", "2013")
wind_speed[[11]] <- upload(wind_speed_folder, "Daily mean wind speed", "2014")
wind_speed[[12]] <- upload(wind_speed_folder, "Daily mean wind speed", "2015")
wind_speed[[13]] <- upload(wind_speed_folder, "Daily mean wind speed", "2016")
wind_speed[[14]] <- upload(wind_speed_folder, "Daily mean wind speed", "2017")
wind_speed[[15]] <- upload(wind_speed_folder, "Daily mean wind speed", "2018")
wind_speed <- map(wind_speed, as_tibble) %>% bind_rows() %>% rename("wind_speed" = col_name)
# daily wind direction
wind_direction_folder <- paste0(getwd(), "/Daily prevailing wind direction (degrees) - Waglan Island/")
wind_direction <- list()
wind_direction[[1]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2004")
wind_direction[[2]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2005")
wind_direction[[3]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2006")
wind_direction[[4]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2007")
wind_direction[[5]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2008")
wind_direction[[6]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2009")
wind_direction[[7]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2010")
wind_direction[[8]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2011")
wind_direction[[9]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2012")
wind_direction[[10]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2013")
wind_direction[[11]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2014")
wind_direction[[12]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2015")
wind_direction[[13]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2016")
wind_direction[[14]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2017")
wind_direction[[15]] <- upload(wind_direction_folder, "Daily prevailing wind direction", "2018")
wind_direction <- map(wind_direction, as_tibble) %>%
  bind_rows() %>% 
  rename("wind_direction" = col_name) %>% 
  transmute(Dates = Dates, lnWind_direction = log(wind_direction))
# daily rainfall 
rainfall_folder <- paste0(getwd(), "/Daily rainfall - HKO/")
rainfall <- list()
rainfall[[1]] <- upload(rainfall_folder, "Daily rainfall", "2004")
rainfall[[2]] <- upload(rainfall_folder, "Daily rainfall", "2005")
rainfall[[3]] <- upload(rainfall_folder, "Daily rainfall", "2006")
rainfall[[4]] <- upload(rainfall_folder, "Daily rainfall", "2007")
rainfall[[5]] <- upload(rainfall_folder, "Daily rainfall", "2008")
rainfall[[6]] <- upload(rainfall_folder, "Daily rainfall", "2009")
rainfall[[7]] <- upload(rainfall_folder, "Daily rainfall", "2010")
rainfall[[8]] <- upload(rainfall_folder, "Daily rainfall", "2011")
rainfall[[9]] <- upload(rainfall_folder, "Daily rainfall", "2012")
rainfall[[10]] <- upload(rainfall_folder, "Daily rainfall", "2013")
rainfall[[11]] <- upload(rainfall_folder, "Daily rainfall", "2014")
rainfall[[12]] <- upload(rainfall_folder, "Daily rainfall", "2015")
rainfall[[13]] <- upload(rainfall_folder, "Daily rainfall", "2016")
rainfall[[14]] <- upload(rainfall_folder, "Daily rainfall", "2017")
rainfall[[15]] <- upload(rainfall_folder, "Daily rainfall", "2018")
rainfall <- map(rainfall, as_tibble) %>% bind_rows() %>% rename("rainfall" = col_name)
# sea temp 
sea_temp_folder <- paste0(getwd(), "/Daily sea temp - North Point/")
sea_temp <- list()
sea_temp[[1]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2004")
sea_temp[[2]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2005")
sea_temp[[3]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2006")
sea_temp[[4]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2007")
sea_temp[[5]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2008")
sea_temp[[6]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2009")
sea_temp[[7]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2010")
sea_temp[[8]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2011")
sea_temp[[9]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2012")
sea_temp[[10]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2013")
sea_temp[[11]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2014")
sea_temp[[12]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2015")
sea_temp[[13]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2016")
sea_temp[[14]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2017")
sea_temp[[15]] <- upload(sea_temp_folder, "Daily mean sea temperature", "2018")
sea_temp <- map(sea_temp, as_tibble) %>% bind_rows() %>% rename("sea_temperature" = col_name)
# solar radiation 
solar_radiation_folder <- paste0(getwd(), "/Daily solar radiation - Kings Park/")
solar_radiation <- list()
solar_radiation[[1]] <- upload(solar_radiation_folder, "Daily solar radiation", "2004")
solar_radiation[[2]] <- upload(solar_radiation_folder, "Daily solar radiation", "2005")
solar_radiation[[3]] <- upload(solar_radiation_folder, "Daily solar radiation", "2006")
solar_radiation[[4]] <- upload(solar_radiation_folder, "Daily solar radiation", "2007")
solar_radiation[[5]] <- upload(solar_radiation_folder, "Daily solar radiation", "2008")
solar_radiation[[6]] <- upload(solar_radiation_folder, "Daily solar radiation", "2009")
solar_radiation[[7]] <- upload(solar_radiation_folder, "Daily solar radiation", "2010")
solar_radiation[[8]] <- upload(solar_radiation_folder, "Daily solar radiation", "2011")
solar_radiation[[9]] <- upload(solar_radiation_folder, "Daily solar radiation", "2012")
solar_radiation[[10]] <- upload(solar_radiation_folder, "Daily solar radiation", "2013")
solar_radiation[[11]] <- upload(solar_radiation_folder, "Daily solar radiation", "2014")
solar_radiation[[12]] <- upload(solar_radiation_folder, "Daily solar radiation", "2015")
solar_radiation[[13]] <- upload(solar_radiation_folder, "Daily solar radiation", "2016")
solar_radiation[[14]] <- upload(solar_radiation_folder, "Daily solar radiation", "2017")
solar_radiation[[15]] <- upload(solar_radiation_folder, "Daily solar radiation", "2018")
solar_radiation <- map(solar_radiation, as_tibble) %>% bind_rows() %>% rename("solar_radiation" = col_name)
# max temp
max_temp_folder <- paste0(getwd(), "/Max temperatures - HKO/")
max_temp <- list()
max_temp[[1]] <- upload(max_temp_folder, "Max temp", "2004")
max_temp[[2]] <- upload(max_temp_folder, "Max temp", "2004")
max_temp[[3]] <- upload(max_temp_folder, "Max temp", "2006")
max_temp[[4]] <- upload(max_temp_folder, "Max temp", "2007")
max_temp[[5]] <- upload(max_temp_folder, "Max temp", "2008")
max_temp[[6]] <- upload(max_temp_folder, "Max temp", "2009")
max_temp[[7]] <- upload(max_temp_folder, "Max temp", "2010")
max_temp[[8]] <- upload(max_temp_folder, "Max temp", "2011")
max_temp[[9]] <- upload(max_temp_folder, "Max temp", "2012")
max_temp[[10]] <- upload(max_temp_folder, "Max temp", "2013")
max_temp[[11]] <- upload(max_temp_folder, "Max temp", "2014")
max_temp[[12]] <- upload(max_temp_folder, "Max temp", "2015")
max_temp[[13]] <- upload(max_temp_folder, "Max temp", "2016")
max_temp[[14]] <- upload(max_temp_folder, "Max temp", "2017")
max_temp[[15]] <- upload(max_temp_folder, "Max temp", "2018")
max_temp <- map(max_temp, as_tibble) %>% bind_rows() %>% rename("max_temperature" = col_name)
# mean temp 
mean_temp_folder <- paste0(getwd(), "/Mean temperatures - HKO/")
mean_temp <- list()
mean_temp[[1]] <- upload(mean_temp_folder, "Daily mean temperature", "2004")
mean_temp[[2]] <- upload(mean_temp_folder, "Daily mean temperature", "2005")
mean_temp[[3]] <- upload(mean_temp_folder, "Daily mean temperature", "2006")
mean_temp[[4]] <- upload(mean_temp_folder, "Daily mean temperature", "2007")
mean_temp[[5]] <- upload(mean_temp_folder, "Daily mean temperature", "2008")
mean_temp[[6]] <- upload(mean_temp_folder, "Daily mean temperature", "2009")
mean_temp[[7]] <- upload(mean_temp_folder, "Daily mean temperature", "2010")
mean_temp[[8]] <- upload(mean_temp_folder, "Daily mean temperature", "2011")
mean_temp[[9]] <- upload(mean_temp_folder, "Daily mean temperature", "2012")
mean_temp[[10]] <- upload(mean_temp_folder, "Daily mean temperature", "2013")
mean_temp[[11]] <- upload(mean_temp_folder, "Daily mean temperature", "2014")
mean_temp[[12]] <- upload(mean_temp_folder, "Daily mean temperature", "2015")
mean_temp[[13]] <- upload(mean_temp_folder, "Daily mean temperature", "2016")
mean_temp[[14]] <- upload(mean_temp_folder, "Daily mean temperature", "2017")
mean_temp[[15]] <- upload(mean_temp_folder, "Daily mean temperature", "2018")
mean_temp <- map(mean_temp, as_tibble) %>% bind_rows() %>% rename("mean_temperature" = col_name)
# min temp
min_temp_folder <- paste0(getwd(), "/Min temperatures - HKO/")
min_temp <- list()
min_temp[[1]] <- upload(min_temp_folder, "Daily minimum temperature", "2004")
min_temp[[2]] <- upload(min_temp_folder, "Daily minimum temperature", "2005")
min_temp[[3]] <- upload(min_temp_folder, "Daily minimum temperature", "2006")
min_temp[[4]] <- upload(min_temp_folder, "Daily minimum temperature", "2007")
min_temp[[5]] <- upload(min_temp_folder, "Daily minimum temperature", "2008")
min_temp[[6]] <- upload(min_temp_folder, "Daily minimum temperature", "2009")
min_temp[[7]] <- upload(min_temp_folder, "Daily minimum temperature", "2010")
min_temp[[8]] <- upload(min_temp_folder, "Daily minimum temperature", "2011")
min_temp[[9]] <- upload(min_temp_folder, "Daily minimum temperature", "2012")
min_temp[[10]] <- upload(min_temp_folder, "Daily minimum temperature", "2013")
min_temp[[11]] <- upload(min_temp_folder, "Daily minimum temperature", "2014")
min_temp[[12]] <- upload(min_temp_folder, "Daily minimum temperature", "2015")
min_temp[[13]] <- upload(min_temp_folder, "Daily minimum temperature", "2016")
min_temp[[14]] <- upload(min_temp_folder, "Daily minimum temperature", "2017")
min_temp[[15]] <- upload(min_temp_folder, "Daily minimum temperature", "2018")
min_temp <- map(min_temp, as_tibble) %>% bind_rows() %>% rename("min_temperature" = col_name)
# total bright sunshine
total_sunshine_folder <- paste0(getwd(), "/Total bright sunshine (hours) - Kings Park/")
total_sunshine <- list()
total_sunshine[[1]] <- upload(total_sunshine_folder, "Total bright sunshine", "2004")
total_sunshine[[2]] <- upload(total_sunshine_folder, "Total bright sunshine", "2005")
total_sunshine[[3]] <- upload(total_sunshine_folder, "Total bright sunshine", "2006")
total_sunshine[[4]] <- upload(total_sunshine_folder, "Total bright sunshine", "2007")
total_sunshine[[5]] <- upload(total_sunshine_folder, "Total bright sunshine", "2008")
total_sunshine[[6]] <- upload(total_sunshine_folder, "Total bright sunshine", "2009")
total_sunshine[[7]] <- upload(total_sunshine_folder, "Total bright sunshine", "2010")
total_sunshine[[8]] <- upload(total_sunshine_folder, "Total bright sunshine", "2011")
total_sunshine[[9]] <- upload(total_sunshine_folder, "Total bright sunshine", "2012")
total_sunshine[[10]] <- upload(total_sunshine_folder, "Total bright sunshine", "2013")
total_sunshine[[11]] <- upload(total_sunshine_folder, "Total bright sunshine", "2014")
total_sunshine[[12]] <- upload(total_sunshine_folder, "Total bright sunshine", "2015")
total_sunshine[[13]] <- upload(total_sunshine_folder, "Total bright sunshine", "2016")
total_sunshine[[14]] <- upload(total_sunshine_folder, "Total bright sunshine", "2017")
total_sunshine[[15]] <- upload(total_sunshine_folder, "Total bright sunshine", "2018")
total_sunshine <- map(total_sunshine, as_tibble) %>% bind_rows() %>% rename("total_bright_sunshine" = col_name)
# merging water quality and climatological data
d <- WaterQ %>% inner_join(wind_speed, by = "Dates")
d <- d %>% inner_join(wind_direction, by = "Dates")
d <- d %>% inner_join(rainfall, by = "Dates")
d <- d %>% inner_join(sea_temp, by = "Dates")
d <- d %>% inner_join(solar_radiation, by = "Dates")
d <- d %>% inner_join(max_temp, by = "Dates")
d <- d %>% inner_join(mean_temp, by = "Dates")
d <- d %>% inner_join(min_temp, by = "Dates")
d <- d %>% inner_join(total_sunshine, by = "Dates")
# drop_na
d <- d %>% drop_na()
# creating a variable with the past day's date
d <- d %>% mutate(past_day = Dates - ddays(1))
# past day E. coli average
past_day_ecoli_average <- d %>%
  group_by(Dates) %>% 
  summarise(past_day_ecoli_avg = mean(lnEcoli)) %>% 
  rename("past_day" = Dates)
final <- d %>% inner_join(past_day_ecoli_average, by = "past_day")
# past day solar radiation 
past_day_solar_rad_avg <- d %>% 
  group_by(Dates) %>% 
  summarise(past_day_sol_rad_avg = mean(solar_radiation)) %>%
  rename(past_day = Dates)
final <- final %>% inner_join(past_day_solar_rad_avg, by = "past_day")
# past day total bright sunshine
past_dat_tot_bright_sunshine <- d %>% 
  group_by(Dates) %>% 
  summarise(past_tot_bright_sunshine = mean(total_bright_sunshine)) %>%
  rename(past_day = Dates)
final <- final %>% inner_join(past_day_solar_rad_avg, by = "past_day") 

# Data analysis
analysis <- final %>% select(-c(Water_zone, Station, Dates, Sample_no, past_day, Depth))
correlation <- cor(analysis, y = analysis$lnEcoli, method="pearson") 
set.seed(1)
s <- analysis %>% initial_time_split(prop = 3/4)
training <- training(s)
testing <- testing(s)
crossV <- vfold_cv(training)
# Best subsets
best_subsets <- regsubsets(lnEcoli~., data = training, nvmax = 10, method = "exhaustive") %>%
  tidy() %>% 
  filter(adj.r.squared == max(adj.r.squared))
# Recipe
f1 <- recipe(lnEcoli ~., data = training) %>% step_normalize(all_predictors()) #%>%
  #step_pca(all_predictors(), num_comp = tune())
# Recipe based on the best coeffiecients from best subsets analysis
f2 <- recipe(lnEcoli ~ lnBiochem_ox_demand + lnAmmonia_nitrogen + lnDissolved_ox + lnNitrate + 
             lnOrthophosphate_phosphorus + lnSilica + sea_temperature + max_temperature +
             past_day_ecoli_avg, 
             data = training) %>%
  step_normalize(all_predictors())  
# Models
m1 <- linear_reg() %>% set_engine("lm")
m2 <- linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet") 
m3 <- linear_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet") # Higher R squared with ridge
m4 <- decision_tree(cost_complexity = tune()) %>% set_engine("rpart") %>% set_mode("regression")
m5 <- bag_tree(cost_complexity = tune()) %>% set_engine("rpart") %>% set_mode("regression")
m6 <- boost_tree(learn_rate = tune()) %>% set_engine("xgboost") %>% set_mode("regression")
m7 <- rand_forest(mtry = tune()) %>% set_engine("ranger") %>% set_mode("regression")
# Workflow 
w <- workflow() %>% add_recipe(f2) %>% add_model(m1) 
# Tuning 
tuning <- w %>% tune_grid(resamples = crossV, grid = 10)
tuning %>% collect_metrics()
# Finalizing workflow, fitting the training set on predicting the testing set
w <- w %>% finalize_workflow(tuning %>% select_best("rsq"))
fit <- w %>% fit(training)
predict <- fit %>% predict(testing) %>%
  bind_cols(testing) %>% 
  filter_all(all_vars(!is.infinite(.pred))) %>% 
  rsq(.pred, adj = TRUE, type = lnEcoli)
predict

rsq
