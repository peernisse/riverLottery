#Model building script for Idaho 4 rivers lottery prediction model
#Peter EerNisse | peernisse@gmail.com
#https://github.com/peernisse/riverLottery


#Library call store--------------
library(tidyverse) # metapackage with lots of helpful functions
library(foreign) #Read files in Stata .dta format
library(reticulate) #Call Python from R
library(tensorflow) #Neural Network Backend for Keras
library(keras) #Neural Network Modeling
library(plyr) #Data manipulation
library(dplyr) # Data Manipulation
#library(caret) #Machine Learning Tools for Training and Validation




#First basic model--------------------
#Wrap the model build code as a function for future use on multiple experiments

basic_model <- function(lst) {
  
  #lst<-df_mf
  
  
  trx<-lst[[1]] %>% as_tibble(.)
  
  specx<-feature_spec(trx, LABEL ~ .) %>% 
    step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
    fit()
  
  #specx
  
  
  input <- layer_input_from_dataset(trx %>% select(-LABEL))
  
  output <- input %>% 
    layer_dense_features(dense_features(specx)) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  return(model)
}




#Model with dropout-------------------------




#Model to predict number of applicants in future years-----------------
#Using 2015 - 2019 data because 2020 was fucked, and to blunt the exponential growth since 2010 to
#minimize variance


applications_model <- function(lst) {
  
  lst<-mf
  
  
  trx<-lst[[1]] %>% as_tibble(.)
  
  specx<-feature_spec(trx, LABEL ~ .) %>% 
    step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
    fit()
  
  #specx
  
  
  input <- layer_input_from_dataset(trx %>% select(-LABEL))
  
  output <- input %>% 
    layer_dense_features(dense_features(specx)) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  return(model)
}






