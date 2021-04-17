#Model building script for Idaho 4 rivers lottery prediction model
#Peter EerNisse | peernisse@gmail.com
#https://github.com/peernisse/riverLottery

#First basic model--------------------
#Wrap the model build code as a function for future use on multiple experiments

basic_model <- function(lst) {
  
  lst<-df_mf
  
  
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
  
  model
}




#Model with dropout-------------------------










