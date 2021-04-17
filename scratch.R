
library(reticulate)
os <- import("os")
os$listdir(".")

library(tensorflow)

install_tensorflow(method = 'conda', envname = 'r-reticulate')

library(keras)
keras::install_keras()


library(reticulate)
os <- import("os")
os$listdir(".")



strsplit(shell("set path",intern=TRUE),split=";")

conda_python()
conda_list()
use_condaenv('r-reticulate')
library(tensorflow)
install_tensorflow(method = 'conda',envname = 'r-reticulate')

library(reticulate)

use_condaenv('r-reticulate')

library(tensorflow)

tf$constant("Hellow Tensorflow")

reticulate::py_discover_config()

reticulate::use_condaenv("r-reticulate", required = TRUE)
library(keras)

#'Deal with error about rpytools

reticulate::py_versions_windows()
reticulate::py_config()
reticulate::py_discover_config("keras")
reticulate::py_discover_config("tensorflow")

#The folder "rpytools" needed is in the reticulate\python folder. 
#Copy this into keras\python and it should work (mine does)....Good luck1
#from:https://www.gitmemory.com/issue/rstudio/keras/615/515803770

library(reticulate)
x <- py_eval("1 + 1", convert = FALSE)
rstudioapi::restartSession()

#Old stuff from the tutorial-----------------------------------------

# The spec created with tfdatasets can be used together with layer_dense_features to 
# perform pre-processing directly in the TensorFlow graph.
# 
# We can take a look at the output of a dense-features layer created by this spec:

# layer<-layer_dense_features(
#   feature_columns = dense_features(specx),
#   dtype = tf$float32
# )
# 
# layer(train_dfx)

#Launch tensorboard--------------
#If this does not work from RStudio, open terminal with comand line and enter 
#tensorboard --logdir logs/run_a
#This should serve at http://localhost:6006/

#Create the Model-----------------------

input<-layer_input_from_dataset(train_dfx %>% select(-LABEL))

output<-input %>% 
  #layer_dense_features(dense_features(specx)) %>% 
  layer_dense_features(dense_features(specx)) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1) 

model <- keras_model(input, output)

summary(model)

#Compile the model-------------------------------------

model %>% 
  compile(
    loss = 'mse',
    optimizer = optimizer_rmsprop(),
    metrics = list('mean_absolute_error')
  )

#Wrap the model build code as a function for future use on multiple experiments

build_model <- function() {
  input <- layer_input_from_dataset(train_dfx %>% select(-LABEL))
  
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

#Train the model-------------------------------
#Create a dot progress meter

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

model <- build_model()

#Remember that Keras fit modifies the model in-place.

history <- model %>% fit(
  x = train_dfx %>% select(-LABEL),
  y = train_dfx$LABEL,
  epochs = 20,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)


#Visualize the model training progress in ggplot2 from the history file we just made

plot(history)

# This graph shows little improvement in the model after about xx epochs. 
# Let’s update the fit method to automatically stop training when the validation score doesn’t improve. 
# We’ll use a callback that tests a training condition for every epoch. 
# If a set amount of epochs elapses without showing improvement, it automatically stops the training.

# The patience parameter is the amount of epochs to check for improvement.
#Early stopping is a tecnique to prevent over fitting

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 12)

model <- build_model()

history <- model %>% fit(
  x = train_dfx %>% select(-LABEL),
  y = train_dfx$LABEL,
  epochs = 20,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback,early_stop)
)

plot(history)

















