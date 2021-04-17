#Model building script for Idaho 4 rivers lottery prediction model
#Peter EerNisse | peernisse@gmail.com
#https://github.com/peernisse/riverLottery

source('dataSetup.R')
source('riverModels.R')


#use_condaenv('r-reticulate')


#Create train and test sets-------------------
###

K <- backend()
K$clear_session()


#Model runs------------------------------------------------------
#Try MF salmon by itself
#Explore data and fit model
head(df_mf[[1]])
train<-df_mf[[1]]
test<-df_mf[[2]]

basic_model(df_mf)#The function will pull out the train df


history <- model %>% fit(
  x = train %>% select(-LABEL),
  y = train$LABEL,
  epochs = 20,
  validation_split = 0.2,
  verbose = 1,
  callbacks = callback_tensorboard("logs/run_a")
)


#See how the model performs on the test data set---------------------

#Evaluate model
results<-model %>% evaluate(test %>% select(-LABEL), test$LABEL, verbose = 0)
results

# Use model to make a prediction---------------------------

#Predict some probabilities from the testing data set

test_predictions <- model %>% predict(test %>% select(-LABEL) %>% filter(GROUP_N ==6))

test_predictions[ , 1]


#Check how the predicted values align with the given train values
xxx<-test %>% filter(GROUP_N ==6) %>% mutate(PRED = test_predictions)

mdl<-lm(xxx$PRED~xxx$LABEL)

plot(xxx$PRED~xxx$LABEL)
abline(mdl)

summary(mdl)

#Plot probs in time

zzz<-xxx %>% 
  filter(GROUP_N == 6) %>% 
  group_by(WEEK,WDAY_N) %>% 
  summarize(PROB=mean(PRED)*100 %>% round(0))

ggplot(zzz,aes(WEEK,PROB,color=WDAY_N))+
  geom_point()+
  geom_smooth()




#Trying tensorboard----------

history <- model %>% fit(
  x = train_dfx %>% select(-LABEL),
  y = train_dfx$LABEL,
  epochs = 20,
  validation_split = 0.2,
  verbose = 1,
  callbacks = callback_tensorboard("logs/run_a")
)

plot(history)

tensorflow::tf_config()

tensorboard("logs/run_a") 
#If this does not work from RStudio, open terminal with comand line and enter 
#tensorboard --logdir logs.run_a
#This should serve at http://localhost:6006/

#Write to local machine
library(keras)
use_condaenv("r-reticulate")

writemygraph<-tf$summary$FileWriter('./graphs',graph)


tf$executing_eagerly()

writemygraph<-tf$summary$create_file_writer('./graphs',tf$Graph())

writemygraph
tensorflow::tf_config()

callbacks = callback_tensorboard('./graphs')

tensorboard(log_dir='./graphs')

tensorflow::tf_config()

tensorboard()


























