#Model building script for Idaho 4 rivers lottery prediction model
#Peter EerNisse | peernisse@gmail.com
#https://github.com/peernisse/riverLottery

source('dataSetup.R')
source('riverModels.R')


use_condaenv('r-reticulate')


#Clear the puthon and tensorflow environments-----------------------
###

K <- backend()
K$clear_session()


#Model runs------------------------------------------------------
#Create train and test sets-------------------
#Try MF salmon by itself
#Explore data and fit model
head(df_mf[[1]])
train<-df_mf[[1]]
test<-df_mf[[2]]

model<-basic_model(df_mf)#The function will pull out the train df


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

test_predictions <- model %>% predict(test %>% select(-LABEL) )

#test_predictions[ , 1]


#Check how the predicted values align with the given train values
xxx<-test %>% mutate(PRED = test_predictions)

mdl<-lm(xxx$PRED~xxx$LABEL)

plot(xxx$PRED~xxx$LABEL)
abline(mdl)

summary(mdl)

plot(data=xxx,LABEL~TOTAL_APPLICATIONS)
abline(v=median(xxx$TOTAL_APPLICATIONS),col="blue")
abline(v=mean(xxx$TOTAL_APPLICATIONS),col="red")
abline(h=median(xxx$LABEL))
abline(h=mean(xxx$LABEL))

median(xxx$TOTAL_APPLICATIONS)
mean(xxx$TOTAL_APPLICATIONS)

median(xxx$LABEL)*100
mean(xxx$LABEL)*100



#Plot probs in time

zzz<-xxx %>% 
  filter(GROUP_N == 6) %>% #Pretend 6 friends apply same dates
  dplyr::group_by(WEEK,WDAY_N) %>% 
  dplyr::summarize(PROB=mean(PRED)*100 %>% round(0))

ggplot(zzz,aes(WEEK,PROB,color=WDAY_N))+
  geom_point()+
  geom_smooth()

ggplot(zzz,aes(x=as.character(WEEK),y=PROB))+
  geom_boxplot(fill="lightblue")

lubridate::week(as.Date('8/1/2020',format="%m/%d/%Y"))


#Save model for future use------------------------------
#Save
model %>% save_model_tf("mf_model")
list.files("mf_model")

#Reload a saved model
new_model <- load_model_tf("mf_model")
summary(new_model)


#TRYING HYPOTHETICAL 2022 dates
new22<-test %>% 
  filter(
    YEAR == 2020
  ) %>% 
  mutate(YEAR = 2022)


test_predictions22 <- model %>% predict(new22 %>% select(-LABEL) )

chk22<-new22 %>% mutate(LABEL=test_predictions22*100) %>% 
  arrange(GROUP_N,WEEK)

ggplot(chk22,aes(WEEK,LABEL,color=as.character(GROUP_N)))+
  theme(plot.margin = margin(.5,.25,.25,.25,"in"))+
  geom_point()+
  geom_smooth()+
  labs(y="Probability of Lottery Sucess (percent)",x="Week",
       title="Middle Fork Salmon River Lottery Predictions - 2022",color="No. of Friends Applying\nSame Day")




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


























