#'Script riverApplications
#'This will provide predicted number of applications for each calendar day
#'to be used before sending the dataframe into the probability of win calculator
#peernisse@gmail.com
#'
#'

#Load libraries sources functions and options

source('dataSetup.R')
source('riverModels.R')
options(scipen = 6)


#Clear the python and tensorflow environments-----------------------
###Need to do a lot during testing
#Ensure python tensorflow environment----------------
use_condaenv('r-reticulate')

use_condaenv('r-tensorflow')

install_tensorflow()


K <- backend()
#K$clear_session()

#Ensure python tensorflow environment----------------
use_condaenv('r-reticulate')


#Objectives-----------------------------
#Get data set per river to predict applications by date
#Using >2015 to blunt exponential shape of application number growth
#Omit 2021 bc lots of 202 apps rolled over to 2021 due to COVID

#Middle fork
mf<-outputdf %>% 
  filter(
    RIVER_N == 1,
    YEAR %in% c(2016,2017,2018,2019,2020)#2021 is not included because COVID affected the numbers
    #GROUP_N == 6
  )

#Data manipulation-------------------------
#Select columns rename predicted value as LABEL
mf<-mf %>% 
  select(MONTH_N, YEAR, WEEK , WDAY_N,TOTAL_APPLICATIONS, GROUP_N) %>% 
  mutate(
    LABEL = TOTAL_APPLICATIONS
  ) %>% 
  select(-TOTAL_APPLICATIONS) %>% 
  arrange(WEEK)

#Convert to train and test dfs------------
mf<-mf %>% 
  mkSets(.)


#Keras regression trials---------------------------


model<-applications_model(mf)#The function will pull out the train df

head(mf[[1]])
train<-mf[[1]]
test<-mf[[2]]


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



#Predict some probabilities from the testing data set

test_predictions <- model %>% predict(test %>% select(-LABEL))

test_predictions[ , 1]


#Check how the predicted values align with the given train values
xxx<-test %>% mutate(PRED = test_predictions)

mdl<-lm(xxx$PRED~xxx$LABEL)

plot(xxx$PRED~xxx$LABEL)
abline(mdl)

summary(mdl)








#Try lm method----------------------
#Data manipulation------------------------- %>% 

mdl<-lm(LABEL ~ MONTH_N,data=mf)




mfpred<-predict(mdl,select(mf,MONTH_N)) %>% 
  as.data.frame(.)
names(mfpred)<-c("APPLICANTS")


xxx<- mf %>% 
  bind_cols(mfpred)

plot(LABEL ~ MONTH_N,data=mf)

plot(log10(LABEL) ~ MONTH_N,data=mf)


#Model dev--------------------------------


#Predict applications for a calendar year---------------------
#Linear model trials----------

mdl2<-glm(LABEL~WEEK,data=mf)
summary(mdl2)

boxplot(LABEL~WEEK,data=mf,ylab = "Applications")
abline(mdl2)

g<-ggplot()+
  #geom_smooth(data=mf,aes(WEEK,y=LABEL))+
  geom_boxplot(data=mf,aes(x=as.character(WEEK),y=LABEL),fill="lightblue")+
  theme(plot.margin = unit(c(0.5,0.5,0.25,0.25), units = "in"))+
  
  labs(y="Number of Applications in that Week",
       x="Week", title = "Distribution of River Applications by Week | 2017 : 2020")

ggplotly(g)


mf %>% filter(WEEK==27) %>% arrange(desc(DATE))

