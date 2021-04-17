#Data setup script for Idaho 4 rivers lottery prediction model
#Peter EerNisse | peernisse@gmail.com
#https://github.com/peernisse/riverLottery


#Script to download river lottery data and calculate probabilities of win for each available date


library(reticulate)
#use_condaenv('r-reticulate')
library(tensorflow)
library(keras)
library(tidyverse)
library(tfdatasets)
library(pdftools)
library(tabulizer)
library(openxlsx)
source("helpers.R")


#Get data sets------------------

#4 rivers 10 year lottery summary stats

#Data files downloaded manually from:

#https://www.fs.usda.gov/detail/scnf/passes-permits/recreation/?cid=stelprdb5448165

files<-list.files('./data/',full.names = T)
files


filesNames<-list.files('./data/')
filesNames

#Load Workbooks----------------------
#Read in workbooks from excel workbooks to a list

rList<-list()

for(i in 1:length(files)){
  
  #Test
  #i<-2
  ###
  
  wb<-loadWorkbook(files[i])
  name<-filesNames[i]
  
  rList[name][[1]]<-wb
  
}

#TODO:If doing this with fresh data files, there are comments in the 2021 workbook that need to be deleted
#first or will not import

#Combine data---------------
#Each river is on its own tab, but the format of each tab is the same among the workbooks, and 
#the fact that each river is on its own tab is consistent

#Loop through each workbook and stack the tab data with a new column for river name
rawData<-data.frame()

lstNames<-names(rList)

for(i in lstNames){
  #Test
  #i<-lstNames[6]
  ####
  wbNames<-names(rList[i][[1]])
  wb<-rList[i][[1]]
  
  for(j in wbNames){
    #Test
    #j<-wbNames[2]
    ####
    
    river<-j
    ds<-read.xlsx(wb,sheet = j, cols = 1:8) 
    
    if(ncol(ds)==6){
      ds<-ds %>% 
        select(1,6) %>% 
        mutate(AVAILABLE = NA_real_,RIVER=river)
      names(ds)<-c("DATE","TOTAL_APPLICATIONS","AVAILABLE","RIVER")
      
    }
    
    if(ncol(ds)==7){
      ds<-ds %>% 
        select(1,6,7) %>% 
        mutate(RIVER=river)
      names(ds)<-c("DATE","TOTAL_APPLICATIONS","AVAILABLE","RIVER")
      
    }
    
    if(ncol(ds)==8){
      ds<-ds %>% 
        select(1,6,8) %>% 
        mutate(RIVER=river)
      names(ds)<-c("DATE","TOTAL_APPLICATIONS","AVAILABLE","RIVER")
      
    }
    
    rawData<-bind_rows(rawData,ds)
  }#end for j
  
  
}#end for i

#Clean data frame
#Format date
data<-rawData %>% 
  mutate(
    DATE = as.numeric(DATE)
  ) %>% 
  filter(
    !is.na(DATE)#These were the totals rows at the bottom of each df
  ) %>% 
  mutate(
    DATE = as.Date(DATE,origin = '1899-12-30'),
    RIVER = case_when(
      
      RIVER %in% c("MF demand","MFdemand",'MiddleForkDemand','2017 Middle Fork',
                   '2018 Middle Fork','2019 Middle Fork','2020 Middle Fork',
                   '2021 Middle Fork','2021 Middle Fork Salmon') ~ "Middle Fork",
      RIVER %in% c('Salmon demand','SalmonDemand','2017 Wild Main Salmon','2018 Wild Main Salmon',
                   '2019 Wild Salmon','2020 Wild Salmon','2021 Wild Salmon') ~ "Main Salmon",
      RIVER %in% c('Selway demand','SelwayDemand','2017 Selway','2018 Selway','2019 Selway',
                   '2020 Selway','2021 Selway') ~ "Selway",
      RIVER %in% c('Snake demand','SnakeDemand','2017 Hells Canyon - Snake',
                   '2018 Hells Canyon - Snake','2019 Hells Canyon - Snake',
                   '2020 Hells Canyon - Snake','2021 Hells Canyon - Snake') ~ "Snake",
      TRUE ~ RIVER
      
    )
  ) %>% 
  mutate(
    MONTH_DAY = format(DATE,'%b-%d')
  )

unique(data$RIVER)#Looks good just 4 labels

#See if any NAs in the data column
table(is.na(data$TOTAL_APPLICATIONS))#Hmmm

xxx<-data %>% 
  filter(is.na(TOTAL_APPLICATIONS))#Seems fine lets kill them

data<-data %>% 
  filter(
    !is.na(TOTAL_APPLICATIONS)
  )

table(is.na(data$TOTAL_APPLICATIONS))#Cool


#Fill missing data with estimates-----------------

#Also, then fill in the missing values from 2011 with the average value in the rest of the data set
fill<-data %>% filter(is.na(AVAILABLE)) %>% #Records with missing data, remove column with missing data
  select(-AVAILABLE)

hold<-data %>% anti_join(fill) #Records that are good and used to calculate estimates for missing values

nFill<-data %>% anti_join(fill) %>% #Calculate mean values from extant data
  select(-c(DATE,TOTAL_APPLICATIONS)) %>% 
  group_by(RIVER,MONTH_DAY) %>% 
  summarize(
    COUNT = length(AVAILABLE),
    MIN = min(AVAILABLE),
    MAX = max(AVAILABLE),
    AVAILABLE = round(mean(AVAILABLE),0)
  )

table(nFill$MONTH_DAY)
filter(nFill,MONTH_DAY=="Jul-04")#Make sure we are getting 10 years about and 4 rivers

#Fill in missing
fill<-fill %>% #Join estimates to dataframe with missing values by river and date
  left_join(nFill, by = c("RIVER","MONTH_DAY")) %>% 
  select(names(hold))

#Stack back
data<-bind_rows(fill,hold) #Put 2 dataframes back together

#Preprocessing--------------------
#Calculate probability of win given 1:10 people and add to LABEL column

#Make lookup tables to convert river and weekday to numbers
rivCodes<-data.frame(
  RIVER = unique(data$RIVER),
  RIVER_N = c(1,2,3,4),
  stringsAsFactors = F
)

wdays<-data.frame(
  WDAY = unique(weekdays(data$DATE)),
  WDAY_N = c(1,2,3,4,5,6,7),
  stringsAsFactors = F
)

#Loop through cases of 1 to 10 people in our group applying for same day and stack the dfs

outputdf<-data.frame()

for(i in 1:10){
  #i<-1
  
  mlData<-data %>% 
    mutate(
      YEAR = lubridate::year(DATE),
      WEEK = lubridate::week(DATE),
      GROUP_N = as.double(i),
      MONTH_N = lubridate::month(DATE),
      DAY_N = as.double(lubridate::day(DATE)),
      WDAY = weekdays(DATE)
    ) %>% 
    left_join(rivCodes) %>% 
    left_join(wdays) %>% 
    mutate(
      LABEL = clp(.,"GROUP_N","TOTAL_APPLICATIONS","AVAILABLE")
    ) %>% 
    select(RIVER_N,DATE,YEAR,MONTH_N,WEEK,DAY_N,WDAY_N,AVAILABLE,TOTAL_APPLICATIONS,GROUP_N,LABEL)
  
  outputdf<-bind_rows(outputdf,mlData)
}

#Get rid of any Inf values in the LABEL
outputdf2<-outputdf %>% 
  filter(
    is.infinite(LABEL)
  )

outputdf<-outputdf %>% 
  anti_join(outputdf2)

#Review weird calculated values for probability at or above 1-------------
misCalcs<-outputdf %>% filter(LABEL >= 1)
head(misCalcs)
#These are records where the total applications are equal to those available, or
#the number of group applicants is greater than the total applications
#I am comfortable just removing these
outputdf<-outputdf %>% 
  anti_join(misCalcs)
hist(outputdf$LABEL)


#Write out data files csv to speed up model trials------------------
#Update when necessary

#Normal dataframe with year, month, day, weekday 
df_1<-outputdf %>% 
  select(c(1,3,4,6,7:11)) %>% 
  mkSets(.)

#Make table of normalized values by me-------------
df_2<-outputdf %>% 
  select(c(1,3,4,6,7:11)) %>% 
  mutate_at(c(-11),normalize) %>% 
  mkSets(.)

#Make normal set with year, week, weekday-----------------
df_3<-outputdf %>% 
  select(c(1,3,5,7:11)) %>% 
  mkSets(.)

#Make normal set without river, with year, month,weekday--------------------
df_4<-outputdf %>% 
  select(c(3,4,6,7:11)) %>% 
  mkSets(.)

#Make one per river with year, week, weekday----------------------
df_mf<-outputdf %>% 
  filter(RIVER_N==1) %>% 
  select(c(3,5,7:11)) %>% 
  mkSets(.)

df_ms<-outputdf %>% 
  filter(RIVER_N==2) %>% 
  select(c(3,5,7:11)) %>% 
  mkSets(.)

df_sy<-outputdf %>% 
  filter(RIVER_N==3) %>% 
  select(c(3,5,7:11)) %>% 
  mkSets(.)

df_sk<-outputdf %>% 
  filter(RIVER_N==4) %>% 
  select(c(3,5,7:11)) %>% 
  mkSets(.)

#Make normal data set with years 2015-2020 only and year, week, weekday----------

df_short<-outputdf %>% 
  filter(YEAR %in% c(2015,2016,2017,2018,2019,2020)) %>% 
  select(c(1,3,5,7:11)) %>% 
  mkSets(.)


#Make dataset where the total applications is to be predicted----------

df_pred_aps<-outputdf %>% 
  select(YEAR,WEEK,WDAY_N,TOTAL_APPLICATIONS) %>% 
  rename(LABEL=TOTAL_APPLICATIONS) %>% 
  mkSets(.)
















