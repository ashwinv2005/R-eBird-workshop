# Assignments 4 and 5

library(lubridate)
library(tidyverse)

## read data using 

# ENSURE THAT YOU USE READ.DELIM WITH ALL THE PARAMETERS SPECIFIED!

data = read.delim("ebd_IN_202001_202012_relMay-2021.txt", sep = "\t", 
                  header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","LAST.EDITED.DATE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","HAS.MEDIA","BREEDING.CODE",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("ebd_IN_202105_202105_relMay-2021.txt", nrows = 1, sep = "\t", header = T, 
                 quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

data = read.delim("ebd_IN_202105_202105_relMay-2021.txt", colClasses = nms, sep = "\t", 
                  header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

# make the date column meaningful

days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

data = data %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         year = year(OBSERVATION.DATE),
         month = month(OBSERVATION.DATE),
         daym = day(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month])


save(data, file = "data2020.RData")



############# ASSIGNMENT 4 - Download data for the entire year of 2020 in your state 
############# Plot a graph of the number of total checklists and the number of unique checklists
############# during every month (histogram or barchart)

library(tidyverse)
load("data2020.RData") ### state names are capitalized in my specific case only, not for you

data1 = data %>%
  filter(STATE == "KARNATAKA")


# using a histogram - check the number of times each day is repeated

data2 = data1 %>%
  distinct(month,SAMPLING.EVENT.IDENTIFIER)
data3 = data1 %>%
  distinct(month,group.id)

hist(data2$month)
hist(data2$month, breaks = 0:12)

hist(data3$month)
hist(data3$month, breaks = 0:12)

# using a barchart

data4 = data1 %>%
  group_by(month) %>% summarize(count = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  ungroup
barplot(data4$count,data4$month,width = 2,space = NULL)

data5 = data1 %>%
  group_by(month) %>% summarize(count = n_distinct(group.id)) %>%
  ungroup
barplot(data5$count,data5$month,width = 2,space = NULL)



############# ASSIGNMENT 5 - Do the top 5 most common species in your state change 
############# during the different months of the year?


data6 = data1 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(month) %>% mutate(lists = n()) %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(month,COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  group_by(month) %>% arrange(desc(freq)) %>% slice(1:5)


