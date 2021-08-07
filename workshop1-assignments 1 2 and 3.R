# Assignments 1,2 and 3

library(lubridate)
library(tidyverse)

## read data using 

# ENSURE THAT YOU USE READ.DELIM WITH ALL THE PARAMETERS SPECIFIED!

data = read.delim("ebd_IN_202105_202105_relMay-2021.txt", sep = "\t", 
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



# ASSIGNMENT 1 - Which state had the highest number of species reported in May 2021?

data1 = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(STATE) %>% summarize(count = n_distinct(COMMON.NAME)) %>%
  arrange(desc(count))



############# ASSIGNMENT 2 - Which was the most commonly reported species in 
############# YOUR HOME STATE in in May 2021?

data2 = data %>%
  filter(STATE == "Karnataka") %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  mutate(lists = n()) %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  arrange(desc(freq))

## WHY IS THERE A DISCREPENCY BETWEEN NO. OF SPECIES FROM KARNATAKA
## IN ASSIGNMENT 1 AND THE NO. OF ROWS IN ASSIGNMENT 2? (290 vs. 287)



############# ASSIGNMENT 3 - Which was the most commonly reported species in 
############# YOUR HOME DISTRICT (county) in in May 2021?

# list of counties in my state with number of checklists

districts = data %>% filter(STATE == "Karnataka") %>% 
  group_by(COUNTY) %>% summarize(count = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(count))

data.frame(districts)

data3 = data %>%
  filter(STATE == "Karnataka", COUNTY == "Bangalore") %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  mutate(lists = n()) %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  arrange(desc(freq))

