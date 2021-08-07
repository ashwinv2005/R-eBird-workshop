library(lubridate)
library(tidyverse)

## read data using 

data = read.delim("ebd_IN_202105_202105_relMay-2021.txt", sep = "\t", 
                  header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

names(data)
head(data)

data$SAMPLING.EVENT.IDENTIFIER[1:50]
data$GROUP.IDENTIFIER[1:50]


# How to filter data by any column? Let's say I want to filter data from Karnataka

data1 = data %>% filter(STATE == "Karnataka")
head(data1)

data1 = data %>% filter(STATE %in% c("Karnataka","Maharashtra"))
head(data1)

data1 = data %>% filter(STATE != "Karnataka")
head(data1)

data1 = data %>% filter(!STATE %in% c("Karnataka","Maharashtra"))
head(data1)

# How to filter by complete checklists

data1 = data %>% filter(ALL.SPECIES.REPORTED == 1)
head(data)

table(data$ALL.SPECIES.REPORTED)
table(data1$ALL.SPECIES.REPORTED)


# How to only select certain columns in the dataframe (data)?

data1 = data %>% select(COMMON.NAME,STATE,COUNTY,OBSERVATION.COUNT)
head(data1)

data1 = data %>% filter(STATE == "Karnataka") %>% select(COMMON.NAME,STATE,COUNTY,OBSERVATION.COUNT)
head(data1)



# How to add a new column? Say calculate "Duration" in hours rather than minutes

data1 = data %>% mutate(DURATION.HOURS = DURATION.MINUTES/60)
head(data1)



# how to refer to any single column?

checklistIDs = data$SAMPLING.EVENT.IDENTIFIER
checklistIDs[1]
checklistIDs[5]
checklistIDs[1:5]

# data frames however have 2 dimensions

data[1,]
data[,1]
data[1,1]


############# total number of observations

totobs = length(data$COMMON.NAME) ## any column is ok

############# total number of checklists

totlists = length(unique(data$SAMPLING.EVENT.IDENTIFIER))

############# total number of birdwatchers

totbir = length(unique(data$OBSERVER.ID))





## Only select columns of interest

names(data)

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







############# total number of species

## remove spuhs and slashes
datas = data %>% filter(CATEGORY %in% c("species","issf"))
totspecs = length(unique(datas$COMMON.NAME))

# create and write a file with common names and scientific names of all species
# useful for mapping
temp = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

write.csv(temp,"allspecies.csv", row.names=FALSE)

############# number of checklists with media

media = data %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(any(HAS.MEDIA == 1)) %>%
  ungroup
media = length(unique(media$SAMPLING.EVENT.IDENTIFIER))




############# number of checklists per observer

lists_obs = data %>%
  group_by(OBSERVER.ID) %>% summarize(count = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  ungroup

lists_obs = lists_obs %>%
  arrange(desc(count))



############# number of checklists per state

lists_state = data %>%
  group_by(STATE) %>% summarize(count = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  ungroup %>%
  arrange(desc(count))

# for unique checklists

ulists_state = data %>%
  group_by(STATE) %>% summarize(count = n_distinct(group.id)) %>%
  ungroup %>%
  arrange(desc(count))



############# number of checklists in each district per state

lists_dist_state = data %>%
  group_by(STATE,COUNTY) %>% summarize(count = n_distinct(group.id)) %>%
  ungroup %>%
  arrange(desc(count))




############# common species based on frequency calculations (most important!)

common_species = data %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>% ungroup %>%
  mutate(lists = n_distinct(group.id)) %>%
  #group_by(STATE) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  #group_by(STATE,COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  group_by(COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  arrange(desc(freq))

write.csv(common_species,"commonspecies_may2021.csv", row.names=FALSE)


############# make simple plots

## number of birders in a state vs. the number of checklists in the state

# determine the number of birders per state

b_state = data %>%
  group_by(STATE) %>% summarize(birders = n_distinct(OBSERVER.ID))

# determine the number of checklists per state

c_state = data %>%
  group_by(STATE) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

# combine the two

bc_state = left_join(b_state,c_state)

# plot one against the other

plot(bc_state$birders,bc_state$lists)
text(bc_state$birders,bc_state$lists,bc_state$STATE,
     cex=0.65, pos=3,col="red") 
fit = lm(bc_state$lists~bc_state$birders)
abline(fit)

# how about unique lists?

# determine the number of unique checklists per state

u_state = data %>%
  group_by(STATE) %>% summarize(ulists = n_distinct(group.id))

bcu_state = left_join(bc_state,u_state)

plot(bcu_state$birders,bcu_state$ulists)

# now how to add axes?

plot(bcu_state$birders,bcu_state$ulists,xlab = "Number of birders",ylab = "Number of unique checklists")
text(bcu_state$birders,bcu_state$ulists,bcu_state$STATE,
     cex=0.65, pos=3,col="red") 
fit = lm(bc_state$lists~bc_state$birders)
abline(fit)
summary(fit)


# plot a bar chart - no of checklists per day during the month

# first, create a data frame with day and SAMPLING.EVENT.IDENTIFIER

data1 = data %>%
  distinct(daym,SAMPLING.EVENT.IDENTIFIER)
head(data1)

# two ways to do this

# 1) using a histogram - check the number of times each day is repeated

hist(data1$daym)
hist(data1$daym, breaks = 0:31)

# 2) use a barchart

data1 = data %>%
  group_by(daym) %>% summarize(count = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  ungroup
head(data1,10)
barplot(data1$count,data1$daym,width = 2,space = NULL)



############# ASSIGNMENT 1 - Which state had the highest number of species reported in May 2021?


############# ASSIGNMENT 2 - Which was the most commonly reported species in 
############# YOUR HOME STATE in in May 2021?

############# ASSIGNMENT 3 - Which was the most commonly reported species in 
############# YOUR HOME DISTRICT (county) in in May 2021?


############# ASSIGNMENT 4 - Download data for the entire year of 2020 in your state 
############# Plot a graph of the number of total checklists and the number of unique checklists
############# during every month (histogram or barchart)


############# ASSIGNMENT 5 - Do the top 5 most common species in your state change 
############# during the different months of the year?


## In subsequent sessions, you will learn how to plot observations on an India map and 
## plot complex graphs