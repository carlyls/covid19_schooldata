## Comparing various data sources about school reopening policies - UPDATED DATA

library(dplyr)
library(tidyr)
library(ecodist)
library(data.table)
library(ggplot2)
library(irr)
library(maps)
library(stringr)
library(glmnet)
library(lme4)
library(ggpubr)
library(wCorr)

## DATA ##

# Facebook
fb_state <- readRDS("FacebookMonthlyState.RDS")
fb_county <- readRDS("FacebookMonthlyCounty.RDS")

# Burbio
burbio_state <- readRDS("BurbioMonthlyState.RDS")
burbio_county <- readRDS("BurbioMonthlyCounty.RDS")
burbio_district <- readRDS("BurbioJanuaryDistrict.RDS")

# MCH
mch <- read.csv("MCH 1-31.csv") %>%
  filter(Control=="Public") %>%
  select(DistrictNCES,TeachingMethod)

# Phones
phone_state <- read.csv("schools_state_csv.csv") %>%
  mutate(month.year = paste(month,year,sep="-")) %>%
  filter(month.year %in% c("12-2020","1-2021","2-2021","3-2021","4-2021","5-2021")) %>%
  mutate(AnyInPerson.Phone = (1-share_all_closed_50)*100,
         AnyInPerson.Phone.75Percent = (1-share_all_closed_75)*100) %>%
  select(state_abbrev,month,AnyInPerson.Phone,AnyInPerson.Phone.75Percent) %>%
  rename(State = state_abbrev) %>%
  na.omit()
#mean(phone_state$AnyInPerson.Phone)
  
phone_county <- read.csv("schools_county_csv.csv") %>% 
  mutate(month.year = paste(month,year,sep="-")) %>%
  filter(month.year %in% c("12-2020","1-2021","2-2021","3-2021","4-2021","5-2021")) %>%
  mutate(AnyInPerson.Phone = (1-share_all_closed_50)*100,
         AnyInPerson.Phone.75Percent = (1-share_all_closed_75)*100) %>%
  select(countyfips3,month,AnyInPerson.Phone,AnyInPerson.Phone.75Percent) %>%
  rename(fips = countyfips3) %>%
  na.omit()

phone_district <- read.csv("schools_district_csv.csv") %>%
  filter(month == "1" & year == "2021") %>%
  mutate(AnyInPerson.Phone = (1-share_all_closed_50)*100,
         AnyInPerson.Phone.75Percent = (1-share_all_closed_75)*100) %>%
  rename(NoInPerson.Phone = share_all_closed_50) %>%
  select(leaid,AnyInPerson.Phone,NoInPerson.Phone,AnyInPerson.Phone.75Percent) %>%
  na.omit()

phone_district_weight <- read.csv("schools_district_csv.csv") %>%
  mutate(month.year = paste(month,year,sep="-")) %>%
  filter(month.year %in% c("12-2020","1-2021","2-2021","3-2021","4-2021","5-2021")) %>%
  mutate(AnyInPerson.Phone = (1-share_all_closed_50)*100,
         AnyInPerson.Phone.75Percent = (1-share_all_closed_75)*100) %>%
  rename(NoInPerson.Phone = share_all_closed_50) %>%
  select(leaid,month,AnyInPerson.Phone,AnyInPerson.Phone.75Percent,total_students,state_abbrev,state_name)

phone_district_weight$total_students <- ifelse(is.na(phone_district_weight$total_students)==T,0,phone_district_weight$total_students)
phone_district_weight %>%
  group_by(leaid) %>%
  summarise(AnyInPerson.Phone = weighted.mean(AnyInPerson.Phone,total_students,na.rm=T),
            total_students = mean(total_students,na.rm=T)) %>%
  summarise(mean = weighted.mean(AnyInPerson.Phone,total_students,na.rm=T))
  
state_pops <- read.csv("State Populations.csv") %>%
  rename(state_name = State) %>%
  select(state_name,Pop)
state_pops$state_name <- toupper(state_pops$state_name)
phone_state_weight <- phone_district_weight %>%
  rename(State = state_abbrev) %>%
  group_by(State,state_name,month) %>%
  summarise(AnyInPerson.Phone.weight = weighted.mean(AnyInPerson.Phone,total_students,na.rm=T),
            AnyInPerson.Phone.75Percent.weight = weighted.mean(AnyInPerson.Phone.75Percent,total_students,na.rm=T)) %>%
  left_join(state_pops,by="state_name")

#overall average
state_avg <- phone_state_weight %>%
  group_by(State) %>%
  summarise(AnyInPerson.Phone.weight = mean(AnyInPerson.Phone.weight,na.rm=T),
            Pop = mean(Pop,na.rm=T))
weighted.mean(state_avg$AnyInPerson.Phone.weight,state_avg$Pop,na.rm=T)
  
  


## STATE DATA ##
all_state <- burbio_state %>%
  left_join(fb_state,by=c("State","month")) %>%
  left_join(phone_state,by=c("State","month")) %>%
  left_join(phone_state_weight,by=c("State","month"))

saveRDS(all_state,"AllMonthlyState.RDS")


## COUNTY DATA ##
burbio_county$fips <- factor(burbio_county$fips)
fb_county$fips <- factor(fb_county$fips)
phone_county$fips <- factor(phone_county$fips)
all_county <- burbio_county %>%
  left_join(fb_county,by=c("fips","month")) %>%
  left_join(phone_county,by=c("fips","month"))

saveRDS(all_county,"AllMonthlyCounty.RDS")

burbio_counties <- read.csv("schoolOpeningTracker - 2-12-21.csv")
burbio_topcounties <- burbio_counties[order(-burbio_counties$County.Pop),]
burbio_topcounties <- burbio_topcounties[1:460,]
all_top_county <- all_county %>% filter(fips %in% burbio_topcounties$FIPS)

saveRDS(all_top_county,"AllMonthlyTopCounty.RDS")


## DISTRICT DATA ##
burbio_district$ID <- as.character(burbio_district$ID)
mch$DistrictNCES <- as.character(mch$DistrictNCES)
phone_district$leaid <- as.character(phone_district$leaid)
all_district <- burbio_district %>%
  left_join(mch,by=c("ID" = "DistrictNCES")) %>%
  left_join(phone_district,by=c("ID"="leaid"))
saveRDS(all_district,"AllJanuaryDistrict.RDS")


## COUNTY FACTORS ##
county_factors <- readRDS("county_factors.RDS")
county_factors$fips <- str_remove(county_factors$fips,"^0+")

unemp <- read.csv("UnempRates.csv",colClasses=c(County.Code="character"))
unemp$Unemploy.Rate <- as.numeric(unemp$Unemploy.Rate)
unemp <- unemp %>%
  mutate(fips = paste(as.character(State.Code),County.Code,sep="")) %>%
  filter(Period %in% c("Dec-20","Jan-21","Feb-21","Mar-21","Apr-21","May-21 p")) %>%
  mutate(month = ifelse(Period=="Dec-20",12,
                        ifelse(Period=="Jan-21",1,
                               ifelse(Period=="Feb-21",2,
                                      ifelse(Period=="Mar-21",3,
                                             ifelse(Period=="Apr-21",4,5)))))) %>%
  select(fips,month,Unemploy.Rate)
saveRDS(unemp,"MonthlyUnemployment.RDS")

