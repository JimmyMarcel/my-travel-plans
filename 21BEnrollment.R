getwd()
library(data.table)
library(ggplot2)
library(reshape2)
#library(plyr)
library(stats)
library(lattice)
library(lubridate)
library(tidyr)
library(dplyr)
library(tidyverse)


#### Date Limits 23 nov to 30th
#goal 332 farmers per FO per Week 

#Load Lobraries
d23 <- read.csv("Season Clients Detailed_20201123-073851.csv", header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "")
d30 <- read.csv("SC_2021_11_30.csv", header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "")
d18 <- read.csv("SC_2021_2020.11.16 (2).csv", header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "")


# We add site and grp id
d23$site.id <- paste(d23$DistrictName, d23$SiteName, sep = "")
d30$site.id <- paste(d30$DistrictName, d30$SiteName, sep = "")
d18$site.id <- paste(d18$DistrictName, d18$SiteName, sep = "")
d21$site.id <- paste(d21$DistrictName, d21$SiteName, sep = "")
#Adding group Id
d23$grp.id <- paste(d23$DistrictName, d23$SiteName, d23$GroupName, sep = "")
d30$grp.id <- paste(d30$DistrictName, d30$SiteName, d30$GroupName, sep = "")
d18$grp.id <- paste(d18$DistrictName, d18$SiteName, d18$GroupName, sep = "")
d21$grp.id <- paste(d21$DistrictName, d21$SiteName, d21$GroupName, sep = "")
# In order to get the clients enrolled this week we 
#deduct total clients enrolled from clients enrolled last week
# We remove season A
d23 <- d23 %>% select(-starts_with("X2021A_"))
d30 <- d30 %>% select(-starts_with("X2021A_"))
d18 <- d18 %>% select(-starts_with("X2021A_"))
d21 <- d21 %>% select(-starts_with("X2021A_"))
#We find sum of all the products
colnames(d21)
d23$sumpro <- rowSums(d23[,46:64])
d30$sumpro <- rowSums(d30[,46:64])
d18$sumpro <- rowSums(d18[,46:64])
d21$sumpro <- rowSums(d21[,46:64])
# Find the enrolled clients per Site last week

    ########## FO ###########
#Week 1
Week18 <- d18 %>% 
  group_by(site.id, RegionName, DistrictName, SiteName) %>%
  summarise(
    Week18 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
#Week2
Week23 <- d23 %>% 
  group_by(site.id, RegionName, DistrictName, SiteName) %>%
  summarise(
    Week23 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
# this week
Week30 <- d30 %>% 
  group_by(site.id, RegionName, DistrictName, SiteName) %>%
  summarise(
    Week30 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
# We combine two dataframes by bringing number of clients enrolled last week 
Week30$Week23 <- Week23$Week23[match(Week30$site.id, Week23$site.id)]
Week30$Week16 <- Week18$Week18[match(Week30$site.id, Week18$site.id)]
# We see the difference to see those who enrolled this week
# Top Sites
getwd()
TopFO <-   Week30 %>% top_n(25,EnroThisweek)
write.csv(Week30, "TopFO.csv")

########## FM ###########
#Week 1
Week18 <- d18 %>% 
  group_by(FieldManagerPayrollID, FieldManager, RegionName, DistrictName) %>%
  summarise(
    Week18 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
#Week 2
Week23 <- d23 %>% 
  group_by(FieldManagerPayrollID, FieldManager, RegionName, DistrictName) %>%
  summarise(
    Week23 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
# this week
Week30 <- d30 %>% 
  group_by(FieldManagerPayrollID, FieldManager, RegionName, DistrictName) %>%
  summarise(
    Week30 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
# We combine two dataframes by bringing number of clients enrolled last week 
Week30$Week23 <- Week23$Week23[match(Week30$FieldManagerPayrollID, Week23$FieldManagerPayrollID)]
Week30$Week16 <- Week18$Week18[match(Week30$FieldManagerPayrollID, Week18$FieldManagerPayrollID)]

# Top Sites
write.csv(Week30, "TopFM.csv")

 ############### District ####################
#Week 1
Week18 <- d18 %>% 
  group_by(RegionName, DistrictName) %>%
  summarise(
    Week18 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
#Week2
Week23 <- d23 %>% 
  group_by(RegionName, DistrictName) %>%
  summarise(
    Week23 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
# this week
Week30 <- d30 %>% 
  group_by(RegionName, DistrictName) %>%
  summarise(
    Week30 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
# We combine two dataframes by bringing number of clients enrolled last week 
Week30$Week23 <- Week23$Week23[match(Week30$DistrictName, Week23$DistrictName)]
Week30$Week16 <- Week18$Week18[match(Week30$DistrictName, Week18$DistrictName)]
# We see the difference to see those who enrolled this week
# Top Sites
getwd()
TopFO <-   Week30 %>% top_n(25,EnroThisweek)
write.csv(Week30, "TopFD.csv")

########## Region  ###########
#Week 1
Week18 <- d18 %>% 
  group_by(RegionName) %>%
  summarise(
    Week18 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
#Week2
Week23 <- d23 %>% 
  group_by(RegionName) %>%
  summarise(
    Week23 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
# this week
Week30 <- d30 %>% 
  group_by(RegionName) %>%
  summarise(
    Week30 = length(unique(GlobalClientID[sumpro >= 1])),
    TotalNewclients = length(unique(GlobalClientID[NewMember == "True"]))
  )
# We combine two dataframes by bringing number of clients enrolled last week 
Week30$Week23 <- Week23$Week23[match(Week30$RegionName, Week23$RegionName)]
Week30$Week16 <- Week18$Week18[match(Week30$RegionName, Week18$RegionName)]
# We see the difference to see those who enrolled this week
# Top Sites
getwd()
TopFO <-   Week30 %>% top_n(25,EnroThisweek)
write.csv(Week30, "TopRegion.csv")
