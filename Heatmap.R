getwd()
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(plyr)
library(stats)
library(lattice)
library(lubridate)
library(tidyr)
detach(package:plyr)
#Set a working directory
getwd()
setwd("/Users/jimmymarcel/Documents/One Acre Fund/DATA_FILES/Key Stats")
#We remove farmers with 0 credit
#% overall repaid
sum(d21$TotalRepaid)/sum(d21$TotalCredit)
# delete columns I do not want to keep
colnames(d21)
d20 <- d20[, -c(41:106)]
colnames(v20)
v20 <- v20[, -c(17:20)]
#To put dates in standard format,
v21$RepaymentDate <- substr(v21$RepaymentDate, 1, 10)
v21$RepaymentDate <- gsub(" ", "", v21$RepaymentDate)
v21$RepaymentDate <- gsub("-", "/", v21$RepaymentDate)
v21$RepaymentDate <- as.Date(v21$RepaymentDate)
class(v21$RepaymentDate)

#Selecting payment transactions for Mobile money or receipt
v21 <- v21[rev(order(as.Date(v21$RepaymentDate))),] %>%
  subset(Type == "Receipt" | Type == "MobileMoney") %>%
  na.omit()
table(v21$Type)
#Adding unique ID for groups
d21$grp.id <- paste(d21$DistrictName, d21$SiteName, d21$GroupName, sep = "")
d21$site.id <- paste(d21$DistrictName, d21$SiteName, sep = "")

#Date as of the analysis
d <- as.Date("2020-09-28")
#Setting date limits

#Selecting weeks

oneweek <- d
twoweeks <- d-14
threeweeks <- d - 21
fourweeks <- d - 28
fiveweeks <- d - 35
sixweeks <- d - 42
sevenweeks <- d - 49
eightweeks <- d - 56
eightweeksplus <- eightweeks - 1
twelveweeksplus <- eightweeks - 29

#Selecting the most recent dates of payments by removing duplicates
v201 <- v21 %>% group_by(GlobalClientID) %>% slice(which.max(RepaymentDate)) 
summary(v201$RepaymentDate)
#Merging both transactions and D2
d20$GlobalClientID <- d20$GlobalClientId
dfD <- merge(d21, v201, by = c("GlobalClientID"), all.x = TRUE, ignore.case = TRUE )

#df <- subset(df, !duplicated(GlobalClientID))
#Adding number of weeks stagnant
dfD <- dfD %>% mutate(weeks = ifelse(RepaymentDate <= oneweek & RepaymentDate > twoweeks , "<= 1 week",
                                     ifelse(RepaymentDate < oneweek & RepaymentDate > threeweeks, "<2 weeks",
                                            ifelse(RepaymentDate < twoweeks & RepaymentDate > fourweeks, "<3 weeks",
                                                   ifelse(RepaymentDate < threeweeks & RepaymentDate > fiveweeks,"<4 weeks",
                                                          ifelse(RepaymentDate < fourweeks & RepaymentDate > sixweeks, "<5 weeks",
                                                                 ifelse(RepaymentDate < fiveweeks & RepaymentDate > sevenweeks, "<6 weeks",
                                                                        ifelse(RepaymentDate < sixweeks & RepaymentDate > eightweeks, "<7 weeks",
                                                                               ifelse(RepaymentDate < sevenweeks & RepaymentDate > eightweeksplus, "<8 weeks",
                                                                                      ifelse(RepaymentDate < eightweeks & RepaymentDate > twelveweeksplus, ">=8 weeks",
                                                                                             ifelse(RepaymentDate <= twelveweeksplus, ">=12 weeks", NA)))))))))))



te <- subset(dfD, is.na(weeks))
#Analysis of heatMap overall
AnalysT <- dfD %>%
  group_by(weeks) %>%
  summarise(
    p60 = length(unique(GlobalClientID[X..Repaid >= 60])),
    p47 = length(unique(GlobalClientID[X..Repaid >= 47 & X..Repaid < 60])),
    p42 = length(unique(GlobalClientID[X..Repaid >= 42 & X..Repaid < 47])),
    p40 = length(unique(GlobalClientID[X..Repaid >= 40 & X..Repaid < 42])),
    p35 =  length(unique(GlobalClientID[X..Repaid >= 35 & X..Repaid < 40 ])),
    P30 = length(unique(GlobalClientID[X..Repaid >= 30 & X..Repaid < 35])),
    p25 = length(unique(GlobalClientID[X..Repaid >= 25 & X..Repaid < 30])),
    p20 = length(unique(GlobalClientID[X..Repaid  >= 20 & X..Repaid < 25 ])),
    p15 = length(unique(GlobalClientID[X..Repaid >= 15 & X..Repaid < 20])),
    p10 = length(unique(GlobalClientID[X..Repaid >= 10 & X..Repaid < 15])),
    p10L = length(unique(GlobalClientID[X..Repaid < 10]))
  )
write.csv(AnalysT, "heatmapdata0928.csv")
#At regional level
Region <- dfD %>%
  group_by(RegionName,weeks) %>%
  summarise(
    p60 = length(unique(GlobalClientID[X..Repaid >= 60])),
    p47 = length(unique(GlobalClientID[X..Repaid >= 47 & X..Repaid < 60])),
    p42 = length(unique(GlobalClientID[X..Repaid >= 42 & X..Repaid < 47])),
    p40 = length(unique(GlobalClientID[X..Repaid >= 40 & X..Repaid < 42])),
    p35 =  length(unique(GlobalClientID[X..Repaid >= 35 & X..Repaid < 40 ])),
    P30 = length(unique(GlobalClientID[X..Repaid >= 30 & X..Repaid < 35])),
    p25 = length(unique(GlobalClientID[X..Repaid >= 25 & X..Repaid < 30])),
    p20 = length(unique(GlobalClientID[X..Repaid  >= 20 & X..Repaid < 25 ])),
    p15 = length(unique(GlobalClientID[X..Repaid >= 15 & X..Repaid < 20])),
    p10 = length(unique(GlobalClientID[X..Repaid >= 10 & X..Repaid < 15])),
    p10L = length(unique(GlobalClientID[X..Repaid < 10]))
  )

write_csv(Region, "heatmapregion.csv")
#Average credits
mean(d21$TotalCredit)

#Geting the heatmap for each district
#Data for each district
AnalysT2Distr <- df %>%
  group_by(DistrictName,weeks) %>%
  summarise(
    p100 = length(unique(GlobalClientID[X..Repaid >= 100])),
    p90.99 = length(unique(GlobalClientID[X..Repaid >= 90 & X..Repaid < 100])),
    p80.89 = length(unique(GlobalClientID[X..Repaid >= 80 & X..Repaid < 90])),
    p70.79 = length(unique(GlobalClientID[X..Repaid >= 70 & X..Repaid < 80])),
    p60.69 =  length(unique(GlobalClientID[X..Repaid >= 60 & X..Repaid < 70])),
    p50.59 = length(unique(GlobalClientID[X..Repaid >= 50 & X..Repaid < 60])),
    p40.49 = length(unique(GlobalClientID[X..Repaid >= 40 & X..Repaid < 50])),
    p30.39 = length(unique(GlobalClientID[X..Repaid  >= 30 & X..Repaid < 40])),
    p20.29 = length(unique(GlobalClientID[X..Repaid >= 20 & X..Repaid < 30])),
    p10.19 = length(unique(GlobalClientID[X..Repaid >= 10 & X..Repaid < 20])),
    p10L = length(unique(GlobalClientID[X..Repaid < 10]))
  )

write.csv(AnalysT2Distr, "heatmapDistrict0622.csv")
#heatmap by FO level
df$siteid <- paste(df$DistrictName, df$SiteName, sep = " ")
AnalysTFO <- df %>%
  group_by(siteid,weeks) %>%
  summarise(
    p100 = length(unique(GlobalClientID[X..Repaid >= 100])),
    p90.99 = length(unique(GlobalClientID[X..Repaid >= 90 & X..Repaid < 100])),
    p80.89 = length(unique(GlobalClientID[X..Repaid >= 80 & X..Repaid < 90])),
    p70.79 = length(unique(GlobalClientID[X..Repaid >= 70 & X..Repaid < 80])),
    p60.69 =  length(unique(GlobalClientID[X..Repaid >= 60 & X..Repaid < 70])),
    p50.59 = length(unique(GlobalClientID[X..Repaid >= 50 & X..Repaid < 60])),
    p40.49 = length(unique(GlobalClientID[X..Repaid >= 40 & X..Repaid < 50])),
    p30.39 = length(unique(GlobalClientID[X..Repaid  >= 30 & X..Repaid < 40])),
    p20.29 = length(unique(GlobalClientID[X..Repaid >= 20 & X..Repaid < 30])),
    p10.19 = length(unique(GlobalClientID[X..Repaid >= 10 & X..Repaid < 20])),
    p10L = length(unique(GlobalClientID[X..Repaid < 10]))
  )

write.csv(AnalysTFO, "HeatmapFO0622.csv")

#############################Enrollment#############
#Looking for the farmers who are enrolled
colnames(d21)
d21$sumpro <- rowSums( d21[,41:64] )
enrolled <- subset(d21, sumpro >=1)
df$enrolled <- match(d20$GlobalClientID, enrolled$GlobalClientID, nomatch = 0)
#looking for the numbers of how many enrolled
length(unique(df$GlobalClientID[df$enrolled >=1]))
colnames(df)
#Looking for each category farmers
datatrack <- df %>% mutate(status = ifelse(X..Repaid == 100, "finished",
                                         ifelse(X..Repaid >= 90 & X..Repaid < 100 & weeks <= "<8 weeks", "on track",
                                                ifelse(X..Repaid >= 90 & X..Repaid < 100 & weeks > "<8 weeks","off track",
                                                       ifelse(X..Repaid >= 70 & X..Repaid < 90 & weeks <= "<8 weeks","off track",
                                                              ifelse(X..Repaid >= 60 & X..Repaid < 70 & weeks <= "<4 weeks" , "off track", 
                                                                     ifelse(X..Repaid >= 50 & X..Repaid < 60 & weeks <= "<= 1 week", "off track",
                                                                            ifelse(X..Repaid >= 70 & X..Repaid < 90 & weeks > "<8 weeks", "of concern",
                                                                                   ifelse( X..Repaid >= 40 & X..Repaid < 70 & weeks >= "<4 weeks" & weeks <= "<8 weeks" , "of concern",
                                                                                           ifelse(X..Repaid >= 50 & X..Repaid < 60 & weeks >= "<2 weeks" & weeks <= "<4 weeks","of concern",
                                                                                                  ifelse(X..Repaid >= 20 & X..Repaid < 50 & weeks <= "<4 weeks", "of concern",
                                                                                                         ifelse(X..Repaid >= 10 & X..Repaid < 40 & weeks >= "<2 weeks" & weeks <= "<4 weeks", "most concerning",
                                                                                                                ifelse(X..Repaid >= 10 & X..Repaid < 30 & weeks <= "<= 1 week", "most concerning",
                                                                                                                       ifelse( X..Repaid >= 40 & X..Repaid < 50 & weeks >= "<4 weeks" & weeks <= "<5 weeks", "most concerning", "most concerning"))))))))))))))
                                                                                      
                                                                                     
                                                                                
                                                                              
                                                                            


         
#Getting the categories
ontrack <- subset(datatrack, status == "on track" & !is.na(weeks))
offtrack <- subset(datatrack, status == "off track" & !is.na(weeks))
of_concern <- subset(datatrack, status == "of concern" & !is.na(weeks))
mostconcern <- subset(datatrack, status == "most concerning" & !is.na(weeks))
finished <- subset(datatrack, status == "finished"& !is.na(weeks))

#average credit
mean(ontrack$TotalCredit)
mean(offtrack$TotalCredit)
mean(of_concern$TotalCredit)
mean(mostconcern$TotalCredit)
#average outstanding
mean(ontrack$RemainingCredit)
mean(offtrack$RemainingCredit)
mean(of_concern$RemainingCredit)
mean(mostconcern$RemainingCredit)
#Enrolled
length(unique(enrolled$GlobalClientID))
length(unique(finished$GlobalClientID[finished$enrolled >0]))
length(unique(ontrack$GlobalClientID[ontrack$enrolled >0]))
length(unique(offtrack$GlobalClientID[offtrack$enrolled >0]))
length(unique(of_concern$GlobalClientID[of_concern$enrolled >0]))
length(unique(mostconcern$GlobalClientID[mostconcern$enrolled >0]))

write.csv(offtrack, "off track.csv")  
write.csv(ontrack, "on Track.csv")
write.csv(concerning, "concerning.csv")
write.csv(mostconcern, "mostconcerning.csv")
write.csv(datatrack, "dataframe.csv")
offtrack1 <- read.csv("offtrack_list_phones.csv", header = TRUE)
phones <- subset(offtrack1, !is.na(phone.number))
nophones <- subset(offtrack1, is.na(phone.number))
###### Eligibility categories
datatrack <- dfD %>% mutate(status = ifelse(X..Repaid == 100, "finished",
                                           ifelse(X..Repaid >= 90 & X..Repaid < 100 & weeks <= "<8 weeks", "on track",
                                                  ifelse(X..Repaid >= 90 & X..Repaid < 100 & weeks > "<8 weeks","off track",
                                                         ifelse(X..Repaid >= 70 & X..Repaid < 90 & weeks <= "<8 weeks","off track",
                                                                ifelse(X..Repaid >= 60 & X..Repaid < 70 & weeks <= "<4 weeks" , "off track", 
                                                                       ifelse(X..Repaid >= 50 & X..Repaid < 60 & weeks <= "<= 1 week", "off track",
                                                                              ifelse(X..Repaid >= 70 & X..Repaid < 90 & weeks > "<8 weeks", "of concern",
                                                                                     ifelse( X..Repaid >= 40 & X..Repaid < 70 & weeks >= "<4 weeks" & weeks <= "<8 weeks" , "of concern",
                                                                                             ifelse(X..Repaid >= 50 & X..Repaid < 60 & weeks >= "<2 weeks" & weeks <= "<4 weeks","of concern",
                                                                                                    ifelse(X..Repaid >= 20 & X..Repaid < 50 & weeks <= "<4 weeks", "of concern",
                                                                                                           ifelse(X..Repaid >= 10 & X..Repaid < 40 & weeks >= "<2 weeks" & weeks <= "<4 weeks", "most concerning",
                                                                                                                  ifelse(X..Repaid >= 10 & X..Repaid < 30 & weeks <= "<= 1 week", "most concerning",
                                                                                                                         ifelse( X..Repaid >= 40 & X..Repaid < 50 & weeks >= "<4 weeks" & weeks <= "<5 weeks", "most concerning", "most concerning"))))))))))))))


########################## group togetherness ###########################################################
#Most concerning <<< how many individual groups have at least one farmers 
colnames(mostconcern)
mostgroup <- mostconcern %>%
  group_by(grp.id, DistrictName, SiteName, GroupName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    tot.fin = length(unique(GlobalClientID[X..Repaid == 100]))
  )

length(unique(mostgroup$grp.id[mostgroup$tot.cl >= 1]))

#Total groups
tgroup<- df %>%
  group_by(grp.id, DistrictName, SiteName, GroupName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID))
  )
length(tgroup$tot.cl)

#Question 2: How many groups have 1 or more client under 65%
under65 <- df %>%
  subset(X..Repaid <= 65) %>%
  group_by(grp.id, DistrictName, SiteName, GroupName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID))
  )


length(unique(under65$grp.id[mostgroup$tot.cl >= 1]))
#Question 4: How many individual groups contain at least 1 farmer who has finished
groufinish <- df %>%
  group_by(grp.id, DistrictName, SiteName, GroupName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    tot.fin = length(unique(GlobalClientID[X..Repaid == 100]))
  )
  
length(unique(groufinish$grp.id[groufinish$tot.fin >= 1]))

#Question 3: How many finishers are in a group with 1 or more farmers in the most concerning bucket
#Additional sub question we didn't discuss: 

finishers <- subset(df, X..Repaid == 100)

fingrp <- df %>%
  group_by(grp.id, DistrictName, SiteName, GroupName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    tot.fin = length(unique(GlobalClientID[RemainingCredit == 0]))
  )
test <- merge(fingrp, mostgroup,  by = c("grp.id"), all.x = TRUE, ignore.case = TRUE )
test <- subset(test, !is.na(DistrictName.y))
sum(test$tot.cl.x)
write.csv(test, "finishersinmostconcer.csv")
############################# yaye data of Map Phase 3 updated ##########

#Attaching phone numbers
colnames(d22)
d22 <- d22[,c(38,30)]
df$Phone.number <- d22$ClientPhone[match(df$GlobalClientID, d22$GlobalClientID)]
head(d20$Phone.number)
#Selecting the columns to keep
colnames(df)
df <- df[,c(1,4,7,10,11,12,22,23,59,58,60)]
#Grouping based on the chriteria
yofftrack <- df %>%
  subset(X..Repaid >= 80 & X..Repaid < 90 & weeks > "<5 weeks" & !is.na(weeks)) 
yofftrackenrolled <- subset(yofftrack, enrolled > 0)
yofftracknotenrolled <- subset(yofftrack, enrolled == 0)
yontrack <- df %>%
  subset(X..Repaid >= 90 & X..Repaid < 100 & !is.na(weeks) )
yontrackless1000 <- subset(yontrack,RemainingCredit <= 1000)
yontrackabove1000 <- subset(yontrack,RemainingCredit > 1000)
yontrackless1000enrolled <- subset(yontrackless1000, enrolled > 0)
yontrackless1000notenrolled <- subset(yontrackless1000, enrolled == 0)
yontrackabove1000enrolled <- subset(yontrackabove1000, enrolled > 0)
yontrackabove1000noenrolled <- subset(yontrackabove1000, enrolled == 0)

yofconcern <- df %>%
  subset(X..Repaid >= 60 & X..Repaid < 80 & weeks >= "<6 weeks" & !is.na(weeks))
yofconcernenrolled <- subset(yofconcern, enrolled>0 )
yofconcernnotenrolled <- subset(yofconcern, enrolled == 0)

ymostconcerning <- df %>%
  subset(X..Repaid >= 30 & X..Repaid < 60 & weeks >= "<6 weeks"& !is.na(weeks))
ymostconcerningenrolled <- subset(ymostconcerning, enrolled > 0)
ymostconcerningnotenrolled <- subset(ymostconcerning, enrolled == 0)

ymostworse <- df %>%
  subset(X..Repaid < 30 & weeks >= "<6 weeks" & !is.na(weeks))
ymostworseenrolled <- subset(ymostworse, enrolled > 0)
ymostworsenotenrolled <- subset(ymostworse, enrolled == 0)

yontrackabovesfl <- df %>%
  subset(X..Repaid >= 60 & X..Repaid < 90 & weeks <= "<5 weeks" & !is.na(weeks) )
yontrackabovesflenrolled <- subset(yontrackabovesfl, enrolled >0)
yontrackabovesflnotenrolled <- subset(yontrackabovesfl, enrolled == 0)

yofconcernbelowsfl <- df %>%
  subset(X..Repaid >= 30 & X..Repaid < 60 & weeks > "<5 weeks" )
yofconcernbelowsflenrolled <- subset(yofconcernbelowsfl, enrolled > 0)
yofconcernbelowsflnotenrolled <- subset(yofconcernbelowsfl, enrolled == 0)


ymostworsbelowsfl <- df %>%
  subset(X..Repaid < 30  & weeks <= "<5 weeks" )
ymostworsbelowsflenrolled <- subset(ymostworsbelowsfl, enrolled >0)
ymostworsbelowsflnotenrolled <- subset(ymostworsbelowsfl, enrolled == 0)


###subgroups###
write.csv(yofftrackenrolled,"offtrack_enrolled.csv")
write.csv(yofftracknotenrolled,"offtrack_notenrolled.csv")
#write.csv(yontrackenrolled,"ontrack_enrolled.csv")
write.csv(yontrackabove1000enrolled, "ontrackabove1000_enrolled.csv")
write.csv(yontrackabove1000noenrolled, "ontrackabove1000_notenrolled.csv")
write.csv(yontrackless1000enrolled, "ontrackless1000_enrolled.csv")
write.csv(yontrackless1000notenrolled, "ontrackless1000_notenrolled.csv")
#write.csv(yontracknotenrolled, "ontrack_notenrolled.csv")
write.csv(yofconcernenrolled, "ofconcern_enrolled.csv")
write.csv(yofconcernnotenrolled, "ofconcern_notenrolled.csv")
write.csv(ymostconcerningenrolled, "mostconcern_enrolled.csv")
write.csv(ymostconcerningnotenrolled, "mostconcern_notenrolled.csv")
write.csv(ymostworseenrolled, "mostconcernworst_enrolled.csv")
write.csv(ymostworsenotenrolled, "mostconcernworst_notenrolled.csv")
write.csv(yontrackabovesflenrolled,"ontrackMOenrolled.csv")
write.csv(yontrackabovesflnotenrolled, "ontracMOnotenrolled.csv")
write.csv(yofconcernbelowsflenrolled,"ofconcernMO_enrolled.csv")
write.csv(yofconcernbelowsflnotenrolled,"ofconcernMO_notenrolled.csv")
write.csv(ymostworsbelowsflenrolled, "mostconcerningMO_enrolled.csv")
write.csv(ymostworsbelowsflnotenrolled, "mostconcerningMO_notenrolled.csv")




