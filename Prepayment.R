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
#Loading data
#Set working directory
setwd("~/Documents/One Acre Fund/DATA_FILES/Key Stats")
d22 <- read.csv("SC_2021_12_02.csv", header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "")

#Adding site Id
d22$site.id <- paste(d22$DistrictName, d22$SiteName, sep = "")
#Adding group Id
d22$grp.id <- paste(d22$DistrictName, d22$SiteName, d22$GroupName, sep = "")
#allGL <- subset(d21, d21$Facilitator == "True")
######## Prepayment Tracker ########
#group_by products 
# totl of ag.pro= Maize + Fertilizers 
#solar= solar products + 
#Chicken == RF 500 per chick
#Trave = travertine
#Rest >> other products 
     #Strong start <<< normal payments plus 5000 RF
     #If your credit is less than 5000 you have to finish your loan

#Calculating total credit
d22$tot.creditAd <-  d22$X2021A_Maize.kg*950 
d22$TotalCredit <- d22$TotalCredit + d22$tot.creditAd
#new percentage repaix new
d22$X..Repaid2 <- round(100*(d22$TotalRepaid_IncludingOverpayments/d22$TotalCredit),1)
#changing the % repaid TO 100
d22$X..Repaid2 <- ifelse(d22$X..Repaid2 > 100, 100, d22$X..Repaid2)

#Removing trials
colnames(d22)
d22 <- d22 %>% select(-ends_with("Trial.qty"))
d22 <- d22 %>% select(-starts_with("X2021B_B"))
#Sum of all products
colna
d22$sumpro <- rowSums(d22[,48:76])
#Enrolled clients
d23 <- subset(d22, sumpro > 0)
d23 <-  subset(d23, TotalCredit > 0)
length(unique(d23$GlobalClientID))

#Remove those without products
length(unique(October$GlobalClientID[dOc$sumpro == 0]))
October <- subset(October, TotalCredit > 0)
October <- subset(October, sumpro >0)
#Total credit and % Repaid
sum(d23$TotalCredit)/sum(d22$TotalRepaid)
################## #Total groups
Sites <- d23 %>% 
  group_by(site.id,SiteName, DistrictName) %>%
  summarise(
    total  = length(unique(GlobalClientID))
  )
################ Credit per region



#Summing the products based on the group
df <- d23 %>% 
  mutate(AG = X2021A_DAP.kg + 
           #X2021A_Maize.kg + 
           X2021A_NPK.17.kg + 
           X2021A_Pan.53.kg + 
           X2021A_Pan.691.kg + 
           X2021A_SC.403.kg + 
           X2021A_SC.637.kg + 
           X2021A_UREA.kg + 
           X2021A_WH.403.kg + 
           X2021A_WH.505.kg +
           X2021A_WH.605.kg,
         Veg = X2021A_TUN.qty + 
           X2021A_TOM.qty + 
           X2021A_KAR.qty + 
           X2021A_SHU.qty + X2021A_POV.qty,
         Pics = X2021A_PICS100KG.qty + X2021A_PICS50KG.qty,
         Pico = X2021A_GLP.Pico.qty,
         Chic = X2021A_Inkoko.Confirmation.qty + 
           X2021A_Inkoko_Oct.qty + 
           X2021A_Inkoko_Sept.qty,
         Avoc = X2021A_Avoka.qty,
         Phn = X2021A_Tecno.qty,
         Solar = X2021A_Biolite.SHS.qty + 
           X2021A_GLP.SKP.200.qty + 
           X2021A_DLight.S200.qty + 
           X2021A_Niwa.300XL.qty,
         Trav = X2021A_Travertine.kg)

#subset total credit more than 3000
df1 <- subset(df, TotalCredit >= 3000)
#subset total credit less than 3000
low <- subset(df, TotalCredit < 3000)
#Calculating prepayment conditions without avocado+10 and chicken 
df2 <- df1 %>% mutate(prepayment = if_else (AG >= 100 & Solar >= 1 , "12000",
                                            if_else(AG < 100 & AG >0 & Solar >= 1, "8000", 
                                                    if_else(AG >= 100 & Solar ==0 , "7000", 
                                                            if_else(AG==0 & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 25 & Avoc ==0 & Veg==0 & Pics==0, "2500",
                                                                    if_else(Avoc < 10 & Avoc > 0 & (AG == 0  & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 0 & Veg==0) , "1000",
                                                                            if_else(Avoc >= 10 & (AG == 0  & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 0 & Veg==0), "500",
                                                                                    if_else(Avoc == 0 & AG == 0  & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 0 & Veg==0 & Chic >= 1, "0",
                                                                                            if_else(AG == 0 & Solar >=1 | Pico >2, "5000",
                                                                                                    if_else(AG < 100 & Solar == 0, "3000", "NA"))))))))))
 


#Calculating prepayment conditions with avocado+10 
#Changing the format on prepayment to numeric
df2$prepayment <- as.numeric(df2$prepayment)
# With Avocat +10
df2$prepaymentAV <- ifelse(df2$Avoc >= 10 , df2$prepayment +1000 ,df2$prepayment)
#With Chicken
df2$prepaymentFi <- ifelse(df2$Chic >=1, (df2$prepaymentAV+(df2$Chic*500)) ,df2$prepaymentAV)
#Re_adding the clients with less than 3000 credit
#### Working with Qualifications####
 #Conditions: Non Starters: Clients who have yet to make any payment towards 21A
 #Starters: Clients who have made a payment, but not yet reached their qualification amount
 #Qualifiers (Minimum): Clients who have met the minimum prepayment amount
 #Qualifiers (Full):Clients who have met their prepayment amount in full
 #Strong Starters: Clients who have paid RF 5000 more than their prepayment amount.

#Adding categories for clients above 3000 credit 
df3 <- df2 %>% mutate(Qual=
                        if_else(TotalRepaid_IncludingOverpayments <100, "Non.starter",
                                if_else(prepaymentFi >= 3000 & (TotalRepaid_IncludingOverpayments >= 100 & TotalRepaid_IncludingOverpayments < 3000 & X..Repaid < 100), "Starters",
                                        if_else(TotalRepaid_IncludingOverpayments >= 3000 & TotalRepaid_IncludingOverpayments < prepaymentFi & X..Repaid < 100, "Qual.min",
                                                      if_else(TotalRepaid_IncludingOverpayments >= prepaymentFi & X..Repaid < 100, "Qual.full",
                                                                if_else(prepaymentFi < 3000 & (TotalRepaid_IncludingOverpayments >= 100 & X..Repaid < 100 & TotalRepaid_IncludingOverpayments < prepaymentFi)  , "Starters",
                                                                        if_else(X..Repaid ==100 , "Finishers", "NA")))))))


#Adding categories to clients with bellow 3000 credit
#Adding empty columns to bind together these two datasets
low$prepayment <- 0
low$prepaymentAV <- 0
low$prepaymentFi <- low$TotalCredit
low <- low %>% mutate(Qual=
                        if_else(TotalRepaid_IncludingOverpayments <100, "Non.starter",
                                if_else(TotalRepaid_IncludingOverpayments >= 100 & TotalRepaid_IncludingOverpayments < prepaymentFi & X..Repaid < 100 , "Starters",
                                                if_else(TotalRepaid_IncludingOverpayments >= prepaymentFi & X..Repaid < 100, "Qual.full",
                                                                if_else(X..Repaid ==100 , "Finishers", "NA")))))





table(low$Qual)
#Readding two dataframes
df4 <- rbind(df3, low)
# Adding a new column of Strong starters
df4$QualF <- ifelse(df4$TotalRepaid_IncludingOverpayments >= df4$prepaymentFi + 5000 , 1, df4$Qual)
table(df4$QualF)
length(unique(df4$GlobalClientID[df4$QualF == "1" | df4$QualF == "Finishers"]))
######Analysis of the site level
sitelevel <- df4 %>%
  group_by(site.id,RegionName, DistrictName, SiteName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    Non.starters = length(unique(GlobalClientID)[QualF == "Non.starter"]),
    Starters = length(unique(GlobalClientID)[QualF == "Starters"]),
    MinimumQualifiers = length(unique(GlobalClientID)[QualF == "Qual.min"]),
    Fullqualifiers = length(unique(GlobalClientID)[QualF== "Qual.full" | QualF == "1" | QualF == "Finishers"]),
    Finishers = length(unique(GlobalClientID)[QualF == "Finishers"]),
    Strong.starters = length(unique(GlobalClientID)[QualF == "1" | QualF == "Finishers"])
  )
#Write a csv

write.csv(sitelevel, "sitelevel06.csv")

grouplevel <- df4 %>%
  group_by(grp.id,DistrictName, SiteName, GroupName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    Non.starters = length(unique(GlobalClientID)[QualF == "Non.starter"]),
    Starters = length(unique(GlobalClientID)[QualF == "Starters"]),
    MinimumQualifiers = length(unique(GlobalClientID)[QualF == "Qual.min"]),
    Fullqualifiers = length(unique(GlobalClientID)[QualF== "Qual.full" | QualF == "1" | QualF == "Finishers"]),
    Finishers = length(unique(GlobalClientID)[QualF == "Finishers"]),
    Strong.starters = length(unique(GlobalClientID)[QualF == "1" | QualF == "Finishers"])
  )
getwd()
write.csv(grouplevel, "grouplevel05.csv")
#Adding the GL to the dataset

allGL <- subset(df4, Facilitator == "True")
allGL <- select(allGL,"grp.id", "LastName" ,"FirstName","OAFID","AccountNumber")
grouplevel <- merge(grouplevel, allGL, by = c("grp.id"), all.x = TRUE, ignore.case = TRUE)
#Select the columns I want 
colnames(grouplevel)
grouplevel <- select(grouplevel,"grp.id","DistrictName","SiteName","GroupName","LastName","FirstName", "OAFID","AccountNumber","tot.cl","Strong.starters")
###Distrist level
districtA <- df4 %>%
  group_by(RegionName,DistrictName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    Non.starters = length(unique(GlobalClientID)[QualF == "Non.starter"]),
    Starters = length(unique(GlobalClientID)[QualF == "Starters"]),
    MinimumQualifiers = length(unique(GlobalClientID)[QualF == "Qual.min"]),
    Fullqualifiers = length(unique(GlobalClientID)[QualF== "Qual.full" | QualF == "1" | QualF == "Finishers"]),
    Finishers = length(unique(GlobalClientID)[QualF == "Finishers"]),
    Strong.starters = length(unique(GlobalClientID)[QualF == "1" | QualF == "Finishers"])
  )

### Region Level
districtB <- df4 %>%
  group_by(RegionName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    starters = length(unique(GlobalClientID)[QualF == "Non.starter"]),
    MinimumQualifiers = length(unique(GlobalClientID)[QualF == "Qual.min"]),
    FullQualifiers = length(unique(GlobalClientID)[QualF == "Qual.full"]),
    Strong.starters = length(unique(GlobalClientID)[QualF == "1"| QualF=="Finishers"])
  )
sum(district$FullQualifiers)
############# carried over clients #################
d21c <- subset(df4, X2021A_Enrollment.Fee.adjustment > 0)
table(d21c$QualF)
############# New clients #################

d21n <- subset(df4, NewMember == "True")
table(d21n$QualF)

################# SMS to clients in various sites ####
schedule <- read.csv("sitedelivery2.csv", header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "") 
#check sites that have Loaded and delivered
length(unique(schedule$Site.id[schedule$Delivery.Status == "Delivered" | schedule$Delivery.Status == "Loaded"]))
#Adding deliverly status
##Site id
df4$Site.id <- paste(df4$DistrictName, df4$SiteName, sep = "")
df4$Delivery.Status <- schedule$Delivery.Status[match(df4$Site.id, schedule$Site.id)]
#Changing the dates formats
v21$Delivery.Dates <- as.Date(v21$Delivery.Dates, "%Y-%m-%d")
##Adding the schedule also to the transactions
v21$Delivery.Status <- schedule$Delivery.Status[match(v21$site.id, schedule$site.id)]

#Subseting clients with phone numbers and delivered or loaded
delivery <- subset(df4,Delivery.Status == "Delivered" |
                       Delivery.Status == "Loaded")
#Adding phone numbers
delivery$ClientPhone2 <- d21B$ClientPhone[match(delivery$GlobalClientID, d21B$GlobalClientID)]
#Selecting clients who have phone
delivery <- subset(delivery, !is.na(ClientPhone))
colnames(delivery)
delivery <- delivery[,c("RegionName" ,"DistrictName","SiteName","ClientPhone2","prepaymentFi","QualF", "Delivery.Status" )]
#Subseting all the categories
nonstarters <- subset(delivery, QualF == "Non.starter")
starters <- subset(delivery, QualF == "Starters")
qualified.min <- subset(delivery, QualF == "Qual.min")
qualified.full <- subset(delivery, QualF == "Qual.full")
strong.starters <- subset(delivery, QualF == "1" | QualF == "Finishers")
#Writing the CSV
setwd("/Users/jimmymarcel/Documents/One Acre Fund/DATA_FILES/Key Stats/list_for_SMS")
write.csv(nonstarters, "Non.starters.csv")
write.csv(starters, "Starters.csv")
write.csv(qualified.min, "qualified.minim.csv")
write.csv(qualified.full, "qualified.full.csv")
write.csv(strong.starters, "strong.starters.csv")

######### farmer level HP#######
# we exclude finishers and strong starters
df5 <- subset(df4, QualF != "1" & QualF != "Finishers")
#Adding a column of required amount to be strong start
df5$topay <- ifelse(df5$TotalCredit > 5000, (df5$prepaymentFi + 5000), df5$prepaymentFi)
#adding the colum to show the remaining to be paid
df5 <- df5 %>% mutate(Remaining.tpay = topay - TotalRepaid_IncludingOverpayments)
colnames(df5)
df5 <- df5[,c(3,6,9,10,11,19,26,30,88,92)]
df5 <- subset(df5, !is.na(ClientPhone))
write.csv(df5, "data.remaingtopay.csv")
#Farmers with 0 prepayment
length(unique(df4$GlobalClientID[df4$TotalRepaid_IncludingOverpayments == 0]))

####### Site before and after pick up ####
#Subsetting the delivered and loaded sites
schedule <- read.csv("21A Official Delivery Schedule - Schedule.csv", header = TRUE,   
                     stringsAsFactors = FALSE, na.strings = "") 
delivered <- subset(schedule,Delivery.Status == "Delivered" |
                     Delivery.Status == "Delivered+Return")

schedule$site.id <- paste(schedule$Districts, schedule$Site.Name, sep = "")
##Adding delivery date to the transactions and delivery status
df4$Delivery.Dates<- schedule$Delivery.Dates[match(df4$site.id, schedule$Site.id)]
## Adding delivery status
colnames(df4)
df4$Delivery.Status <- schedule$Delivery.Status[match(df4$site.id, schedule$site.id)]
table(df4$Delivery.Status)
#Before the delivery on August 13the and after delivery(after 13th)
#After delivery
sitedeliveryafter <- df4 %>%
  group_by(site.id, RegionName, DistrictName, SiteName, Delivery.Status) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    starters = length(unique(GlobalClientID)[QualF == "Non.starter"]),
    MinimumQualifiers = length(unique(GlobalClientID)[QualF == "Qual.min"]),
    FullQualifiers = length(unique(GlobalClientID)[QualF == "Qual.full"]),
    Strong.starters = length(unique(GlobalClientID)[QualF == "1"| QualF=="Finishers"])
  )

write.csv(sitedeliveryafter, "Sitedeliveryafter11.csv")
write.csv(sitedeliverybefore, "Sitedeliverybefore.csv")



###matching with dama's results
df4$dama <- dama$Prep.required[match(df4$AccountNumber, dama$AccountNumber)]
df4$check <- ifelse(df4$prepaymentFi != df4$dama , 1, 0)
check <- subset(df4, check== "1")
colnames(check)
check <- check[,c(3,6,31,78:93)]
write_csv(check, "Check.csv")

#### Adrian request data
# Adding a column of Agcredit
df5 <- df4 %>% mutate(Agcredit = X2021A_DAP.kg*480 + 
                        X2021A_Maize.kg + 
                        X2021A_NPK.17.kg*603 + 
                        X2021A_Pan.53.kg*1547 + 
                        X2021A_Pan.691.kg*962 + 
                        X2021A_SC.403.kg*1486 + 
                        X2021A_SC.637.kg*1486 + 
                        X2021A_UREA.kg*447 + 
                        X2021A_WH.403.kg*526 + 
                        X2021A_WH.505.kg*526 +
                        X2021A_WH.605.kg*526
)


data <- df5 %>%
  group_by(RegionName) %>%
  summarise(
    totalclients = length(unique(GlobalClientID)),
    Ag.credit = sum(Agcredit),
    avgcre = Ag.credit/totalclients
  )
#Repayment Position by -returning clients completed loan, - returning clients carryover- New clients,
dt <- df4 %>%
  group_by(RegionName) %>%
summarise(
  new.clients = length(unique(GlobalClientID[NewMember == "True"])),
  new.clcredit = sum(TotalCredit[NewMember == "True"]),
  new.clpaid = sum(TotalRepaid[NewMember == "True"]),
  new.clrepaid = new.clpaid/new.clcredit,
  returnfinclients = length(unique(GlobalClientID[NewMember =="False" & X2021A_Enrollment.Fee.adjustment == 0])),
  returnfincredit = sum(TotalCredit[NewMember =="False" & X2021A_Enrollment.Fee.adjustment == 0]),
  returnfinpaid = sum(TotalRepaid[NewMember =="False" & X2021A_Enrollment.Fee.adjustment == 0]),
  returnfinrepaid = returnfinpaid/returnfincredit,
  returncaryclients = length(unique(GlobalClientID[NewMember =="False" & X2021A_Enrollment.Fee.adjustment > 0])),
  returncarycredit = sum(TotalCredit[NewMember =="False" & X2021A_Enrollment.Fee.adjustment > 0]),
  returncarypaid = sum(TotalRepaid[NewMember =="False" & X2021A_Enrollment.Fee.adjustment > 0]),
  returncaryrepaid = returncarypaid/returncarycredit
)

#Check
ds <- subset(df4, NewMember == "False" & X2021A_Enrollment.Fee.adjustment >0 & RegionName == "West")
sum(ds$TotalRepaid)/sum(ds$TotalCredit)
write_csv(dt, "dataA.csv")
  
  ########## Checking snea through clients ##########
 #Ordered 
SeasonAclients <- subset(df4, X2021A_CycleCredit >0)
#We verify sneak throught clients
SeasonAclients <- SeasonAclients %>% mutate(Sneak = ifelse(TotalRepaid_IncludingOverpayments < prepaymentFi, "Sneakthrough", "Good"))
#Grouping all the clients to find the Sneak through and good clients
colnames(SeasonAclients)
SeasonAclients$Site.id <- paste(SeasonAclients$DistrictName, SeasonAclients$SiteName, sep = "")
SeasonAclients$Grp.id <- paste(SeasonAclients$DistrictName, SeasonAclients$SiteName, SeasonAclients$GroupName, sep = "")
####Summary Sitele
SneakS <- SeasonAclients %>%
  group_by(Site.id, RegionName, DistrictName, SiteName) %>%
  summarise(
    Clientstookinputs = length(unique(GlobalClientID)),
    Sneakthrough = length(unique(GlobalClientID[Sneak == "Sneakthrough"])),
    Percentage = round(100*(sum(Sneakthrough)/sum(Clientstookinputs)),2)
  )
sum(SneakS$Sneakthrough)/sum(SneakS$Clientstookinputs)
#### District
SneakD <- SeasonAclients %>%
  group_by(RegionName, DistrictName) %>%
  summarise(
    Clientstookinputs = length(unique(GlobalClientID)),
    Sneakthrough = length(unique(GlobalClientID[Sneak == "Sneakthrough"])),
    Percentage = round(100*(sum(Sneakthrough)/sum(Clientstookinputs)),2)
  )
#Region
SneakR <- SeasonAclients %>%
  group_by(RegionName) %>%
  summarise(
    Clientstookinputs = length(unique(GlobalClientID)),
    Sneakthrough = length(unique(GlobalClientID[Sneak == "Sneakthrough"])),
    Percentage = round(100*(sum(Sneakthrough)/sum(Clientstookinputs)),2)
  )
#####Selecting only the sneak through clients
SneakF <- subset(SeasonAclients, Sneak == "Sneakthrough")

colnames(SneakF)
SneakF <- SneakF[,c(2:10, 19:22,26,38:39, 48:76,91,93)]
write.csv(SneakS, "sneakthroughclientsS.csv")
write.csv(SneakD, "sneakthroughclientslistD.csv")
write.csv(SneakR, "sneakthroughclientslistR.csv")