getwd()
library(data.table)
library(ggplot2)
library(reshape2)
#library(plyr)
library(stats)
library(lattice)
library(lubridate)
library(tidyverse)
#Loading data
#Set working directory
setwd("~/Documents/One Acre Fund/DATA_FILES/Key Stats")
d21 <- read.csv("Season Clients Detailed_20210102-110340.csv", header = TRUE,   
              stringsAsFactors = FALSE, na.strings = "")

#Adding site Id
d21$site.id <- paste(d21$DistrictName, d21$SiteName, sep = "")
#Adding group Id
d21$grp.id <- paste(d21$DistrictName, d21$SiteName, d21$GroupName, sep = "")
d21$AmountRemainingonHP = ifelse(d21$X..Repaid < 60, (d21$TotalCredit*0.60)-d21$TotalRepaid,0)
#Adding the credits added
d21 <- d21 %>% mutate(Newcredit = 
                      X2021B_B.BIG.2245.kg*1400 +
                      X2021B_B.DAP.kg*480 +
                      X2021B_B.Maize.kg*950 +
                      X2021B_B.NPK.17.kg*603 + 
                      X2021B_B.UREA.kg*447 +
                      X2021B_B.KAR.qty*140 + 
                      X2021B_B.POV.qty*140 + 
                      X2021B_B.SHU.qty*140 + 
                      X2021B_B.TOM.qty*140 + 
                      X2021B_B.TUN.qty*140 +
                      X2021B_B.PICS100KG.qty*2240 + 
                      X2021B_B.PICS50KG.qty*1800 +
                      X2021B_B.GLP.SKP.200.qty*25000 +
                      X2021B_B.Tecno.qty*12000 +
                      X2021B_B.Biolite.SHS.qty*85000 + 
                      X2021B_B.GLP.Pico.qty*9000 + 
                      X2021B_B.DLight.S200.qty*25000 + 
                      X2021B_B.Niwa.300XL.qty*35000 +
                      X2021B_B.Travertine.kg*100)
### Adding new credit to old credit to make total credit added
d21$TotalnewCredit <- d21$TotalCredit + d21$Newcredit
d21$NewX..Repaid <- round(100*(d21$TotalRepaid_IncludingOverpayments/d21$TotalnewCredit),2)
length(unique(d21$GlobalClientID[d21$NewX..Repaid >= 42]))/length(unique(d21$GlobalClientID[d21$NewX..Repaid]))
#Removing trials and 21A Data
colnames(d21)
d211 <- d21 %>% select(-ends_with("Trial.qty"))
d211 <- d21 %>% select(-starts_with("X2021A_"))
#Sum of all products
colnames(d21)
d21$sumproB <- rowSums(d21[,85:103])
#Enrolled clients
d211 <- subset(d21, sumproB > 0)
#Summing the products based on the group
df <- d211 %>% 
  mutate(AG = 
           X2021B_B.BIG.2245.kg +
           X2021B_B.DAP.kg +
           X2021B_B.Maize.kg +
           X2021B_B.NPK.17.kg + 
           X2021B_B.UREA.kg,
         Veg = X2021B_B.KAR.qty + 
           X2021B_B.POV.qty + 
           X2021B_B.SHU.qty + 
           X2021B_B.TOM.qty + 
           X2021B_B.TUN.qty,
         Pics = X2021B_B.PICS100KG.qty + 
           X2021B_B.PICS50KG.qty,
         Pico = X2021B_B.GLP.Pico.qty ,
         Phn = X2021B_B.Tecno.qty,
         Solar = X2021B_B.Biolite.SHS.qty + 
              X2021B_B.GLP.SKP.200.qty + 
              X2021B_B.DLight.S200.qty + 
              X2021B_B.Niwa.300XL.qty,
         Trav = X2021B_B.Travertine.kg)

#Calculating prepayment conditions without avocado+10 and chicken 
#New Clients
df$Newcredit <- as.numeric(df$Newcredit)
NEWGbig <- df %>% filter(TotalCredit == 0 & Newcredit >= 3000 ) %>% 
  mutate(prepayment = if_else (AG >= 100 & Solar >= 1 , "15000",
                                            if_else(AG < 100 & AG >0 & Solar >= 1, "11000", 
                                                    if_else(AG >= 100 & Solar ==0 , "7000", 
                                                            if_else(AG==0 & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 25 & Pics==0, "2500",
                                                                    if_else(AG == 0  & Solar == 0 & Pico == 0 & Phn == 0 & Trav >= 50, "3000",
                                                                            if_else(AG == 0  & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 0 & Veg > 0, "3000",
                                                                                    #if_else(Avoc == 0 & AG == 0  & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 0 & Veg==0 & Chic >= 1, "0",
                                                                                            if_else(AG == 0 & Solar >=1 | Pico >2, "8000",
                                                                                                    if_else(AG < 100 & AG >0 & Solar == 0, "3000", "3000")))))))))

Newsmal <- df %>% filter(TotalCredit == 0 & Newcredit < 3000) %>%
  mutate(prepayment = Newcredit)



NEW <- rbind(NEWGbig,Newsmal )
#Old Clients: For old clients
#Add the amount the amount remaining to be on health path
OLD <- subset(df, df$TotalCredit >0)
OLD$AmountRemainingonHP = ifelse(OLD$X..Repaid < 60, (OLD$TotalCredit*0.60)-OLD$TotalRepaid,0)
OLD$AmountRemainingonHP <- as.numeric(OLD$AmountRemainingonHP)
OLD <- OLD %>%
  mutate(prepayment = if_else (AG >= 100 & Solar >= 1 , RemainingCredit + 8000,
                              if_else(AG < 100 & AG >0 & Solar >= 1, RemainingCredit + 8000, 
                                      if_else(AG == 0 & Solar >=1 | Pico >2, RemainingCredit + 8000 ,AmountRemainingonHP))))
                                    
#Re adding together the datasets
df2 <- rbind(OLD, NEW)

########################## Clustering clients #################
 #Old clients not enrolled
length(d21$GlobalClientID[d21$TotalCredit>0 &
                            d21$sumproB == 0 &
                            d21$X..Repaid < 60])
#Old clients not enrolled
length(d21$GlobalClientID[d21$TotalCredit>0 &
                            d21$sumproB == 0 &
                            d21$X..Repaid < 60])
#Old clients not enrolled<<< total credit
mean(d21$TotalCredit[d21$TotalCredit>0 &
                            d21$sumproB == 0 &
                            d21$X..Repaid >=60])
#Old clients not enrolled<<< Remaining credit
sum(d21$AmountRemainingonHP[d21$NewMember == "False" &
                           d21$sumproB == 0 &
                           d21$TotalCredit > 0 &
                           d21$X..Repaid <60])/length(unique(d21$GlobalClientID[d21$X..Repaid < 60 & d21$sumproB > 0]))

sum(d21$RemainingCredit)/length(unique(d21$GlobalClientID[d21$TotalCredit>0]))
#Old clients not enrolled<<< Total Repaid
mean(d21$TotalRepaid[d21$TotalCredit>0 &
                           d21$sumproB == 0 &
                           d21$TotalCredit > 0 &
                           d21$X..Repaid <60])

#Old clients enrolled 
length(d21$GlobalClientID[d21$TotalCredit >0 &
                            d21$sumproB > 0 &
                            d21$TotalCredit >0 &
                            d21$X..Repaid >= 60])

#Old clients  enrolled<<< total credit
mean(d21$TotalCredit[d21$TotalCredit>0 &
                       d21$sumproB > 0 &
                       d21$TotalCredit > 0 &
                       d21$X..Repaid <60])
#Old clients  enrolled<<< total credit added for new season
mean(df$TotalnewCredit[d21$TotalCredit >0 &
                       d21$sumproB > 0 &
                       d21$TotalCredit > 0 &
                       d21$X..Repaid < 60])

#Old clients enrolled<<< Remaining credit
sum(d21$RemainingCredit[d21$TotalCredit>0 &
                           d21$sumproB >0 &
                           d21$TotalCredit > 0 &
                           d21$X..Repaid <60])/

#Old clients  enrolled<<< Total Repaid
mean(d21$TotalRepaid[d21$TotalCredit >0 &
                       d21$sumproB > 0 &
                       d21$TotalCredit > 0 &
                       d21$X..Repaid < 60])
#Old clients at Jan HP


######################  New Clients ################
#removing outliers
NEW$TotalRepaidN <- ifelse(NEW$TotalRepaid_IncludingOverpayments> NEW$Newcredit, NEW$Newcredit, NEW$TotalRepaid_IncludingOverpayments)
NEW$NewX..Repaid <- round(100*(NEW$TotalRepaidN/NEW$Newcredit),2)
NEW$prepayment <- as.numeric(NEW$prepayment)
###Adding the prepayment amount remainig to all new clients
NEW$RemainingPrep <- ifelse(NEW$TotalRepaid_IncludingOverpayments < NEW$prepayment, NEW$prepayment - NEW$TotalRepaid_IncludingOverpayments  , 0)
mean(NEW$RemainingPrep)
## New Clients Qualified and not qualified for 21B inputs
length(unique(NEW$GlobalClientID[NEW$RemainingPrep > 0]))
### Average credit ###
mean(NEW$Newcredit[NEW$RemainingPrep > 0])
# Average Remaining credit ###
NEW$RemainingCreditN <- NEW$Newcredit -  NEW$TotalRepaidN
mean(NEW$Newcredit[NEW$RemainingPrep == 0])
# Avegerage prepayment remaining
mean(NEW$RemainingPrep[NEW$RemainingPrep > 0])
# Averge repaid
mean(NEW$TotalRepaidN[NEW$RemainingPrep > 0])
#See where we will be if all clients made prepayments
NEW$X..RepaidPre <- round(100*(NEW$prepayment/NEW$Newcredit),2)
sum(NEW$prepayment[NEW$RemainingPrep == 0])/ sum(NEW$Newcredit[NEW$RemainingPrep == 0])
#Average Remaining to rech Jan HP
NEW$AmountRemainingonJanHP = ifelse(NEW$NewX..Repaid < 42, (NEW$Newcredit*0.42)-NEW$TotalRepaidN,0)
mean(NEW$AmountRemainingonJanHP[NEW$RemainingPrep > 0])
sum(NEW$prepayment)/sum(NEW$Newcredit)
#Printing a list of new clients to send SMS with the amount remaining to prepayment
colnames(NEW)
PrintNEW <- subset(NEW, !is.na(ClientPhone) & RemainingPrep >10)
PrintNEW <- select(PrintNEW,"DistrictName","SiteName","LastName","FirstName","GlobalClientID", "ClientPhone", "RemainingPrep" )
write.csv(PrintNEW, "NEWclientsEnrolled.csv")
################ Telling the clients how much they need to pay to be at 100% by July ############
length(unique(NEW$GlobalClientID[NEW$Pico>0]))

list <- subset(NEW, NEW$X2021B_B.GLP.Pico.qty >0 | NEW$X2021B_B.GLP.SKP.200.qty >0 & !is.na(NEW$ClientPhone))
list <- select(list,"DistrictName","SiteName","LastName","FirstName","GlobalClientID", "ClientPhone", "RemainingPrep" )
write.csv(list, "Newlist.csv")

#### For Dama if we add 3000 to each clients

df$Janrepaid <- df$TotalRepaid + 3000
sum(df$Janrepaid)/sum(df$TotalnewCredit)

########### GL Agents ###
agents <- subset(d21, d21$X2021A_GL.Agent.Trial.qty >0)
d210$AgentsR <- agents$X..Repaid[match(d210$grp.id, agents$grp.id)]
d210$AgentsN <- agents$LastName[match(d210$grp.id, agents$grp.id)]
#Enrolled GL and not enrolled
agents$sumproB <- rowSums(agents[,85:103])
agents$sumproA <- rowSums(agents[,48:84])

#Selecting only groups with GL agents
groups <- subset(d210, !is.na(d210$AgentsN))
colnames(groups)
groups <- select(groups,"grp.id","DistrictName", "SiteName" , "LastName" ,"FirstName","X..Repaid","AgentsR", "AgentsN" )
# % Repaid GL and groups
groupS <- groups %>%
  group_by(grp.id) %>%
  summarise(
  avgme = mean(X..Repaid),
  avgGL = mean(AgentsR))

mean(groups$Agents)
write.csv(groupS,"GLvsgroups.csv")
getwd()

################## chicken checking
chicken <- subset(EnrblHP, EnrblHP$X2021A_Inkoko.Confirmation.qty>0|
                           EnrblHP$X2021A_Inkoko_Oct.qty |
                           EnrblHP$X2021A_Inkoko_Sept.qty)



