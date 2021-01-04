### Debt forgiviness approch
#Creating a big dataframe with different criterion
   # Vlookup in most concerning


library(dplyr)
#Adding 10% for each %repaid

d20$GlobalClientId <- d20$GlobalClientID
colnames(d20)

deptforg<- d20 %>%
  group_by(grp.id) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    tot.fin = length(unique(GlobalClientID[RemainingCredit == 0])),
    tot.most = length(unique(GlobalClientID[status == "most concerning"])),
    tot.enro = length(unique(GlobalClientID[enrolled > 0])),
    tot.cr = sum(RemainingCredit),
    below100 = length(unique(GlobalClientID[added10 <= 100])),
    below95 = length(unique(GlobalClientID[added10 <= 95])),
    below90 = length(unique(GlobalClientID[added10 <= 90])),
    below85 = length(unique(GlobalClientID[added10 <= 85])),
    below80 = length(unique(GlobalClientID[added10 <= 80])),
    below75 = length(unique(GlobalClientID[added10 <= 75])),
    below70 = length(unique(GlobalClientID[added10 <= 70])),
    below65 = length(unique(GlobalClientID[added10 <= 65])),
    below60 = length(unique(GlobalClientID[added10 <= 60])),
    below55 = length(unique(GlobalClientID[added10 <= 55])),
    below50 = length(unique(GlobalClientID[added10 <= 50])),
    below45 = length(unique(GlobalClientID[added10 <= 45])),
    below40 = length(unique(GlobalClientID[added10 <= 40])),
    below35 = length(unique(GlobalClientID[added10 <= 35])),
    amount = mean(RemainingCredit)
    )

#Adding a column of who need to finish
deptforg <- deptforg %>% mutate(needtofinish = tot.cl- tot.fin )
#Groups with 1 or more client in the "most concerning" bucket:
length(unique(deptforg$grp.id[deptforg$tot.most >= 1]))
#Groups with 1 or more client below 65%
length(unique(deptforg$grp.id[deptforg$below65 >= 1]))

#Finishers in groups with 1 or more "most concerning" client
  ##groups with 1+ most concerning
sum(deptforg$tot.cl[deptforg$tot.most >=1])
getwd()
write.csv(deptforg,"dept.forgiveness.csv")
#Groups with at least 1 finisher
length(unique(deptforg$grp.id[deptforg$tot.fin >= 1]))

#Groups finished
length(unique(deptforg$grp.id[deptforg$tot.fin == deptforg$tot.cl]))
#Groups with only 1 non finishers
length(unique(deptforg$grp.id[deptforg$needtofinish == 1]))
#
sum(deptforg$tot.cl[deptforg$needtofinish >=1])
#### Parcentge paid #####
### clients below threshold 
length(unique(df$GlobalClientID[df$X..Repaid <= 85]))
#Total Banned





#### Banned and enrolled
getwd()
length(unique(df$GlobalClientID[df$X..Repaid <= 25 & df$enrolled >0]))

#Calculating the total number of farmers banned after 10%
sum(deptforg$tot.cl[deptforg$below100 !=0])
#Clients bellow the threshold
sum(deptforg$below100)
#Clients in Groups where 1 or more client is below thrshold (after 10% Investment added)
sum(deptforg$tot.cl[deptforg$below35 !=0 &
                      deptforg$below35 >= 1])

#enrolled bellow threshold
sum(deptforg$below100[deptforg$below100 !=0 &
                      deptforg$tot.enro !=0])

#Enrolled clients in Groups where 1 or more client is below thrshold (after 10% Investment added)
sum(deptforg$tot.cl[deptforg$below100 !=0 &
                      deptforg$below95 >= 1 &
                      deptforg$tot.enro !=0])


#5. Enrolled Clients above this threshold but below 100%
length(unique(datatrack2$GlobalClientID[datatrack2$enrolled >=1 & datatrack2$added10 < 100 & 
                                          datatrack2$added10 >= 95 ]))

# enrolled not finished
length(unique(df$GlobalClientID[df$enrolled >=1 & df$X..Repaid < 100 ]))

#5. A - Group LIABILITY - Clients Using Roll-Over - Enrolled Clients above this threshold but below 100% (after 10% investment), 
#and not banned.
   # a. clients not banned
deptforg2 <- subset(deptforg,tot.enro != 0)
sum(deptforg2$tot.cl[deptforg2$below100 !=0 &
                      deptforg2$below95 >= 1 &
                      deptforg2$needtofinish < 95])




#5. b. Average Amount Owed by these clients
mean(datatrack2$RemainingCredit[datatrack2$added10 < 100 &
                                datatrack2$added10 >= 35])

#6. credit rolled over
sum(deptforg$tot.cr[deptforg$below100 !=0 &
                      deptforg$below35 >= 1])

#Groups with only 1 nonfinishers
length(deptforg$tot.cl[deptforg$need == 1])


#Credit rollover Depth Forgiviness planner
#Repayment_position_by_category
Posit <- datatrack %>% group_by(RegionName) %>% 
  summarise(
    newclients = length(unique(GlobalClientID[NewMember == "True"])),
    new.Credit = sum(TotalCredit[NewMember == "True"]),
    new.Repaid = sum(TotalRepaid[NewMember == "True"]),
    returnfini = length(unique(GlobalClientID[NewMember == "False" & X2021A_Enrollment.Fee.adjustment == 0])),
    returnfinicredit = sum(TotalCredit[NewMember == "False" & X2021A_Enrollment.Fee.adjustment == 0]),
    returnfinirepaid = sum(TotalRepaid[NewMember == "False" & X2021A_Enrollment.Fee.adjustment == 0]),
    returnnotfini = length(unique(GlobalClientID[NewMember == "False" & X2021A_Enrollment.Fee.adjustment > 0])),
    returnnotfinicredit = sum(TotalCredit[NewMember == "False" & X2021A_Enrollment.Fee.adjustment > 0]),
    returnnotfinirepaid = sum(TotalRepaid[NewMember == "False" & X2021A_Enrollment.Fee.adjustment > 0])
    
  )
write_csv(Posit, "repaymentposit.csv")
  



