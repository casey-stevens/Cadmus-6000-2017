#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))

#Read in data for analysis
survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Combined")
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))




#############################################################################################
#Item 124: DISTRIBUTION OF HOMES BY OWNERSHIP TYPE AND STATE (SF table 131, MH table 106)
#############################################################################################
#subset to columns needed for analysis
item124.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Do.you.own.or.rent.your.home?"))]
colnames(item124.dat) <- c("Ownership.Type", "CK_Cadmus_ID")
item124.dat$count <- 1

#merge together analysis data with cleaned RBSA data
item124.dat1 <- left_join(rbsa.dat, item124.dat, by = "CK_Cadmus_ID")

unique(item124.dat1$Ownership.Type)

#remove NA from ownership type
item124.dat2 <- item124.dat1[which(!(is.na(item124.dat1$Ownership.Type))),]

#summarise by state
#by ownership.type
item124.state1 <- summarise(group_by(item124.dat2, BuildingType, State, Ownership.Type)
                           ,Count = sum(count)
                           ,SampleSize = length(unique(CK_Cadmus_ID)))
#across ownership.type
item124.state2 <- summarise(group_by(item124.dat2, BuildingType, State)
                            ,Ownership.Type = "Total"
                            ,Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID)))
#summarise across state
#by ownership.type
item124.region1 <- summarise(group_by(item124.dat2, BuildingType, Ownership.Type)
                            ,State = "Region"
                            ,Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID)))
#across ownership.type
item124.region2 <- summarise(group_by(item124.dat2, BuildingType)
                            ,State = "Region"
                            ,Ownership.Type = "Total"
                            ,Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID)))

item124.merge <- rbind.data.frame(item124.state1, item124.state2, item124.region1, item124.region2, stringsAsFactors = F)

item124.tot.count <- rbind.data.frame(item124.state2, item124.region2, stringsAsFactors = F)
item124.tot.count <- item124.tot.count[which(colnames(item124.tot.count) %in% c("BuildingType","State", "Count", "SampleSize"))]
colnames(item124.tot.count) <- c("BuildingType","State", "Total.Count", "Denom.SampleSize")

item124.final <- left_join(item124.merge, item124.tot.count, by = c("BuildingType","State"))
item124.final$Percent <- item124.final$Count / item124.final$Total.Count
item124.final$SE <- sqrt(item124.final$Percent * (1 - item124.final$Percent) / item124.final$Denom.SampleSize)

item124.cast <- dcast(setDT(item124.final)
                      ,formula = BuildingType + Ownership.Type ~ State
                      ,value.var = c("Percent", "SE", "SampleSize"))


item124.table <- data.frame("BuildingType" = item124.cast$BuildingType
                            ,"Ownership.Type" = item124.cast$Ownership.Type
                            ,"Percent_ID" = NA#item124.cast$Percent_ID
                            ,"SE_ID" = NA#item124.cast$SE_ID
                            ,"Percent_MT" = item124.cast$Percent_MT
                            ,"SE_MT" = item124.cast$SE_MT
                            ,"Percent_OR" = NA#item124.cast$Percent_OR
                            ,"SE_OR" = NA#item124.cast$SE_OR
                            ,"Percent_WA" = item124.cast$Percent_WA
                            ,"SE_WA" = item124.cast$SE_WA
                            ,"Percent_Region" = item124.cast$Percent_Region
                            ,"SE_Region" = item124.cast$SE_Region
                            ,"SampleSize" = item124.cast$SampleSize_Region)

item124.table1 <- item124.table[which(item124.table$BuildingType %in% c("Single Family", "Manufactured")),]





#############################################################################################
#Item 125: PERCENTAGE OF HOMES AS PRIMARY RESIDENCE BY STATE (SF table 132, MH table 107)
#############################################################################################
#subset to columns needed for analysis
item125.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_IsThisYourPrimaryHome_Y_N"
                                                                                     ,""))])
colnames(item125.dat) <- c("CK_Cadmus_ID", "Primary_Home")
item125.dat$count <- 1

#remove any repeat header rows from exporting
item125.dat0 <- item125.dat[which(item125.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item125.dat1 <- left_join(item125.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item125.dat1$Primary_Home)

item125.dat2 <- item125.dat1[which(!(is.na(item125.dat1$Primary_Home))),]

#summarise by state
item125.sum1 <- summarise(group_by(item125.dat2, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = length(which(Primary_Home == "Primary"))
                          ,Total.Count = sum(count)
                          ,Percent = Count / Total.Count
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#summarise across states
item125.sum2 <- summarise(group_by(item125.dat2, BuildingType)
                          ,State = "Region"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = length(which(Primary_Home == "Primary"))
                          ,Total.Count = sum(count)
                          ,Percent = Count / Total.Count
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

item125.sum <- rbind.data.frame(item125.sum1, item125.sum2, stringsAsFactors = F)


item125.final <- data.frame("BuildingType" = item125.sum$BuildingType
                            ,"State" = item125.sum$State
                            ,"Percent" = item125.sum$Percent
                            ,"SE" = item125.sum$SE
                            ,"SampleSize" = item125.sum$SampleSize)

item125.table <- item125.final[which(item125.final$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
# Needs to be updated once we get the correct fields for this table
#############################################################################################
#Item 126: PERCENTAGE OF HOMES WITH HOME OFFICES BY STATE (SF table 133, MH table 108)
#############################################################################################
#subset to columns needed for analysis
item126.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,""
                                                                                     ,""))])
colnames(item126.dat) <- c("CK_Cadmus_ID", "")
item126.dat$count <- 1

#remove any repeat header rows from exporting
item126.dat0 <- item126.dat[which(item126.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item126.dat1 <- left_join(item126.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item126.dat1$Primary_Home)

item126.dat2 <- item126.dat1[which(!(is.na(item126.dat1$Primary_Home))),]

#summarise by state
item126.sum1 <- summarise(group_by(item126.dat2, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = length(which(Primary_Home == "Primary"))
                          ,Total.Count = sum(count)
                          ,Percent = Count / Total.Count
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#summarise across states
item126.sum2 <- summarise(group_by(item126.dat2, BuildingType)
                          ,State = "Region"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = length(which(Primary_Home == "Primary"))
                          ,Total.Count = sum(count)
                          ,Percent = Count / Total.Count
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

item126.sum <- rbind.data.frame(item126.sum1, item126.sum2, stringsAsFactors = F)


item126.final <- data.frame("BuildingType" = item126.sum$BuildingType
                            ,"State" = item126.sum$State
                            ,"Percent" = item126.sum$Percent
                            ,"SE" = item126.sum$SE
                            ,"SampleSize" = item126.sum$SampleSize)

item126.table <- item126.final[which(item126.final$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
#Item 127: DISTRIBUTION OF HOMES WITH ELECTRIC FUEL ASSISTANCE BY PERCENTAGE OF ASSISTANCE AND STATE (SF table 134, MH table 109)
#############################################################################################
#subset to columns needed for analysis
item127.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Does.your.household.receive.financial.assistance.to.pay.a.portion.or.all.of.your.electric.utility.bill?"
                                                            ,"For.what.share.does.your.household.receive.assistance?"))]
colnames(item127.dat) <- c("Financial.Assistance", "Percent.Assistance", "Remove", "CK_Cadmus_ID")
item127.dat0 <- item127.dat[which(colnames(item127.dat) != "Remove")]

#merge together analysis data with cleaned RBSA data
item127.dat1 <- left_join(rbsa.dat, item127.dat0, by = "CK_Cadmus_ID")
item127.dat1$count <- 1

unique(item127.dat1$Financial.Assistance)

item127.dat2 <- item127.dat1[which(!(is.na(item127.dat1$Financial.Assistance))),]

item127.dat2$Percent.Assistance[which(is.na(item127.dat2$Percent.Assistance))] <- "No Utility Bill Assistance"

#summarise by state
# by percent assistance
item127.state1 <- summarise(group_by(item127.dat2, BuildingType, State, Percent.Assistance)
                            ,Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID)))
# across percent assistance
item127.state2 <- summarise(group_by(item127.dat2, BuildingType, State)
                            ,Percent.Assistance = "Total"
                            ,Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID)))
#summarise by region
# by percent assistance
item127.region1 <- summarise(group_by(item127.dat2, BuildingType, Percent.Assistance)
                            , State = "Region"
                            ,Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID)))
# across percent assistance
item127.region2 <- summarise(group_by(item127.dat2, BuildingType)
                            ,State = "Region"
                            ,Percent.Assistance = "Total"
                            ,Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID)))

item127.merge <- rbind.data.frame(item127.state1, item127.state2, item127.region1, item127.region2, stringsAsFactors = F)

item127.tot.count <- rbind.data.frame(item127.state2, item127.region2, stringsAsFactors = F)
item127.tot.count <- item127.tot.count[which(colnames(item127.tot.count) %in% c("BuildingType","State", "Count", "SampleSize"))]
colnames(item127.tot.count) <- c("BuildingType","State", "Total.Count", "Denom.SampleSize")

item127.final <- left_join(item127.merge, item127.tot.count, by = c("BuildingType","State"))
item127.final$Percent <- item127.final$Count / item127.final$Total.Count
item127.final$SE <- sqrt(item127.final$Percent * (1 - item127.final$Percent) / item127.final$Denom.SampleSize)

item127.cast <- dcast(setDT(item127.final)
                      ,formula = BuildingType + Percent.Assistance ~ State
                      ,value.var = c("Percent", "SE", "SampleSize"))


item127.table <- data.frame("BuildingType" = item127.cast$BuildingType
                            ,"Percent.Assistance" = item127.cast$Percent.Assistance
                            ,"Percent_ID" = NA#item127.cast$Percent_ID
                            ,"SE_ID" = NA#item127.cast$SE_ID
                            ,"Percent_MT" = item127.cast$Percent_MT
                            ,"SE_MT" = item127.cast$SE_MT
                            ,"Percent_OR" = NA#item127.cast$Percent_OR
                            ,"SE_OR" = NA#item127.cast$SE_OR
                            ,"Percent_WA" = item127.cast$Percent_WA
                            ,"SE_WA" = item127.cast$SE_WA
                            ,"Percent_Region" = item127.cast$Percent_Region
                            ,"SE_Region" = item127.cast$SE_Region
                            ,"SampleSize" = item127.cast$SampleSize_Region)

item127.table1 <- item127.table[which(item127.table$BuildingType %in% c("Single Family", "Manufactured")),]
