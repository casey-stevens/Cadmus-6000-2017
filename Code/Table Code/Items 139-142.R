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





#############################################################################################
#Item 139: PERCENTAGE OF HOUSEHOLDS REPORTING RECENT SELF-FUNDED CONSERVATION BY STATE (SF table 146, MH table 121)
#############################################################################################
#subset to columns needed for analysis
item139.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsConversation_OnYourOwn_LastFewYears_Y_N"
                                                                                     ,""))])

colnames(item139.dat) <- c("CK_Cadmus_ID", "Reporting")
#remove any repeat header rows from exporting
item139.dat0 <- item139.dat[which(item139.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item139.dat0.5 <- item139.dat0[which(!(is.na(item139.dat0$Reporting))),]
item139.dat1   <- item139.dat0.5[which(item139.dat0.5$Reporting != "Unknown"),]


item139.dat2 <- left_join(item139.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item139.dat2$count <- 1

#summarise by state
#by reporting
item139.state1 <- summarise(group_by(item139.dat2, BuildingType, State, Reporting)
                           ,Count = sum(count))
#obtain sample sizes and total counts
item139.state2 <- summarise(group_by(item139.dat2, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Total.Count = sum(count))

item139.state <- left_join(item139.state1, item139.state2, by = c("BuildingType", "State"))


#summarise across states
#by reporting
item139.region1 <- summarise(group_by(item139.dat2, BuildingType, Reporting)
                            ,State = "Region"
                            ,Count = sum(count))
#obtain sample sizes and total counts
item139.region2 <- summarise(group_by(item139.dat2, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Total.Count = sum(count))

item139.region <- left_join(item139.region1, item139.region2, by = c("BuildingType", "State"))


item139.merge <- rbind.data.frame(item139.state, item139.region, stringsAsFactors = F)


item139.final <- item139.merge[which(item139.merge$Reporting == "Yes"),]
item139.final$Percent <- item139.final$Count / item139.final$Total.Count
item139.final$SE <- sqrt(item139.final$Percent * (1 - item139.final$Percent) / item139.final$SampleSize)

#put columns in correct order
item139.table <- data.frame("BuildingType" = item139.final$BuildingType
                            ,"State" = item139.final$State
                            ,"Percent" = item139.final$Percent
                            ,"SE" = item139.final$SE
                            ,"SampleSize" = item139.final$SampleSize)

#subset to only relevant buildingtypes
item139.table1 <- item139.table[which(item139.table$BuildingType %in% c("Single Family", "Manufactured")),]





#############################################################################################
#Item 140: PERCENTAGE OF HOUSEHOLDS REPORTING RECENT USE OF UTILITY CONSERVATION PROGRAMS BY STATE (SF table 147, MH table 122)
#############################################################################################
#subset to columns needed for analysis
item140.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsParticipate_Utility_Conservation_Program_LastTwoYears_Y_N"
                                                                                     ,""))])

colnames(item140.dat) <- c("CK_Cadmus_ID", "Reporting")
#remove any repeat header rows from exporting
item140.dat0 <- item140.dat[which(item140.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item140.dat0.5 <- item140.dat0[which(!(is.na(item140.dat0$Reporting))),]
item140.dat1   <- item140.dat0.5[which(item140.dat0.5$Reporting != "Unknown"),]


item140.dat2 <- left_join(item140.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item140.dat2$count <- 1

#summarise by state
#by reporting
item140.state1 <- summarise(group_by(item140.dat2, BuildingType, State, Reporting)
                            ,Count = sum(count))
#obtain sample sizes and total counts
item140.state2 <- summarise(group_by(item140.dat2, BuildingType, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Total.Count = sum(count))

item140.state <- left_join(item140.state1, item140.state2, by = c("BuildingType", "State"))


#summarise across states
#by reporting
item140.region1 <- summarise(group_by(item140.dat2, BuildingType, Reporting)
                             ,State = "Region"
                             ,Count = sum(count))
#obtain sample sizes and total counts
item140.region2 <- summarise(group_by(item140.dat2, BuildingType)
                             ,State = "Region"
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Total.Count = sum(count))

item140.region <- left_join(item140.region1, item140.region2, by = c("BuildingType", "State"))


item140.merge <- rbind.data.frame(item140.state, item140.region, stringsAsFactors = F)


item140.final <- item140.merge[which(item140.merge$Reporting == "Yes"),]
item140.final$Percent <- item140.final$Count / item140.final$Total.Count
item140.final$SE <- sqrt(item140.final$Percent * (1 - item140.final$Percent) / item140.final$SampleSize)

#put columns in correct order
item140.table <- data.frame("BuildingType" = item140.final$BuildingType
                            ,"State" = item140.final$State
                            ,"Percent" = item140.final$Percent
                            ,"SE" = item140.final$SE
                            ,"SampleSize" = item140.final$SampleSize)

#subset to only relevant buildingtypes
item140.table1 <- item140.table[which(item140.table$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
#Item 141: PERCENTAGE OF HOUSEHOLDS REPORTING USE OF CONSERVATION TAX CREDIT (SF table 148, MH table 123)
#############################################################################################
#subset to columns needed for analysis
item141.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsConversation_DidYouReceiveATaxCreditForThisWork_Y_N"
                                                                                     ,""))])

colnames(item141.dat) <- c("CK_Cadmus_ID", "Reporting")
#remove any repeat header rows from exporting
item141.dat0 <- item141.dat[which(item141.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item141.dat0.5 <- item141.dat0[which(!(is.na(item141.dat0$Reporting))),]
item141.dat1   <- item141.dat0.5[which(item141.dat0.5$Reporting != "Unknown"),]


item141.dat2 <- left_join(item141.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item141.dat2$count <- 1

#summarise by state
#by reporting
item141.state1 <- summarise(group_by(item141.dat2, BuildingType, State, Reporting)
                            ,Count = sum(count))
#obtain sample sizes and total counts
item141.state2 <- summarise(group_by(item141.dat2, BuildingType, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Total.Count = sum(count))

item141.state <- left_join(item141.state1, item141.state2, by = c("BuildingType", "State"))


#summarise across states
#by reporting
item141.region1 <- summarise(group_by(item141.dat2, BuildingType, Reporting)
                             ,State = "Region"
                             ,Count = sum(count))
#obtain sample sizes and total counts
item141.region2 <- summarise(group_by(item141.dat2, BuildingType)
                             ,State = "Region"
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Total.Count = sum(count))

item141.region <- left_join(item141.region1, item141.region2, by = c("BuildingType", "State"))


item141.merge <- rbind.data.frame(item141.state, item141.region, stringsAsFactors = F)


item141.final <- item141.merge[which(item141.merge$Reporting == "Yes"),]
item141.final$Percent <- item141.final$Count / item141.final$Total.Count
item141.final$SE <- sqrt(item141.final$Percent * (1 - item141.final$Percent) / item141.final$SampleSize)

#put columns in correct order
item141.table <- data.frame("BuildingType" = item141.final$BuildingType
                            ,"State" = item141.final$State
                            ,"Percent" = item141.final$Percent
                            ,"SE" = item141.final$SE
                            ,"SampleSize" = item141.final$SampleSize)

#subset to only relevant buildingtypes
item141.table1 <- item141.table[which(item141.table$BuildingType %in% c("Single Family", "Manufactured")),]













#############################################################################################
#Item 142: PERCENTAGE OF HOUSEHOLDS REPORTING USE OF BOTH UTILITY AND TAX CREDIT CONSERVATION PROGRAMS (SF table 149, MH table 124)
#############################################################################################
#subset to columns needed for analysis
item142.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsConversation_DidYouReceiveATaxCreditForThisWork_Y_N"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsParticipate_Utility_Conservation_Program_LastTwoYears_Y_N"))])

colnames(item142.dat) <- c("CK_Cadmus_ID", "Reporting.Utility", "Reporting.Tax.Credit")
#remove any repeat header rows from exporting
item142.dat0 <- item142.dat[which(item142.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

# item142.dat0.5 <- item142.dat0[which(!(is.na(item142.dat0$Reporting))),]
# item142.dat1   <- item142.dat0.5[which(item142.dat0.5$Reporting != "Unknown"),]


item142.dat2 <- left_join(item142.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item142.dat2$count <- 1

#summarise by state
#by reporting
item142.state1 <- summarise(group_by(item142.dat2, BuildingType, State, Reporting)
                            ,Count = sum(count))
#obtain sample sizes and total counts
item142.state2 <- summarise(group_by(item142.dat2, BuildingType, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Total.Count = sum(count))

item142.state <- left_join(item142.state1, item142.state2, by = c("BuildingType", "State"))


#summarise across states
#by reporting
item142.region1 <- summarise(group_by(item142.dat2, BuildingType, Reporting)
                             ,State = "Region"
                             ,Count = sum(count))
#obtain sample sizes and total counts
item142.region2 <- summarise(group_by(item142.dat2, BuildingType)
                             ,State = "Region"
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Total.Count = sum(count))

item142.region <- left_join(item142.region1, item142.region2, by = c("BuildingType", "State"))


item142.merge <- rbind.data.frame(item142.state, item142.region, stringsAsFactors = F)


item142.final <- item142.merge[which(item142.merge$Reporting == "Yes"),]
item142.final$Percent <- item142.final$Count / item142.final$Total.Count
item142.final$SE <- sqrt(item142.final$Percent * (1 - item142.final$Percent) / item142.final$SampleSize)

#put columns in correct order
item142.table <- data.frame("BuildingType" = item142.final$BuildingType
                            ,"State" = item142.final$State
                            ,"Percent" = item142.final$Percent
                            ,"SE" = item142.final$SE
                            ,"SampleSize" = item142.final$SampleSize)

#subset to only relevant buildingtypes
item142.table1 <- item142.table[which(item142.table$BuildingType %in% c("Single Family", "Manufactured")),]

