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
item140.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Did.you.receive.a.rebate.or.incentive.from.your.utility.for.making.any.energy-efficiency.improvements.or.installing.energy-efficiency.equipment.in.the.last.two.years?"
                                                            ,""))]

colnames(item140.dat) <- c("Reporting", "CK_Cadmus_ID")

item140.dat1   <- item140.dat[which(!(item140.dat$Reporting %in% c(NA, "Don't know", "Prefer not to say"))),]
unique(item140.dat1$Reporting)

item140.dat2 <- left_join(rbsa.dat, item140.dat1, by = "CK_Cadmus_ID")
item140.dat2$Reporting.Count <- 0
item140.dat2$Reporting.Count[which(item140.dat2$Reporting == "Yes")] <- 1
item140.dat2$count <- 1

#summarise by state
item140.state <- summarise(group_by(item140.dat2, BuildingType, State)
                           ,Percent = sum(Reporting.Count) / sum(count)
                           ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                           ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across states
#by reporting
item140.region <- summarise(group_by(item140.dat2, BuildingType)
                            ,State = "Region"
                            ,Percent = sum(Reporting.Count) / sum(count)
                            ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                            ,SampleSize = length(unique(CK_Cadmus_ID)))


item140.final <- rbind.data.frame(item140.state, item140.region, stringsAsFactors = F)


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

colnames(item141.dat) <- c("CK_Cadmus_ID", "Reporting.tax")
#remove any repeat header rows from exporting
item141.dat0 <- item141.dat[which(item141.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item141.dat1   <- item141.dat0[which(item141.dat0$Reporting.tax %in% c("Yes", "No")),]

item141.dat2 <- left_join(rbsa.dat, item141.dat1, by = "CK_Cadmus_ID")
item141.dat2$count <- 1
item141.dat2$Reporting.tax.count <- 0
item141.dat2$Reporting.tax.count[which(item141.dat2$Reporting.tax == "Yes")] <- 1


#summarise by state
#by Reporting.tax
item141.state <- summarise(group_by(item141.dat2, BuildingType, State)
                           ,Percent = sum(Reporting.tax.count) / sum(count)
                           ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                           ,SampleSize = length(unique(CK_Cadmus_ID)))


#summarise across states
#by Reporting.tax
item141.region <- summarise(group_by(item141.dat2, BuildingType)
                            ,State = "Region"
                            ,Percent = sum(Reporting.tax.count) / sum(count)
                            ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                            ,SampleSize = length(unique(CK_Cadmus_ID)))


item141.final <- rbind.data.frame(item141.state, item141.region, stringsAsFactors = F)


#subset to only relevant buildingtypes
item141.table <- item141.final[which(item141.final$BuildingType %in% c("Single Family", "Manufactured")),]













#############################################################################################
#Item 142: PERCENTAGE OF HOUSEHOLDS REPORTING USE OF BOTH UTILITY AND TAX CREDIT CONSERVATION PROGRAMS (SF table 149, MH table 124)
#############################################################################################
item141.tmp <- item141.dat2[which(colnames(item141.dat2) %in% c("CK_Cadmus_ID"
                                                                ,"Reporting.tax"
                                                                ,"Reporting.tax.count"))]
item142.dat <- left_join(item140.dat2, item141.tmp, by = "CK_Cadmus_ID")

item142.dat$Total.Reporting.Count <- 0
item142.dat$Total.Reporting.Count[which(item142.dat$Reporting.Count == 1 & item142.dat$Reporting.tax.count == 1)] <- 1

unique(item142.dat$Total.Reporting.Count)

#summarise by state
#by Reporting.tax
item142.state <- summarise(group_by(item142.dat, BuildingType, State)
                           ,Percent = sum(Total.Reporting.Count) / sum(count)
                           ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                           ,SampleSize = length(unique(CK_Cadmus_ID)))


#summarise across states
#by Reporting.tax
item142.region <- summarise(group_by(item142.dat, BuildingType)
                            ,State = "Region"
                            ,Percent = sum(Total.Reporting.Count) / sum(count)
                            ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                            ,SampleSize = length(unique(CK_Cadmus_ID)))


item142.final <- rbind.data.frame(item142.state, item142.region, stringsAsFactors = F)


#subset to only relevant buildingtypes
item142.table <- item142.final[which(item142.final$BuildingType %in% c("Single Family", "Manufactured")),]
