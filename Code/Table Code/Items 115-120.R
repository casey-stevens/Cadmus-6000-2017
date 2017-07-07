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
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))



#############################################################################################
#Item 115: PERCENTAGE OF HOMES WITH GAMING SYSTEMS (SF table 122, MH table 97)
#############################################################################################
#subset to columns needed for analysis
item115.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item115.dat$count <- 1

#remove any repeat header rows from exporting
item115.dat0 <- item115.dat[which(item115.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item115.dat1 <- left_join(item115.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item115.dat1$Type)
#subset to only Game COnsoles

item115.dat2 <- item115.dat1[which(item115.dat1$Type == "Game Console"),]

#summarise by states
#total sample size info
item115.state1 <- summarise(group_by(item115.dat1, BuildingType, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID)))
#total sample size info
item115.state2 <- summarise(group_by(item115.dat2, BuildingType, State)
                            ,Game.Count = length(unique(CK_Cadmus_ID)))

item115.state <- left_join(item115.state1, item115.state2, by = c("BuildingType", "State"))

#summarise across states
#total sample size info
item115.region1 <- summarise(group_by(item115.dat1, BuildingType)
                             ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID)))
#total sample size info
item115.region2 <- summarise(group_by(item115.dat2, BuildingType)
                             ,State = "Region"
                            ,Game.Count = length(unique(CK_Cadmus_ID)))
item115.region <- left_join(item115.region1, item115.region2, by = c("BuildingType", "State"))

# row bind state and region info
item115.final <- rbind.data.frame(item115.state, item115.region, stringsAsFactors = F)
item115.final$Percent <- item115.final$Game.Count / item115.final$SampleSize
item115.final$SE <- sqrt(item115.final$Percent * (1 - item115.final$Percent) / item115.final$SampleSize)

item115.table <- data.frame("BuildingType" = item115.final$BuildingType
                            ,"State" = item115.final$State
                            ,"Percent" = item115.final$Percent
                            ,"SE" = item115.final$SE
                            ,"SampleSize" = item115.final$SampleSize)
item115.table1 <- item115.table[which(item115.table$BuildingType %in% c("Single Family", "Manufactured")),]













#############################################################################################
#Item 116: AVERAGE NUMBER OF GAMING SYSTEMS PER HOME WITH GAMING SYSTEMS (SF table 123, MH table 98)
#############################################################################################
#subset to columns needed for analysis
item116.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item116.dat$count <- 1

#remove any repeat header rows from exporting
item116.dat0 <- item116.dat[which(item116.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item116.dat1 <- left_join(item116.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item116.dat1$Type)
#subset to only Game COnsoles

item116.dat2 <- item116.dat1[which(item116.dat1$Type == "Game Console"),]


#total gaming systems by site
item116.dat3 <- summarise(group_by(item116.dat2, CK_Cadmus_ID, BuildingType, State)
                          ,Site.Count = sum(count))

##average by state
item116.state <- summarise(group_by(item116.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Site.Count)
                           ,SE = sd(Site.Count) / sqrt(SampleSize))

#average across states
item116.region <- summarise(group_by(item116.dat3, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(Site.Count)
                            ,SE = sd(Site.Count) / sqrt(SampleSize))

item116.final <- rbind.data.frame(item116.state, item116.region, stringsAsFactors = F)

item116.table <- data.frame("BuildingType" = item116.final$BuildingType
                            ,"State" = item116.final$State
                            ,"Mean" = item116.final$Mean
                            ,"SE" = item116.final$SE
                            ,"SampleSize" = item116.final$SampleSize)
item116.table1 <- item116.table[which(item116.table$BuildingType %in% c("Single Family", "Manufactured")),]












#############################################################################################
#Item 117: AVERAGE NUMBER OF COMPUTERS PER HOME WITH COMPUTERS (SF table 124, MH table 99)
#############################################################################################
#subset to columns needed for analysis
item117.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item117.dat$count <- 1

#remove any repeat header rows from exporting
item117.dat0 <- item117.dat[which(item117.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item117.dat1 <- left_join(item117.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item117.dat1$Type)
#subset to only Game COnsoles

item117.dat2 <- item117.dat1[which(item117.dat1$Type %in% c("Laptop", "Desktop")),]


#total computers by site
item117.dat3 <- summarise(group_by(item117.dat2, CK_Cadmus_ID, BuildingType, State)
                          ,Site.Count = sum(count))

##average by state
item117.state <- summarise(group_by(item117.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Site.Count)
                           ,SE = sd(Site.Count) / sqrt(SampleSize))

#average across states
item117.region <- summarise(group_by(item117.dat3, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(Site.Count)
                            ,SE = sd(Site.Count) / sqrt(SampleSize))

item117.final <- rbind.data.frame(item117.state, item117.region, stringsAsFactors = F)

item117.table <- data.frame("BuildingType" = item117.final$BuildingType
                            ,"State" = item117.final$State
                            ,"Mean" = item117.final$Mean
                            ,"SE" = item117.final$SE
                            ,"SampleSize" = item117.final$SampleSize)
item117.table1 <- item117.table[which(item117.table$BuildingType %in% c("Single Family", "Manufactured")),]















#############################################################################################
#Item 118: PERCENTAGE OF HOMES WITH COMPUTERS BY STATE (SF table 125, MH table 100)
#############################################################################################
#subset to columns needed for analysis
item118.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item118.dat$count <- 1

#remove any repeat header rows from exporting
item118.dat0 <- item118.dat[which(item118.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item118.dat1 <- left_join(item118.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item118.dat1$Type)
#subset to only Game COnsoles

item118.dat2 <- item118.dat1[which(item118.dat1$Type %in% c("Desktop", "Laptop")),]

#summarise by states
#total sample size info
item118.state1 <- summarise(group_by(item118.dat1, BuildingType, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID)))
#total sample size info
item118.state2 <- summarise(group_by(item118.dat2, BuildingType, State)
                            ,Comp.Count = length(unique(CK_Cadmus_ID)))

item118.state <- left_join(item118.state1, item118.state2, by = c("BuildingType", "State"))

#summarise across states
#total sample size info
item118.region1 <- summarise(group_by(item118.dat1, BuildingType)
                             ,State = "Region"
                             ,SampleSize = length(unique(CK_Cadmus_ID)))
#total sample size info
item118.region2 <- summarise(group_by(item118.dat2, BuildingType)
                             ,State = "Region"
                             ,Comp.Count = length(unique(CK_Cadmus_ID)))
item118.region <- left_join(item118.region1, item118.region2, by = c("BuildingType", "State"))

# row bind state and region info
item118.final <- rbind.data.frame(item118.state, item118.region, stringsAsFactors = F)
item118.final$Percent <- item118.final$Comp.Count / item118.final$SampleSize
item118.final$SE <- sqrt(item118.final$Percent * (1 - item118.final$Percent) / item118.final$SampleSize)

item118.table <- data.frame("BuildingType" = item118.final$BuildingType
                            ,"State" = item118.final$State
                            ,"Percent" = item118.final$Percent
                            ,"SE" = item118.final$SE
                            ,"SampleSize" = item118.final$SampleSize)
item118.table1 <- item118.table[which(item118.table$BuildingType %in% c("Single Family", "Manufactured")),]













#############################################################################################
#Item 119: AVERAGE NUMBER OF AUDIO SYSTEMS PER HOME BY STATE (SF table 126, MH table 101)
#############################################################################################
#subset to columns needed for analysis
item119.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item119.dat$count <- 1

#remove any repeat header rows from exporting
item119.dat0 <- item119.dat[which(item119.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item119.dat1 <- left_join(item119.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item119.dat1$Type)
#subset to only Game COnsoles

item119.dat2 <- item119.dat1[which(item119.dat1$Type %in% c("Audio Equipment")),]


#total computers by site
item119.dat3 <- summarise(group_by(item119.dat2, CK_Cadmus_ID, BuildingType, State)
                          ,Site.Count = sum(count))

##average by state
item119.state <- summarise(group_by(item119.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Site.Count)
                           ,SE = sd(Site.Count) / sqrt(SampleSize))

#average across states
item119.region <- summarise(group_by(item119.dat3, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(Site.Count)
                            ,SE = sd(Site.Count) / sqrt(SampleSize))

item119.final <- rbind.data.frame(item119.state, item119.region, stringsAsFactors = F)

item119.table <- data.frame("BuildingType" = item119.final$BuildingType
                            ,"State" = item119.final$State
                            ,"Mean" = item119.final$Mean
                            ,"SE" = item119.final$SE
                            ,"SampleSize" = item119.final$SampleSize)
item119.table1 <- item119.table[which(item119.table$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
#Item 120: AVERAGE NUMBER OF SUBWOOFERS PER HOME BY STATE (SF table 127, MH table 102)
#############################################################################################
#subset to columns needed for analysis
item120.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"Contains.Suboofer"))]
item120.dat$count <- 1

#remove any repeat header rows from exporting
item120.dat0 <- item120.dat[which(item120.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item120.dat1 <- left_join(item120.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item120.dat1$Type)
#subset to only Game COnsoles

item120.dat2 <- item120.dat1[which(item120.dat1$Contains.Suboofer %in% c("Yes")),]
unique(item120.dat2$Contains.Suboofer)

#total subwoofers by site
item120.dat3 <- summarise(group_by(item120.dat2, CK_Cadmus_ID, BuildingType, State)
                          ,Site.Count = sum(count))

##average by state
item120.state <- summarise(group_by(item120.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Site.Count)
                           ,SE = sd(Site.Count) / sqrt(SampleSize))

#average across states
item120.region <- summarise(group_by(item120.dat3, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(Site.Count)
                            ,SE = sd(Site.Count) / sqrt(SampleSize))

item120.final <- rbind.data.frame(item120.state, item120.region, stringsAsFactors = F)

item120.table <- data.frame("BuildingType" = item120.final$BuildingType
                            ,"State" = item120.final$State
                            ,"Mean" = item120.final$Mean
                            ,"SE" = item120.final$SE
                            ,"SampleSize" = item120.final$SampleSize)
item120.table1 <- item120.table[which(item120.table$BuildingType %in% c("Single Family", "Manufactured")),]

