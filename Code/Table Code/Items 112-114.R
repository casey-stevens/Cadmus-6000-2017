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
#Item 112: AVERAGE NUMBER OF SET-TOP BOXES PER HOME BY STATE (SF table 119, MH table 94)
#############################################################################################
#subset to columns needed for analysis
item112.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"TV.Set.Top.Box"
                                                                    ,"STB.Records?"))]
item112.dat$count <- 1

#remove any repeat header rows from exporting
item112.dat0 <- item112.dat[which(item112.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item112.dat1 <- left_join(item112.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#subset to set.top.box not NA
item112.dat2 <- item112.dat1[which(item112.dat1$TV.Set.Top.Box == "Yes"),]

#summarise by site
item112.dat3 <- summarise(group_by(item112.dat2, CK_Cadmus_ID, BuildingType, State)
                          ,Site.Count = sum(count))

#summarise by state
item112.state <- summarise(group_by(item112.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Site.Count)
                           ,SE = sd(Site.Count) / sqrt(SampleSize))

#summarise by region
item112.region <- summarise(group_by(item112.dat3, BuildingType)
                            ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Site.Count)
                           ,SE = sd(Site.Count) / sqrt(SampleSize))

item112.final <- rbind.data.frame(item112.state, item112.region, stringsAsFactors = F)

item112.table <- data.frame("BuildingType" = item112.final$BuildingType
                            ,"State" = item112.final$State
                            ,"Mean" = item112.final$Mean
                            ,"SE" = item112.final$SE
                            ,"SampleSize" = item112.final$SampleSize)
item112.table1 <- item112.table[which(item112.table$BuildingType %in% c("Single Family", "Manufactured")),]










#############################################################################################
#Item 113: PERCENTAGE OF HOMES WITH SET-TOP BOXES (SF table 120, MH table 95)
#############################################################################################
#subset to columns needed for analysis
item113.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"TV.Set.Top.Box"
                                                                    ,"STB.Records?"))]
item113.dat$count <- 1

#remove any repeat header rows from exporting
item113.dat0 <- item113.dat[which(item113.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item113.dat1 <- left_join(item113.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#subset to set.top.box not NA
item113.dat2 <- item113.dat1[which(item113.dat1$TV.Set.Top.Box == "Yes"),]


#summarise by state
#obtain sample size total
item113.total <- summarise(group_by(item113.dat1, BuildingType, State)
                           ,SampleSize.Total = length(unique(CK_Cadmus_ID)))

#obtain sample size for only sites with set top boxes
item113.setTopBox <- summarise(group_by(item113.dat2, BuildingType, State)
                               ,SampleSize.STB = length(unique(CK_Cadmus_ID)))

item113.state <- left_join(item113.total, item113.setTopBox, by = c("BuildingType", "State"))


#summarise across states
#obtain sample size total
item113.total1 <- summarise(group_by(item113.dat1, BuildingType)
                            ,State = "Region"
                           ,SampleSize.Total = length(unique(CK_Cadmus_ID)))

#obtain sample size for only sites with set top boxes
item113.setTopBox1 <- summarise(group_by(item113.dat2, BuildingType)
                                ,State = "Region"
                               ,SampleSize.STB = length(unique(CK_Cadmus_ID)))

item113.region <- left_join(item113.total1, item113.setTopBox1, by = c("BuildingType", "State"))


item113.final <- rbind.data.frame(item113.state, item113.region, stringsAsFactors = F)
item113.final$Percent <- item113.final$SampleSize.STB / item113.final$SampleSize.Total
item113.final$SE <- sqrt(item113.final$Percent * (1 - item113.final$Percent) / item113.final$SampleSize.Total)

item113.table <- data.frame("BuildingType" = item113.final$BuildingType
                            ,"State" = item113.final$State
                            ,"Percent" = item113.final$Percent
                            ,"SE" = item113.final$SE
                            ,"SampleSize" = item113.final$SampleSize.Total
                            ,stringsAsFactors = F)

item113.table1 <- item113.table[which(item113.table$BuildingType %in% c("Single Family", "Manufactured")),]










#############################################################################################
#Item 114: PERCENTAGE OF SET-TOP BOXES WITH DVR CAPABILITY BY STATE (SF table 121, MH table 96)
#############################################################################################
#subset to columns needed for analysis
item114.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"TV.Set.Top.Box"
                                                                    ,"STB.Records?"))]
item114.dat$count <- 1

#remove any repeat header rows from exporting
item114.dat0 <- item114.dat[which(item114.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item114.dat1 <- left_join(item114.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#subset to set.top.box not NA
item114.dat2 <- item114.dat1[which(item114.dat1$TV.Set.Top.Box == "Yes"),]

#remove unknown or DP not asked for
item114.dat3 <- item114.dat2[which(item114.dat2$`STB.Records?` %in% c("Yes", "No")),]

#summarise by state
#count of total settopboxes
item114.state1 <- summarise(group_by(item114.dat2, BuildingType, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Total.Count = sum(count))
#count of settopboxes with DVR capabilities
item114.state2 <- summarise(group_by(item114.dat3, BuildingType, State)
                          ,Count = sum(count))

item114.state <- left_join(item114.state1, item114.state2, by = c("BuildingType", "State"))

#summarise across states
#count of total settopboxes
item114.region1 <- summarise(group_by(item114.dat2, BuildingType)
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,State = "Region"
                            ,Total.Count = sum(count))
#count of settopboxes with DVR capabilities
item114.region2 <- summarise(group_by(item114.dat3, BuildingType)
                            ,State = "Region"
                            ,Count = sum(count))

item114.region <- left_join(item114.region1, item114.region2, by = c("BuildingType", "State"))

item114.final <- rbind.data.frame(item114.state, item114.region, stringsAsFactors = F)
item114.final$Percent <- item114.final$Count / item114.final$Total.Count
item114.final$SE <- sqrt(item114.final$Percent * (1 - item114.final$Percent) / item114.final$SampleSize)

item114.table <- data.frame("BuildingType" = item114.final$BuildingType
                            ,"State" = item114.final$State
                            ,"Percent" = item114.final$Percent
                            ,"SE" = item114.final$SE
                            ,"SampleSize" = item114.final$SampleSize)

item114.table1 <- item114.table[which(item114.table$BuildingType %in% c("Single Family", "Manufactured")),]
