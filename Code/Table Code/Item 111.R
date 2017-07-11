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
#Item 111: AVERAGE PRIMARY TELEVISION ON-TIME HOURS PER DAY PER HOME BY STATE (SF table 118, MH table 93)
#############################################################################################
#subset to columns needed for analysis
item111.dat <- sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHomeUse_HowManyHoursPerDayIsThePrimaryTVOn"
                                                                    ,""))]
colnames(item111.dat) <- c("CK_Cadmus_ID", "TV_on_hours")
item111.dat$count <- 1

#remove any repeat header rows from exporting
item111.dat0 <- item111.dat[which(item111.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item111.dat1 <- left_join(item111.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#make tv hours to numeric
item111.dat1$TV_on_hours <- as.numeric(as.character(item111.dat1$TV_on_hours))
unique(item111.dat1$TV_on_hours)

#remove any NA in tv hours
item111.dat2 <- item111.dat1[which(!(is.na(item111.dat1$TV_on_hours))),]

#summarise by site
item111.sum <- summarise(group_by(item111.dat2, CK_Cadmus_ID, BuildingType, State)
                         ,Site.Mean = mean(TV_on_hours))

#by state
item111.state <- summarise(group_by(item111.sum, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Site.Mean)
                           ,SE = sd(Site.Mean) / sqrt(SampleSize))

#by region (across states)
item111.region <- summarise(group_by(item111.sum, BuildingType)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Site.Mean)
                           ,SE = sd(Site.Mean) / sqrt(SampleSize))

item111.final <- rbind.data.frame(item111.state, item111.region, stringsAsFactors = F)
item111.table <- item111.final[which(item111.final$BuildingType %in% c("Single Family", "Manufactured")),]
item111.table1 <- data.frame("BuildingType" = item111.table$BuildingType
                             ,"State" = item111.table$State
                             ,"Mean" = item111.table$Mean
                             ,"SE" = item111.table$SE
                             ,"SampleSize" = item111.table$SampleSize)
