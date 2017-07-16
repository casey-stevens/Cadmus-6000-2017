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
#Item 179: PERCENT OF HOMES WITH REPLACEMENT WINDOWS BY STATE (MH TABLE 22)
#############################################################################################
#subset to columns needed for analysis
item179.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsConversation_ReplacementWindows_Y_N"
                                                                                     ,""))])
colnames(item179.dat) <- c("CK_Cadmus_ID", "Replacement_Windows")

#remove any repeat header rows from exporting
item179.dat0 <- item179.dat[which(item179.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item179.dat1 <- left_join(item179.dat0, rbsa.dat, by = "CK_Cadmus_ID")
unique(item179.dat1$Replacement_Windows)

item179.dat2 <- item179.dat1[which(!(is.na(item179.dat1$Replacement_Windows))),]

item179.dat2$Replaced_Window_ind <- item179.dat2$Replacement_Windows
item179.dat2$Replaced_Window_ind[which(item179.dat2$Replacement_Windows == "Yes")] <- 1
item179.dat2$Replaced_Window_ind[which(item179.dat2$Replacement_Windows == "No")] <- 0
unique(item179.dat2$Replaced_Window_ind)
item179.dat2$Replaced_Window_ind <- as.numeric(as.character(item179.dat2$Replaced_Window_ind))

item179.dat2$count <- 1
# str(item179.dat2)

#summarise by state
item179.state <- summarise(group_by(item179.dat2, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Percent = sum(Replaced_Window_ind) / sum(count)
                           ,SE = sqrt(Percent * (1 - Percent) / SampleSize))
#summarise across states
item179.region <- summarise(group_by(item179.dat2, BuildingType)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Percent = sum(Replaced_Window_ind) / sum(count)
                           ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

item179.final <- rbind.data.frame(item179.state, item179.region, stringsAsFactors = F)

item179.table <- data.frame("BuildingType" = item179.final$BuildingType
                            ,"State" = item179.final$State
                            ,"Percent" = item179.final$Percent
                            ,"SE" = item179.final$SE
                            ,"SampleSize" = item179.final$SampleSize)
item179.table1 <- item179.table[which(item179.table$BuildingType == "Manufactured"),]
