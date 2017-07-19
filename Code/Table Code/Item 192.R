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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
#Item 192: CROSSOVER DUCT CONDITION IN MULTI-SECTION HOMES (MH TABLE 46)
#############################################################################################
#subset to columns needed for analysis
item192.dat <- unique(mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                           ,"MECH_Ducting_DUCTS_CrossoverCondition"
                                                                           ,"MECH_Ducting_DUCTS_DuctCrossoverPresent_Y_N"
                                                                           , ""))])

#remove any repeat header rows from exporting
item192.dat0 <- item192.dat[which(item192.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item192.dat1 <- left_join(item192.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#subset to only crossover present == Yes or No
item192.dat2 <- item192.dat1[which(item192.dat1$MECH_Ducting_DUCTS_DuctCrossoverPresent_Y_N %in% c("Yes", "No")),]





#############################################################################################
# Analysis (summarise by unit type (BuildingTypeXX))
#############################################################################################
item192.dat2$count <- 1
item192.dat2$DuctCount <- 0
item192.dat2$DuctCount[which(item192.dat2$MECH_Ducting_DUCTS_DuctCrossoverPresent_Y_N == "Yes")] <- 1

#summarise by unit type (BuildingTypeXX)
item192.sum1 <- summarise(group_by(item192.dat2, BuildingType, BuildingTypeXX)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Percent = sum(DuctCount) / sum(count)
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#summarise across unit types (BuildingTypeXX)
item192.sum2 <- summarise(group_by(item192.dat2, BuildingType)
                          ,BuildingTypeXX = "All Types"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Percent = sum(DuctCount) / sum(count)
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#merge by and across unit type info
item192.final <- rbind.data.frame(item192.sum1, item192.sum2, stringsAsFactors = F)

#Put columns in correct order
item192.table <- data.frame("BuildingType" = item192.final$BuildingType
                            ,"Unit.Type" = item192.final$BuildingTypeXX
                            ,"Percent" = item192.final$Percent
                            ,"SE" = item192.final$SE
                            ,"SampleSize" = item192.final$SampleSize)

#subset to relevant building types
item192.table1 <- item192.table[which(item192.table$BuildingType %in% c("Manufactured")),]











