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
#Item 82: DISTRIBUTION OF REFRIGERATORS BY TYPE (SF table 89, MH table 70)
#############################################################################################
#subset to columns needed for analysis
item82.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,""
                                                                   ,""
                                                                   ,""
                                                                   ,""))]
item82.dat$count <- 1

item82.dat0 <- item82.dat[which(item82.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item82.dat1 <- left_join(item82.dat0, rbsa.dat, by = "CK_Cadmus_ID")










#############################################################################################
#Item 83: AVERAGE REFRIGERATOR VOLUME BY TYPE (SF table 90, MH table 71)
#############################################################################################
#subset to columns needed for analysis
item83.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                   ,"Refrigerator/Freezer.Size"
                                                                   ,""
                                                                   ,""))]
item83.dat$count <- 1

item83.dat0 <- item83.dat[which(item83.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item83.dat1 <- left_join(item83.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item83.dat2 <- item83.dat1[which(item83.dat1$Type == "Refrigerator"),]











#############################################################################################
#Item 84: DISTRIBUTION OF FREEZERS BY TYPE IN HOMES (SF table 91, MH table 72)
#############################################################################################
#subset to columns needed for analysis
item84.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                   ,"Refrigerator/Freezer.Size"
                                                                   ,""
                                                                   ,""))]
item84.dat$count <- 1

item84.dat0 <- item84.dat[which(item84.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item84.dat1 <- left_join(item84.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item84.dat2 <- item84.dat1[which(item84.dat1$Type == "Refrigerator"),]












#############################################################################################
#Item 85: AVERAGE FREEZER VOLUME BY TYPE (SF table 92, MH table 73)
#############################################################################################
#subset to columns needed for analysis
item85.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                   ,"Refrigerator/Freezer.Size"
                                                                   ,""
                                                                   ,""))]
item85.dat$count <- 1

item85.dat0 <- item85.dat[which(item85.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item85.dat1 <- left_join(item85.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item85.dat2 <- item85.dat1[which(item85.dat1$Type == "Refrigerator"),]
