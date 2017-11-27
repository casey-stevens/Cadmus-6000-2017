#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))




##############################################################################################################################
#
# Prep for items 38 - 40
#
##############################################################################################################################
mech.dat.sub <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"))]

mech.dat.sub1 <- left_join(rbsa.dat, mech.dat.sub, by = "CK_Cadmus_ID")
length(unique(mech.dat.sub1$CK_Cadmus_ID))

mech.dat.sub2 <- mech.dat.sub1[which(!is.na(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial | mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial)),]
mech.dat.sub3 <- mech.dat.sub2[which(!is.na(mech.dat.sub2$MECH_Blower_DOOR_P25_CFM_FirstTrial | mech.dat.sub2$MECH_Blower_DOOR_P25_CFM_SecondTrial)),]
mech.dat.sub4 <- mech.dat.sub3[which(!is.na(mech.dat.sub3$MECH_Blower_DOOR_P50_HousePressure_FirstTrial | mech.dat.sub3$MECH_Blower_DOOR_P50_HousePressure_SecondTrial)),]
mech.dat.sub5 <- mech.dat.sub4[which(!is.na(mech.dat.sub4$MECH_Blower_DOOR_P25_HousePressure_FirstTrial | mech.dat.sub4$MECH_Blower_DOOR_P25_HousePressure_SecondTrial)),]

mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_FirstTrial          <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_FirstTrial          <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_FirstTrial  <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_FirstTrial  <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_SecondTrial         <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_SecondTrial         <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_SecondTrial))

length(unique(mech.dat.sub5$CK_Cadmus_ID))

##############################################################################################################################
# End Prep
##############################################################################################################################





#############################################################################################
#Item 38: AVERAGE BLOWER DOOR AIR FLOW BY STATE  (SF table 45, MH table 27)
#############################################################################################
item38.dat  <- mech.dat.sub5
item38.dat$Flow.Exponent.FirstTrial  <- log(item38.dat$MECH_Blower_DOOR_P50_CFM_FirstTrial / item38.dat$MECH_Blower_DOOR_P25_CFM_FirstTrial) / log(item38.dat$MECH_Blower_DOOR_P50_HousePressure_FirstTrial / item38.dat$MECH_Blower_DOOR_P25_HousePressure_FirstTrial)
item38.dat$Flow.Exponent.SecondTrial <- log(item38.dat$MECH_Blower_DOOR_P50_CFM_SecondTrial / item38.dat$MECH_Blower_DOOR_P25_CFM_SecondTrial) / log(item38.dat$MECH_Blower_DOOR_P50_HousePressure_SecondTrial / item38.dat$MECH_Blower_DOOR_P25_HousePressure_SecondTrial)



######################################
#Pop and Sample Sizes for weights
######################################
item38.data <- weightedData(item38.dat[which(colnames(item38.dat) %notin% c("MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                              ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                              ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                              ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                              ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                              ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                              ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                              ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                              ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                              ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                              ,"Flow.Exponent.FirstTrial"
                                                                              ,"Flow.Exponent.SecondTrial"))])

item38.data <- left_join(item38.data, item38.dat[which(colnames(item38.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                                     ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                                     ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                                     ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                                     ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                                     ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                                     ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                                     ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                                     ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                                     ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                                     ,"Flow.Exponent.FirstTrial"
                                                                                     ,"Flow.Exponent.SecondTrial"))])
item38.data$count <- 1


######################
# weighted analysis
######################



# ##  Write out confidence/precision info
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
write.xlsx(item38.dat, paste(filepathCleaningDocs, "Insulation Exports", paste("blower.door.flow.exponents ", rundate, ".xlsx", sep = ""), sep="/"),
           append = T, row.names = F, showNA = F)










#############################################################################################
#Item 39: AVERAGE BLOWER DOOR AIR FLOW BY STATE  (SF table 46, MH table 28)
#############################################################################################
item39.dat  <- mech.dat.sub5
item39.dat$ACH50 <- item39.dat$MECH_Blower_DOOR_P50_CFM_FirstTrial * 60 / 


######################################
#Pop and Sample Sizes for weights
######################################
item39.data <- weightedData(item39.dat[which(colnames(item39.dat) %notin% c("MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                            ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                            ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                            ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                            ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                            ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                            ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                            ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                            ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                            ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                            ,"ACH50"))])

item39.data <- left_join(item39.data, item39.dat[which(colnames(item39.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                                   ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                                   ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                                   ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                                   ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                                   ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                                   ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                                   ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                                   ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                                   ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                                   ,"ACH50"))])
item39.data$count <- 1


######################
# weighted analysis
######################
