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
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))




#############################################################################################
#
#For electrically heated homes
#
#############################################################################################
#subset to columns needed for analysis
item169.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item169.dat.11 <- item169.dat.1[which(item169.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item169.dat.12 <- item169.dat.11[which(item169.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item169.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item169.dat.12$Primary.Heating.System)
item169.dat.12$count <- 1

item169.dat.13 <- unique(item169.dat.12[which(item169.dat.12$Heating.Fuel == "Electric"),])

item169.sum <- summarise(group_by(item169.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item169.sum$Count <- 1
which(duplicated(item169.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item169.sum$Heating.Fuel)

item169.merge <- left_join(rbsa.dat, item169.sum)
item169.merge <- item169.merge[which(!is.na(item169.merge$Heating.Fuel)),]

item169.mechanical <- item169.merge














#############################################################################################
#Item 169: AVERAGE BLOWER DOOR AIR FLOW BY STATE, electrically heated homes  (SF table B-14)
#############################################################################################
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

mech.dat.sub1 <- left_join(item169.mechanical, mech.dat.sub, by = "CK_Cadmus_ID")
length(unique(mech.dat.sub1$CK_Cadmus_ID))

mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial          <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_FirstTrial          <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_FirstTrial  <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_FirstTrial  <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial         <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_SecondTrial         <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_SecondTrial))
mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_SecondTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_SecondTrial))


mech.dat.sub2 <- mech.dat.sub1[which(!is.na(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial | mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial)),]
mech.dat.sub3 <- mech.dat.sub2[which(!is.na(mech.dat.sub2$MECH_Blower_DOOR_P25_CFM_FirstTrial | mech.dat.sub2$MECH_Blower_DOOR_P25_CFM_SecondTrial)),]
mech.dat.sub4 <- mech.dat.sub3[which(!is.na(mech.dat.sub3$MECH_Blower_DOOR_P50_HousePressure_FirstTrial | mech.dat.sub3$MECH_Blower_DOOR_P50_HousePressure_SecondTrial)),]
mech.dat.sub5 <- mech.dat.sub4[which(!is.na(mech.dat.sub4$MECH_Blower_DOOR_P25_HousePressure_FirstTrial | mech.dat.sub4$MECH_Blower_DOOR_P25_HousePressure_SecondTrial)),]


length(unique(mech.dat.sub5$CK_Cadmus_ID))



item169.dat  <- mech.dat.sub5
item169.dat$Flow.Exponent.FirstTrial  <- log(item169.dat$MECH_Blower_DOOR_P50_CFM_FirstTrial / item169.dat$MECH_Blower_DOOR_P25_CFM_FirstTrial) / log(item169.dat$MECH_Blower_DOOR_P50_HousePressure_FirstTrial / item169.dat$MECH_Blower_DOOR_P25_HousePressure_FirstTrial)
item169.dat$Flow.Exponent.SecondTrial <- log(item169.dat$MECH_Blower_DOOR_P50_CFM_SecondTrial / item169.dat$MECH_Blower_DOOR_P25_CFM_SecondTrial) / log(item169.dat$MECH_Blower_DOOR_P50_HousePressure_SecondTrial / item169.dat$MECH_Blower_DOOR_P25_HousePressure_SecondTrial)

item169.dat1 <- item169.dat[which(item169.dat$Flow.Exponent.FirstTrial >= 0.5 & item169.dat$Flow.Exponent.FirstTrial <= 0.75 | item169.dat$Flow.Exponent.SecondTrial >= 0.5 & item169.dat$Flow.Exponent.SecondTrial <= 0.75 ),]
item169.dat1$Flow.Exponent <- item169.dat1$Flow.Exponent.FirstTrial
item169.dat1$Flow.Exponent[which(is.na(item169.dat1$Flow.Exponent))] <- 0
item169.dat1$P50_CFM       <- item169.dat1$MECH_Blower_DOOR_P50_CFM_FirstTrial
item169.dat1$P50_CFM[which(is.na(item169.dat1$P50_CFM))] <- 0
item169.dat1$P50_HousePressure       <- item169.dat1$MECH_Blower_DOOR_P50_HousePressure_FirstTrial
item169.dat1$P50_HousePressure[which(is.na(item169.dat1$P50_HousePressure))] <- 0

for (ii in 1:length(item169.dat1$Flow.Exponent)){
  if (item169.dat1$Flow.Exponent[ii] < 0.5 | item169.dat1$Flow.Exponent[ii] > 0.75){
    item169.dat1$Flow.Exponent[ii]     <- item169.dat1$Flow.Exponent.SecondTrial[ii]
    item169.dat1$P50_CFM[ii]           <- item169.dat1$MECH_Blower_DOOR_P50_CFM_SecondTrial[ii]
    item169.dat1$P50_HousePressure[ii] <- item169.dat1$MECH_Blower_DOOR_P50_HousePressure_SecondTrial[ii]
  }
}

item169.dat1$CFM50 <- item169.dat1$P50_CFM * (50 / abs(item169.dat1$P50_HousePressure)) ^ item169.dat1$Flow.Exponent


######################################
#Pop and Sample Sizes for weights
######################################
item169.data <- weightedData(item169.dat1[which(colnames(item169.dat1) %notin% c("MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
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
                                                                                 ,"Flow.Exponent.SecondTrial"
                                                                                 ,"Flow.Exponent"
                                                                                 ,"P50_CFM"
                                                                                 ,"P50_HousePressure"
                                                                                 ,"CFM50"
                                                                                 ,"Heating.Fuel"
                                                                                 ,"Count"))])

item169.data <- left_join(item169.data, item169.dat1[which(colnames(item169.dat1) %in% c("CK_Cadmus_ID"
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
                                                                                         ,"Flow.Exponent.SecondTrial"
                                                                                         ,"Flow.Exponent"
                                                                                         ,"P50_CFM"
                                                                                         ,"P50_HousePressure"
                                                                                         ,"CFM50"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"Count"))])
item169.data$count <- 1

item169.data$ACH50 <- item169.data$P50_CFM * 60 / item169.data$Conditioned.Volume

item169.data <- item169.data[which(item169.data$ACH50 != "Inf"),] #only 665/961 have a recorded conditioned floor volume

######################
# weighted analysis
######################
item169.final <- mean_one_group(CustomerLevelData = item169.data
                               ,valueVariable = 'ACH50'
                               ,byVariable = 'State'
                               ,aggregateRow = "Region")

item169.final.SF <- item169.final[which(item169.final$BuildingType == "Single Family")
                                ,which(colnames(item169.final) %notin% c("BuildingType"))]
exportTable(item169.final.SF, "SF", "Table B-14", weighted = TRUE)

######################
# unweighted analysis
######################
item169.final <- mean_one_group_unweighted(CustomerLevelData = item169.data
                                          ,valueVariable = 'ACH50'
                                          ,byVariable = 'State'
                                          ,aggregateRow = "Region")

item169.final.SF <- item169.final[which(item169.final$BuildingType == "Single Family")
                                ,which(colnames(item169.final) %notin% c("BuildingType"))]
exportTable(item169.final.SF, "SF", "Table B-14", weighted = FALSE)
