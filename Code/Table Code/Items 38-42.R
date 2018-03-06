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
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
mechanical.dat <- read.xlsx(mechanical.export)
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
                                                                   ,"MECH_Blower_DOOR_P25_FanPressure_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P25_FanPressure_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P50_FanPressure_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P50_FanPressure_SecondTrial"))]

mech.dat.sub1 <- left_join(rbsa.dat, mech.dat.sub, by = "CK_Cadmus_ID")
length(unique(mech.dat.sub1$CK_Cadmus_ID))
mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_FirstTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_SecondTrial))
mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_FirstTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_SecondTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_FirstTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_SecondTrial))

mech.dat.sub2 <- mech.dat.sub1[which(!is.na(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial | mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial)),]
mech.dat.sub3 <- mech.dat.sub2[which(!is.na(mech.dat.sub2$MECH_Blower_DOOR_P25_CFM_FirstTrial | mech.dat.sub2$MECH_Blower_DOOR_P25_CFM_SecondTrial)),]
mech.dat.sub4 <- mech.dat.sub3[which(!is.na(mech.dat.sub3$MECH_Blower_DOOR_P50_HousePressure_FirstTrial | mech.dat.sub3$MECH_Blower_DOOR_P50_HousePressure_SecondTrial)),]
mech.dat.sub5 <- mech.dat.sub4[which(!is.na(mech.dat.sub4$MECH_Blower_DOOR_P25_HousePressure_FirstTrial | mech.dat.sub4$MECH_Blower_DOOR_P25_HousePressure_SecondTrial)),]

mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_FirstTrial            <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_FirstTrial            <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_FirstTrial  <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_FirstTrial  <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_FirstTrial    <- abs(as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_FirstTrial)))
mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_FirstTrial    <- abs(as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_FirstTrial)))
mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_SecondTrial           <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_SecondTrial           <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_SecondTrial   <- abs(as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_SecondTrial)))
mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_SecondTrial   <- abs(as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_SecondTrial)))

length(unique(mech.dat.sub5$CK_Cadmus_ID))

mech.dat.sub6 <- mech.dat.sub5[which(mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_FirstTrial    >= 26 | 
                                       mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_SecondTrial >= 26 &
                                       mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_FirstTrial  >= 26 |
                                       mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_SecondTrial >= 26),]

##############################################################################################################################
# End Prep
##############################################################################################################################





#############################################################################################
#Item 38: AVERAGE BLOWER DOOR AIR FLOW BY STATE  (SF table 45, MH table 27)
#############################################################################################
item38.dat  <-mech.dat.sub6
item38.dat$Flow.Exponent.FirstTrial  <- log(item38.dat$MECH_Blower_DOOR_P50_CFM_FirstTrial / item38.dat$MECH_Blower_DOOR_P25_CFM_FirstTrial) / log(item38.dat$MECH_Blower_DOOR_P50_HousePressure_FirstTrial / item38.dat$MECH_Blower_DOOR_P25_HousePressure_FirstTrial)
item38.dat$Flow.Exponent.SecondTrial <- log(item38.dat$MECH_Blower_DOOR_P50_CFM_SecondTrial / item38.dat$MECH_Blower_DOOR_P25_CFM_SecondTrial) / log(item38.dat$MECH_Blower_DOOR_P50_HousePressure_SecondTrial / item38.dat$MECH_Blower_DOOR_P25_HousePressure_SecondTrial)

item38.dat1 <- item38.dat[which(item38.dat$Flow.Exponent.FirstTrial >= 0.5 & item38.dat$Flow.Exponent.FirstTrial <= 0.75 | item38.dat$Flow.Exponent.SecondTrial >= 0.5 & item38.dat$Flow.Exponent.SecondTrial <= 0.75 ),]
item38.dat1$Flow.Exponent <- item38.dat1$Flow.Exponent.FirstTrial
item38.dat1$Flow.Exponent[which(is.na(item38.dat1$Flow.Exponent))] <- 0
item38.dat1$P50_CFM       <- item38.dat1$MECH_Blower_DOOR_P50_CFM_FirstTrial
item38.dat1$P50_CFM[which(is.na(item38.dat1$P50_CFM))] <- 0
item38.dat1$P50_HousePressure       <- item38.dat1$MECH_Blower_DOOR_P50_HousePressure_FirstTrial
item38.dat1$P50_HousePressure[which(is.na(item38.dat1$P50_HousePressure))] <- 0

for (ii in 1:length(item38.dat1$Flow.Exponent)){
  if (item38.dat1$Flow.Exponent[ii] < 0.5 | item38.dat1$Flow.Exponent[ii] > 0.75){
    item38.dat1$Flow.Exponent[ii]     <- item38.dat1$Flow.Exponent.SecondTrial[ii]
    item38.dat1$P50_CFM[ii]           <- item38.dat1$MECH_Blower_DOOR_P50_CFM_SecondTrial[ii]
    item38.dat1$P50_HousePressure[ii] <- item38.dat1$MECH_Blower_DOOR_P50_HousePressure_SecondTrial[ii]
  }
}

item38.dat1$CFM50 <- item38.dat1$P50_CFM * (50 / abs(item38.dat1$P50_HousePressure)) ^ item38.dat1$Flow.Exponent
item38.dat2 <- item38.dat1[grep("site", item38.dat1$CK_Building_ID, ignore.case = T),]
# ##  Write out confidence/precision info
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(item38.dat1, paste(filepathCleaningDocs, "Insulation Exports", paste("blower.door.flow.exponents ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)


######################################
#Pop and Sample Sizes for weights
######################################
item38.data <- weightedData(item38.dat2[which(colnames(item38.dat2) %notin% c("MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                              ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                              ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                              ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                              ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                              ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                              ,"MECH_Blower_DOOR_P25_FanPressure_FirstTrial"
                                                                              ,"MECH_Blower_DOOR_P25_FanPressure_SecondTrial"
                                                                              ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                              ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                              ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                              ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                              ,"MECH_Blower_DOOR_P50_FanPressure_FirstTrial"
                                                                              ,"MECH_Blower_DOOR_P50_FanPressure_SecondTrial"
                                                                              ,"Flow.Exponent.FirstTrial"
                                                                              ,"Flow.Exponent.SecondTrial"
                                                                              ,"Flow.Exponent"
                                                                              ,"P50_CFM"
                                                                              ,"P50_HousePressure"
                                                                              ,"CFM50"))])

item38.data <- left_join(item38.data, item38.dat2[which(colnames(item38.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                                     ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                                     ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                                     ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                                     ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                                     ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                                     ,"MECH_Blower_DOOR_P25_FanPressure_FirstTrial"
                                                                                     ,"MECH_Blower_DOOR_P25_FanPressure_SecondTrial"
                                                                                     ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                                     ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                                     ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                                     ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                                     ,"MECH_Blower_DOOR_P50_FanPressure_FirstTrial"
                                                                                     ,"MECH_Blower_DOOR_P50_FanPressure_SecondTrial"
                                                                                     ,"Flow.Exponent.FirstTrial"
                                                                                     ,"Flow.Exponent.SecondTrial"
                                                                                     ,"Flow.Exponent"
                                                                                     ,"P50_CFM"
                                                                                     ,"P50_HousePressure"
                                                                                     ,"CFM50"))])
item38.data$count <- 1

#  Write out
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(item38.data, paste(filepathCleaningDocs, "Insulation Exports", paste("blower.door.calcs_v2 ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)

######################
# weighted analysis
######################
item38.final <- mean_one_group(CustomerLevelData = item38.data
                               ,valueVariable = 'CFM50'
                               ,byVariable    = 'State'
                               ,aggregateRow  = "Region")

item38.final.SF <- item38.final[which(item38.final$BuildingType == "Single Family")
                                ,which(colnames(item38.final) %notin% c("BuildingType"))]
# exportTable(item38.final.SF, "SF", "Table 45", weighted = TRUE)

item38.final.MH <- item38.final[which(item38.final$BuildingType == "Manufactured")
                                ,which(colnames(item38.final) %notin% c("BuildingType"))]
exportTable(item38.final.MH, "MH", "Table 27", weighted = TRUE)


######################
# unweighted analysis
######################
item38.final <- mean_one_group_unweighted(CustomerLevelData = item38.data
                               ,valueVariable = 'CFM50'
                               ,byVariable = 'State'
                               ,aggregateRow = "Region")

item38.final.SF <- item38.final[which(item38.final$BuildingType == "Single Family")
                                ,which(colnames(item38.final) %notin% c("BuildingType"))]
# exportTable(item38.final.SF, "SF", "Table 45", weighted = FALSE)

item38.final.MH <- item38.final[which(item38.final$BuildingType == "Manufactured")
                                ,which(colnames(item38.final) %notin% c("BuildingType"))]
exportTable(item38.final.MH, "MH", "Table 27", weighted = FALSE)









#############################################################################################
#Item 39: AVERAGE BLOWER DOOR AIR FLOW BY STATE  (SF table 46, MH table 28)
#############################################################################################
item39.dat  <- item38.data

item39.dat$ACH50 <- item39.dat$P50_CFM * 60 / item39.dat$Conditioned.Volume

item39.dat1 <- item39.dat[which(item39.dat$ACH50 != "Inf"),]

# #  Write out
# item39.dat.tmp <- item39.dat1[grep("SCL",item39.dat1$Territory),]
# item39.dat.tmp <- item39.dat.tmp[which(!is.na(item39.dat.tmp$HomeYearBuilt)),]
# item39.dat.tmp <- item39.dat.tmp[which(item39.dat.tmp$BuildingType == "Single Family"),]
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(item39.dat.tmp, paste(filepathCleaningDocs, "Insulation Exports", paste("blower.door.calcs_Andrew Grant ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)

######################
# weighted analysis
######################
item39.final <- mean_one_group(CustomerLevelData = item39.dat1
                               ,valueVariable = 'ACH50'
                               ,byVariable = 'State'
                               ,aggregateRow = "Region")

item39.final.SF <- item39.final[which(item39.final$BuildingType == "Single Family")
                                ,which(colnames(item39.final) %notin% c("BuildingType"))]
# exportTable(item39.final.SF, "SF", "Table 46", weighted = TRUE)

item39.final.MH <- item39.final[which(item39.final$BuildingType == "Manufactured")
                                ,which(colnames(item39.final) %notin% c("BuildingType"))]
exportTable(item39.final.MH, "MH", "Table 28", weighted = TRUE)

######################
# unweighted analysis
######################
item39.final <- mean_one_group_unweighted(CustomerLevelData = item39.dat1
                               ,valueVariable = 'ACH50'
                               ,byVariable = 'State'
                               ,aggregateRow = "Region")

item39.final.SF <- item39.final[which(item39.final$BuildingType == "Single Family")
                                ,which(colnames(item39.final) %notin% c("BuildingType"))]
# exportTable(item39.final.SF, "SF", "Table 46", weighted = FALSE)

item39.final.MH <- item39.final[which(item39.final$BuildingType == "Manufactured")
                                ,which(colnames(item39.final) %notin% c("BuildingType"))]
exportTable(item39.final.MH, "MH", "Table 28", weighted = FALSE)









#############################################################################################
#Item 40: AVERAGE BLOWER DOOR AIR FLOW BY HOME VINTAGE (SF table 47, MH table 29)
#############################################################################################
item40.dat  <- item38.dat1[which(!is.na(item38.dat1$HomeYearBuilt)),]

item40.dat$count <- 1


item40.dat$ACH50 <- item40.dat$P50_CFM * 60 / item40.dat$Conditioned.Volume

item40.dat1 <- item40.dat[which(item40.dat$ACH50 != "Inf"),] #only 883/959 have a recorded conditioned floor volume


#average within houses
item40.customer <- summarise(group_by(item40.dat1
                                     , CK_Cadmus_ID
                                     , HomeYearBuilt)
                            ,y_bar_ilk  = mean(ACH50)
                            ,y_ilk      = sum(ACH50)
                            ,m_ilk      = sum(count)
)

item40.merge <- left_join(rbsa.dat, item40.customer)
item40.merge <- item40.merge[which(!is.na(item40.merge$y_bar_ilk)),]

######################################
#Pop and Sample Sizes for weights
######################################
item40.data <- weightedData(item40.merge[which(colnames(item40.merge) %notin% c("MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                            ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                            ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                            ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                            ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                            ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                            ,"MECH_Blower_DOOR_P25_FanPressure_FirstTrial"
                                                                            ,"MECH_Blower_DOOR_P25_FanPressure_SecondTrial"
                                                                            ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                            ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                            ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                            ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                            ,"MECH_Blower_DOOR_P50_FanPressure_FirstTrial"
                                                                            ,"MECH_Blower_DOOR_P50_FanPressure_SecondTrial"
                                                                            ,"Flow.Exponent.FirstTrial"
                                                                            ,"Flow.Exponent.SecondTrial"
                                                                            ,"Flow.Exponent"
                                                                            ,"P50_CFM"
                                                                            ,"P50_HousePressure"
                                                                            ,"CFM50"
                                                                            ,"ACH50"
                                                                            ,"y_bar_ilk"
                                                                            ,"y_ilk"
                                                                            ,"m_ilk"))])

item40.data <- left_join(item40.data, item40.merge[which(colnames(item40.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                                   ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                                   ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                                   ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                                   ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                                   ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                                   ,"MECH_Blower_DOOR_P25_FanPressure_FirstTrial"
                                                                                   ,"MECH_Blower_DOOR_P25_FanPressure_SecondTrial"
                                                                                   ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                                   ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                                   ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                                   ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                                   ,"MECH_Blower_DOOR_P50_FanPressure_FirstTrial"
                                                                                   ,"MECH_Blower_DOOR_P50_FanPressure_SecondTrial"
                                                                                   ,"Flow.Exponent.FirstTrial"
                                                                                   ,"Flow.Exponent.SecondTrial"
                                                                                   ,"Flow.Exponent"
                                                                                   ,"P50_CFM"
                                                                                   ,"P50_HousePressure"
                                                                                   ,"CFM50"
                                                                                   ,"ACH50"
                                                                                   ,"y_bar_ilk"
                                                                                   ,"y_ilk"
                                                                                   ,"m_ilk"))])
#############################################################################################
# For Single Family
#############################################################################################
######################
# weighted analysis
######################
item40.final <- mean_one_group_domain(CustomerLevelData = item40.data
                               ,valueVariable = 'y_bar_ilk'
                               ,byVariable = 'HomeYearBuilt_bins4'
                               ,aggregateRow = "All Vintages")

unique(item40.final$HomeYearBuilt_bins4)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1985"
              ,"1986-1990"
              ,"1991-1995"
              ,"1996-2000"
              ,"2001-2005"
              ,"2006-2010"
              ,"Post 2010"
              ,"All Vintages")
item40.final <- item40.final %>% mutate(HomeYearBuilt_bins4 = factor(HomeYearBuilt_bins4, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins4)  
item40.final <- data.frame(item40.final)

item40.final.SF <- item40.final[which(item40.final$BuildingType == "Single Family")
                                ,which(colnames(item40.final) %notin% c("BuildingType"))]
# exportTable(item40.final.SF, "SF", "Table 47", weighted = TRUE)

######################
# unweighted analysis
######################
item40.final <- mean_one_group_unweighted(CustomerLevelData = item40.data
                                          ,valueVariable = 'y_bar_ilk'
                                          ,byVariable = 'HomeYearBuilt_bins4'
                                          ,aggregateRow = "All Vintages")

unique(item40.final$HomeYearBuilt_bins4)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1985"
              ,"1986-1990"
              ,"1991-1995"
              ,"1996-2000"
              ,"2001-2005"
              ,"2006-2010"
              ,"Post 2010"
              ,"All Vintages")
item40.final <- item40.final %>% mutate(HomeYearBuilt_bins4 = factor(HomeYearBuilt_bins4, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins4)  
item40.final <- data.frame(item40.final)

item40.final.SF <- item40.final[which(item40.final$BuildingType == "Single Family")
                                ,which(colnames(item40.final) %notin% c("BuildingType"))]
# exportTable(item40.final.SF, "SF", "Table 47", weighted = FALSE)


#############################################################################################
# For Manufactured
#############################################################################################
######################
# weighted analysis
######################
item40.final <- mean_one_group_domain(CustomerLevelData = item40.data
                               ,valueVariable = 'y_bar_ilk'
                               ,byVariable = 'HomeYearBuilt_bins2'
                               ,aggregateRow = "All Vintages")

unique(item40.final$HomeYearBuilt_bins2)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item40.final <- item40.final %>% mutate(HomeYearBuilt_bins2 = factor(HomeYearBuilt_bins2, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins2)  
item40.final <- data.frame(item40.final)


item40.final.MH <- item40.final[which(item40.final$BuildingType == "Manufactured")
                                ,which(colnames(item40.final) %notin% c("BuildingType"))]
exportTable(item40.final.MH, "MH", "Table 29", weighted = TRUE)

######################
# unweighted analysis
######################
item40.final <- mean_one_group_unweighted(CustomerLevelData = item40.data
                                          ,valueVariable = 'y_bar_ilk'
                                          ,byVariable = 'HomeYearBuilt_bins2'
                                          ,aggregateRow = "All Vintages")

unique(item40.final$HomeYearBuilt_bins2)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item40.final <- item40.final %>% mutate(HomeYearBuilt_bins2 = factor(HomeYearBuilt_bins2, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins2)  
item40.final <- data.frame(item40.final)

item40.final.MH <- item40.final[which(item40.final$BuildingType == "Manufactured")
                                ,which(colnames(item40.final) %notin% c("BuildingType"))]
exportTable(item40.final.MH, "MH", "Table 29", weighted = FALSE)









#############################################################################################
#Item 41: AVERAGE INFILTRATION RATE BY STATE, ACH50 DIVIDED BY 20 (SF table 48, MH table 30)
#############################################################################################
item41.dat  <- item38.data

item41.dat$ACH50 <- item41.dat$P50_CFM * 60 / item41.dat$Conditioned.Volume

item41.dat1 <- item41.dat[which(item41.dat$ACH50 != "Inf"),] #only 665/961 have a recorded conditioned floor volume

item41.dat1$Infiltration.Rate <- item41.dat1$ACH50 / 20
######################
# weighted analysis
######################
item41.final <- mean_one_group(CustomerLevelData = item41.dat1
                               ,valueVariable = 'Infiltration.Rate'
                               ,byVariable = 'State'
                               ,aggregateRow = "Region")

item41.final.SF <- item41.final[which(item41.final$BuildingType == "Single Family")
                                ,which(colnames(item41.final) %notin% c("BuildingType"))]
# exportTable(item41.final.SF, "SF", "Table 48", weighted = TRUE)

item41.final.MH <- item41.final[which(item41.final$BuildingType == "Manufactured")
                                ,which(colnames(item41.final) %notin% c("BuildingType"))]
exportTable(item41.final.MH, "MH", "Table 30", weighted = TRUE)

######################
# unweighted analysis
######################
item41.final <- mean_one_group_unweighted(CustomerLevelData = item41.dat1
                                          ,valueVariable = 'Infiltration.Rate'
                                          ,byVariable = 'State'
                                          ,aggregateRow = "Region")

item41.final.SF <- item41.final[which(item41.final$BuildingType == "Single Family")
                                ,which(colnames(item41.final) %notin% c("BuildingType"))]
# exportTable(item41.final.SF, "SF", "Table 48", weighted = FALSE)

item41.final.MH <- item41.final[which(item41.final$BuildingType == "Manufactured")
                                ,which(colnames(item41.final) %notin% c("BuildingType"))]
exportTable(item41.final.MH, "MH", "Table 30", weighted = FALSE)























############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]


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
                                                                     ,"MECH_Blower_DOOR_P25_FanPressure_FirstTrial"
                                                                     ,"MECH_Blower_DOOR_P25_FanPressure_SecondTrial"
                                                                     ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                     ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                     ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"
                                                                     ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                     ,"MECH_Blower_DOOR_P50_FanPressure_FirstTrial"
                                                                     ,"MECH_Blower_DOOR_P50_FanPressure_SecondTrial"))]

mech.dat.sub1 <- left_join(os.dat, mech.dat.sub, by = "CK_Cadmus_ID")
length(unique(mech.dat.sub1$CK_Cadmus_ID))
mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_FirstTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_CFM_SecondTrial))
mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_FirstTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P50_HousePressure_SecondTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_FirstTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_FirstTrial))
mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub1$MECH_Blower_DOOR_P25_HousePressure_SecondTrial))

mech.dat.sub2 <- mech.dat.sub1[which(!is.na(mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_FirstTrial | mech.dat.sub1$MECH_Blower_DOOR_P50_CFM_SecondTrial)),]
mech.dat.sub3 <- mech.dat.sub2[which(!is.na(mech.dat.sub2$MECH_Blower_DOOR_P25_CFM_FirstTrial | mech.dat.sub2$MECH_Blower_DOOR_P25_CFM_SecondTrial)),]
mech.dat.sub4 <- mech.dat.sub3[which(!is.na(mech.dat.sub3$MECH_Blower_DOOR_P50_HousePressure_FirstTrial | mech.dat.sub3$MECH_Blower_DOOR_P50_HousePressure_SecondTrial)),]
mech.dat.sub5 <- mech.dat.sub4[which(!is.na(mech.dat.sub4$MECH_Blower_DOOR_P25_HousePressure_FirstTrial | mech.dat.sub4$MECH_Blower_DOOR_P25_HousePressure_SecondTrial)),]

mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_FirstTrial            <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_FirstTrial            <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_FirstTrial  <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_FirstTrial  <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_FirstTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_FirstTrial    <- abs(as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_FirstTrial)))
mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_FirstTrial    <- abs(as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_FirstTrial)))
mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_SecondTrial           <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_CFM_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_SecondTrial           <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_CFM_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_HousePressure_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_SecondTrial <- as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_HousePressure_SecondTrial))
mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_SecondTrial   <- abs(as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_SecondTrial)))
mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_SecondTrial   <- abs(as.numeric(as.character(mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_SecondTrial)))

length(unique(mech.dat.sub5$CK_Cadmus_ID))

mech.dat.sub6 <- mech.dat.sub5[which(mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_FirstTrial    >= 26 | 
                                       mech.dat.sub5$MECH_Blower_DOOR_P25_FanPressure_SecondTrial >= 26 &
                                       mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_FirstTrial  >= 26 |
                                       mech.dat.sub5$MECH_Blower_DOOR_P50_FanPressure_SecondTrial >= 26),]


#############################################################################################
#Item 38: AVERAGE BLOWER DOOR AIR FLOW BY CK_Building_ID  (SF table 45, MH table 27)
#############################################################################################
item38.os.dat  <- mech.dat.sub6
item38.os.dat$Flow.Exponent.FirstTrial  <- log(item38.os.dat$MECH_Blower_DOOR_P50_CFM_FirstTrial / item38.os.dat$MECH_Blower_DOOR_P25_CFM_FirstTrial) / log(item38.os.dat$MECH_Blower_DOOR_P50_HousePressure_FirstTrial / item38.os.dat$MECH_Blower_DOOR_P25_HousePressure_FirstTrial)
item38.os.dat$Flow.Exponent.SecondTrial <- log(item38.os.dat$MECH_Blower_DOOR_P50_CFM_SecondTrial / item38.os.dat$MECH_Blower_DOOR_P25_CFM_SecondTrial) / log(item38.os.dat$MECH_Blower_DOOR_P50_HousePressure_SecondTrial / item38.os.dat$MECH_Blower_DOOR_P25_HousePressure_SecondTrial)

item38.os.dat1 <- item38.os.dat[which(item38.os.dat$Flow.Exponent.FirstTrial >= 0.5 & item38.os.dat$Flow.Exponent.FirstTrial <= 0.75 | item38.os.dat$Flow.Exponent.SecondTrial >= 0.5 & item38.os.dat$Flow.Exponent.SecondTrial <= 0.75 ),]
item38.os.dat1$Flow.Exponent <- item38.os.dat1$Flow.Exponent.FirstTrial
item38.os.dat1$Flow.Exponent[which(is.na(item38.os.dat1$Flow.Exponent))] <- 0
item38.os.dat1$P50_CFM       <- item38.os.dat1$MECH_Blower_DOOR_P50_CFM_FirstTrial
item38.os.dat1$P50_CFM[which(is.na(item38.os.dat1$P50_CFM))] <- 0
item38.os.dat1$P50_HousePressure       <- item38.os.dat1$MECH_Blower_DOOR_P50_HousePressure_FirstTrial
item38.os.dat1$P50_HousePressure[which(is.na(item38.os.dat1$P50_HousePressure))] <- 0

for (ii in 1:length(item38.os.dat1$Flow.Exponent)){
  if (item38.os.dat1$Flow.Exponent[ii] < 0.5 | item38.os.dat1$Flow.Exponent[ii] > 0.75){
    item38.os.dat1$Flow.Exponent[ii]     <- item38.os.dat1$Flow.Exponent.SecondTrial[ii]
    item38.os.dat1$P50_CFM[ii]           <- item38.os.dat1$MECH_Blower_DOOR_P50_CFM_SecondTrial[ii]
    item38.os.dat1$P50_HousePressure[ii] <- item38.os.dat1$MECH_Blower_DOOR_P50_HousePressure_SecondTrial[ii]
  }
}

item38.os.dat1$CFM50 <- item38.os.dat1$P50_CFM * (50 / abs(item38.os.dat1$P50_HousePressure)) ^ item38.os.dat1$Flow.Exponent
item38.os.dat2 <- item38.os.dat1
which(duplicated(item38.os.dat2$CK_Cadmus_ID))

#average within houses
item38.os.customer <- summarise(group_by(item38.os.dat2
                                         , CK_Cadmus_ID, CK_Building_ID, HomeYearBuilt)
                                ,CFM50  = mean(CFM50)
                                ,P50_CFM = mean(P50_CFM)
)

item38.os.merge <- left_join(os.dat, item38.os.customer)
item38.os.merge <- item38.os.merge[which(!is.na(item38.os.merge$CFM50)),]

######################################
#Pop and Sample Sizes for weights
######################################
item38.os.data <- weightedData(item38.os.merge[which(colnames(item38.os.merge) %notin% c("CFM50"
                                                                                         ,"P50_CFM"))])

item38.os.data <- left_join(item38.os.data, unique(item38.os.merge[which(colnames(item38.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"CFM50"
                                                                                                   ,"P50_CFM"))]))
item38.os.data$count <- 1

######################
# weighted analysis
######################
item38.os.final <- mean_one_group(CustomerLevelData = item38.os.data
                               ,valueVariable = 'CFM50'
                               ,byVariable    = 'CK_Building_ID'
                               ,aggregateRow  = "Remove")
names(item38.os.final)


levels(item38.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item38.os.final <- item38.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
item38.os.final <- data.frame(item38.os.final)


item38.os.final.SF <- item38.os.final[which(item38.os.final$CK_Building_ID != "Remove")
                                ,which(colnames(item38.os.final) %notin% c("BuildingType"))]

exportTable(item38.os.final.SF, "SF", "Table 45", weighted = TRUE, osIndicator = export.ind, OS = T)

######################
# unweighted analysis
######################
item38.os.final <- mean_one_group_unweighted(CustomerLevelData = item38.os.data
                                          ,valueVariable = 'CFM50'
                                          ,byVariable = 'CK_Building_ID'
                                          ,aggregateRow = "Remove")
names(item38.os.final)

levels(item38.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item38.os.final <- item38.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
item38.os.final <- data.frame(item38.os.final)

item38.os.final.SF <- item38.os.final[which(item38.os.final$CK_Building_ID != "Remove")
                                ,which(colnames(item38.os.final) %notin% c("BuildingType"))]

exportTable(item38.os.final.SF, "SF", "Table 45", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 39: AVERAGE BLOWER DOOR AIR FLOW BY CK_Building_ID  (SF table 46, MH table 28)
#############################################################################################
item39.os.dat  <- item38.os.data

item39.os.dat$ACH50 <- item39.os.dat$P50_CFM * 60 / item39.os.dat$Conditioned.Volume

item39.os.dat1 <- item39.os.dat[which(item39.os.dat$ACH50 != "Inf"),]

######################
# weighted analysis
######################
item39.os.final <- mean_one_group(CustomerLevelData = item39.os.dat1
                               ,valueVariable = 'ACH50'
                               ,byVariable = 'CK_Building_ID'
                               ,aggregateRow = "Remove")
names(item39.os.final)

levels(item39.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item39.os.final <- item39.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
item39.os.final <- data.frame(item39.os.final)


item39.os.final.SF <- item39.os.final[which(item39.os.final$CK_Building_ID != "Remove")
                                ,which(colnames(item39.os.final) %notin% c("BuildingType"))]
exportTable(item39.os.final.SF, "SF", "Table 46", weighted = TRUE, osIndicator = export.ind, OS = T)

######################
# unweighted analysis
######################
item39.os.final <- mean_one_group_unweighted(CustomerLevelData = item39.os.dat1
                                          ,valueVariable = 'ACH50'
                                          ,byVariable = 'CK_Building_ID'
                                          ,aggregateRow = "Remove")
names(item39.os.final)

levels(item39.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item39.os.final <- item39.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
item39.os.final <- data.frame(item39.os.final)

item39.os.final.SF <- item39.os.final[which(item39.os.final$CK_Building_ID != "Remove")
                                ,which(colnames(item39.os.final) %notin% c("BuildingType"))]
exportTable(item39.os.final.SF, "SF", "Table 46", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 40: AVERAGE BLOWER DOOR AIR FLOW BY HOME VINTAGE (SF table 47, MH table 29)
#############################################################################################
item40.os.dat  <- item38.os.dat1[which(!is.na(item38.os.dat1$HomeYearBuilt)),]

item40.os.dat$count <- 1

item40.os.dat$ACH50 <- item40.os.dat$P50_CFM * 60 / as.numeric(item40.os.dat$Conditioned.Volume)

item40.os.dat1 <- item40.os.dat[which(item40.os.dat$ACH50 != "Inf"),] #only 883/959 have a recorded conditioned floor volume

item40.os.merge <- left_join(os.dat, item40.os.dat1)
item40.os.merge <- item40.os.merge[which(!is.na(item40.os.merge$ACH50)),]

######################################
#Pop and Sample Sizes for weights
######################################
item40.os.data <- weightedData(item40.os.merge[which(colnames(item40.os.merge) %notin% c("MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                                ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                                ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                                ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                                ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                                ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                                ,"MECH_Blower_DOOR_P25_FanPressure_FirstTrial"
                                                                                ,"MECH_Blower_DOOR_P25_FanPressure_SecondTrial"
                                                                                ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                                ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                                ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                                ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                                ,"MECH_Blower_DOOR_P50_FanPressure_FirstTrial"
                                                                                ,"MECH_Blower_DOOR_P50_FanPressure_SecondTrial"
                                                                                ,"Flow.Exponent.FirstTrial"
                                                                                ,"Flow.Exponent.SecondTrial"
                                                                                ,"Flow.Exponent"
                                                                                ,"P50_CFM"
                                                                                ,"P50_HousePressure"
                                                                                ,"CFM50"
                                                                                ,"ACH50"
                                                                                ,"count"))])

item40.os.data <- left_join(item40.os.data, unique(item40.os.merge[which(colnames(item40.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                                       ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                                       ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"            
                                                                                       ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                                       ,"MECH_Blower_DOOR_P25_HousePressure_FirstTrial"
                                                                                       ,"MECH_Blower_DOOR_P25_HousePressure_SecondTrial"   
                                                                                       ,"MECH_Blower_DOOR_P25_FanPressure_FirstTrial"
                                                                                       ,"MECH_Blower_DOOR_P25_FanPressure_SecondTrial"
                                                                                       ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                                       ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                                       ,"MECH_Blower_DOOR_P50_HousePressure_FirstTrial"    
                                                                                       ,"MECH_Blower_DOOR_P50_HousePressure_SecondTrial"
                                                                                       ,"MECH_Blower_DOOR_P50_FanPressure_FirstTrial"
                                                                                       ,"MECH_Blower_DOOR_P50_FanPressure_SecondTrial"
                                                                                       ,"Flow.Exponent.FirstTrial"
                                                                                       ,"Flow.Exponent.SecondTrial"
                                                                                       ,"Flow.Exponent"
                                                                                       ,"P50_CFM"
                                                                                       ,"P50_HousePressure"
                                                                                       ,"CFM50"
                                                                                       ,"ACH50"
                                                                                       ,"count"))]))

#############################################################################################
# For Single Family
#############################################################################################
######################
# weighted analysis
######################
item40.os.summary <- mean_two_groups(CustomerLevelData = item40.os.data
                                  ,valueVariable = 'ACH50'
                                  ,byVariableRow = "HomeYearBuilt_bins4"
                                  ,byVariableColumn = "CK_Building_ID"
                                  # ,columnAggregate = "Remove"
                                  # ,rowAggregate = "Remove"
                                  )
item40.os.summary <- item40.os.summary[which(item40.os.summary$HomeYearBuilt_bins4 != "All Vintages"),]

item40.os.all.vintages <- mean_one_group(CustomerLevelData = item40.os.data
                                         ,valueVariable = 'ACH50'
                                         ,byVariable = 'CK_Building_ID'
                                         ,aggregateRow = "Remove")
item40.os.all.vintages <- dcast(setDT(item40.os.all.vintages)
                                ,formula = BuildingType ~ CK_Building_ID
                                ,value.var = c("Mean", "SE", "n","EB"))
item40.os.all.vintages$HomeYearBuilt_bins4 <- "All Vintages"

names(item40.os.summary)
names(item40.os.all.vintages)

item40.os.cast <- rbind.data.frame(item40.os.summary, item40.os.all.vintages, stringsAsFactors = F)

if(os.ind == "scl"){
  item40.os.final <- data.frame("BuildingType"          = item40.os.cast$BuildingType
                                ,"Housing.Vintage"      = item40.os.cast$HomeYearBuilt_bins4
                                ,"Mean_SCL.GenPop"      = item40.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item40.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item40.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = item40.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = item40.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item40.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = item40.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = item40.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item40.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item40.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item40.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item40.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item40.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item40.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item40.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item40.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item40.os.final <- data.frame("BuildingType"          = item40.os.cast$BuildingType
                                ,"Housing.Vintage"      = item40.os.cast$HomeYearBuilt_bins4
                                ,"Mean_SnoPUD"          = item40.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item40.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item40.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item40.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item40.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item40.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item40.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item40.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item40.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"            = item40.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"      = item40.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"           = item40.os.cast$`EB_2017 RBSA NW`)
}


unique(item40.os.final$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1985"
              ,"1986-1990"
              ,"1991-1995"
              ,"1996-2000"
              ,"2001-2005"
              ,"2006-2010"
              ,"Post 2010"
              ,"All Vintages")
item40.os.final <- item40.os.final %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item40.os.final <- data.frame(item40.os.final)

item40.os.final.SF <- item40.os.final[,which(colnames(item40.os.final) %notin% c("BuildingType"))]
exportTable(item40.os.final.SF, "SF", "Table 47", weighted = TRUE, osIndicator = export.ind, OS = T)

######################
# unweighted analysis
######################
item40.os.summary <- mean_two_groups_unweighted(CustomerLevelData = item40.os.data
                                     ,valueVariable = 'ACH50'
                                     ,byVariableRow = "HomeYearBuilt_bins4"
                                     ,byVariableColumn = "CK_Building_ID"
                                     # ,columnAggregate = "Remove"
                                     # ,rowAggregate = "Remove"
)
item40.os.summary <- item40.os.summary[which(item40.os.summary$HomeYearBuilt_bins4 != "All Vintages"),]

item40.os.all.vintages <- mean_one_group_unweighted(CustomerLevelData = item40.os.data
                                         ,valueVariable = 'ACH50'
                                         ,byVariable = 'CK_Building_ID'
                                         ,aggregateRow = "Remove")
item40.os.all.vintages <- dcast(setDT(item40.os.all.vintages)
                                ,formula = BuildingType ~ CK_Building_ID
                                ,value.var = c("Mean", "SE", "n"))
item40.os.all.vintages$HomeYearBuilt_bins4 <- "All Vintages"

names(item40.os.summary)
names(item40.os.all.vintages)

item40.os.cast <- rbind.data.frame(item40.os.summary, item40.os.all.vintages, stringsAsFactors = F)

if(os.ind == "scl"){
  item40.os.final <- data.frame("BuildingType"          = item40.os.cast$BuildingType
                                ,"Housing.Vintage"      = item40.os.cast$HomeYearBuilt_bins4
                                ,"Mean_SCL.GenPop"      = item40.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item40.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item40.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = item40.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = item40.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item40.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = item40.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = item40.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item40.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item40.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item40.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item40.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item40.os.final <- data.frame("BuildingType"          = item40.os.cast$BuildingType
                                ,"Housing.Vintage"      = item40.os.cast$HomeYearBuilt_bins4
                                ,"Mean_SnoPUD"          = item40.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item40.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item40.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item40.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item40.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item40.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item40.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item40.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item40.os.cast$`n_2017 RBSA NW`)
}



unique(item40.os.final$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1985"
              ,"1986-1990"
              ,"1991-1995"
              ,"1996-2000"
              ,"2001-2005"
              ,"2006-2010"
              ,"Post 2010"
              ,"All Vintages")
item40.os.final <- item40.os.final %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item40.os.final <- data.frame(item40.os.final)

item40.os.final.SF <- item40.os.final[,which(colnames(item40.os.final) %notin% c("BuildingType"))]
exportTable(item40.os.final.SF, "SF", "Table 47", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 41: AVERAGE INFILTRATION RATE BY CK_Building_ID, ACH50 DIVIDED BY 20 (SF table 48, MH table 30)
#############################################################################################
item41.os.dat  <- item38.os.data

item41.os.dat$ACH50 <- item41.os.dat$P50_CFM * 60 / as.numeric(item41.os.dat$Conditioned.Volume)

item41.os.dat1 <- item41.os.dat[which(item41.os.dat$ACH50 != "Inf"),] #only 665/961 have a recorded conditioned floor volume

item41.os.dat1$Infiltration.Rate <- item41.os.dat1$ACH50 / 20

######################
# weighted analysis
######################
item41.os.final <- mean_one_group(CustomerLevelData = item41.os.dat1
                               ,valueVariable = 'Infiltration.Rate'
                               ,byVariable = 'CK_Building_ID'
                               ,aggregateRow = "Remove")
names(item41.os.final)
levels(item41.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item41.os.final <- item41.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
item41.os.final <- data.frame(item41.os.final)

item41.os.final.SF <- item41.os.final[which(item41.os.final$CK_Building_ID != "Remove")
                                ,which(colnames(item41.os.final) %notin% c("BuildingType"))]
exportTable(item41.os.final.SF, "SF", "Table 48", weighted = TRUE, osIndicator = export.ind, OS = T)

######################
# unweighted analysis
######################
item41.os.final <- mean_one_group_unweighted(CustomerLevelData = item41.os.dat1
                                          ,valueVariable = 'Infiltration.Rate'
                                          ,byVariable = 'CK_Building_ID'
                                          ,aggregateRow = "Remove")
names(item41.os.final)
levels(item41.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item41.os.final <- item41.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
item41.os.final <- data.frame(item41.os.final)

item41.os.final.SF <- item41.os.final[which(item41.os.final$CK_Building_ID != "Remove")
                                ,which(colnames(item41.os.final) %notin% c("BuildingType"))]
exportTable(item41.os.final.SF, "SF", "Table 48", weighted = FALSE, osIndicator = export.ind, OS = T)

