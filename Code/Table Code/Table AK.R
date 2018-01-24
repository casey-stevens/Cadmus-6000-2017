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
#Table AK: Average CFM by Tons of System Capacity by System Type and/or State
#############################################################################################
#####################
# For FLow
#####################
tableAK.flow <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                     ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                     ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                     ,"MECH_TrueFLow_NSOP"
                                                                     ,"MECH_TrueFLow_TFSOP"
                                                                     ,"MECH_TrueFLow_SOP_NoFilter"))]
tableAK.flow1 <- tableAK.flow[which(!is.na(tableAK.flow$MECH_TrueFLow_NSOP)),]
tableAK.flow2 <- tableAK.flow1[which(!is.na(tableAK.flow1$MECH_TrueFLow_SOP_NoFilter)),]

tableAK.flow2$MECH_TrueFLow_NSOP <- as.numeric(as.character(tableAK.flow2$MECH_TrueFLow_NSOP))
tableAK.flow2$MECH_TrueFLow_Plate14_PressureDifference <- as.numeric(as.character(tableAK.flow2$MECH_TrueFLow_Plate14_PressureDifference))
tableAK.flow2$MECH_TrueFLow_Plate20_PressureDifference <- as.numeric(as.character(tableAK.flow2$MECH_TrueFLow_Plate20_PressureDifference))
tableAK.flow2$MECH_TrueFLow_SOP_NoFilter <- as.numeric(as.character(tableAK.flow2$MECH_TrueFLow_SOP_NoFilter))
tableAK.flow2$MECH_TrueFLow_TFSOP <- as.numeric(as.character(tableAK.flow2$MECH_TrueFLow_TFSOP))


ii=145
for(ii in 1:nrow(tableAK.flow2)){
  if(!is.na(tableAK.flow2$MECH_TrueFLow_Plate14_PressureDifference[ii])){
    tableAK.flow2$Flow[ii] <- sqrt(tableAK.flow2$MECH_TrueFLow_NSOP[ii] / tableAK.flow2$MECH_TrueFLow_SOP_NoFilter[ii]) * (115 * sqrt(abs(tableAK.flow2$MECH_TrueFLow_Plate14_PressureDifference[ii])))
  }else{
    tableAK.flow2$Flow[ii] <- sqrt(tableAK.flow2$MECH_TrueFLow_NSOP[ii] / tableAK.flow2$MECH_TrueFLow_SOP_NoFilter[ii]) * (154 * sqrt(abs(tableAK.flow2$MECH_TrueFLow_Plate20_PressureDifference[ii])))
  }
}

unique(tableAK.flow2$Flow)
tableAK.flow3 <- tableAK.flow2[which(tableAK.flow2$Flow %notin% c("NaN",NA)),]



#####################
# For Tons
#####################
#subset to columns needed for analysis
tableAK.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"Primary.Heating.System"
                                                                    ,"Heating.Fuel"
                                                                    ,"System.Sub-Type"
                                                                    ,"Input.Heating.Capacity.-.High"
                                                                    ,"Output.Heating.Capacity.-.High"
                                                                    ,"Heating.Capacity.Units.-.High"))]

#remove datapoint not asked for and repeated header lines
tableAK.dat1 <- tableAK.dat[which(tableAK.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
tableAK.dat2 <- tableAK.dat1[which(tableAK.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(tableAK.dat2$Primary.Heating.System)



tableAK.dat2$Heating.System.Ind <- tableAK.dat2$Primary.Heating.System
tableAK.dat2$Heating.System.Ind[which(tableAK.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
tableAK.dat2$Heating.System.Ind[which(tableAK.dat2$Primary.Heating.System == "No")]  <- "Secondary Heating System"

unique(tableAK.dat2$`System.Sub-Type`)
unique(tableAK.dat2$Generic)

tableAK.dat3 <-tableAK.dat2#[which(tableAK.dat2$Primary.Heating.System == "Yes"),]

tableAK.dat4 <- tableAK.dat3[which(!is.na(tableAK.dat3$`Heating.Capacity.Units.-.High`)),]

for(i in 1:ncol(tableAK.dat4)){
  tableAK.dat4[,i] <- ifelse(tableAK.dat4[,i] %in% c("Unknown","N/A","-- Datapoint not asked for --"), NA, tableAK.dat4[,i])
}

tableAK.dat4$Heating.Capacity <- as.numeric(tableAK.dat4$`Output.Heating.Capacity.-.High`)
tableAK.dat4$`Input.Heating.Capacity.-.High` <- as.numeric(tableAK.dat4$`Input.Heating.Capacity.-.High`)

ii = 2
for(i in 1:length(tableAK.dat4$Heating.Capacity)){
  ifelse(is.na(tableAK.dat4$Heating.Capacity[i]), tableAK.dat4$Heating.Capacity[i] <- tableAK.dat4$`Input.Heating.Capacity.-.High`[i], tableAK.dat4$Heating.Capacity[i])
}

tableAK.dat5 <- tableAK.dat4[which(!is.na(tableAK.dat4$Heating.Capacity)),]
tableAK.dat6 <- tableAK.dat5[which(tableAK.dat5$Heating.Fuel == "Electric"),]
unique(tableAK.dat6$Generic)
tableAK.dat7 <- tableAK.dat6[which(tableAK.dat6$Generic %in% c("Air Source Heat Pump"
                                                               ,"Furnace"
                                                               ,"GeoThermal Heat Pump"
                                                               ,"Water Source Heat Pump")),]

unique(tableAK.dat7$`Heating.Capacity.Units.-.High`)

ii = 3
for(i in 1:length(tableAK.dat7$Heating.Capacity)){
  ifelse(tableAK.dat7$`Heating.Capacity.Units.-.High`[i] == "kW", tableAK.dat7$Heating.Capacity[i] <- tableAK.dat7$Heating.Capacity[i] * 3412, tableAK.dat7$Heating.Capacity[i])
}

tableAK.dat8 <- tableAK.dat7[which(tableAK.dat7$Heating.Capacity > 0),]
tableAK.dat8$Tons <- tableAK.dat8$Heating.Capacity / 12000


#######################
# Merge Flow and Tons
#######################
tableAK.merge0 <- left_join(tableAK.flow3, tableAK.dat8)
tableAK.sub    <- tableAK.merge0[which(!is.na(tableAK.merge0$Tons)),]
tableAK.sub$CFM.Per.Ton <- tableAK.sub$Flow / tableAK.sub$Tons

tableAK.merge <- left_join(rbsa.dat, tableAK.sub)
tableAK.merge <- tableAK.merge[which(!is.na(tableAK.merge$CFM.Per.Ton)),]


################################################
# Adding pop and sample sizes for weights
################################################
tableAK.data <- weightedData(tableAK.merge[-which(colnames(tableAK.merge) %in% c("MECH_TrueFLow_NSOP"
                                                                                 ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                                 ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                                 ,"MECH_TrueFLow_SOP_NoFilter"
                                                                                 ,"MECH_TrueFLow_TFSOP"
                                                                                 ,"Flow"
                                                                                 ,"Generic"
                                                                                 ,"System.Sub-Type"
                                                                                 ,"Primary.Heating.System"
                                                                                 ,"Heating.Fuel"
                                                                                 ,"Input.Heating.Capacity.-.High"
                                                                                 ,"Output.Heating.Capacity.-.High"
                                                                                 ,"Heating.Capacity.Units.-.High"
                                                                                 ,"Heating.System.Ind"
                                                                                 ,"Heating.Capacity"
                                                                                 ,"Tons"
                                                                                 ,"CFM.Per.Ton"))])
tableAK.data <- left_join(tableAK.data, tableAK.merge[which(colnames(tableAK.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"MECH_TrueFLow_NSOP"
                                                                                           ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                                           ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                                           ,"MECH_TrueFLow_SOP_NoFilter"
                                                                                           ,"MECH_TrueFLow_TFSOP"
                                                                                           ,"Flow"
                                                                                           ,"Generic"
                                                                                           ,"System.Sub-Type"
                                                                                           ,"Primary.Heating.System"
                                                                                           ,"Heating.Fuel"
                                                                                           ,"Input.Heating.Capacity.-.High"
                                                                                           ,"Output.Heating.Capacity.-.High"
                                                                                           ,"Heating.Capacity.Units.-.High"
                                                                                           ,"Heating.System.Ind"
                                                                                           ,"Heating.Capacity"
                                                                                           ,"Tons"
                                                                                           ,"CFM.Per.Ton"))])
tableAK.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAK.final <- mean_one_group(CustomerLevelData = tableAK.data
                               ,valueVariable = "CFM.Per.Ton"
                               ,byVariable = "Generic"
                               ,aggregateRow = "All Systems")
tableAK.final.SF <- tableAK.final[which(tableAK.final$BuildingType == "Single Family"),-which(colnames(tableAK.final) == "BuildingType")]
tableAK.final.MH <- tableAK.final[which(tableAK.final$BuildingType == "Manufactured"),-which(colnames(tableAK.final) == "BuildingType")]
# exportTable(tableAK.final.SF, "SF", "Table AK", weighted = TRUE)
exportTable(tableAK.final.MH, "MH", "Table AK", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
tableAK.final <- mean_one_group_unweighted(CustomerLevelData = tableAK.data
                                ,valueVariable = "CFM.Per.Ton"
                                ,byVariable = "Generic"
                                ,aggregateRow = "All Systems")
tableAK.final.SF <- tableAK.final[which(tableAK.final$BuildingType == "Single Family"),-which(colnames(tableAK.final) == "BuildingType")]
tableAK.final.MH <- tableAK.final[which(tableAK.final$BuildingType == "Manufactured"),-which(colnames(tableAK.final) == "BuildingType")]
# exportTable(tableAK.final.SF, "SF", "Table AK", weighted = FALSE)
exportTable(tableAK.final.MH, "MH", "Table AK", weighted = FALSE)
