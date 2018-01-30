#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


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
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
sites.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, sites.export))
                             ,stringsAsFactors = FALSE)
#clean cadmus IDs
sites.dat$CK_Cadmus_ID <- trimws(toupper(sites.dat$CK_Cadmus_ID))




#############################################################################################
# Table RR: Percentage of homes with connected lighting by state
#############################################################################################
tableRR.dat <- sites.dat[which(colnames(sites.dat) %in% c("CK_Cadmus_ID"
                                                          ,"SITE_GENL_INFO_SmartHome_DevicesList"))]
tableRR.dat1 <- tableRR.dat[grep("Lighting Controls",tableRR.dat$SITE_GENL_INFO_SmartHome_DevicesList,ignore.case = T),]
unique(tableRR.dat$SITE_GENL_INFO_SmartHome_DevicesList)

tableRR.dat2 <- left_join(rbsa.dat, tableRR.dat1)

tableRR.dat2$Ind <- 0
tableRR.dat2$Ind[grep("lighting controls",tableRR.dat2$SITE_GENL_INFO_SmartHome_DevicesList,ignore.case = T)] <- 1

################################################
# Adding pop and sample sizes for weights
################################################
tableRR.data <- weightedData(tableRR.dat2[-which(colnames(tableRR.dat2) %in% c("Ind"
                                                                                 ,"SITE_GENL_INFO_SmartHome_DevicesList"))])
tableRR.data <- left_join(tableRR.data, tableRR.dat2[which(colnames(tableRR.dat2) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"
                                                                                           ,"SITE_GENL_INFO_SmartHome_DevicesList"))])
tableRR.data$count <- 1
tableRR.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableRR.table <- proportions_one_group(CustomerLevelData = tableRR.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)
tableRR.table.SF <- tableRR.table[which(tableRR.table$BuildingType == "Single Family")
                                  ,which(colnames(tableRR.table) %notin% c("BuildingType"))]
tableRR.table.MH <- tableRR.table[which(tableRR.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableRR.table) %notin% c("BuildingType"))]

# exportTable(tableRR.table.SF, "SF", "Table RR", weighted = TRUE)
exportTable(tableRR.table.MH, "MH", "Table RR", weighted = TRUE)

#######################
# MULTIFAMILY
#######################
tableRR.table.MF <- proportions_one_group(CustomerLevelData = tableRR.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Types"
                                       ,weighted = TRUE)
tableRR.table.MF <- tableRR.table.MF[which(tableRR.table.MF$BuildingType == "Multifamily")
                                  ,which(colnames(tableRR.table.MF) %notin% c("BuildingType"))]
exportTable(tableRR.table.MF, "MF", "Table RR", weighted = TRUE)



#######################
# unweighted Analysis
#######################
tableRR.table <- proportions_one_group(CustomerLevelData = tableRR.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)
tableRR.table.SF <- tableRR.table[which(tableRR.table$BuildingType == "Single Family")
                                  ,which(colnames(tableRR.table) %notin% c("BuildingType"))]
tableRR.table.MH <- tableRR.table[which(tableRR.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableRR.table) %notin% c("BuildingType"))]

# exportTable(tableRR.table.SF, "SF", "Table RR", weighted = FALSE)
exportTable(tableRR.table.MH, "MH", "Table RR", weighted = FALSE)

#######################
# MULTIFAMILY
#######################
tableRR.table.MF <- proportions_one_group(CustomerLevelData = tableRR.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "HomeType"
                                          ,total.name = "All Types"
                                          ,weighted = FALSE)
tableRR.table.MF <- tableRR.table.MF[which(tableRR.table.MF$BuildingType == "Multifamily")
                                     ,which(colnames(tableRR.table.MF) %notin% c("BuildingType"))]
exportTable(tableRR.table.MF, "MF", "Table RR", weighted = FALSE)







#############################################################################################
# Table TT: Percentage of homes reporting having an electric vehicle by state
#############################################################################################
tableTT.dat <- sites.dat[which(colnames(sites.dat) %in% c("CK_Cadmus_ID"
                                                          ,"SITE_GENL_INFO_Plug_InElectricVehiclePresent"))]
tableTT.dat1 <- tableTT.dat[grep("yes|no",tableTT.dat$SITE_GENL_INFO_Plug_InElectricVehiclePresent,ignore.case = T),]
unique(tableTT.dat$SITE_GENL_INFO_Plug_InElectricVehiclePresent)

tableTT.dat2 <- left_join(rbsa.dat, tableTT.dat1)

tableTT.dat2$Ind <- 0
tableTT.dat2$Ind[grep("yes",tableTT.dat2$SITE_GENL_INFO_Plug_InElectricVehiclePresent,ignore.case = T)] <- 1

################################################
# Adding pop and sample sizes for weights
################################################
tableTT.data <- weightedData(tableTT.dat2[-which(colnames(tableTT.dat2) %in% c("Ind"
                                                                               ,"SITE_GENL_INFO_Plug_InElectricVehiclePresent"))])
tableTT.data <- left_join(tableTT.data, tableTT.dat2[which(colnames(tableTT.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Ind"
                                                                                         ,"SITE_GENL_INFO_Plug_InElectricVehiclePresent"))])
tableTT.data$count <- 1
tableTT.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableTT.table <- proportions_one_group(CustomerLevelData = tableTT.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)
tableTT.table.SF <- tableTT.table[which(tableTT.table$BuildingType == "Single Family")
                                  ,which(colnames(tableTT.table) %notin% c("BuildingType"))]
tableTT.table.MH <- tableTT.table[which(tableTT.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableTT.table) %notin% c("BuildingType"))]

# exportTable(tableTT.table.SF, "SF", "Table TT", weighted = TRUE)
exportTable(tableTT.table.MH, "MH", "Table TT", weighted = TRUE)


#######################
# unweighted Analysis
#######################
tableTT.table <- proportions_one_group(CustomerLevelData = tableTT.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)
tableTT.table.SF <- tableTT.table[which(tableTT.table$BuildingType == "Single Family")
                                  ,which(colnames(tableTT.table) %notin% c("BuildingType"))]
tableTT.table.MH <- tableTT.table[which(tableTT.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableTT.table) %notin% c("BuildingType"))]

# exportTable(tableTT.table.SF, "SF", "Table TT", weighted = FALSE)
exportTable(tableTT.table.MH, "MH", "Table TT", weighted = FALSE)








#############################################################################################
# Table UU: Percentage of homes reporting having solar panels by state
#############################################################################################
TableUU.dat <- sites.dat[which(colnames(sites.dat) %in% c("CK_Cadmus_ID"
                                                          ,"SITE_GENL_INFO_SolarPanelsPresent_Y_N"))]
TableUU.dat1 <- TableUU.dat[grep("yes|no",TableUU.dat$SITE_GENL_INFO_SolarPanelsPresent_Y_N,ignore.case = T),]
unique(TableUU.dat$SITE_GENL_INFO_SolarPanelsPresent_Y_N)

TableUU.dat2 <- left_join(rbsa.dat, TableUU.dat1)

TableUU.dat2$Ind <- 0
TableUU.dat2$Ind[grep("yes",TableUU.dat2$SITE_GENL_INFO_SolarPanelsPresent_Y_N,ignore.case = T)] <- 1

################################################
# Adding pop and sample sizes for weights
################################################
TableUU.data <- weightedData(TableUU.dat2[-which(colnames(TableUU.dat2) %in% c("Ind"
                                                                               ,"SITE_GENL_INFO_SolarPanelsPresent_Y_N"))])
TableUU.data <- left_join(TableUU.data, TableUU.dat2[which(colnames(TableUU.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Ind"
                                                                                         ,"SITE_GENL_INFO_SolarPanelsPresent_Y_N"))])
TableUU.data$count <- 1
TableUU.data$Count <- 1

#######################
# Weighted Analysis
#######################
TableUU.table <- proportions_one_group(CustomerLevelData = TableUU.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)
TableUU.table.SF <- TableUU.table[which(TableUU.table$BuildingType == "Single Family")
                                  ,which(colnames(TableUU.table) %notin% c("BuildingType"))]
TableUU.table.MH <- TableUU.table[which(TableUU.table$BuildingType == "Manufactured")
                                  ,which(colnames(TableUU.table) %notin% c("BuildingType"))]

# exportTable(TableUU.table.SF, "SF", "Table UU", weighted = TRUE)
exportTable(TableUU.table.MH, "MH", "Table UU", weighted = TRUE)


#######################
# unweighted Analysis
#######################
TableUU.table <- proportions_one_group(CustomerLevelData = TableUU.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)
TableUU.table.SF <- TableUU.table[which(TableUU.table$BuildingType == "Single Family")
                                  ,which(colnames(TableUU.table) %notin% c("BuildingType"))]
TableUU.table.MH <- TableUU.table[which(TableUU.table$BuildingType == "Manufactured")
                                  ,which(colnames(TableUU.table) %notin% c("BuildingType"))]

# exportTable(TableUU.table.SF, "SF", "Table UU", weighted = FALSE)
exportTable(TableUU.table.MH, "MH", "Table UU", weighted = FALSE)








#############################################################################################
# Table VV: Percentage of homes reporting having solar panels by state
#############################################################################################
TableVV.dat <- sites.dat[which(colnames(sites.dat) %in% c("CK_Cadmus_ID"
                                                          ,"SITES_General_GENL_INFO_SmartHome_Devices_Y_N"))]
TableVV.dat1 <- TableVV.dat[grep("yes|no",TableVV.dat$SITES_General_GENL_INFO_SmartHome_Devices_Y_N,ignore.case = T),]
unique(TableVV.dat$SITES_General_GENL_INFO_SmartHome_Devices_Y_N)

TableVV.dat2 <- left_join(rbsa.dat, TableVV.dat1)
TableVV.dat2 <- TableVV.dat2[grep("site",TableVV.dat2$CK_Building_ID,ignore.case = T),]

TableVV.dat2$Ind <- 0
TableVV.dat2$Ind[grep("yes",TableVV.dat2$SITES_General_GENL_INFO_SmartHome_Devices_Y_N,ignore.case = T)] <- 1

################################################
# Adding pop and sample sizes for weights
################################################
TableVV.data <- weightedData(TableVV.dat2[-which(colnames(TableVV.dat2) %in% c("Ind"
                                                                               ,"SITES_General_GENL_INFO_SmartHome_Devices_Y_N"))])
TableVV.data <- left_join(TableVV.data, TableVV.dat2[which(colnames(TableVV.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Ind"
                                                                                         ,"SITES_General_GENL_INFO_SmartHome_Devices_Y_N"))])
TableVV.data$count <- 1
TableVV.data$Count <- 1

#######################
# Weighted Analysis
#######################
TableVV.table <- proportions_one_group(CustomerLevelData = TableVV.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)
TableVV.table.SF <- TableVV.table[which(TableVV.table$BuildingType == "Single Family")
                                  ,which(colnames(TableVV.table) %notin% c("BuildingType"))]
TableVV.table.MH <- TableVV.table[which(TableVV.table$BuildingType == "Manufactured")
                                  ,which(colnames(TableVV.table) %notin% c("BuildingType"))]

# exportTable(TableVV.table.SF, "SF", "Table VV", weighted = TRUE)
exportTable(TableVV.table.MH, "MH", "Table VV", weighted = TRUE)

#######################
# MULTIFAMILY
#######################
TableVV.table.MF <- proportions_one_group(CustomerLevelData = TableVV.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Types"
                                       ,weighted = TRUE)
TableVV.table.MF <- TableVV.table.MF[which(TableVV.table.MF$BuildingType == "Multifamily")
                                  ,which(colnames(TableVV.table.MF) %notin% c("BuildingType"))]
exportTable(TableVV.table.MF,"MF","Table VV",weighted = TRUE)



#######################
# unweighted Analysis
#######################
TableVV.table <- proportions_one_group(CustomerLevelData = TableVV.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)
TableVV.table.SF <- TableVV.table[which(TableVV.table$BuildingType == "Single Family")
                                  ,which(colnames(TableVV.table) %notin% c("BuildingType"))]
TableVV.table.MH <- TableVV.table[which(TableVV.table$BuildingType == "Manufactured")
                                  ,which(colnames(TableVV.table) %notin% c("BuildingType"))]

# exportTable(TableVV.table.SF, "SF", "Table VV", weighted = FALSE)
exportTable(TableVV.table.MH, "MH", "Table VV", weighted = FALSE)


#######################
# MULTIFAMILY
#######################
TableVV.table.MF <- proportions_one_group(CustomerLevelData = TableVV.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "HomeType"
                                          ,total.name = "All Types"
                                          ,weighted = FALSE)
TableVV.table.MF <- TableVV.table.MF[which(TableVV.table.MF$BuildingType == "Multifamily")
                                     ,which(colnames(TableVV.table.MF) %notin% c("BuildingType"))]
exportTable(TableVV.table.MF,"MF","Table VV",weighted = FALSE)
