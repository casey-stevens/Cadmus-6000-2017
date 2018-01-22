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


#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
mechanical.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
mechanical.dat.11 <- mechanical.dat.1[which(mechanical.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
mechanical.dat.12 <- mechanical.dat.11[which(mechanical.dat.11$Primary.Heating.System == "Yes"),]
length(unique(mechanical.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(mechanical.dat.12$Primary.Heating.System)
mechanical.dat.12$count <- 1

mechanical.dat.13 <- unique(mechanical.dat.12[which(mechanical.dat.12$Heating.Fuel == "Electric"),])

mechanical.sum <- summarise(group_by(mechanical.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
mechanical.sum$Count <- 1
which(duplicated(mechanical.sum$CK_Cadmus_ID)) #none are duplicated!
unique(mechanical.sum$Heating.Fuel)

mechanical.merge <- left_join(rbsa.dat, mechanical.sum)
mechanical.merge <- mechanical.merge[which(!is.na(mechanical.merge$Heating.Fuel)),]

item170.mechanical <- mechanical.merge



#############################################################################################
# 
#Sites Interview Data
# 
#############################################################################################
#############################################################################################
#Item 170: AVERAGE HEATING THERMOSTAT SETPOINT BY STATE (SF table 136, MH table 111)
#############################################################################################
#subset to columns needed for analysis
item170.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,""))])
colnames(item170.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint")
item170.dat$count <- 1
item170.dat$Thermostat_Setpoint <- as.numeric(as.character(item170.dat$Thermostat_Setpoint))

#remove any repeat header rows from exporting
item170.dat0 <- item170.dat[which(item170.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item170.dat1 <- left_join(rbsa.dat, item170.dat0, by = "CK_Cadmus_ID")

unique(item170.dat1$Thermostat_Setpoint)

item170.dat2.0 <- item170.dat1[which(!(is.na(item170.dat1$Thermostat_Setpoint))),]
item170.dat2 <- item170.dat2.0[which(item170.dat2.0$Thermostat_Setpoint != 0),]
colnames(item170.dat2)



item170.merge <- left_join(item170.dat2, item170.mechanical)
item170.merge <- item170.merge[which(!is.na(item170.merge$Heating.Fuel)),]

################################################
# Adding pop and sample sizes for weights
################################################
item170.data <- weightedData(item170.merge[-which(colnames(item170.merge) %in% c("Thermostat_Setpoint"
                                                                               ,"count"
                                                                               ,"Heating.Fuel"
                                                                               ,"Count"))])
item170.data <- left_join(item170.data, item170.merge[which(colnames(item170.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"Thermostat_Setpoint"
                                                                                         ,"count"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"Count"))])

#######################
# Weighted Analysis
#######################
item170.final <- mean_one_group(item170.data
                                ,valueVariable = 'Thermostat_Setpoint'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item170.final.SF <- item170.final[which(item170.final$BuildingType == "Single Family")
                                  ,-which(colnames(item170.final) %in% c("BuildingType"))]

exportTable(item170.final.SF, "SF", "Table B-15", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item170.final <- mean_one_group_unweighted(item170.data
                                           ,valueVariable = 'Thermostat_Setpoint'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item170.final.SF <- item170.final[which(item170.final$BuildingType == "Single Family")
                                  ,-which(colnames(item170.final) %in% c("BuildingType"))]

exportTable(item170.final.SF, "SF", "Table B-15", weighted = FALSE)






#############################################################################################
#Item 171: PERCENTAGE OF HOMES REPORTING A HEATING SETBACK BY STATE (SF table 137, MH table 112)
#############################################################################################
#subset to columns needed for analysis
item171.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForHeating"))])
colnames(item171.dat) <- c("CK_Cadmus_ID", "Nighttime_Heating", "Thermostat_Setpoint")

#remove any repeat header rows from exporting
item171.dat0 <- item171.dat[which(item171.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item171.dat1 <- left_join(item171.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item171.dat2.0 <- item171.dat1[which(!(is.na(item171.dat1$Thermostat_Setpoint))),]
item171.dat2 <- item171.dat2.0[which(item171.dat2.0$Thermostat_Setpoint != 0),]
unique(item171.dat2$Thermostat_Setpoint)
unique(item171.dat2$Nighttime_Heating)

item171.dat3.0 <- item171.dat2[which(!(is.na(item171.dat2$Nighttime_Heating))),]
item171.dat3 <- item171.dat3.0[which(item171.dat3.0$Nighttime_Heating != 0),]

item171.dat3$Heating.Setback <- 0
item171.dat3$Heating.Setback[which(item171.dat3$Nighttime_Heating < item171.dat3$Thermostat_Setpoint)] <- 1

item171.sum <- summarise(group_by(item171.dat3, CK_Cadmus_ID)
                         ,Ind = sum(Heating.Setback))

item171.merge <- left_join(rbsa.dat, item171.sum)
item171.merge <- item171.merge[which(!is.na(item171.merge$Ind)),]


item171.merge <- left_join(item171.merge, item170.mechanical)
item171.merge <- item171.merge[which(!is.na(item171.merge$Heating.Fuel)),]

################################################
# Adding pop and sample sizes for weights
################################################
item171.data <- weightedData(item171.merge[-which(colnames(item171.merge) %in% c("Ind"
                                                                                 ,"Heating.Fuel"
                                                                                 ,"Count"))])
item171.data <- left_join(item171.data, item171.merge[which(colnames(item171.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"
                                                                                           ,"Heating.Fuel"
                                                                                           ,"Count"))])
item171.data$Count <- 1
#######################
# Weighted Analysis
#######################
item171.final <- proportions_one_group(CustomerLevelData = item171.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE
                                       ,two.prop.total = NA)

item171.final.SF <- item171.final[which(item171.final$BuildingType == "Single Family")
                                  ,-which(colnames(item171.final) %in% c("BuildingType"))]

exportTable(item171.final.SF, "SF", "Table B-16", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item171.final <- proportions_one_group(CustomerLevelData = item171.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE
                                       ,two.prop.total = NA)

item171.final.SF <- item171.final[which(item171.final$BuildingType == "Single Family")
                                  ,-which(colnames(item171.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item171.final.SF, "SF", "Table B-16", weighted = FALSE)



