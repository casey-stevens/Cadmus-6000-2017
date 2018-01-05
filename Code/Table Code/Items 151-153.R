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

# Bring in Usages
billing.dat <- read.xlsx(xlsxFile = file.path(filepathBillingData, billing.data))
billing.dat$CK_Cadmus_ID <- trimws(toupper(billing.dat$CK_Cadmus_ID))

results.dat <- merge(rbsa.dat, billing.dat, 
                     by = "CK_Cadmus_ID", all.y = T)

results.dat2 <- results.dat[-grep("bldg",results.dat$CK_Building_ID, ignore.case = T),]

### Bring in primary system fuel types
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Primary.Heating.System"
                                                                        ,"Heating.Fuel",
                                                                        "System.Sub-Type"))]

#remove datapoint not asked for and repeated header lines
mechanical.dat1 <- mechanical.dat1[which(mechanical.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]
mechanical.dat2 <- mechanical.dat1[which(mechanical.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(mechanical.dat2$Primary.Heating.System)

mechanical.dat2$Heating.System.Ind <- mechanical.dat2$Primary.Heating.System
mechanical.dat2$Heating.System.Ind[which(mechanical.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
mechanical.dat2$Heating.System.Ind[which(mechanical.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
mechanical.dat2$Heating.Fuel[which(mechanical.dat2$Heating.Fuel == "Natural gas")]    <- "Natural Gas"
mechanical.dat2$Heating.Fuel[which(mechanical.dat2$Heating.Fuel == "Wood (pellets)")] <- "Pellets"
mechanical.dat2$Heating.Fuel[which(mechanical.dat2$Heating.Fuel == "Wood (cord)")]    <- "Wood"
unique(mechanical.dat2$Heating.Fuel)

for (ii in 1:nrow(mechanical.dat2)){
  if (mechanical.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
    mechanical.dat2$Generic[ii] <- mechanical.dat2$`System.Sub-Type`[ii]
  }
  if (mechanical.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater")){
    mechanical.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
}

mechanical.dat2$Generic[which(mechanical.dat2$Generic == "Electric Baseboard")] <- "Electric Baseboard and Wall Heaters"


mechanical.dat3 <- unique(data.frame("CK_Cadmus_ID" = mechanical.dat2$CK_Cadmus_ID
                                     ,"Heating_Type" = mechanical.dat2$Generic
                                     ,"Heating_Fuel" = mechanical.dat2$Heating.Fuel
                                     ,"Primary_Secondary" = mechanical.dat2$Heating.System.Ind,
                                     stringsAsFactors = F))

mechanical.dat4 <- left_join(rbsa.dat, mechanical.dat3, by = "CK_Cadmus_ID")

mechanical.dat5 <- mechanical.dat4[which(mechanical.dat4$Primary_Secondary == "Primary Heating System"),]
length(unique(mechanical.dat5$CK_Cadmus_ID))
mechanical.dat5$count <- 1

unique(mechanical.dat5$Heating_Fuel)
mechanical.dat5$Heating_Fuel[which(mechanical.dat5$Heating_Fuel == "Kerosene")] <- "Oil"
mechanical.dat5$Heating_Fuel[which(mechanical.dat5$Heating_Fuel == "Natural Gas")] <- "Gas"
mecahnical.dat.final <- 
  mechanical.dat5[which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID", "Heating_Fuel"))]

final.data <- unique(left_join(results.dat2, mecahnical.dat.final))
# There are four people with multiple heating systems wil run with this for now





##################################################################################################
# Item 151: AVERAGE ANNUAL ELECTRICITY AND GAS USE PER HOME BY STATE (SF table 158, MH table 133)
##################################################################################################
item151.dat1 <- final.data[which(final.data$UsageNAC_kWh > 0), ]
item151.dat2 <- item151.dat1[-which(is.na(item151.dat1$UsageNAC_therms) &
                                      item151.dat1$Heating_Fuel != "Electric"),]
item151.dat2$UsageNAC_therms[which(is.na(item151.dat2$UsageNAC_therms))] <- 0

item151.dat2$kBtu <- item151.dat2$UsageNAC_kWh * 3.412 + item151.dat2$UsageNAC_therms * 99.98

#############################
# Add pop and sample sizes
#############################
drop.columns <- c("CADID"
                  ,"UsageNAC_kWh"
                  ,"UsageRaw_kWh"
                  ,"heating_kWh"
                  ,"UsageNAC_therms"
                  ,"UsageRaw_therms"
                  ,"heating_therms"
                  ,"Heating_Fuel"
                  ,"kBtu")
item151.data <- weightedData(item151.dat2[-which(colnames(item151.dat2) %in% drop.columns)])
item151.data <- left_join(item151.data, item151.dat2[which(colnames(item151.dat2) %in% c("CK_Cadmus_ID",drop.columns))])


#####################
# weighted analysis
#####################
item151.final <- mean_one_group(CustomerLevelData = item151.data
                                ,valueVariable = 'kBtu'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")
item151.final.SF <- item151.final[which(item151.final$BuildingType == "Single Family")
                                  ,-which(colnames(item151.final) == "BuildingType")]
item151.final.MH <- item151.final[which(item151.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item151.final) == "BuildingType")]

exportTable(item151.final.SF, "SF", "Table 158", weighted = TRUE)
exportTable(item151.final.MH, "MH", "Table 133", weighted = TRUE)

#####################
# unweighted analysis
#####################
item151.final <- mean_one_group_unweighted(CustomerLevelData = item151.data
                                ,valueVariable = 'kBtu'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")
item151.final.SF <- item151.final[which(item151.final$BuildingType == "Single Family")
                                  ,-which(colnames(item151.final) == "BuildingType")]
item151.final.MH <- item151.final[which(item151.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item151.final) == "BuildingType")]

exportTable(item151.final.SF, "SF", "Table 158", weighted = FALSE)
exportTable(item151.final.MH, "MH", "Table 133", weighted = FALSE)




#############################################################################################
# Item 152: AVERAGE ELECTRICITY AND GAS EUI BY STATE - SF TABLE 159, MH TABLE 134
#############################################################################################
item152.dat <- final.data[which(final.data$UsageNAC_kWh > 0),]
item152.dat2 <- item152.dat[-which(is.na(item152.dat$UsageNAC_therms) &
                                     item152.dat$Heating_Fuel != "Electric"),]
item152.dat3 <- item152.dat2[which(item152.dat2$Conditioned.Area > 0),]
item152.dat3$UsageNAC_therms[which(is.na(item152.dat3$UsageNAC_therms))] <- 0
item152.dat3$kBtu <- item152.dat3$UsageNAC_kWh * 3.412 + item152.dat3$UsageNAC_therms * 99.98
item152.dat3$EUI <- item152.dat3$kBtu/item152.dat3$Conditioned.Area


#############################
# Add pop and sample sizes
#############################
drop.columns <- c("CADID"
                  ,"UsageNAC_kWh"
                  ,"UsageRaw_kWh"
                  ,"heating_kWh"
                  ,"UsageNAC_therms"
                  ,"UsageRaw_therms"
                  ,"heating_therms"
                  ,"Heating_Fuel"
                  ,"kBtu"
                  ,"EUI")
item152.data <- weightedData(item152.dat3[-which(colnames(item152.dat3) %in% drop.columns)])
item152.data <- left_join(item152.data, item152.dat3[which(colnames(item152.dat3) %in% c("CK_Cadmus_ID",drop.columns))])


#####################
# weighted analysis
#####################
item152.final <- mean_one_group(CustomerLevelData = item152.data
                                ,valueVariable = 'EUI'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")
item152.final.SF <- item152.final[which(item152.final$BuildingType == "Single Family")
                                  ,-which(colnames(item152.final) == "BuildingType")]
item152.final.MH <- item152.final[which(item152.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item152.final) == "BuildingType")]

exportTable(item152.final.SF, "SF", "Table 159", weighted = TRUE)
exportTable(item152.final.MH, "MH", "Table 134", weighted = TRUE)

#####################
# unweighted analysis
#####################
item152.final <- mean_one_group_unweighted(CustomerLevelData = item152.data
                                           ,valueVariable = 'EUI'
                                           ,byVariable = 'State'
                                           ,aggregateRow = "Region")
item152.final.SF <- item152.final[which(item152.final$BuildingType == "Single Family")
                                  ,-which(colnames(item152.final) == "BuildingType")]
item152.final.MH <- item152.final[which(item152.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item152.final) == "BuildingType")]

exportTable(item152.final.SF, "SF", "Table 159", weighted = FALSE)
exportTable(item152.final.MH, "MH", "Table 134", weighted = FALSE)







#############################################################################################
# Item 153: AVERAGE WEATHER-NORMALIZED ELECTRICITY AND GAS EUI BY STATE  - SF TABLE 160, MH TABLE 135
#############################################################################################

item153.dat <- final.data[which(final.data$UsageNAC_kWh > 0),]
item153.dat <- item153.dat[-which(is.na(item153.dat$UsageNAC_therms) &
                                    item153.dat$Heating_Fuel != "Electric"),]
item153.dat <- item153.dat[which(item153.dat$Conditioned.Area > 0),]
item153.dat$UsageNAC_therms[which(is.na(item153.dat$UsageNAC_therms))] <- 0
item153.dat$kBtu <- item153.dat$UsageNAC_kWh * 3.412 + item153.dat$UsageNAC_therms * 99.98
item153.dat$EUI <- item153.dat$kBtu/item153.dat$Conditioned.Area


#############################
# Add pop and sample sizes
#############################
drop.columns <- c("CADID"
                  ,"UsageNAC_kWh"
                  ,"UsageRaw_kWh"
                  ,"heating_kWh"
                  ,"UsageNAC_therms"
                  ,"UsageRaw_therms"
                  ,"heating_therms"
                  ,"Heating_Fuel"
                  ,"kBtu"
                  ,"EUI")
item153.data <- weightedData(item153.dat[-which(colnames(item153.dat) %in% drop.columns)])
item153.data <- left_join(item153.data, item153.dat[which(colnames(item153.dat) %in% c("CK_Cadmus_ID",drop.columns))])


#####################
# weighted analysis
#####################
item153.final <- mean_one_group(CustomerLevelData = item153.data
                                ,valueVariable = 'EUI'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")
item153.final.SF <- item153.final[which(item153.final$BuildingType == "Single Family")
                                  ,-which(colnames(item153.final) == "BuildingType")]
item153.final.MH <- item153.final[which(item153.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item153.final) == "BuildingType")]

exportTable(item153.final.SF, "SF", "Table 160", weighted = TRUE)
exportTable(item153.final.MH, "MH", "Table 135", weighted = TRUE)

#####################
# unweighted analysis
#####################
item153.final <- mean_one_group_unweighted(CustomerLevelData = item153.data
                                           ,valueVariable = 'EUI'
                                           ,byVariable = 'State'
                                           ,aggregateRow = "Region")
item153.final.SF <- item153.final[which(item153.final$BuildingType == "Single Family")
                                  ,-which(colnames(item153.final) == "BuildingType")]
item153.final.MH <- item153.final[which(item153.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item153.final) == "BuildingType")]

exportTable(item153.final.SF, "SF", "Table 160", weighted = FALSE)
exportTable(item153.final.MH, "MH", "Table 135", weighted = FALSE)
