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

billing.dat <- read.xlsx(xlsxFile = file.path(filepathBillingData, billing.data))
billing.dat$CK_Cadmus_ID <- trimws(toupper(billing.dat$CADID))

results.dat <- merge(rbsa.dat, billing.dat, 
                     by = "CK_Cadmus_ID", all.y = T)

results.dat <- results.dat[-grep("bldg",results.dat$CK_Building_ID, ignore.case = T),]

usage.columns <- c("CADID", "UsageNAC_kWh", "UsageRaw_kWh", "heating_kWh", 
                   "UsageNAC_therms", "UsageRaw_therms", "heating_therms")

### Bring in primary system fuel types
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Primary.Heating.System"
                                                                        ,"Heating.Fuel"))]

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
#############################################################################################
# Item 143: AVERAGE ANNUAL KWH PER HOME BY STATE - TABLE 150 
#############################################################################################
item143.data <- results.dat
drop.for.weighting <- c(usage.columns)

item143.weighted <- weightedData(item143.data[-c(which(colnames(item143.data) %in% 
                                                                  drop.for.weighting))])
item143.weighted <- 
  left_join(item143.weighted, item143.data[c(1,
                                             c(which(colnames(item143.data) %in% 
                                                       drop.for.weighting)))])

################################
# Weighted Analysis
################################
item143.final <- mean_one_group(CustomerLevelData = item143.weighted
                                , valueVariable = 'UsageRaw_kWh'
                                , byVariable    = 'State'
                                , aggregateRow  = 'Region')

#subset by home type
item143.final.SF <- item143.final[which(item143.final$BuildingType == "Single Family"),-1]
item143.final.MH <- item143.final[which(item143.final$BuildingType == "Manufactured"),-1]
#export data - haven't changed any of the datsets waiting for Casey to QC
exportTable(item143.final.SF, "SF", "Table 150"
            , weighted = TRUE)
exportTable(item143.final.MH, "MH", "Table 125"
            , weighted = TRUE)

################################
# Unweighted Analysis
################################
item143.final <- mean_one_group_unweighted(CustomerLevelData = item143.weighted
                                         , valueVariable = 'UsageRaw_kWh'
                                         , byVariable    = 'State'
                                         , aggregateRow  = 'Region')
#subset by home type
item143.final.SF <- item143.final[which(item143.final$BuildingType == "Single Family"),-1]
item143.final.MH <- item143.final[which(item143.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item7.final.SF, "SF", "Table 150"
            , weighted = FALSE)
exportTable(item7.final.MH, "MH", "Table 125"
            , weighted = FALSE)

#############################################################################################
# Item 144: AVERAGE WEATHER NORMALIZED KWH PER HOME BY STATE  - SF TABLE 151, MH TABLE 126
#############################################################################################

item144.data <- results.dat
drop.for.weighting <- c(usage.columns)

item144.weighted <- weightedData(item144.data[-c(which(colnames(item144.data) %in% 
                                                         drop.for.weighting))])
item144.weighted <- 
  left_join(item144.weighted, item144.data[c(1,
                                             c(which(colnames(item144.data) %in% 
                                                       drop.for.weighting)))])


################################
# Weighted Analysis
################################
item144.final <- mean_one_group(CustomerLevelData = item144.weighted
                                , valueVariable = 'UsageNAC_kWh'
                                , byVariable    = 'State'
                                , aggregateRow  = 'Region')

#subset by home type
item144.final.SF <- item144.final[which(item144.final$BuildingType == "Single Family"),-1]
item144.final.MH <- item144.final[which(item144.final$BuildingType == "Manufactured"),-1]
#export data - haven't changed any of the datsets waiting for Casey to QC
exportTable(item7.final.SF, "SF", "Table 151"
            , weighted = TRUE)
exportTable(item7.final.MH, "MH", "Table 126"
            , weighted = TRUE)


################################
# Unweighted Analysis
################################
item144.final <- mean_one_group_unweighted(CustomerLevelData = item144.weighted
                                           , valueVariable = 'UsageRaw_kWh'
                                           , byVariable    = 'State'
                                           , aggregateRow  = 'Region')
#subset by home type
item144.final.SF <- item144.final[which(item144.final$BuildingType == "Single Family"),-1]
item144.final.MH <- item144.final[which(item144.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item7.final.SF, "SF", "Table 151"
            , weighted = FALSE)
exportTable(item7.final.MH, "MH", "Table 126"
            , weighted = FALSE)
#############################################################################################
# Item 146: AVERAGE ESTIMATED ANNUAL ELECTRIC SPACE HEAT PER HOME BY STATE   - SF TABLE 153, MH TABLE 128
#############################################################################################

item146.customer <- results.dat
# Need to merge on primary heating system fuel type
mechanical.final <- unique(mechanical.dat5[which(colnames(mechanical.dat5) %in% 
                                            c("CK_Cadmus_ID", "Heating_Fuel"))])

item146.customer2 <- left_join(item146.customer, mechanical.final)
item146.customer3 <- unique(item146.customer2[which(item146.customer2$Heating_Fuel == "Electric"),])

drop.for.weighting <- c(usage.columns, "Heating_Fuel")
item146.weighted <- weightedData(item146.customer3[-c(which(colnames(item146.customer3) %in% 
                                                         drop.for.weighting))])
item146.weighted <- 
  left_join(item146.weighted, item146.customer3[c(1,
                                             c(which(colnames(item146.customer3) %in% 
                                                       drop.for.weighting)))])

################################
# Weighted Analysis
################################
item146.final <- mean_one_group(CustomerLevelData = item146.weighted
                                , valueVariable = 'heating_kWh'
                                , byVariable    = 'State'
                                , aggregateRow  = 'Region')

#subset by home type
item146.final.SF <- item146.final[which(item146.final$BuildingType == "Single Family"),-1]
item146.final.MH <- item146.final[which(item146.final$BuildingType == "Manufactured"),-1]
#export data - haven't changed any of the datsets waiting for Casey to QC
exportTable(item143.final.SF, "SF", "Table 153"
            , weighted = TRUE)
exportTable(item143.final.MH, "MH", "Table 128"
            , weighted = TRUE)

################################
# Unweighted Analysis
################################
item146.final <- mean_one_group_unweighted(CustomerLevelData = item146.weighted
                                           , valueVariable = 'heating_kWh'
                                           , byVariable    = 'State'
                                           , aggregateRow  = 'Region')
#subset by home type
item146.final.SF <- item146.final[which(item146.final$BuildingType == "Single Family"),-1]
item146.final.MH <- item146.final[which(item146.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item7.final.SF, "SF", "Table 153"
            , weighted = FALSE)
exportTable(item7.final.MH, "MH", "Table 128"
            , weighted = FALSE)

#############################################################################################
# Item 147: AVERAGE ANNUAL THERMS PER HOME BY STATE - SF TABLE 154, MH TABLE 129
#############################################################################################

item147.data <- results.dat[which(results.dat$UsageRaw_therms > 0), ]
drop.for.weighting <- c(usage.columns)

item147.weighted <- weightedData(item147.data[-c(which(colnames(item147.data) %in% 
                                                         drop.for.weighting))])
item147.weighted <- 
  left_join(item147.weighted, item147.data[c(1,
                                             c(which(colnames(item147.data) %in% 
                                                       drop.for.weighting)))])


################################
# Weighted Analysis
################################
item147.final <- mean_one_group(CustomerLevelData = item147.weighted
                                , valueVariable = 'UsageRaw_therms'
                                , byVariable    = 'State'
                                , aggregateRow  = 'Region')

#subset by home type
item147.final.SF <- item147.final[which(item147.final$BuildingType == "Single Family"),-1]
item147.final.MH <- item147.final[which(item147.final$BuildingType == "Manufactured"),-1]
#export data - haven't changed any of the datsets waiting for Casey to QC
exportTable(item143.final.SF, "SF", "Table 154"
            , weighted = TRUE)
exportTable(item143.final.MH, "MH", "Table 129"
            , weighted = TRUE)

################################
# Unweighted Analysis
################################
item147.final <- mean_one_group_unweighted(CustomerLevelData = item147.weighted
                                           , valueVariable = 'UsageRaw_therms'
                                           , byVariable    = 'State'
                                           , aggregateRow  = 'Region')
#subset by home type
item147.final.SF <- item147.final[which(item147.final$BuildingType == "Single Family"),-1]
item147.final.MH <- item147.final[which(item147.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item7.final.SF, "SF", "Table 154"
            , weighted = FALSE)
exportTable(item7.final.MH, "MH", "Table 129"
            , weighted = FALSE)

#############################################################################################
# Item 148: AVERAGE WEATHER NORMALIZED GAS USE PER HOME BY STATE  - SF TABLE 155, MH TABLE 130
#############################################################################################

item148.data <- results.dat[which(results.dat$UsageNAC_therms > 0), ]
drop.for.weighting <- c(usage.columns)

item148.weighted <- weightedData(item148.data[-c(which(colnames(item148.data) %in% 
                                                         drop.for.weighting))])
item148.weighted <- 
  left_join(item148.weighted, item148.data[c(1,
                                             c(which(colnames(item148.data) %in% 
                                                       drop.for.weighting)))])

################################
# Weighted Analysis
################################
item148.final <- mean_one_group(CustomerLevelData = item148.weighted
                                , valueVariable = 'UsageNAC_therms'
                                , byVariable    = 'State'
                                , aggregateRow  = 'Region')

#subset by home type
item148.final.SF <- item148.final[which(item148.final$BuildingType == "Single Family"),-1]
item148.final.MH <- item148.final[which(item148.final$BuildingType == "Manufactured"),-1]
#export data - haven't changed any of the datsets waiting for Casey to QC
exportTable(item143.final.SF, "SF", "Table 155"
            , weighted = TRUE)
exportTable(item143.final.MH, "MH", "Table 130"
            , weighted = TRUE)

################################
# Unweighted Analysis
################################
item148.final <- mean_one_group_unweighted(CustomerLevelData = item148.weighted
                                           , valueVariable = 'UsageNAC_therms'
                                           , byVariable    = 'State'
                                           , aggregateRow  = 'Region')
#subset by home type
item148.final.SF <- item148.final[which(item148.final$BuildingType == "Single Family"),-1]
item148.final.MH <- item148.final[which(item148.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item7.final.SF, "SF", "Table 155"
            , weighted = FALSE)
exportTable(item7.final.MH, "MH", "Table 130"
            , weighted = FALSE)

#############################################################################################
# Item 150: AVERAGE ESTIMATED ANNUAL GAS SPACE HEAT PER HOME BY STATE   - SF TABLE 157, MH TABLE 132
#############################################################################################

item150.customer <- results.dat[which(results.dat$heating_therms > 0),]
# Need to merge on primary heating system fuel type
mechanical.final <- unique(mechanical.dat5[which(colnames(mechanical.dat5) %in% 
                                                   c("CK_Cadmus_ID", "Heating_Fuel"))])

item150.customer2 <- left_join(item150.customer, mechanical.final)
item150.customer3 <- unique(item150.customer2[which(item150.customer2$Heating_Fuel == "Gas"),])

drop.for.weighting <- c(usage.columns, "Heating_Fuel")
item150.weighted <- weightedData(item150.customer3[-c(which(colnames(item150.customer3) %in% 
                                                              drop.for.weighting))])
item150.weighted <- 
  left_join(item150.weighted, item150.customer3[c(1,
                                                  c(which(colnames(item150.customer3) %in% 
                                                            drop.for.weighting)))])

################################
# Weighted Analysis
################################
item150.final <- mean_one_group(CustomerLevelData = item150.weighted
                                , valueVariable = 'heating_therms'
                                , byVariable    = 'State'
                                , aggregateRow  = 'Region')

#subset by home type
item150.final.SF <- item150.final[which(item150.final$BuildingType == "Single Family"),-1]
item150.final.MH <- item150.final[which(item150.final$BuildingType == "Manufactured"),-1]
#export data - haven't changed any of the datsets waiting for Casey to QC
exportTable(item143.final.SF, "SF", "Table 157"
            , weighted = TRUE)
exportTable(item143.final.MH, "MH", "Table 132"
            , weighted = TRUE)

################################
# Unweighted Analysis
################################
item150.final <- mean_one_group_unweighted(CustomerLevelData = item150.weighted
                                           , valueVariable = 'heating_therms'
                                           , byVariable    = 'State'
                                           , aggregateRow  = 'Region')
#subset by home type
item150.final.SF <- item150.final[which(item150.final$BuildingType == "Single Family"),-1]
item150.final.MH <- item150.final[which(item150.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item7.final.SF, "SF", "Table 157"
            , weighted = FALSE)
exportTable(item7.final.MH, "MH", "Table 132"
            , weighted = FALSE)


