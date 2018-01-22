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
billing.dat <- read.xlsx(xlsxFile = file.path(filepathBillingData, billing.data)
                        ,startRow = 1, sheet = 1)

results.dat <- merge(rbsa.dat, billing.dat, 
                     by = "CK_Cadmus_ID", all.y = T)

#results.dat2 <- results.dat[-grep("bldg",results.dat$CK_Building_ID, ignore.case = T),]
results.dat2 <- results.dat

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

#############################################################################################
# Item 145: AVERAGE ELECTRIC EUI PER HOME BY HEATING FUEL TYPE AND STATE  - TABLE 152, Table 127
#############################################################################################
item145.dat <- final.data[which(final.data$UsageNAC_kWh > 0),]
unique(item145.dat$Heating_Fuel)
# Remove entries with missing fuel types
item145.dat2 <- item145.dat %>%
  filter(!is.na(Heating_Fuel))

unique(item145.dat2$Heating_Fuel)

item145.dat3 <- item145.dat2 %>%
  filter(Heating_Fuel %notin% c("Unknown"
                                , "Can't Determine"
                                , "Hydronic Gas-Water Fan Heater"
                                , "Hot Water from Water Heater"
                                , "Other"))

drop.columns <- c("CADID", "UsageNAC_kWh", "UsageRaw_kWh", "heating_kWh", 
                   "UsageNAC_therms", "UsageRaw_therms", "heating_therms", "Heating_Fuel")
item145.dat4 <- item145.dat3[which(item145.dat3$Conditioned.Area > 0),]
unique(item145.dat4$Heating_Fuel)     
item145.dat4$Heating_Fuel[which(item145.dat4$Heating_Fuel %in% c("Wood",
                                                                 "Oil",
                                                                 "Pellets",
                                                                 "Other",
                                                                 "Propane"))] <- "Other"
unique(item145.dat4$Heating_Fuel)     
item145.data <- weightedData(item145.dat4[-which(colnames(item145.dat4) %in% drop.columns)])

item145.data <- left_join(item145.data, item145.dat4[c(1, which(colnames(item145.dat4) %in% drop.columns))])

item145.data$EUI <- item145.data$UsageNAC_kWh/item145.data$Conditioned.Area

#Create Quartiles
quartiles <- quantile(item145.data$EUI)
item145.data$EUI_Quartile <- 4

item145.data$EUI_Quartile[which(item145.data$EUI >= 0 & item145.data$EUI < 3.9245651)] <- 1
item145.data$EUI_Quartile[which(item145.data$EUI >= 3.9245651 & item145.data$EUI < 6.7524004)] <- 2
item145.data$EUI_Quartile[which(item145.data$EUI >= 6.7524004 & item145.data$EUI < 10.7592748)] <- 3

#Export Quartiles
billing.dat <- write.xlsx(item145.data, file = file.path(filepathBillingData, "EUI with Quartiles.xlsx"))

##############################
# Weighted Analysis
##############################
item145.final <- mean_two_groups(CustomerLevelData  = item145.data
                                 , valueVariable    = 'EUI'
                                 , byVariableRow    = 'State'
                                 , byVariableColumn = 'Heating_Fuel'
                                 , columnAggregate  = "All Homes"
                                 , rowAggregate     = "Region")

# If final table have <NA> something was named incorrectly
levels(item145.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
item145.final <- item145.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
item145.final <- data.frame(item145.final)


item145.table.SF <- item145.final[which(item145.final$BuildingType %in% c("Single Family")),-1]
item145.table.MH <- item145.final[which(item145.final$BuildingType %in% c("Manufactured")),-1]

exportTable(item145.table.SF, "SF", "Table 152"
            , weighted = TRUE)
exportTable(item145.table.MH, "MH", "Table 127"
            , weighted = TRUE)


##############################
# Unweighted Analysis
##############################
item145.final <- mean_two_groups_unweighted(CustomerLevelData  = item145.data
                                             , valueVariable    = 'EUI'
                                             , byVariableRow    = 'State'
                                             , byVariableColumn = 'Heating_Fuel'
                                             , columnAggregate  = "All Homes"
                                             , rowAggregate     = "Region")

# If final table have <NA> something was named incorrectly
levels(item145.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
item145.final <- item145.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
item145.final <- data.frame(item145.final)

item145.table.SF <- item145.final[which(item145.final$BuildingType %in% c("Single Family")),-1]
item145.table.MH <- item145.final[which(item145.final$BuildingType %in% c("Manufactured")),-1]

exportTable(item145.table.SF, "SF", "Table 152"
            , weighted = FALSE)
exportTable(item145.table.MH, "MH", "Table 127"
            , weighted = FALSE)

#############################################################################################
# Item 149: AVERAGE GAS EUI PER HOME BY HEATING FUEL AND STATE  - SF TABLE 156, MH TABLE 131
#############################################################################################
item149.dat <- final.data[which(final.data$UsageNAC_therms > 0),]
unique(item149.dat$Heating_Fuel)
# Remove entries with missing fuel types
item149.dat2 <- item149.dat %>%
  filter(!is.na(Heating_Fuel))

unique(item149.dat2$Heating_Fuel)

item149.dat3 <- item149.dat2 %>%
  filter(Heating_Fuel %notin% c("Unknown"
                                , "Can't Determine"
                                , "Hydronic Gas-Water Fan Heater"
                                , "Hot Water from Water Heater"
                                , "Other"
                                ,"N/A"))

drop.columns <- c("CADID", "UsageNAC_kWh", "UsageRaw_kWh", "heating_kWh", 
                  "UsageNAC_therms", "UsageRaw_therms", "heating_therms", "Heating_Fuel")

item149.dat4 <- item149.dat3[which(item149.dat3$Conditioned.Area > 0),]
unique(item149.dat4$Heating_Fuel)     
item149.dat4$Heating_Fuel[which(item149.dat4$Heating_Fuel %in% c("Wood",
                                                                 "Oil",
                                                                 "Pellets",
                                                                 "Other",
                                                                 "Propane"))] <- "Other"
unique(item149.dat4$Heating_Fuel)     
item149.data <- weightedData(item149.dat4[-which(colnames(item149.dat4) %in% drop.columns)])

item149.data <- left_join(item149.data, item149.dat4[c(1, which(colnames(item149.dat4) %in% drop.columns))])

item149.data$EUI <- item149.data$UsageNAC_therms/item149.data$Conditioned.Area

##############################
# Weighted Analysis
##############################
item149.final <- mean_two_groups(CustomerLevelData  = item149.data
                                 , valueVariable    = 'EUI'
                                 , byVariableRow    = 'State'
                                 , byVariableColumn = 'Heating_Fuel'
                                 , columnAggregate  = "All Homes"
                                 , rowAggregate     = "Region")

# If final table have <NA> something was named incorrectly
levels(item149.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
item149.final <- item149.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
item149.final <- data.frame(item149.final)

item149.table.SF <- item149.final[which(item149.final$BuildingType %in% c("Single Family")),-1]
item149.table.MH <- item149.final[which(item149.final$BuildingType %in% c("Manufactured")),-1]

exportTable(item149.table.SF, "SF", "Table 156"
            , weighted = TRUE)
exportTable(item149.table.MH, "MH", "Table 131"
            , weighted = TRUE)



##############################
# Unweighted Analysis
##############################
item149.final <- mean_two_groups_unweighted(CustomerLevelData  = item149.data
                                            , valueVariable    = 'EUI'
                                            , byVariableRow    = 'State'
                                            , byVariableColumn = 'Heating_Fuel'
                                            , columnAggregate  = "All Homes"
                                            , rowAggregate     = "Region")

# If final table have <NA> something was named incorrectly
levels(item149.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
item149.final <- item149.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
item149.final <- data.frame(item149.final)

item149.table.SF <- item149.final[which(item149.final$BuildingType %in% c("Single Family")),-1]
item149.table.MH <- item149.final[which(item149.final$BuildingType %in% c("Manufactured")),-1]

exportTable(item149.table.SF, "SF", "Table 156"
            , weighted = FALSE)
exportTable(item149.table.MH, "MH", "Table 131"
            , weighted = FALSE)
