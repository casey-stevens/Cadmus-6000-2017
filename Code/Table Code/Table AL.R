#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

### EUI Table For Steve

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
                         ,startRow = 1, sheet = "RESULTSCOMPILED")

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

mechanical.dat5 <- mechanical.dat5 %>%
  filter(!is.na(Heating_Fuel))

unique(mechanical.dat5$Heating_Fuel)

mechanical.dat6 <- mechanical.dat5 %>%
  filter(Heating_Fuel %notin% c("Unknown"
                                , "Can't Determine"
                                , "Hydronic Gas-Water Fan Heater"
                                , "Hot Water from Water Heater"
                                , "Other",
                                "N/A"))
mechanical.dat6$Electric <- 0
mechanical.dat6$Electric[which(mechanical.dat6$Heating_Fuel == "Electric")] <- 1
heating.final <- summarize(group_by(mechanical.dat6,CK_Cadmus_ID),
                           ElectricInd = sum(Electric))
heating.final$ElectricInd[which(heating.final$ElectricInd > 0)] <- 1

# There are four people with multiple heating systems wil run with this for now

##### Bring in lighting information
lighting <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export),startRow = 2)
keep.cols <- c("CK_Cadmus_ID","Clean.Room", "Lamp.Category", "LIGHTING_BulbsPerFixture", "Fixture.Qty")
keep.cols.ind <- which(colnames(lighting) %in% keep.cols)
lighting.clean <- lighting[,keep.cols.ind]
lighting.clean2 <- lighting.clean[-which(lighting.clean$Clean.Room == "Storage"),]
unique(lighting.clean2$Lamp.Category)
lighting.clean2$Efficient <- 0 
lighting.clean2$Efficient[which(lighting.clean2$Lamp.Category %in% c("Light Emitting Diode", 
                                                                     "Compact Fluorescent"))] <- 1
table(lighting.clean2$Lamp.Category)
sum(lighting.clean2$Efficient) # Good

lighting.clean3 <- 
  lighting.clean2[-which(lighting.clean2$LIGHTING_BulbsPerFixture == "Unknown"),]

lighting.clean3$AllBulbs <- 
  as.numeric(lighting.clean3$Fixture.Qty) * 
  as.numeric(lighting.clean3$LIGHTING_BulbsPerFixture)
lighting.clean3$EfficientBulbs <- 
  as.numeric(lighting.clean3$Fixture.Qty) * 
  as.numeric(lighting.clean3$LIGHTING_BulbsPerFixture) * lighting.clean3$Efficient
View(lighting.clean3)
lighting.clean4 <- summarize(group_by(lighting.clean3,CK_Cadmus_ID),
                             BulbTotal = sum(AllBulbs),
                             EfficientTotal = sum(EfficientBulbs))

lighting.clean4$EfficientSaturation <- 
  lighting.clean4$EfficientTotal/lighting.clean4$BulbTotal

lighting.final <- lighting.clean4

##### Bring in Central AC information
central_Ac.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Provides", "System.Type"))]

central_Ac.dat1$AC <- 0
central_Ac.dat1$AC[which(central_Ac.dat1$System.Type %in% c("Air Source Heat Pump",
                                                            "Central AC", 
                                                            "Packaged AC",
                                                            "Evaporative Cooling",
                                                            "GeoThermal Heat Pump",
                                                            "Mini-split AC", 
                                                            "Water Source Heat Pump", 
                                                            "Mini-split HP", 
                                                            "Packaged HP",
                                                            "Air Handler"))] <- 1

central_Ac.dat2 <- summarize(group_by(central_Ac.dat1,CK_Cadmus_ID),
                             ACtotal = sum(AC))
central_Ac.dat2$Has_AC <- 0
central_Ac.dat2$Has_AC[which(central_Ac.dat2$ACtotal > 0)] <- 1
central_Ac.final <- central_Ac.dat2

##### Bring in Water Heater information
dhw.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                 , "DHW.Fuel"))]
unique(dhw.dat1$DHW.Fuel)

dhw.dat2 <- dhw.dat1[-which(dhw.dat1$DHW.Fuel %in% c("Unknown", "N/A")),]
dhw.dat2$ElectricDHW <- 0
dhw.dat2$ElectricDHW[which(dhw.dat2$DHW.Fuel == "Electric")] <- 1
dhw.dat3 <- summarize(group_by(dhw.dat2,CK_Cadmus_ID),
                             Electric_DWH = sum(ElectricDHW))
dhw.dat3$Electric_DWH[which(dhw.dat3$Electric_DWH > 0)] <- 1
dhw.final <- dhw.dat3



###### Now bring in Average number of occupants
#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))

#subset to columns needed for analysis
item122.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"Qty.Occupants"
                                                                                     ,""))])
item122.dat$count <- 1

#remove any repeat header rows from exporting
item122.dat0 <- item122.dat[which(item122.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item122.dat1 <- left_join(rbsa.dat, item122.dat0, by = "CK_Cadmus_ID")

item122.dat2 <- item122.dat1[which(!is.na(item122.dat1$Qty.Occupants)), ]

survey.final <- item122.dat2
survey.final$Qty.Occupants <- as.numeric(item122.dat2$Qty.Occupants)
survey.final <- survey.final[which(!is.na(item122.dat2$Qty.Occupants)),]

##### Now start doing the various summaries
UsageDataSF <- results.dat2[which(results.dat2$BuildingType == "Single Family"),]
UsageDataSF_2 <- UsageDataSF[-which(is.na(UsageDataSF$UsageNAC_kWh)),]
UsageDataSF_3 <- UsageDataSF_2[which(UsageDataSF_2$Conditioned.Area > 0),]
UsageDataSF_3$EUI <- UsageDataSF_3$UsageNAC_kWh/UsageDataSF_3$Conditioned.Area

quantile(UsageDataSF_3$EUI)

UsageDataSF_3$EUI_Quartile <- 4
UsageDataSF_3$EUI_Quartile[which(UsageDataSF_3$EUI >= 0 & UsageDataSF_3$EUI < 3.5316948)] <- 1
UsageDataSF_3$EUI_Quartile[which(UsageDataSF_3$EUI >= 3.5316948 & UsageDataSF_3$EUI < 6.0273354)] <- 2
UsageDataSF_3$EUI_Quartile[which(UsageDataSF_3$EUI >= 6.0273354 & UsageDataSF_3$EUI < 9.3661975)] <- 3
keep.cols <- c("CK_Cadmus_ID","EUI", "EUI_Quartile", "Conditioned.Area")

UsageDataSF_Final <- UsageDataSF_3[,which(colnames(UsageDataSF_3) %in% keep.cols)]
View(UsageDataSF_Final)
UsageDataSF_Final2 <- merge(UsageDataSF_Final, heating.final, by = "CK_Cadmus_ID",all.x = T)
UsageDataSF_Final3 <- merge(UsageDataSF_Final2, lighting.final, by = "CK_Cadmus_ID",all.x = T)
UsageDataSF_Final4 <- merge(UsageDataSF_Final3, central_Ac.final, by = "CK_Cadmus_ID",all.x = T)
UsageDataSF_Final5 <- merge(UsageDataSF_Final4, dhw.final, by = "CK_Cadmus_ID",all.x = T)
UsageDataSF_Final6 <- merge(UsageDataSF_Final5, survey.final, by = "CK_Cadmus_ID", all.x = T)
View(UsageDataSF_Final6)
FinalSummary <- summarize(group_by(UsageDataSF_Final6,UsageDataSF_Final6$EUI_Quartile),
                          count = length(unique(CK_Cadmus_ID)),
                          Avg_Area = mean(Conditioned.Area.x, na.rm =T),
                          Total_Electric_Heat = sum(ElectricInd, na.rm = T),
                          NonMissing_Elec_Heat = sum(!is.na(ElectricInd)),
                          Avg_Eff_Lighting = sum(EfficientTotal, na.rm =T)/sum(BulbTotal, na.rm =T) ,
                          NonMissing_Lighting = sum(is.na(EfficientSaturation)),
                          Total_AC = sum(Has_AC, na.rm = T),
                          NonMissing_AC = sum(!is.na(Has_AC)),
                          Electric_DHW = sum(Electric_DWH, na.rm = T),
                          NonMissing_DHW = sum(!is.na(Electric_DWH)),
                          Average_Number_Occupants = mean(Qty.Occupants, na.rm = T),
                          NonMissing_People = sum(!is.na(Qty.Occupants)))

##### Bring in lighting workbook
## Need to use: Lamp Category, LIGHTING_BulbsPerFixture, Fixture Qty

# Before anything get rid of Clean.Room = "Storage"
# Lamp Category = 
# Light Emitting Diode = 1 - efficient indicator
# OR
# Compact Fluorescent = 1 - eff
# 
### Central AC
##  System_Type = Central AC then yes otherwise no - Keep everything yes is only yes everything else is no

### Water Heat 
### DHW Fuel = "Electric" then electric otherwise 0 - if no fuel type then exclude from table

### Average number of occupants
##  Participant SUrvey file - survey.export
## Total = sum(
#Including yourself, how many people in your household are … Under the age of 18
#Including yourself, how many people in your household are … Between 18 and 34
#Including yourself, how many people in your household are … Between 34 and 54
#Including yourself, how many people in your household are … Between 55 and 64
#Including yourself, how many people in your household are … 65 and older)

