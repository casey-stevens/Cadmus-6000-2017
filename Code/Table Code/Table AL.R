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
                         ,startRow = 1, sheet = 1)

results.dat <- left_join(rbsa.dat, billing.dat, by = "CK_Cadmus_ID")

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
                             TotalBulbs = sum(AllBulbs),
                             EfficientTotal = sum(EfficientBulbs))

lighting.clean4$EfficientSaturation <- 
  lighting.clean4$EfficientTotal/lighting.clean4$TotalBulbs

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
UsageDataSF <- results.dat2[which(results.dat2$BuildingType == "Manufactured"),]
UsageDataSF_2 <- UsageDataSF[-which(is.na(UsageDataSF$UsageNAC_kWh)),]
UsageDataSF_3 <- UsageDataSF_2[which(UsageDataSF_2$Conditioned.Area > 0),]
UsageDataSF_3$EUI <- UsageDataSF_3$UsageNAC_kWh/UsageDataSF_3$Conditioned.Area
UsageDataSF_3 <- unique(UsageDataSF_3)

keep.cols <- c("CK_Cadmus_ID","EUI", "Conditioned.Area")

UsageDataSF_Final <- UsageDataSF_3[,which(colnames(UsageDataSF_3) %in% keep.cols)]
# View(UsageDataSF_Final)
UsageDataSF_Final2 <- merge(UsageDataSF_Final,  heating.final   , by = "CK_Cadmus_ID",all.x = T)
UsageDataSF_Final3 <- merge(UsageDataSF_Final2, lighting.final  , by = "CK_Cadmus_ID",all.x = T)
UsageDataSF_Final4 <- merge(UsageDataSF_Final3, central_Ac.final, by = "CK_Cadmus_ID",all.x = T)
UsageDataSF_Final5 <- merge(UsageDataSF_Final4, dhw.final       , by = "CK_Cadmus_ID",all.x = T)
UsageDataSF_Final6 <- merge(UsageDataSF_Final5, survey.final    , by = "CK_Cadmus_ID",all.x = T)
# View(UsageDataSF_Final6)

ii=5
for (ii in colnames(UsageDataSF_Final6)){
  UsageDataSF_Final6[is.na(UsageDataSF_Final6[ii]),ii] <- 0
}


UsageDataSF_Final7 <- left_join(rbsa.dat, UsageDataSF_Final6)
UsageDataSF_Final7 <- UsageDataSF_Final7[which(!is.na(UsageDataSF_Final7$count)),]
which(duplicated(UsageDataSF_Final7$CK_Cadmus_ID))
View(UsageDataSF_Final7)

unique(UsageDataSF_Final7$EUI)
quantile(UsageDataSF_Final7$EUI)
summary(UsageDataSF_Final7$EUI)

#for single family
# UsageDataSF_Final7$EUI_Quartile <- 4
# UsageDataSF_Final7$EUI_Quartile[which(UsageDataSF_Final7$EUI >= 0 & UsageDataSF_Final7$EUI < 3.5486421)] <- 1
# UsageDataSF_Final7$EUI_Quartile[which(UsageDataSF_Final7$EUI >= 3.5486421 & UsageDataSF_Final7$EUI < 5.9583333)] <- 2
# UsageDataSF_Final7$EUI_Quartile[which(UsageDataSF_Final7$EUI >= 5.9583333 & UsageDataSF_Final7$EUI < 9.2550041)] <- 3

#for Manufactured
UsageDataSF_Final7$EUI_Quartile <- 4
UsageDataSF_Final7$EUI_Quartile[which(UsageDataSF_Final7$EUI >= 0 & UsageDataSF_Final7$EUI < 6.365929)] <- 1
UsageDataSF_Final7$EUI_Quartile[which(UsageDataSF_Final7$EUI >= 6.365929 & UsageDataSF_Final7$EUI < 10.069864)] <- 2
UsageDataSF_Final7$EUI_Quartile[which(UsageDataSF_Final7$EUI >= 10.069864 & UsageDataSF_Final7$EUI < 13.727086)] <- 3

###########################
#Pull in weights
###########################
UsageDataSF_data <- weightedData(UsageDataSF_Final7[-which(colnames(UsageDataSF_Final7) %in% c("Conditioned.Area.x"
                                                                                               ,"EUI"
                                                                                               ,"EUI_Quartile"
                                                                                               ,"ElectricInd"
                                                                                               ,"TotalBulbs"
                                                                                               ,"EfficientTotal"
                                                                                               ,"EfficientSaturation"
                                                                                               ,"ACtotal"
                                                                                               ,"Has_AC"
                                                                                               ,"Electric_DWH"
                                                                                               ,"Conditioned.Area.y"
                                                                                               ,"Qty.Occupants"
                                                                                               ,"count"))])

UsageDataSF_data <- left_join(UsageDataSF_data, UsageDataSF_Final7[which(colnames(UsageDataSF_Final7) %in% c("CK_Cadmus_ID"
                                                                                                             ,"Conditioned.Area.x"
                                                                                                             ,"EUI"
                                                                                                             ,"EUI_Quartile"
                                                                                                             ,"ElectricInd"
                                                                                                             ,"TotalBulbs"
                                                                                                             ,"EfficientTotal"
                                                                                                             ,"EfficientSaturation"
                                                                                                             ,"ACtotal"
                                                                                                             ,"Has_AC"
                                                                                                             ,"Electric_DWH"
                                                                                                             ,"Conditioned.Area.y"
                                                                                                             ,"Qty.Occupants"
                                                                                                             ,"count"))])

UsageDataSF_data$count <- 1
UsageDataSF_data$Count <- 1
#######################
# Weighted Analysis - Average Area
#######################
UsageDataSF_sum1 <- mean_one_group(CustomerLevelData = UsageDataSF_data
                                   ,valueVariable = "Conditioned.Area"
                                   ,byVariable = "EUI_Quartile"
                                   ,aggregateRow = "Remove")
UsageDataSF_sum1 <- UsageDataSF_sum1[which(UsageDataSF_sum1$EUI_Quartile != "Remove"),-which(names(UsageDataSF_sum1) %in% c("BuildingType","n_h","N_h","Precision","n","SE"))]
names(UsageDataSF_sum1)[which(names(UsageDataSF_sum1) %in% c("Mean"))] <- c("Mean_Conditioned_Area")

#######################
# Weighted Analysis - % electrically heated
#######################
UsageDataSF_sum2 <- proportions_one_group(CustomerLevelData = UsageDataSF_data
                                          ,valueVariable = "ElectricInd"
                                          ,groupingVariable = "EUI_Quartile"
                                          ,total.name = "Remove")
UsageDataSF_sum2 <- UsageDataSF_sum2[which(UsageDataSF_sum2$EUI_Quartile != "Total"),-which(names(UsageDataSF_sum2) %in% c("BuildingType","n_h","N_h","N","count","n","w.SE","EUI_Quartile"))]
names(UsageDataSF_sum2)[which(names(UsageDataSF_sum2) %in% c("w.percent"))] <- c("Percent_Elec_Heated")

#######################
# Weighted Analysis - Efficient lighting %
#######################
UsageDataSF_sum3 <- proportions_one_group(CustomerLevelData = UsageDataSF_data
                                          ,valueVariable = "EfficientTotal"
                                          ,groupingVariable = "EUI_Quartile"
                                          ,total.name = "Remove")
UsageDataSF_sum3 <- UsageDataSF_sum3[which(UsageDataSF_sum3$EUI_Quartile != "Total"),-which(names(UsageDataSF_sum3) %in% c("BuildingType","n_h","N_h","N","count","n","w.SE","EUI_Quartile"))]
names(UsageDataSF_sum3)[which(names(UsageDataSF_sum3) %in% c("w.percent"))] <- c("Percent_Eff_Lighting")

#######################
# Weighted Analysis - % with AC
#######################
UsageDataSF_sum4 <- proportions_one_group(CustomerLevelData = UsageDataSF_data
                                          ,valueVariable = "Has_AC"
                                          ,groupingVariable = "EUI_Quartile"
                                          ,total.name = "Remove")
UsageDataSF_sum4 <- UsageDataSF_sum4[which(UsageDataSF_sum4$EUI_Quartile != "Total"),-which(names(UsageDataSF_sum4) %in% c("BuildingType","n_h","N_h","N","count","n","w.SE","EUI_Quartile"))]
names(UsageDataSF_sum4)[which(names(UsageDataSF_sum4) %in% c("w.percent"))] <- c("Percent_AC")

#######################
# Weighted Analysis - % with Elec DHW
#######################
UsageDataSF_sum5 <- proportions_one_group(CustomerLevelData = UsageDataSF_data
                                          ,valueVariable = "Electric_DWH"
                                          ,groupingVariable = "EUI_Quartile"
                                          ,total.name = "Remove")
UsageDataSF_sum5 <- UsageDataSF_sum5[which(UsageDataSF_sum5$EUI_Quartile != "Total"),-which(names(UsageDataSF_sum5) %in% c("BuildingType","n_h","N_h","N","count","w.SE","EUI_Quartile"))]
names(UsageDataSF_sum5)[which(names(UsageDataSF_sum5) %in% c("w.percent"))] <- c("Percent_Elec_DHW")


#######################
# Combine into table format
#######################
UsageDataSF_table <- cbind.data.frame(UsageDataSF_sum1,UsageDataSF_sum2,UsageDataSF_sum3,UsageDataSF_sum4,UsageDataSF_sum5)

# exportTable(UsageDataSF_table, "SF", "Table AL", weighted = TRUE)
exportTable(UsageDataSF_table, "MH", "Table AL", weighted = TRUE)







# FinalSummary <- summarize(group_by(UsageDataSF_Final7,UsageDataSF_Final7$EUI_Quartile),
#                           count = length(unique(CK_Cadmus_ID)),
#                           Avg_Area = mean(Conditioned.Area.x, na.rm =T),
#                           Total_Electric_Heat = sum(ElectricInd, na.rm = T),
#                           NonMissing_Elec_Heat = sum(!is.na(ElectricInd)),
#                           Avg_Eff_Lighting = sum(EfficientTotal, na.rm =T)/sum(TotalBulbs, na.rm =T) ,
#                           NonMissing_Lighting = sum(is.na(EfficientSaturation)),
#                           Total_AC = sum(Has_AC, na.rm = T),
#                           NonMissing_AC = sum(!is.na(Has_AC)),
#                           Electric_DHW = sum(Electric_DWH, na.rm = T),
#                           NonMissing_DHW = sum(!is.na(Electric_DWH)),
#                           Average_Number_Occupants = mean(Qty.Occupants, na.rm = T),
#                           NonMissing_People = sum(!is.na(Qty.Occupants)))
# # exportTable(FinalSummary, "SF", "Table AL", weighted = FALSE)
# exportTable(FinalSummary, "MH", "Table AL", weighted = FALSE)

