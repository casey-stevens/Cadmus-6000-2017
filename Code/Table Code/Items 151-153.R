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
billing.dat$CK_Cadmus_ID <- trimws(toupper(billing.dat$CADID))

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

rbsa.dat.clean <- unique(left_join(results.dat2, mecahnical.dat.final))

#############################################################################################
# Item 151: AVERAGE ANNUAL ELECTRICITY AND GAS USE PER HOME BY STATE - SF TABLE 158, MH TABLE 133
#############################################################################################

item151.dat <- rbsa.dat.clean
## Check this equation
item151.dat$kBtu <- item151.dat$kWh_NAC_fake * 3.412 + item151.dat$therm_NAC_fake * 99.98






#############################################################################################
# Item 152: AVERAGE ELECTRICITY AND GAS EUI BY STATE - SF TABLE 159, MH TABLE 134
#############################################################################################

item152.dat <- rbsa.dat.clean
item152.dat$kBtu <- item152.dat$kWh_usage_fake * 3.412 + item152.dat$therm_usage_fake * 99.98
item152.dat$EUI <- item152.dat$kBtu/item152.dat$SqFt


######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item152.strata <- summarise(group_by(item152.dat, BuildingType, State, Region, Territory)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataEUI = sum(EUI) / n_h
                            ,strataSD   = sd(EUI)
                            ,n          = length(unique(CK_Cadmus_ID))
)

item152.strata$strataSD[which(item152.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item152.state <- summarise(group_by(item152.strata, BuildingType, State)
                           ,Mean       = sum(N_h * strataEUI) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)

######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item152.region <- summarise(group_by(item152.strata, BuildingType)
                            ,State      = "Region"
                            ,Mean       = sum(N_h * strataEUI) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))

######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item152.final <- rbind.data.frame(item152.state, item152.region, stringsAsFactors = F) 
item152.table.SF <- item152.final[which(item152.final$BuildingType %in% c("Single Family")),-1]
item152.table.MH <- item152.final[which(item152.final$BuildingType %in% c("Manufactured")),-1]

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 158", x = item152.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 133", x = item152.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)

#############################################################################################
# Item 153: AVERAGE WEATHER-NORMALIZED ELECTRICITY AND GAS EUI BY STATE  - SF TABLE 160, MH TABLE 135
#############################################################################################

item153.dat <- rbsa.dat.clean
item153.dat$kBtu <- item153.dat$kWh_NAC_fake * 3.412 + item153.dat$therm_NAC_fake * 99.98
item153.dat$EUI <- item153.dat$kBtu/item153.dat$SqFt


######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item153.strata <- summarise(group_by(item153.dat, BuildingType, State, Region, Territory)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataEUI = sum(EUI) / n_h
                            ,strataSD   = sd(EUI)
                            ,n          = length(unique(CK_Cadmus_ID))
)

item153.strata$strataSD[which(item153.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item153.state <- summarise(group_by(item153.strata, BuildingType, State)
                           ,Mean       = sum(N_h * strataEUI) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)

######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item153.region <- summarise(group_by(item153.strata, BuildingType)
                            ,State      = "Region"
                            ,Mean       = sum(N_h * strataEUI) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))

######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item153.final <- rbind.data.frame(item153.state, item153.region, stringsAsFactors = F) 
item153.table.SF <- item153.final[which(item153.final$BuildingType %in% c("Single Family")),-1]
item153.table.MH <- item153.final[which(item153.final$BuildingType %in% c("Manufactured")),-1]

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 158", x = item153.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 133", x = item153.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)




