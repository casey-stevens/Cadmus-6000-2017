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

source("Code/Table Code/SourceCode.R")

## Imported Weighted RBSA data- this file currentyl has fake usages maybe wont in future
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", '06Oct17', ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

## Import Mechanical Data for Heating Fuel Type
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
#subset to columns needed for analysis
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Heating.Fuel"
                                                                        ,"Primary.Heating.System"))]
# Subset to only primary heating systems
mechanical.dat2 <- mechanical.dat1[which(mechanical.dat1$Primary.Heating.System == 'Yes'),]

# Get rid of anything without a Heating Fuel listed
mechanical.dat.clean <- unique(mechanical.dat2[which(!is.na(mechanical.dat2$Heating.Fuel)),])

#Check for any duplciate Cadmus ID's, this indicates two different fuels and primary systems
DupAccounts <- mechanical.dat.clean$CK_Cadmus_ID[which(duplicated(mechanical.dat.clean$CK_Cadmus_ID))]

CheckDuplicates <- mechanical.dat.clean[which(mechanical.dat.clean$CK_Cadmus_ID %in% DupAccounts),]

# I will allow duplicates for now, discuss with Andrew and Casey
rbsa.dat1 <- merge(rbsa.dat,
                   mechanical.dat.clean,
                   by = "CK_Cadmus_ID")

## Now I need to bring in square footage from Envelope
#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]

envelope.dat1$BuildingSqFt <- as.numeric(as.character(envelope.dat1$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))
envelope.dat1.clean <- unique(envelope.dat1[!is.na(envelope.dat1$BuildingSqFt),])
envelope.dat1.clean2 <- unique(envelope.dat1.clean[which(envelope.dat1.clean$BuildingSqFt > 0),])

## Do we sum Square footage by site or what, will do that for now
envelope.dat1.clean3 <- summarise(group_by(envelope.dat1.clean2,
                                           CK_Cadmus_ID),
                                  SqFt = sum(BuildingSqFt))

rbsa.dat2 <- merge(rbsa.dat1,
                   envelope.dat1.clean3,
                   by = "CK_Cadmus_ID")

keep.cols <- c("CK_Cadmus_ID",
               "BuildingType",
               "State",
               "Region",
               "Territory",
               "n.h",
               "N.h",
               "SqFt",
               "Heating.Fuel",
               "kWh_NAC_fake",
               "therm_NAC_fake",
               "kWh_usage_fake",
               "therm_usage_fake")

rbsa.dat.clean <- rbsa.dat2[,which(colnames(rbsa.dat2) %in% keep.cols)]

#############################################################################################
# Item 151: AVERAGE ANNUAL ELECTRICITY AND GAS USE PER HOME BY STATE - SF TABLE 158, MH TABLE 133
#############################################################################################

item151.dat <- rbsa.dat.clean
## Check this equation
item151.dat$kBtu <- item151.dat$kWh_NAC_fake * 3.412 + item151.dat$therm_NAC_fake * 99.98

######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item151.strata <- summarise(group_by(item151.dat, BuildingType, State, Region, Territory)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataKbtu = sum(kBtu) / n_h
                            ,strataSD   = sd(kBtu)
                            ,n          = length(unique(CK_Cadmus_ID))
)

item151.strata$strataSD[which(item151.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item151.state <- summarise(group_by(item151.strata, BuildingType, State)
                           ,Mean       = sum(N_h * strataKbtu) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)

######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item151.region <- summarise(group_by(item151.strata, BuildingType)
                            ,State      = "Region"
                            ,Mean       = sum(N_h * strataKbtu) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))

######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item151.final <- rbind.data.frame(item151.state, item151.region, stringsAsFactors = F) 
item151.table.SF <- item151.final[which(item151.final$BuildingType %in% c("Single Family")),-1]
item151.table.MH <- item151.final[which(item151.final$BuildingType %in% c("Manufactured")),-1]

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 158", x = item151.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 133", x = item151.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)


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




