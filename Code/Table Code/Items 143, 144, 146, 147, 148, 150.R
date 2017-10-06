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

#########################

## Imported Weighted RBSA data- this file currentyl has fake usages maybe wont in future
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

### Clean data to only the necessary fields for all of the folowing items 
# 143, 144, 146, 147, 148, 150

#############################################################################################
# Item 143: AVERAGE ANNUAL KWH PER HOME BY STATE - TABLE 150 
#############################################################################################

item143.customer <- rbsa.dat

######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item143.strata <- summarise(group_by(item143.customer, BuildingType, State, Region, Territory)
                          ,n_h        = unique(n.h)
                          ,N_h        = unique(N.h)
                          ,fpc        = (1 - n_h / N_h)
                          ,w_h        = n_h / N_h
                          ,strataKwh = sum(kWh_usage_fake) / n_h
                          ,strataSD   = sd(kWh_usage_fake)
                          ,n          = length(unique(CK_Cadmus_ID))
)

item143.strata$strataSD[which(item143.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item143.state <- summarise(group_by(item143.strata, BuildingType, State)
                           ,Mean       = sum(N_h * strataKwh) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)

######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item143.region <- summarise(group_by(item143.strata, BuildingType)
                          ,State      = "Region"
                          ,Mean       = sum(N_h * strataKwh) / sum(N_h)
                          ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                          ,SampleSize = sum(unique(n)))

######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item143.final <- rbind.data.frame(item143.state, item143.region, stringsAsFactors = F) 
item143.table.SF <- item143.final[which(item143.final$BuildingType %in% c("Single Family")),-1]
item143.table.MH <- item143.final[which(item143.final$BuildingType %in% c("Manufactured")),-1]

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 150", x = item143.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 125", x = item143.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)

#############################################################################################
# Item 144: AVERAGE WEATHER NORMALIZED KWH PER HOME BY STATE  - SF TABLE 151, MH TABLE 126
#############################################################################################

item144.customer <- rbsa.dat

######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item144.strata <- summarise(group_by(item144.customer, BuildingType, State, Region, Territory)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataKwh = sum(kWh_NAC_fake) / n_h
                            ,strataSD   = sd(kWh_NAC_fake)
                            ,n          = length(unique(CK_Cadmus_ID))
)

item144.strata$strataSD[which(item144.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item144.state <- summarise(group_by(item144.strata, BuildingType, State)
                           ,Mean       = sum(N_h * strataKwh) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)

######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item144.region <- summarise(group_by(item144.strata, BuildingType)
                            ,State      = "Region"
                            ,Mean       = sum(N_h * strataKwh) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))

######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item144.final <- rbind.data.frame(item144.state, item144.region, stringsAsFactors = F) 
item144.table.SF <- item144.final[which(item144.final$BuildingType %in% c("Single Family")),-1]
item144.table.MH <- item144.final[which(item144.final$BuildingType %in% c("Manufactured")),-1]

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 151", x = item144.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 126", x = item144.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)

#############################################################################################
# Item 146: AVERAGE ESTIMATED ANNUAL ELECTRIC SPACE HEAT PER HOME BY STATE   - SF TABLE 153, MH TABLE 128
#############################################################################################

item146.customer <- rbsa.dat

######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item146.strata <- summarise(group_by(item146.customer, BuildingType, State, Region, Territory)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataKwh = sum(Heating_kWh_fake) / n_h
                            ,strataSD   = sd(Heating_kWh_fake)
                            ,n          = length(unique(CK_Cadmus_ID))
)

item146.strata$strataSD[which(item146.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item146.state <- summarise(group_by(item146.strata, BuildingType, State)
                           ,Mean       = sum(N_h * strataKwh) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)

######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item146.region <- summarise(group_by(item146.strata, BuildingType)
                            ,State      = "Region"
                            ,Mean       = sum(N_h * strataKwh) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))

######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item146.final <- rbind.data.frame(item146.state, item146.region, stringsAsFactors = F) 
item146.table.SF <- item146.final[which(item146.final$BuildingType %in% c("Single Family")),-1]
item146.table.MH <- item146.final[which(item146.final$BuildingType %in% c("Manufactured")),-1]

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 153", x = item146.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 128", x = item146.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)


#############################################################################################
# Item 147: AVERAGE ANNUAL KWH PER HOME BY STATE - TABLE 150 
#############################################################################################

item147.customer <- rbsa.dat

######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item147.strata <- summarise(group_by(item147.customer, BuildingType, State, Region, Territory)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataTherms = sum(therm_usage_fake) / n_h
                            ,strataSD   = sd(therm_usage_fake)
                            ,n          = length(unique(CK_Cadmus_ID))
)

item147.strata$strataSD[which(item147.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item147.state <- summarise(group_by(item147.strata, BuildingType, State)
                           ,Mean       = sum(N_h * strataTherms) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)

######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item147.region <- summarise(group_by(item147.strata, BuildingType)
                            ,State      = "Region"
                            ,Mean       = sum(N_h * strataTherms) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))

######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item147.final <- rbind.data.frame(item147.state, item147.region, stringsAsFactors = F) 
item147.table.SF <- item147.final[which(item147.final$BuildingType %in% c("Single Family")),-1]
item147.table.MH <- item147.final[which(item147.final$BuildingType %in% c("Manufactured")),-1]

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 150", x = item147.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 125", x = item147.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)

#############################################################################################
# Item 148: AVERAGE WEATHER NORMALIZED KWH PER HOME BY STATE  - SF TABLE 151, MH TABLE 126
#############################################################################################

item148.customer <- rbsa.dat

######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item148.strata <- summarise(group_by(item148.customer, BuildingType, State, Region, Territory)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataTherms = sum(therm_NAC_fake) / n_h
                            ,strataSD   = sd(therm_NAC_fake)
                            ,n          = length(unique(CK_Cadmus_ID))
)

item148.strata$strataSD[which(item148.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item148.state <- summarise(group_by(item148.strata, BuildingType, State)
                           ,Mean       = sum(N_h * strataTherms) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)

######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item148.region <- summarise(group_by(item148.strata, BuildingType)
                            ,State      = "Region"
                            ,Mean       = sum(N_h * strataTherms) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))

######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item148.final <- rbind.data.frame(item148.state, item148.region, stringsAsFactors = F) 
item148.table.SF <- item148.final[which(item148.final$BuildingType %in% c("Single Family")),-1]
item148.table.MH <- item148.final[which(item148.final$BuildingType %in% c("Manufactured")),-1]

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 151", x = item148.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 126", x = item148.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)

#############################################################################################
# Item 150: AVERAGE ESTIMATED ANNUAL ELECTRIC SPACE HEAT PER HOME BY STATE   - SF TABLE 153, MH TABLE 128
#############################################################################################

item150.customer <- rbsa.dat

######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item150.strata <- summarise(group_by(item150.customer, BuildingType, State, Region, Territory)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataTherms = sum(Heating_therm_fake) / n_h
                            ,strataSD   = sd(Heating_therm_fake)
                            ,n          = length(unique(CK_Cadmus_ID))
)

item150.strata$strataSD[which(item150.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item150.state <- summarise(group_by(item150.strata, BuildingType, State)
                           ,Mean       = sum(N_h * strataTherms) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)

######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item150.region <- summarise(group_by(item150.strata, BuildingType)
                            ,State      = "Region"
                            ,Mean       = sum(N_h * strataTherms) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))

######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item150.final <- rbind.data.frame(item150.state, item150.region, stringsAsFactors = F) 
item150.table.SF <- item150.final[which(item150.final$BuildingType %in% c("Single Family")),-1]
item150.table.MH <- item150.final[which(item150.final$BuildingType %in% c("Manufactured")),-1]

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 153", x = item150.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 128", x = item150.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)



