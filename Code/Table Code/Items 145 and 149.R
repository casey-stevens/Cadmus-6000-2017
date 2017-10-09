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
               "therm_NAC_fake")

rbsa.dat.clean <- rbsa.dat2[,which(colnames(rbsa.dat2) %in% keep.cols)]

#############################################################################################
# Item 145: AVERAGE ELECTRIC EUI PER HOME BY HEATING FUEL TYPE AND STATE  - TABLE 152
#############################################################################################
item145.dat <- rbsa.dat.clean
unique(item145.dat$Heating.Fuel)
item145.dat$FuelType <- NA
item145.dat$FuelType[which(item145.dat$Heating.Fuel %in% c("Wood (cord)",
                                                           "Propane",
                                                           "Other",
                                                           "Oil",
                                                           "Wood (pellets)",
                                                           "Natural Gas",
                                                           "Natural gas"))] <- "Other"

item145.dat$EUI <- item145.dat$kWh_NAC_fake/item145.dat$SqFt

######################################################
# Step 1.1: Using customer level data,
#   Summarise data up to strata level
######################################################
item145.strata.fuel <- summarise(group_by(item145.dat, BuildingType, State, Region, Territory, FuelType)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataEUI = sum(EUI) / n_h
                            ,strataSD   = sd(EUI)
                            ,n          = length(unique(CK_Cadmus_ID))
)

item145.strata.all <- summarise(group_by(item145.dat, BuildingType, State, Region, Territory)
                                 ,FuelType = "All Homes"
                                 ,n_h        = unique(n.h)
                                 ,N_h        = unique(N.h)
                                 ,fpc        = (1 - n_h / N_h)
                                 ,w_h        = n_h / N_h
                                 ,strataEUI = sum(EUI) / n_h
                                 ,strataSD   = sd(EUI)
                                 ,n          = length(unique(CK_Cadmus_ID))
)

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################

item145.state.fuel <- summarise(group_by(item145.strata.fuel, BuildingType, State, FuelType)
                           ,Mean       = sum(N_h * strataEUI) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
)
item145.state.all <- summarise(group_by(item145.strata.all, BuildingType, State)
                                ,FuelType = "All Homes"
                                ,Mean       = sum(N_h * strataEUI) / sum(N_h)
                                ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                                ,SampleSize = sum(unique(n))
)

item145.state <- rbind.data.frame(item145.state.fuel, item145.state.all,
                                  stringsAsFactors = F)
######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item145.region.fuel <- summarise(group_by(item145.strata.fuel, BuildingType, FuelType)
                            ,State      = "Region"
                            ,Mean       = sum(N_h * strataEUI) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))
item145.region.all <- summarise(group_by(item145.strata.all, BuildingType)
                                 ,FuelType = "All Homes"
                                 ,State      = "Region"
                                 ,Mean       = sum(N_h * strataEUI) / sum(N_h)
                                 ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                                 ,SampleSize = sum(unique(n)))

item145.region <- rbind.data.frame(item145.region.fuel, 
                                   item145.region.all, stringsAsFactors = F)
######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item145.final <- rbind.data.frame(item145.state, item145.region, stringsAsFactors = F)
item145.table.SF <- item145.final[which(item145.final$BuildingType %in% c("Single Family")),-1]
item145.table.MH <- item145.final[which(item145.final$BuildingType %in% c("Manufactured")),-1]

item145.table.SF2 <- dcast(setDT(item145.table.SF)
                      ,formula = State ~ FuelType
                      ,value.var = c("Mean", "SE", "SampleSize"))
