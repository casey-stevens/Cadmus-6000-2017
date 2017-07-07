#############################################################################################
##  Title:            sampleCounts.R                      
##  Author:           Andrew Bernath, Cadmus Group               
##  Created:          07/05/2017
##  Updated:                                             
##  Billing Code(s):  
##  Description:      Code to import and count pop and sample sizes for each 
##                    post-strata region
#############################################################################################

##  Clear variables
# rm(list=ls())
# rundate <-  format(Sys.time(), "%d%b%y")
# options(scipen=999)
# 
# ##  Include packages
# library(plyr)
# library(dplyr)
# library(lubridate)
# library(tidyr)
# library(openxlsx)
# library(stringr)
# library(data.table)

################################################################################
# Use FILEPATHS from Step 1 for folders and file names of:
# - METER data
# - ZIP Code data (with pop counts from ACS)
# - output data
################################################################################
<<<<<<< HEAD

# Call file names
meter.export  <- "METERS_2017.06.16.xlsx"
popZIP.datMap <- "ZIP_Code_Utility_Mapping.xlsx"
=======
rootpath <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data"
analysisFolder <- rootpath
meterDataPath  <- file.path(analysisFolder,"Data for PSE")
bldgDataPath   <- file.path(analysisFolder,"Data for PSE")
weightDataPath <- file.path(analysisFolder,"Weight Source")     ##  Also output folder

stopifnot(all(file.exists(rootpath, analysisFolder, meterDataPath, weightDataPath)))

# Call file names
meter.export  <- "METERS_2017.06.16.xlsx"
popZIP.export <- "ZIP_Code_Utility_Mapping.xlsx"
>>>>>>> 1dc9891fe08cd6ce091aecd9e60a0948d10856f6
bldg.export   <- "SITES_2017.06.16.xlsx"

#############################################################################################
# Import and Subset Data
#############################################################################################

<<<<<<< HEAD
# Import clean RBSA data
cleanRBSA.dat <- read.xlsx(paste(filepathCleanData
                                 , paste("clean.rbsa.data", rundate, ".xlsx", sep = "")
                                 , sep="/")
)
names(cleanRBSA.dat)
cleanRBSA.dat1 <- data.frame("CK_Cadmus_ID"     = cleanRBSA.dat$CK_Cadmus_ID
                             , "BuildingType"   = cleanRBSA.dat$BuildingType
                             , stringsAsFactors = F)
cleanRBSA.dat1$CK_Cadmus_ID   <- trimws(toupper(cleanRBSA.dat1$CK_Cadmus_ID))
length(unique(cleanRBSA.dat1$CK_Cadmus_ID))  ## 601 unique ID's


# Import ID and ZIP data
id_zip.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, meter.export), sheet=1)
names(id_zip.dat)
id_zip.dat1 <- data.frame("CK_Cadmus_ID"     = id_zip.dat$CK_Cadmus_ID
                          , "ZIPCode"        = id_zip.dat$SITE_ZIP
                          , stringsAsFactors = F)
id_zip.dat1$CK_Cadmus_ID <- trimws(toupper(id_zip.dat1$CK_Cadmus_ID))
id_zip.dat1$ZIPCode      <- as.numeric(substr(id_zip.dat1$ZIPCode, 1, 5))  ## Remove ZIP-Ext
length(unique(id_zip.dat1$CK_Cadmus_ID))  ## 567 unique respondent ID's

# Indicate invalid ZIP codes
id_zip.dat1$invalidZIP <- rep(0, nrow(id_zip.dat1))
id_zip.dat1$invalidZIP[which(id_zip.dat1$ZIPCode < 10000)] <- 1
id_zip.dat1$invalidZIP[which(is.na(id_zip.dat1$ZIPCode))] <- 1


# Import ZIP code mapping
zipMap.dat <- read.xlsx(xlsxFile = file.path(filepathWeightingDocs, popZIP.datMap), sheet=1)
head(zipMap.dat)
zipMap.dat1 <- data.frame("ZIPCode"          = zipMap.dat$zip
                          , "State"          = zipMap.dat$state
                          , "Region"         = zipMap.dat$region
                          , "Utility"        = zipMap.dat$name
                          , "BPA_vs_IOU"     = zipMap.dat$`BPA.non-IOU?`
                          , stringsAsFactors = F)
zipMap.dat1$Utility <- trimws(toupper(zipMap.dat1$Utility))
zipMap.dat1$Utility <- gsub('[[:punct:]]+', '', zipMap.dat1$Utility)
zipMap.dat1$ZIPCode <- as.numeric(zipMap.dat1$ZIPCode)  



#############################################################################################
# Merge data and count sample sizes
#############################################################################################

# Join ZIP codes to building type data
samp.dat.0 <- left_join(cleanRBSA.dat1, id_zip.dat1, by="CK_Cadmus_ID")
# Join ZIP mapping to previous step
samp.dat.1 <- left_join(samp.dat.0, zipMap.dat1, by="ZIPCode")
samp.dat.1$tally <- rep(1, nrow(samp.dat.1))
head(samp.dat.1)

sampCounts <- summarise(group_by(samp.dat.1
                                 , BuildingType, State, Region, Utility, BPA_vs_IOU)
                        , n = sum(tally))
=======
# Import ID and ZIP data
id_zip.dat <- read.xlsx(xlsxFile = file.path(meterDataPath, meter.export), sheet=1)
names(id_zip.dat)
id_zip.dat1 <- data.frame("CK_Cadmus_ID"     = id_zip.dat$CK_Cadmus_ID
                          , "PK_Site_ID"     = id_zip.dat$PK_SiteID
                          , "ZIPCode"        = id_zip.dat$SITE_ZIP
                          , stringsAsFactors = F)
id_zip.dat1$CK_Cadmus_ID <- trimws(toupper(id_zip.dat1$CK_Cadmus_ID))
id_zip.dat1$PK_Site_ID   <- trimws(toupper(id_zip.dat1$PK_Site_ID))
id_zip.dat1$ZIPCode      <- substr(id_zip.dat1$ZIPCode, 1, 5)  ## Remove ZIP-Ext
length(unique(id_zip.dat1$CK_Cadmus_ID))  ## 567 unique respondent ID's
length(unique(id_zip.dat1$PK_Site_ID))  ## 574 unique site ID's


# Import bldg type data
bldg.dat <- read.xlsx(xlsxFile = file.path(bldgDataPath, bldg.export), sheet=1)
names(bldg.dat)
bldg.dat1 <- data.frame("PK_Site_ID"       = bldg.dat$PK_SiteID
                        , "BuildingType"   = bldg.dat$bldg_type
                        , stringsAsFactors = F)
bldg.dat1$PK_Site_ID   <- trimws(toupper(bldg.dat1$PK_Site_ID))
length(unique(bldg.dat1$PK_Site_ID))  ## 567 unique ID's
>>>>>>> 1dc9891fe08cd6ce091aecd9e60a0948d10856f6


