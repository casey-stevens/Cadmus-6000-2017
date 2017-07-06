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
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

##  Include packages
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(openxlsx)
library(stringr)
library(data.table)

################################################################################
# SET FILEPATHS for folders and file names of:
# - METER data
# - ZIP Code data (with pop counts from ACS)
# - output data
################################################################################
rootpath <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data"
analysisFolder <- rootpath
meterDataPath  <- file.path(analysisFolder,"Data for PSE")
bldgDataPath   <- file.path(analysisFolder,"Data for PSE")
weightDataPath <- file.path(analysisFolder,"Weight Source")     ##  Also output folder

stopifnot(all(file.exists(rootpath, analysisFolder, meterDataPath, weightDataPath)))

# Call file names
meter.export  <- "METERS_2017.06.16.xlsx"
popZIP.export <- "ZIP_Code_Utility_Mapping.xlsx"
bldg.export   <- "SITES_2017.06.16.xlsx"

#############################################################################################
# Import and Subset Data
#############################################################################################

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


