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
weightDataPath <- file.path(analysisFolder,"Weighting Files")     ##  Also output folder

stopifnot(all(file.exists(rootpath, analysisFolder, meterDataPath, weightDataPath)))

# Call file names
meter.export  <- "METERS_2017.06.16.xlsx"
popZIP.export <- "ZIP_Code_Utility_Mapping.xlsx"


#############################################################################################
# Import and Subset Data
#############################################################################################

# Import site data (Items 1, 2, 3-maybe)
site.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, meter.export), sheet=1)
names(site.dat)
site.dat1 <- data.frame("CK_Cadmus_ID" = site.dat$CK_Cadmus_ID
                        , "PK_Site_ID"      = site.dat$PK_SiteID
                        , "ZIPCode"         = site.dat$SITE_ZIP
                        , stringsAsFactors  = F)
head(site.dat1)
site.dat1$CK_Cadmus_ID <- trimws(toupper(site.dat1$CK_Cadmus_ID))
site.dat1$PK_Site_ID   <- trimws(toupper(site.dat1$PK_Site_ID))
site.dat1$ZIPCode      <- substr(site.dat1$ZIPCode, 1, 5)  ## Remove ZIP-Ext
length(unique(site.dat1$CK_Cadmus_ID)) #601


