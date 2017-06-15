#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:                                             
##  Billing Code(s):  
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

#############################################################################################
# Import and Subset Data
#############################################################################################

# Define File Path
SPPath   <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Data for SCL"
stopifnot(file.exists(SPPath))

# Import site data (Items 1, 2, 3-maybe)
site.dat <- read.xlsx(xlsxFile = file.path(SPPath, "SITES_2017.03.30.xlsx"))
site.dat1 <- data.frame("CK_Cadmus_ID" = site.dat$CK_Cadmus_ID
                               , "BuildingTypeXX"  = site.dat$SITE_GENL_INFO_BuildingType
                               , "HomeYearBuiltXX" = site.dat$SITES_General_GENL_INFO_HomeYearBuilt
                               , "State"           = site.dat$SITE_ST
                               , "BuildingHeight"  = site.dat$SITE_Construction_TotalLevelsThisSite
                               , stringsAsFactors  = F)
head(site.dat1)
site.dat1$CK_Cadmus_ID <- trimws(site.dat1$CK_Cadmus_ID)
length(unique(site.dat1$CK_Cadmus_ID)) #565


#############################################################################################
# Clean Data
#############################################################################################

### Deal with lowercase / missing / unknown states

#make all states toupper
site.dat1$State <- toupper(site.dat1$State)

#replace all missing states with actual
state.na.ind <- site.dat1$CK_Cadmus_ID[which(is.na(site.dat1$State))]
missing.state <- site.dat1[which(site.dat1$CK_Cadmus_ID %in% state.na.ind),] ## note all are washington
site.dat1$State[which(is.na(site.dat1$State))] <- "WA"

#replace all unknown states with actual
state.unk.ind <- site.dat1$CK_Cadmus_ID[which(site.dat1$State == "UNKNOWN")]
site.dat1$State[which(site.dat1$CK_Cadmus_ID == "MM0016N20")] <- "MT"
site.dat1$State[which(site.dat1$CK_Cadmus_ID == "WM1463PM")]  <- "WA"

unique(site.dat1$State)

length(unique(site.dat1$CK_Cadmus_ID)) #566

#############################################################################################
# Clean building type and home year built info
#############################################################################################

rbsa.dat <- site.dat1

# Convert building types to what we want
rbsa.dat$BuildingType <- rbsa.dat$BuildingTypeXX
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Apartment Building (3 or fewer floors)")] <- "Multifamily - Low Rise"
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Apartment Building (4 to 6 floors)")] <- "Multifamily - High Rise"
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Apartment Building (More than 6 floors)")] <- "Multifamily - High Rise"
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Single Wide" | rbsa.dat$BuildingType == "Double Wide"| rbsa.dat$BuildingType == "Triple Wide")] <- "Manufactured"
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Townhome or Rowhome" | rbsa.dat$BuildingType == "Duplex, Triplex, or Fourplex" | rbsa.dat$BuildingType == "Single Family Detached")] <- "Single Family"
unique(rbsa.dat$BuildingType)

# Convert home year built to New / Existing
rbsa.dat$HomeYearBuilt <- rbsa.dat$HomeYearBuiltXX
rbsa.dat$HomeYearBuilt[which(as.numeric(as.character(rbsa.dat$HomeYearBuilt)) < 2012)] <- "Existing"
rbsa.dat$HomeYearBuilt[which(as.numeric(as.character(rbsa.dat$HomeYearBuilt)) > 2012 | as.numeric(as.character(rbsa.dat$HomeYearBuilt)) == 2012)] <- "New"
##Warning okay here
unique(rbsa.dat$HomeYearBuilt)

# Convert home year built to specific bins
rbsa.dat$HomeYearBuiltXX <- as.numeric(as.character(rbsa.dat$HomeYearBuiltXX))
rbsa.dat$HomeYearBuilt_bins <- as.numeric(as.character(rbsa.dat$HomeYearBuiltXX))
rbsa.dat$HomeYearBuilt_bins[which(rbsa.dat$HomeYearBuiltXX < 1951)] <- "Pre 1951"
rbsa.dat$HomeYearBuilt_bins[which(rbsa.dat$HomeYearBuiltXX >= 1951 & rbsa.dat$HomeYearBuiltXX < 1961)] <- "1951-1960"
rbsa.dat$HomeYearBuilt_bins[which(rbsa.dat$HomeYearBuiltXX >= 1961 & rbsa.dat$HomeYearBuiltXX < 1971)] <- "1961-1970"
rbsa.dat$HomeYearBuilt_bins[which(rbsa.dat$HomeYearBuiltXX >= 1971 & rbsa.dat$HomeYearBuiltXX < 1981)] <- "1971-1980"
rbsa.dat$HomeYearBuilt_bins[which(rbsa.dat$HomeYearBuiltXX >= 1981 & rbsa.dat$HomeYearBuiltXX < 1991)] <- "1981-1990"
rbsa.dat$HomeYearBuilt_bins[which(rbsa.dat$HomeYearBuiltXX >= 1991 & rbsa.dat$HomeYearBuiltXX < 2001)] <- "1991-2000"
rbsa.dat$HomeYearBuilt_bins[which(rbsa.dat$HomeYearBuiltXX >= 2001)] <- "Post 2000"
unique(rbsa.dat$HomeYearBuilt_bins)

# Convert home year built to specific bins (4 categories)
rbsa.dat$HomeYearBuilt_bins4 <- as.numeric(as.character(rbsa.dat$HomeYearBuiltXX))
rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuiltXX < 1981)] <- "Pre 1981"
rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuiltXX >= 1981 & rbsa.dat$HomeYearBuiltXX < 1991)] <- "1981-1990"
rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuiltXX >= 1991 & rbsa.dat$HomeYearBuiltXX < 2001)] <- "1991-2000"
rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuiltXX >= 2001)] <- "Post 2000"
unique(rbsa.dat$HomeYearBuilt_bins4)

length(unique(rbsa.dat$CK_Cadmus_ID)) #565

rbsa.dat1 <- rbsa.dat

##  Which sample pull?
rbsa.dat1$SampleInd <- tolower(rbsa.dat1$CK_Cadmus_ID)
rbsa.dat1$SampleInd[grep("core"   ,rbsa.dat1$SampleInd)] <- "CORE"
rbsa.dat1$SampleInd[grep("bpa"    ,rbsa.dat1$SampleInd)] <- "BPA"
rbsa.dat1$SampleInd[grep("scl"    ,rbsa.dat1$SampleInd)] <- "SCL"
rbsa.dat1$SampleInd[grep("pse"    ,rbsa.dat1$SampleInd)] <- "PSE"
rbsa.dat1$SampleInd[grep("snopud" ,rbsa.dat1$SampleInd)] <- "SnoPUD"
rbsa.dat1$SampleInd[grep("ws"     ,rbsa.dat1$SampleInd)] <- "CORE"
rbsa.dat1$SampleInd[grep("ms"     ,rbsa.dat1$SampleInd)] <- "CORE"
rbsa.dat1$SampleInd[grep("mm"     ,rbsa.dat1$SampleInd)] <- "CORE"
rbsa.dat1$SampleInd[grep("mh"     ,rbsa.dat1$SampleInd)] <- "CORE"
rbsa.dat1$SampleInd[grep("wm"     ,rbsa.dat1$SampleInd)] <- "CORE"
rbsa.dat1$SampleInd[grep("wh"     ,rbsa.dat1$SampleInd)] <- "CORE"
unique(rbsa.dat1$SampleInd)

length(unique(rbsa.dat1$CK_Cadmus_ID)) #565

## clean building height info
rbsa.sub <- rbsa.dat1[which(!(is.na(rbsa.dat1$BuildingHeight))),]
rbsa.sub1 <- rbsa.sub[which(colnames(rbsa.sub) %in% c("CK_Cadmus_ID", "BuildingHeight"))]

rbsa.dat1 <- rbsa.dat1[which(colnames(rbsa.dat1) != "BuildingHeight")]
rbsa.dat2 <- left_join(rbsa.dat1, rbsa.sub1, by = "CK_Cadmus_ID")

#############################################################################################
# Fix missing building type information
#############################################################################################

rbsa.dat3 <- rbsa.dat2[which(!(is.na(rbsa.dat2$BuildingType))),]
length(unique(rbsa.dat3$CK_Cadmus_ID)) #557 -- missing 8
#check to see which site IDs are lost
missing.ind <- unique(rbsa.dat2$CK_Cadmus_ID[which(!(rbsa.dat2$CK_Cadmus_ID %in% rbsa.dat3$CK_Cadmus_ID))])
rbsa.dat2[which(rbsa.dat2$CK_Cadmus_ID %in% missing.ind),]

#update building type (generic)
rbsa.dat2$BuildingType[which(rbsa.dat2$CK_Cadmus_ID == "SL2263 OS SCL")]   <- "Multifamily - Low Rise"
rbsa.dat2$BuildingType[which(rbsa.dat2$CK_Cadmus_ID == "SG0048 OS SCL")]   <- "Single Family"
rbsa.dat2$BuildingType[which(rbsa.dat2$CK_Cadmus_ID == "SL1953 OS SCL")]   <- "Single Family"
rbsa.dat2$BuildingType[which(rbsa.dat2$CK_Cadmus_ID == "WM1463PM")]        <- "Multifamily - High Rise"
rbsa.dat2$BuildingType[which(rbsa.dat2$CK_Cadmus_ID == "PNM22969 OS PSE")] <- "Multifamily - High Rise"
rbsa.dat2$BuildingType[which(rbsa.dat2$CK_Cadmus_ID == "KM24876 OS PSE")]  <- "Multifamily - High Rise"
rbsa.dat2$BuildingType[which(rbsa.dat2$CK_Cadmus_ID == "KM23468 OS PSE")]  <- "Multifamily - High Rise"
rbsa.dat2$BuildingType[which(rbsa.dat2$CK_Cadmus_ID == "MS3085")]          <- "Single Family"

# update building type (specific)
rbsa.dat2$BuildingTypeXX[which(rbsa.dat2$CK_Cadmus_ID == "SL2263 OS SCL")]   <- "Apartment Building (3 or fewer floors)"
rbsa.dat2$BuildingTypeXX[which(rbsa.dat2$CK_Cadmus_ID == "SG0048 OS SCL")]   <- "Single Family Detached"
rbsa.dat2$BuildingTypeXX[which(rbsa.dat2$CK_Cadmus_ID == "SL1953 OS SCL")]   <- "Single Family Detached"
rbsa.dat2$BuildingTypeXX[which(rbsa.dat2$CK_Cadmus_ID == "WM1463PM")]        <- "Apartment Building (More than 6 floors)"
rbsa.dat2$BuildingTypeXX[which(rbsa.dat2$CK_Cadmus_ID == "PNM22969 OS PSE")] <- "Apartment Building (More than 6 floors)"
rbsa.dat2$BuildingTypeXX[which(rbsa.dat2$CK_Cadmus_ID == "KM24876 OS PSE")]  <- "Apartment Building (More than 6 floors)"
rbsa.dat2$BuildingTypeXX[which(rbsa.dat2$CK_Cadmus_ID == "KM23468 OS PSE")]  <- "Apartment Building (More than 6 floors)"
rbsa.dat2$BuildingTypeXX[which(rbsa.dat2$CK_Cadmus_ID == "MS3085")]          <- "Single Family Detached"

##Clean up duplicating information
rbsa.dat2$BuildingTypeXX[which(rbsa.dat2$CK_Cadmus_ID == "SE0872 OS SCL")]   <- "Single Family Detached"
rbsa.dat2$HomeYearBuiltXX[which(rbsa.dat2$CK_Cadmus_ID == "SE0872 OS SCL")]   <- 1948
rbsa.dat2$HomeYearBuilt[which(rbsa.dat2$CK_Cadmus_ID == "SE0872 OS SCL")]   <- "Existing"
rbsa.dat2$HomeYearBuilt_bins[which(rbsa.dat2$CK_Cadmus_ID == "SE0872 OS SCL")]   <- "Pre 1951"
rbsa.dat2$HomeYearBuilt_bins4[which(rbsa.dat2$CK_Cadmus_ID == "SE0872 OS SCL")]   <- "Pre 1981"
rbsa.dat2$HomeYearBuiltXX[which(rbsa.dat2$CK_Cadmus_ID == "WS3209")]   <- 1946
rbsa.dat2$HomeYearBuilt_bins[which(rbsa.dat2$CK_Cadmus_ID == "WS3209")]   <- "Pre 1951"
rbsa.dat2$HomeYearBuilt_bins4[which(rbsa.dat2$CK_Cadmus_ID == "WS3209")]   <- "Pre 1981"
rbsa.dat2$HomeYearBuiltXX[which(rbsa.dat2$CK_Cadmus_ID == "SG0808 OS SCL")]   <- 1957
rbsa.dat2$HomeYearBuilt_bins[which(rbsa.dat2$CK_Cadmus_ID == "SG0808 OS SCL")]   <- "1951-1960"
rbsa.dat2$BuildingHeight[which(rbsa.dat2$CK_Cadmus_ID == "SL1953 OS SCL")]   <- 1.5

rbsa.dat4 <- unique(rbsa.dat2[which(!(is.na(rbsa.dat2$BuildingType))),])
length(unique(rbsa.dat4$CK_Cadmus_ID)) #565

############check to see which IDs are duplicated in##########################
# dup.ind <- which(duplicated(rbsa.dat4$CK_Cadmus_ID))
# cadmus.id.ind <- rbsa.dat4$CK_Cadmus_ID[dup.ind]
# rbsa.tmp <- rbsa.dat4[which(rbsa.dat4$CK_Cadmus_ID %in% cadmus.id.ind),]

rbsa.dat5 <- rbsa.dat4


#############################################################################################
# Write out cleaned building type information
#############################################################################################

##  Set outpath directory
outPath <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Analysis Documents/Clean Data"
stopifnot(file.exists(outPath))

##  Write out confidence/precision info
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
write.xlsx(rbsa.dat5, paste(outPath, paste("clean.rbsa.data", rundate, ".xlsx", sep = ""), sep="/"),
           append = T, row.names = F, showNA = F)



