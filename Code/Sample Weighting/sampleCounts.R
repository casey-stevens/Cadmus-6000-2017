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

# Call file names
meter.export  <- "METERS_2017.06.16.xlsx"
popZIP.datMap <- "ZIP_Code_Utility_Mapping.xlsx"
bldg.export   <- "SITES_2017.06.16.xlsx"

#############################################################################################
# Import and Subset Data
#############################################################################################

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

# standardize MF to a single category
cleanRBSA.dat1$BuildingType[grep("Multifamily", cleanRBSA.dat1$BuildingType)] <- "Multifamily"
unique(cleanRBSA.dat1$BuildingType)

# Import ID and ZIP data
id_zip.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, meter.export), sheet=1)
names(id_zip.dat)
id_zip.dat1 <- data.frame("CK_Cadmus_ID"     = id_zip.dat$CK_Cadmus_ID
                          , "ZIPCode"        = id_zip.dat$SITE_ZIP
                          , "Utility"        = id_zip.dat$Utility
                          , "MeterType"      = id_zip.dat$Type
                          , stringsAsFactors = F)
id_zip.dat1$CK_Cadmus_ID <- trimws(toupper(id_zip.dat1$CK_Cadmus_ID))
id_zip.dat1$Utility      <- trimws(toupper(id_zip.dat1$Utility))
id_zip.dat1$MeterType    <- trimws(toupper(id_zip.dat1$MeterType))
id_zip.dat1$ZIPCode      <- as.numeric(substr(id_zip.dat1$ZIPCode, 1, 5))  ## Remove ZIP-Ext
length(unique(id_zip.dat1$CK_Cadmus_ID))  ## 567 unique respondent ID's

# Indicate invalid ZIP codes
id_zip.dat1$invalidZIP <- rep(0, nrow(id_zip.dat1))
id_zip.dat1$invalidZIP[which(id_zip.dat1$ZIPCode < 10000)] <- 1
id_zip.dat1$invalidZIP[which(is.na(id_zip.dat1$ZIPCode))] <- 1

# Subset to electric meters only
id_zip.dat2 <- id_zip.dat1[which(id_zip.dat1$MeterType == "ELECTRIC"),]

      ##  QA/QC: Any lost customers?
      length(unique(id_zip.dat1$CK_Cadmus_ID)) == length(unique(id_zip.dat2$CK_Cadmus_ID))
      ##  0 lost customers

# Import ZIP code mapping
zipMap.dat <- read.xlsx(xlsxFile = file.path(filepathWeightingDocs, popZIP.datMap), sheet=1)
names(zipMap.dat)   <- c("ZIPCode", "City", "County", "State", "Region", "FERC_ID", "Utility"
                         , "Fraction", "BPA_vs_IOU", "SF.N", "MF.N", "MH.N", "SF.N.adj"
                         , "MF.N.adj", "MH.N.adj")
head(zipMap.dat)

# Clean up data
zipMap.dat$Utility <- trimws(toupper(zipMap.dat$Utility))
zipMap.dat$Utility <- gsub('[[:punct:]]+', '', zipMap.dat$Utility)
zipMap.dat$ZIPCode <- as.numeric(zipMap.dat$ZIPCode)  

zipMap.dat1 <- data.frame("ZIPCode"          = zipMap.dat$ZIPCode
                          , "State"          = zipMap.dat$State
                          , "Region"         = zipMap.dat$Region
                          , "Utility"        = zipMap.dat$Utility
                          , "BPA_vs_IOU"     = zipMap.dat$BPA_vs_IOU
                          , stringsAsFactors = F)

      ##  QA/QC: Check names of utilities for mismatches
      sort(unique(zipMap.dat1$Utility), decreasing=F)
      sort(unique(id_zip.dat2$Utility), decreasing=F)
      
      ##  Fix mismatches
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "PUD NO 1 OF SKAMANIA CO")] <-
        "PUD #1 SKAMANIA COUNTY"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "TACOMA CITY OF")] <-
        "CITY OF TACOMA"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "ELMHURST MUTUAL POWER  LIGHT CO")] <-
        "ELMHURST MUTUAL POWER AND LIGHT"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "FLATHEAD ELECTRIC COOP INC")] <-
        "FLATHEAD ELECTRIC COOPERATIVE"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "GLACIER ELECTRIC COOP INC")] <-
        "GLACIER ELECTRIC COOP"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "LAKEVIEW LIGHT  POWER")] <-
        "LAKEVIEW POWER & LIGHT"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "")] <-
        "MISSION VALLEY POWER"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "MISSOULA ELECTRIC COOP INC")] <-
        "MISSOULA ELECTRIC COOP"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "NORTHWESTERN CORPORATION")] <-
        "NORTHWESTERN ENERGY"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "OHOP MUTUAL LIGHT COMPANY INC")] <-
        "OHOP MUTUAL LIGHT CO"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "PUGET SOUND ENERGY INC")] <-
        "PUGET SOUND ENERGY"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "SEATTLE CITY OF")] <-
        "SEATTLE CITY LIGHT"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "SNOHOMISH COUNTY PUD NO 1")] <-
        "SNOHOMISH PUD"
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "USBIAMISSION VALLEY POWER")] <-
        "MISSION VALLEY POWER"
      

      # id_zip.dat1$ZIPCode[which(id_zip.dat1$Utility == "NORTHWEST NATURAL")] #Not in id_zip
      # which(id_zip.dat1$ZIPCode[which(id_zip.dat1$Utility == "NORTHWESTERN ENERGY")] %in%
      #       zipMap.dat1$ZIPCode[which(zipMap.dat1$Utility == "NORTHWESTERN CORPORATION")])
      # chkZip <- c(id_zip.dat1$ZIPCode[which(id_zip.dat1$Utility == "NORTHWESTERN ENERGY")])[c(10,11,13,14)]
      # id_zip.dat1$Utility[chkZip]
      
      ##  QA/QC: How many missing?
      length(id_zip.dat2$Utility[which(id_zip.dat2$Utility == "-- DID NOT ENTER! --")])  ## 0 not entered
      
      
#############################################################################################
# Merge data and assign electric utility
#############################################################################################

# Join ZIP codes to building type data
samp.dat.0       <- left_join(cleanRBSA.dat1, id_zip.dat2, by="CK_Cadmus_ID")
# Join ZIP mapping to previous step
samp.dat.1       <- left_join(samp.dat.0, zipMap.dat1, by="ZIPCode")
samp.dat.1$tally <- rep(1, nrow(samp.dat.1))
head(samp.dat.1)  ##  959 rows

##  Replace missing utility from sample data with utility from zip code mapping
# missingInd <- which(samp.dat.1$Utility.x == "-- DID NOT ENTER! --")
samp.dat.2 <- samp.dat.1
# samp.dat.2$Utility.x[missingInd] <- samp.dat.2$Utility.y[missingInd]

# Remove full row duplicates
dupRows    <- which(duplicated(samp.dat.2))  
samp.dat.3 <- samp.dat.2[-dupRows,]   ##  862 rows

##  Cust ID's with duplicates
dupCustIDs <- unique(samp.dat.3$CK_Cadmus_ID[which(duplicated(samp.dat.3$CK_Cadmus_ID))])
dupUtil.0  <- samp.dat.3[which(samp.dat.3$CK_Cadmus_ID %in% dupCustIDs),]


########################################################################################
##                                                                                    ##
##  STEP 1:
##  IF    Cust data utility is "-- DID NOT ENTER! --"
##        ->  Replace with ZIP map utility (Completed above)
##                                                                                    ##
##  STEP 2:
##  IF    ZIP map utility has duplicates
##        IF    Cust data has duplicates
##              ->  Tag for manual fix
##        ELSE  Use ZIP map utility
##                                                                                    ##
##  STEP 3:
##  IF    ZIP map has no duplicates
##        IF    Cust data has no duplicates
##              ->  Tag for manual fix
##        ELSE  Use cust data utility
##                                                                                    ##
########################################################################################


# Initialize counter and output vector
cntr              <- 1
dupUtil.0$Utility <- rep("MISSING", nrow(dupUtil.0))

##  Create "Not In" operator   
"%notin%" <- Negate("%in%")   


##  For loops to assign utility as per above logic
##  STEP 2
for(cntr in 1:length(dupCustIDs)) {
  if("TRUE" %in% duplicated(dupUtil.0$Utility.y[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])])) {
    if("TRUE" %in% duplicated(dupUtil.0$Utility.x[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])])) {
        dupUtil.0$Utility[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])] <- "MANUAL FIX"
    } 
    else {
      dupUtil.0$Utility[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])] <- 
        dupUtil.0$Utility.y[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])]
    }
  }
}

##  STEP 3
for(cntr in 1:length(dupCustIDs)) {
  if("TRUE" %notin% duplicated(dupUtil.0$Utility.y[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])])) {
    if("TRUE" %notin% duplicated(dupUtil.0$Utility.x[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])])) {
      dupUtil.0$Utility[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])] <- "MANUAL FIX"
    } 
    else {
      dupUtil.0$Utility[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])] <- 
        dupUtil.0$Utility.x[which(dupUtil.0$CK_Cadmus_ID == dupCustIDs[cntr])]
    }
  }
}

##  Subset to ID and Utility column and merge back into sample data
names(dupUtil.0)
dupUtil.1  <- unique(dupUtil.0[,c(1,12)])
samp.dat.4 <- left_join(samp.dat.3, dupUtil.1, by="CK_Cadmus_ID")

##  For non-duplicates, use cust data
samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID %notin% dupCustIDs)] <- 
  samp.dat.4$Utility.x[which(samp.dat.4$CK_Cadmus_ID %notin% dupCustIDs)]

##########################################
##                                      ##
##  MANUAL FIXES FOR MISSING UTILITIES  ##
##                                      ##
##########################################

utilFix <- samp.dat.4[,which(names(samp.dat.4) %in% c("CK_Cadmus_ID"
                                                      , "ZIPCode"
                                                      , "Utility.x"
                                                      , "Utility.y"))]

##  Based on inspection
samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID =="BPS25495 OS BPA")]  <- "CITY OF TACOMA"
samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID =="WH3590")]           <- "PUGET SOUND ENERGY"

## Based on others in ZIP
samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID =="SG0200 OS SCL")]    <- "SEATTLE CITY LIGHT"
samp.dat.4$ZIPCode[which(samp.dat.4$CK_Cadmus_ID =="SG0200 OS SCL")]    <- 98118
samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID =="SL2122 OS SCL")]    <- "SEATTLE CITY LIGHT"
samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID =="SE2163 OS SCL")]    <- "SEATTLE CITY LIGHT"
samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID =="SL1673 OS SCL")]    <- "SEATTLE CITY LIGHT"
samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID =="WH1221")]           <- "CITY OF TACOMA"

##  Based on which sample round
samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID =="SG0048 OS SCL")]    <- "SEATTLE CITY LIGHT"

##  Still needs fix
# samp.dat.4$Utility[which(samp.dat.4$CK_Cadmus_ID =="SE2257 OS SCL")]    <- "SCL -OR- PSE"


##  Fix BPA vs IOU
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$Utility == "SEATTLE CITY LIGHT")]     <- "BPA"
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$Utility == "SNOHOMISH PUD")]          <- "BPA"
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$Utility == "PUGET SOUND ENERGY")]     <- "IOU"
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$Utility == "NORTHWESTERN ENERGY")]    <- "IOU"
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$Utility == "MISSION VALLEY POWER")]   <- "BPA"
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$Utility == "MISSOULA ELECTRIC COOP")] <- "BPA"

##  Remove old utility columns and duplicate rows
samp.dat.5 <- unique(samp.dat.4[,-which(names(samp.dat.4) %in% c("Utility.x", "Utility.y"))])
which(duplicated(samp.dat.5$CK_Cadmus_ID))

##  Last fixes
##  Still needs fix
# samp.dat.5$Utility[which(samp.dat.5$CK_Cadmus_ID == "MS3162 OS")]    <- "MISSOULA ELECTRIC COOP -OR- NORTHWESTERN ENERGY"
# samp.dat.5$BPA_vs_IOU[which(samp.dat.5$CK_Cadmus_ID == "MS3162 OS")] <- NA
##  Remove invalid ZIP and leave correct record
# samp.dat.5 <- samp.dat.5[-which(samp.dat.5$CK_Cadmus_ID == "SG0200 OS SCL" &
                                # is.na(samp.dat.5$State)),] ##

# Summarize sample counts
sampCounts.0 <- summarise(group_by(samp.dat.5
                                 , BuildingType, State, Region, Utility, BPA_vs_IOU)
                        , n = sum(tally))



#############################################################################################
# Merge data and count sample sizes
#############################################################################################

# Subset and define strata
# Initialize the vector for strata names
sampCounts.0$Strata <- rep("MISSING", nrow(sampCounts.0))
unique(sampCounts.0$Utility)

      ##  QA/QC: Make sure oversample utilities are in expected BPA territory
      sampCounts.0$BPA_vs_IOU[grep("SEATTLE CITY LIGHT", sampCounts.0$Utility)] == "BPA"
      sampCounts.0$BPA_vs_IOU[grep("SNOHOMISH",       sampCounts.0$Utility)]    == "BPA"
      sampCounts.0$BPA_vs_IOU[grep("PUGET SOUND",     sampCounts.0$Utility)]    == "IOU"

# Assign strata
sampCounts.0$Strata[which(sampCounts.0$BPA_vs_IOU == "BPA")]       <- "BPA"
sampCounts.0$Strata[which(sampCounts.0$BPA_vs_IOU == "IOU")]       <- "Non-BPA"
sampCounts.0$Strata[grep("SNOHOMISH",          sampCounts.0$Utility)] <- "SnoPUD"
sampCounts.0$Strata[grep("PUGET SOUND",        sampCounts.0$Utility)] <- "PSE"
sampCounts.0$Strata[grep("SEATTLE CITY LIGHT", sampCounts.0$Utility)] <- "SCL"

# Get sample sizes in each strata
sampCounts.1 <- summarise(group_by(sampCounts.0, 
                                   BuildingType, State, Region, Strata)
                          , n.h = sum(n))
      
      
#############################################################################################
# Count population sizes
#############################################################################################

# Join ZIP codes to building type data
names(zipMap.dat)
popCounts.0 <- summarise(group_by(zipMap.dat, State, Region, Utility, BPA_vs_IOU)
                         , SF.pop = round(sum(SF.N.adj), 0)
                         , MH.pop = round(sum(MH.N.adj), 0)
                         , MF.pop = round(sum(MF.N.adj), 0)
                         )

# Initialize the vector for strata names
popCounts.0$Strata <- rep("MISSING", nrow(popCounts.0))

      ##  QA/QC: Make sure oversample utilities are in expected BPA territory
      popCounts.0$BPA_vs_IOU[grep("SEATTLE CITY", popCounts.0$Utility)] == "BPA"
      popCounts.0$BPA_vs_IOU[grep("SNOHOMISH",    popCounts.0$Utility)] == "BPA"
      popCounts.0$BPA_vs_IOU[grep("PUGET SOUND",  popCounts.0$Utility)] == "IOU"

# Assign strata
popCounts.0$Strata[which(popCounts.0$BPA_vs_IOU == "BPA")]    <- "BPA"
popCounts.0$Strata[which(popCounts.0$BPA_vs_IOU == "IOU")]    <- "Non-BPA"
popCounts.0$Strata[grep("SNOHOMISH",    popCounts.0$Utility)] <- "SnoPUD"
popCounts.0$Strata[grep("PUGET SOUND",  popCounts.0$Utility)] <- "PSE"
popCounts.0$Strata[grep("SEATTLE CITY", popCounts.0$Utility)] <- "SCL"

# Get sample sizes in each strata
popCounts.1 <- summarise(group_by(popCounts.0, 
                                   State, Region, Strata)
                         , N_SF.h = sum(SF.pop)
                         , N_MH.h = sum(MH.pop)
                         , N_MF.h = sum(MF.pop))



#############################################################################################
# Combine sample and population counts
#############################################################################################

# Join pop to samp by state/region/strata
names(sampCounts.1)
names(popCounts.1)
allCounts.0 <- left_join(sampCounts.1, popCounts.1, by=c("State", "Region", "Strata"))

# Set pop  size in strata based on bldg type
# Initialize the vector for pop sizes
allCounts.0$N.h <- rep("MISSING", nrow(allCounts.0))

# Single Family Homes
allCounts.0$N.h[which(allCounts.0$BuildingType == "Single Family")] <- 
  allCounts.0$N_SF.h[which(allCounts.0$BuildingType == "Single Family")]

# Manufactured Homes
allCounts.0$N.h[which(allCounts.0$BuildingType == "Manufactured")] <- 
  allCounts.0$N_MH.h[which(allCounts.0$BuildingType == "Manufactured")]

# Multifamily Homes
allCounts.0$N.h[which(allCounts.0$BuildingType == "Multifamily")] <- 
  allCounts.0$N_MF.h[which(allCounts.0$BuildingType == "Multifamily")]

# Remove unnecessary columns
allCounts.1 <- allCounts.0[,-which(names(allCounts.0) %in% c("N_SF.h", "N_MH.h", "N_MF.h"))]
allCounts.1$n.h <- as.numeric(allCounts.1$n.h)
allCounts.1$N.h <- as.numeric(allCounts.1$N.h)

# Compute expansion weights
allCounts.1$w.h <- round(allCounts.1$N.h/allCounts.1$n.h, 2)


