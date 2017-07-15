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
names(zipMap.dat)   <- c("ZIPCode", "City", "County", "State", "Region", "FERC_ID", "Utility"
                         , "Fraction", "BPA_vs_IOU", "SF.N", "MF.N", "MH.N", "SF.N.adj"
                         , "MF.N.adj", "MH.N.adj")
head(zipMap.dat)

# Clean up data
zipMap.dat$Utility <- trimws(toupper(zipMap.dat$Utility))
zipMap.dat$Utility <- gsub('[[:punct:]]+', '', zipMap.dat$Utility)
zipMap.dat$ZIPCode <- as.numeric(zipMap.dat$ZIPCode)  

zipMap.dat1 <- data.frame("ZIPCode"              = zipMap.dat$ZIPCode
                          , "State"          = zipMap.dat$State
                          , "Region"         = zipMap.dat$Region
                          , "Utility"        = zipMap.dat$Utility
                          , "BPA_vs_IOU"     = zipMap.dat$BPA_vs_IOU
                          , stringsAsFactors = F)


#############################################################################################
# Merge data and count sample sizes
#############################################################################################

# Join ZIP codes to building type data
samp.dat.0 <- left_join(cleanRBSA.dat1, id_zip.dat1, by="CK_Cadmus_ID")
# Join ZIP mapping to previous step
samp.dat.1 <- left_join(samp.dat.0, zipMap.dat1, by="ZIPCode")
samp.dat.1$tally <- rep(1, nrow(samp.dat.1))
head(samp.dat.1)

# Summarize sample counts
sampCounts.0 <- summarise(group_by(samp.dat.1
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
      sampCounts.0$BPA_vs_IOU[grep("SEATTLE CITY OF", sampCounts.0$Utility)] == "IOU"
      sampCounts.0$BPA_vs_IOU[grep("SNOHOMISH",       sampCounts.0$Utility)] == "IOU"
      sampCounts.0$BPA_vs_IOU[grep("PUGET SOUND",     sampCounts.0$Utility)] == "BPA"

# Assign strata
sampCounts.0$Strata[which(sampCounts.0$BPA_vs_IOU == "BPA")]       <- "BPA"
sampCounts.0$Strata[which(sampCounts.0$BPA_vs_IOU == "IOU")]       <- "Non-BPA"
sampCounts.0$Strata[grep("SNOHOMISH",       sampCounts.0$Utility)] <- "SnoPUD"
sampCounts.0$Strata[grep("PUGET SOUND",     sampCounts.0$Utility)] <- "PSE"
sampCounts.0$Strata[grep("SEATTLE CITY OF", sampCounts.0$Utility)] <- "SCL"

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
      popCounts.0$BPA_vs_IOU[grep("SEATTLE CITY OF", popCounts.0$Utility)] == "IOU"
      popCounts.0$BPA_vs_IOU[grep("SNOHOMISH",       popCounts.0$Utility)] == "IOU"
      popCounts.0$BPA_vs_IOU[grep("PUGET SOUND",     popCounts.0$Utility)] == "BPA"

# Assign strata
popCounts.0$Strata[which(popCounts.0$BPA_vs_IOU == "BPA")]       <- "BPA"
popCounts.0$Strata[which(popCounts.0$BPA_vs_IOU == "IOU")]       <- "Non-BPA"
popCounts.0$Strata[grep("SNOHOMISH",       popCounts.0$Utility)] <- "SnoPUD"
popCounts.0$Strata[grep("PUGET SOUND",     popCounts.0$Utility)] <- "PSE"
popCounts.0$Strata[grep("SEATTLE CITY OF", popCounts.0$Utility)] <- "SCL"

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


