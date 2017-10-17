#############################################################################################
##  Title:            sampleCounts.R                      
##  Author:           Andrew Bernath, Cadmus Group               
##  Created:          07/05/2017
##  Updated:                                             
##  Billing Code(s):  
##  Description:      Code to import and count pop and sample sizes for each 
##                    post-strata region
#############################################################################################
################################################################################
# Use FILEPATHS from Step 1 for folders and file names of:
# - METER data
# - ZIP Code data (with pop counts from ACS)
# - output data
################################################################################

weightedData <- function(itemData){

##  Clear variables
# rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")


#source
SourcePath <- "C:/Users/Casey.Stevens/Documents/Git/Cadmus-6000-2017/Cadmus-6000-2017/Code/Table Code"
source(file.path(SourcePath, "SourceCode.R"))
source("Code/Table Code/SourceCode.R")

# Call file names
popZIP.datMap <- "ZIP_Code_Utility_Mapping.xlsx"

meter.export  <- "METERS_2017.06.16.xlsx"
bldg.export   <- "SITES_2017.06.16.xlsx"

#############################################################################################
# Import, Subset, CLean Data
#############################################################################################

# Import clean RBSA data
# cleanRBSA.dat <- read.xlsx(paste(filepathCleanData
#                                  , paste("clean.rbsa.data.unweighted", rundate, ".xlsx", sep = "")
#                                  , sep="/")
# )
cleanRBSA.dat <- itemData
names(cleanRBSA.dat)

cleanRBSA.dat1 <- data.frame(cleanRBSA.dat, stringsAsFactors = F)

# clean and count Cadmus IDs
cleanRBSA.dat1$CK_Cadmus_ID   <- trimws(toupper(cleanRBSA.dat1$CK_Cadmus_ID))
length(unique(cleanRBSA.dat1$CK_Cadmus_ID))  ## 601 unique ID's

# Import ID and ZIP data
id_zip.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, meter.export), sheet=1)
length(unique(id_zip.dat$CK_Cadmus_ID)) #568

# subset to necessary columns
id_zip.dat0.1 <- unique(data.frame("CK_Cadmus_ID"     = id_zip.dat$CK_Cadmus_ID
                            , "Utility"        = id_zip.dat$Utility
                            , "MeterType"      = id_zip.dat$Type
                            , stringsAsFactors = F))

id_zip.dat1 <- left_join(cleanRBSA.dat1, id_zip.dat0.1, by = "CK_Cadmus_ID")
length(unique(id_zip.dat1$CK_Cadmus_ID))
colnames(id_zip.dat1)[which(colnames(id_zip.dat1) == "ZIP")] <- "ZIPCode"

# clean and count Cadmus IDs, clean utility and meter type
id_zip.dat1$CK_Cadmus_ID <- trimws(toupper(id_zip.dat1$CK_Cadmus_ID))
id_zip.dat1$Utility      <- trimws(toupper(id_zip.dat1$Utility))
id_zip.dat1$MeterType    <- trimws(toupper(id_zip.dat1$MeterType))
id_zip.dat1$ZIPCode      <- as.numeric(substr(id_zip.dat1$ZIPCode, 1, 5))  ## Remove ZIP-Ext
length(unique(id_zip.dat1$CK_Cadmus_ID))  ## 601 unique respondent ID's

# Indicate invalid ZIP codes
id_zip.dat1$invalidZIP <- rep(0, nrow(id_zip.dat1))
id_zip.dat1$invalidZIP[which(id_zip.dat1$ZIPCode < 10000)] <- 1
id_zip.dat1$invalidZIP[which(is.na(id_zip.dat1$ZIPCode))] <- 1
unique(id_zip.dat1$invalidZIP)


### just for now -- come back when final data is in and delete the two following lines.
id_zip.dat1$MeterType[which(is.na(id_zip.dat1$MeterType))]          <- "UNKNOWN"
id_zip.dat1$MeterType[which(id_zip.dat1$MeterType == "THERMOSTAT")] <- "UNKNOWN"

# Subset to electric meters only
# When final data comes in, all sites will have an identified electric utility
id_zip.dat2.0 <- id_zip.dat1[which(id_zip.dat1$MeterType != "THERMOSTAT"),]
id_zip.dat2.1 <- id_zip.dat2.0[with(id_zip.dat2.0, order(MeterType, decreasing = F)),]
which(duplicated(id_zip.dat2.1$CK_Cadmus_ID))
id_zip.dat2   <- id_zip.dat2.1[which(!(duplicated(id_zip.dat2.1$CK_Cadmus_ID))),]
stopifnot(length(which(duplicated(id_zip.dat2$CK_Cadmus_ID))) == 0)
stopifnot(length(which(id_zip.dat2$MeterType == "GAS")) == 0)

      ##  QA/QC: Any lost customers?
      length(unique(id_zip.dat1$CK_Cadmus_ID)) == length(unique(id_zip.dat2$CK_Cadmus_ID))
      
      length(unique(id_zip.dat1$CK_Cadmus_ID)) - length(unique(id_zip.dat2$CK_Cadmus_ID))
      ## This should be zero when the full data come in - if not, export list, send to Rietz 
      ## 0
      
      
# Import ZIP code mapping
zipMap.dat <- read.xlsx(xlsxFile = file.path(filepathWeightingDocs, popZIP.datMap), sheet=1)
names(zipMap.dat)   <- c("ZIPCode"
                         , "City"
                         , "County"
                         , "State"
                         , "Region"
                         , "FERC_ID"
                         , "Utility"
                         , "Fraction"
                         , "BPA_vs_IOU"
                         , "SF.N"
                         , "MF.N"
                         , "MH.N"
                         , "SF.N.adj"
                         , "MF.N.adj"
                         , "MH.N.adj")
head(zipMap.dat)

zipMap.dat$State[which(is.na(zipMap.dat$Region))]
table(zipMap.dat$Region, zipMap.dat$State)

# Clean up data: clean utility, remove any punctuation from utility, make zip codes numeric
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
      #export these lists for Rietz to convert to the correct names
      
##  Andrew: were these reviewed with Rietz or Steve?, are there any others that could have been missed?
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
## Double check this is right --  Mission Valley Power     
      zipMap.dat1$Utility[which(zipMap.dat1$Utility == "USBIAMISSION VALLEY POWER")] <-
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

      
      ##  QA/QC: How many missing?
      length(id_zip.dat2$Utility[which(id_zip.dat2$Utility == "-- DID NOT ENTER! --")])  ## 0 not entered
      
     
      
       
#############################################################################################
# Merge data and assign electric utility
#############################################################################################

# Join ZIP codes to cleaned building type data
samp.dat.0       <- id_zip.dat2
# Join ZIP mapping to previous step
samp.dat.1       <- left_join(samp.dat.0, zipMap.dat1, by="ZIPCode")
samp.dat.1$tally <- rep(1, nrow(samp.dat.1))
head(samp.dat.1)  
nrow(samp.dat.1)##  959 rows (old) - 9/12 671 rows - 10/4 461 rows
colnames(samp.dat.1)
colnames(samp.dat.1) <- c("CK_Cadmus_ID"
                          ,"HomeType"
                          ,"HomeYearBuilt"
                          ,"State"
                          ,"BuildingType"
                          ,"HomeYearBuilt_bins1"
                          ,"HomeYearBuilt_bins2"
                          ,"HomeYearBuilt_bins3"
                          ,"HomeYearBuilt_bins_MF"
                          ,"BuildingHeight"
                          ,"ZIPCode"
                          ,"Utility.Customer.Data"
                          ,"MeterType"
                          ,"invalidZIP"
                          ,"Utility.Data.State"
                          ,"Region"
                          ,"Utility.ZIP.map"
                          ,"BPA_vs_IOU"
                          ,"tally")

#QAQC: check that no rows in the dataset are duplicates
stopifnot(length(which(duplicated(samp.dat.1))) == 0)

########################################################################################
##                                                                                    
##  STEP 1:
##  IF    Cust data utility is "-- DID NOT ENTER! --"
##        ->  Replace with ZIP map utility
##                                                                                    
##  STEP 2:
##  IF    ZIP map utility has multiple utilities
##        IF    Cust data has multiple utilities
##              ->  Tag for manual fix (i.e. export to Rietz)
##
##  STEP 3:
##  IF    ZIP map utility has multiple utilities
##        IF    Cust data has one utility
##              ->  Use Cust data
##                                                                                    
##
##  IF    ZIP map has one unique utility
##        IF    Cust data has multiple utilities <- IMPOSSIBLE! - we checked id_zip.dat2 in line 84
##
##  IF    Customer data has one unique utility
##        IF    ZIP MAP data has one unique utility but different from customer data utility 
##              -> Use customer data, corrected below after merge of samp.dat.3 in lines 278-279 
##                                                                                    
########################################################################################

## STEP 1:
#  Replace missing utility from sample data with utility from zip code mapping
missingInd <- which(samp.dat.1$Utility.Customer.Data == "-- DID NOT ENTER! --")
length(missingInd) 

samp.dat.2 <- samp.dat.1
if(length(missingInd) > 0){
  samp.dat.2$Utility.Customer.Data[missingInd] <- samp.dat.2$Utility.ZIP.map[missingInd]
}



## Prep for steps 2 and 3
##  Cust ID's with duplicates
dupCustIDs <- unique(samp.dat.2$CK_Cadmus_ID[which(duplicated(samp.dat.2$CK_Cadmus_ID))])
dupData  <- samp.dat.2[which(samp.dat.2$CK_Cadmus_ID %in% dupCustIDs),]


# Initialize counter and output vector
cntr              <- 1
dupData$Utility <- rep("MISSING", nrow(dupData))



##  For loops to assign utility as per above logic
##  STEP 2 - if the utility zip map has more than a single utility and customer data has more than one, flag for manual fix
cntr = 1
for(cntr in 1:length(dupCustIDs)) {
  if("TRUE" %in% duplicated(dupData$Utility.ZIP.map[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])])) {
        dupData$Utility[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])] <- "MANUAL FIX"
    }
}

##  STEP 3 - if the utility zip map has more than a single utility and customer data has only one utility - use customer data
for(cntr in 1:length(dupCustIDs)) {
  if("TRUE" %notin% duplicated(dupData$Utility.ZIP.map[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])])) {
      dupData$Utility[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])] <-
        dupData$Utility.Customer.Data[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])]
    }
}

##  Subset to ID and Utility column and merge back into sample data
names(dupData)
dupData.1  <- unique(dupData[which(colnames(dupData) %in% c("CK_Cadmus_ID", "Utility"))])
names(dupData.1)
samp.dat.3 <- left_join(samp.dat.2, dupData.1, by = "CK_Cadmus_ID")

##  For non-duplicates, use cust data
samp.dat.3$Utility[which(samp.dat.3$CK_Cadmus_ID %notin% dupCustIDs)] <-
  samp.dat.3$Utility.Customer.Data[which(samp.dat.3$CK_Cadmus_ID %notin% dupCustIDs)]


##########################################
##                                      ##
##  MANUAL FIXES FOR MISSING UTILITIES  ##
##                                      ##
##########################################

##  Fix BPA vs IOU
samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "SEATTLE CITY LIGHT")]     <- "BPA"
samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "SNOHOMISH PUD")]          <- "BPA"
samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "PUGET SOUND ENERGY")]     <- "IOU"
samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "NORTHWESTERN ENERGY")]    <- "IOU"
samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "MISSION VALLEY POWER")]   <- "BPA"
samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "MISSOULA ELECTRIC COOP")] <- "BPA"

#if State is missing, but utility data state is not missing, replace customer state with utility state
samp.dat.3$State[which(is.na(samp.dat.3$State))] <-
  samp.dat.3$Utility.Data.State[which(is.na(samp.dat.3$State))]


      ##  Manual updates early (using partial data)
      samp.dat.3$State[which(samp.dat.3$CK_Cadmus_ID  =="SE2257 OS SCL")] <- "WA"
      samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID =="SE2257 OS SCL")] <- "PS"
      samp.dat.3$State[which(samp.dat.3$CK_Cadmus_ID  =="SG0200 OS SCL")] <- "WA"
      samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID =="SG0200 OS SCL")] <- "PS"
      samp.dat.3$State[which(samp.dat.3$CK_Cadmus_ID  =="SL0418 OS SCL")] <- "WA"
      samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID =="SL0418 OS SCL")] <- "PS"
      samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID =="SL2263 OS SCL")] <- "PS"

##  Find remaining missing states and regions in full data
#     NOTE: there are cases where zip codes found in our data collection did not show up in the population data from ACS.
#     Make sure this gets written in weighting section of report
missing.region <- samp.dat.3$CK_Cadmus_ID[which(is.na(samp.dat.3$Region))]
#if region is missing in MT or ID, then replace with correct region
# otherwise export CK_Cadmus_ID, send to Rietz to get correct region information
samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID %in% missing.region & samp.dat.3$State == "MT")] <- "W"
samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID %in% missing.region & samp.dat.3$State == "ID")] <- "-"
#print this: send to Rietz (MF does not need region)
samp.dat.3[which(samp.dat.3$CK_Cadmus_ID %in% missing.region & samp.dat.3$State %in% c("WA", "OR") & samp.dat.3$BuildingType != "Multifamily"),]




##  Remove old utility columns and duplicate rows
samp.dat.4 <- unique(samp.dat.3[,-which(names(samp.dat.3) %in% c("Utility.Customer.Data", "Utility.ZIP.map"))])
stopifnot(length(which(duplicated(samp.dat.4$CK_Cadmus_ID))) == 0)

dup.ind <- samp.dat.4$CK_Cadmus_ID[which(duplicated(samp.dat.4$CK_Cadmus_ID))]
samp.dat.4[which(samp.dat.4$CK_Cadmus_ID %in% dup.ind),]


#########################################
# All of this needs to be reviewed
# when the final data comes in
#########################################

## Fix issues with BPA/IOU
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$CK_Cadmus_ID == "PNM20364 OS PSE")] <- "IOU"
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$CK_Cadmus_ID == "PSM26731 CORE")]   <- "BPA"
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$CK_Cadmus_ID == "PSM35054 CORE")]   <- "BPA"
samp.dat.4$BPA_vs_IOU[which(samp.dat.4$CK_Cadmus_ID == "BPS26690 OS BPA")] <- "BPA"

samp.dat.5 <- unique(samp.dat.4)

##  QA/QC: Make sure oversample utilities are in expected BPA territory
stopifnot(all(samp.dat.5$BPA_vs_IOU[grep("SEATTLE CITY LIGHT", samp.dat.5$Utility)] == "BPA"))
stopifnot(all(samp.dat.5$BPA_vs_IOU[grep("SNOHOMISH",       samp.dat.5$Utility)]    == "BPA"))
stopifnot(all(samp.dat.5$BPA_vs_IOU[grep("PUGET SOUND",     samp.dat.5$Utility)]    == "IOU"))

# Subset and define Territory
# Initialize the vector for strata names
samp.dat.5$Territory <- rep("MISSING", nrow(samp.dat.5))
unique(samp.dat.5$Utility)


# Assign Territory
samp.dat.5$Territory[which(samp.dat.5$BPA_vs_IOU == "BPA")]          <- "BPA"
samp.dat.5$Territory[which(samp.dat.5$BPA_vs_IOU == "IOU")]          <- "Non-BPA.Non-PSE"
samp.dat.5$Territory[grep("SNOHOMISH",          samp.dat.5$Utility)] <- "SnoPUD"
samp.dat.5$Territory[grep("PUGET SOUND",        samp.dat.5$Utility)] <- "PSE"
samp.dat.5$Territory[grep("SEATTLE CITY LIGHT", samp.dat.5$Utility)] <- "SCL"
unique(samp.dat.5$Territory)

##incorrect territory: If WA and PS territory cannot be Non-BPA.Non-PSE
territory.ind <- samp.dat.5$CK_Cadmus_ID[which(samp.dat.5$State == "WA" & samp.dat.5$Region == "PS" & samp.dat.5$Territory == "Non-BPA.Non-PSE")]
incorrect.territory <- samp.dat.5$CK_Cadmus_ID[which(samp.dat.5$CK_Cadmus_ID %in% territory.ind)]
# incorrect.territory1 <- samp.dat.5[which(samp.dat.5$CK_Cadmus_ID %in% territory.ind),]
pse.territory       <- samp.dat.5$CK_Cadmus_ID[grep ("PSE",samp.dat.5$CK_Cadmus_ID)]

territory.sub <- samp.dat.5[which(samp.dat.5$CK_Cadmus_ID %in% incorrect.territory),]
territory.sub1 <- territory.sub[which(colnames(territory.sub) %in% c("CK_Cadmus_ID", "Territory"))]

territory.sub1$Territory[grep("PSE", territory.sub1$CK_Cadmus_ID)] <- "PSE"
territory.sub1$Territory[grep("BPA", territory.sub1$CK_Cadmus_ID)] <- "BPA"
territory.sub1$Territory[grep("CORE|WM", territory.sub1$CK_Cadmus_ID)] <- "BPA"

samp.dat.5$Territory[which(samp.dat.5$CK_Cadmus_ID %in% incorrect.territory)] <- territory.sub1$Territory[which(territory.sub1$CK_Cadmus_ID %in% incorrect.territory)]


#########################################
# 
# End review
# 
#########################################


samp.dat.6 <- data.frame(samp.dat.5, stringsAsFactors = F)
length(unique(samp.dat.6$CK_Cadmus_ID))


# Get sample sizes in each strata
sampCounts.1 <- summarise(group_by(samp.dat.6, 
                                   BuildingType, State, Region, Territory)
                          , n.h = sum(tally))
      
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

# Initialize the vector for Territory names
popCounts.0$Territory <- rep("MISSING", nrow(popCounts.0))

      ##  QA/QC: Make sure oversample utilities are in expected BPA territory
      stopifnot(all(popCounts.0$BPA_vs_IOU[grep("SEATTLE CITY", popCounts.0$Utility)] == "BPA"))
      stopifnot(all(popCounts.0$BPA_vs_IOU[grep("SNOHOMISH",    popCounts.0$Utility)] == "BPA"))
      stopifnot(all(popCounts.0$BPA_vs_IOU[grep("PUGET SOUND",  popCounts.0$Utility)] == "IOU"))

# Assign Territory
popCounts.0$Territory[which(popCounts.0$BPA_vs_IOU == "BPA")]    <- "BPA"
popCounts.0$Territory[which(popCounts.0$BPA_vs_IOU == "IOU")]    <- "Non-BPA.Non-PSE"
popCounts.0$Territory[which(popCounts.0$BPA_vs_IOU == "NOT BPA")]<- "Non-BPA.Non-PSE"
popCounts.0$Territory[which(is.na(popCounts.0$BPA_vs_IOU))]      <- "Non-BPA.Non-PSE"
popCounts.0$Territory[grep("SNOHOMISH",    popCounts.0$Utility)] <- "SnoPUD"
popCounts.0$Territory[grep("PUGET SOUND",  popCounts.0$Utility)] <- "PSE"
popCounts.0$Territory[grep("SEATTLE CITY", popCounts.0$Utility)] <- "SCL"
unique(popCounts.0$Territory)

# Get sample sizes in each Territory
popCounts.1 <- summarise(group_by(popCounts.0, 
                                   State, Region, Territory)
                         , N_SF.h = sum(SF.pop)
                         , N_MH.h = sum(MH.pop)
                         , N_MF.h = sum(MF.pop))


popMelt <- melt(popCounts.1, id.vars = c("State", "Region", "Territory"))
popMelt$BuildingType <- NA
popMelt$BuildingType[grep("SF", popMelt$variable)] <- "Single Family"
popMelt$BuildingType[grep("MF", popMelt$variable)] <- "Multifamily"
popMelt$BuildingType[grep("MH", popMelt$variable)] <- "Manufactured"




#############################################################################################
# Combine population and sample sizes by Territory
#############################################################################################

total.counts <- full_join(popMelt, sampCounts.1, by = c("BuildingType"
                                                        ,"State"
                                                        ,"Region"
                                                        ,"Territory"))

## check that there are no NA's in final sample sizes
## Put zero as a placeholder until final comes in
total.counts$n.h[which(is.na(total.counts$n.h))] <- 0


colnames(total.counts)[which(colnames(total.counts) == "value")] <- "N.h"

#Note: This will be written out
final.counts <- total.counts[which(!(colnames(total.counts) %in% c("variable")))]


#############################################################################################
# Cmerge pop and sample sizes onto cleaned data
# export final counts and final data
#############################################################################################

samp.dat.7 <- left_join(samp.dat.6, final.counts, by = c("BuildingType"
                                                         ,"State"
                                                         ,"Region"
                                                         ,"Territory"))

# samp.dat.8 <- samp.dat.7[which(!(is.na(samp.dat.7$N.h))),]

samp.dat.final <- samp.dat.7
unique(samp.dat.final$HomeType)

############    ADD FAKE KWH USAGE ONTO SAMP.DAT.FINAL ######################
samp.dat.final$kWh_usage_fake <- ceiling(runif(n = nrow(samp.dat.final),
                                               min = 3000, 
                                               max = 30000))

samp.dat.final$kWh_NAC_fake <- samp.dat.final$kWh_usage_fake * runif(n = nrow(samp.dat.final),
                                                                     min = .8,
                                                                     max = .95)

samp.dat.final$Heating_kWh_fake <- samp.dat.final$kWh_NAC_fake * runif(n = nrow(samp.dat.final),
                                                                         min = .05,
                                                                         max = .8)

samp.dat.final$Heating_kWh_fake[which(samp.dat.final$Heating_kWh_fake < 3000)] <- 0
samp.dat.final$Cooling_kWh_fake <- samp.dat.final$kWh_NAC_fake * runif(n = nrow(samp.dat.final),
                                                                         min = .05,
                                                                         max = .5)
samp.dat.final$Cooling_kWh_fake[which(samp.dat.final$Cooling_kWh_fake < 1500)] <- 0

samp.dat.final$Heating_kWh_fake[which(samp.dat.final$kWh_NAC_fake -
                                        samp.dat.final$Heating_kWh_fake - 
                                        samp.dat.final$Cooling_kWh_fake < 0)] <- 0


samp.dat.final$therm_usage_fake <- ceiling(runif(n = nrow(samp.dat.final),
                                               min = 200, 
                                               max = 3000))
samp.dat.final$therm_NAC_fake <- samp.dat.final$therm_usage_fake * runif(n = nrow(samp.dat.final),
                                                                     min = .8,
                                                                     max = .95)

samp.dat.final$Heating_therm_fake <- samp.dat.final$therm_NAC_fake * runif(n = nrow(samp.dat.final),
                                                                             min = .1,
                                                                             max = .9)
samp.dat.final$Heating_therm_fake[which(samp.dat.final$Heating_therm_fake < 300)] <- 0


# ##  Export clean data merged with weights
# write.xlsx(samp.dat.final, paste(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)
# 
# ##  Export 
# write.xlsx(final.counts, paste(filepathCleanData, paste("weights.data", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)

}

