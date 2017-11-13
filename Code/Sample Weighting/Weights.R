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
# itemData <- item223.dat2[which(colnames(item223.dat2) %notin% c("Nonres.Area"
#                                                                 ,"Ind"))]
weightedData <- function(itemData){
  
  rundate <-  format(Sys.time(), "%d%b%y")
  options(scipen=999)
  
  ##  Create "Not In" operator
  "%notin%" <- Negate("%in%")
  
  
  #source
  source("Code/Table Code/SourceCode.R")
  
  # Call file names
  popZIP.datMap <- "ZIP_Code_Utility_Mapping.xlsx"
  
  meter.export  <- "One Line Summary.xlsm"
  
  #Bring in R-value table
  UtilityMap <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "UtilityMapping.xlsx"), sheet = 1)
  UtilityMap <- UtilityMap[,1:2]
  
  
  #############################################################################################
  # Import, Subset, CLean Data
  #############################################################################################
  
  # itemData <- item1.dat0
  cleanRBSA.dat <- itemData
  names(cleanRBSA.dat)
  
  cleanRBSA.dat1 <- data.frame(cleanRBSA.dat, stringsAsFactors = F)
  
  # clean and count Cadmus IDs
  cleanRBSA.dat1$CK_Cadmus_ID   <- trimws(toupper(cleanRBSA.dat1$CK_Cadmus_ID))
  length(unique(cleanRBSA.dat1$CK_Cadmus_ID))  
  
  # Import ID and ZIP data
  cadmus.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, meter.export), sheet=1)
  cadmus.dat$CK_Cadmus_ID <- trimws(toupper(cadmus.dat$Cadmus.ID))
  length(unique(cadmus.dat$CK_Cadmus_ID)) 
  
  # subset to necessary columns
  cadmus.dat0.1 <- unique(data.frame("CK_Cadmus_ID"     = cadmus.dat$CK_Cadmus_ID
                                     , "Utility"        = cadmus.dat$Electric.Utility
                                     , stringsAsFactors = F))
  
  cadmus.dat1 <- left_join(cleanRBSA.dat1, cadmus.dat0.1, by = "CK_Cadmus_ID")
  
  #check to see which are missing
  rbsa.data.ids      <- unique(cleanRBSA.dat1$CK_Cadmus_ID)
  one.line.data.ids  <- unique(cadmus.dat0.1$CK_Cadmus_ID)
  
  rbsa.data.ids[which(rbsa.data.ids %notin% one.line.data.ids)]
  
  
  length(unique(cadmus.dat1$CK_Cadmus_ID))
  colnames(cadmus.dat1)[which(colnames(cadmus.dat1) == "ZIP")] <- "ZIPCode"
  
  # clean and count Cadmus IDs, clean utility and meter type
  cadmus.dat1$CK_Cadmus_ID <- trimws(toupper(cadmus.dat1$CK_Cadmus_ID))
  cadmus.dat1$Utility      <- trimws(toupper(cadmus.dat1$Utility))
  length(unique(cadmus.dat1$CK_Cadmus_ID))  ## 601 unique respondent ID's
  
  which(duplicated(cadmus.dat1$CK_Cadmus_ID))
  cadmus.dat2   <- cadmus.dat1#[which(!(duplicated(cadmus.dat1$CK_Cadmus_ID))),]
  cadmus.dat2$Utility[which(cadmus.dat2$Utility == "PACIFIC POWER")] <- "PACIFICORP"
  cadmus.dat2$Utility[which(cadmus.dat2$Utility == "ROCKY MOUNTAIN POWER")] <- "PACIFICORP"
  # stopifnot(length(which(duplicated(cadmus.dat2$CK_Cadmus_ID))) == 0)
  
  ##  QA/QC: Any lost customers?
  stopifnot(length(unique(cadmus.dat1$CK_Cadmus_ID)) == length(unique(cadmus.dat2$CK_Cadmus_ID)))
  
  length(unique(cadmus.dat1$CK_Cadmus_ID)) - length(unique(cadmus.dat2$CK_Cadmus_ID))
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
                           , "Remove"
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
  zipMap.dat$Utility <- gsub('NO ', '', zipMap.dat$Utility)
  zipMap.dat$Utility <- gsub(' INC', '', zipMap.dat$Utility)
  cadmus.dat2$Utility <- gsub('[[:punct:]]+', '', cadmus.dat2$Utility)
  cadmus.dat2$Utility <- gsub('NO ', '', cadmus.dat2$Utility)
  cadmus.dat2$Utility <- gsub(' INC', '', cadmus.dat2$Utility)
  
  zipMap.dat1 <- data.frame("ZIPCode"          = zipMap.dat$ZIPCode
                            , "State"          = zipMap.dat$State
                            , "Region"         = zipMap.dat$Region
                            , "Utility"        = zipMap.dat$Utility
                            , "BPA_vs_IOU"     = zipMap.dat$BPA_vs_IOU
                            , stringsAsFactors = F)
  
  ##  QA/QC: Check names of utilities for mismatches
  sort(unique(zipMap.dat1$Utility), decreasing=F)
  sort(unique(cadmus.dat2$Utility), decreasing=F)
  #export these lists for Rietz to convert to the correct names
  
  sort(unique(cadmus.dat2$Utility[which(cadmus.dat2$Utility %notin% zipMap.dat1$Utility)]))
  
  
  ##  Andrew: were these reviewed with Rietz or Steve?, are there any others that could have been missed?
  ##  Fix mismatches
  zipMap.dat1$Utility[which(zipMap.dat1$Utility == "TACOMA CITY OF")] <-
    "CITY OF TACOMA"
  zipMap.dat1$Utility[which(zipMap.dat1$Utility == "ELMHURST MUTUAL POWER  LIGHT CO")] <-
    "ELMHURST MUTUAL POWER AND LIGHT"
  zipMap.dat1$Utility[which(zipMap.dat1$Utility == "FLATHEAD ELECTRIC COOP")] <-
    "FLATHEAD ELECTRIC COOPERATIVE"
  zipMap.dat1$Utility[which(zipMap.dat1$Utility == "LAKEVIEW LIGHT  POWER")] <-
    "LAKEVIEW POWER  LIGHT"
  zipMap.dat1$Utility[which(zipMap.dat1$Utility == "USBIAMISSION VALLEY POWER")] <-
    "MISSION VALLEY POWER"
  zipMap.dat1$Utility[which(zipMap.dat1$Utility == "NORTHWESTERN CORPORATION")] <-
    "NORTHWESTERN ENERGY"
  cadmus.dat2$Utility[which(cadmus.dat2$Utility == "SUN RIVER ELECTRIC COOP")] <-
    "NORTHWESTERN ENERGY"
  zipMap.dat1$Utility[which(zipMap.dat1$Utility == "OHOP MUTUAL LIGHT COMPANY")] <-
    "OHOP MUTUAL LIGHT CO"
  zipMap.dat1$Utility[which(zipMap.dat1$Utility == "SEATTLE CITY OF")] <-
    "SEATTLE CITY LIGHT"
  zipMap.dat1$Utility[which(zipMap.dat1$Utility == "SNOHOMISH COUNTY PUD 1")] <-
    "SNOHOMISH PUD"
  
  
  for (i in 1:length(UtilityMap$ACS.Data)){
    zipMap.dat1$Utility[which(zipMap.dat1$Utility == UtilityMap$ACS.Data[i])] <- UtilityMap$Map[i]
  }
  unique(zipMap.dat1$Utility)
  
  
  cadmus.dat3 <- cadmus.dat2[which(!is.na(cadmus.dat2$Utility)),]
  colnames(cadmus.dat3)
  
  cadmus.dat3[which(is.na(cadmus.dat3$State)),]
  
  
  
  #############################################################################################
  # Merge data and assign electric utility
  #############################################################################################
  
  # Join ZIP codes to cleaned building type data
  samp.dat.0       <- cadmus.dat3
  # Join ZIP mapping to previous step
  samp.dat.1       <- left_join(samp.dat.0, zipMap.dat1)
  samp.dat.1$tally <- rep(1, nrow(samp.dat.1))
  head(samp.dat.1)  
  nrow(samp.dat.1)
  colnames(samp.dat.1)
  colnames(samp.dat.1) <- c("CK_Cadmus_ID"
                            ,"CK_Building_ID"
                            ,"HomeType"
                            ,"BuildingType"
                            ,"State"
                            ,"HomeYearBuilt"
                            ,"HomeYearBuilt_bins1"
                            ,"HomeYearBuilt_bins2"
                            ,"HomeYearBuilt_bins3"
                            ,"HomeYearBuilt_bins_MF"
                            ,"BuildingHeight"
                            ,"ZIPCode"
                            ,"Utility.Customer.Data"
                            ,"Region"
                            ,"BPA_vs_IOU"
                            ,"tally")
  
  samp.dat.1 <- unique(samp.dat.1)
  
  # samp.dat.1       <- left_join(samp.dat.0, zipMap.dat1, by = c("ZIPCode", "Utility", "State"))
  
  # samp.dat.1 <- samp.dat.1[which(!is.na(samp.dat.1$State)),]
  samp.dat.1$BPA_vs_IOU[which(is.na(samp.dat.1$BPA_vs_IOU))] <- "BPA"
  
  unique(samp.dat.1$Region)
  samp.dat.1$Region[which(is.na(samp.dat.1$Region) & samp.dat.1$State == "ID")] <- "-"
  samp.dat.1$Region[which(is.na(samp.dat.1$Region) & samp.dat.1$State == "MT")] <- "W"
  
  samp.dat.2 <- samp.dat.1[which(!is.na(samp.dat.1$Region)),]
  
  # export <- samp.dat.1[which(is.na(samp.dat.1$Region)),]
  # ##  Write out confidence/precision info
  # Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
  # write.xlsx(export, paste(analysisFolder, "Missing From Population.xlsx", sep="/"),
  #            append = T, row.names = F, showNA = F)
  
  
  
  #QAQC: check that no rows in the dataset are duplicates
  # stopifnot(length(which(duplicated(samp.dat.1))) == 0)
  
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
  ##        IF    Cust data has multiple utilities <- IMPOSSIBLE! - we checked cadmus.dat2 in line 84
  ##
  ##  IF    Customer data has one unique utility
  ##        IF    ZIP MAP data has one unique utility but different from customer data utility 
  ##              -> Use customer data, corrected below after merge of samp.dat.3 in lines 278-279 
  ##                                                                                    
  ########################################################################################
  
  ## STEP 1:
  #  Replace missing utility from sample data with utility from zip code mapping
  missingInd <- which(samp.dat.2$Utility.Customer.Data == "-- DID NOT ENTER! --")
  length(missingInd) 
  
  
  ## Prep for steps 2 and 3
  ##  Cust ID's with duplicates
  dupCustIDs <- unique(samp.dat.2$CK_Cadmus_ID[which(duplicated(samp.dat.2$CK_Cadmus_ID))])
  dupData    <- samp.dat.2[which(samp.dat.2$CK_Cadmus_ID %in% dupCustIDs),]
  
  
  # # Initialize counter and output vector
  # cntr              <- 1
  # dupData$Utility <- rep("MISSING", nrow(dupData))
  # 
  # 
  # 
  # ##  For loops to assign utility as per above logic
  # ##  STEP 2 - if the utility zip map has more than a single utility and customer data has more than one, flag for manual fix
  # cntr = 1
  # for(cntr in 1:length(dupCustIDs)) {
  #   if("TRUE" %in% duplicated(dupData$Utility.ZIP.map[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])])) {
  #         dupData$Utility[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])] <- "MANUAL FIX"
  #     }
  # }
  # 
  # ##  STEP 3 - if the utility zip map has more than a single utility and customer data has only one utility - use customer data
  # for(cntr in 1:length(dupCustIDs)) {
  #   if("TRUE" %notin% duplicated(dupData$Utility.ZIP.map[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])])) {
  #       dupData$Utility[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])] <-
  #         dupData$Utility.Customer.Data[which(dupData$CK_Cadmus_ID == dupCustIDs[cntr])]
  #     }
  # }
  # 
  # ##  Subset to ID and Utility column and merge back into sample data
  # names(dupData)
# dupData.1  <- unique(dupData[which(colnames(dupData) %in% c("CK_Cadmus_ID", "Utility"))])
# names(dupData.1)
# samp.dat.3 <- left_join(samp.dat.2, dupData.1, by = "CK_Cadmus_ID")
# 
# ##  For non-duplicates, use cust data
# samp.dat.3$Utility[which(samp.dat.3$CK_Cadmus_ID %notin% dupCustIDs)] <-
#   samp.dat.3$Utility.Customer.Data[which(samp.dat.3$CK_Cadmus_ID %notin% dupCustIDs)]
# 
# ## Give to Rietz
# samp.dat.3[which(is.na(samp.dat.3$BPA_vs_IOU)),]


##########################################
##                                      ##
##  MANUAL FIXES FOR MISSING UTILITIES  ##
##                                      ##
##########################################

# ##  Fix BPA vs IOU
# samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "SEATTLE CITY LIGHT")]     <- "BPA"
# samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "SNOHOMISH PUD")]          <- "BPA"
# samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "PUGET SOUND ENERGY")]     <- "IOU"
# samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "NORTHWESTERN ENERGY")]    <- "IOU"
# samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "MISSION VALLEY POWER")]   <- "BPA"
# samp.dat.3$BPA_vs_IOU[which(samp.dat.3$Utility == "MISSOULA ELECTRIC COOP")] <- "BPA"

#if State is missing, but utility data state is not missing, replace customer state with utility state
# samp.dat.3$State[which(is.na(samp.dat.3$State))] <-
#   samp.dat.3$Utility.Data.State[which(is.na(samp.dat.3$State))]


      # ##  Manual updates early (using partial data)
      # samp.dat.3$State[which(samp.dat.3$CK_Cadmus_ID  =="SE2257 OS SCL")] <- "WA"
      # samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID =="SE2257 OS SCL")] <- "PS"
      # samp.dat.3$State[which(samp.dat.3$CK_Cadmus_ID  =="SG0200 OS SCL")] <- "WA"
      # samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID =="SG0200 OS SCL")] <- "PS"
      # samp.dat.3$State[which(samp.dat.3$CK_Cadmus_ID  =="SL0418 OS SCL")] <- "WA"
      # samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID =="SL0418 OS SCL")] <- "PS"
      # samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID =="SL2263 OS SCL")] <- "PS"

##  Find remaining missing states and regions in full data
#     NOTE: there are cases where zip codes found in our data collection did not show up in the population data from ACS.
#     Make sure this gets written in weighting section of report
missing.region <- samp.dat.2$CK_Cadmus_ID[which(is.na(samp.dat.2$Region))]
#if region is missing in MT or ID, then replace with correct region
# otherwise export CK_Cadmus_ID, send to Rietz to get correct region information
  # samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID %in% missing.region & samp.dat.3$State == "MT")] <- "W"
  # samp.dat.3$Region[which(samp.dat.3$CK_Cadmus_ID %in% missing.region & samp.dat.3$State == "ID")] <- "-"
#print this: send to Rietz (MF does not need region)
samp.dat.2[which(samp.dat.2$CK_Cadmus_ID %in% missing.region & samp.dat.2$State %in% c("WA", "OR") & samp.dat.2$BuildingType != "Multifamily"),]




##  Remove old utility columns and duplicate rows
samp.dat.4 <- samp.dat.2#unique(samp.dat.3[-which(samp.dat.3$CK_Cadmus_ID %in% missing.region),-which(names(samp.dat.3) %in% c("Utility.Customer.Data", "Utility.Data.State"))])
# stopifnot(length(which(duplicated(samp.dat.4$CK_Cadmus_ID))) == 0)

dup.ind <- samp.dat.4$CK_Cadmus_ID[which(duplicated(samp.dat.4$CK_Cadmus_ID))]
dup.data.2 <- samp.dat.4[which(samp.dat.4$CK_Cadmus_ID %in% dup.ind),]


#########################################
# All of this needs to be reviewed
# when the final data comes in
#########################################

# ## Fix issues with BPA/IOU
# samp.dat.4$BPA_vs_IOU[which(samp.dat.4$CK_Cadmus_ID == "PNM20364 OS PSE")] <- "IOU"
# samp.dat.4$BPA_vs_IOU[which(samp.dat.4$CK_Cadmus_ID == "PSM26731 CORE")]   <- "BPA"
# samp.dat.4$BPA_vs_IOU[which(samp.dat.4$CK_Cadmus_ID == "PSM35054 CORE")]   <- "BPA"
# samp.dat.4$BPA_vs_IOU[which(samp.dat.4$CK_Cadmus_ID == "BPS26690 OS BPA")] <- "BPA"

samp.dat.5 <- unique(samp.dat.4)

##  QA/QC: Make sure oversample utilities are in expected BPA territory
# stopifnot(all(samp.dat.5$BPA_vs_IOU[grep("SEATTLE CITY LIGHT", samp.dat.5$Utility)] == "BPA"))
# stopifnot(all(samp.dat.5$BPA_vs_IOU[grep("SNOHOMISH",       samp.dat.5$Utility)]    == "BPA"))
# stopifnot(all(samp.dat.5$BPA_vs_IOU[grep("PUGET SOUND",     samp.dat.5$Utility)]    == "IOU"))

# Subset and define Territory
# Initialize the vector for strata names
samp.dat.5$Territory <- rep("MISSING", nrow(samp.dat.5))
unique(samp.dat.5$Utility)


# Assign Territory
samp.dat.5$Territory[which(samp.dat.5$BPA_vs_IOU == "BPA")]             <- "BPA"
samp.dat.5$Territory[grep("IOU|NOT BPA",        samp.dat.5$BPA_vs_IOU)] <- "Non-BPA.Non-PSE"
samp.dat.5$Territory[grep("SNOHOMISH",          samp.dat.5$Utility)]    <- "SnoPUD"
samp.dat.5$Territory[grep("PUGET SOUND",        samp.dat.5$Utility)]    <- "PSE"
samp.dat.5$Territory[grep("SEATTLE CITY LIGHT", samp.dat.5$Utility)]    <- "SCL"
unique(samp.dat.5$Territory)


##incorrect territory: If WA and PS territory cannot be Non-BPA.Non-PSE
territory.ind <- samp.dat.5$CK_Cadmus_ID[which(samp.dat.5$State == "WA" & samp.dat.5$Region == "PS" & samp.dat.5$Territory == "Non-BPA.Non-PSE")]
  # incorrect.territory <- samp.dat.5$CK_Cadmus_ID[which(samp.dat.5$CK_Cadmus_ID %in% territory.ind)]
  # # incorrect.territory1 <- samp.dat.5[which(samp.dat.5$CK_Cadmus_ID %in% territory.ind),]
  # pse.territory       <- samp.dat.5$CK_Cadmus_ID[grep ("PSE",samp.dat.5$CK_Cadmus_ID)]
  # 
  # territory.sub <- samp.dat.5[which(samp.dat.5$CK_Cadmus_ID %in% incorrect.territory),]
  # territory.sub1 <- territory.sub[which(colnames(territory.sub) %in% c("CK_Cadmus_ID", "Territory"))]
  # 
  # territory.sub1$Territory[grep("PSE", territory.sub1$CK_Cadmus_ID)] <- "PSE"
  # territory.sub1$Territory[grep("BPA", territory.sub1$CK_Cadmus_ID)] <- "BPA"
  # territory.sub1$Territory[grep("CORE|WM", territory.sub1$CK_Cadmus_ID)] <- "BPA"
  # 
  # samp.dat.5$Territory[which(samp.dat.5$CK_Cadmus_ID %in% incorrect.territory)] <- territory.sub1$Territory[which(territory.sub1$CK_Cadmus_ID %in% incorrect.territory)]


#########################################
# 
# End review
# 
#########################################


samp.dat.6 <- data.frame(samp.dat.5, stringsAsFactors = F)
length(unique(samp.dat.6$CK_Cadmus_ID))
samp.dat.6$tally <- 1

# Get sample sizes in each strata
sampCounts.SF <- summarise(group_by(samp.dat.6[which(samp.dat.6$BuildingType == "Single Family"),], 
                                   BuildingType, State, Region, Territory)
                          , n.h = sum(tally))
sampCounts.MH <- summarise(group_by(samp.dat.6[which(samp.dat.6$BuildingType == "Manufactured"),], 
                                    BuildingType, State, Region, Territory)
                           ,n.h = sum(tally))
sampCounts.MF <- summarise(group_by(samp.dat.6[which(samp.dat.6$BuildingType == "Multifamily"),], 
                                    BuildingType, State, Region, Territory)
                           ,n.h = sum(tally))

#############################################################################################
# Count population sizes
#############################################################################################

# Join ZIP codes to building type data
names(zipMap.dat)
popCounts.0 <- summarise(group_by(zipMap.dat, State, Region, Utility, BPA_vs_IOU)
                         , SF.pop = round(sum(SF.N.adj), 0)
                         , MH.pop = round(sum(MH.N.adj), 0)
                         , MF.pop = round(sum(MF.N.adj), 0))


# Initialize the vector for Territory names
popCounts.0$Territory <- rep("MISSING", nrow(popCounts.0))

      ##  QA/QC: Make sure oversample utilities are in expected BPA territory
      # stopifnot(all(popCounts.0$BPA_vs_IOU[grep("SEATTLE CITY", popCounts.0$Utility)] == "BPA"))
      # stopifnot(all(popCounts.0$BPA_vs_IOU[grep("SNOHOMISH",    popCounts.0$Utility)] == "BPA"))
      # stopifnot(all(popCounts.0$BPA_vs_IOU[grep("PUGET SOUND",  popCounts.0$Utility)] == "IOU"))

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
popCounts.SF <- summarise(group_by(popCounts.0, 
                                   State, Region, Territory)
                          ,BuildingType = "Single Family"
                         , N.h = sum(SF.pop)
                         # , N_MH.h = sum(unique(MH.pop))
                         # , N_MF.h = sum(unique(MF.pop))
                         )
popCounts.MH <- summarise(group_by(popCounts.0, 
                                   State, Region, Territory)
                          ,BuildingType = "Manufactured"
                          ,N.h = sum(MH.pop))
popCounts.MF <- summarise(group_by(popCounts.0, 
                                   State, Region, Territory)
                          ,BuildingType = "Multifamily"
                          ,N.h = sum(MH.pop))



#############################################################################################
# Combine population and sample sizes by Territory
#############################################################################################

total.counts.SF <- full_join(popCounts.SF, sampCounts.SF, by = c("BuildingType"
                                                        ,"State"
                                                        ,"Region"
                                                        ,"Territory"))
total.counts.MH <- full_join(popCounts.MH, sampCounts.MH, by = c("BuildingType"
                                                                 ,"State"
                                                                 ,"Region"
                                                                 ,"Territory"))
total.counts.MF <- full_join(popCounts.MF, sampCounts.MF, by = c("BuildingType"
                                                                 ,"State"
                                                                 ,"Region"
                                                                 ,"Territory"))

## check that there are no NA's in final sample sizes
## Put zero as a placeholder until final comes in
total.counts.SF$n.h[which(is.na(total.counts.SF$n.h))] <- 0

#############################################################################################
# Cmerge pop and sample sizes onto cleaned data
# export final counts and final data
#############################################################################################

samp.dat.SF <- left_join(samp.dat.6[which(samp.dat.6$BuildingType == "Single Family"),]
                         , total.counts.SF)
samp.dat.MH <- left_join(samp.dat.6[which(samp.dat.6$BuildingType == "Manufactured"),]
                         , total.counts.MH)
samp.dat.MF <- left_join(samp.dat.6[which(samp.dat.6$BuildingType == "Multifamily"),]
                         , total.counts.MF)

samp.dat.final <- rbind.data.frame(samp.dat.SF, samp.dat.MH, samp.dat.MF, stringsAsFactors = F)
unique(samp.dat.final$n.h)

which(duplicated(samp.dat.final$CK_Cadmus_ID))

length(unique(cleanRBSA.dat1$CK_Cadmus_ID)) - length(unique(samp.dat.final$CK_Cadmus_ID))

missing.ind <- cleanRBSA.dat1[which(cleanRBSA.dat1$CK_Cadmus_ID %notin% samp.dat.2$CK_Cadmus_ID),]
# samp.dat.2[which(samp.dat.2$CK_Cadmus_ID %in% missing.ind),]

return(samp.dat.final)
# return(final.counts)
# ##  Export clean data merged with weights
# write.xlsx(samp.dat.final, paste(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)
# 
# ##  Export 
# write.xlsx(final.counts, paste(filepathCleanData, paste("weights.data", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)

# write.xlsx(missing.data, paste(analysisFolder, paste("Missing Population Data", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)

}

