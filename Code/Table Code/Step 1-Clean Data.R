#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################


#############################################################################################
#
# Need to run this before any other scripts can be ran
#
#############################################################################################


##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


SourcePath <- "C:/Users/Casey.Stevens/Documents/Git/Cadmus-6000-2017/Cadmus-6000-2017/Code/Table Code"
source(file.path(SourcePath, "SourceCode.R"))





#############################################################################################
# Import and Subset Data
#############################################################################################

# Import site data
site.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.export))
site.dat1 <- data.frame("CK_Cadmus_ID" = site.dat$CK_Cadmus_ID
                               , "BuildingTypeXX"  = site.dat$SITE_GENL_INFO_BuildingType
                               , "HomeYearBuilt" = site.dat$SITES_General_GENL_INFO_HomeYearBuilt
                               , "State"           = site.dat$SITE_ST
                               , "BuildingHeight"  = site.dat$SITE_Construction_TotalLevelsThisSite
                               , "ZIP"             = site.dat$SITE_ZIP
                               , stringsAsFactors  = F)
head(site.dat1)
site.dat1$CK_Cadmus_ID <- trimws(toupper(site.dat1$CK_Cadmus_ID))
length(unique(site.dat1$CK_Cadmus_ID)) #601

#############################################################################################
# Clean States
#############################################################################################

### Deal with missing / unknown states

#replace all missing states with actual
state.na.ind <- site.dat1$CK_Cadmus_ID[which(is.na(site.dat1$State))]
state.sub    <- site.dat1[which(!(site.dat1$CK_Cadmus_ID %in% state.na.ind)),]
state.sub1   <- unique(state.sub[which(colnames(state.sub) %in% c("CK_Cadmus_ID", "State"))])

site.dat2 <- site.dat1[which(colnames(site.dat1) != "State")]
site.dat3 <- left_join(site.dat2, state.sub1, by = "CK_Cadmus_ID")

site.dat3$State <- trimws(toupper(site.dat3$State))
unique(site.dat3$State)

#replace all unknown states with actual
state.unk.ind <- unique(site.dat3$CK_Cadmus_ID[which(site.dat3$State == "UNKNOWN")])
  state.unk.ind ## export into tab for State for Rietz to fill in

site.dat3$State[which(site.dat3$CK_Cadmus_ID == "MM0016N20")] <- "MT"
site.dat3$State[which(site.dat3$CK_Cadmus_ID == "WM1463PM")]  <- "WA"

unique(site.dat3$State)

length(unique(site.dat3$CK_Cadmus_ID)) #601

#############################################################################################
# Clean building type
#############################################################################################

rbsa.dat <- site.dat3

# Convert building types to what we want
rbsa.dat$BuildingType <- rbsa.dat$BuildingTypeXX
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Apartment Building (3 or fewer floors)")] <- "Multifamily"
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Apartment Building (4 to 6 floors)")] <- "Multifamily"
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Apartment Building (More than 6 floors)")] <- "Multifamily"
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Single Wide" | rbsa.dat$BuildingType == "Double Wide"| rbsa.dat$BuildingType == "Triple Wide")] <- "Manufactured"
rbsa.dat$BuildingType[which(rbsa.dat$BuildingType == "Townhome or Rowhome" | rbsa.dat$BuildingType == "Duplex, Triplex, or Fourplex" | rbsa.dat$BuildingType == "Single Family Detached")] <- "Single Family"
unique(rbsa.dat$BuildingType)

#############################################################################################
# Clean home year built info
#############################################################################################
unique(rbsa.dat$HomeYearBuilt)
rbsa.dat$HomeYearBuilt <- gsub("\n", "", rbsa.dat$HomeYearBuilt)
rbsa.dat$HomeYearBuilt <- gsub("*.[0-9]{4}", "", rbsa.dat$HomeYearBuilt)
rbsa.dat$HomeYearBuilt[which(rbsa.dat$HomeYearBuilt %in% c(0,00,000,0000,"0","00","000","0000"))] <- NA
unique(rbsa.dat$HomeYearBuilt)

rbsa.check <- rbsa.dat[which(!(is.na(rbsa.dat$HomeYearBuilt))),]
length(unique(rbsa.check$CK_Cadmus_ID))
missing.year.ind <- unique(rbsa.dat$CK_Cadmus_ID[which(!(rbsa.dat$CK_Cadmus_ID %in% rbsa.check$CK_Cadmus_ID))])
missing.year.ind## send list to Rietz - fill in correct information export to missing year tab
length(missing.year.ind) 


rbsa.dat$HomeYearBuilt[which(duplicated(rbsa.dat$CK_Cadmus_ID))]
rbsa.dat$HomeYearBuilt[which(rbsa.dat$CK_Cadmus_ID == "SE0872 OS SCL")]   <- 1948
rbsa.dat$HomeYearBuilt[which(rbsa.dat$CK_Cadmus_ID == "WS3209")]          <- 1946
rbsa.dat$HomeYearBuilt[which(rbsa.dat$CK_Cadmus_ID == "SG0808 OS SCL")]   <- 1957


    # Convert home year built to New / Existing
    rbsa.dat$HomeYearBuilt_bins1 <- rbsa.dat$HomeYearBuilt
    rbsa.dat$HomeYearBuilt_bins1[which(as.numeric(as.character(rbsa.dat$HomeYearBuilt)) < 2012)] <- "Existing"
    rbsa.dat$HomeYearBuilt_bins1[which(as.numeric(as.character(rbsa.dat$HomeYearBuilt)) > 2012 | as.numeric(as.character(rbsa.dat$HomeYearBuilt)) == 2012)] <- "New"
    ##Warning okay here
    unique(rbsa.dat$HomeYearBuilt)
    
    # Convert home year built to specific bins
    rbsa.dat$HomeYearBuilt_bins2 <- as.numeric(as.character(rbsa.dat$HomeYearBuilt))
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt < 1951)] <- "Pre 1951"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1951 & rbsa.dat$HomeYearBuilt < 1961)] <- "1951-1960"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1961 & rbsa.dat$HomeYearBuilt < 1971)] <- "1961-1970"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1971 & rbsa.dat$HomeYearBuilt < 1981)] <- "1971-1980"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1981 & rbsa.dat$HomeYearBuilt < 1991)] <- "1981-1990"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1991 & rbsa.dat$HomeYearBuilt < 2001)] <- "1991-2000"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 2001)] <- "Post 2000"
    unique(rbsa.dat$HomeYearBuilt_bins2)
    
    # Convert home year built to specific bins (4 categories)
    rbsa.dat$HomeYearBuilt_bins3 <- as.numeric(as.character(rbsa.dat$HomeYearBuilt))
    rbsa.dat$HomeYearBuilt_bins3[which(rbsa.dat$HomeYearBuilt < 1981)] <- "Pre 1981"
    rbsa.dat$HomeYearBuilt_bins3[which(rbsa.dat$HomeYearBuilt >= 1981 & rbsa.dat$HomeYearBuilt < 1991)] <- "1981-1990"
    rbsa.dat$HomeYearBuilt_bins3[which(rbsa.dat$HomeYearBuilt >= 1991 & rbsa.dat$HomeYearBuilt < 2001)] <- "1991-2000"
    rbsa.dat$HomeYearBuilt_bins3[which(rbsa.dat$HomeYearBuilt >= 2001)] <- "Post 2000"
    unique(rbsa.dat$HomeYearBuilt_bins3)
    
    # Convert home year built to specific bins for multifamily
    rbsa.dat$HomeYearBuilt_MF <- as.numeric(as.character(rbsa.dat$HomeYearBuilt))
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt < 1955)] <- "Pre 1955"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 1955 & rbsa.dat$HomeYearBuilt < 1971)] <- "1955-1970"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 1971 & rbsa.dat$HomeYearBuilt < 1981)] <- "1971-1980"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 1981 & rbsa.dat$HomeYearBuilt < 1991)] <- "1981-1990"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 1991 & rbsa.dat$HomeYearBuilt < 2001)] <- "1991-2000"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 2001)] <- "Post 2000"
    unique(rbsa.dat$HomeYearBuilt_MF)

    
        #QAQC - stop if there are NA in any Home Year Built category
        stopifnot(all(!(is.na(c(rbsa.dat$HomeYearBuilt_bins, rbsa.dat$HomeYearBuilt_bins4, rbsa.dat$HomeYearBuilt_MF)))))


rbsa.dat0 <- rbsa.dat
#QAQC length of ids - make sure none are lost
stopifnot(length(unique(site.dat1$CK_Cadmus_ID)) == length(unique(rbsa.dat0$CK_Cadmus_ID)))


#############################################################################################
# Clean building height info
#############################################################################################
  rbsa.sub <- rbsa.dat0[which(!(is.na(rbsa.dat0$BuildingHeight))),]
  rbsa.sub1 <- rbsa.sub[which(colnames(rbsa.sub) %in% c("CK_Cadmus_ID", "BuildingHeight"))]

rbsa.dat1 <- rbsa.dat0[which(colnames(rbsa.dat0) != "BuildingHeight")]
rbsa.dat2 <- left_join(rbsa.dat1, rbsa.sub1, by = "CK_Cadmus_ID")

  height.sub <- unique(rbsa.dat2[which(colnames(rbsa.dat2) %in% c("CK_Cadmus_ID", "BuildingHeight"))])
  height.dup.ind <- height.sub$CK_Cadmus_ID[which(duplicated(height.sub$CK_Cadmus_ID))]
  height.sub[which(height.sub$CK_Cadmus_ID %in% height.dup.ind),] #export this to let Rietz give correct info

  #for site ID SL1953 Discrepancy occurs in number of floors
  rbsa.dat2$BuildingHeight[which(rbsa.dat2$CK_Cadmus_ID == "SL1953 OS SCL")]   <- 1.5
  
#############################################################################################
# Clean ZIP info
#############################################################################################
rbsa.sub2 <- rbsa.dat2[which(!(is.na(rbsa.dat2$ZIP))),]
rbsa.sub3 <- rbsa.sub2[which(colnames(rbsa.sub2) %in% c("CK_Cadmus_ID", "ZIP"))]

rbsa.dat3 <- rbsa.dat2[which(colnames(rbsa.dat2) != "ZIP")]
rbsa.dat4 <- left_join(rbsa.dat3, rbsa.sub3, by = "CK_Cadmus_ID")

  # Clean ZIP info when there is a dash in zip
  unique(rbsa.dat4$ZIP)
  rbsa.dat4$ZIP[which(rbsa.dat4$ZIP == "Unknown")] <- NA
  rbsa.dat4$ZIP <- substr(rbsa.dat4$ZIP, 1, 5)
  unique(rbsa.dat4$ZIP)

  #identify which accounts have multiple zips
  zip.sub <- unique(rbsa.dat4[which(colnames(rbsa.dat4) %in% c("CK_Cadmus_ID", "ZIP"))])
  zip.dup.ind <- zip.sub$CK_Cadmus_ID[which(duplicated(zip.sub$CK_Cadmus_ID))]
  zip.sub[which(zip.sub$CK_Cadmus_ID %in% zip.dup.ind),] #export this to let Rietz give correct info

  #identify which accounts have NA for ZIP
  missing.zip <- zip.sub$CK_Cadmus_ID[which(is.na(zip.sub$ZIP))]
  missing.zip #export this to let Rietz give correct info
  
  #for site ID WS3209, discrepancy occurs in ZIP
  rbsa.dat4$ZIP[which(rbsa.dat4$CK_Cadmus_ID == "WS3209")]   <- 98030
  
  ### FAKE: Delete when Rietz gives cleaned data
  rbsa.dat4$ZIP[which(rbsa.dat4$CK_Cadmus_ID == "BPM21777 OS BPA")]   <- 98103
  rbsa.dat4$ZIP[which(rbsa.dat4$CK_Cadmus_ID == "BPM24677 OS BPA")]   <- 98125
  rbsa.dat4$ZIP[which(rbsa.dat4$CK_Cadmus_ID == "PSM26922 CORE")]     <- 98108
  rbsa.dat4$ZIP[which(rbsa.dat4$CK_Cadmus_ID == "SG0200 OS SCL")]     <- 98118
  rbsa.dat4$ZIP[which(rbsa.dat4$CK_Cadmus_ID == "PNM22969 OS PSE")]   <- 98439
  rbsa.dat4$ZIP[which(rbsa.dat4$CK_Cadmus_ID == "KM23438 OS PSE")]    <- 98033
  

#############################################################################################
# Fix missing building type information
#############################################################################################
rbsa.dat5 <- rbsa.dat4

  rbsa.check2 <- rbsa.dat5[which(!(is.na(rbsa.dat5$BuildingType))),]
  length(unique(rbsa.check2$CK_Cadmus_ID)) #596 -- missing 5
  #check to see which site IDs are lost
  missing.ind <- unique(rbsa.dat5$CK_Cadmus_ID[which(!(rbsa.dat5$CK_Cadmus_ID %in% rbsa.check2$CK_Cadmus_ID))])
  missing.ind #export into missing building type tab - ask Rietz for both Generic and Specific building type info
  rbsa.dat5[which(rbsa.dat5$CK_Cadmus_ID %in% missing.ind),]

### Delete when Rietz gives cleaned data
#update building type (generic)
rbsa.dat5$BuildingType[which(rbsa.dat5$CK_Cadmus_ID == "SL2263 OS SCL")]   <- "Multifamily" #- Low Rise
rbsa.dat5$BuildingType[which(rbsa.dat5$CK_Cadmus_ID == "SG0048 OS SCL")]   <- "Single Family"
rbsa.dat5$BuildingType[which(rbsa.dat5$CK_Cadmus_ID == "SL1953 OS SCL")]   <- "Single Family"
rbsa.dat5$BuildingType[which(rbsa.dat5$CK_Cadmus_ID == "WM1463PM")]        <- "Multifamily" #- High Rise
rbsa.dat5$BuildingType[which(rbsa.dat5$CK_Cadmus_ID == "MS3085")]          <- "Single Family"

# update building type (specific)
rbsa.dat5$BuildingTypeXX[which(rbsa.dat5$CK_Cadmus_ID == "SL2263 OS SCL")]   <- "Apartment Building (3 or fewer floors)"
rbsa.dat5$BuildingTypeXX[which(rbsa.dat5$CK_Cadmus_ID == "SG0048 OS SCL")]   <- "Single Family Detached"
rbsa.dat5$BuildingTypeXX[which(rbsa.dat5$CK_Cadmus_ID == "SL1953 OS SCL")]   <- "Single Family Detached"
rbsa.dat5$BuildingTypeXX[which(rbsa.dat5$CK_Cadmus_ID == "WM1463PM")]        <- "Apartment Building (More than 6 floors)"
rbsa.dat5$BuildingTypeXX[which(rbsa.dat5$CK_Cadmus_ID == "MS3085")]          <- "Single Family Detached"

  #QAQC - check that we haven't lost any sites
  stopifnot(length(unique(site.dat1$CK_Cadmus_ID)) == length(unique(rbsa.dat5$CK_Cadmus_ID)))

#############################################################################################
# Fix any duplicating information
#   if length(unique(ID)) does not equal the number of rows in the dataset
#   then there are duplicated IDs because of discrepancies in the data
#############################################################################################
rbsa.dat6 <- unique(rbsa.dat5[which(!(is.na(rbsa.dat5$BuildingType))),])
  
  #QAQC - are the number of unique IDs equal to the number of rows in the dataset?
  stopifnot(length(unique(rbsa.dat6$CK_Cadmus_ID)) == nrow(rbsa.dat6))
  
    #if not, identify where the duplicates are occurring
    dup.ind1 <- unique(rbsa.dat6$CK_Cadmus_ID[which(duplicated(rbsa.dat6$CK_Cadmus_ID))])
    rbsa.dat6[which(rbsa.dat6$CK_Cadmus_ID %in% dup.ind1),]
    
    #For Site ID SE0872, the disrepancy occurs in home type
    rbsa.dat6$BuildingTypeXX[which(rbsa.dat6$CK_Cadmus_ID == "SE0872 OS SCL")]   <- "Single Family Detached"
    #For Site ID SG0200, the disrepancy occurs in State
    rbsa.dat6$State[which(rbsa.dat6$CK_Cadmus_ID == "SG0200 OS SCL")]   <- "WA"


rbsa.dat7 <- unique(rbsa.dat6[which(!(duplicated(rbsa.dat6$CK_Cadmus_ID))),])

  #QAQC - are the number of unique IDs equal to the number of rows in the dataset?
  stopifnot(length(unique(rbsa.dat7$CK_Cadmus_ID)) == nrow(rbsa.dat7))
  stopifnot(length(unique(site.dat1$CK_Cadmus_ID)) == length(unique(rbsa.dat7$CK_Cadmus_ID)))

#############################################################################################
# Write out cleaned building type information
#############################################################################################

##  Write out confidence/precision info
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
write.xlsx(rbsa.dat7, paste(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = ""), sep="/"),
           append = T, row.names = F, showNA = F)



