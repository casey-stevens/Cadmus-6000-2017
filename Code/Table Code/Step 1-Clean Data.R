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


# SourcePath <- "C:/Users/Casey.Stevens/Documents/Git/Cadmus-6000-2017/Cadmus-6000-2017/Code/Table Code"
# source(file.path(SourcePath, "SourceCode.R"))

source("Code/Table Code/SourceCode.R")




#############################################################################################
# Import and Subset Data
#############################################################################################

# Import site data
one.line.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.export), sheet = "Site One Line Summary", startRow = 2)
one.line.dat1 <- data.frame("CK_Cadmus_ID"         = one.line.dat$Cadmus.ID
                            , "BuildingTypeXX"     = one.line.dat$`Home.Type.-.FMP.Detailed`
                            , "BuildingType"       = one.line.dat$`Home.Type.-.Final`
                            , "HomeYearBuilt"      = one.line.dat$Year.Built
                            , "State"              = one.line.dat$State
                            , "Detailed.Region"    = one.line.dat$Region
                            , "Conditioned.Area"   = one.line.dat$Conditioned.Area
                            , "Conditioned.Volume" = one.line.dat$Conditioned.Volume
                            , "Cooling.Zone"       = one.line.dat$Cooling.Zone
                            , "ZIP"                = one.line.dat$Zip
                            , stringsAsFactors     = F)
building.id.dat <- data.frame("CK_Cadmus_ID"      = one.line.dat$Cadmus.ID
                              ,"CK_Building_ID"   = one.line.dat$CK_BuildingID
                              ,"CK_SiteID"        = one.line.dat$PK_SiteID
                              , stringsAsFactors  = F)
building.id.dat <- melt(building.id.dat, id.vars = "CK_Cadmus_ID")
names(building.id.dat) <- c("CK_Cadmus_ID", "Remove", "CK_Building_ID")
building.id.dat <- building.id.dat[which(!is.na(building.id.dat$CK_Building_ID)),which(colnames(building.id.dat) != "Remove")]

one.line.dat1 <- left_join(building.id.dat, one.line.dat1)
names(one.line.dat1)

site.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.export))
site.dat0 <- data.frame("CK_Cadmus_ID" = site.dat$CK_Cadmus_ID
                        , "BuildingHeight"  = site.dat$SITE_Construction_TotalLevelsThisSite
                        , stringsAsFactors  = F)

one.line.dat1$CK_Cadmus_ID <- trimws(toupper(one.line.dat1$CK_Cadmus_ID))
site.dat0$CK_Cadmus_ID     <- trimws(toupper(site.dat0$CK_Cadmus_ID))


site.dat1 <- left_join(one.line.dat1, site.dat0)
length(unique(site.dat1$CK_Cadmus_ID)) #2068

#############################################################################################
# Clean States
#############################################################################################

### Deal with missing / unknown states

site.dat1$State <- trimws(toupper(site.dat1$State))

# #replace all missing states with actual
# state.na.ind <- site.dat1$CK_Cadmus_ID[which(is.na(site.dat1$State))]
# state.sub    <- site.dat1[which(!(site.dat1$CK_Cadmus_ID %in% state.na.ind)),]
# state.sub1   <- unique(state.sub[which(colnames(state.sub) %in% c("CK_Cadmus_ID", "State"))])
# 
# site.dat2 <- site.dat1[which(colnames(site.dat1) != "State")]
# site.dat3 <- left_join(site.dat2, state.sub1, by = "CK_Cadmus_ID")
# 
# site.dat3$State <- trimws(toupper(site.dat3$State))
# unique(site.dat3$State)
# 
# #replace all unknown states with actual
# state.unk.ind <- unique(site.dat3$CK_Cadmus_ID[which(site.dat3$State == "UNKNOWN")])
#   state.unk.ind ## export into tab for State for Rietz to fill in
# 
# site.dat3$State[which(site.dat3$CK_Cadmus_ID == "MM0016N20")] <- "MT"
# site.dat3$State[which(site.dat3$CK_Cadmus_ID == "WM1463PM")]  <- "WA"
# 
# unique(site.dat3$State)
# 
# length(unique(site.dat1$CK_Cadmus_ID)) #2068

#############################################################################################
# Clean building type
#############################################################################################

rbsa.dat <- site.dat1

# Convert building types to what we want
rbsa.dat$BuildingType[grep("Multifamily",rbsa.dat$BuildingType, ignore.case = T)] <- "Multifamily"
rbsa.dat$BuildingType[grep("Single fam",rbsa.dat$BuildingType, ignore.case = T)]  <- "Single Family"
rbsa.dat$BuildingType[grep("Manufa",rbsa.dat$BuildingType, ignore.case = T)]      <- "Manufactured"


# rbsa.dat$BuildingType[grep("Apartment",rbsa.dat$BuildingTypeXX, ignore.case = T)]          <- "Multifamily"
# rbsa.dat$BuildingType[grep("Single family|Town|Duplex",rbsa.dat$BuildingTypeXX, ignore.case = T)] <- "Single Family"
# rbsa.dat$BuildingType[grep("Wide|Modular",rbsa.dat$BuildingTypeXX, ignore.case = T)]       <- "Manufactured"

unique(rbsa.dat$BuildingType)


# rbsa.dat$BuildingTypeXX[grep("single wide",rbsa.dat$BuildingTypeXX, ignore.case = T)]       <- "Single Wide"


#############################################################################################
# Clean home year built info
#############################################################################################
unique(rbsa.dat$HomeYearBuilt)
# rbsa.dat$HomeYearBuilt <- gsub("_x000D_\n", "", rbsa.dat$HomeYearBuilt)
# rbsa.dat$HomeYearBuilt <- gsub("*.[0-9]{4}", "", rbsa.dat$HomeYearBuilt)
# rbsa.dat$HomeYearBuilt <- gsub("9999", "", rbsa.dat$HomeYearBuilt)
# rbsa.dat$HomeYearBuilt[which(rbsa.dat$HomeYearBuilt == "1905.2")] <- "1905"
rbsa.dat$HomeYearBuilt <- as.numeric(as.character(rbsa.dat$HomeYearBuilt))
unique(rbsa.dat$HomeYearBuilt)

rbsa.check <- rbsa.dat[which(is.na(rbsa.dat$HomeYearBuilt)),]
length(unique(rbsa.check$CK_Cadmus_ID))
missing.year.ind <- unique(rbsa.check$CK_Cadmus_ID)
# missing.year.ind <- rbsa.dat$CK_Cadmus_ID[which(!(rbsa.dat$CK_Cadmus_ID %in% rbsa.check$CK_Cadmus_ID))]
unique(missing.year.ind)## send list to Rietz - fill in correct information export to missing year tab

rbsa.dat <- unique(rbsa.dat)
rbsa.dat$HomeYearBuilt[which(duplicated(rbsa.dat$CK_Cadmus_ID))]


    # Convert home year built to New / Existing
    rbsa.dat$HomeYearBuilt_bins1 <- rbsa.dat$HomeYearBuilt
    rbsa.dat$HomeYearBuilt_bins1[which(as.numeric(as.character(rbsa.dat$HomeYearBuilt)) < 2012)] <- "Existing"
    rbsa.dat$HomeYearBuilt_bins1[which(as.numeric(as.character(rbsa.dat$HomeYearBuilt)) > 2012 | as.numeric(as.character(rbsa.dat$HomeYearBuilt)) == 2012)] <- "New"
    ##Warning okay here
    unique(rbsa.dat$HomeYearBuilt_bins1)
    
    # Convert home year built to specific bins
    rbsa.dat$HomeYearBuilt_bins2 <- as.numeric(as.character(rbsa.dat$HomeYearBuilt))
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt < 1951)] <- "Pre 1951"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1951 & rbsa.dat$HomeYearBuilt < 1961)] <- "1951-1960"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1961 & rbsa.dat$HomeYearBuilt < 1971)] <- "1961-1970"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1971 & rbsa.dat$HomeYearBuilt < 1981)] <- "1971-1980"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1981 & rbsa.dat$HomeYearBuilt < 1991)] <- "1981-1990"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 1991 & rbsa.dat$HomeYearBuilt < 2001)] <- "1991-2000"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 2001 & rbsa.dat$HomeYearBuilt < 2011)] <- "2001-2010"
    rbsa.dat$HomeYearBuilt_bins2[which(rbsa.dat$HomeYearBuilt >= 2011)] <- "Post 2010"
    unique(rbsa.dat$HomeYearBuilt_bins2)
    
    # Convert home year built to specific bins (4 categories)
    rbsa.dat$HomeYearBuilt_bins3 <- as.numeric(as.character(rbsa.dat$HomeYearBuilt))
    rbsa.dat$HomeYearBuilt_bins3[which(rbsa.dat$HomeYearBuilt < 1981)] <- "Pre 1981"
    rbsa.dat$HomeYearBuilt_bins3[which(rbsa.dat$HomeYearBuilt >= 1981 & rbsa.dat$HomeYearBuilt < 1991)] <- "1981-1990"
    rbsa.dat$HomeYearBuilt_bins3[which(rbsa.dat$HomeYearBuilt >= 1991 & rbsa.dat$HomeYearBuilt < 2001)] <- "1991-2000"
    rbsa.dat$HomeYearBuilt_bins3[which(rbsa.dat$HomeYearBuilt >= 2001 & rbsa.dat$HomeYearBuilt < 2011)] <- "2001-2010"
    rbsa.dat$HomeYearBuilt_bins3[which(rbsa.dat$HomeYearBuilt >= 2011)] <- "Post 2010"
    unique(rbsa.dat$HomeYearBuilt_bins3)
    
    # Convert home year built to specific bins
    rbsa.dat$HomeYearBuilt_bins4 <- as.numeric(as.character(rbsa.dat$HomeYearBuilt))
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt < 1951)] <- "Pre 1951"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 1951 & rbsa.dat$HomeYearBuilt < 1961)] <- "1951-1960"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 1961 & rbsa.dat$HomeYearBuilt < 1971)] <- "1961-1970"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 1971 & rbsa.dat$HomeYearBuilt < 1981)] <- "1971-1980"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 1981 & rbsa.dat$HomeYearBuilt < 1986)] <- "1981-1985"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 1986 & rbsa.dat$HomeYearBuilt < 1991)] <- "1986-1990"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 1991 & rbsa.dat$HomeYearBuilt < 1996)] <- "1991-1995"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 1996 & rbsa.dat$HomeYearBuilt < 2001)] <- "1996-2000"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 2001 & rbsa.dat$HomeYearBuilt < 2006)] <- "2001-2005"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 2006 & rbsa.dat$HomeYearBuilt < 2011)] <- "2006-2010"
    rbsa.dat$HomeYearBuilt_bins4[which(rbsa.dat$HomeYearBuilt >= 2011)] <- "Post 2010"
    unique(rbsa.dat$HomeYearBuilt_bins4)

    # Convert home year built to specific bins for multifamily
    rbsa.dat$HomeYearBuilt_MF <- as.numeric(as.character(rbsa.dat$HomeYearBuilt))
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt < 1955)] <- "Pre 1955"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 1955 & rbsa.dat$HomeYearBuilt < 1971)] <- "1955-1970"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 1971 & rbsa.dat$HomeYearBuilt < 1981)] <- "1971-1980"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 1981 & rbsa.dat$HomeYearBuilt < 1991)] <- "1981-1990"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 1991 & rbsa.dat$HomeYearBuilt < 2001)] <- "1991-2000"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 2001 & rbsa.dat$HomeYearBuilt < 2011)] <- "2001-2010"
    rbsa.dat$HomeYearBuilt_MF[which(rbsa.dat$HomeYearBuilt >= 2011)] <- "Post 2010"
    unique(rbsa.dat$HomeYearBuilt_MF)
    
    
    
        #QAQC - stop if there are NA in any Home Year Built category
        # stopifnot(all(!(is.na(c(rbsa.dat$HomeYearBuilt_bins, rbsa.dat$HomeYearBuilt_bins4, rbsa.dat$HomeYearBuilt_MF)))))


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
  unique(rbsa.dat4$ZIP)

  #identify which accounts have multiple zips
  zip.sub <- unique(rbsa.dat4[which(colnames(rbsa.dat4) %in% c("CK_Cadmus_ID", "ZIP"))])
  zip.dup.ind <- zip.sub$CK_Cadmus_ID[which(duplicated(zip.sub$CK_Cadmus_ID))]
  zip.sub[which(zip.sub$CK_Cadmus_ID %in% zip.dup.ind),] #export this to let Rietz give correct info

  #identify which accounts have NA for ZIP
  missing.zip <- zip.sub$CK_Cadmus_ID[which(is.na(zip.sub$ZIP))]
  missing.zip #export this to let Rietz give correct info
  

#############################################################################################
# Fix missing building type information
#############################################################################################
rbsa.dat5 <- rbsa.dat4

  rbsa.check2 <- rbsa.dat5[which(!(is.na(rbsa.dat5$BuildingType))),]
  length(unique(rbsa.check2$CK_Cadmus_ID)) #2068 - None missing
  #check to see which site IDs are lost
  missing.ind <- unique(rbsa.dat5$CK_Cadmus_ID[which(!(rbsa.dat5$CK_Cadmus_ID %in% rbsa.check2$CK_Cadmus_ID))])
  missing.ind #export into missing building type tab - ask Rietz for both Generic and Specific building type info
  rbsa.dat5[which(rbsa.dat5$CK_Cadmus_ID %in% missing.ind),]

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
    # rbsa.dat6$BuildingTypeXX[which(rbsa.dat6$CK_Cadmus_ID == "SE0872 OS SCL")]   <- "Single Family Detached"
    # #For Site ID SG0200, the disrepancy occurs in State
    # rbsa.dat6$State[which(rbsa.dat6$CK_Cadmus_ID == "SG0200 OS SCL")]   <- "WA"


rbsa.dat7 <- unique(rbsa.dat6)

  #QAQC - are the number of unique IDs equal to the number of rows in the dataset?
  # stopifnot(length(unique(rbsa.dat7$CK_Cadmus_ID)) == nrow(rbsa.dat7))
  stopifnot(length(unique(site.dat1$CK_Cadmus_ID)) == length(unique(rbsa.dat7$CK_Cadmus_ID)))

  rbsa.dat.sf.mh <- rbsa.dat7[grep("site", rbsa.dat7$CK_Building_ID, ignore.case = T),]
  rbsa.dat.sf.mh <- rbsa.dat.sf.mh[which(rbsa.dat.sf.mh$BuildingType != "Multifamily"),]
  rbsa.dat.mf <- rbsa.dat7[which(rbsa.dat7$BuildingType == "Multifamily"),]

rbsa.dat8 <- rbind.data.frame(rbsa.dat.sf.mh, rbsa.dat.mf, stringsAsFactors = F)
#############################################################################################
# Write out cleaned building type information
#############################################################################################

##  Write out confidence/precision info
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
write.xlsx(rbsa.dat8, paste(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = ""), sep="/"),
           append = T, row.names = F, showNA = F)



