#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
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
# Import Data
#############################################################################################
# Define File Path
SPPath   <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Data for SCL"
cleanInPath <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Analysis Documents/Clean Data"
analysisInPath <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Analysis Documents"
stopifnot(all(file.exists(SPPath, cleanInPath, analysisInPath)))

rbsa.dat <- read.xlsx(xlsxFile = file.path(cleanInPath, paste("clean.rbsa.data", rundate, ".xlsx")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

envelope.dat <- read.xlsx(xlsxFile = file.path(SPPath, "Envelope_EquipConsol_2017.04.25.xlsx"))

rooms.dat <- read.xlsx(xlsxFile = file.path(SPPath, "ROOMS_2017.03.30.xlsx"))



#############################################################################################
# Item 19: PERCENTAGE OF HOMES WITH BASEMENTS BY STATE (SF table 26)
#############################################################################################
item19.dat <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID" #remove BLDG info
                                                               ,"Clean.Type"))]
item19.dat1 <- item19.dat[-grep("BLDG", item19.dat$CK_SiteID),]

item19.dat2 <- left_join(rbsa.dat, item19.dat1, by = "CK_Cadmus_ID")
length(unique(item19.dat2$CK_Cadmus_ID))#565

#subset to only single family homes
item19.dat3 <- item19.dat2[which(item19.dat2$BuildingType == "Single Family"),]
item19.dat3$count <- 1

#summarise -- State
item19.tmp1 <- summarise(group_by(item19.dat3, BuildingType, State)
                          , SampleSize = length(unique(CK_Cadmus_ID)))
##Region
item19.tmp2 <- summarise(group_by(item19.dat3, BuildingType)
                         , State = "Region"
                         , SampleSize = length(unique(CK_Cadmus_ID)))
## rbind state and region sample sizes
item19.tmp00 <- rbind.data.frame(item19.tmp1, item19.tmp2, stringsAsFactors = F)


item19.dat4 <- item19.dat3[which(item19.dat3$Clean.Type == "Basement"),]
## Basement only -- State
item19.tmp3 <- summarise(group_by(item19.dat4, BuildingType, State)
                         , BasementCount = length(unique(CK_Cadmus_ID)))
## Basement only -- Region
item19.tmp4 <- summarise(group_by(item19.dat4, BuildingType)
                         , State = "Region"
                         , BasementCount = length(unique(CK_Cadmus_ID)))
## rbind state and region sample sizes
item19.tmp01 <- rbind.data.frame(item19.tmp3, item19.tmp4, stringsAsFactors = F)


item19.final <- left_join(item19.tmp00, item19.tmp01, by = c("BuildingType", "State"))
item19.final$Percent <- item19.final$BasementCount / item19.final$SampleSize
item19.final$SE      <- sqrt(item19.final$Percent * (1 - item19.final$Percent) / item19.final$SampleSize)

#list of site IDs with basements for future tables
basement.ids <- unique(item19.dat4$CK_Cadmus_ID)


#############################################################################################
# Item 20: PERCENTAGE OF BASEMENTS THAT ARE CONDITIONED BY STATE (SF table 27)
#############################################################################################

item20.dat <- envelope.dat[,c(which(colnames(envelope.dat) == "CK_Cadmus_ID"), grep("BSMT", colnames(envelope.dat)))]

item20.dat1 <- left_join(rbsa.dat, item20.dat, by = "CK_Cadmus_ID")

item20.dat2 <- item20.dat1[which(item20.dat1$BuildingType == "Single Family"),]

item20.dat3 <- item20.dat2[which(item20.dat2$CK_Cadmus_ID %in% basement.ids),]
full.ind <- unique(item20.dat3$CK_Cadmus_ID) ##72

item20.dat4 <- item20.dat3[which(!(is.na(item20.dat3$ENV_Foundation_BSMT_BasementConditioned))),]
sub.ind <- unique(item20.dat4$CK_Cadmus_ID) ##68 (four are missing)

full.ind[which(!(full.ind %in% sub.ind))]

item20.dat4$count <- 1
item20.dat4$bsmt.ind <- 0
item20.dat4$bsmt.ind[which(item20.dat4$ENV_Foundation_BSMT_BasementConditioned == "Yes")] <- 1

item20.tmp1 <- summarise(group_by(item20.dat4, BuildingType, State)
                          ,insulatedCount = sum(bsmt.ind)
                          ,SampleSize = sum(count))

item20.tmp2 <- summarise(group_by(item20.dat4, BuildingType)
                         , State = "Region"
                          ,insulatedCount = sum(bsmt.ind)
                          ,SampleSize = sum(count))

item20.final <- rbind.data.frame(item20.tmp1, item20.tmp2, stringsAsFactors = F)
item20.final$Percent <- item20.final$insulatedCount / item20.final$SampleSize
item20.final$SE      <- sqrt(item20.final$Percent * (1 - item20.final$Percent) / item20.final$SampleSize)



#############################################################################################
# Item 21: DISTRIBUTION OF BASEMENT SLAB INSULATION BY INSULATION LEVEL (SF table 28)
#############################################################################################

item21.dat <- envelope.dat[,c(which(colnames(envelope.dat) == "CK_Cadmus_ID"), grep("BSMT", colnames(envelope.dat)))]
