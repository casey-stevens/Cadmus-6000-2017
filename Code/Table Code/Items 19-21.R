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

#summarise
item19.tmp1 <- summarise(group_by(item19.dat3, BuildingType, State)
                          , SampleSize = length(unique(CK_Cadmus_ID)))


item19.dat4 <- item19.dat3[which(item19.dat3$Clean.Type == "Basement"),]
item19.tmp2 <- summarise(group_by(item19.dat4, BuildingType, State)
                         , BasementCount = length(unique(CK_Cadmus_ID)))

item19.final <- left_join(item19.tmp1, item19.tmp2, by = c("BuildingType", "State"))
item19.final$Percent <- item19.final$BasementCount / item19.final$SampleSize
item19.final$SE      <- sqrt(item19.final$Percent * (1 - item19.final$Percent) / item19.final$SampleSize)

################################################################################################################ Need to do this for region level as well

basement.ids <- unique(item19.dat4$CK_Cadmus_ID)


#############################################################################################
# Item 20: PERCENTAGE OF BASEMENTS THAT ARE CONDITIONED BY STATE (SF table 27)
#############################################################################################


################################################################################################################ Need to do this for region level as well

item20.dat <- envelope.dat[,c(which(colnames(envelope.dat) == "CK_Cadmus_ID"), grep("BSMT", colnames(envelope.dat)))]

item20.dat1 <- left_join(rbsa.dat, item20.dat, by = "CK_Cadmus_ID")

item20.dat2 <- item20.dat1[which(item20.dat1$BuildingType == "Single Family"),]

item20.dat3 <- item20.dat2[which(item20.dat2$CK_Cadmus_ID %in% basement.ids),]
length(unique(item20.dat3$CK_Cadmus_ID)) ##72


