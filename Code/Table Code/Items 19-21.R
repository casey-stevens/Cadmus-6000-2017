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

rbsa.dat <- read.xlsx(xlsxFile = file.path(cleanInPath, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

envelope.dat <- read.xlsx(xlsxFile = file.path(SPPath, "Envelope_EquipConsol_2017.04.25.xlsx"))








#############################################################################################
# Item 19: PERCENTAGE OF HOMES WITH BASEMENTS BY STATE (SF table 26)
#############################################################################################
item19.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Floor.Type"
                                                               ,"Floor.Sub-Type"))]

item19.dat1 <- left_join(rbsa.dat, item19.dat, by = "CK_Cadmus_ID")
length(unique(item19.dat1$CK_Cadmus_ID))#565

#subset to only single family homes
item19.dat2 <- item19.dat1[which(item19.dat1$BuildingType == "Single Family"),]
item19.dat2$count <- 1

item19.dat3 <- item19.dat2[which(item19.dat2$Floor.Type == "Basement"),]
item19.dat3$cond.ind <- 0
item19.dat3$cond.ind[which(item19.dat3$`Floor.Sub-Type` == "Conditioned")] <- 1

#summarise -- State JUST Basement
item19.sum1 <- summarise(group_by(item19.dat3, BuildingType, State)
                         , BSMTCount = sum(count))
#summarise -- State All Sites Basement
item19.sum2 <- summarise(group_by(item19.dat2, BuildingType, State)
                         , SampleSize = length(unique(CK_Cadmus_ID)))

#summarise -- Region JUST Basement
item19.sum3 <- summarise(group_by(item19.dat3, BuildingType)
                         , State = "Region"
                         , BSMTCount = sum(count))
#summarise -- Region All Sites Basement
item19.sum4 <- summarise(group_by(item19.dat2, BuildingType)
                         , State = "Region"
                         , SampleSize = length(unique(CK_Cadmus_ID)))

## rbind state and region sample sizes
item19.tmp1 <- left_join(item19.sum1, item19.sum2, by = c("BuildingType", "State"))
item19.tmp2 <- left_join(item19.sum3, item19.sum4, by = c("BuildingType", "State"))

item19.final <- rbind.data.frame(item19.tmp1, item19.tmp2, stringsAsFactors = F)

item19.final$Percent <- item19.final$BSMTCount / item19.final$SampleSize
item19.final$SE <- sqrt(item19.final$Percent * (1 - item19.final$Percent) / item19.final$SampleSize)







#############################################################################################
# Item 20: PERCENTAGE OF BASEMENTS THAT ARE CONDITIONED BY STATE (SF table 27)
#############################################################################################
item20.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Floor.Type"
                                                               ,"Floor.Sub-Type"))]

item20.dat1 <- left_join(rbsa.dat, item20.dat, by = "CK_Cadmus_ID")
length(unique(item20.dat1$CK_Cadmus_ID))#565

#subset to only single family homes
item20.dat2 <- item20.dat1[which(item20.dat1$BuildingType == "Single Family"),]
item20.dat2$count <- 1

item20.dat3 <- item20.dat2[which(item20.dat2$Floor.Type == "Basement"),]
item20.dat3$cond.ind <- 0
item20.dat3$cond.ind[which(item20.dat3$`Floor.Sub-Type` == "Conditioned")] <- 1

#summarise -- State
item20.sum1 <- summarise(group_by(item20.dat3, BuildingType, State)
                         , ConditionedCount = sum(cond.ind) 
                         , SampleSize = sum(count)
                         , Percent = ConditionedCount / SampleSize
                         , SE = sqrt(Percent * (1 - Percent) / SampleSize))
##Region
item20.sum2 <- summarise(group_by(item20.dat3, BuildingType)
                         , State = "Region"
                         , ConditionedCount = sum(cond.ind) 
                         , SampleSize = sum(count)
                         , Percent = ConditionedCount / SampleSize
                         , SE = sqrt(Percent * (1 - Percent) / SampleSize))
## rbind state and region sample sizes
item20.final <- rbind.data.frame(item20.sum1, item20.sum2, stringsAsFactors = F)



#############################################################################################
# Item 21: DISTRIBUTION OF BASEMENT SLAB INSULATION BY INSULATION LEVEL (SF table 28)
#############################################################################################

item21.dat <- envelope.dat[,c(which(colnames(envelope.dat) == "CK_Cadmus_ID"), grep("BSMT", colnames(envelope.dat)))]
