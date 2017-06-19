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
SPPath   <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Data for PSE"
cleanInPath <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Analysis Documents/Clean Data"
analysisInPath <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Analysis Documents"
stopifnot(all(file.exists(SPPath, cleanInPath, analysisInPath)))

rbsa.dat <- read.xlsx(xlsxFile = file.path(cleanInPath, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

envelope.dat <- read.xlsx(xlsxFile = file.path(SPPath, "Envelope_EquipConsol_2017.06.16.xlsx"))

#############################################################################################
# Item 3: DISTRIBUTION OF HOMES BY GROUND CONTACT TYPE AND STATE 
#############################################################################################
# Bring in clean ground contact types
GroundContactTypes <- read.xlsx(xlsxFile = file.path(analysisInPath, "Ground Contact Types.xlsx"), sheet = 1)
GroundContactTypes <- GroundContactTypes[,-3]

#subset to columns need in table
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_FoundationType"))]
env.dat1 <- env.dat[which(!(is.na(env.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType))),]

#merge table columns to generic columns
item3.dat <- unique(left_join(rbsa.dat, env.dat1, by = "CK_Cadmus_ID"))
item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType <- trimws(item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType)

###########################
# Clean Ground Contact types
###########################
item3.dat$GroundContact <- item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType
for (i in 1:length(GroundContactTypes$Raw.data.categories)){
  item3.dat$GroundContact[which(item3.dat$GroundContact == GroundContactTypes$Raw.data.categories[i])] <- GroundContactTypes$New.categories[i]
}
###########################
# End cleaning Step
###########################
unique(item3.dat$GroundContact)

# Remove unwanted ground contact types
item3.dat1 <- item3.dat[which(item3.dat$GroundContact != "Remove"),]
item3.dat2 <- item3.dat1[which(item3.dat1$BuildingType == "Single Family"),]


##Summarise by state and ground contact type
item3.dat2$count <- 1
item3.dat.3.1 <- summarise(group_by(item3.dat2, BuildingType, State, GroundContact)
                        ,Count = sum(count)
                        )
item3.dat.3.2 <- summarise(group_by(item3.dat2, BuildingType, State)
                           ,State_Count = sum(count)
)

item3.dat3 <- left_join(item3.dat.3.1, item3.dat.3.2, by = c("BuildingType", "State"))
item3.dat3$Percent <- item3.dat3$Count / item3.dat3$State_Count


##Summarise by state across ground contact type - Totals by state
item3.dat4 <- summarise(group_by(item3.dat2, BuildingType, State)
                           ,GroundContact = "Total"
                           ,Count = sum(count)
                           ,State_Count = sum(count)
)

item3.dat4$Percent <- item3.dat4$Count / item3.dat4$State_Count

#rbind state with and across ground contact types
item3.dat5 <- rbind.data.frame(item3.dat3, item3.dat4, stringsAsFactors = F)


###Sumamrise by ground contact type across states
item3.dat.6.1 <- summarise(group_by(item3.dat2, BuildingType, GroundContact)
                           ,State = "Region"
                           ,Count = sum(count)
)
item3.dat.6.2 <- summarise(group_by(item3.dat2, BuildingType)
                           ,State = "Region"
                           ,State_Count = sum(count)
)

item3.dat6 <- left_join(item3.dat.6.1, item3.dat.6.2, by = c("BuildingType", "State"))
item3.dat6$Percent <- item3.dat6$Count / item3.dat6$State_Count


##Summarise across states and ground contact types
item3.dat7 <- summarise(group_by(item3.dat2, BuildingType)
                           ,State = "Region"
                           ,GroundContact = "Total"
                           ,Count = sum(count)
                           ,State_Count = sum(count)
)

item3.dat7$Percent <- item3.dat7$Count / item3.dat7$State_Count

#rbind state with and across States
item3.dat8 <- rbind.data.frame(item3.dat6, item3.dat7, stringsAsFactors = F)

# Join tem3.dat5 with item3.dat8
item3.final <- rbind.data.frame(item3.dat5, item3.dat8, stringsAsFactors = F)
item3.final$SE <- sqrt(item3.final$Percent * (1 - item3.final$Percent) / item3.final$State_Count)



#############################################################################################
# Item 4: AVERAGE CONDITIONED FLOOR AREA BY STATE
#############################################################################################

item4.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]

#merge
item4.dat1 <- left_join(rbsa.dat, item4.dat, by = "CK_Cadmus_ID")
length(unique(item4.dat1$CK_Cadmus_ID)) #565, yay!

#remove NAs
item4.dat2 <- item4.dat1[which(!(is.na(item4.dat1$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))),]
length(unique(item4.dat2$CK_Cadmus_ID)) #410, boo!

#make conditioned area as.numeric
item4.dat2$ConditionedArea <- as.numeric(as.character(item4.dat2$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))

#summarise by state
item4.state.dat <- summarise(group_by(item4.dat2,BuildingType , CK_Cadmus_ID, State)
                      ,siteAreaConditioned = sum(ConditionedArea)
)

item4.state.dat1 <- summarise(group_by(item4.state.dat, BuildingType, State)
                        ,AveAreaConditioned = mean(siteAreaConditioned)
                        ,SEAreaConditioned  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                        ,SampleSize         = length(unique(CK_Cadmus_ID))
                        )

#summarise by region
item4.region.dat <- summarise(group_by(item4.dat2, BuildingType, CK_Cadmus_ID)
                              ,State = "Region"
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item4.region.dat1 <- summarise(group_by(item4.region.dat, BuildingType)
                               ,State = "Region"
                              ,AveAreaConditioned = mean(siteAreaConditioned)
                              ,SEAreaConditioned  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize         = length(unique(CK_Cadmus_ID))
                              )

item4.final <- rbind.data.frame(item4.state.dat1, item4.region.dat1, stringsAsFactors = F) 

#############################################################################################
# Item 5: AVERAGE CONDITIONED FLOOR AREA BY STATE AND VINTAGE
##########################################################################item4.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]

item5.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]

#merge
item5.dat1 <- left_join(rbsa.dat, item5.dat, by = "CK_Cadmus_ID")
length(unique(item5.dat1$CK_Cadmus_ID)) #565, yay!

#remove NAs
item5.dat2 <- item5.dat1[which(!(is.na(item5.dat1$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))),]
length(unique(item5.dat2$CK_Cadmus_ID)) #410, boo!

#make conditioned area as.numeric
item5.dat2$ConditionedArea <- as.numeric(as.character(item5.dat2$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))


#### By state/region across homeyearbuilt
#summarise by state
item5.state.dat00 <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID, State)
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item5.state.dat01 <- summarise(group_by(item5.state.dat00, BuildingType, State)
                               ,HomeYearBuilt_bins = "All Vintages"
                              ,AveAreaConditioned = mean(siteAreaConditioned)
                              ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize = length(unique(CK_Cadmus_ID))
)

#summarise by region
item5.region.dat00 <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID)
                              ,State = "Region"
                              ,siteAreaConditioned = sum(ConditionedArea)
)

item5.region.dat01 <- summarise(group_by(item5.region.dat00, BuildingType)
                               ,State = "Region"
                               ,HomeYearBuilt_bins = "All Vintages"
                               ,AveAreaConditioned = mean(siteAreaConditioned)
                               ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)

item5.tmp1 <- rbind.data.frame(item5.state.dat01, item5.region.dat01, stringsAsFactors = F) 



#### By state/region and homeyearbuilt bins
#summarise by state
item5.state.dat <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID, State, HomeYearBuilt_bins)
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item5.state.dat1 <- summarise(group_by(item5.state.dat, BuildingType, State, HomeYearBuilt_bins)
                              ,AveAreaConditioned = mean(siteAreaConditioned)
                              ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize = length(unique(CK_Cadmus_ID))
                              )

#summarise by region
item5.region.dat <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID, HomeYearBuilt_bins)
                              ,State = "Region"
                              ,siteAreaConditioned = sum(ConditionedArea)
)

item5.region.dat1 <- summarise(group_by(item5.region.dat, BuildingType, HomeYearBuilt_bins)
                               ,State = "Region"
                               ,AveAreaConditioned = mean(siteAreaConditioned)
                               ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)

item5.tmp2 <- rbind.data.frame(item5.state.dat1, item5.region.dat1, stringsAsFactors = F) 


item5.final <- rbind.data.frame(item5.tmp1, item5.tmp2, stringsAsFactors = F)

