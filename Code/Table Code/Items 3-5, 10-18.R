#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
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
stopifnot(all(file.exists(SPPath)))

rbsa.dat <- read.xlsx(xlsxFile = file.path(cleanInPath, paste("clean.rbsa.data", rundate, ".xlsx")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

envelope.dat <- read.xlsx(xlsxFile = file.path(SPPath, "Envelope_EquipConsol_2017.04.25.xlsx"))

#############################################################################################
# Item 3: DISTRIBUTION OF HOMES BY GROUND CONTACT TYPE AND STATE 
#############################################################################################
#subset to columns need in table
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_FoundationType"))]
env.dat1 <- env.dat[which(!(is.na(env.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType))),]

#merge table columns to generic columns
item3.dat <- unique(left_join(rbsa.dat, env.dat1, by = "CK_Cadmus_ID"))
item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType <- trimws(item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType)

## clean ground contact type info
unique(item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType)
item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType[which(item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType == "90% slab")] <- ">90% Slab"
item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType[which(item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType == ">90% crawl")] <- ">90% Crawlspace"
item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType[which(item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType == ">90% slab")] <- ">90% Slab"








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

item4.dat3 <- summarise(group_by(item4.dat2, CK_Cadmus_ID, State)
                      ,siteAreaConditioned = sum(ConditionedArea)
)

item4.dat4 <- summarise(group_by(item4.dat3, State)
                        ,AveAreaConditioned = mean(siteAreaConditioned))


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

item5.dat3 <- summarise(group_by(item5.dat2, CK_Cadmus_ID, State, HomeYearBuilt_bins)
                        ,siteAreaConditioned = sum(ConditionedArea)
)

item5.dat4 <- summarise(group_by(item5.dat3, State, HomeYearBuilt_bins)
                        ,AveAreaConditioned = mean(siteAreaConditioned))





#############################################################################################
# Item 10: AVERAGE CONDITIONED FLOOR AREA BY STATE AND BUILDING HEIGHT
#############################################################################################





















#############################################################################################
# Item 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE 
#############################################################################################


item23.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                              , "Category"
                                                              , "Floor.Type"
                                                              , "Floor.Insulated?"
                                                              , "Floor.Insulation.Type.1"
                                                              , "Floor.Insulation.Thickness.1"
                                                              , "Floor.Insulation.Type.2"
                                                              , "Floor.Insulation.Thickness.2"
                                                              , "Floor.Insulation.Type.3"
                                                              , "Floor.Insulation.Thickness.3"
                                                              , "Type.of.Insulation"
                                                              , "Floor.Area"))]
item23.dat1 <- left_join(rbsa.dat, item23.dat, by = "CK_Cadmus_ID")
item23.dat2 <- item23.dat1[which(item23.dat1$Category == "Floor"),]
item23.dat3 <- item23.dat1[which(item23.dat1$Floor.Type != "Slab"),]

#check single family information
sf.dat <- rbsa.dat[which(rbsa.dat$BuildingType == "Single Family"),]
length(unique(sf.dat$CK_Cadmus_ID)) #333
sf.sub.dat <- item23.dat3[which(item23.dat3$BuildingType == "Single Family"),]
length(unique(sf.sub.dat$CK_Cadmus_ID)) #275 - lost 58 sites b/c no floor insulation recorded

unique(item23.dat3$Floor.Type)

#convert No wall insulation to zero and remove NA
item23.dat3$Floor.Insulation.Type.1[which(item23.dat3$`Floor.Insulated?` == "No")] <- 0 ##question for Rietz, should we be removing "Unknown" floor insulation?
item23.dat4 <- item23.dat3[-which(is.na(item23.dat3$Floor.Insulation.Type.1)),]

#fix missing / unknown floor insulation thickness values
item23.dat4$Floor.Insulation.Thickness.1[which(item23.dat4$Floor.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item23.dat4$Floor.Insulation.Thickness.1[which(item23.dat4$Floor.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item23.dat4$Floor.Insulation.Thickness.1[which(is.na(item23.dat4$Floor.Insulation.Thickness.1))] <- "N/A N/A"
item23.dat4$Floor.Insulation.Thickness.2[which(item23.dat4$Floor.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item23.dat4$Floor.Insulation.Thickness.2[which(item23.dat4$Floor.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item23.dat4$Floor.Insulation.Thickness.2[which(is.na(item23.dat4$Floor.Insulation.Thickness.2))] <- "N/A N/A"
item23.dat4$Floor.Insulation.Thickness.3[which(item23.dat4$Floor.Insulation.Thickness.3 == "Unknown")] <- "Unknown Unknown"
item23.dat4$Floor.Insulation.Thickness.3[which(item23.dat4$Floor.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
item23.dat4$Floor.Insulation.Thickness.3[which(is.na(item23.dat4$Floor.Insulation.Thickness.3))] <- "N/A N/A"

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(sourcePath, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-23,-3]

# add new ID variable for merging
item23.dat4$count <- 1
item23.dat4$TMP_ID <- cumsum(item23.dat4$count) 

## r-values ##
clean.insul1 <- unlist(strsplit(item23.dat4$Floor.Insulation.Thickness.1, " "))
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item23.dat4$CK_Cadmus_ID
                                   , "TMP_ID" = item23.dat4$TMP_ID
                                   , as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul1.1)

##ignore for now -- get rietz to look into floor.insulation.thickness.2 for SG0776 OS SCL################################################################
# clean.insul2 <- unlist(strsplit(item23.dat4$Floor.Insulation.Thickness.2, " "))
# clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item23.dat4$CK_Cadmus_ID
#                                    , "TMP_ID" = item23.dat4$TMP_ID
#                                    , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
#                                                    , stringsAsFactors = F))
# dim(clean.insul2.1)

#make into dataframe
item23.dat5 <- as.data.frame(left_join(item23.dat4, clean.insul1.1, by = c("CK_Cadmus_ID", "TMP_ID"))
                            , stringsAsFactors = F) # warning here is OK

# rename columns
item23.dat5$inches1 <- as.numeric(as.character(item23.dat5$V1)) # warning here is OK
item23.dat5$rvalues1 <- item23.dat5$Floor.Insulation.Type.1

#replace names with values
for (i in 1:length(rvals$Type.of.Insulation)){
  item23.dat5$rvalues1[which(item23.dat5$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

#replace NA and none with zero
item23.dat5$rvalues1[which(item23.dat5$rvalues1 == "None")] <- 0
item23.dat5$inches1[which(is.na(item23.dat5$inches1))] <- 0

#QC the clean bulb values
unique(item23.dat5$rvalues1)
unique(item23.dat5$inches1)

# r values multiplied by inches
item23.dat5$total.r.val <- (as.numeric(as.character(item23.dat5$rvalues1)) * item23.dat5$inches1)

#caluclate u factors = inverse of Rvalue
item23.dat5$ufactor <- 1 / as.numeric(as.character(item23.dat5$total.r.val))
item23.dat5$ufactor <- as.numeric(as.character(item23.dat5$ufactor))

# replace inf with 0
item23.dat5$ufactor[which(item23.dat5$ufactor == "Inf")] <- 0

#make area numeric
item23.dat5$Floor.Area <- as.numeric(as.character(item23.dat5$Floor.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item23.dat5, CK_Cadmus_ID)
                       ,aveUval = sum(Floor.Area * as.numeric(as.character(ufactor))) / sum(Floor.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

item23.dat6 <- unique(item23.dat5[which(colnames(item23.dat5) %in% c("CK_Cadmus_ID", "BuildingType", "HomeYearBuilt", "State"))])

item23.dat7 <- left_join(weightedU, item23.dat6, by = "CK_Cadmus_ID")
length(unique(item23.dat7$CK_Cadmus_ID))



