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
# Item 17: DISTRIBUTION OF MASONRY WALL INSULATION LEVELS BY HOME VINTAGE (SF table 24)
#############################################################################################

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(analysisInPath, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-23,-3]

#subset envelope data to necessary columns
item17.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Wall.Type"
                                                               , "Wall.Area"
                                                               , "Furred.Wall?"                                                                   
                                                               , "Furred.Wall.Insulated?"                                                         
                                                               , "Furred.Wall.Insulation.Type"                                                    
                                                               , "Furred.Wall.Insulation.Thickness"                                               
                                                               , "Furred.Wall.Insulation.Condition"))]
length(unique(item17.dat$CK_Cadmus_ID))#547

#trim white space from cadmus IDs
item17.dat$CK_Cadmus_ID <- trimws(item17.dat$CK_Cadmus_ID)

#change Masonry (basement) to just Masonry
item17.dat[which(item17.dat$Wall.Type == "Masonry (Basement)"),] <- "Masonry"

#subset to only wall information
item17.dat1 <- item17.dat[which(item17.dat$Category == "Wall"),]
unique(item17.dat1$Wall.Type)

#remove unneccesary wall types
item17.dat2 <- item17.dat1[which(item17.dat1$Wall.Type == "Masonry"),]
length(unique(item17.dat2$CK_Cadmus_ID))#154
unique(item17.dat2$Wall.Type)


item17.dat3 <- item17.dat2

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
item17.dat3$Furred.Wall.Insulation.Thickness[which(item17.dat3$Furred.Wall.Insulation.Thickness == "Unknown")] <- "Unknown Unknown"
item17.dat3$Furred.Wall.Insulation.Thickness[which(item17.dat3$Furred.Wall.Insulation.Thickness == "N/A")] <- "N/A N/A"
item17.dat3$Furred.Wall.Insulation.Thickness[which(is.na(item17.dat3$Furred.Wall.Insulation.Thickness))] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
item17.dat3$count <- 1
item17.dat3$TMP_ID <- cumsum(item17.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item17.dat3$Furred.Wall.Insulation.Thickness, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul <- cbind.data.frame("CK_Cadmus_ID" = item17.dat3$CK_Cadmus_ID
                                , "TMP_ID" = item17.dat3$TMP_ID
                                , clean.insul2)
dim(clean.insul)

###########################
# End cleaning step
###########################

#make into dataframe
item17.dat4 <- as.data.frame(left_join(item17.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                             , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item17.dat4$inches1 <- as.numeric(as.character(item17.dat4$V1)) # warning here is OK

item17.dat4$rvalues1 <- item17.dat4$Furred.Wall.Insulation.Type

#check uniques
unique(item17.dat4$rvalues1)

#fix names that are not in R value table
item17.dat4$rvalues1[which(item17.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item17.dat4$rvalues1[which(item17.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item17.dat4$rvalues1[which(item17.dat4$rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
item17.dat4$rvalues1[which(item17.dat4$rvalues1 == "Expanded polystyrene foam board (white)")]        <- "Expanded polystyrene foam board"
item17.dat4$rvalues1[which(item17.dat4$rvalues1 == "Foil-faced polyiscyanurate foam board")]          <- "Foil-faced polyurethan foam board"


###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item17.dat4$rvalues1[which(item17.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item17.dat5 <- item17.dat4

###########################
# Cleaning step (NA to zero)
###########################
#For now: convert NAs in rvalues and inches to zero for summarizing purposes (will revert back to NAs after)
item17.dat5$rvalues1[which(is.na(item17.dat5$rvalues1))] <- 0
item17.dat5$rvalues1[which(item17.dat5$rvalues1 == "None")] <- 0

#QC the clean bulb values
unique(item17.dat5$rvalues1)

item17.dat5$inches1[which(is.na(item17.dat5$inches1))] <- 0

#QC the clean bulb values -- There are NAs in each 
unique(item17.dat5$inches1)
###########################
# End Cleaning step
###########################


# r values multiplied by inches
item17.dat5$total.r.val <- (as.numeric(as.character(item17.dat5$rvalues1)) * item17.dat5$inches1)
unique(item17.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item17.dat5$ufactor <- 1 / as.numeric(as.character(item17.dat5$total.r.val))

# replace inf with 0
item17.dat5$ufactor[which(item17.dat5$ufactor == "Inf")] <- 0

#make area numeric
item17.dat5$ufactor <- as.numeric(as.character(item17.dat5$ufactor))
item17.dat5$Wall.Area <- as.numeric(as.character(item17.dat5$Wall.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item17.dat5, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * as.numeric(as.character(ufactor))) / sum(Wall.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

wall.unique <- unique(item17.dat5[which(colnames(item17.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item17.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")



############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
item17.dat6$aveRval[which(item17.dat6$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
wall.ins.ind  <- item17.dat5$CK_Cadmus_ID[which(item17.dat5$`Furred.Wall.Insulated?` == "No")]
wall.r.NA.ind <- item17.dat6$CK_Cadmus_ID[which(is.na(item17.dat6$aveRval))]

#which NA R values are in no insulation?
no.insulation <- wall.r.NA.ind[which(wall.r.NA.ind %in% wall.ins.ind)]

#replace no insulation in home ceiling with zero
item17.dat6$aveRval[which(item17.dat6$CK_Cadmus_ID %in% no.insulation)] <- 0
############################################################################################################
## END Process
############################################################################################################

item17.dat7 <- left_join(item17.dat6, rbsa.dat, by = "CK_Cadmus_ID")
unique(item17.dat7$BuildingType)

#Bin R values -- SF only
item17.dat7$rvalue.bins <- "Unknown"
item17.dat7$rvalue.bins[which(item17.dat7$aveRval == 0)] <- "R0"
item17.dat7$rvalue.bins[which(item17.dat7$aveRval > 0  & item17.dat7$aveRval < 11)]  <- "R1.R17"
item17.dat7$rvalue.bins[which(item17.dat7$aveRval >= 11 & item17.dat7$aveRval < 17)]  <- "R11.R16"
item17.dat7$rvalue.bins[which(item17.dat7$aveRval >= 17 & item17.dat7$aveRval < 23)]  <- "R17.R22"
item17.dat7$rvalue.bins[which(item17.dat7$aveRval >= 22)] <- "RGT22"
unique(item17.dat7$rvalue.bins)

##cast data
item17.dat7$count <- 1
item17.dat.cast <- dcast(setDT(item17.dat7),
                         formula   = CK_Cadmus_ID + BuildingType + HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')

head(item17.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item17.SF.dat <- subset(item17.dat.cast, item17.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item17.sum <- summarise(group_by(item17.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,r0.percent      = sum(R0) / sampleSizeNoNA ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
                        ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                        ,r1.r17.percent  = sum(R1.R17)  / sampleSizeNoNA
                        ,r1.r17.se       = sd(R1.R17) / sqrt(sampleSizeNoNA)
                        ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                        ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                        ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                        ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                        ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                        ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)

item17.SF.dat$count <- 1
item17.sum.allLevels <- summarise(group_by(item17.SF.dat, BuildingType, HomeYearBuilt_bins4)
                                  ,WallTypeCount = sum(count)
                                  ,TotalCount = sum(item17.SF.dat$count)
                                  ,AllInsulationLevelPercent = WallTypeCount / TotalCount
                                  ,AllInsulationSE = sqrt((AllInsulationLevelPercent * (1 - AllInsulationLevelPercent)) / WallTypeCount)
)

#Check to make sure they add to 1
sum(item17.sum.allLevels$AllInsulationLevelPercent)

#join all insulation levels onto rvalue summary
item17.withVintages.final <- cbind.data.frame(item17.sum
                                              , "All Insulation Levels Mean" = item17.sum.allLevels$AllInsulationLevelPercent
                                              , "All Insulation Levels SE"   = item17.sum.allLevels$AllInsulationSE)

####across vintages
#summarize --SF only
item17.sum.allVintages <- summarise(group_by(item17.SF.dat, BuildingType)
                                    ,HomeYearBuilt_bins4 = "All Vintages"
                                    ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                                    ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                                    ,r0.percent      = sum(R0) / sampleSizeNoNA ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
                                    ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                                    ,r1.r17.percent  = sum(R1.R17)  / sampleSizeNoNA
                                    ,r1.r17.se       = sd(R1.R17) / sqrt(sampleSizeNoNA)
                                    ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                                    ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                                    ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                                    ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                                    ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                                    ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)

item17.sum.allLevelsVintages <- summarise(group_by(item17.SF.dat, BuildingType)
                                          ,HomeYearBuilt_bins4 = "All Vintages"
                                          ,WallTypeCount = sum(count)
                                          ,TotalCount = sum(item17.SF.dat$count)
                                          ,AllInsulationLevelPercent = WallTypeCount / TotalCount
                                          ,AllInsulationSE = sqrt((AllInsulationLevelPercent * (1 - AllInsulationLevelPercent)) / WallTypeCount)
)

#Check to make sure they add to 1
sum(item17.sum.allLevelsVintages$AllInsulationLevelPercent)

#join all insulation levels onto rvalue summary
item17.allVintages.final <- cbind.data.frame(item17.sum.allVintages
                                             , "All Insulation Levels Mean" = item17.sum.allLevelsVintages$AllInsulationLevelPercent
                                             , "All Insulation Levels SE"   = item17.sum.allLevelsVintages$AllInsulationSE)

item17.final <- rbind.data.frame(item17.withVintages.final, item17.allVintages.final, stringsAsFactors = F)













#############################################################################################
# Item 18: DISTRIBUTION OF OBSERVED WALL SHEATHING INSULATION BY FRAMING TYPE (SF table 25)
#############################################################################################

#subset envelope data to necessary columns
item18.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Wall.Type"
                                                               , "Wall.Area"
                                                               , "Wall.Framing.Size"
                                                               , "Wall.Exterior.Insulated?"
                                                               , "Wall.Exterior.Insulation.Type.1"
                                                               , "Wall.Exterior.Insulation.Thickness.1"
                                                               , "Wall.Exterior.Insulation.Condition.1"
                                                               # , "Wall.Exterior.Insulation.Type.2"
                                                               # , "Wall.Exterior.Insulation.Thickness.2"
                                                               # , "Wall.Exterior.Insulation.Condition.2"
                                                               # , "Wall.Exterior.Insulation.Type.3"
                                                               # , "Wall.Exterior.Insulation.Thickness.3"
                                                               # , "Wall.Exterior.Insulation.Condition.3"
))]
length(unique(item18.dat$CK_Cadmus_ID))#547


#trim white space from cadmus IDs
item18.dat$CK_Cadmus_ID <- trimws(item18.dat$CK_Cadmus_ID)

#merge with cleaned RBSA data
item18.dat0 <- left_join(rbsa.dat, item18.dat, by = "CK_Cadmus_ID")

#subset to only wall information
item18.dat1 <- item18.dat0[which(item18.dat0$Category == "Wall"),]

#remove unneccesary wall types
item18.dat2 <- item18.dat1[which(!(item18.dat1$Wall.Type %in% c("Masonry","Masonry (Basement)","Log","Adiabatic"))),]

#create "Alternative" category
item18.dat2$Wall.Type[which(item18.dat2$Wall.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
length(unique(item18.dat2$CK_Cadmus_ID))#473
unique(item18.dat2$Wall.Type)

#remove items that have the datapoint was not asked for from wall exterior insulated
item18.dat3 <- item18.dat2[which(item18.dat2$`Wall.Exterior.Insulated?` != "-- Datapoint not asked for --"),]
length(unique(item18.dat3$CK_Cadmus_ID))#469
unique(item18.dat3$`Wall.Exterior.Insulated?`)


#remove items that have the datapoint was not asked for from wall framing size
item18.dat4 <- item18.dat3[which(item18.dat3$Wall.Framing.Size != "-- Datapoint not asked for --"),]

item18.dat4$Wall.Exterior.Insulation.Thickness.1[which(item18.dat4$`Wall.Exterior.Insulated?` == "No")] <- "None"
item18.dat4$Wall.Exterior.Insulation.Thickness.1[which(item18.dat4$`Wall.Exterior.Insulated?` == "Unknown")] <- "Unknown Insulation"


item18.dat5 <- item18.dat4[which(item18.dat4$BuildingType == "Single Family"),]

###########################
# Summarise
###########################
item18.dat5$count <- 1
item18.frameType <- summarise(group_by(item18.dat5, BuildingType, Wall.Framing.Size, Wall.Exterior.Insulation.Thickness.1)
                              , Count = sum(count))
item18.allFrameTypes <- summarise(group_by(item18.dat5, BuildingType, Wall.Exterior.Insulation.Thickness.1)
                                  , Wall.Framing.Size = "All Framing Types"
                                  , Count = sum(count))

item18.tmp <- rbind.data.frame(item18.frameType, item18.allFrameTypes, stringsAsFactors = F)


item18.frameTypeCount <- summarise(group_by(item18.dat5, BuildingType, Wall.Framing.Size)
                                   ,sampleSize = sum(count))
item18.sampleSize <- summarise(group_by(item18.dat5, BuildingType)
                               ,Wall.Framing.Size = "All Framing Types"
                               ,sampleSize = sum(count))

item18.tmp1 <- rbind.data.frame(item18.frameTypeCount, item18.sampleSize, stringsAsFactors = F)


item18.final <- left_join(item18.tmp, item18.tmp1, by = c("Wall.Framing.Size", "BuildingType"))
item18.final$Percent <- item18.final$Count / item18.final$sampleSize
item18.final$SE      <- sqrt(item18.final$Percent * (1 - item18.final$Percent) / item18.final$sampleSize)

detach(package:reshape2)
library(data.table)
item18.table <- dcast(setDT(item18.final)
                      , formula = BuildingType + Wall.Framing.Size + sampleSize ~ Wall.Exterior.Insulation.Thickness.1
                      , value.var = c("Percent", "SE"))
