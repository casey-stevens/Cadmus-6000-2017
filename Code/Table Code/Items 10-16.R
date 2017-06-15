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
# Item 10: DISTRIBUTION OF FRAME WALL INSULATION LEVELS BY FRAMING TYPE (SF table 17)
#############################################################################################

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(analysisInPath, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-23,-3]

#subset envelope data to necessary columns
item10.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Wall.Type"
                                                               , "Wall.Area"
                                                               , "Wall.Framing.Size"
                                                               , "Wall.Cavity.Insulated?"
                                                               , "Wall.Cavity.Insulation.Type.1"
                                                               , "Wall.Cavity.Insulation.Thickness.1"
                                                               , "Wall.Cavity.Insulation.Type.2"                                                  
                                                               , "Wall.Cavity.Insulation.Thickness.2"))]
length(unique(item10.dat$CK_Cadmus_ID))#547

#trim white space from cadmus IDs
item10.dat$CK_Cadmus_ID <- trimws(item10.dat$CK_Cadmus_ID)

#subset to only wall information
item10.dat1 <- item10.dat[which(item10.dat$Category == "Wall"),]

#remove unneccesary wall types
item10.dat2 <- item10.dat1[which(!(item10.dat1$Wall.Type %in% c("Masonry","Masonry (Basement)","Log","Adiabatic"))),]

#create "Alternative" category
item10.dat2$Wall.Type[which(item10.dat2$Wall.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
length(unique(item10.dat2$CK_Cadmus_ID))#473
unique(item10.dat2$Wall.Type)

#remove items have the datapoint was not asked for
item10.dat3 <- item10.dat2[which(item10.dat2$Wall.Framing.Size != "-- Datapoint not asked for --"),]
length(unique(item10.dat3$CK_Cadmus_ID))#469
unique(item10.dat3$Wall.Framing.Size)

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
item10.dat3$Wall.Cavity.Insulation.Thickness.1[which(item10.dat3$Wall.Cavity.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item10.dat3$Wall.Cavity.Insulation.Thickness.1[which(item10.dat3$Wall.Cavity.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item10.dat3$Wall.Cavity.Insulation.Thickness.1[which(is.na(item10.dat3$Wall.Cavity.Insulation.Thickness.1))] <- "N/A N/A"
item10.dat3$Wall.Cavity.Insulation.Thickness.2[which(item10.dat3$Wall.Cavity.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item10.dat3$Wall.Cavity.Insulation.Thickness.2[which(item10.dat3$Wall.Cavity.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item10.dat3$Wall.Cavity.Insulation.Thickness.2[which(is.na(item10.dat3$Wall.Cavity.Insulation.Thickness.2))] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
item10.dat3$count <- 1
item10.dat3$TMP_ID <- cumsum(item10.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item10.dat3$Wall.Cavity.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item10.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item10.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item10.dat3$Wall.Cavity.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item10.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item10.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
item10.dat4 <- as.data.frame(left_join(item10.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                             , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item10.dat4$inches1 <- as.numeric(as.character(item10.dat4$V1.x)) # warning here is OK
item10.dat4$inches2 <- as.numeric(as.character(item10.dat4$V1.y)) # warning here is OK

item10.dat4$rvalues1 <- item10.dat4$Wall.Cavity.Insulation.Type.1
item10.dat4$rvalues2 <- item10.dat4$Wall.Cavity.Insulation.Type.2

#check uniques
unique(item10.dat4$rvalues1)
unique(item10.dat4$rvalues2)

#fix names that are not in R value table
item10.dat4$rvalues1[which(item10.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item10.dat4$rvalues1[which(item10.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item10.dat4$rvalues2[which(item10.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item10.dat4$rvalues2[which(item10.dat4$rvalues2 == "N/A")]                              <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item10.dat4$rvalues1[which(item10.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item10.dat4$rvalues2[which(item10.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item10.dat5 <- item10.dat4

###########################
# Cleaning step (NA to zero)
###########################
#For now: convert NAs in rvalues and inches to zero for summarizing purposes (will revert back to NAs after)
item10.dat5$rvalues1[which(is.na(item10.dat5$rvalues1))] <- 0
item10.dat5$rvalues1[which(item10.dat5$rvalues1 == "None")] <- 0
item10.dat5$rvalues2[which(is.na(item10.dat5$rvalues2))] <- 0

#QC the clean bulb values
unique(item10.dat5$rvalues1)
unique(item10.dat5$rvalues2)

item10.dat5$inches1[which(is.na(item10.dat5$inches1))] <- 0
item10.dat5$inches2[which(is.na(item10.dat5$inches2))] <- 0

#QC the clean bulb values -- There are NAs in each 
unique(item10.dat5$inches1)
unique(item10.dat5$inches2)
###########################
# End Cleaning step
###########################


# r values multiplied by inches
item10.dat5$total.r.val <- (as.numeric(as.character(item10.dat5$rvalues1)) * item10.dat5$inches1) + (as.numeric(as.character(item10.dat5$rvalues2)) * item10.dat5$inches2)
unique(item10.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item10.dat5$ufactor <- 1 / as.numeric(as.character(item10.dat5$total.r.val))

# replace inf with 0
item10.dat5$ufactor[which(item10.dat5$ufactor == "Inf")] <- 0

#make area numeric
item10.dat5$ufactor <- as.numeric(as.character(item10.dat5$ufactor))
item10.dat5$Wall.Area <- as.numeric(as.character(item10.dat5$Wall.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item10.dat5, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * as.numeric(as.character(ufactor))) / sum(Wall.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

wall.unique <- unique(item10.dat5[which(colnames(item10.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item10.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")



############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
item10.dat6$aveRval[which(item10.dat6$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
wall.ins.ind  <- item10.dat5$CK_Cadmus_ID[which(item10.dat5$`Wall.Cavity.Insulated?` == "No")]
wall.r.NA.ind <- item10.dat6$CK_Cadmus_ID[which(is.na(item10.dat6$aveRval))]

#which NA R values are in no insulation?
no.insulation <- wall.r.NA.ind[which(wall.r.NA.ind %in% wall.ins.ind)]

#replace no insulation in home ceiling with zero
item10.dat6$aveRval[which(item10.dat6$CK_Cadmus_ID %in% no.insulation)] <- 0
############################################################################################################
## END Process
############################################################################################################

item10.dat7 <- left_join(item10.dat6, rbsa.dat, by = "CK_Cadmus_ID")


#Bin R values -- SF only
item10.dat7$rvalue.bins <- "Unknown"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval == 0)] <- "R0"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval > 0  & item10.dat7$aveRval < 11)]  <- "R1.R10"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >= 11 & item10.dat7$aveRval < 17)]  <- "R11.R16"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >= 17 & item10.dat7$aveRval < 23)]  <- "R17.R22"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >= 22)] <- "RGT22"
unique(item10.dat7$rvalue.bins)

##cast data
item10.dat7$count <- 1
item10.dat.cast <- dcast(setDT(item10.dat7),
                         formula   = CK_Cadmus_ID + BuildingType +  Wall.Type ~ rvalue.bins, sum,
                         value.var = 'count')

for (i in 3:length(item10.dat.cast)){
  item10.dat.cast[i][which(is.na(item10.dat.cast[i])),] <- 0
}

head(item10.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item10.SF.dat <- subset(item10.dat.cast, item10.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item10.sum <- summarise(group_by(item10.SF.dat, BuildingType, Wall.Type)
                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,r0.percent      = sum(R0) / sampleSizeNoNA ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
                        ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                        ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                        ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                        ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                        ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                        ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                        ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                        ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                        ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)

item10.SF.dat$count <- 1
item10.sum.allLevels <- summarise(group_by(item10.SF.dat, BuildingType, Wall.Type)
                                  ,WallTypeCount = sum(count)
                                  ,TotalCount = sum(item10.SF.dat$count)
                                  ,AllInsulationLevelPercent = WallTypeCount / TotalCount
                                  ,AllInsulationSE = sqrt((AllInsulationLevelPercent * (1 - AllInsulationLevelPercent)) / WallTypeCount)
)

#Check to make sure they add to 1
sum(item10.sum.allLevels$AllInsulationLevelPercent)

#join all insulation levels onto rvalue summary
item10.final <- cbind.data.frame(item10.sum
                                 , "All Insulation Levels Mean" = item10.sum.allLevels$AllInsulationLevelPercent
                                 , "All Insulation Levels SE"   = item10.sum.allLevels$AllInsulationSE)











#############################################################################################
# Item 11: DISTRIBUTION OF WALL FRAMING TYPES BY VINTAGE (SF table 18)
#############################################################################################
## Note: For this table, you must run up to item10.dat3 for the cleaned data
item11.dat <- left_join(item10.dat3, rbsa.dat, by = "CK_Cadmus_ID")

#cast out by wall frame types
item11.dat$count <- 1
item11.dat1 <- dcast(setDT(item11.dat),
                     formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ Wall.Type, sum,
                     value.var = 'count')

item11.dat2 <- summarise(group_by(item11.dat1, BuildingType, HomeYearBuilt_bins4)
                         ,Count_2x4 = sum(`Framed 2x4`)
                         ,SE_2x4      = sd(`Framed 2x4`) / sqrt(length(unique(CK_Cadmus_ID)))
                         ,Count_2x6 = sum(`Framed 2x6`)
                         ,SE_2x6      = sd(`Framed 2x6`) / sqrt(length(unique(CK_Cadmus_ID)))
                         ,Count_ALT = sum(Alternative)
                         ,SE_ALT      = sd(Alternative) / sqrt(length(unique(CK_Cadmus_ID)))
)

item11.dat3 <- summarise(group_by(item11.dat1, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Home Vintages"
                         ,Count_2x4 = sum(`Framed 2x4`)
                         ,SE_2x4      = sd(`Framed 2x4`) / sqrt(length(unique(CK_Cadmus_ID)))
                         ,Count_2x6 = sum(`Framed 2x6`)
                         ,SE_2x6      = sd(`Framed 2x6`) / sqrt(length(unique(CK_Cadmus_ID)))
                         ,Count_ALT = sum(Alternative)
                         ,SE_ALT      = sd(Alternative) / sqrt(length(unique(CK_Cadmus_ID)))
)

item11.dat4 <- rbind.data.frame(item11.dat2, item11.dat3, stringsAsFactors = F)
item11.dat4$TotalCount <- item11.dat4$Count_2x4 + item11.dat4$Count_2x6 + item11.dat4$Count_ALT
item11.dat4$Percent_2x4 <- item11.dat4$Count_2x4 / item11.dat4$TotalCount
item11.dat4$Percent_2x6 <- item11.dat4$Count_2x6 / item11.dat4$TotalCount
item11.dat4$Percent_ALT <- item11.dat4$Count_ALT / item11.dat4$TotalCount

item11.final <- item11.dat4[which(colnames(item11.dat4) %in% c("BuildingType"
                                                               ,"HomeYearBuilt_bins4"
                                                               ,"Percent_2x4"
                                                               ,"Percent_2x6"
                                                               ,"Percent_ALT"
                                                               ,"SE_2x4"
                                                               ,"SE_2x6"
                                                               ,"SE_ALT"
))]













#############################################################################################
# Item 12: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE  (SF table 19)
#############################################################################################
## Note: For this table, you must run up to item10.dat5 for the cleaned data
item12.dat <- item10.dat5

#weight the u factor per home
weightedU <- summarise(group_by(item12.dat, CK_Cadmus_ID)
                       ,aveUval = sum(Wall.Area * as.numeric(as.character(ufactor))) / sum(Wall.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

wall.unique <- unique(item12.dat[which(colnames(item12.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

item12.dat1 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")



############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
item12.dat1$aveRval[which(item12.dat1$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
wall.ins.ind  <- item12.dat$CK_Cadmus_ID[which(item12.dat$`Wall.Cavity.Insulated?` == "No")]
wall.r.NA.ind <- item12.dat1$CK_Cadmus_ID[which(is.na(item12.dat1$aveRval))]

#which NA R values are in no insulation?
no.insulation <- wall.r.NA.ind[which(wall.r.NA.ind %in% wall.ins.ind)]

#replace no insulation in home ceiling with zero
item12.dat1$aveRval[which(item12.dat1$CK_Cadmus_ID %in% no.insulation)] <- 0
############################################################################################################
## END Process
############################################################################################################

item12.dat2 <- left_join(item12.dat1, rbsa.dat, by = "CK_Cadmus_ID")


############################################################################################################
############################################################################################################
############################################################################################################
## For single family
############################################################################################################

#Bin R values -- SF only
item12.dat2$rvalue.bins <- "Unknown"
item12.dat2$rvalue.bins[which(item12.dat2$aveRval == 0)] <- "R0"
item12.dat2$rvalue.bins[which(item12.dat2$aveRval > 0  & item12.dat2$aveRval < 11)]  <- "R1.R10"
item12.dat2$rvalue.bins[which(item12.dat2$aveRval >= 11 & item12.dat2$aveRval < 17)]  <- "R11.R16"
item12.dat2$rvalue.bins[which(item12.dat2$aveRval >= 17 & item12.dat2$aveRval < 23)]  <- "R17.R22"
item12.dat2$rvalue.bins[which(item12.dat2$aveRval >= 22)] <- "RGT22"
unique(item12.dat2$rvalue.bins)


item12.dat2$count <- 1
item12.dat.cast <- dcast(setDT(item12.dat2),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item12.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item12.SF.dat <- subset(item12.dat.cast, item12.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item12.SF.sum <- summarise(group_by(item12.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,r0.percent      = sum(R0) / sampleSizeNoNA 
                        ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                        ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                        ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                        ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                        ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                        ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                        ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                        ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                        ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)

item12.SF.sum1 <- summarise(group_by(item12.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                         ,r0.percent      = sum(R0) / sampleSizeNoNA
                         ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                         ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                         ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                         ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                         ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                         ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                         ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                         ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                         ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)


#join all insulation levels onto rvalue summary
item12.SF.final <- rbind.data.frame(item12.SF.sum, item12.SF.sum1, stringsAsFactors = F)
item12.SF.final$check  <- item12.SF.final$r0.percent + item12.SF.final$r1.r10.percent + item12.SF.final$r11.r16.percent + item12.SF.final$r17.r22.percent + item12.SF.final$rGT22.percent


############################################################################################################
############################################################################################################
############################################################################################################
## For manufactured homes
############################################################################################################
#Bin R values -- SF only
item12.dat2$rvalue.bins <- "Unknown"
item12.dat2$rvalue.bins[which(item12.dat2$aveRval >= 0  & item12.dat2$aveRval < 9)]  <- "R0.R8"
item12.dat2$rvalue.bins[which(item12.dat2$aveRval >= 9 & item12.dat2$aveRval < 15)]  <- "R9.R14"
item12.dat2$rvalue.bins[which(item12.dat2$aveRval >= 15 & item12.dat2$aveRval < 22)]  <- "R15.R21"
item12.dat2$rvalue.bins[which(item12.dat2$aveRval >= 22 & item12.dat2$aveRval < 31)]  <- "R22.R30"
unique(item12.dat2$rvalue.bins)


item12.dat2$count <- 1
item12.dat.cast <- dcast(setDT(item12.dat2),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item12.dat.cast)


## Manufactured Homes
item12.MH.dat <- subset(item12.dat.cast, item12.dat.cast$BuildingType == "Manufactured")

#summarize
item12.MH.sum <- summarise(group_by(item12.MH.dat, BuildingType, HomeYearBuilt_bins4)
                           ,sampleSize  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                           ,r1.r8.percent  = sum(R0.R8)  / sampleSize
                           ,r1.r8.se       = sd(R0.R8) / sqrt(sampleSize)
                           ,r9.r14.percent = sum(R9.R14) / sampleSize
                           ,r9.r14.se      = sd(R9.R14) / sqrt(sampleSize)
                           ,r15.r21.percent = sum(R15.R21) / sampleSize
                           ,r15.r21.se      = sd(R15.R21) / sqrt(sampleSize)
                           ,r22.r30.percent = sum(R22.R30) / sampleSize
                           ,r22.r30.se      = sd(R22.R30) / sqrt(sampleSize)
)

item12.MH.sum1 <- summarise(group_by(item12.MH.dat, BuildingType)
                            ,HomeYearBuilt_bins4 = "All Vintages"
                            ,sampleSize  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                            ,r1.r8.percent  = sum(R0.R8)  / sampleSize
                            ,r1.r8.se       = sd(R0.R8) / sqrt(sampleSize)
                            ,r9.r14.percent = sum(R9.R14) / sampleSize
                            ,r9.r14.se      = sd(R9.R14) / sqrt(sampleSize)
                            ,r15.r21.percent = sum(R15.R21) / sampleSize
                            ,r15.r21.se      = sd(R15.R21) / sqrt(sampleSize)
                            ,r22.r30.percent = sum(R22.R30) / sampleSize
                            ,r22.r30.se      = sd(R22.R30) / sqrt(sampleSize)
)


#join all insulation levels onto rvalue summary
item12.MH.final <- rbind.data.frame(item12.MH.sum, item12.MH.sum1, stringsAsFactors = F)
check  <- item12.MH.final$r1.r8.percent + 
  item12.MH.final$r9.r14.percent + 
  item12.MH.final$r15.r21.percent + 
  item12.MH.final$r22.r30.percent











#############################################################################################
# Item 13: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, IDAHO  (SF table 20)
#############################################################################################
## Note: For this table, you must run up to item12.SF.dat2 for the cleaned data
item13.dat <- item12.dat2[which(item12.dat2$State == "ID")]


item13.dat.cast <- dcast(setDT(item13.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item13.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item13.SF.dat <- subset(item13.dat.cast, item13.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item13.sum <- summarise(group_by(item13.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,r0.percent      = sum(R0) / sampleSizeNoNA 
                        ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                        ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                        ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                        ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                        ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                        ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                        ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                        ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                        ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)

item13.sum1 <- summarise(group_by(item13.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                         ,r0.percent      = sum(R0) / sampleSizeNoNA
                         ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                         ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                         ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                         ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                         ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                         ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                         ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                         ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                         ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)


#join all insulation levels onto rvalue summary
item13.final <- rbind.data.frame(item13.sum, item13.sum1, stringsAsFactors = F)
item13.final$check  <- item13.final$r0.percent + item13.final$r1.r10.percent + item13.final$r11.r16.percent + item13.final$r17.r22.percent + item13.final$rGT22.percent












#############################################################################################
# Item 14: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, MONTANA  (SF table 21)
#############################################################################################
## Note: For this table, you must run up to item12.dat2 for the cleaned data
item14.dat <- item12.dat2[which(item12.dat2$State == "MT")]


item14.dat.cast <- dcast(setDT(item14.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item14.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item14.SF.dat <- subset(item14.dat.cast, item14.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item14.sum <- summarise(group_by(item14.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,r0.percent      = sum(R0) / sampleSizeNoNA 
                        ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                        ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                        ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                        ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                        ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                        ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                        ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                        ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                        ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)

item14.sum1 <- summarise(group_by(item14.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                         ,r0.percent      = sum(R0) / sampleSizeNoNA
                         ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                         ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                         ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                         ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                         ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                         ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                         ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                         ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                         ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)


#join all insulation levels onto rvalue summary
item14.final <- rbind.data.frame(item14.sum, item14.sum1, stringsAsFactors = F)
item14.final$check  <- item14.final$r0.percent + item14.final$r1.r10.percent + item14.final$r11.r16.percent + item14.final$r17.r22.percent + item14.final$rGT22.percent










#############################################################################################
# Item 15: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, OREGON  (SF table 22)
#############################################################################################
## Note: For this table, you must run up to item12.dat2 for the cleaned data
item15.dat <- item12.dat2[which(item12.dat2$State == "OR")]


item15.dat.cast <- dcast(setDT(item15.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item15.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item15.SF.dat <- subset(item15.dat.cast, item15.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item15.sum <- summarise(group_by(item15.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,r0.percent      = sum(R0) / sampleSizeNoNA 
                        ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                        ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                        ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                        ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                        ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                        ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                        ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                        ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                        ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)

item15.sum1 <- summarise(group_by(item15.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                         ,r0.percent      = sum(R0) / sampleSizeNoNA
                         ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                         ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                         ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                         ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                         ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                         ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                         ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                         ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                         ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)


#join all insulation levels onto rvalue summary
item15.final <- rbind.data.frame(item15.sum, item15.sum1, stringsAsFactors = F)
item15.final$check  <- item15.final$r0.percent + item15.final$r1.r10.percent + item15.final$r11.r16.percent + item15.final$r17.r22.percent + item15.final$rGT22.percent











#############################################################################################
# Item 16: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, WASHINGTON  (SF table 23)
#############################################################################################
## Note: For this table, you must run up to item12.dat2 for the cleaned data
item16.dat <- item12.dat2[which(item12.dat2$State == "WA")]


item16.dat.cast <- dcast(setDT(item16.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item16.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item16.SF.dat <- subset(item16.dat.cast, item16.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item16.sum <- summarise(group_by(item16.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,r0.percent      = sum(R0) / sampleSizeNoNA 
                        ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                        ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                        ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                        ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                        ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                        ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                        ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                        ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                        ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)

item16.sum1 <- summarise(group_by(item16.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                         ,r0.percent      = sum(R0) / sampleSizeNoNA
                         ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                         ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                         ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                         ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                         ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                         ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                         ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                         ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                         ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
)


#join all insulation levels onto rvalue summary
item16.final <- rbind.data.frame(item16.sum, item16.sum1, stringsAsFactors = F)
item16.final$check  <- item16.final$r0.percent + item16.final$r1.r10.percent + item16.final$r11.r16.percent + item16.final$r17.r22.percent + item16.final$rGT22.percent












