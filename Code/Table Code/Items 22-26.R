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


#############################################################################################
# Item 22: PERCENTAGE OF HOMES WITH FLOOR AREA OVER CRAWLSPACE BY STATE (SF table 29)
#############################################################################################












#############################################################################################
# Item 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (SF table 30) (also need MH)
#############################################################################################

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(analysisInPath, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-23,-3]

#subset envelope data to necessary columns
item23.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Floor.Type"
                                                               , "Floor.Sub-Type"
                                                               , "Floor.Area"
                                                               , "Floor.Insulated?"
                                                               , "Floor.Insulation.Type.1"
                                                               , "Floor.Insulation.Thickness.1"
                                                               , "Floor.Insulation.Type.2"                                                  
                                                               , "Floor.Insulation.Thickness.2"
                                                               # , "Floor.Insulation.Type.3"                                                  
                                                               # , "Floor.Insulation.Thickness.3"
                                                               ))]
length(unique(item23.dat$CK_Cadmus_ID))#547

#trim white space from cadmus IDs
item23.dat$CK_Cadmus_ID <- trimws(item23.dat$CK_Cadmus_ID)

#subset to only wall information
item23.dat1 <- item23.dat[which(item23.dat$Category == "Floor"),]

#remove unneccesary wall types
# item23.dat2 <- item23.dat1[which(!(item23.dat1$Floor.Type %in% c("Masonry","Masonry (Basement)","Log","Adiabatic"))),]
item23.dat2 <- left_join(rbsa.dat, item23.dat1, by = "CK_Cadmus_ID")

item23.SF.dat2.5 <- item23.dat2[which(item23.dat2$BuildingType == "Single Family"),]

#create "Alternative" category
# item23.SF.dat2$Floor.Type[which(item23.SF.dat2$Floor.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
# length(unique(item23.SF.dat2$CK_Cadmus_ID))#473
# unique(item23.SF.dat2$Floor.Type)

#remove items have the datapoint was not asked for
item23.SF.dat3 <- item23.SF.dat2.5[which(item23.SF.dat2.5$`Floor.Insulated?` %in% c("Yes", "No")),]
length(unique(item23.SF.dat3$CK_Cadmus_ID))#280
unique(item23.SF.dat2$`Floor.Insulated?`)


###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
item23.SF.dat3$Floor.Insulation.Thickness.1[which(item23.SF.dat3$Floor.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item23.SF.dat3$Floor.Insulation.Thickness.1[which(item23.SF.dat3$Floor.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item23.SF.dat3$Floor.Insulation.Thickness.1[which(is.na(item23.SF.dat3$Floor.Insulation.Thickness.1))] <- "N/A N/A"
item23.SF.dat3$Floor.Insulation.Thickness.2[which(item23.SF.dat3$Floor.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item23.SF.dat3$Floor.Insulation.Thickness.2[which(item23.SF.dat3$Floor.Insulation.Thickness.2 == "200024390821")] <- "Unknown Unknown"
item23.SF.dat3$Floor.Insulation.Thickness.2[which(item23.SF.dat3$Floor.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item23.SF.dat3$Floor.Insulation.Thickness.2[which(is.na(item23.SF.dat3$Floor.Insulation.Thickness.2))] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
item23.SF.dat3$count <- 1
item23.SF.dat3$TMP_ID <- cumsum(item23.SF.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item23.SF.dat3$Floor.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item23.SF.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item23.SF.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item23.SF.dat3$Floor.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item23.SF.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item23.SF.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
item23.SF.dat4 <- as.data.frame(left_join(item23.SF.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                             , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item23.SF.dat4$inches1 <- as.numeric(as.character(item23.SF.dat4$V1.x)) # warning here is OK
item23.SF.dat4$inches2 <- as.numeric(as.character(item23.SF.dat4$V1.y)) # warning here is OK

item23.SF.dat4$rvalues1 <- item23.SF.dat4$Floor.Insulation.Type.1
item23.SF.dat4$rvalues2 <- item23.SF.dat4$Floor.Insulation.Type.2

#check uniques
unique(item23.SF.dat4$rvalues1)
unique(item23.SF.dat4$rvalues2)

#fix names that are not in R value table
item23.SF.dat4$rvalues1[which(item23.SF.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item23.SF.dat4$rvalues1[which(item23.SF.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item23.SF.dat4$rvalues1[which(item23.SF.dat4$rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
item23.SF.dat4$rvalues2[which(item23.SF.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item23.SF.dat4$rvalues2[which(item23.SF.dat4$rvalues2 == "N/A")]                              <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item23.SF.dat4$rvalues1[which(item23.SF.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item23.SF.dat4$rvalues2[which(item23.SF.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item23.SF.dat5 <- item23.SF.dat4

###########################
# Cleaning step (NA to zero)
###########################
#For now: convert NAs in rvalues and inches to zero for summarizing purposes (will revert back to NAs after)
item23.SF.dat5$rvalues1[which(is.na(item23.SF.dat5$rvalues1))] <- 0
item23.SF.dat5$rvalues1[which(item23.SF.dat5$rvalues1 == "None")] <- 0
item23.SF.dat5$rvalues2[which(is.na(item23.SF.dat5$rvalues2))] <- 0

#QC the clean bulb values
unique(item23.SF.dat5$rvalues1)
unique(item23.SF.dat5$rvalues2)

item23.SF.dat5$inches1[which(is.na(item23.SF.dat5$inches1))] <- 0
item23.SF.dat5$inches2[which(is.na(item23.SF.dat5$inches2))] <- 0

#QC the clean bulb values -- There are NAs in each 
unique(item23.SF.dat5$inches1)
unique(item23.SF.dat5$inches2)
###########################
# End Cleaning step
###########################


# r values multiplied by inches
item23.SF.dat5$total.r.val <- (as.numeric(as.character(item23.SF.dat5$rvalues1)) * item23.SF.dat5$inches1) + (as.numeric(as.character(item23.SF.dat5$rvalues2)) * item23.SF.dat5$inches2)
unique(item23.SF.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item23.SF.dat5$ufactor <- 1 / as.numeric(as.character(item23.SF.dat5$total.r.val))

# replace inf with 0
item23.SF.dat5$ufactor[which(item23.SF.dat5$ufactor == "Inf")] <- 0

#make area numeric
item23.SF.dat5$ufactor <- as.numeric(as.character(item23.SF.dat5$ufactor))
item23.SF.dat5$Floor.Area <- as.numeric(as.character(item23.SF.dat5$Floor.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item23.SF.dat5, CK_Cadmus_ID, Floor.Type)
                       ,aveUval = sum(Floor.Area * as.numeric(as.character(ufactor))) / sum(Floor.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

Floor.unique <- unique(item23.SF.dat5[which(colnames(item23.SF.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item23.SF.dat6 <- left_join(weightedU, Floor.unique, by = "CK_Cadmus_ID")



############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
item23.SF.dat6$aveRval[which(item23.SF.dat6$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
Floor.ins.ind  <- item23.SF.dat5$CK_Cadmus_ID[which(item23.SF.dat5$`Floor.Insulated?` == "No")]
Floor.r.NA.ind <- item23.SF.dat6$CK_Cadmus_ID[which(is.na(item23.SF.dat6$aveRval))]

#which NA R values are in no insulation?
no.insulation <- Floor.r.NA.ind[which(Floor.r.NA.ind %in% Floor.ins.ind)]

#replace no insulation in home ceiling with zero
item23.SF.dat6$aveRval[which(item23.SF.dat6$CK_Cadmus_ID %in% no.insulation)] <- 0
############################################################################################################
## END Process
############################################################################################################

item23.SF.dat7 <- left_join(item23.SF.dat6, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))


#Bin R values -- SF only
item23.SF.dat7$rvalue.bins <- "Unknown"
item23.SF.dat7$rvalue.bins[which(item23.SF.dat7$aveRval == 0)] <- "R0"
item23.SF.dat7$rvalue.bins[which(item23.SF.dat7$aveRval > 0  & item23.SF.dat7$aveRval < 4)]  <- "R1.R3"
item23.SF.dat7$rvalue.bins[which(item23.SF.dat7$aveRval >= 4 & item23.SF.dat7$aveRval < 11)]  <- "R4.R10"
item23.SF.dat7$rvalue.bins[which(item23.SF.dat7$aveRval >= 11 & item23.SF.dat7$aveRval < 16)]  <- "R11.R15"
item23.SF.dat7$rvalue.bins[which(item23.SF.dat7$aveRval >= 16 & item23.SF.dat7$aveRval < 23)]  <- "R16.R22"
item23.SF.dat7$rvalue.bins[which(item23.SF.dat7$aveRval >= 23 & item23.SF.dat7$aveRval < 28)]  <- "R23.R27"
item23.SF.dat7$rvalue.bins[which(item23.SF.dat7$aveRval >= 28 & item23.SF.dat7$aveRval < 38)]  <- "R28.R37"
item23.SF.dat7$rvalue.bins[which(item23.SF.dat7$aveRval >= 38)] <- "RGT38"
unique(item23.SF.dat7$rvalue.bins)

##cast data
item23.SF.dat7$count <- 1
item23.SF.dat.cast <- dcast(setDT(item23.SF.dat7),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item23.SF.dat.cast)


# ## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
# SF.item23.SF.dat <- subset(item23.SF.dat.cast, item23.SF.dat.cast$BuildingType == "Single Family")

#summarize --SF
item23.SF.sum <- summarise(group_by(item23.SF.dat.cast, BuildingType, HomeYearBuilt_bins4)
                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,r0.percent      = sum(R0) / sampleSizeNoNA ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
                        ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                        ,r1.r3.percent  = 0#sum(R1.R3)  / sampleSizeNoNA
                        ,r1.r3.se       = 0#sd(R1.R3) / sqrt(sampleSizeNoNA)
                        ,r4.r10.percent  = sum(R4.R10)  / sampleSizeNoNA
                        ,r4.r10.se       = sd(R4.R10) / sqrt(sampleSizeNoNA)
                        ,r11.r15.percent = sum(R11.R15) / sampleSizeNoNA
                        ,r11.r15.se      = sd(R11.R15) / sqrt(sampleSizeNoNA)
                        ,r16.r22.percent = sum(R16.R22) / sampleSizeNoNA
                        ,r16.r22.se      = sd(R16.R22) / sqrt(sampleSizeNoNA)
                        ,r23.r27.percent = sum(R23.R27) / sampleSizeNoNA
                        ,r23.r27.se      = sd(R23.R27) / sqrt(sampleSizeNoNA)
                        ,r28.r37.percent = sum(R28.R37) / sampleSizeNoNA
                        ,r28.r37.se      = sd(R28.R37) / sqrt(sampleSizeNoNA)
                        ,rGT38.percent   = sum(RGT38) / sampleSizeNoNA
                        ,rGT38.se        = sd(RGT38) / sqrt(sampleSizeNoNA)
)

item23.SF.dat.cast$count <- 1
item23.SF.sum.allVintages <- summarise(group_by(item23.SF.dat.cast, BuildingType)
                                    ,HomeYearBuilt_bins4 = "All Vintages"
                                    ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                                    ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                                    ,r0.percent      = sum(R0) / sampleSizeNoNA ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
                                    ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                                    ,r1.r3.percent  = 0#sum(R1.R3)  / sampleSizeNoNA
                                    ,r1.r3.se       = 0#sd(R1.R3) / sqrt(sampleSizeNoNA)
                                    ,r4.r10.percent  = sum(R4.R10)  / sampleSizeNoNA
                                    ,r4.r10.se       = sd(R4.R10) / sqrt(sampleSizeNoNA)
                                    ,r11.r15.percent = sum(R11.R15) / sampleSizeNoNA
                                    ,r11.r15.se      = sd(R11.R15) / sqrt(sampleSizeNoNA)
                                    ,r16.r22.percent = sum(R16.R22) / sampleSizeNoNA
                                    ,r16.r22.se      = sd(R16.R22) / sqrt(sampleSizeNoNA)
                                    ,r23.r27.percent = sum(R23.R27) / sampleSizeNoNA
                                    ,r23.r27.se      = sd(R23.R27) / sqrt(sampleSizeNoNA)
                                    ,r28.r37.percent = sum(R28.R37) / sampleSizeNoNA
                                    ,r28.r37.se      = sd(R28.R37) / sqrt(sampleSizeNoNA)
                                    ,rGT38.percent   = sum(RGT38) / sampleSizeNoNA
                                    ,rGT38.se        = sd(RGT38) / sqrt(sampleSizeNoNA)
)

#join all insulation levels onto rvalue summary
item23.SF.final <- rbind.data.frame(item23.SF.sum, item23.SF.sum.allVintages
                                 , stringsAsFactors = F)




#############################################################################################
#############################################################################################
# Item 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (MH table 18)
#############################################################################################
#############################################################################################

item23.MH.dat2.5 <- item23.dat2[which(item23.dat2$BuildingType == "Manufactured"),]

#create "Alternative" category
# item23.MH.dat2$Floor.Type[which(item23.MH.dat2$Floor.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
# length(unique(item23.MH.dat2$CK_Cadmus_ID))#473
# unique(item23.MH.dat2$Floor.Type)

#remove items have the datapoint was not asked for
item23.MH.dat3 <- item23.MH.dat2.5[which(item23.MH.dat2.5$`Floor.Insulated?` %in% c("Yes", "No")),]
length(unique(item23.MH.dat3$CK_Cadmus_ID))#280
unique(item23.MH.dat3$`Floor.Insulated?`)


###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
item23.MH.dat3$Floor.Insulation.Thickness.1[which(item23.MH.dat3$Floor.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item23.MH.dat3$Floor.Insulation.Thickness.1[which(item23.MH.dat3$Floor.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item23.MH.dat3$Floor.Insulation.Thickness.1[which(is.na(item23.MH.dat3$Floor.Insulation.Thickness.1))] <- "N/A N/A"
item23.MH.dat3$Floor.Insulation.Thickness.2[which(item23.MH.dat3$Floor.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item23.MH.dat3$Floor.Insulation.Thickness.2[which(item23.MH.dat3$Floor.Insulation.Thickness.2 == "200024390821")] <- "Unknown Unknown"
item23.MH.dat3$Floor.Insulation.Thickness.2[which(item23.MH.dat3$Floor.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item23.MH.dat3$Floor.Insulation.Thickness.2[which(is.na(item23.MH.dat3$Floor.Insulation.Thickness.2))] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
item23.MH.dat3$count <- 1
item23.MH.dat3$TMP_ID <- cumsum(item23.MH.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item23.MH.dat3$Floor.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item23.MH.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item23.MH.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item23.MH.dat3$Floor.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item23.MH.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item23.MH.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
item23.MH.dat4 <- as.data.frame(left_join(item23.MH.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                                , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item23.MH.dat4$inches1 <- as.numeric(as.character(item23.MH.dat4$V1.x)) # warning here is OK
item23.MH.dat4$inches2 <- as.numeric(as.character(item23.MH.dat4$V1.y)) # warning here is OK

item23.MH.dat4$rvalues1 <- item23.MH.dat4$Floor.Insulation.Type.1
item23.MH.dat4$rvalues2 <- item23.MH.dat4$Floor.Insulation.Type.2

#check uniques
unique(item23.MH.dat4$rvalues1)
unique(item23.MH.dat4$rvalues2)

#fix names that are not in R value table
item23.MH.dat4$rvalues1[which(item23.MH.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item23.MH.dat4$rvalues1[which(item23.MH.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item23.MH.dat4$rvalues1[which(item23.MH.dat4$rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
item23.MH.dat4$rvalues2[which(item23.MH.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item23.MH.dat4$rvalues2[which(item23.MH.dat4$rvalues2 == "N/A")]                              <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item23.MH.dat4$rvalues1[which(item23.MH.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item23.MH.dat4$rvalues2[which(item23.MH.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item23.MH.dat5 <- item23.MH.dat4

###########################
# Cleaning step (NA to zero)
###########################
#For now: convert NAs in rvalues and inches to zero for summarizing purposes (will revert back to NAs after)
item23.MH.dat5$rvalues1[which(is.na(item23.MH.dat5$rvalues1))] <- 0
item23.MH.dat5$rvalues1[which(item23.MH.dat5$rvalues1 == "None")] <- 0
item23.MH.dat5$rvalues2[which(is.na(item23.MH.dat5$rvalues2))] <- 0

#QC the clean bulb values
unique(item23.MH.dat5$rvalues1)
unique(item23.MH.dat5$rvalues2)

item23.MH.dat5$inches1[which(is.na(item23.MH.dat5$inches1))] <- 0
item23.MH.dat5$inches2[which(is.na(item23.MH.dat5$inches2))] <- 0

#QC the clean bulb values -- There are NAs in each 
unique(item23.MH.dat5$inches1)
unique(item23.MH.dat5$inches2)
###########################
# End Cleaning step
###########################


# r values multiplied by inches
item23.MH.dat5$total.r.val <- (as.numeric(as.character(item23.MH.dat5$rvalues1)) * item23.MH.dat5$inches1) + (as.numeric(as.character(item23.MH.dat5$rvalues2)) * item23.MH.dat5$inches2)
unique(item23.MH.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item23.MH.dat5$ufactor <- 1 / as.numeric(as.character(item23.MH.dat5$total.r.val))

# replace inf with 0
item23.MH.dat5$ufactor[which(item23.MH.dat5$ufactor == "Inf")] <- 0

#make area numeric
item23.MH.dat5$ufactor <- as.numeric(as.character(item23.MH.dat5$ufactor))
item23.MH.dat5$Floor.Area <- as.numeric(as.character(item23.MH.dat5$Floor.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item23.MH.dat5, CK_Cadmus_ID, Floor.Type)
                       ,aveUval = sum(Floor.Area * as.numeric(as.character(ufactor))) / sum(Floor.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

Floor.unique <- unique(item23.MH.dat5[which(colnames(item23.MH.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item23.MH.dat6 <- left_join(weightedU, Floor.unique, by = "CK_Cadmus_ID")



############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
item23.MH.dat6$aveRval[which(item23.MH.dat6$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
Floor.ins.ind  <- item23.MH.dat5$CK_Cadmus_ID[which(item23.MH.dat5$`Floor.Insulated?` == "No")]
Floor.r.NA.ind <- item23.MH.dat6$CK_Cadmus_ID[which(is.na(item23.MH.dat6$aveRval))]

#which NA R values are in no insulation?
no.insulation <- Floor.r.NA.ind[which(Floor.r.NA.ind %in% Floor.ins.ind)]

#replace no insulation in home ceiling with zero
item23.MH.dat6$aveRval[which(item23.MH.dat6$CK_Cadmus_ID %in% no.insulation)] <- 0
############################################################################################################
## END Process
############################################################################################################

item23.MH.dat7 <- left_join(item23.MH.dat6, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))


#Bin R values -- MH only
item23.MH.dat7$rvalue.bins <- "Unknown"
item23.MH.dat7$rvalue.bins[which(item23.MH.dat7$aveRval > 0  & item23.MH.dat7$aveRval < 9)]  <- "R0.R8"
item23.MH.dat7$rvalue.bins[which(item23.MH.dat7$aveRval >= 9 & item23.MH.dat7$aveRval < 15)]  <- "R9.R14"
item23.MH.dat7$rvalue.bins[which(item23.MH.dat7$aveRval >= 15 & item23.MH.dat7$aveRval < 22)]  <- "R15.R21"
item23.MH.dat7$rvalue.bins[which(item23.MH.dat7$aveRval >= 22 & item23.MH.dat7$aveRval < 31)]  <- "R22.R30"
item23.MH.dat7$rvalue.bins[which(item23.MH.dat7$aveRval >= 31 & item23.MH.dat7$aveRval < 41)]  <- "R31.R40"
unique(item23.MH.dat7$rvalue.bins)

##cast data
item23.MH.dat7$count <- 1
item23.MH.dat.cast <- dcast(setDT(item23.MH.dat7),
                            formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                            value.var = 'count')
head(item23.MH.dat.cast)


# ## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
# MH.item23.MH.dat <- subset(item23.MH.dat.cast, item23.MH.dat.cast$BuildingType == "Single Family")

#summarize --SF
item23.MH.sum <- summarise(group_by(item23.MH.dat.cast, BuildingType, HomeYearBuilt_bins4)
                           ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                           ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                           ,r0.r8.percent  = 0#sum(R0.R8)  / sampleSizeNoNA
                           ,r0.r8.se       = 0#sd(R0.R8) / sqrt(sampleSizeNoNA)
                           ,r9.r14.percent  = sum(R9.R14)  / sampleSizeNoNA
                           ,r9.r14.se       = sd(R9.R14) / sqrt(sampleSizeNoNA)
                           ,r15.r21.percent = sum(R15.R21) / sampleSizeNoNA
                           ,r15.r21.se      = sd(R15.R21) / sqrt(sampleSizeNoNA)
                           ,r22.r30.percent = sum(R22.R30) / sampleSizeNoNA
                           ,r22.r30.se      = sd(R22.R30) / sqrt(sampleSizeNoNA)
                           ,r31.r40.percent = sum(R31.R40) / sampleSizeNoNA
                           ,r31.r40.se      = sd(R31.R40) / sqrt(sampleSizeNoNA)
)

item23.MH.dat.cast$count <- 1
item23.MH.sum.allVintages <- summarise(group_by(item23.MH.dat.cast, BuildingType)
                                       ,HomeYearBuilt_bins4 = "All Vintages"
                                       ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                                       ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                                       ,r0.r8.percent  = 0#sum(R0.R8)  / sampleSizeNoNA
                                       ,r0.r8.se       = 0#sd(R0.R8) / sqrt(sampleSizeNoNA)
                                       ,r9.r14.percent  = sum(R9.R14)  / sampleSizeNoNA
                                       ,r9.r14.se       = sd(R9.R14) / sqrt(sampleSizeNoNA)
                                       ,r15.r21.percent = sum(R15.R21) / sampleSizeNoNA
                                       ,r15.r21.se      = sd(R15.R21) / sqrt(sampleSizeNoNA)
                                       ,r22.r30.percent = sum(R22.R30) / sampleSizeNoNA
                                       ,r22.r30.se      = sd(R22.R30) / sqrt(sampleSizeNoNA)
                                       ,r31.r40.percent = sum(R31.R40) / sampleSizeNoNA
                                       ,r31.r40.se      = sd(R31.R40) / sqrt(sampleSizeNoNA)
)

#join all insulation levels onto rvalue summary
item23.MH.final <- rbind.data.frame(item23.MH.sum, item23.MH.sum.allVintages
                                    , stringsAsFactors = F)

















#############################################################################################
# Item 24: PERCENTAGE OF CRAWLSPACES WITH INSULATED WALLS BY STATE (SF table 31)
#############################################################################################
item24.dat <- envelope.dat[grep("CK_Cadmus_ID|Crawlspace", colnames(envelope.dat))]

item24.dat1 <- left_join(rbsa.dat, item24.dat, by = "CK_Cadmus_ID")
length(unique(item24.dat1$CK_Cadmus_ID)) #565

item24.dat2 <- item24.dat1[which(item24.dat1$`Crawlspace.Walls.Insulated?` %in% c("Yes", "No")),]
length(unique(item24.dat2$CK_Cadmus_ID)) #183

item24.dat2$count <- 1
item24.dat2$crawl.ins.ind <- 0
item24.dat2$crawl.ins.ind[which(item24.dat2$`Crawlspace.Walls.Insulated?` == "Yes")] <- 1

item24.final <- summarise(group_by(item24.dat2, BuildingType, State)
                          ,InsulatedCount = sum(crawl.ins.ind)
                          ,SampleSize     = sum(count)
                          ,Percent        = InsulatedCount / SampleSize
                          ,SE             = sqrt(Percent * (1 - Percent) / SampleSize)
                          )












#############################################################################################
# Item 25: PERCENTAGE OF HOMES WITH ATTICS BY STATE (SF table 32)
#############################################################################################
# ENV_CEILING_ATTIC_CeilingInsulated_Y_N
item25.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_CEILING_ATTIC_CeilingInsulated_Y_N"))]

item25.dat0 <- left_join(rbsa.dat, item25.dat, by = "CK_Cadmus_ID")

item25.dat1 <- item25.dat0[which(item25.dat0$BuildingType == "Single Family"),]

item25.dat2 <- item25.dat1[which(!(is.na(item25.dat1$ENV_CEILING_ATTIC_CeilingInsulated_Y_N))),]
item25.dat2$count <- 1
item25.dat2$insul.ind <- 0
item25.dat2$insul.ind[which(item25.dat2$ENV_CEILING_ATTIC_CeilingInsulated_Y_N == "Yes")] <- 1

item25.cnt <- summarise(group_by(item25.dat2, BuildingType, State)
                       , InsulatedCount = length(unique(CK_Cadmus_ID))
                         )

item25.SS <- summarise(group_by(item25.dat1, BuildingType, State)
                       , SampleSize = length(unique(CK_Cadmus_ID))
)

item25.final <- left_join(item25.cnt, item25.SS, by = c("BuildingType", "State"))
item25.final$Percent <- item25.final$InsulatedCount / item25.final$SampleSize
item25.final$SE      <- sqrt(item25.final$Percent * (1 - item25.final$Percent) / item25.final$SampleSize)


#############################################################################################
# Item 26: DISTRIBUTION OF ATTIC INSULATION LEVELS (SF table 33)
#############################################################################################
# "ENV_CEILING_ATTIC_CeilingCavityDepth"                                           
# "ENV_CEILING_ATTIC_CeilingInsulated_Y_N"                                         
# "ENV_CEILING_ATTIC_FramingSize"                                                  
# "ENV_CEILING_ATTIC_FramingSpacing"                                               
# "ENV_CEILING_ATTIC_InsulationCondition1"                                         
# "ENV_CEILING_ATTIC_InsulationCondition2"                                         
# "ENV_CEILING_ATTIC_InsulationCondition3"                                         
# "ENV_CEILING_ATTIC_InsulationThickness1"                                         
# "ENV_CEILING_ATTIC_InsulationThickness2"                                         
# "ENV_CEILING_ATTIC_InsulationThickness3"                                         
# "ENV_CEILING_ATTIC_InsulationType1"                                              
# "ENV_CEILING_ATTIC_InsulationType2"                                              
# "ENV_CEILING_ATTIC_InsulationType3"                                              
# "ENV_CEILING_ATTIC_Notes"               





