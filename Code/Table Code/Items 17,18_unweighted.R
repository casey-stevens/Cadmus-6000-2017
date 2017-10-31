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

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]





















#############################################################################################
# Item 17: DISTRIBUTION OF Masonry Wall Insulation Levels by Vintage (SF table 24)
#############################################################################################
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
item17.dat0 <- item17.dat[which(item17.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item17.dat1 <- item17.dat0[which(item17.dat0$`Furred.Wall.Insulated?` %in% c("Yes", "No")),]
item17.dat2 <- item17.dat1[which(item17.dat1$Furred.Wall.Insulation.Thickness != "Unknown"),]

unique(item17.dat2$Furred.Wall.Insulation.Type)

item17.dat3 <- item17.dat2

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
unique(item17.dat3$Furred.Wall.Insulation.Thickness)
item17.dat3$Furred.Wall.Insulation.Thickness[which(item17.dat3$Furred.Wall.Insulation.Thickness == "1.5")] <- "1.5 inches"


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


item17.dat4$rvalues1[which(item17.dat4$`Furred.Wall.Insulated?` == "No")] <- 0
item17.dat4$inches1[ which(item17.dat4$`Furred.Wall.Insulated?` == "No")] <- 0


item17.dat5 <- item17.dat4
item17.dat5$rvalues1 <- as.numeric(as.character(item17.dat5$rvalues1))


# r values multiplied by inches
item17.dat5$total.r.val <- item17.dat5$rvalues1 * item17.dat5$inches1
unique(item17.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item17.dat5$ufactor <- 1 / item17.dat5$total.r.val

# replace inf with 0
item17.dat5$ufactor[which(item17.dat5$ufactor == "Inf")] <- 0

#make area numeric
item17.dat5$ufactor <- as.numeric(as.character(item17.dat5$ufactor))
item17.dat5$Wall.Area <- as.numeric(as.character(item17.dat5$Wall.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item17.dat5, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * ufactor) / sum(Wall.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

wall.unique <- unique(item17.dat5[which(colnames(item17.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item17.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item17.dat7 <- left_join(item17.dat6, rbsa.dat, by = "CK_Cadmus_ID")
unique(item17.dat7$BuildingType)

item17.dat8 <- item17.dat7[which(!(is.na(item17.dat7$HomeYearBuilt_bins))),]

#Bin R values -- SF only
item17.dat8$rvalue.bins <- "Unknown"
item17.dat8$rvalue.bins[which(item17.dat8$aveRval == 0)] <- "R0"
item17.dat8$rvalue.bins[which(item17.dat8$aveRval > 0  & item17.dat8$aveRval < 10)]  <- "R1.R9"
item17.dat8$rvalue.bins[which(item17.dat8$aveRval >= 10 & item17.dat8$aveRval < 15)]  <- "R10.R14"
item17.dat8$rvalue.bins[which(item17.dat8$aveRval >= 15 & item17.dat8$aveRval < 21)]  <- "R15.R20"
item17.dat8$rvalue.bins[which(item17.dat8$aveRval >= 21)] <- "RGT21"
unique(item17.dat8$rvalue.bins)

##cast data
item17.dat8$count <- 1
item17.dat.cast <- dcast(setDT(item17.dat8),
                         formula   = CK_Cadmus_ID + BuildingType + HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')

head(item17.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item17.SF.dat <- subset(item17.dat.cast, item17.dat.cast$BuildingType == "Single Family")

#summarize by housing vintage
item17.sum1 <- summarise(group_by(item17.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,r0.percent      = sum(R0) / SampleSize 
                        ,r0.se           = sd(R0) / sqrt(SampleSize)
                        ,r1.r9.percent   = sum(R1.R9)  / SampleSize
                        ,r1.r9.se        = sd(R1.R9) / sqrt(SampleSize)
                        ,r10.r14.percent = sum(R10.R14) / SampleSize
                        ,r10.r14.se      = sd(R10.R14) / sqrt(SampleSize)
                        ,r15.r20.percent = sum(R15.R20) / SampleSize
                        ,r15.r20.se      = sd(R15.R20) / sqrt(SampleSize)
                        # ,rGT21.percent   = sum(RGT21) / SampleSize
                        # ,rGT21.se        = sd(RGT21) / sqrt(SampleSize)
)
#summarize across housing vintage
item17.sum2 <- summarise(group_by(item17.SF.dat, BuildingType)
                        ,HomeYearBuilt_bins4 = "All Vintages"
                        ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,r0.percent      = sum(R0) / SampleSize
                        ,r0.se           = sd(R0) / sqrt(SampleSize)
                        ,r1.r9.percent  = sum(R1.R9)  / SampleSize
                        ,r1.r9.se       = sd(R1.R9) / sqrt(SampleSize)
                        ,r10.r14.percent = sum(R10.R14) / SampleSize
                        ,r10.r14.se      = sd(R10.R14) / sqrt(SampleSize)
                        ,r15.r20.percent = sum(R15.R20) / SampleSize
                        ,r15.r20.se      = sd(R15.R20) / sqrt(SampleSize)
                        # ,rGT21.percent   = sum(RGT21) / SampleSize
                        # ,rGT21.se        = sd(RGT21) / sqrt(SampleSize)
)
item17.merge1 <- rbind.data.frame(item17.sum1, item17.sum2, stringsAsFactors = F)




item17.SF.dat$count <- 1
# by housing vintage
item17.sum.allLevels1 <- summarise(group_by(item17.SF.dat, BuildingType, HomeYearBuilt_bins4)
                                  ,WallTypeCount = sum(count)
                                  ,TotalCount = sum(item17.SF.dat$count)
                                  ,AllInsulationLevelPercent = WallTypeCount / TotalCount
                                  ,AllInsulationSE = sqrt((AllInsulationLevelPercent * (1 - AllInsulationLevelPercent)) / WallTypeCount)
)
# across housing vintage
item17.sum.allLevels2 <- summarise(group_by(item17.SF.dat, BuildingType)
                                   ,HomeYearBuilt_bins4 = "All Vintages"
                                   ,WallTypeCount = sum(count)
                                   ,TotalCount = sum(item17.SF.dat$count)
                                   ,AllInsulationLevelPercent = WallTypeCount / TotalCount
                                   ,AllInsulationSE = sqrt((AllInsulationLevelPercent * (1 - AllInsulationLevelPercent)) / WallTypeCount)
)


item17.sum.allLevels <- rbind.data.frame(item17.sum.allLevels1, item17.sum.allLevels2, stringsAsFactors = F)

#join all insulation levels onto rvalue summary
item17.final <- data.frame("BuildingType" = item17.merge1$BuildingType
                           ,"Housing.Vintage" = item17.merge1$HomeYearBuilt_bins4
                           ,"Percent.R0" = item17.merge1$r0.percent
                           ,"SE.R0" = item17.merge1$r0.se
                           ,"Percent.R1.R9" = item17.merge1$r1.r9.percent
                           ,"SE.R1.R9" = item17.merge1$r1.r9.se
                           ,"Percent.R10.R14" = item17.merge1$r10.r14.percent
                           ,"SE.R10.R14" = item17.merge1$r10.r14.se
                           ,"Percent.R15.R20" = item17.merge1$r15.r20.percent
                           ,"SE.R15.R20" = item17.merge1$r15.r20.se
                           # ,"Percent.RGT21" = item17.merge1$rGT21.percent
                           # ,"SE.RGT21" = item17.merge1$rGT21.se
                           ,"Percent_All Insulation Levels" = item17.sum.allLevels$AllInsulationLevelPercent
                           ,"SE_All Insulation Levels"   = item17.sum.allLevels$AllInsulationSE
                           ,"SampleSize" = item17.merge1$SampleSize)












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

item18.dat1 <- item18.dat0[which(item18.dat0$`Wall.Exterior.Insulated?` %in% c("Yes", "No")),]
item18.dat2 <- item18.dat1[which(item18.dat1$Wall.Exterior.Insulation.Thickness.1 != "Unknown"),]

unique(item18.dat2$Wall.Exterior.Insulation.Type.1)

#create "Alternative" category
item18.dat2$Wall.Type[which(item18.dat2$Wall.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
length(unique(item18.dat2$CK_Cadmus_ID))#473
unique(item18.dat2$Wall.Type)

item18.dat3 <- item18.dat2[which(!(item18.dat2$Wall.Type %in% c("Masonry (Basement)","Adiabatic"))),]


#remove items that have the datapoint was not asked for from wall framing size
item18.dat3.1 <- item18.dat3[which(item18.dat3$Wall.Framing.Size != "-- Datapoint not asked for --"),]
item18.dat4 <- item18.dat3.1[which(item18.dat3.1$Wall.Exterior.Insulation.Thickness.1 != "-- Datapoint not asked for --"),]

item18.dat4$Wall.Exterior.Insulation.Thickness.1[which(item18.dat4$`Wall.Exterior.Insulated?` == "No")] <- "None"

item18.dat5 <- item18.dat4[which(item18.dat4$BuildingType == "Single Family"),]

###########################
# Summarise
###########################
item18.dat5$count <- 1


#summarise by insulation thickness
#by wall framing sizes
item18.frameType1 <- summarise(group_by(item18.dat5, BuildingType, Wall.Framing.Size, Wall.Exterior.Insulation.Thickness.1)
                              , Count = sum(count))
#across wall framing sizes
item18.allFrameTypes <- summarise(group_by(item18.dat5, BuildingType, Wall.Exterior.Insulation.Thickness.1)
                                  , Wall.Framing.Size = "All Framing Types"
                                  , Count = sum(count))
#merge together
item18.tmp1 <- rbind.data.frame(item18.frameType1, item18.allFrameTypes, stringsAsFactors = F)

#summarise across wall insulation thickness
#by wall framing sizes
item18.frameType2 <- summarise(group_by(item18.dat5, BuildingType, Wall.Framing.Size)
                                   ,SampleSize = sum(count))
#across wall framing sizes
item18.sampleSize <- summarise(group_by(item18.dat5, BuildingType)
                               ,Wall.Framing.Size = "All Framing Types"
                               ,SampleSize = sum(count))
#merge together
item18.tmp2 <- rbind.data.frame(item18.frameType2, item18.sampleSize, stringsAsFactors = F)

#left join
item18.final <- left_join(item18.tmp1, item18.tmp2, by = c("Wall.Framing.Size", "BuildingType"))
#calculated percents and standard errors
item18.final$Percent <- item18.final$Count / item18.final$SampleSize
item18.final$SE      <- sqrt(item18.final$Percent * (1 - item18.final$Percent) / item18.final$SampleSize)

detach(package:reshape2)
library(data.table)
item18.cast <- dcast(setDT(item18.final)
                      , formula = BuildingType + Wall.Framing.Size + SampleSize ~ Wall.Exterior.Insulation.Thickness.1
                      , value.var = c("Percent", "SE"))

item18.table <- data.frame("BuildingType" = item18.cast$BuildingType
                           ,"Wall.Frame.Size" = item18.cast$Wall.Framing.Size
                           ,"Percent_0.5_inch" = item18.cast$`Percent_0.5 inch`
                           ,"SE_0.5_inch" = item18.cast$`SE_0.5 inch`
                           ,"Percent_0.75_inch" = item18.cast$`Percent_0.75 inch`
                           ,"SE_0.75_inch" = item18.cast$`SE_0.75 inch`
                           ,"Percent_1_inch" = item18.cast$`Percent_1 inch`
                           ,"SE_1_inch" = item18.cast$`SE_1 inch`
                           ,"Percent_2_inches" = item18.cast$`Percent_2 inches`
                           ,"SE_2_inces" = item18.cast$`SE_2 inches`
                           ,"Percent_None" = item18.cast$Percent_None
                           ,"SE_None" = item18.cast$SE_None
                           ,"SampleSize" = item18.cast$SampleSize)
