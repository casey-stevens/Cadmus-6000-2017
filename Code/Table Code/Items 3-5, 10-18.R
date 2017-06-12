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
analysisInPath <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Analysis Documents"
stopifnot(all(file.exists(SPPath, cleanInPath, analysisInPath)))

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

#summarise by state
item4.state.dat <- summarise(group_by(item4.dat2, CK_Cadmus_ID, State)
                      ,siteAreaConditioned = sum(ConditionedArea)
)

item4.state.dat1 <- summarise(group_by(item4.state.dat, State)
                        ,AveAreaConditioned = mean(siteAreaConditioned)
                        ,SEAreaConditioned  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                        ,SampleSize         = length(unique(CK_Cadmus_ID))
                        )

#summarise by region
item4.region.dat <- summarise(group_by(item4.dat2, CK_Cadmus_ID)
                              ,State = "Region"
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item4.region.dat1 <- summarise(item4.region.dat
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
item5.state.dat00 <- summarise(group_by(item5.dat2, CK_Cadmus_ID, State)
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item5.state.dat01 <- summarise(group_by(item5.state.dat00, State)
                               ,HomeYearBuilt_bins = "All Vintages"
                              ,AveAreaConditioned = mean(siteAreaConditioned)
                              ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize = length(unique(CK_Cadmus_ID))
)

#summarise by region
item5.region.dat00 <- summarise(group_by(item5.dat2, CK_Cadmus_ID)
                              ,State = "Region"
                              ,siteAreaConditioned = sum(ConditionedArea)
)

item5.region.dat01 <- summarise(group_by(item5.region.dat00)
                               ,State = "Region"
                               ,HomeYearBuilt_bins = "All Vintages"
                               ,AveAreaConditioned = mean(siteAreaConditioned)
                               ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)

item5.tmp1 <- rbind.data.frame(item5.state.dat01, item5.region.dat01, stringsAsFactors = F) 



#### By state/region and homeyearbuilt bins
#summarise by state
item5.state.dat <- summarise(group_by(item5.dat2, CK_Cadmus_ID, State, HomeYearBuilt_bins)
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item5.state.dat1 <- summarise(group_by(item5.state.dat, State, HomeYearBuilt_bins)
                              ,AveAreaConditioned = mean(siteAreaConditioned)
                              ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize = length(unique(CK_Cadmus_ID))
                              )

#summarise by region
item5.region.dat <- summarise(group_by(item5.dat2, CK_Cadmus_ID, HomeYearBuilt_bins)
                              ,State = "Region"
                              ,siteAreaConditioned = sum(ConditionedArea)
)

item5.region.dat1 <- summarise(group_by(item5.region.dat, HomeYearBuilt_bins)
                               ,State = "Region"
                               ,AveAreaConditioned = mean(siteAreaConditioned)
                               ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)

item5.tmp2 <- rbind.data.frame(item5.state.dat1, item5.region.dat1, stringsAsFactors = F) 


item5.final <- rbind.data.frame(item5.tmp1, item5.tmp2, stringsAsFactors = F)











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
SF.item10.dat <- subset(item10.dat.cast, item10.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item10.sum <- summarise(group_by(SF.item10.dat, BuildingType, Wall.Type)
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

SF.item10.dat$count <- 1
item10.sum.allLevels <- summarise(group_by(SF.item10.dat, BuildingType, Wall.Type)
                                  ,WallTypeCount = sum(count)
                                  ,TotalCount = sum(SF.item10.dat$count)
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
SF.item12.dat <- subset(item12.dat.cast, item12.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item12.sum <- summarise(group_by(SF.item12.dat, BuildingType, HomeYearBuilt_bins4)
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

item12.sum1 <- summarise(group_by(SF.item12.dat, BuildingType)
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
item12.final <- rbind.data.frame(item12.sum, item12.sum1, stringsAsFactors = F)
item12.final$check  <- item12.final$r0.percent + item12.final$r1.r10.percent + item12.final$r11.r16.percent + item12.final$r17.r22.percent + item12.final$rGT22.percent









#############################################################################################
# Item 13: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, IDAHO  (SF table 20)
#############################################################################################
## Note: For this table, you must run up to item12.dat2 for the cleaned data
item13.dat <- item12.dat2[which(item12.dat2$State == "ID")]


item13.dat.cast <- dcast(setDT(item13.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item13.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
SF.item13.dat <- subset(item13.dat.cast, item13.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item13.sum <- summarise(group_by(SF.item13.dat, BuildingType, HomeYearBuilt_bins4)
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

item13.sum1 <- summarise(group_by(SF.item13.dat, BuildingType)
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
SF.item14.dat <- subset(item14.dat.cast, item14.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item14.sum <- summarise(group_by(SF.item14.dat, BuildingType, HomeYearBuilt_bins4)
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

item14.sum1 <- summarise(group_by(SF.item14.dat, BuildingType)
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
SF.item15.dat <- subset(item15.dat.cast, item15.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item15.sum <- summarise(group_by(SF.item15.dat, BuildingType, HomeYearBuilt_bins4)
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

item15.sum1 <- summarise(group_by(SF.item15.dat, BuildingType)
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
SF.item16.dat <- subset(item16.dat.cast, item16.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item16.sum <- summarise(group_by(SF.item16.dat, BuildingType, HomeYearBuilt_bins4)
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

item16.sum1 <- summarise(group_by(SF.item16.dat, BuildingType)
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
item17.dat2 <- item17.dat1[which(item17.dat1$Wall.Type %in% c("Masonry","Masonry (Basement)")),]
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
SF.item17.dat <- subset(item17.dat.cast, item17.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item17.sum <- summarise(group_by(SF.item17.dat, BuildingType, HomeYearBuilt_bins4)
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

SF.item17.dat$count <- 1
item17.sum.allLevels <- summarise(group_by(SF.item17.dat, BuildingType, HomeYearBuilt_bins4)
                                  ,WallTypeCount = sum(count)
                                  ,TotalCount = sum(SF.item17.dat$count)
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
item17.sum.allVintages <- summarise(group_by(SF.item17.dat, BuildingType)
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

item17.sum.allLevelsVintages <- summarise(group_by(SF.item17.dat, BuildingType)
                                          ,HomeYearBuilt_bins4 = "All Vintages"
                                  ,WallTypeCount = sum(count)
                                  ,TotalCount = sum(SF.item17.dat$count)
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


# "Wall.Exterior.Insulated?"                                                       
# "Wall.Exterior.Insulation.Type.1"                                                
# "Wall.Exterior.Insulation.Thickness.1"                                           
# "Wall.Exterior.Insulation.Condition.1"                                           
# "Wall.Exterior.Insulation.Type.2"                                                
# "Wall.Exterior.Insulation.Thickness.2"                                           
# "Wall.Exterior.Insulation.Condition.2"                                           
# "Wall.Exterior.Insulation.Type.3"                                                
# "Wall.Exterior.Insulation.Thickness.3"                                           
# "Wall.Exterior.Insulation.Condition.3" 




























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



