#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
# Item 160: DISTRIBUTION OF FRAME WALL INSULATION LEVELS, ELECTRICALLY HEATED HOMES (SF table B-5)
#############################################################################################
#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item160.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item160.dat.11 <- item160.dat.1[which(item160.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item160.dat.12 <- item160.dat.11[which(item160.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item160.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item160.dat.12$Primary.Heating.System)
item160.dat.12$count <- 1

item160.dat.13 <- unique(item160.dat.12[which(item160.dat.12$Heating.Fuel == "Electric"),])

item160.sum1 <- summarise(group_by(item160.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item160.sum1$Count <- 1
which(duplicated(item160.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item160.sum1$Heating.Fuel)

item160.mechanical <- item160.sum1







#############################################################################################
#
#For Envelope information
#
#############################################################################################

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-24,-3]

#subset envelope data to necessary columns
item160.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Wall.Type"
                                                               , "Wall.Area"
                                                               , "Wall.Framing.Size"
                                                               , "Wall.Cavity.Insulated?"
                                                               , "Wall.Cavity.Insulation.Type.1"
                                                               , "Wall.Cavity.Insulation.Thickness.1"
                                                               , "Wall.Cavity.Insulation.Type.2"                                                  
                                                               , "Wall.Cavity.Insulation.Thickness.2"
                                                               , "Wall.Exterior.Insulated?"
                                                               , "Wall.Exterior.Insulation.Type.1"
                                                               , "Wall.Exterior.Insulation.Thickness.1"
                                                               , "Wall.Exterior.Insulation.Type.2"                                                  
                                                               , "Wall.Exterior.Insulation.Thickness.2"))]
length(unique(item160.dat$CK_Cadmus_ID))#584

#subset to only wall information
item160.dat1 <- item160.dat[which(item160.dat$Category == "Wall"),]

#remove unneccesary wall types
item160.dat2 <- item160.dat1[which(!(item160.dat1$Wall.Type %in% c("Masonry","Masonry (Basement)","Log","Adiabatic"))),]

#create "Alternative" category
item160.dat2$Wall.Type[which(item160.dat2$Wall.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
length(unique(item160.dat2$CK_Cadmus_ID))#473
unique(item160.dat2$Wall.Type)

#remove items have the datapoint was not asked for
item160.dat3 <- item160.dat2[which(item160.dat2$Wall.Framing.Size != "-- Datapoint not asked for --"),]
length(unique(item160.dat3$CK_Cadmus_ID))#469
unique(item160.dat3$Wall.Framing.Size)

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
#cleaning for wall.cavity
item160.dat3$Wall.Cavity.Insulation.Thickness.1[which(item160.dat3$Wall.Cavity.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item160.dat3$Wall.Cavity.Insulation.Thickness.1[which(item160.dat3$Wall.Cavity.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item160.dat3$Wall.Cavity.Insulation.Thickness.1[which(item160.dat3$Wall.Cavity.Insulation.Thickness.1 == "-- Datapoint not asked for --")] <- "N/A N/A"
item160.dat3$Wall.Cavity.Insulation.Thickness.1[which(is.na(item160.dat3$Wall.Cavity.Insulation.Thickness.1))] <- "N/A N/A"
item160.dat3$Wall.Cavity.Insulation.Thickness.2[which(item160.dat3$Wall.Cavity.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item160.dat3$Wall.Cavity.Insulation.Thickness.2[which(item160.dat3$Wall.Cavity.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item160.dat3$Wall.Cavity.Insulation.Thickness.2[which(item160.dat3$Wall.Cavity.Insulation.Thickness.2 == "-- Datapoint not asked for --")] <- "N/A N/A"
item160.dat3$Wall.Cavity.Insulation.Thickness.2[which(is.na(item160.dat3$Wall.Cavity.Insulation.Thickness.2))] <- "N/A N/A"
unique(item160.dat3$Wall.Cavity.Insulation.Thickness.1)
unique(item160.dat3$Wall.Cavity.Insulation.Thickness.2)

#cleaning for wall exterior
item160.dat3$Wall.Exterior.Insulation.Thickness.1[which(item160.dat3$Wall.Exterior.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item160.dat3$Wall.Exterior.Insulation.Thickness.1[which(item160.dat3$Wall.Exterior.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item160.dat3$Wall.Exterior.Insulation.Thickness.1[which(item160.dat3$Wall.Exterior.Insulation.Thickness.1 == "-- Datapoint not asked for --")] <- "N/A N/A"
item160.dat3$Wall.Exterior.Insulation.Thickness.1[which(is.na(item160.dat3$Wall.Exterior.Insulation.Thickness.1))] <- "N/A N/A"
item160.dat3$Wall.Exterior.Insulation.Thickness.2[which(item160.dat3$Wall.Exterior.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item160.dat3$Wall.Exterior.Insulation.Thickness.2[which(item160.dat3$Wall.Exterior.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item160.dat3$Wall.Exterior.Insulation.Thickness.2[which(item160.dat3$Wall.Exterior.Insulation.Thickness.2 == "-- Datapoint not asked for --")] <- "N/A N/A"
item160.dat3$Wall.Exterior.Insulation.Thickness.2[which(is.na(item160.dat3$Wall.Exterior.Insulation.Thickness.2))] <- "N/A N/A"
unique(item160.dat3$Wall.Exterior.Insulation.Thickness.1)
unique(item160.dat3$Wall.Exterior.Insulation.Thickness.2)

# add new ID variable for merging -- don't know if we need this
item160.dat3$count <- 1
item160.dat3$TMP_ID <- cumsum(item160.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item160.dat3$Wall.Cavity.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item160.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item160.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item160.dat3$Wall.Cavity.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item160.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item160.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul3 <- unlist(strsplit(item160.dat3$Wall.Exterior.Insulation.Thickness.1, " "))
clean.insul3 <- as.data.frame(matrix(clean.insul3, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul3.1 <- cbind.data.frame("CK_Cadmus_ID" = item160.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item160.dat3$TMP_ID
                                   , clean.insul3)
dim(clean.insul3.1)

clean.insul4 <- unlist(strsplit(item160.dat3$Wall.Cavity.Insulation.Thickness.2, " "))
clean.insul4.1 <- cbind.data.frame("CK_Cadmus_ID" = item160.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item160.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul4, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul4.1)

clean.insul.join1 <- left_join(clean.insul1.1,    clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join2 <- left_join(clean.insul.join1, clean.insul3.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join3 <- left_join(clean.insul.join2, clean.insul4.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################


#make into dataframe
item160.merge1 <- as.data.frame(left_join(item160.dat3, clean.insul.join3, by = c("CK_Cadmus_ID", "TMP_ID"))
                             , stringsAsFactors = F) 
# warning here is OK

item160.merge2 <- left_join(item160.merge1, item160.mechanical, by = c("CK_Cadmus_ID"))

item160.dat4 <- item160.merge2[which(item160.merge2$Heating.Fuel == "Electric"),]


###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item160.dat4$inches1 <- as.numeric(as.character(item160.dat4$V1.x)) # warning here is OK
item160.dat4$inches2 <- as.numeric(as.character(item160.dat4$V1.y)) # warning here is OK
item160.dat4$inches3 <- as.numeric(as.character(item160.dat4$V1.x.x)) # warning here is OK
# item160.dat4$inches4 <- as.numeric(as.character(item160.dat4$V1.y.y)) # warning here is OK

item160.dat4$rvalues1 <- item160.dat4$Wall.Cavity.Insulation.Type.1
item160.dat4$rvalues2 <- item160.dat4$Wall.Cavity.Insulation.Type.2
item160.dat4$rvalues3 <- item160.dat4$Wall.Exterior.Insulation.Type.1
# item160.dat4$rvalues4 <- item160.dat4$Wall.Exterior.Insulation.Type.2

#check uniques
unique(item160.dat4$rvalues1)
unique(item160.dat4$rvalues2)
unique(item160.dat4$rvalues3)
# unique(item160.dat4$rvalues4)

#fix names that are not in R value table
item160.dat4$rvalues1[which(item160.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item160.dat4$rvalues1[which(item160.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item160.dat4$rvalues1[which(item160.dat4$rvalues1 == "-- Datapoint not asked for --")]    <- NA
item160.dat4$rvalues2[which(item160.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item160.dat4$rvalues2[which(item160.dat4$rvalues2 == "N/A")]                              <- NA
item160.dat4$rvalues2[which(item160.dat4$rvalues2 == "-- Datapoint not asked for --")]    <- NA
item160.dat4$rvalues3[which(item160.dat4$rvalues3 == "-- Datapoint not asked for --")]    <- NA
item160.dat4$rvalues3[which(item160.dat4$rvalues3 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
item160.dat4$rvalues3[which(item160.dat4$rvalues3 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"
item160.dat4$rvalues3[which(item160.dat4$rvalues3 == "Foil-faced polyisocyanurate foam board")] <- "Foil-faced polyiscyanurate foam board"
# item160.dat4$rvalues4[which(item160.dat4$rvalues4 == "-- Datapoint not asked for --")]  <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item160.dat4$rvalues1[which(item160.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item160.dat4$rvalues2[which(item160.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item160.dat4$rvalues3[which(item160.dat4$rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item160.dat5 <- item160.dat4

###########################
# Cleaning step (NA to zero)
###########################
#For now: convert NAs in rvalues and inches to zero for summarizing purposes (will revert back to NAs after)
item160.dat5$rvalues1[which(is.na(item160.dat5$rvalues1))] <- 0
item160.dat5$rvalues1[which(item160.dat5$rvalues1 == "None")] <- 0
item160.dat5$rvalues2[which(is.na(item160.dat5$rvalues2))] <- 0
item160.dat5$rvalues3[which(is.na(item160.dat5$rvalues3))] <- 0

#QC the clean bulb values
unique(item160.dat5$rvalues1)
unique(item160.dat5$rvalues2)
unique(item160.dat5$rvalues3)

item160.dat5$inches1[which(is.na(item160.dat5$inches1))] <- 0
item160.dat5$inches2[which(is.na(item160.dat5$inches2))] <- 0
item160.dat5$inches3[which(is.na(item160.dat5$inches3))] <- 0

#QC the clean bulb values -- There are NAs in each 
unique(item160.dat5$inches1)
unique(item160.dat5$inches2)
unique(item160.dat5$inches3)
###########################
# End Cleaning step
###########################


# r values multiplied by inches
item160.dat5$total.r.val <- (as.numeric(as.character(item160.dat5$rvalues1)) * item160.dat5$inches1) +  
  (as.numeric(as.character(item160.dat5$rvalues2)) * item160.dat5$inches2) + 
  (as.numeric(as.character(item160.dat5$rvalues3)) * item160.dat5$inches3)
#check
unique(item160.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item160.dat5$ufactor <- 1 / as.numeric(as.character(item160.dat5$total.r.val))

# replace inf with 0
item160.dat5$ufactor[which(item160.dat5$ufactor == "Inf")] <- 0

#make area numeric
item160.dat5$ufactor <- as.numeric(as.character(item160.dat5$ufactor))
item160.dat5$Wall.Area <- as.numeric(as.character(item160.dat5$Wall.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item160.dat5, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * as.numeric(as.character(ufactor))) / sum(Wall.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

wall.unique <- unique(item160.dat5[which(colnames(item160.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item160.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")



############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
item160.dat6$aveRval[which(item160.dat6$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
wall.ins.ind  <- item160.dat5$CK_Cadmus_ID[which(item160.dat5$`Wall.Cavity.Insulated?` == "No")]
wall.r.NA.ind <- item160.dat6$CK_Cadmus_ID[which(is.na(item160.dat6$aveRval))]

#which NA R values are in no insulation?
no.insulation <- wall.r.NA.ind[which(wall.r.NA.ind %in% wall.ins.ind)]

#replace no insulation in home ceiling with zero
item160.dat6$aveRval[which(item160.dat6$CK_Cadmus_ID %in% no.insulation)] <- 0

#replace any overwritten exterior insulation with correct values
item160.dat6$aveRval[which(item160.dat6$aveRval == 0 & item160.dat6$aveUval > 0)] <- (1 / item160.dat6$aveUval[which(item160.dat6$aveRval == 0 & item160.dat6$aveUval > 0)])
############################################################################################################
## END Process
############################################################################################################

item160.dat7 <- left_join(item160.dat6, rbsa.dat, by = "CK_Cadmus_ID")


#Bin R values -- SF only
item160.dat7$rvalue.bins <- "Unknown"
item160.dat7$rvalue.bins[which(item160.dat7$aveRval == 0)] <- "R0"
item160.dat7$rvalue.bins[which(item160.dat7$aveRval > 0  & item160.dat7$aveRval < 11)]  <- "R1.R10"
item160.dat7$rvalue.bins[which(item160.dat7$aveRval >= 11 & item160.dat7$aveRval < 17)]  <- "R11.R16"
item160.dat7$rvalue.bins[which(item160.dat7$aveRval >= 17 & item160.dat7$aveRval < 23)]  <- "R17.R22"
item160.dat7$rvalue.bins[which(item160.dat7$aveRval >= 22)] <- "RGT22"
unique(item160.dat7$rvalue.bins)

##cast data
item160.dat7$count <- 1
item160.dat.cast <- dcast(setDT(item160.dat7),
                         formula   = CK_Cadmus_ID + BuildingType +  Wall.Type ~ rvalue.bins, sum,
                         value.var = 'count')

for (i in 3:length(item160.dat.cast)){
  item160.dat.cast[i][which(is.na(item160.dat.cast[i])),] <- 0
}

head(item160.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item160.SF.dat <- subset(item160.dat.cast, item160.dat.cast$BuildingType == "Single Family")

#summarize by frame type
item160.sum1 <- summarise(group_by(item160.SF.dat, BuildingType, Wall.Type)
                         ,sampleSize      = length(unique(CK_Cadmus_ID))
                         ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
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
# sum across frame types
item160.sum2 <- summarise(group_by(item160.SF.dat, BuildingType)
                         ,Wall.Type = "All Frame Types"
                         ,sampleSize      = length(unique(CK_Cadmus_ID))
                         ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
                         ,r0.percent      = sum(R0) / sampleSizeNoNA ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
                         ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
                         ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
                         ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
                         ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
                         ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
                         ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
                         ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
                         ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
                         ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA))

item160.frame.types <- rbind.data.frame(item160.sum1, item160.sum2, stringsAsFactors = F)


item160.SF.dat$count <- 1
#across R levels, by frame type
item160.sum.allLevels1 <- summarise(group_by(item160.SF.dat, BuildingType, Wall.Type)
                                   ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
                                   ,WallTypeCount = sum(count)
                                   ,TotalCount = sum(item160.SF.dat$count)
                                   ,AllInsulationLevelPercent = WallTypeCount / TotalCount
                                   ,AllInsulationSE = sqrt((AllInsulationLevelPercent * (1 - AllInsulationLevelPercent)) / WallTypeCount)
)
#across R levels, across frame types
item160.sum.allLevels2 <- summarise(group_by(item160.SF.dat, BuildingType)
                                   ,Wall.Type = "All Frame Types"
                                   ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
                                   ,WallTypeCount = sum(count)
                                   ,TotalCount = sum(item160.SF.dat$count)
                                   ,AllInsulationLevelPercent = WallTypeCount / TotalCount
                                   ,AllInsulationSE = sqrt((AllInsulationLevelPercent * (1 - AllInsulationLevelPercent)) / WallTypeCount)
)
item160.sum.allLevels <- rbind.data.frame(item160.sum.allLevels1, item160.sum.allLevels2, stringsAsFactors = F)

#Check to make sure they add to 2
sum(item160.sum.allLevels$AllInsulationLevelPercent)

#join all insulation levels onto rvalue summary
item160.final <- data.frame("BuildingType" = item160.frame.types$BuildingType
                           ,"Wall.Type" = item160.frame.types$Wall.Type
                           ,"Percent.R0" = item160.frame.types$r0.percent
                           ,"SE.R0" = item160.frame.types$r0.se
                           ,"Percent.R1.R10" = item160.frame.types$r1.r10.percent
                           ,"SE.R1.R10" = item160.frame.types$r1.r10.se
                           ,"Percent.R11.R16" = item160.frame.types$r11.r16.percent
                           ,"SE.R11.R16" = item160.frame.types$r11.r16.se
                           ,"Percent.R17.R22" = item160.frame.types$r17.r22.percent
                           ,"SE.R17.R22" = item160.frame.types$r17.r22.se
                           ,"Percent.RGT22" = item160.frame.types$rGT22.percent
                           ,"SE.RGT22" = item160.frame.types$rGT22.se
                           ,"Percent_All Insulation Levels" = item160.sum.allLevels$AllInsulationLevelPercent
                           ,"SE_All Insulation Levels"   = item160.sum.allLevels$AllInsulationSE
                           ,"SampleSize" = item160.sum.allLevels$sampleSizeNoNA)

