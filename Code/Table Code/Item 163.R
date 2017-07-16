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
# Item 163: DISTRIBUTION OF FLOOR INSULATION, ELECTRICALLY HEATED HOMES (SF table B-8)
#############################################################################################
#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item163.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item163.dat.11 <- item163.dat.1[which(item163.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item163.dat.12 <- item163.dat.11[which(item163.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item163.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item163.dat.12$Primary.Heating.System)
item163.dat.12$count <- 1

item163.dat.13 <- unique(item163.dat.12[which(item163.dat.12$Heating.Fuel == "Electric"),])

item163.sum1 <- summarise(group_by(item163.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item163.sum1$Count <- 1
which(duplicated(item163.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item163.sum1$Heating.Fuel)

item163.mechanical <- item163.sum1






#############################################################################################
# Similar to Item 23
#############################################################################################
#
#For Envelope information
#
#############################################################################################
#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]

#subset envelope data to necessary columns
item163.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
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
                                                               , "Slab.Insulated?"
                                                               , "Slab.Insulation.Type.1"
                                                               , "Slab.Insulation.Thickness.1"
                                                               , "Slab.Insulation.Type.2"                                                  
                                                               , "Slab.Insulation.Thickness.2"
                                                               # , "Slab.Insulation.Type.3"                                                  
                                                               # , "Slab.Insulation.Thickness.3"
))]

## Note that we don't have any usable observations for slab insulation -- excluding from analysis 
# to make my life easier
length(unique(item163.dat$CK_Cadmus_ID))#584

#subset to only wall information
item163.dat1 <- item163.dat[which(item163.dat$Category == "Floor"),]

#remove unneccesary floor types
item163.dat2 <- left_join(item163.dat1, rbsa.dat, by = "CK_Cadmus_ID")

item163.SF.dat1 <- item163.dat2[which(item163.dat2$BuildingType == "Single Family"),]

item163.SF.dat2 <- left_join(item163.SF.dat1, item163.mechanical, by = c("CK_Cadmus_ID"))

item163.elec <- item163.SF.dat2[which(item163.SF.dat2$Heating.Fuel == "Electric"),]

#remove items have the datapoint was not asked for
item163.SF.dat3 <- item163.elec[which(item163.elec$`Floor.Insulated?` %in% c("Yes", "No")),]
length(unique(item163.SF.dat3$CK_Cadmus_ID))#94
unique(item163.SF.dat3$`Floor.Insulated?`)


###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
unique(item163.SF.dat3$Floor.Insulation.Thickness.1)
item163.SF.dat3$Floor.Insulation.Thickness.1[which(item163.SF.dat3$Floor.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item163.SF.dat3$Floor.Insulation.Thickness.1[which(item163.SF.dat3$Floor.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item163.SF.dat3$Floor.Insulation.Thickness.1[which(item163.SF.dat3$Floor.Insulation.Thickness.1 == "-- Datapoint not asked for --")] <- "N/A N/A"
item163.SF.dat3$Floor.Insulation.Thickness.1[which(is.na(item163.SF.dat3$Floor.Insulation.Thickness.1))] <- "N/A N/A"
item163.SF.dat3$Floor.Insulation.Thickness.2[which(item163.SF.dat3$Floor.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item163.SF.dat3$Floor.Insulation.Thickness.2[which(item163.SF.dat3$Floor.Insulation.Thickness.2 == "200024390821")] <- "Unknown Unknown"
item163.SF.dat3$Floor.Insulation.Thickness.2[which(item163.SF.dat3$Floor.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item163.SF.dat3$Floor.Insulation.Thickness.2[which(item163.SF.dat3$Floor.Insulation.Thickness.2 == "-- Datapoint not asked for --")] <- "N/A N/A"
item163.SF.dat3$Floor.Insulation.Thickness.2[which(is.na(item163.SF.dat3$Floor.Insulation.Thickness.2))] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
item163.SF.dat3$count <- 1
item163.SF.dat3$TMP_ID <- cumsum(item163.SF.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item163.SF.dat3$Floor.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item163.SF.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item163.SF.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item163.SF.dat3$Floor.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item163.SF.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item163.SF.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
item163.SF.dat4 <- as.data.frame(left_join(item163.SF.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                                , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item163.SF.dat4$inches1 <- as.numeric(as.character(item163.SF.dat4$V1.x)) # warning here is OK
item163.SF.dat4$inches2 <- as.numeric(as.character(item163.SF.dat4$V1.y)) # warning here is OK

item163.SF.dat4$rvalues1 <- item163.SF.dat4$Floor.Insulation.Type.1
item163.SF.dat4$rvalues2 <- item163.SF.dat4$Floor.Insulation.Type.2

#check uniques
unique(item163.SF.dat4$rvalues1)
unique(item163.SF.dat4$rvalues2)

#fix names that are not in R value table
item163.SF.dat4$rvalues1[which(item163.SF.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item163.SF.dat4$rvalues1[which(item163.SF.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item163.SF.dat4$rvalues1[which(item163.SF.dat4$rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
item163.SF.dat4$rvalues2[which(item163.SF.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item163.SF.dat4$rvalues2[which(item163.SF.dat4$rvalues2 == "N/A")]                              <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item163.SF.dat4$rvalues1[which(item163.SF.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item163.SF.dat4$rvalues2[which(item163.SF.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item163.SF.dat5 <- item163.SF.dat4

###########################
# Cleaning step (NA to zero)
###########################
#For now: convert NAs in rvalues and inches to zero for summarizing purposes (will revert back to NAs after)
item163.SF.dat5$rvalues1[which(is.na(item163.SF.dat5$rvalues1))] <- 0
item163.SF.dat5$rvalues1[which(item163.SF.dat5$rvalues1 == "None")] <- 0
item163.SF.dat5$rvalues2[which(is.na(item163.SF.dat5$rvalues2))] <- 0

#QC the clean bulb values
unique(item163.SF.dat5$rvalues1)
unique(item163.SF.dat5$rvalues2)

item163.SF.dat5$inches1[which(is.na(item163.SF.dat5$inches1))] <- 0
item163.SF.dat5$inches2[which(is.na(item163.SF.dat5$inches2))] <- 0

#QC the clean bulb values -- There are NAs in each 
unique(item163.SF.dat5$inches1)
unique(item163.SF.dat5$inches2)
###########################
# End Cleaning step
###########################


# r values multiplied by inches
item163.SF.dat5$total.r.val <- (as.numeric(as.character(item163.SF.dat5$rvalues1)) * item163.SF.dat5$inches1) + (as.numeric(as.character(item163.SF.dat5$rvalues2)) * item163.SF.dat5$inches2)
unique(item163.SF.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item163.SF.dat5$ufactor <- 1 / as.numeric(as.character(item163.SF.dat5$total.r.val))

# replace inf with 0
item163.SF.dat5$ufactor[which(item163.SF.dat5$ufactor == "Inf")] <- 0

#make area numeric
item163.SF.dat5$ufactor <- as.numeric(as.character(item163.SF.dat5$ufactor))
item163.SF.dat5$Floor.Area <- as.numeric(as.character(item163.SF.dat5$Floor.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item163.SF.dat5, CK_Cadmus_ID, Floor.Type)
                       ,aveUval = sum(Floor.Area * as.numeric(as.character(ufactor))) / sum(Floor.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

Floor.unique <- unique(item163.SF.dat5[which(colnames(item163.SF.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item163.SF.dat6 <- left_join(weightedU, Floor.unique, by = "CK_Cadmus_ID")



############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
item163.SF.dat6$aveRval[which(item163.SF.dat6$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
Floor.ins.ind  <- item163.SF.dat5$CK_Cadmus_ID[which(item163.SF.dat5$`Floor.Insulated?` == "No")]
Floor.r.NA.ind <- item163.SF.dat6$CK_Cadmus_ID[which(is.na(item163.SF.dat6$aveRval))]

#which NA R values are in no insulation?
no.insulation <- Floor.r.NA.ind[which(Floor.r.NA.ind %in% Floor.ins.ind)]

#replace no insulation in home ceiling with zero
item163.SF.dat6$aveRval[which(item163.SF.dat6$CK_Cadmus_ID %in% no.insulation)] <- 0
############################################################################################################
## END Process
############################################################################################################

item163.SF.dat7 <- left_join(item163.SF.dat6, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))


#Bin R values -- SF only
item163.SF.dat7$rvalue.bins <- "Unknown"
item163.SF.dat7$rvalue.bins[which(item163.SF.dat7$aveRval == 0)] <- "R0"
item163.SF.dat7$rvalue.bins[which(item163.SF.dat7$aveRval > 0  & item163.SF.dat7$aveRval < 4)]  <- "R1.R3"
item163.SF.dat7$rvalue.bins[which(item163.SF.dat7$aveRval >= 4 & item163.SF.dat7$aveRval < 11)]  <- "R4.R10"
item163.SF.dat7$rvalue.bins[which(item163.SF.dat7$aveRval >= 11 & item163.SF.dat7$aveRval < 16)]  <- "R11.R15"
item163.SF.dat7$rvalue.bins[which(item163.SF.dat7$aveRval >= 16 & item163.SF.dat7$aveRval < 23)]  <- "R16.R22"
item163.SF.dat7$rvalue.bins[which(item163.SF.dat7$aveRval >= 23 & item163.SF.dat7$aveRval < 28)]  <- "R23.R27"
item163.SF.dat7$rvalue.bins[which(item163.SF.dat7$aveRval >= 28 & item163.SF.dat7$aveRval < 38)]  <- "R28.R37"
item163.SF.dat7$rvalue.bins[which(item163.SF.dat7$aveRval >= 38)] <- "RGT38"
unique(item163.SF.dat7$rvalue.bins)

##cast data
item163.SF.dat7$count <- 1
item163.SF.dat.cast <- dcast(setDT(item163.SF.dat7),
                            formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                            value.var = 'count')
head(item163.SF.dat.cast)


# ## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
# SF.item163.SF.dat <- subset(item163.SF.dat.cast, item163.SF.dat.cast$BuildingType == "Single Family")

#summarize --SF
item163.SF.sum <- summarise(group_by(item163.SF.dat.cast, BuildingType, HomeYearBuilt_bins4)
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

item163.SF.dat.cast$count <- 1
item163.SF.sum.allVintages <- summarise(group_by(item163.SF.dat.cast, BuildingType)
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
item163.SF.final <- rbind.data.frame(item163.SF.sum, item163.SF.sum.allVintages
                                    , stringsAsFactors = F)

item163.SF.table <- data.frame("BuildingType" = item163.SF.final$BuildingType
                              ,"Housing.Vintage" = item163.SF.final$HomeYearBuilt_bins4
                              ,"Percent_R1_R3" = item163.SF.final$r1.r3.percent
                              ,"SE_R1_R3" = item163.SF.final$r1.r3.se
                              ,"Percent_R4_R10" = item163.SF.final$r4.r10.percent
                              ,"SE_R4_R10" = item163.SF.final$r4.r10.se
                              ,"Percent_R11_R15" = item163.SF.final$r11.r15.percent
                              ,"SE_R11_R15" = item163.SF.final$r11.r15.se
                              ,"Percent_R16_R22" = item163.SF.final$r16.r22.percent
                              ,"SE_R16_R22" = item163.SF.final$r16.r22.se
                              ,"Percent_R23_R27" = item163.SF.final$r23.r27.percent
                              ,"SE_R23_R27" = item163.SF.final$r23.r27.se
                              ,"Percent_R28_R37" = item163.SF.final$r28.r37.percent
                              ,"SE_R28_R37" = item163.SF.final$r28.r37.se
                              ,"Percent_RGT38" = item163.SF.final$rGT38.percent
                              ,"SE_RGT38" = item163.SF.final$rGT38.se
                              ,"Percent_None" = item163.SF.final$r0.percent
                              ,"SE_None" = item163.SF.final$r0.se
                              ,"SampleSize" = item163.SF.final$sampleSizeNoNA)
item163.SF.table1 <- item163.SF.table[which(!(is.na(item163.SF.table$Housing.Vintage))),]




#############################################################################################
#############################################################################################
# Item 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (MH table 18)
#############################################################################################
#############################################################################################

item163.MH.dat2.5 <- item163.dat2[which(item163.dat2$BuildingType == "Manufactured"),]

#create "Alternative" category
# item163.MH.dat2$Floor.Type[which(item163.MH.dat2$Floor.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
# length(unique(item163.MH.dat2$CK_Cadmus_ID))#473
# unique(item163.MH.dat2$Floor.Type)

#remove items have the datapoint was not asked for
item163.MH.dat3 <- item163.MH.dat2.5[which(item163.MH.dat2.5$`Floor.Insulated?` %in% c("Yes", "No")),]
length(unique(item163.MH.dat3$CK_Cadmus_ID))#280
unique(item163.MH.dat3$`Floor.Insulated?`)


###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
item163.MH.dat3$Floor.Insulation.Thickness.1[which(item163.MH.dat3$Floor.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item163.MH.dat3$Floor.Insulation.Thickness.1[which(item163.MH.dat3$Floor.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item163.MH.dat3$Floor.Insulation.Thickness.1[which(is.na(item163.MH.dat3$Floor.Insulation.Thickness.1))] <- "N/A N/A"
item163.MH.dat3$Floor.Insulation.Thickness.2[which(item163.MH.dat3$Floor.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item163.MH.dat3$Floor.Insulation.Thickness.2[which(item163.MH.dat3$Floor.Insulation.Thickness.2 == "200024390821")] <- "Unknown Unknown"
item163.MH.dat3$Floor.Insulation.Thickness.2[which(item163.MH.dat3$Floor.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item163.MH.dat3$Floor.Insulation.Thickness.2[which(is.na(item163.MH.dat3$Floor.Insulation.Thickness.2))] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
item163.MH.dat3$count <- 1
item163.MH.dat3$TMP_ID <- cumsum(item163.MH.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item163.MH.dat3$Floor.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item163.MH.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item163.MH.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item163.MH.dat3$Floor.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item163.MH.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item163.MH.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
item163.MH.dat4 <- as.data.frame(left_join(item163.MH.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                                , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item163.MH.dat4$inches1 <- as.numeric(as.character(item163.MH.dat4$V1.x)) # warning here is OK
item163.MH.dat4$inches2 <- as.numeric(as.character(item163.MH.dat4$V1.y)) # warning here is OK

item163.MH.dat4$rvalues1 <- item163.MH.dat4$Floor.Insulation.Type.1
item163.MH.dat4$rvalues2 <- item163.MH.dat4$Floor.Insulation.Type.2

#check uniques
unique(item163.MH.dat4$rvalues1)
unique(item163.MH.dat4$rvalues2)

#fix names that are not in R value table
item163.MH.dat4$rvalues1[which(item163.MH.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item163.MH.dat4$rvalues1[which(item163.MH.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item163.MH.dat4$rvalues1[which(item163.MH.dat4$rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
item163.MH.dat4$rvalues2[which(item163.MH.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item163.MH.dat4$rvalues2[which(item163.MH.dat4$rvalues2 == "N/A")]                              <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item163.MH.dat4$rvalues1[which(item163.MH.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item163.MH.dat4$rvalues2[which(item163.MH.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item163.MH.dat5 <- item163.MH.dat4

###########################
# Cleaning step (NA to zero)
###########################
#For now: convert NAs in rvalues and inches to zero for summarizing purposes (will revert back to NAs after)
item163.MH.dat5$rvalues1[which(is.na(item163.MH.dat5$rvalues1))] <- 0
item163.MH.dat5$rvalues1[which(item163.MH.dat5$rvalues1 == "None")] <- 0
item163.MH.dat5$rvalues2[which(is.na(item163.MH.dat5$rvalues2))] <- 0

#QC the clean bulb values
unique(item163.MH.dat5$rvalues1)
unique(item163.MH.dat5$rvalues2)

item163.MH.dat5$inches1[which(is.na(item163.MH.dat5$inches1))] <- 0
item163.MH.dat5$inches2[which(is.na(item163.MH.dat5$inches2))] <- 0

#QC the clean bulb values -- There are NAs in each 
unique(item163.MH.dat5$inches1)
unique(item163.MH.dat5$inches2)
###########################
# End Cleaning step
###########################


# r values multiplied by inches
item163.MH.dat5$total.r.val <- (as.numeric(as.character(item163.MH.dat5$rvalues1)) * item163.MH.dat5$inches1) + (as.numeric(as.character(item163.MH.dat5$rvalues2)) * item163.MH.dat5$inches2)
unique(item163.MH.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item163.MH.dat5$ufactor <- 1 / as.numeric(as.character(item163.MH.dat5$total.r.val))

# replace inf with 0
item163.MH.dat5$ufactor[which(item163.MH.dat5$ufactor == "Inf")] <- 0

#make area numeric
item163.MH.dat5$ufactor <- as.numeric(as.character(item163.MH.dat5$ufactor))
item163.MH.dat5$Floor.Area <- as.numeric(as.character(item163.MH.dat5$Floor.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item163.MH.dat5, CK_Cadmus_ID, Floor.Type)
                       ,aveUval = sum(Floor.Area * as.numeric(as.character(ufactor))) / sum(Floor.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

Floor.unique <- unique(item163.MH.dat5[which(colnames(item163.MH.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item163.MH.dat6 <- left_join(weightedU, Floor.unique, by = "CK_Cadmus_ID")



############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
item163.MH.dat6$aveRval[which(item163.MH.dat6$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
Floor.ins.ind  <- item163.MH.dat5$CK_Cadmus_ID[which(item163.MH.dat5$`Floor.Insulated?` == "No")]
Floor.r.NA.ind <- item163.MH.dat6$CK_Cadmus_ID[which(is.na(item163.MH.dat6$aveRval))]

#which NA R values are in no insulation?
no.insulation <- Floor.r.NA.ind[which(Floor.r.NA.ind %in% Floor.ins.ind)]

#replace no insulation in home ceiling with zero
item163.MH.dat6$aveRval[which(item163.MH.dat6$CK_Cadmus_ID %in% no.insulation)] <- 0
############################################################################################################
## END Process
############################################################################################################

item163.MH.dat7 <- left_join(item163.MH.dat6, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))


#Bin R values -- MH only
item163.MH.dat7$rvalue.bins <- "Unknown"
item163.MH.dat7$rvalue.bins[which(item163.MH.dat7$aveRval > 0  & item163.MH.dat7$aveRval < 9)]  <- "R0.R8"
item163.MH.dat7$rvalue.bins[which(item163.MH.dat7$aveRval >= 9 & item163.MH.dat7$aveRval < 15)]  <- "R9.R14"
item163.MH.dat7$rvalue.bins[which(item163.MH.dat7$aveRval >= 15 & item163.MH.dat7$aveRval < 22)]  <- "R15.R21"
item163.MH.dat7$rvalue.bins[which(item163.MH.dat7$aveRval >= 22 & item163.MH.dat7$aveRval < 31)]  <- "R22.R30"
item163.MH.dat7$rvalue.bins[which(item163.MH.dat7$aveRval >= 31 & item163.MH.dat7$aveRval < 41)]  <- "R31.R40"
unique(item163.MH.dat7$rvalue.bins)
