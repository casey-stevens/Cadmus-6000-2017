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
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item164.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item164.dat.11 <- item164.dat.1[which(item164.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item164.dat.12 <- item164.dat.11[which(item164.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item164.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item164.dat.12$Primary.Heating.System)
item164.dat.12$count <- 1

item164.dat.13 <- unique(item164.dat.12[which(item164.dat.12$Heating.Fuel == "Electric"),])

item164.sum1 <- summarise(group_by(item164.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item164.sum1$Count <- 1
which(duplicated(item164.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item164.sum1$Heating.Fuel)

item164.mechanical <- item164.sum1






#############################################################################################
# Similar to Item 26
#############################################################################################
#
#For Envelope information
#
#############################################################################################
#############################################################################################
#############################################################################################
# PREP FOR ITEMS 26, 30, 31
#############################################################################################
#############################################################################################


#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]

#subset envelope data to necessary columns
ceiling.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                , "Category"
                                                                , "Ceiling.Type"
                                                                , "Ceiling.Sub-Type"
                                                                , "Ceiling.Area"
                                                                , "Ceiling.Insulated?"
                                                                , "Ceiling.Insulation.Type.1"
                                                                , "Ceiling.Insulation.Thickness.1"
                                                                , "Ceiling.Insulation.Type.2"                                                  
                                                                , "Ceiling.Insulation.Thickness.2"
                                                                # , "Ceiling.Insulation.Type.3"                                                  
                                                                # , "Ceiling.Insulation.Thickness.3"
))]
length(unique(ceiling.dat$CK_Cadmus_ID))#584 - missing 15 sites

#subset to only wall information
ceiling.dat1 <- ceiling.dat[which(ceiling.dat$Category == "Ceiling"),]

#merge analysis data with cleaned RBSA data
ceiling.dat2 <- left_join(rbsa.dat, ceiling.dat1, by = "CK_Cadmus_ID")

ceiling.SF.dat1 <- ceiling.dat2[which(ceiling.dat2$BuildingType == "Single Family"),]

ceiling.SF.dat2 <- left_join(ceiling.SF.dat1, item164.mechanical, by = c("CK_Cadmus_ID"))

ceiling.elec <- ceiling.SF.dat2[which(ceiling.SF.dat2$Heating.Fuel == "Electric"),]

#remove items have the "-- datapoint not asked for --"
ceiling.dat3 <- ceiling.elec[which(ceiling.elec$`Ceiling.Insulated?` %in% c("Yes", "No")),]
length(unique(ceiling.dat3$CK_Cadmus_ID))#89
unique(ceiling.dat3$`Ceiling.Insulated?`)


###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
ceiling.dat3$Ceiling.Insulation.Thickness.1[which(ceiling.dat3$Ceiling.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
ceiling.dat3$Ceiling.Insulation.Thickness.1[which(ceiling.dat3$Ceiling.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.1[which(is.na(ceiling.dat3$Ceiling.Insulation.Thickness.1))] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.1[which(ceiling.dat3$Ceiling.Insulation.Thickness.1 == "20 or more inches")] <- "20 inches"
ceiling.dat3$Ceiling.Insulation.Thickness.2[which(ceiling.dat3$Ceiling.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
ceiling.dat3$Ceiling.Insulation.Thickness.2[which(ceiling.dat3$Ceiling.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.2[which(is.na(ceiling.dat3$Ceiling.Insulation.Thickness.2))] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.2[which(ceiling.dat3$Ceiling.Insulation.Thickness.2 == "-- Datapoint not asked for --")] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
ceiling.dat3$count <- 1
ceiling.dat3$TMP_ID <- cumsum(ceiling.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(ceiling.dat3$Ceiling.Insulation.Thickness.1, " "))
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = ceiling.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = ceiling.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(ceiling.dat3$Ceiling.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = ceiling.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = ceiling.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
ceiling.dat4 <- as.data.frame(left_join(ceiling.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                              , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
ceiling.dat4$inches1 <- as.numeric(as.character(ceiling.dat4$V1.x)) # warning here is OK
ceiling.dat4$inches2 <- as.numeric(as.character(ceiling.dat4$V1.y)) # warning here is OK

ceiling.dat4$rvalues1 <- ceiling.dat4$Ceiling.Insulation.Type.1
ceiling.dat4$rvalues2 <- ceiling.dat4$Ceiling.Insulation.Type.2

#check uniques
unique(ceiling.dat4$rvalues1)
unique(ceiling.dat4$rvalues2)

#fix names that are not in R value table
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
ceiling.dat4$rvalues2[which(ceiling.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
ceiling.dat4$rvalues2[which(ceiling.dat4$rvalues2 == "N/A")]                              <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  ceiling.dat4$rvalues2[which(ceiling.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

ceiling.dat5 <- ceiling.dat4
length(unique(ceiling.dat5$CK_Cadmus_ID)) #254 -- check with line 66

###########################
# Cleaning step (NA to zero)
###########################
#For now: convert NAs in rvalues and inches to zero for summarizing purposes (will revert back to NAs after)
ceiling.dat5$rvalues1[which(is.na(ceiling.dat5$rvalues1))] <- 0
ceiling.dat5$rvalues1[which(ceiling.dat5$rvalues1 == "None")] <- 0
ceiling.dat5$rvalues2[which(is.na(ceiling.dat5$rvalues2))] <- 0

#QC the clean bulb values
unique(ceiling.dat5$rvalues1)
unique(ceiling.dat5$rvalues2)

ceiling.dat5$inches1[which(is.na(ceiling.dat5$inches1))] <- 0
ceiling.dat5$inches2[which(is.na(ceiling.dat5$inches2))] <- 0

#QC the clean bulb values -- There are NAs in each 
unique(ceiling.dat5$inches1)
unique(ceiling.dat5$inches2)
###########################
# End Cleaning step
###########################


# r values multiplied by inches
ceiling.dat5$total.r.val <- (as.numeric(as.character(ceiling.dat5$rvalues1)) * ceiling.dat5$inches1) + (as.numeric(as.character(ceiling.dat5$rvalues2)) * ceiling.dat5$inches2)
unique(ceiling.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
ceiling.dat5$ufactor <- 1 / as.numeric(as.character(ceiling.dat5$total.r.val))

# replace inf with 0
ceiling.dat5$ufactor[which(ceiling.dat5$ufactor == "Inf")] <- 0

#make area numeric
ceiling.dat5$ufactor <- as.numeric(as.character(ceiling.dat5$ufactor))
ceiling.dat5$Ceiling.Area <- as.numeric(as.character(ceiling.dat5$Ceiling.Area))

#weight the u factor per home
weightedU <- summarise(group_by(ceiling.dat5, CK_Cadmus_ID, Ceiling.Type)
                       ,aveUval = sum(Ceiling.Area * as.numeric(as.character(ufactor))) / sum(Ceiling.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

Ceiling.unique <- unique(ceiling.dat5[which(colnames(ceiling.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

ceiling.dat6 <- left_join(weightedU, Ceiling.unique, by = "CK_Cadmus_ID")
length(unique(ceiling.dat6$CK_Cadmus_ID)) #254 - check with line 66


############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
ceiling.dat6$aveRval[which(ceiling.dat6$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
Ceiling.ins.ind  <- ceiling.dat5$CK_Cadmus_ID[which(ceiling.dat5$`Ceiling.Insulated?` == "No")]
Ceiling.r.NA.ind <- ceiling.dat6$CK_Cadmus_ID[which(is.na(ceiling.dat6$aveRval))]

#which NA R values are in no insulation?
no.insulation <- Ceiling.r.NA.ind[which(Ceiling.r.NA.ind %in% Ceiling.ins.ind)]

#replace no insulation in home ceiling with zero
ceiling.dat6$aveRval[which(ceiling.dat6$CK_Cadmus_ID %in% no.insulation)] <- 0
############################################################################################################
## END Process
############################################################################################################

ceiling.dat7 <- left_join(ceiling.dat6, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))
length(unique(ceiling.dat7$CK_Cadmus_ID)) #89 - check with line 66

#############################################################################################
#############################################################################################
# END PREP
#############################################################################################
#############################################################################################







#############################################################################################
# Item 164: DISTRIBUTION OF ATTIC INSULATION, ELECTRICALLY HEATED HOMES (SF table B-9)
#############################################################################################

item164.dat <- ceiling.dat7[which(ceiling.dat7$Ceiling.Type == "Attic"),]


#Bin R values -- SF only
item164.dat$rvalue.bins <- "Unknown"
item164.dat$rvalue.bins[which(item164.dat$aveRval == 0)] <- "R0"
item164.dat$rvalue.bins[which(item164.dat$aveRval > 0  & item164.dat$aveRval < 11)]  <- "R1.R10"
item164.dat$rvalue.bins[which(item164.dat$aveRval >= 11 & item164.dat$aveRval < 16)]  <- "R11.R15"
item164.dat$rvalue.bins[which(item164.dat$aveRval >= 16 & item164.dat$aveRval < 21)]  <- "R16.R20"
item164.dat$rvalue.bins[which(item164.dat$aveRval >= 21 & item164.dat$aveRval < 26)]  <- "R21.R25"
item164.dat$rvalue.bins[which(item164.dat$aveRval >= 26 & item164.dat$aveRval < 31)]  <- "R26.R30"
item164.dat$rvalue.bins[which(item164.dat$aveRval >= 31 & item164.dat$aveRval < 41)]  <- "R31.R40"
item164.dat$rvalue.bins[which(item164.dat$aveRval >= 41 & item164.dat$aveRval < 51)]  <- "R41.R50"
item164.dat$rvalue.bins[which(item164.dat$aveRval >= 51)] <- "RGT50"
unique(item164.dat$rvalue.bins)

item164.dat0 <- item164.dat[which(item164.dat$rvalue.bins != "Unknown"),]
item164.dat0$count <- 1

#summarise by rvalue bins
item164.sum1 <- summarise(group_by(item164.dat0, BuildingType, rvalue.bins)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item164.sum2 <- summarise(group_by(item164.dat0, BuildingType)
                         ,rvalue.bins = "Total"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item164.merge <- rbind.data.frame(item164.sum1,item164.sum2, stringsAsFactors = F)

item164.tot.count <- item164.sum2[which(colnames(item164.sum2) %in% c("BuildingType"
                                                                   ,"Count"))]
colnames(item164.tot.count) <- c("BuildingType","Total.Count")

item164.final <- left_join(item164.merge, item164.tot.count, by = "BuildingType")

item164.final$Percent <- item164.final$Count / item164.final$Total.Count
item164.final$SE <- sqrt(item164.final$Percent * (1 - item164.final$Percent) / item164.final$SampleSize)


item164.table <- data.frame("BuildingType" = item164.final$BuildingType
                           ,"R Values" = item164.final$rvalue.bins
                           ,"Percent" = item164.final$Percent
                           ,"SE" = item164.final$SE
                           ,"SampleSize" = item164.final$SampleSize)









#############################################################################################
# Item 165: DISTRIBUTION OF VAULT CEILING INSULATION LEVEL, ELECTRICALLY HEATED HOMES (SF table B-10)
#############################################################################################
item165.dat <- ceiling.dat7[which(ceiling.dat7$Ceiling.Type == "Sloped / Vaulted (no attic)"),]


#Bin R values -- SF only
item165.dat$rvalue.bins <- "Unknown"
item165.dat$rvalue.bins[which(item165.dat$aveRval == 0)] <- "R0"
item165.dat$rvalue.bins[which(item165.dat$aveRval > 0  & item165.dat$aveRval < 11)]  <- "R1.R10"
item165.dat$rvalue.bins[which(item165.dat$aveRval >= 11 & item165.dat$aveRval < 16)]  <- "R11.R15"
item165.dat$rvalue.bins[which(item165.dat$aveRval >= 16 & item165.dat$aveRval < 21)]  <- "R16.R20"
item165.dat$rvalue.bins[which(item165.dat$aveRval >= 21 & item165.dat$aveRval < 26)]  <- "R21.R25"
item165.dat$rvalue.bins[which(item165.dat$aveRval >= 26 & item165.dat$aveRval < 31)]  <- "R26.R30"
item165.dat$rvalue.bins[which(item165.dat$aveRval >= 31 & item165.dat$aveRval < 41)]  <- "R31.R40"
item165.dat$rvalue.bins[which(item165.dat$aveRval >= 41 & item165.dat$aveRval < 51)]  <- "R41.R50"
item165.dat$rvalue.bins[which(item165.dat$aveRval >= 51)] <- "RGT50"
unique(item165.dat$rvalue.bins)

item165.dat0 <- item165.dat[which(item165.dat$rvalue.bins != "Unknown"),]
item165.dat0$count <- 1

#summarise by rvalue bins
item165.sum1 <- summarise(group_by(item165.dat0, BuildingType, rvalue.bins)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item165.sum2 <- summarise(group_by(item165.dat0, BuildingType)
                         ,rvalue.bins = "Total"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item165.merge <- rbind.data.frame(item165.sum1,item165.sum2, stringsAsFactors = F)

item165.tot.count <- item165.sum2[which(colnames(item165.sum2) %in% c("BuildingType"
                                                                   ,"Count"))]
colnames(item165.tot.count) <- c("BuildingType","Total.Count")

item165.final <- left_join(item165.merge, item165.tot.count, by = "BuildingType")

item165.final$Percent <- item165.final$Count / item165.final$Total.Count
item165.final$SE <- sqrt(item165.final$Percent * (1 - item165.final$Percent) / item165.final$SampleSize)


item165.table <- data.frame("BuildingType" = item165.final$BuildingType
                           ,"R Values" = item165.final$rvalue.bins
                           ,"Percent" = item165.final$Percent
                           ,"SE" = item165.final$SE
                           ,"SampleSize" = item165.final$SampleSize)


