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
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))



#############################################################################################
# Item 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (SF table 30) (also need MH)
#############################################################################################

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]

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
                                                               , "Slab.Insulated?"
                                                               , "Slab.Insulation.Type.1"
                                                               , "Slab.Insulation.Thickness.1"
                                                               , "Slab.Insulation.Type.2"                                                  
                                                               , "Slab.Insulation.Thickness.2"
                                                               # , "Slab.Insulation.Type.3"                                                  
                                                               # , "Slab.Insulation.Thickness.3"
))]

unique(item23.dat$Floor.Insulation.Type.1)
unique(item23.dat$Floor.Insulation.Type.2)
unique(item23.dat$Floor.Insulation.Type.3) #nothing in this column
unique(item23.dat$Slab.Insulation.Type.1) #nothing in this column
unique(item23.dat$Slab.Insulation.Type.2) #nothing in this column
unique(item23.dat$Slab.Insulation.Type.3) #nothing in this column

#subset to only wall information
item23.dat1 <- item23.dat[which(item23.dat$Category == "Floor"),]

#remove unneccesary floor types
item23.dat2 <- left_join(rbsa.dat, item23.dat1, by = "CK_Cadmus_ID")

# item23.dat2.5 <- item23.dat2[which(item23.dat2$BuildingType == "Single Family"),]

#remove items have the datapoint was not asked for
item23.dat3 <- item23.dat2[which(item23.dat2$`Floor.Insulated?` %in% c("Yes", "No")),]
length(unique(item23.dat3$CK_Cadmus_ID))#258
unique(item23.dat3$`Floor.Insulated?`)


###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
unique(item23.dat3$Floor.Insulation.Thickness.1)
item23.dat3$Floor.Insulation.Thickness.1[which(item23.dat3$Floor.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item23.dat3$Floor.Insulation.Thickness.1[which(item23.dat3$Floor.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item23.dat3$Floor.Insulation.Thickness.1[which(item23.dat3$Floor.Insulation.Thickness.1 == "-- Datapoint not asked for --")] <- "N/A N/A"
item23.dat3$Floor.Insulation.Thickness.1[which(is.na(item23.dat3$Floor.Insulation.Thickness.1))] <- "N/A N/A"
item23.dat3$Floor.Insulation.Thickness.2[which(item23.dat3$Floor.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item23.dat3$Floor.Insulation.Thickness.2[which(item23.dat3$Floor.Insulation.Thickness.2 == "200024390821")] <- "Unknown Unknown"
item23.dat3$Floor.Insulation.Thickness.2[which(item23.dat3$Floor.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item23.dat3$Floor.Insulation.Thickness.2[which(item23.dat3$Floor.Insulation.Thickness.2 == "-- Datapoint not asked for --")] <- "N/A N/A"
item23.dat3$Floor.Insulation.Thickness.2[which(is.na(item23.dat3$Floor.Insulation.Thickness.2))] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
item23.dat3$count <- 1
item23.dat3$TMP_ID <- cumsum(item23.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item23.dat3$Floor.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item23.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item23.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item23.dat3$Floor.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item23.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item23.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
item23.dat4 <- as.data.frame(left_join(item23.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                             , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item23.dat4$inches1 <- as.numeric(as.character(item23.dat4$V1.x)) # warning here is OK
item23.dat4$inches2 <- as.numeric(as.character(item23.dat4$V1.y)) # warning here is OK

item23.dat4$rvalues1 <- item23.dat4$Floor.Insulation.Type.1
item23.dat4$rvalues2 <- item23.dat4$Floor.Insulation.Type.2

#check uniques
unique(item23.dat4$rvalues1)
unique(item23.dat4$rvalues2)

#fix names that are not in R value table
item23.dat4$rvalues1[which(item23.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item23.dat4$rvalues1[which(item23.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item23.dat4$rvalues1[which(item23.dat4$rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
item23.dat4$rvalues2[which(item23.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item23.dat4$rvalues2[which(item23.dat4$rvalues2 == "N/A")]                              <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item23.dat4$rvalues1[which(item23.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item23.dat4$rvalues2[which(item23.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item23.dat5 <- item23.dat4

###########################
# Cleaning step (No Insulation to zero)
###########################
item23.dat5$rvalues1[which(item23.dat5$`Floor.Insulated?` == "No")] <- 0
item23.dat5$rvalues2[which(item23.dat5$`Floor.Insulated?` == "No")] <- 0
item23.dat5$inches1[which(item23.dat5$`Floor.Insulated?` == "No")] <- 0
item23.dat5$inches2[which(item23.dat5$`Floor.Insulated?` == "No")] <- 0
###########################
# End Cleaning step
###########################
item23.dat5$rvalues1 <- as.numeric(as.character(item23.dat5$rvalues1))
item23.dat5$rvalues2 <- as.numeric(as.character(item23.dat5$rvalues2))


#identify which rows that do not contain NAs for any rvalues
Non_NA_ind <- which(!(is.na(item23.dat5$rvalues2)))






###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################

#create total.r.value column
item23.dat5$total.r.val <- NA


#calculate the weighted r value where wall cavity insulation type is not NA
for (i in Non_NA_ind){
  item23.dat5$total.r.val[i] <- (item23.dat5$rvalues1[i] * item23.dat5$inches1[i]) +  
    (item23.dat5$rvalues2[i] * item23.dat5$inches2[i])
}


#calculate the weighted r value where wall cavity insulation type is NA
item23.dat5$total.r.val[which(is.na(item23.dat5$total.r.val))] <- 
  (item23.dat5$rvalues1[which(is.na(item23.dat5$total.r.val))] *
     item23.dat5$inches1[which(is.na(item23.dat5$total.r.val))])

#check -- NOTE -- NONE SHOULD BE NA
unique(item23.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item23.dat5$ufactor <- 1 / item23.dat5$total.r.val

# replace inf with 0
item23.dat5$ufactor[which(item23.dat5$ufactor == "Inf")] <- 0

#make area numeric
item23.dat5$ufactor <- as.numeric(as.character(item23.dat5$ufactor))
item23.dat5$Floor.Area <- as.numeric(as.character(item23.dat5$Floor.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item23.dat5, CK_Cadmus_ID, Floor.Type)
                       ,aveUval = sum(Floor.Area * ufactor) / sum(Floor.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

Floor.unique <- unique(item23.dat5[which(colnames(item23.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item23.dat6 <- left_join(weightedU, Floor.unique, by = "CK_Cadmus_ID")

item23.dat7 <- left_join(item23.dat6, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))

item23.dat8 <- item23.dat7[which(!(is.na(item23.dat7$aveUval))),]

item23.SF <- item23.dat8[which(item23.dat8$BuildingType == "Single Family"),]

#Bin R values -- SF only
item23.SF$rvalue.bins <- "Unknown"
item23.SF$rvalue.bins[which(item23.SF$aveRval == 0)] <- "R0"
item23.SF$rvalue.bins[which(item23.SF$aveRval > 0   & item23.SF$aveRval < 4) ]  <- "R1.R3"
item23.SF$rvalue.bins[which(item23.SF$aveRval >= 4  & item23.SF$aveRval < 11)]  <- "R4.R10"
item23.SF$rvalue.bins[which(item23.SF$aveRval >= 11 & item23.SF$aveRval < 16)]  <- "R11.R15"
item23.SF$rvalue.bins[which(item23.SF$aveRval >= 16 & item23.SF$aveRval < 23)]  <- "R16.R22"
item23.SF$rvalue.bins[which(item23.SF$aveRval >= 23 & item23.SF$aveRval < 28)]  <- "R23.R27"
item23.SF$rvalue.bins[which(item23.SF$aveRval >= 28 & item23.SF$aveRval < 38)]  <- "R28.R37"
item23.SF$rvalue.bins[which(item23.SF$aveRval >= 38)] <- "RGT38"
unique(item23.SF$rvalue.bins)

##cast data
item23.SF$count <- 1
item23.SF.cast <- dcast(setDT(item23.SF),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
colnames(item23.SF.cast)

#summarize --Single Family only
item23.SF.sum <- summarise(group_by(item23.SF.cast, BuildingType, HomeYearBuilt_bins4)
                        ,SampleSize      = length(unique(CK_Cadmus_ID))
                        ,r0.percent      = sum(R0) / SampleSize ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
                        ,r0.se           = sd(R0) / sqrt(SampleSize)
                        ,r1.r3.percent  = 0#sum(R1.R3)  / SampleSize
                        ,r1.r3.se       = 0#sd(R1.R3) / sqrt(SampleSize)
                        ,r4.r10.percent  = sum(R4.R10)  / SampleSize
                        ,r4.r10.se       = sd(R4.R10) / sqrt(SampleSize)
                        ,r11.r15.percent = sum(R11.R15) / SampleSize
                        ,r11.r15.se      = sd(R11.R15) / sqrt(SampleSize)
                        ,r16.r22.percent = sum(R16.R22) / SampleSize
                        ,r16.r22.se      = sd(R16.R22) / sqrt(SampleSize)
                        ,r23.r27.percent = sum(R23.R27) / SampleSize
                        ,r23.r27.se      = sd(R23.R27) / sqrt(SampleSize)
                        ,r28.r37.percent = sum(R28.R37) / SampleSize
                        ,r28.r37.se      = sd(R28.R37) / sqrt(SampleSize)
                        ,rGT38.percent   = sum(RGT38) / SampleSize
                        ,rGT38.se        = sd(RGT38) / sqrt(SampleSize)
)

item23.SF.sum.allVintages <- summarise(group_by(item23.SF.cast, BuildingType)
                                    ,HomeYearBuilt_bins4 = "All Vintages"
                                    ,SampleSize      = length(unique(CK_Cadmus_ID))
                                    ,r0.percent      = sum(R0) / SampleSize ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
                                    ,r0.se           = sd(R0) / sqrt(SampleSize)
                                    ,r1.r3.percent  = 0#sum(R1.R3)  / SampleSize
                                    ,r1.r3.se       = 0#sd(R1.R3) / sqrt(SampleSize)
                                    ,r4.r10.percent  = sum(R4.R10)  / SampleSize
                                    ,r4.r10.se       = sd(R4.R10) / sqrt(SampleSize)
                                    ,r11.r15.percent = sum(R11.R15) / SampleSize
                                    ,r11.r15.se      = sd(R11.R15) / sqrt(SampleSize)
                                    ,r16.r22.percent = sum(R16.R22) / SampleSize
                                    ,r16.r22.se      = sd(R16.R22) / sqrt(SampleSize)
                                    ,r23.r27.percent = sum(R23.R27) / SampleSize
                                    ,r23.r27.se      = sd(R23.R27) / sqrt(SampleSize)
                                    ,r28.r37.percent = sum(R28.R37) / SampleSize
                                    ,r28.r37.se      = sd(R28.R37) / sqrt(SampleSize)
                                    ,rGT38.percent   = sum(RGT38) / SampleSize
                                    ,rGT38.se        = sd(RGT38) / sqrt(SampleSize)
)

#join all insulation levels onto rvalue summary
item23.SF.final <- rbind.data.frame(item23.SF.sum, item23.SF.sum.allVintages
                                 , stringsAsFactors = F)

item23.SF.table <- data.frame("BuildingType" = item23.SF.final$BuildingType
                              ,"Housing.Vintage" = item23.SF.final$HomeYearBuilt_bins4
                              ,"Percent_R0" = item23.SF.final$r0.percent
                              ,"SE_R0" = item23.SF.final$r0.se
                              ,"Percent_R1_R3" = item23.SF.final$r1.r3.percent
                              ,"SE_R1_R3" = item23.SF.final$r1.r3.se
                              ,"Percent_R4_R10" = item23.SF.final$r4.r10.percent
                              ,"SE_R4_R10" = item23.SF.final$r4.r10.se
                              ,"Percent_R11_R15" = item23.SF.final$r11.r15.percent
                              ,"SE_R11_R15" = item23.SF.final$r11.r15.se
                              ,"Percent_R16_R22" = item23.SF.final$r16.r22.percent
                              ,"SE_R16_R22" = item23.SF.final$r16.r22.se
                              ,"Percent_R23_R27" = item23.SF.final$r23.r27.percent
                              ,"SE_R23_R27" = item23.SF.final$r23.r27.se
                              ,"Percent_R28_R37" = item23.SF.final$r28.r37.percent
                              ,"SE_R28_R37" = item23.SF.final$r28.r37.se
                              ,"Percent_RGT38" = item23.SF.final$rGT38.percent
                              ,"SE_RGT38" = item23.SF.final$rGT38.se
                              ,"SampleSize" = item23.SF.final$SampleSize)
item23.SF.table1 <- item23.SF.table[which(!(is.na(item23.SF.table$Housing.Vintage))),]




#############################################################################################
#############################################################################################
# Item 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (MH table 18)
#############################################################################################
#############################################################################################
item23.MH <- item23.dat8[which(item23.dat8$BuildingType == "Manufactured"),]

#Bin R values -- MH only
item23.MH$rvalue.bins <- "Unknown"
item23.MH$rvalue.bins[which(item23.MH$aveRval > 0  & item23.MH$aveRval < 9)]  <- "R0.R8"
item23.MH$rvalue.bins[which(item23.MH$aveRval >= 9 & item23.MH$aveRval < 15)]  <- "R9.R14"
item23.MH$rvalue.bins[which(item23.MH$aveRval >= 15 & item23.MH$aveRval < 22)]  <- "R15.R21"
item23.MH$rvalue.bins[which(item23.MH$aveRval >= 22 & item23.MH$aveRval < 31)]  <- "R22.R30"
item23.MH$rvalue.bins[which(item23.MH$aveRval >= 31 & item23.MH$aveRval < 41)]  <- "R31.R40"
unique(item23.MH$rvalue.bins)

##cast data
item23.MH$count <- 1
item23.MH.cast <- dcast(setDT(item23.MH),
                            formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                            value.var = 'count')
head(item23.MH.cast)


# ## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
# MH.item23.MH.dat <- subset(item23.MH.dat.cast, item23.MH.dat.cast$BuildingType == "Single Family")

#summarize --SF
item23.MH.sum <- summarise(group_by(item23.MH.cast, BuildingType, HomeYearBuilt_bins4)
                           ,sampleSize      = length(unique(CK_Cadmus_ID))
                           ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
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

item23.MH.sum.allVintages <- summarise(group_by(item23.MH.cast, BuildingType)
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


item23.MH.table <- data.frame("BuildingType" = item23.MH.final$BuildingType
                              ,"Housing.Vintage" = item23.MH.final$HomeYearBuilt_bins4
                              ,"Percent_R0_R8" = item23.MH.final$r0.r8.percent
                              ,"SE_R0_R8" = item23.MH.final$r0.r8.se
                              ,"Percent_R9_R14" = item23.MH.final$r9.r14.percent
                              ,"SE_R9_R14" = item23.MH.final$r9.r14.se
                              ,"Percent_R15_R21" = item23.MH.final$r15.r21.percent
                              ,"SE_R15_R21" = item23.MH.final$r15.r21.percent
                              ,"Percent_R22_R30" = item23.MH.final$r22.r30.percent
                              ,"SE_R22_R30" = item23.MH.final$r22.r30.se
                              ,"Percent_R31_R40" = item23.MH.final$r31.r40.percent
                              ,"SE_R31_R40" = item23.MH.final$r31.r40.se
                              ,"SampleSize" = item23.MH.final$sampleSizeNoNA)
item23.MH.table1 <- item23.MH.table[which(!(is.na(item23.MH.table$Housing.Vintage))),]






