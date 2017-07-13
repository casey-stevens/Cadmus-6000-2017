#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))








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

#subset to only single family sites
ceiling.dat2.5 <- ceiling.dat2[which(ceiling.dat2$BuildingType == "Single Family"),]

#remove items have the "-- datapoint not asked for --"
ceiling.dat3 <- ceiling.dat2.5[which(ceiling.dat2.5$`Ceiling.Insulated?` %in% c("Yes", "No")),]
length(unique(ceiling.dat3$CK_Cadmus_ID))#254
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
length(unique(ceiling.dat7$CK_Cadmus_ID)) #254 - check with line 66

#############################################################################################
#############################################################################################
# END PREP
#############################################################################################
#############################################################################################




















############################################################################################################
## Item 26
############################################################################################################
item26.dat <- ceiling.dat7[which(ceiling.dat7$Ceiling.Type == "Attic"),]


#Bin R values -- SF only
item26.dat$rvalue.bins <- "Unknown"
item26.dat$rvalue.bins[which(item26.dat$aveRval == 0)] <- "R0"
item26.dat$rvalue.bins[which(item26.dat$aveRval > 0  & item26.dat$aveRval < 11)]  <- "R1.R10"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 11 & item26.dat$aveRval < 16)]  <- "R11.R15"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 16 & item26.dat$aveRval < 21)]  <- "R16.R20"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 21 & item26.dat$aveRval < 26)]  <- "R21.R25"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 26 & item26.dat$aveRval < 31)]  <- "R26.R30"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 31 & item26.dat$aveRval < 41)]  <- "R31.R40"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 41 & item26.dat$aveRval < 51)]  <- "R41.R50"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 51)] <- "RGT50"
unique(item26.dat$rvalue.bins)

##cast data
item26.dat$count <- 1
item26.dat.cast <- dcast(setDT(item26.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item26.dat.cast)

#summarize --SF
item26.sum <- summarise(group_by(item26.dat.cast, BuildingType)
                        ,TotalCount       = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,R0.Count       = sum(R0)
                        ,R0.sd            = sd(R0)
                        ,R1.R10.Count   = sum(R1.R10)
                        ,R1.R10.sd        = sd(R1.R10)
                        ,R11.R15.Count  = sum(R11.R15)
                        ,R11.R15.sd       = sd(R11.R15)
                        ,R16.R20.Count  = sum(R16.R20)
                        ,R16.R20.sd       = sd(R16.R20)
                        ,R21.R25.Count  = sum(R21.R25)
                        ,R21.R25.sd       = sd(R21.R25)
                        ,R26.R30.Count  = sum(R26.R30)
                        ,R26.R30.sd       = sd(R26.R30)
                        ,R31.R40.Count  = sum(R31.R40)
                        ,R31.R40.sd       = sd(R31.R40)
                        ,R41.R50.Count  = sum(R41.R50)
                        ,R41.R50.sd       = sd(R41.R50)
                        ,RGT50.Count    = sum(RGT50)
                        ,RGT50.sd         = sd(RGT50)
)


library(reshape2)
item26.dat1 <- melt(item26.sum, id.vars = c("BuildingType", "TotalCount"))
detach(package:reshape2)

item26.dat1$Ind <- "SampleSize"
item26.dat1$Ind[grep("sd", item26.dat1$variable)] <- "SD"

item26.dat1$rValueBins <- "Blank"
item26.dat1$rValueBins[grep("R0." , item26.dat1$variable)] <- "R0"
item26.dat1$rValueBins[grep("R1." , item26.dat1$variable)] <- "R1-R10"
item26.dat1$rValueBins[grep("R11.", item26.dat1$variable)] <- "R11-R15"
item26.dat1$rValueBins[grep("R16.", item26.dat1$variable)] <- "R16-R20"
item26.dat1$rValueBins[grep("R21.", item26.dat1$variable)] <- "R21-R25"
item26.dat1$rValueBins[grep("R26.", item26.dat1$variable)] <- "R26-R30"
item26.dat1$rValueBins[grep("R31.", item26.dat1$variable)] <- "R31-R40"
item26.dat1$rValueBins[grep("R41.", item26.dat1$variable)] <- "R40-R50"
item26.dat1$rValueBins[grep("RGT50.", item26.dat1$variable)] <- "R50+"

colnames(item26.dat1)

library(data.table)
#now cast by IND
item26.dat.cast2 <- dcast(setDT(item26.dat1),
                          formula   = BuildingType + TotalCount + rValueBins~ Ind, sum,
                          value.var = 'value')
head(item26.dat.cast2)

item26.dat.cast2$Percent <- item26.dat.cast2$SampleSize / item26.dat.cast2$TotalCount
item26.dat.cast2$SE <- item26.dat.cast2$SD / sqrt(item26.dat.cast2$SampleSize)

item26.dat.cast2 <- data.frame(item26.dat.cast2, stringsAsFactors = F)

cols.remove <- which(colnames(item26.dat.cast2) %in% c("SD","TotalCount"))
item26.final <- item26.dat.cast2[,-cols.remove]
item26.total.row <- data.frame("BuildingType" = "Manufactured"
                               ,"rValueBins" = "Total"
                               ,"Percent" = sum(item26.final$Percent)
                               ,"SE" = "NA"
                               ,"SampleSize" = sum(item26.final$SampleSize))
item26.table <- rbind.data.frame(item26.final, item26.total.row, stringsAsFactors = F)

item26.table1 <- data.frame("BuildingType" = item26.table$BuildingType
                            ,"rValueBins" = item26.table$rValueBins
                            ,"Percent" = item26.table$Percent
                            ,"SE" = item26.table$SE
                            ,"SampleSize" = item26.table$SampleSize)







############################################################################################################
## Item 30
############################################################################################################
item30.dat <- ceiling.dat7[which(ceiling.dat7$Ceiling.Type == "Sloped / Vaulted (no attic)"),]


#Bin R values -- SF only
item30.dat$rvalue.bins <- "Unknown"
item30.dat$rvalue.bins[which(item30.dat$aveRval == 0)] <- "R0"
item30.dat$rvalue.bins[which(item30.dat$aveRval > 0  & item30.dat$aveRval < 11)]  <- "R1.R10"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 11 & item30.dat$aveRval < 16)]  <- "R11.R15"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 16 & item30.dat$aveRval < 21)]  <- "R16.R20"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 21 & item30.dat$aveRval < 26)]  <- "R21.R25"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 26 & item30.dat$aveRval < 31)]  <- "R26.R30"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 31 & item30.dat$aveRval < 41)]  <- "R31.R40"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 41 & item30.dat$aveRval < 51)]  <- "R41.R50"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 51)] <- "RGT50"
unique(item30.dat$rvalue.bins)

##cast data
item30.dat$count <- 1
item30.dat.cast <- dcast(setDT(item30.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item30.dat.cast)

#summarize --SF
item30.sum <- summarise(group_by(item30.dat.cast, BuildingType)
                        ,TotalCount       = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,R0.Count       = sum(R0)
                        ,R0.sd            = sd(R0)
                        ,R1.R10.Count   = sum(R1.R10)
                        ,R1.R10.sd        = sd(R1.R10)
                        ,R11.R15.Count  = sum(R11.R15)
                        ,R11.R15.sd       = sd(R11.R15)
                        ,R16.R20.Count  = sum(R16.R20)
                        ,R16.R20.sd       = sd(R16.R20)
                        ,R21.R25.Count  = sum(R21.R25)
                        ,R21.R25.sd       = sd(R21.R25)
                        ,R26.R30.Count  = sum(R26.R30)
                        ,R26.R30.sd       = sd(R26.R30)
                        ,R31.R40.Count  = sum(R31.R40)
                        ,R31.R40.sd       = sd(R31.R40)
                        ,R41.R50.Count  = 0#sum(R41.R50)
                        ,R41.R50.sd       = 0#sd(R41.R50)
                        ,RGT50.Count    = 0#sum(RGT50)
                        ,RGT50.sd         = 0#sd(RGT50)
)


library(reshape2)
item30.dat1 <- melt(item30.sum, id.vars = c("BuildingType", "TotalCount"))
detach(package:reshape2)

item30.dat1$Ind <- "SampleSize"
item30.dat1$Ind[grep("sd", item30.dat1$variable)] <- "SD"

item30.dat1$rValueBins <- "Blank"
item30.dat1$rValueBins[grep("R0." , item30.dat1$variable)] <- "R0"
item30.dat1$rValueBins[grep("R1." , item30.dat1$variable)] <- "R1-R10"
item30.dat1$rValueBins[grep("R11.", item30.dat1$variable)] <- "R11-R15"
item30.dat1$rValueBins[grep("R16.", item30.dat1$variable)] <- "R16-R20"
item30.dat1$rValueBins[grep("R21.", item30.dat1$variable)] <- "R21-R25"
item30.dat1$rValueBins[grep("R26.", item30.dat1$variable)] <- "R26-R30"
item30.dat1$rValueBins[grep("R31.", item30.dat1$variable)] <- "R31-R40"
item30.dat1$rValueBins[grep("R41.", item30.dat1$variable)] <- "R40-R50"
item30.dat1$rValueBins[grep("RGT50.", item30.dat1$variable)] <- "R50+"

colnames(item30.dat1)

library(data.table)
#now cast by IND
item30.dat.cast2 <- dcast(setDT(item30.dat1),
                          formula   = BuildingType + TotalCount + rValueBins~ Ind, sum,
                          value.var = 'value')
head(item30.dat.cast2)

item30.dat.cast2$Percent <- item30.dat.cast2$SampleSize / item30.dat.cast2$TotalCount
item30.dat.cast2$SE <- item30.dat.cast2$SD / sqrt(item30.dat.cast2$SampleSize)

item30.dat.cast2 <- data.frame(item30.dat.cast2, stringsAsFactors = F)

cols.remove <- which(colnames(item30.dat.cast2) %in% c("SD","TotalCount"))
item30.final <- item30.dat.cast2[,-cols.remove]

item30.total.row <- data.frame("BuildingType" = "Manufactured"
                               ,"rValueBins" = "Total"
                               ,"Percent" = sum(item30.final$Percent)
                               ,"SE" = "NA"
                               ,"SampleSize" = sum(item30.final$SampleSize))
item30.table <- rbind.data.frame(item30.final, item30.total.row, stringsAsFactors = F)

item30.table1 <- data.frame("BuildingType" = item30.table$BuildingType
                            ,"rValueBins" = item30.table$rValueBins
                            ,"Percent" = item30.table$Percent
                            ,"SE" = item30.table$SE
                            ,"SampleSize" = item30.table$SampleSize)









############################################################################################################
## Item 31
############################################################################################################
item31.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Ceiling.Type"
                                                               , "Ceiling.Sub-Type"
                                                               , "Ceiling.Area"
                                                               , "Ceiling.Insulated?"
                                                               , "Ceiling.Insulation.Type.1"
                                                               , "Ceiling.Insulation.Thickness.1"))]
item31.dat1 <- item31.dat[which(item31.dat$Ceiling.Type == "Roof Deck"),]
length(unique(item31.dat1$CK_Cadmus_ID)) #only 18

item31.dat1$`Ceiling.Insulated?`[which(is.na(item31.dat1$`Ceiling.Insulated?`))] <- "No"

