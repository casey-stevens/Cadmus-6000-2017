#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#read in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-24,-3]





#############################################################################################
#Item 61: PERCENTAGE OF HOMES WITH DUCT SYSTEMS BY STATE (SF table 66)
#############################################################################################
#subset to columns needed for analysis
item61.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"System.Type"
                                                                   ,"Duct.Insulation.Condition"
                                                                   ,"Duct.Plenum.Insulation.Thickness.1"
                                                                   ,"Duct.Plenum.Insulation.Thickness.2"
                                                                   ,"Duct.Plenum.Insulation.Type.1"
                                                                   ,"Duct.Plenum.Insulation.Type.2"
                                                                   ,"Duct.Runs.Insulation.Type.1"
                                                                   ,"Duct.Runs.Insulation.Type.2"))]
item61.dat$count <- 1

#############################################################################################
# Data selection and cleaning
#############################################################################################

#remove any repeat header rows 
item61.dat00 <- item61.dat[which(item61.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
#subset to only ducting
item61.dat01 <- item61.dat00[which(item61.dat00$Generic == "Ducting"),]

#join ducting data onto RBSA cleaned data
item61.dat0 <- left_join(item61.dat01, rbsa.dat, by = "CK_Cadmus_ID")

#remove datapoint not asked for
item61.dat1 <- item61.dat0[which(item61.dat0$Duct.Insulation.Condition != "-- Datapoint not asked for --"),]

#remove Unknown
item61.dat2 <- item61.dat1[which(item61.dat1$Duct.Insulation.Condition != "Unknown"),]

#remove N/A
item61.dat3 <- item61.dat2[which(item61.dat2$Duct.Insulation.Condition != "N/A"),]

#remove datapoint not asked for from insulation type
item61.dat4 <- item61.dat3[which(item61.dat3$Duct.Plenum.Insulation.Type.1 != "-- Datapoint not asked for --"),]

#remove unknown from insulation thickness
item61.dat5 <- item61.dat4[which(item61.dat4$Duct.Plenum.Insulation.Type.1 != "Unknown"),]

#change N/A to None per Rietz
i=6
for (i in 5:13){
item61.dat5[which(item61.dat5[,i] == "N/A"),i] <- "None"
}

#change datapoint not asked for to NA
i=6
for (i in 5:13){
  item61.dat5[which(item61.dat5[,i] == "-- Datapoint not asked for --"),i] <- NA
}


#############################################################################################
# Insulation Analysis
#############################################################################################
###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
item61.dat5$Duct.Plenum.Insulation.Thickness.1[which(item61.dat5$Duct.Plenum.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item61.dat5$Duct.Plenum.Insulation.Thickness.1[which(item61.dat5$Duct.Plenum.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item61.dat5$Duct.Plenum.Insulation.Thickness.1[which(item61.dat5$Duct.Plenum.Insulation.Thickness.1 == "None")] <- "None None"
item61.dat5$Duct.Plenum.Insulation.Thickness.1[which(is.na(item61.dat5$Duct.Plenum.Insulation.Thickness.1))] <- "N/A N/A"
item61.dat5$Duct.Plenum.Insulation.Thickness.2[which(item61.dat5$Duct.Plenum.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item61.dat5$Duct.Plenum.Insulation.Thickness.2[which(item61.dat5$Duct.Plenum.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item61.dat5$Duct.Plenum.Insulation.Thickness.2[which(item61.dat5$Duct.Plenum.Insulation.Thickness.2 == "None")] <- "None None"
item61.dat5$Duct.Plenum.Insulation.Thickness.2[which(is.na(item61.dat5$Duct.Plenum.Insulation.Thickness.2))] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
item61.dat5$count <- 1
item61.dat5$TMP_ID <- cumsum(item61.dat5$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item61.dat5$Duct.Plenum.Insulation.Thickness.1, " "))
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item61.dat5$CK_Cadmus_ID
                                   , "TMP_ID" = item61.dat5$TMP_ID
                                   , as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item61.dat5$Duct.Plenum.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item61.dat5$CK_Cadmus_ID
                                   , "TMP_ID" = item61.dat5$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
item61.dat6 <- as.data.frame(left_join(item61.dat5, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                              , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item61.dat6$inches1 <- as.numeric(as.character(item61.dat6$V1.x)) # warning here is OK
item61.dat6$inches2 <- as.numeric(as.character(item61.dat6$V1.y)) # warning here is OK

item61.dat6$rvalues1 <- item61.dat6$Duct.Plenum.Insulation.Type.1
item61.dat6$rvalues2 <- item61.dat6$Duct.Plenum.Insulation.Type.2

#check uniques
unique(item61.dat6$rvalues1)
unique(item61.dat6$rvalues2)

#fix names that are not in R value table
item61.dat6$rvalues1[which(item61.dat6$rvalues1 == "Foil-faced bubble wrap")] <- "Foil-faced polyiscyanurate foam board"
item61.dat6$rvalues1[which(item61.dat6$rvalues1 == "Foil-faced fiberglass")]  <- "Foil-faced polyiscyanurate foam board"
item61.dat6$rvalues1[which(item61.dat6$rvalues1 == "Foam board")]             <- 5.8
item61.dat6$rvalues2[which(item61.dat6$rvalues2 == "Foil-faced bubble wrap")] <- "Foil-faced polyiscyanurate foam board"
item61.dat6$rvalues2[which(item61.dat6$rvalues2 == "Foil-faced fiberglass")]  <- "Foil-faced polyiscyanurate foam board"
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item61.dat6$rvalues1[which(item61.dat6$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item61.dat6$rvalues2[which(item61.dat6$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item61.dat7 <- item61.dat6
length(unique(item61.dat7$CK_Cadmus_ID)) #115

###########################
# Cleaning step (NA to zero)
###########################
#For now: convert NAs in rvalues and inches to zero for summarizing purposes (will revert back to NAs after)
item61.dat7$rvalues1[which(item61.dat7$rvalues1 == "None")] <- 0
item61.dat7$rvalues2[which(is.na(item61.dat7$rvalues2))] <- 0
item61.dat7$rvalues2[which(item61.dat7$rvalues2 == "None")] <- 0

#QC the clean bulb values
unique(item61.dat7$rvalues1)
unique(item61.dat7$rvalues2)

item61.dat7$inches1[which(is.na(item61.dat7$inches1))] <- 0
item61.dat7$inches2[which(is.na(item61.dat7$inches2))] <- 0

#QC the clean bulb values -- There are NAs in each 
unique(item61.dat7$inches1)
unique(item61.dat7$inches2)
###########################
# End Cleaning step
###########################


# r values multiplied by inches
item61.dat7$total.r.val <- (as.numeric(as.character(item61.dat7$rvalues1)) * as.numeric(as.character(item61.dat7$inches1))) + (as.numeric(as.character(item61.dat7$rvalues2)) * as.numeric(as.character(item61.dat7$inches2)))
unique(item61.dat7$total.r.val)

#caluclate u factors = inverse of Rvalue
item61.dat7$ufactor <- 1 / as.numeric(as.character(item61.dat7$total.r.val))

# replace inf with 0
item61.dat7$ufactor[which(item61.dat7$ufactor == "Inf")] <- 0

item61.sum <- summarise(group_by(item61.dat7, BuildingType, CK_Cadmus_ID)
                        ,aveRval = mean(total.r.val))

# #make area numeric
# item61.dat7$ufactor <- as.numeric(as.character(item61.dat7$ufactor))
# item61.dat7$Area <- as.numeric(as.character(item61.dat7$Area))
# 
# #weight the u factor per home
# weightedU <- summarise(group_by(item61.dat7, CK_Cadmus_ID, )
#                        ,aveUval = sum(Ceiling.Area * as.numeric(as.character(ufactor))) / sum(Ceiling.Area)
# )
# 
# #back-calculate the weight r values
# weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
# weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0
# 
Ceiling.unique <- unique(item61.dat7[which(colnames(item61.dat7) %in% c("CK_Cadmus_ID"
                                                                        , "BuildingType"
                                                                        , "Duct.Plenum.Insulation.Type.1"
                                                                        , "Duct.Runs.Insulation.Type.1"
                                                                        , "Duct.Runs.Insulation.Type.2"))])

item61.dat8 <- left_join(item61.sum, Ceiling.unique, by = c("CK_Cadmus_ID", "BuildingType"))
length(unique(item61.dat8$CK_Cadmus_ID)) #115


############################################################################################################
## This is a process to determine which Rvalues are NA and which are zero.
############################################################################################################
#determine which rvalues were NA and which were 0
item61.dat8$aveRval[which(item61.dat8$aveRval == 0)] <- NA

#get indicators for ceilings that are not insulated and rvalues that are NA
item61.dat8$aveRval[which(item61.dat8$Duct.Plenum.Insulation.Type.1 == "None")] <- 0

############################################################################################################
## END Process
############################################################################################################

item61.dat9 <- left_join(item61.dat8, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))
length(unique(item61.dat9$CK_Cadmus_ID)) #115

unique(item61.dat9$Duct.Runs.Insulation.Type.1)
item61.dat9$Duct.Runs.RValue1 <- ""
item61.dat9$Duct.Runs.RValue1[which(item61.dat9$Duct.Runs.Insulation.Type.1 == "R8")] <- 8
item61.dat9$Duct.Runs.RValue1[which(item61.dat9$Duct.Runs.Insulation.Type.1 == "R6")] <- 6
item61.dat9$Duct.Runs.RValue1[which(item61.dat9$Duct.Runs.Insulation.Type.1 == "R4")] <- 4
item61.dat9$Duct.Runs.RValue1[which(item61.dat9$Duct.Runs.Insulation.Type.1 == "R4.2")] <- 4.2
item61.dat9$Duct.Runs.RValue1[which(item61.dat9$Duct.Runs.Insulation.Type.1 == "None")] <- 0

item61.dat9$Duct.Runs.RValue1[which(item61.dat9$Duct.Runs.Insulation.Type.1 == "Unknown")] <- ""
unique(item61.dat9$Duct.Runs.RValue1)
item61.dat9$Duct.Runs.RValue1 <- as.numeric(as.character(item61.dat9$Duct.Runs.RValue1))


item61.dat9$AverageRValueFinal <- ifelse(is.na(item61.dat9$Duct.Runs.RValue1), item61.dat9$aveRval, (item61.dat9$Duct.Runs.RValue1 + item61.dat9$aveRval) / 2)
item61.dat9$AverageRValueFinal[which(is.na(item61.dat9$AverageRValueFinal))] <- item61.dat9$Duct.Runs.RValue1[which(is.na(item61.dat9$AverageRValueFinal))]


############################################################################################################
## Final R value binning
############################################################################################################

#Bin R values -- SF only
item61.dat9$rvalue.bins <- "Unknown"
item61.dat9$rvalue.bins[which(item61.dat9$AverageRValueFinal == 0)] <- "None"
item61.dat9$rvalue.bins[which(item61.dat9$AverageRValueFinal >  0 & item61.dat9$AverageRValueFinal < 5)]  <- "R1.R4"
item61.dat9$rvalue.bins[which(item61.dat9$AverageRValueFinal >= 5 & item61.dat9$AverageRValueFinal < 8)]  <- "R5.R7"
item61.dat9$rvalue.bins[which(item61.dat9$AverageRValueFinal >= 8 & item61.dat9$AverageRValueFinal < 11)]  <- "R8.R10"
item61.dat9$rvalue.bins[which(item61.dat9$AverageRValueFinal >= 11)] <- "RGT10"
unique(item61.dat9$rvalue.bins)

##cast data
item61.dat9$count <- 1
item61.dat9.cast <- dcast(setDT(item61.dat9),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item61.dat9.cast)

#summarize --SF
item61.sum <- summarise(group_by(item61.dat9.cast, BuildingType)
                        ,TotalCount       = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,None.Count       = sum(None)
                        ,None.sd            = sd(None)
                        ,R1.R4.Count   = sum(R1.R4)
                        ,R1.R4.sd        = sd(R1.R4)
                        ,R5.R7.Count  = sum(R5.R7)
                        ,R5.R7.sd       = sd(R5.R7)
                        ,R8.R10.Count  = sum(R8.R10)
                        ,R8.R10.sd       = sd(R8.R10)
                        ,RGT10.Count    = sum(RGT10)
                        ,RGT10.sd         = sd(RGT10)
)


library(reshape2)
item61.dat10 <- melt(item61.sum, id.vars = c("BuildingType", "TotalCount"))
detach(package:reshape2)

item61.dat10$Ind <- "SampleSize"
item61.dat10$Ind[grep("sd", item61.dat10$variable)] <- "SD"

item61.dat10$rValueBins <- "Blank"
item61.dat10$rValueBins[grep("None" , item61.dat10$variable)] <- "None"
item61.dat10$rValueBins[grep("R1." , item61.dat10$variable)] <- "R1-R4"
item61.dat10$rValueBins[grep("R5.", item61.dat10$variable)] <- "R5-R7"
item61.dat10$rValueBins[grep("R8.", item61.dat10$variable)] <- "R8-R10"
item61.dat10$rValueBins[grep("RGT10.", item61.dat10$variable)] <- "R10+"

colnames(item61.dat10)

library(data.table)
#now cast by IND
item61.dat.cast2 <- dcast(setDT(item61.dat10),
                          formula   = BuildingType + TotalCount + rValueBins ~ Ind, sum,
                          value.var = 'value')
head(item61.dat.cast2)

item61.dat.cast2$Percent <- item61.dat.cast2$SampleSize / item61.dat.cast2$TotalCount
item61.dat.cast2$SE <- item61.dat.cast2$SD / sqrt(item61.dat.cast2$SampleSize)

item61.dat.cast2 <- data.frame(item61.dat.cast2, stringsAsFactors = F)

cols.remove <- which(colnames(item61.dat.cast2) %in% c("SD","TotalCount"))
item61.final <- item61.dat.cast2[,-cols.remove]


item61.final1 <- item61.final[which(item61.final$BuildingType == "Single Family"),]
