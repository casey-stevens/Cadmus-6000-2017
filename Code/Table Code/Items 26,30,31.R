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


##  Create "Not In" operator
"%notin%" <- Negate("%in%")


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
#
# PREP FOR ITEMS 26, 30, 31
#
#############################################################################################
#subset envelope data to necessary columns
prep.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                , "Category"
                                                                , "Ceiling.Type"
                                                                , "Ceiling.Sub-Type"
                                                                , "Ceiling.Area"
                                                                , "Ceiling.Insulated?"
                                                                , "Ceiling.Insulation.Type.1"
                                                                , "Ceiling.Insulation.Thickness.1"
                                                                , "Ceiling.Insulation.Condition.1"
                                                                , "Ceiling.Insulation.Type.2"                                                  
                                                                , "Ceiling.Insulation.Thickness.2"
                                                                , "Ceiling.Insulation.Condition.2"
                                                                , "Ceiling.Insulation.Type.3"
                                                                , "Ceiling.Insulation.Thickness.3"
                                                                , "Ceiling.Insulation.Condition.3"))]
prep.dat0 <- prep.dat[which(prep.dat$`Ceiling.Insulated?` %in% c("Yes", "No", "-- Datapoint not asked for --")),]
prep.dat1.0 <- prep.dat0[which(!(is.na(prep.dat0$Ceiling.Area))),]
prep.dat1.2 <- prep.dat1.0[which(prep.dat1.0$Ceiling.Insulation.Thickness.1 != "Unknown"),]

#review types
unique(prep.dat1.2$Ceiling.Insulation.Type.1)
unique(prep.dat1.2$Ceiling.Insulation.Type.2)
unique(prep.dat1.2$Ceiling.Insulation.Type.3) #nothing in this column

#review insulation thicknesses
unique(prep.dat1.2$Ceiling.Insulation.Thickness.1)
unique(prep.dat1.2$Ceiling.Insulation.Thickness.2)
unique(prep.dat1.2$Ceiling.Insulation.Thickness.3)

#review conditions
unique(prep.dat1.2$Ceiling.Insulation.Condition.1)
unique(prep.dat1.2$Ceiling.Insulation.Condition.2)
unique(prep.dat1.2$Ceiling.Insulation.Condition.3)

#assign new dataset
prep.dat3 <- prep.dat1.2

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################

for(i in 1:ncol(prep.dat3)){
  prep.dat3[,i] <- ifelse(prep.dat3[,i] == "-- Datapoint not asked for --", NA, prep.dat3[,i])
}

#cleaning for wall.cavity
prep.dat3$Ceiling.Insulation.Thickness.1[which(prep.dat3$Ceiling.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
prep.dat3$Ceiling.Insulation.Thickness.1[which(is.na(prep.dat3$Ceiling.Insulation.Thickness.1))] <- "N/A N/A"
prep.dat3$Ceiling.Insulation.Thickness.2[which(prep.dat3$Ceiling.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Ceiling.Insulation.Thickness.2[which(prep.dat3$Ceiling.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
prep.dat3$Ceiling.Insulation.Thickness.2[which(is.na(prep.dat3$Ceiling.Insulation.Thickness.2))] <- "N/A N/A"
prep.dat3$Ceiling.Insulation.Thickness.3[which(prep.dat3$Ceiling.Insulation.Thickness.3 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Ceiling.Insulation.Thickness.3[which(prep.dat3$Ceiling.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
prep.dat3$Ceiling.Insulation.Thickness.3[which(is.na(prep.dat3$Ceiling.Insulation.Thickness.3))] <- "N/A N/A"
unique(prep.dat3$Ceiling.Insulation.Thickness.1)
unique(prep.dat3$Ceiling.Insulation.Thickness.2)
unique(prep.dat3$Ceiling.Insulation.Thickness.3)

#Clean Condition unknown values
prep.dat3$Ceiling.Insulation.Condition.1[which(prep.dat3$Ceiling.Insulation.Condition.1 == "Unknown")] <- "100%"
prep.dat3$Ceiling.Insulation.Condition.2[which(prep.dat3$Ceiling.Insulation.Condition.2 == "Unknown")] <- "100%"
prep.dat3$Ceiling.Insulation.Condition.3[which(prep.dat3$Ceiling.Insulation.Condition.3 == "Unknown")] <- "100%"

prep.dat3$Ceiling.Insulation.Condition.1[which(is.na(prep.dat3$Ceiling.Insulation.Condition.1) & prep.dat3$Ceiling.Insulation.Thickness.1 != "N/A N/A")] <- "100%"
prep.dat3$Ceiling.Insulation.Condition.2[which(is.na(prep.dat3$Ceiling.Insulation.Condition.2) & prep.dat3$Ceiling.Insulation.Thickness.2 != "N/A N/A")] <- "100%"
prep.dat3$Ceiling.Insulation.Condition.3[which(is.na(prep.dat3$Ceiling.Insulation.Condition.3) & prep.dat3$Ceiling.Insulation.Thickness.3 != "N/A N/A")] <- "100%"

prep.dat3$Ceiling.Insulation.Condition.1[which(prep.dat3$`Ceiling.Insulated?` == "No")] <- "0%"


#remove percent signs and make numeric
prep.dat3$Ceiling.Insulation.Condition.1 <- gsub("%", "", prep.dat3$Ceiling.Insulation.Condition.1)
prep.dat3$Ceiling.Insulation.Condition.1 <- as.numeric(as.character(prep.dat3$Ceiling.Insulation.Condition.1))
prep.dat3$Ceiling.Insulation.Condition.2 <- gsub("%", "", prep.dat3$Ceiling.Insulation.Condition.2)
prep.dat3$Ceiling.Insulation.Condition.2 <- as.numeric(as.character(prep.dat3$Ceiling.Insulation.Condition.2))
prep.dat3$Ceiling.Insulation.Condition.3 <- gsub("%", "", prep.dat3$Ceiling.Insulation.Condition.3)
prep.dat3$Ceiling.Insulation.Condition.3 <- as.numeric(as.character(prep.dat3$Ceiling.Insulation.Condition.3))

# add new ID variable for merging -- don't know if we need this
prep.dat3$count <- 1
prep.dat3$TMP_ID <- cumsum(prep.dat3$count)

prep.dat3$Ceiling.Insulation.Thickness.1[which(prep.dat3$Ceiling.Insulation.Thickness.1 == "20 or more inches")] <- "20 inches"

clean.insul1 <- unlist(strsplit(prep.dat3$Ceiling.Insulation.Thickness.1, " "))
clean.insul1.1 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul1.1)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(prep.dat3$Ceiling.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul3 <- unlist(strsplit(prep.dat3$Ceiling.Insulation.Thickness.3, " "))
clean.insul3.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul3, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul3.1)


clean.insul.join1 <- left_join(clean.insul1.2,    clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join2 <- left_join(clean.insul.join1, clean.insul3.1, by = c("CK_Cadmus_ID", "TMP_ID"))

colnames(clean.insul.join2) <- c("CK_Cadmus_ID"
                                 ,"TMP_ID"
                                 ,"ceiling.inches1"
                                 ,"Remove.1"
                                 ,"ceiling.inches2"
                                 ,"Remove.2"
                                 ,"ceiling.inches3"
                                 ,"Remove.3")

clean.thickness.data <- clean.insul.join2[-grep("Remove", colnames(clean.insul.join2))]

###########################
# End cleaning step
###########################

#make into dataframe
prep.dat4 <- as.data.frame(left_join(prep.dat3, clean.thickness.data, by = c("CK_Cadmus_ID", "TMP_ID"))
                           , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# make numeric
prep.dat4$ceiling.inches1   <- as.numeric(as.character(prep.dat4$ceiling.inches1)) # warning here is OK
prep.dat4$ceiling.inches2   <- as.numeric(as.character(prep.dat4$ceiling.inches2)) # warning here is OK
prep.dat4$ceiling.inches3   <- as.numeric(as.character(prep.dat4$ceiling.inches3)) # warning here is OK

#replace any inches that are NA with zeros
for(i in grep("inches", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}

#update column names
prep.dat4$ceiling.rvalues1 <- prep.dat4$Ceiling.Insulation.Type.1
prep.dat4$ceiling.rvalues2 <- prep.dat4$Ceiling.Insulation.Type.2
prep.dat4$ceiling.rvalues3 <- prep.dat4$Ceiling.Insulation.Type.3

#fix names that are not in R value table
prep.dat4$ceiling.rvalues1[which(prep.dat4$ceiling.rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
prep.dat4$ceiling.rvalues1[which(prep.dat4$ceiling.rvalues1 == "Other")]                                          <- "Unknown"
prep.dat4$ceiling.rvalues2[which(prep.dat4$ceiling.rvalues2 == "Expanded polystyrene foam board (white)")]        <- "Expanded polystyrene foam board"

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  prep.dat4$ceiling.rvalues1[which(prep.dat4$ceiling.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$ceiling.rvalues2[which(prep.dat4$ceiling.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$ceiling.rvalues3[which(prep.dat4$ceiling.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################
unique(prep.dat4$ceiling.rvalues1)
unique(prep.dat4$ceiling.rvalues2)
unique(prep.dat4$ceiling.rvalues3)


prep.dat4$ceiling.rvalues1[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0
prep.dat4$ceiling.rvalues2[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0
prep.dat4$ceiling.rvalues3[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0

prep.dat4$ceiling.inches1[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0
prep.dat4$ceiling.inches2[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0
prep.dat4$ceiling.inches3[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0

#replace any inches and rvalues that are NA with zeros
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}


#make all inches and rvalue columns numeric
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- as.numeric(as.character(prep.dat4[,i]))
}

prep.dat4.5 <- prep.dat4


#create total.r.value column
prep.dat4.5$total.r.val <- NA


#check uniques -- None should be NA
unique(prep.dat4.5$ceiling.rvalues1)
unique(prep.dat4.5$ceiling.rvalues2)
unique(prep.dat4.5$ceiling.rvalues3)

# clean up condition information
prep.condition.sub1 <- prep.dat4.5[which(prep.dat4.5$Ceiling.Insulation.Condition.1 %notin% c(100, NA)),]
prep.condition.sub1$Ceiling.Insulation.Condition.1 <- 100 - prep.condition.sub1$Ceiling.Insulation.Condition.1
prep.condition.sub1$total.r.val <- NA

prep.dat5 <- rbind.data.frame(prep.dat4.5
                              ,prep.condition.sub1
                              , stringsAsFactors = F)

unique(prep.dat5$Ceiling.Insulation.Condition.1)

###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################


#calculate the weighted r value
na.ind <- which(is.na(prep.dat5$total.r.val))
prep.dat5$total.r.val[na.ind] <- (prep.dat5$ceiling.rvalues1[na.ind] * prep.dat5$ceiling.inches1[na.ind]) +  
  (prep.dat5$ceiling.rvalues2[na.ind] * prep.dat5$ceiling.inches2[na.ind]) +  
  (prep.dat5$ceiling.rvalues3[na.ind] * prep.dat5$ceiling.inches3[na.ind]) 

#check -- NOTE -- NONE SHOULD BE NA
unique(prep.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
prep.dat5$uvalue <- 1 / (1 + prep.dat5$total.r.val)
unique(prep.dat5$uvalue)

#make area numeric
prep.dat5$uvalue       <- as.numeric(as.character(prep.dat5$uvalue))
prep.dat5$Ceiling.Area <- as.numeric(as.character(prep.dat5$Ceiling.Area))

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID, Ceiling.Type)
                       ,aveUval = sum(Ceiling.Area * Ceiling.Insulation.Condition.1 * uvalue) / sum(Ceiling.Area * Ceiling.Insulation.Condition.1)
)

#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval))) - 1
unique(weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
wall.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
prep.dat7 <- left_join(prep.dat6, rbsa.dat)
prep.dat7$aveUval[which(is.na(prep.dat7$aveUval))] <- 0
prep.dat7$aveRval[which(is.na(prep.dat7$aveRval))] <- 0
###################################################################################################################
#
#
# End Prep
#
#
###################################################################################################################




















############################################################################################################
## Item 26
############################################################################################################
item26.dat <- prep.dat7[which(prep.dat7$Ceiling.Type == "Attic"),]

item26.data <- weightedData(item26.dat[-which(colnames(item26.dat) %in% c("Ceiling.Type"
                                                                          ,"aveUval"
                                                                          ,"aveRval"))])
item26.data <- left_join(item26.data, item26.dat[which(colnames(item26.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"Ceiling.Type"
                                                                                   ,"aveUval"
                                                                                   ,"aveRval"))])




#Bin R values -- SF only
item26.data$rvalue.bins <- "Unknown"
item26.data$rvalue.bins[which(item26.data$aveRval == 0)] <- "R0"
item26.data$rvalue.bins[which(item26.data$aveRval >  0  & item26.data$aveRval < 11)]  <- "R1.R10"
item26.data$rvalue.bins[which(item26.data$aveRval >= 11 & item26.data$aveRval < 16)]  <- "R11.R15"
item26.data$rvalue.bins[which(item26.data$aveRval >= 16 & item26.data$aveRval < 21)]  <- "R16.R20"
item26.data$rvalue.bins[which(item26.data$aveRval >= 21 & item26.data$aveRval < 26)]  <- "R21.R25"
item26.data$rvalue.bins[which(item26.data$aveRval >= 26 & item26.data$aveRval < 31)]  <- "R26.R30"
item26.data$rvalue.bins[which(item26.data$aveRval >= 31 & item26.data$aveRval < 41)]  <- "R31.R40"
item26.data$rvalue.bins[which(item26.data$aveRval >= 41 & item26.data$aveRval < 51)]  <- "R41.R50"
item26.data$rvalue.bins[which(item26.data$aveRval >= 51)] <- "RGT50"
unique(item26.data$rvalue.bins)

item26.data$count <- 1


item26.final <- proportions_one_group(CustomerLevelData = item26.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,columnName       = "Attic Insulation Levels"
                                      ,weighted         = TRUE)
item26.final.SF <- item26.final[which(item26.final$BuildingType == "Single Family"),-which(colnames(item26.final) %in% c("BuildingType", "Attic.Insulation.Levels"))]
exportTable(item26.final.SF, "SF", "Table 33", weighted = TRUE)

item26.final <- proportions_one_group(CustomerLevelData = item26.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,columnName       = "Attic Insulation Levels"
                                      ,weighted         = FALSE)
item26.final.SF <- item26.final[which(item26.final$BuildingType == "Single Family"),-which(colnames(item26.final) %in% c("BuildingType", "Attic.Insulation.Levels"))]
exportTable(item26.final.SF, "SF", "Table 33", weighted = FALSE)




# #summarise by rvalue bins
# item26.sum1 <- summarise(group_by(item26.dat0, BuildingType, rvalue.bins)
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count))
# 
# item26.sum2 <- summarise(group_by(item26.dat0, BuildingType)
#                          ,rvalue.bins = "Total"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count))
# 
# item26.merge <- rbind.data.frame(item26.sum1,item26.sum2, stringsAsFactors = F)
# 
# item26.tot.count <- item26.sum2[which(colnames(item26.sum2) %in% c("BuildingType"
#                                                                    ,"Count"))]
# colnames(item26.tot.count) <- c("BuildingType","Total.Count")
# 
# item26.final <- left_join(item26.merge, item26.tot.count, by = "BuildingType")
# 
# item26.final$Percent <- item26.final$Count / item26.final$Total.Count
# item26.final$SE <- sqrt(item26.final$Percent * (1 - item26.final$Percent) / item26.final$SampleSize)
# 
# 
# item26.table <- data.frame("BuildingType" = item26.final$BuildingType
#                             ,"R Values" = item26.final$rvalue.bins
#                             ,"Percent" = item26.final$Percent
#                             ,"SE" = item26.final$SE
#                             ,"SampleSize" = item26.final$SampleSize)







############################################################################################################
## Item 30
############################################################################################################
item30.dat <- prep.dat7[which(prep.dat7$Ceiling.Type == "Sloped / Vaulted (no attic)"),]

item30.data <- weightedData(item30.dat[-which(colnames(item30.dat) %in% c("Ceiling.Type"
                                                                          ,"aveUval"
                                                                          ,"aveRval"))])
item30.data <- left_join(item30.data, item30.dat[which(colnames(item30.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"Ceiling.Type"
                                                                                   ,"aveUval"
                                                                                   ,"aveRval"))])


#Bin R values -- SF only
item30.data$rvalue.bins <- "Unknown"
item30.data$rvalue.bins[which(item30.data$aveRval == 0)] <- "R0"
item30.data$rvalue.bins[which(item30.data$aveRval >  0  & item30.data$aveRval < 11)]  <- "R1.R10"
item30.data$rvalue.bins[which(item30.data$aveRval >= 11 & item30.data$aveRval < 16)]  <- "R11.R15"
item30.data$rvalue.bins[which(item30.data$aveRval >= 16 & item30.data$aveRval < 21)]  <- "R16.R20"
item30.data$rvalue.bins[which(item30.data$aveRval >= 21 & item30.data$aveRval < 26)]  <- "R21.R25"
item30.data$rvalue.bins[which(item30.data$aveRval >= 26 & item30.data$aveRval < 31)]  <- "R26.R30"
item30.data$rvalue.bins[which(item30.data$aveRval >= 31 & item30.data$aveRval < 41)]  <- "R31.R40"
item30.data$rvalue.bins[which(item30.data$aveRval >= 41 & item30.data$aveRval < 51)]  <- "R41.R50"
item30.data$rvalue.bins[which(item30.data$aveRval >= 51)] <- "RGT50"
unique(item30.data$rvalue.bins)

item30.data$count <- 1



###################
# Analysis
###################
item30.final <- proportions_one_group(CustomerLevelData = item30.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,columnName       = "Attic Insulation Levels"
                                      ,weighted         = TRUE)
item30.final.SF <- item30.final[which(item30.final$BuildingType == "Single Family"),-which(colnames(item30.final) %in% c("BuildingType", "Attic.Insulation.Levels"))]
exportTable(item30.final.SF, "SF", "Table 37", weighted = TRUE)

item30.final <- proportions_one_group(CustomerLevelData = item30.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,columnName       = "Attic Insulation Levels"
                                      ,weighted         = FALSE)
item30.final.SF <- item30.final[which(item30.final$BuildingType == "Single Family"),-which(colnames(item30.final) %in% c("BuildingType", "Attic.Insulation.Levels"))]
exportTable(item30.final.SF, "SF", "Table 37", weighted = FALSE)


# #summarise by rvalue bins
# item30.sum1 <- summarise(group_by(item30.dat0, BuildingType, rvalue.bins)
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count))
# 
# item30.sum2 <- summarise(group_by(item30.dat0, BuildingType)
#                          ,rvalue.bins = "Total"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count))
# 
# item30.merge <- rbind.data.frame(item30.sum1,item30.sum2, stringsAsFactors = F)
# 
# item30.tot.count <- item30.sum2[which(colnames(item30.sum2) %in% c("BuildingType"
#                                                                    ,"Count"))]
# colnames(item30.tot.count) <- c("BuildingType","Total.Count")
# 
# item30.final <- left_join(item30.merge, item30.tot.count, by = "BuildingType")
# 
# item30.final$Percent <- item30.final$Count / item30.final$Total.Count
# item30.final$SE <- sqrt(item30.final$Percent * (1 - item30.final$Percent) / item30.final$SampleSize)
# 
# 
# item30.table <- data.frame("BuildingType" = item30.final$BuildingType
#                            ,"R Values" = item30.final$rvalue.bins
#                            ,"Percent" = item30.final$Percent
#                            ,"SE" = item30.final$SE
#                            ,"SampleSize" = item30.final$SampleSize)










############################################################################################################
## Item 31
############################################################################################################
item31.dat <- prep.dat5[which(prep.dat5$Ceiling.Type == "Roof Deck"),]
item31.dat$count <- 1

item31.dat1 <- left_join(item31.dat, rbsa.dat)

item31.data <- weightedData(item31.dat1[-c(grep("Ceiling|ceiling", colnames(item31.dat1)),which(colnames(item31.dat1) %in% c("Category"
                                                                          ,"count"
                                                                          ,"uvalue"
                                                                          ,"total.r.val"
                                                                          ,"TMP_ID")))])
item31.data <- left_join(item31.data,item31.dat1[c(grep("Ceiling|ceiling", colnames(item31.dat1)),which(colnames(item31.dat1) %in% c("CK_Cadmus_ID"
                                                                                                                                     ,"Category"
                                                                                                                                     ,"count"
                                                                                                                                     ,"uvalue"
                                                                                                                                     ,"total.r.val"
                                                                                                                                     ,"TMP_ID")))] )

item31.final <- proportions_one_group(CustomerLevelData = item31.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Ceiling.Insulation.Thickness.1'
                                      ,total.name       = "Total"
                                      ,columnName       = "Roof Deck Insulation Levels"
                                      ,weighted         = TRUE)
item31.final.SF <- item31.final[which(item31.final$BuildingType == "Single Family"), -which(colnames(item31.final) %in% c("BuildingType", "Roof.Deck.Insulation.Levels"))]
exportTable(item31.final.SF, "SF", "Table 38", weighted = TRUE)

item31.final <- proportions_one_group(CustomerLevelData = item31.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Ceiling.Insulation.Thickness.1'
                                      ,total.name       = "Total"
                                      ,columnName       = "Roof Deck Insulation Levels"
                                      ,weighted         = FALSE)
item31.final.SF <- item31.final[which(item31.final$BuildingType == "Single Family"), -which(colnames(item31.final) %in% c("BuildingType", "Roof.Deck.Insulation.Levels"))]
exportTable(item31.final.SF, "SF", "Table 38", weighted = FALSE)





# item31.sum1 <- summarise(group_by(item31.dat2, Ceiling.Insulation.Thickness.1)
#                          ,Count = sum(count)
#                          ,SampleSize = length(unique(CK_Cadmus_ID)))
# 
# item31.sum2 <- summarise(group_by(item31.dat2)
#                          ,Ceiling.Insulation.Thickness.1 = "Total"
#                          ,Count = sum(count)
#                          ,SampleSize = length(unique(CK_Cadmus_ID)))
# 
# item31.final <- rbind.data.frame(item31.sum1, item31.sum2, stringsAsFactors = F)
# 
# item31.final$Total.Count <- item31.sum2$Count
# item31.final$Denom.SampleSize <- item31.sum2$SampleSize
# 
# item31.final$Percent <- item31.final$Count / item31.final$Total.Count
# item31.final$SE <- sqrt(item31.final$Percent * (1 - item31.final$Percent) / item31.final$Denom.SampleSize)
# 
# item31.table <- data.frame("Insulation.Level" = item31.final$Ceiling.Insulation.Thickness.1
#                            ,"Percent" = item31.final$Percent
#                            ,"SE" = item31.final$SE
#                            ,"SampleSize" = item31.final$SampleSize)
