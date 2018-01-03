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
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#read in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]





#############################################################################################
#Item 61: PERCENTAGE OF HOMES WITH DUCT SYSTEMS BY Insulation level (SF table 68)
#############################################################################################
#subset to columns needed for analysis
prep.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                 ,"Generic"
                                                                 ,"System.Type"
                                                                 ,"Duct.Type"
                                                                 ,"Duct.Insulation.Condition"
                                                                 ,"Duct.Plenum.Insulation.Thickness.1"
                                                                 ,"Duct.Plenum.Insulation.Thickness.2"
                                                                 ,"Duct.Plenum.Insulation.Thickness.3"
                                                                 ,"Duct.Plenum.Insulation.Type.1"
                                                                 ,"Duct.Plenum.Insulation.Type.2"
                                                                 ,"Duct.Plenum.Insulation.Type.3"
                                                                 ,"Duct.Runs.Insulation.Thickness.1"
                                                                 ,"Duct.Runs.Insulation.Thickness.2"
                                                                 ,"Duct.Runs.Insulation.Thickness.3"
                                                                 ,"Duct.Runs.Insulation.Type.1"
                                                                 ,"Duct.Runs.Insulation.Type.2"
                                                                 ,"Duct.Runs.Insulation.Type.3"))]
prep.data.duct <- prep.dat[which(prep.dat$Generic == "Ducting"),]

unique(prep.data.duct$System.Type)

#clean system type
prep.data.duct$System.Type[grep("Not", prep.data.duct$System.Type)] <- "Not Present"
prep.data.duct$System.Type[grep("Unknown|unknown", prep.data.duct$System.Type)] <- "Unknown"

prep.data.duct1 <- prep.data.duct[which(prep.data.duct$System.Type %notin% c("Unknown")),]
prep.data.duct1 <- prep.data.duct1[which(prep.data.duct1$Duct.Plenum.Insulation.Thickness.1 %notin% c("Unknown","unknown",NA,"NA","N/A") & prep.data.duct1$Duct.Runs.Insulation.Type.1 %notin% c("Unknown","unknown",NA,"NA","N/A")),]


#review types
unique(prep.data.duct1$Duct.Plenum.Insulation.Type.1)
unique(prep.data.duct1$Duct.Plenum.Insulation.Type.2)
unique(prep.data.duct1$Duct.Plenum.Insulation.Type.3) #nothing in this column
unique(prep.data.duct1$Duct.Runs.Insulation.Type.1)
unique(prep.data.duct1$Duct.Runs.Insulation.Type.2)
unique(prep.data.duct1$Duct.Runs.Insulation.Type.3) #nothing in this column

#review insulation thicknesses
unique(prep.data.duct1$Duct.Plenum.Insulation.Thickness.1)
unique(prep.data.duct1$Duct.Plenum.Insulation.Thickness.2)
unique(prep.data.duct1$Duct.Plenum.Insulation.Thickness.3)

#assign new dataset
prep.dat3 <- prep.data.duct1

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################

for(i in 1:ncol(prep.dat3)){
  prep.dat3[,i] <- ifelse(prep.dat3[,i] == "-- Datapoint not asked for --", NA, prep.dat3[,i])
}

#cleaning for wall.cavity
prep.dat3$Duct.Plenum.Insulation.Thickness.1[which(prep.dat3$Duct.Plenum.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
prep.dat3$Duct.Plenum.Insulation.Thickness.1[which(is.na(prep.dat3$Duct.Plenum.Insulation.Thickness.1))] <- "N/A N/A"
prep.dat3$Duct.Plenum.Insulation.Thickness.1[which(prep.dat3$Duct.Plenum.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Duct.Plenum.Insulation.Thickness.2[which(prep.dat3$Duct.Plenum.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Duct.Plenum.Insulation.Thickness.2[which(prep.dat3$Duct.Plenum.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
prep.dat3$Duct.Plenum.Insulation.Thickness.2[which(is.na(prep.dat3$Duct.Plenum.Insulation.Thickness.2))] <- "N/A N/A"
prep.dat3$Duct.Plenum.Insulation.Thickness.3[which(prep.dat3$Duct.Plenum.Insulation.Thickness.3 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Duct.Plenum.Insulation.Thickness.3[which(prep.dat3$Duct.Plenum.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
prep.dat3$Duct.Plenum.Insulation.Thickness.3[which(is.na(prep.dat3$Duct.Plenum.Insulation.Thickness.3))] <- "N/A N/A"
unique(prep.dat3$Duct.Plenum.Insulation.Thickness.1)
unique(prep.dat3$Duct.Plenum.Insulation.Thickness.2)
unique(prep.dat3$Duct.Plenum.Insulation.Thickness.3)
prep.dat3$Duct.Plenum.Insulation.Thickness.1[which(prep.dat3$Duct.Plenum.Insulation.Thickness.1 == "1/2 inch")] <- "0.5 inches"
prep.dat3$Duct.Plenum.Insulation.Thickness.1[which(prep.dat3$Duct.Plenum.Insulation.Thickness.1 == "<1\"")]     <- "1 inch"
prep.dat3$Duct.Plenum.Insulation.Thickness.1[which(prep.dat3$Duct.Plenum.Insulation.Thickness.1 == "3.5")]      <- "3.5 inches"
prep.dat3$Duct.Plenum.Insulation.Thickness.1[which(prep.dat3$Duct.Plenum.Insulation.Thickness.1 == "r30")]      <- "N/A N/A"


#Clean Condition unknown values
prep.dat3$Duct.Insulation.Condition[which(prep.dat3$Duct.Insulation.Condition == "Unknown")] <- "100%"
prep.dat3$Duct.Insulation.Condition[which(is.na(prep.dat3$Duct.Insulation.Condition) & prep.dat3$Duct.Plenum.Insulation.Thickness.1 != "N/A N/A")] <- "100%"
prep.dat3$Duct.Insulation.Condition[which(prep.dat3$`Duct.Plenum.Insulated?` == "No")] <- "0%"
unique(prep.dat3$Duct.Insulation.Condition)

#remove percent signs and make numeric
prep.dat3$Duct.Insulation.Condition <- gsub("%", "", prep.dat3$Duct.Insulation.Condition)
prep.dat3$Duct.Insulation.Condition <- as.numeric(as.character(prep.dat3$Duct.Insulation.Condition))

# add new ID variable for merging -- don't know if we need this
prep.dat3$count <- 1
prep.dat3$TMP_ID <- cumsum(prep.dat3$count)


clean.insul1 <- unlist(strsplit(prep.dat3$Duct.Plenum.Insulation.Thickness.1, " "))
clean.insul1.1 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul1.1)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(prep.dat3$Duct.Plenum.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul3 <- unlist(strsplit(prep.dat3$Duct.Plenum.Insulation.Thickness.3, " "))
clean.insul3.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul3, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul3.1)


clean.insul.join1 <- left_join(clean.insul1.2,    clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join2 <- left_join(clean.insul.join1, clean.insul3.1, by = c("CK_Cadmus_ID", "TMP_ID"))

colnames(clean.insul.join2) <- c("CK_Cadmus_ID"
                                 ,"TMP_ID"
                                 ,"Duct.Plenum.inches1"
                                 ,"Remove.1"
                                 ,"Duct.Plenum.inches2"
                                 ,"Remove.2"
                                 ,"Duct.Plenum.inches3"
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
prep.dat4$Duct.Plenum.inches1   <- as.numeric(as.character(prep.dat4$Duct.Plenum.inches1)) # warning here is OK
prep.dat4$Duct.Plenum.inches2   <- as.numeric(as.character(prep.dat4$Duct.Plenum.inches2)) # warning here is OK
prep.dat4$Duct.Plenum.inches3   <- as.numeric(as.character(prep.dat4$Duct.Plenum.inches3)) # warning here is OK

#replace any inches that are NA with zeros
for(i in grep("inches", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}

#update column names
prep.dat4$Duct.Plenum.rvalues1 <- prep.dat4$Duct.Plenum.Insulation.Type.1
prep.dat4$Duct.Plenum.rvalues2 <- prep.dat4$Duct.Plenum.Insulation.Type.2
prep.dat4$Duct.Plenum.rvalues3 <- prep.dat4$Duct.Plenum.Insulation.Type.3
prep.dat4$Duct.Runs.rvalues1 <- prep.dat4$Duct.Runs.Insulation.Type.1
prep.dat4$Duct.Runs.rvalues2 <- prep.dat4$Duct.Runs.Insulation.Type.2
prep.dat4$Duct.Runs.rvalues3 <- prep.dat4$Duct.Runs.Insulation.Type.3

#fix names that are not in R value table
prep.dat4$Duct.Plenum.rvalues1[grep("Foil|foil", prep.dat4$Duct.Plenum.rvalues1)]  <- "Foil-faced polyurethane foam board"
prep.dat4$Duct.Plenum.rvalues1[grep("Foam board", prep.dat4$Duct.Plenum.rvalues1)] <- "Extruded polystyrene foam board"
prep.dat4$Duct.Plenum.rvalues1[grep("N/A|None", prep.dat4$Duct.Plenum.rvalues1)] <- NA
prep.dat4$Duct.Plenum.rvalues2[grep("Foil|foil", prep.dat4$Duct.Plenum.rvalues2)]  <- "Foil-faced polyurethane foam board"
prep.dat4$Duct.Plenum.rvalues2[grep("N/A|None", prep.dat4$Duct.Plenum.rvalues2)] <- NA
prep.dat4$Duct.Plenum.rvalues3[grep("Foil|foil", prep.dat4$Duct.Plenum.rvalues3)]  <- "Foil-faced polyurethane foam board"
prep.dat4$Duct.Plenum.rvalues3[grep("N/A|None", prep.dat4$Duct.Plenum.rvalues3)] <- NA

prep.dat4$Duct.Runs.rvalues1 <- as.numeric(as.character(gsub("R", "", prep.dat4$Duct.Runs.rvalues1)))
prep.dat4$Duct.Runs.rvalues2 <- as.numeric(as.character(gsub("R", "", prep.dat4$Duct.Runs.rvalues2)))
prep.dat4$Duct.Runs.rvalues3 <- as.numeric(as.character(gsub("R", "", prep.dat4$Duct.Runs.rvalues3)))

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  prep.dat4$Duct.Plenum.rvalues1[which(prep.dat4$Duct.Plenum.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$Duct.Plenum.rvalues2[which(prep.dat4$Duct.Plenum.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$Duct.Plenum.rvalues3[which(prep.dat4$Duct.Plenum.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$Duct.Runs.rvalues1[which(prep.dat4$Duct.Runs.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$Duct.Runs.rvalues2[which(prep.dat4$Duct.Runs.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$Duct.Runs.rvalues3[which(prep.dat4$Duct.Runs.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################
unique(prep.dat4$Duct.Plenum.rvalues1)
unique(prep.dat4$Duct.Plenum.rvalues2)
unique(prep.dat4$Duct.Plenum.rvalues3)
unique(prep.dat4$Duct.Runs.rvalues1)
unique(prep.dat4$Duct.Runs.rvalues2)
unique(prep.dat4$Duct.Runs.rvalues3)


prep.dat4$Duct.Plenum.rvalues1[which(prep.dat4$System.Type == "Not Present")] <- 0
prep.dat4$Duct.Plenum.rvalues2[which(prep.dat4$System.Type == "Not Present")] <- 0
prep.dat4$Duct.Plenum.rvalues3[which(prep.dat4$System.Type == "Not Present")] <- 0

prep.dat4$Duct.Plenum.inches1[which(prep.dat4$System.Type == "Not Present")] <- 0
prep.dat4$Duct.Plenum.inches2[which(prep.dat4$System.Type == "Not Present")] <- 0
prep.dat4$Duct.Plenum.inches3[which(prep.dat4$System.Type == "Not Present")] <- 0

#replace any inches and rvalues that are NA with zeros
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}


#make all inches and rvalue columns numeric
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- as.numeric(as.character(prep.dat4[,i]))
}


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
unique(prep.dat4.5$Duct.Plenum.rvalues1)
unique(prep.dat4.5$Duct.Plenum.rvalues2)
unique(prep.dat4.5$Duct.Plenum.rvalues3)

# clean up condition information
prep.dat4.5$Duct.Insulation.Condition <- as.numeric(as.character(prep.dat4.5$Duct.Insulation.Condition)) / 100
prep.dat4.5$Duct.Insulation.Condition[which(is.na(prep.dat4.5$Duct.Insulation.Condition))] <- 1

prep.condition.sub1 <- prep.dat4.5[which(prep.dat4.5$Duct.Insulation.Condition %notin% c(1, NA)),]
prep.condition.sub1$Duct.Insulation.Condition <- 1 - prep.condition.sub1$Duct.Insulation.Condition
prep.condition.sub1$total.r.val <- NA

prep.dat5 <- rbind.data.frame(prep.dat4.5
                              ,prep.condition.sub1
                              , stringsAsFactors = F)

unique(prep.dat5$Duct.Insulation.Condition)

###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################


#calculate the weighted r value
na.ind <- which(is.na(prep.dat5$total.r.val))
prep.dat5$total.r.val[na.ind] <- (prep.dat5$Duct.Plenum.rvalues1[na.ind] * prep.dat5$Duct.Plenum.inches1[na.ind]) +  
  (prep.dat5$Duct.Plenum.rvalues2[na.ind] * prep.dat5$Duct.Plenum.inches2[na.ind]) +  
  (prep.dat5$Duct.Plenum.rvalues3[na.ind] * prep.dat5$Duct.Plenum.inches3[na.ind]) + 
  prep.dat5$Duct.Runs.rvalues1[na.ind] + 
  prep.dat5$Duct.Runs.rvalues2[na.ind] + 
  prep.dat5$Duct.Runs.rvalues3[na.ind]

#check -- NOTE -- NONE SHOULD BE NA
unique(prep.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
prep.dat5$uvalue <- 1 / (1 + prep.dat5$total.r.val)
unique(prep.dat5$uvalue)

#make area numeric
prep.dat5$uvalue           <- as.numeric(as.character(prep.dat5$uvalue))
# prep.dat5$Duct.Plenum.Area <- as.numeric(as.character(prep.dat5$Duct.Plenum.Area))

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID)
                       # ,aveUval = sum(Duct.Plenum.Area * Duct.Plenum.Insulation.Condition.1 * uvalue) / sum(Duct.Plenum.Area * Duct.Plenum.Insulation.Condition.1)
                       ,aveUval = mean(uvalue)
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
## Item 61
############################################################################################################
item61.dat <- prep.dat7

#Bin R values -- SF only
item61.dat$rvalue.bins <- "Unknown"
item61.dat$rvalue.bins[which(item61.dat$aveRval == 0)] <- "None"
item61.dat$rvalue.bins[which(item61.dat$aveRval >  0 & item61.dat$aveRval < 5)]  <- "R1.R4"
item61.dat$rvalue.bins[which(item61.dat$aveRval >= 5 & item61.dat$aveRval < 8)]  <- "R5.R7"
item61.dat$rvalue.bins[which(item61.dat$aveRval >= 8 & item61.dat$aveRval < 11)]  <- "R8.R10"
item61.dat$rvalue.bins[which(item61.dat$aveRval >= 11)] <- "RGT10"
unique(item61.dat$rvalue.bins)

item61.dat$count <- 1


################################################
# Adding pop and sample sizes for weights
################################################
item61.data <- weightedData(item61.dat[-which(colnames(item61.dat) %in% c("System.Type"
                                                                          ,"aveUval"
                                                                          ,"aveRval"
                                                                          ,"rvalue.bins"
                                                                          ,"count"))])
item61.data <- left_join(item61.data, item61.dat[which(colnames(item61.dat) %in% c("CK_Cadmus_ID"
                                                                                    ,"System.Type"
                                                                                    ,"aveUval"
                                                                                    ,"aveRval"
                                                                                    ,"rvalue.bins"
                                                                                    ,"count"))])

#######################
# Weighted Analysis
#######################
item61.final <- proportions_one_group(CustomerLevelData = item61.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,columnName       = "Duct Insulation Level"
                                      ,weighted         = TRUE)
item61.final.SF <- item61.final[which(item61.final$BuildingType == "Single Family"), -which(colnames(item61.final) %in% c("BuildingType", "Duct.Insulation.Level"))]
exportTable(item61.final.SF, "SF", "Table 68", TRUE)

#######################
# Unweighted Analysis
#######################
item61.final <- proportions_one_group(CustomerLevelData = item61.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,columnName       = "Duct Insulation Level"
                                      ,weighted         = FALSE)
item61.final.SF <- item61.final[which(item61.final$BuildingType == "Single Family"), -which(colnames(item61.final) %in% c("BuildingType", "Duct.Insulation.Level", "Total.Count"))]
exportTable(item61.final.SF, "SF", "Table 68", FALSE)

