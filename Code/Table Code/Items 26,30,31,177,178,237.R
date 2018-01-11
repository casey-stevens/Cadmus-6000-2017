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
                                                             ,"PK_Envelope_ID"
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
prep.dat1.0 <- prep.dat0[which(!(is.na(as.numeric(as.character(prep.dat0$Ceiling.Area))))),]
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
prep.dat3$Ceiling.Insulation.Thickness.1[which(prep.dat3$Ceiling.Insulation.Thickness.1 == "12")] <- "12 inches"
prep.dat3$Ceiling.Insulation.Thickness.1[which(prep.dat3$Ceiling.Insulation.Thickness.1 == "5.5")] <- "5.5 inches"
prep.dat3$Ceiling.Insulation.Thickness.1[which(prep.dat3$Ceiling.Insulation.Thickness.1 == "20 or more inches")] <- "20 inches"
prep.dat3$Ceiling.Insulation.Thickness.2[which(prep.dat3$Ceiling.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Ceiling.Insulation.Thickness.2[which(prep.dat3$Ceiling.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
prep.dat3$Ceiling.Insulation.Thickness.2[which(is.na(prep.dat3$Ceiling.Insulation.Thickness.2))] <- "N/A N/A"
prep.dat3$Ceiling.Insulation.Thickness.2[which(prep.dat3$Ceiling.Insulation.Thickness.2 == "20 or more inches")] <- "20 inches"
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
unique(prep.dat4.5$ceiling.inches1)
unique(prep.dat4.5$ceiling.inches2)
unique(prep.dat4.5$ceiling.inches3)

## Clean condition values
prep.dat4.5$Ceiling.Insulation.Condition.1   <- as.character(prep.dat4.5$Ceiling.Insulation.Condition.1)
prep.dat4.5$Ceiling.Insulation.Condition.1[which(is.na(prep.dat4.5$Ceiling.Insulation.Condition.1))] <- "NA"
unique(prep.dat4.5$Ceiling.Insulation.Condition.1)

for(ii in 1:nrow(prep.dat4.5)){
  if(prep.dat4.5$Ceiling.Insulation.Condition.1[ii] %notin% c("1","0.75", "0.9")){
    prep.dat4.5$Ceiling.Insulation.Condition.1[ii] <- as.numeric(prep.dat4.5$Ceiling.Insulation.Condition.1)[ii] / 100
  }
}

prep.dat4.5$Ceiling.Insulation.Condition.1 <- as.numeric(as.character(prep.dat4.5$Ceiling.Insulation.Condition.1))

prep.condition.sub1 <- prep.dat4.5[which(prep.dat4.5$Ceiling.Insulation.Condition.1 %notin% c(1, NA, 0)),]
prep.condition.sub1$Ceiling.Insulation.Condition.1 <- 1 - prep.condition.sub1$Ceiling.Insulation.Condition.1
prep.condition.sub1$ceiling.rvalues1 <- 0
prep.condition.sub1$ceiling.rvalues2 <- 0
prep.condition.sub1$ceiling.rvalues3 <- 0
prep.condition.sub1$total.r.val <- NA

prep.dat5 <- rbind.data.frame(prep.dat4.5
                              ,prep.condition.sub1
                              , stringsAsFactors = F)

prep.dat5$Ceiling.Insulation.Condition.1[which(is.na(prep.dat5$Ceiling.Insulation.Condition.1))] <- 1 
prep.dat5 <- prep.dat5[which(prep.dat5$CK_Cadmus_ID != "BUILDING"),]

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
prep.dat5$uvalue <- 1 / (prep.dat5$total.r.val)
prep.dat5$uvalue[which(prep.dat5$uvalue == "Inf")] <- 1
unique(prep.dat5$uvalue)

#make area numeric
prep.dat5$uvalue       <- as.numeric(as.character(prep.dat5$uvalue))
prep.dat5$Ceiling.Area <- as.numeric(as.character(prep.dat5$Ceiling.Area))

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID, Ceiling.Type)
                       ,aveUval = sum(Ceiling.Area * Ceiling.Insulation.Condition.1 * uvalue) / sum(Ceiling.Area * Ceiling.Insulation.Condition.1)
)

#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
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






rbsa.ceiling <- rbsa.dat[which(colnames(rbsa.dat) %in% c("CK_Cadmus_ID","BuildingType","HomeYearBuilt"))]
ceiling.merge <- left_join(rbsa.ceiling, prep.dat5)
#########export rvalues
##  Write out confidence/precision info
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
write.xlsx(ceiling.merge, paste(filepathCleaningDocs, "Insulation Exports", paste("Ceiling Insulation Values ", rundate, ".xlsx", sep = ""), sep="/"),
           append = T, row.names = F, showNA = F)














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

##############################
# Weighted Analysis
##############################
item26.final <- proportions_one_group(CustomerLevelData = item26.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = TRUE)
item26.final.SF <- item26.final[which(item26.final$BuildingType == "Single Family")
                                ,-which(colnames(item26.final) %in% c("BuildingType"))]
exportTable(item26.final.SF, "SF", "Table 33", weighted = TRUE)

##############################
# Unweighted Analysis
##############################
item26.final <- proportions_one_group(CustomerLevelData = item26.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = FALSE)
item26.final.SF <- item26.final[which(item26.final$BuildingType == "Single Family")
                                ,-which(colnames(item26.final) %in% c("BuildingType"))]
exportTable(item26.final.SF, "SF", "Table 33", weighted = FALSE)




######## Weighted
tableJJ.attic <- mean_one_group(CustomerLevelData = item26.data
                                ,valueVariable = "aveRval"
                                ,byVariable = "State"
                                ,aggregateRow = "Region")
tableJJ.attic.SF <- tableJJ.attic[which(tableJJ.attic$BuildingType == "Single Family")
                                  , which(colnames(tableJJ.attic) != "BuildingType")]

######## Unweighted
tableJJ.attic.unw <- mean_one_group_unweighted(CustomerLevelData = item26.data
                                ,valueVariable = "aveRval"
                                ,byVariable = "State"
                                ,aggregateRow = "Region")
tableJJ.attic.unw.SF <- tableJJ.attic.unw[which(tableJJ.attic.unw$BuildingType == "Single Family")
                                          , which(colnames(tableJJ.attic.unw) != "BuildingType")]







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
item30.data$rvalue.bins[which(item30.data$aveRval >  0 & item30.data$aveRval < 16)]   <- "R1.R15"
item30.data$rvalue.bins[which(item30.data$aveRval >= 16 & item30.data$aveRval < 21)]  <- "R16.R20"
item30.data$rvalue.bins[which(item30.data$aveRval >= 21 & item30.data$aveRval < 26)]  <- "R21.R25"
item30.data$rvalue.bins[which(item30.data$aveRval >= 26 & item30.data$aveRval < 31)]  <- "R26.R30"
item30.data$rvalue.bins[which(item30.data$aveRval >= 31 & item30.data$aveRval < 41)]  <- "R31.R40"
item30.data$rvalue.bins[which(item30.data$aveRval >= 41)]  <- "R41.R50"
unique(item30.data$rvalue.bins)

item30.data$count <- 1



##############################
# Weighted Analysis
##############################
item30.final <- proportions_one_group(CustomerLevelData = item30.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = TRUE)
item30.final.SF <- item30.final[which(item30.final$BuildingType == "Single Family")
                                ,-which(colnames(item30.final) %in% c("BuildingType"))]
exportTable(item30.final.SF, "SF", "Table 37", weighted = TRUE)

##############################
# Unweighted Analysis
##############################
item30.final <- proportions_one_group(CustomerLevelData = item30.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = FALSE)


item30.final.SF <- item30.final[which(item30.final$BuildingType == "Single Family")
                                ,-which(colnames(item30.final) %in% c("BuildingType"))]
exportTable(item30.final.SF, "SF", "Table 37", weighted = FALSE)







############################################################################################################
## Item 31
############################################################################################################
item31.dat <- prep.dat5[which(prep.dat5$Ceiling.Type == "Roof Deck"),]
item31.dat$count <- 1

item31.dat0 <- item31.dat[which(item31.dat$Ceiling.Insulation.Thickness.1 != "N/A N/A"),]

item31.dat1 <- left_join(rbsa.dat, item31.dat0)
item31.dat2 <- item31.dat1[which(!is.na(item31.dat1$uvalue)),]

item31.data <- weightedData(item31.dat2[-c(grep("Ceiling|ceiling", colnames(item31.dat2))
                                           ,which(colnames(item31.dat2) %in% c("Category"
                                                                               ,"count"
                                                                               ,"uvalue"
                                                                               ,"total.r.val"
                                                                               ,"TMP_ID")))])
item31.data <- left_join(item31.data,item31.dat2[c(grep("Ceiling|ceiling", colnames(item31.dat2))
                                                   ,which(colnames(item31.dat2) %in% c("CK_Cadmus_ID"
                                                                                       ,"Category"
                                                                                       ,"count"
                                                                                       ,"uvalue"
                                                                                       ,"total.r.val"
                                                                                       ,"TMP_ID")))] )

##############################
# Weighted Analysis
##############################
item31.final <- proportions_one_group(CustomerLevelData = item31.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Ceiling.Insulation.Thickness.1'
                                      ,total.name       = "Total"
                                      ,weighted         = TRUE)
item31.final.SF <- item31.final[which(item31.final$BuildingType == "Single Family")
                                , -which(colnames(item31.final) %in% c("BuildingType"))]
exportTable(item31.final.SF, "SF", "Table 38", weighted = TRUE)

##############################
# Unweighted Analysis
##############################
item31.final <- proportions_one_group(CustomerLevelData = item31.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Ceiling.Insulation.Thickness.1'
                                      ,total.name       = "Total"
                                      ,weighted         = FALSE)
item31.final.SF <- item31.final[which(item31.final$BuildingType == "Single Family")
                                , -which(colnames(item31.final) %in% c("BuildingType"))]
exportTable(item31.final.SF, "SF", "Table 38", weighted = FALSE)
















#############################################################################################
#Item 177: DISTRIBUTION OF CEILING INSULATION (MH TABLE 20)
#############################################################################################
item177.dat <- prep.dat7[which(prep.dat7$BuildingType == "Manufactured"),]

#Bin R values -- SF only
item177.dat$rvalue.bins <- "Unknown"
item177.dat$rvalue.bins[which(item177.dat$aveRval >= 0  & item177.dat$aveRval < 9) ]  <- "R0.R8"
item177.dat$rvalue.bins[which(item177.dat$aveRval >= 9  & item177.dat$aveRval < 15)]  <- "R9.R14"
item177.dat$rvalue.bins[which(item177.dat$aveRval >= 15 & item177.dat$aveRval < 22)]  <- "R15.R21"
item177.dat$rvalue.bins[which(item177.dat$aveRval >= 22 & item177.dat$aveRval < 31)]  <- "R22.R30"
item177.dat$rvalue.bins[which(item177.dat$aveRval >= 31)]  <- "R31.R40"
unique(item177.dat$rvalue.bins)

item177.merge <- left_join(rbsa.dat, item177.dat)
item177.merge <- item177.merge[which(!is.na(item177.merge$rvalue.bins)),]
item177.merge <- item177.merge[which(!is.na(item177.merge$HomeYearBuilt_bins2)),]


##########################################
# add pop and sample sizes by strata
##########################################
item177.data <- weightedData(item177.merge[-which(colnames(item177.merge) %in% c("Ceiling.Type"
                                                                                 ,"aveUval"
                                                                                 ,"aveRval"
                                                                                 ,"rvalue.bins"))])
item177.data <- left_join(item177.data, item177.merge[which(colnames(item177.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ceiling.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"rvalue.bins"))])
item177.data$count <- 1
##############################
# Weighted Analysis
##############################
item177.summary <- proportionRowsAndColumns1(CustomerLevelData     = item177.data
                                             , valueVariable       = 'count'
                                             , columnVariable      = 'HomeYearBuilt_bins2'
                                             , rowVariable         = 'rvalue.bins'
                                             , aggregateColumnName = "All Housing Vintages"
)
item177.summary <- item177.summary[which(item177.summary$rvalue.bins != "Total"),]
item177.summary <- item177.summary[which(item177.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]

## Summary only for "All Vintages"
item177.all.vintages <- proportions_one_group(item177.data
                                              ,valueVariable    = "count"
                                              ,groupingVariable = "rvalue.bins"
                                              ,total.name       = "All Housing Vintages"
                                              ,columnName       = "HomeYearBuilt_bins2"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
item177.all.vintages$rvalue.bins[which(item177.all.vintages$rvalue.bins == "Total")] <- "All Ceilings"

## Summary for only "All Ceilings"
item177.all.ceilings <-  proportions_one_group(item177.data
                                               ,valueVariable    = "count"
                                               ,groupingVariable = "HomeYearBuilt_bins2"
                                               ,total.name       = "All Ceilings"
                                               ,columnName       = "rvalue.bins"
                                               ,weighted = TRUE
                                               ,two.prop.total = TRUE)
item177.all.ceilings <-  item177.all.ceilings[which(item177.all.ceilings$HomeYearBuilt_bins2 != "Total"),]


#merge together!
item177.final <- rbind.data.frame(item177.summary
                                  , item177.all.ceilings
                                  , item177.all.vintages
                                  , stringsAsFactors = F)

item177.cast <- dcast(setDT(item177.final),
                      formula   = BuildingType +  HomeYearBuilt_bins2 ~ rvalue.bins,
                      value.var = c("w.percent", "w.SE", "count", "n", "N"))

item177.table <- data.frame("BuildingType"          = item177.cast$BuildingType
                            ,"Housing.Vintage"      = item177.cast$HomeYearBuilt_bins2
                            ,"Percent.R0.R8"        = item177.cast$w.percent_R0.R8
                            ,"SE.R0.R8"             = item177.cast$w.SE_R0.R8
                            # ,"n.R0.R8"              = item177.cast$n_R0.R8
                            # ,"Count.R0.R8"          = item177.cast$count_R0.R8
                            ,"Percent.R9.R14"       = item177.cast$w.percent_R9.R14
                            ,"SE.R9.R14"            = item177.cast$w.SE_R9.R14
                            # ,"n.R9.R14"             = item177.cast$n_R9.R14
                            # ,"Count.R9.R14"          = item177.cast$count_R9.R14
                            ,"Percent.R15.R21"      = item177.cast$w.percent_R15.R21
                            ,"SE.R15.R21"           = item177.cast$w.SE_R15.R21
                            # ,"n.R15.R21"            = item177.cast$n_R15.R21
                            # ,"Count.R15.R21"          = item177.cast$count_R15.R21
                            ,"Percent.R22.R30"      = item177.cast$w.percent_R22.R30
                            ,"SE.R22.R30"           = item177.cast$w.SE_R22.R30
                            # ,"n.R22.R30"            = item177.cast$n_R22.R30
                            # ,"Count.R22.R30"          = item177.cast$count_R22.R30
                            ,"Percent.R31.R40"      = item177.cast$w.percent_R31.R40
                            ,"SE.R31.R40"           = item177.cast$w.SE_R31.R40
                            # ,"n.R31.R40"            = item177.cast$n_R31.R40
                            # ,"Count.R31.R40"          = item177.cast$count_R31.R40
                            ,"Percent.All.Ceilings" = item177.cast$`w.percent_All Ceilings`
                            ,"SE.All.Ceilings"      = item177.cast$`w.SE_All Ceilings`
                            # ,"SampleSize"           = item177.cast$`n_All Ceilings`
                            # ,"Count.All.Ceilings"   = item177.cast$`count_All Ceilings`
                            ,"n" = item177.cast$`n_All Ceilings`
                            )

levels(item177.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item177.table <- item177.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item177.table <- data.frame(item177.table)

item177.table.MH <- item177.table[which(item177.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item177.table.MH, "MH", "Table 20", weighted = TRUE)

############################################################################################################
# Unweighted - Manufactured
############################################################################################################
item177.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item177.data
                                                     , valueVariable       = 'count'
                                                     , columnVariable      = 'HomeYearBuilt_bins2'
                                                     , rowVariable         = 'rvalue.bins'
                                                     , aggregateColumnName = "All Housing Vintages"
)
item177.summary <- item177.summary[which(item177.summary$rvalue.bins != "Total"),]
item177.summary <- item177.summary[which(item177.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]

## Summary only for "All Vintages"
item177.all.vintages <- proportions_one_group(item177.data
                                              ,valueVariable    = "count"
                                              ,groupingVariable = "rvalue.bins"
                                              ,total.name       = "All Housing Vintages"
                                              ,columnName       = "HomeYearBuilt_bins2"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item177.all.vintages$rvalue.bins[which(item177.all.vintages$rvalue.bins == "Total")] <- "All Ceilings"

## Summary for only "All Ceilings"
item177.all.ceilings <-  proportions_one_group(item177.data
                                               ,valueVariable    = "count"
                                               ,groupingVariable = "HomeYearBuilt_bins2"
                                               ,total.name       = "All Ceilings"
                                               ,columnName       = "rvalue.bins"
                                               ,weighted = FALSE
                                               ,two.prop.total = TRUE)
item177.all.ceilings <-  item177.all.ceilings[which(item177.all.ceilings$HomeYearBuilt_bins2 != "Total"),]


#merge together!
item177.final <- rbind.data.frame(item177.summary
                                  , item177.all.ceilings
                                  , item177.all.vintages
                                  , stringsAsFactors = F)

item177.cast <- dcast(setDT(item177.final),
                      formula   = BuildingType +  HomeYearBuilt_bins2 ~ rvalue.bins,
                      value.var = c("Percent", "SE", "Count", "n"))

item177.table <- data.frame("BuildingType"          = item177.cast$BuildingType
                            ,"Housing.Vintage"      = item177.cast$HomeYearBuilt_bins2
                            ,"Percent.R0.R8"        = item177.cast$Percent_R0.R8
                            ,"SE.R0.R8"             = item177.cast$SE_R0.R8
                            # ,"n.R0.R8"              = item177.cast$n_R0.R8
                            # ,"Count.R0.R8"          = item177.cast$Count_R0.R8
                            ,"Percent.R9.R14"       = item177.cast$Percent_R9.R14
                            ,"SE.R9.R14"            = item177.cast$SE_R9.R14
                            # ,"n.R9.R14"             = item177.cast$n_R9.R14
                            # ,"Count.R9.R14"          = item177.cast$Count_R9.R14
                            ,"Percent.R15.R21"      = item177.cast$Percent_R15.R21
                            ,"SE.R15.R21"           = item177.cast$SE_R15.R21
                            # ,"n.R15.R21"            = item177.cast$n_R15.R21
                            # ,"Count.R15.R21"          = item177.cast$Count_R15.R21
                            ,"Percent.R22.R30"      = item177.cast$Percent_R22.R30
                            ,"SE.R22.R30"           = item177.cast$SE_R22.R30
                            # ,"n.R22.R30"            = item177.cast$n_R22.R30
                            # ,"Count.R22.R30"          = item177.cast$Count_R22.R30
                            ,"Percent.R31.R40"      = item177.cast$Percent_R31.R40
                            ,"SE.R31.R40"           = item177.cast$SE_R31.R40
                            # ,"n.R31.R40"            = item177.cast$n_R31.R40
                            # ,"Count.R31.R40"          = item177.cast$Count_R31.R40
                            ,"Percent.All.Ceilings" = item177.cast$`Percent_All Ceilings`
                            ,"SE.All.Ceilings"      = item177.cast$`SE_All Ceilings`
                            # ,"SampleSize"           = item177.cast$`n_All Ceilings`
                            # ,"Count.All.Ceilings"   = item177.cast$`Count_All Ceilings`
                            ,"n" = item177.cast$`n_All Ceilings`
                            )

levels(item177.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item177.table <- item177.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item177.table <- data.frame(item177.table)

item177.table.MH <- item177.table[which(item177.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item177.table.MH, "MH", "Table 20", weighted = FALSE)







#############################################################################################
#Item 178: DISTRIBUTION OF CEILING U-VALUE BY STATE (MH TABLE 21)
#############################################################################################
item178.dat <- prep.dat7[which(prep.dat7$BuildingType == "Manufactured"),]

#merge weighted u values onto cleaned RBSA data
item178.merge <- left_join(rbsa.dat, item178.dat)
item178.merge <- item178.merge[which(!(is.na(item178.merge$aveUval))),]

################################################
# Adding pop and sample sizes for weights
################################################
item178.data <- weightedData(item178.merge[-which(colnames(item178.merge) %in% c("Ceiling.Type"
                                                                                 ,"aveUval"
                                                                                 ,"aveRval"))])
item178.data <- left_join(item178.data, item178.merge[which(colnames(item178.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ceiling.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"))])

#######################
# Weighted Analysis
#######################
item178.final <- mean_one_group(item178.data
                                ,valueVariable = 'aveUval'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item178.final.MH <- item178.final[which(item178.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item178.final) %in% c("BuildingType"))]

exportTable(item178.final.MH, "MH", "Table 21", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item178.final <- mean_one_group_unweighted(item178.data
                                           ,valueVariable = 'aveUval'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item178.final.MH <- item178.final[which(item178.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item178.final) %in% c("BuildingType"))]

exportTable(item178.final.MH, "MH", "Table 21", weighted = FALSE)


















#############################################################################################
#Item 237: DISTRIBUTION OF CEILING INSULATION BY CEILING TYPE (MF TABLE 29)
#############################################################################################

