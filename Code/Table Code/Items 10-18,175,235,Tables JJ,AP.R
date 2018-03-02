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
rbsa.dat.orig <- read.xlsx(xlsxFile = file.path(filepathCleanData ,paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat <- rbsa.dat.orig[grep("site",rbsa.dat.orig$CK_Building_ID, ignore.case = T),]
rbsa.dat.MF <- rbsa.dat.orig[grep("bldg", rbsa.dat.orig$CK_Building_ID, ignore.case = T),]


#Read in data for analysis
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Envelope.xlsx', envelope.export, mode = 'wb')
envelope.dat <- read.xlsx(envelope.export)
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]

###################################################################################################################
#
#
# This will be used for items 10, 12, 13, 14, 15, and 16
#
#
###################################################################################################################
#subset envelope data to necessary columns
prep.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                             ,"CK_SiteID"
                                                             ,"PK_Envelope_ID"
                                                             , "Category"
                                                             , "Wall.Type"
                                                             , "Wall.Area"
                                                             , "Wall.Framing.Size"
                                                             , "Wall.Cavity.Insulated?"
                                                             , "Wall.Cavity.Insulation.Type.1"
                                                             , "Wall.Cavity.Insulation.Thickness.1"
                                                             , "Wall.Cavity.Insulation.Condition.1"
                                                             , "Wall.Cavity.Insulation.Type.2"                                                  
                                                             , "Wall.Cavity.Insulation.Thickness.2"
                                                             , "Wall.Cavity.Insulation.Condition.2"
                                                             , "Wall.Cavity.Insulation.Type.3"                                                  
                                                             , "Wall.Cavity.Insulation.Thickness.3"
                                                             , "Wall.Cavity.Insulation.Condition.3"
                                                             , "Wall.Exterior.Insulated?"
                                                             , "Wall.Exterior.Insulation.Type.1"
                                                             , "Wall.Exterior.Insulation.Thickness.1"
                                                             , "Wall.Exterior.Insulation.Condition.1"
                                                             , "Wall.Exterior.Insulation.Type.2"                                                  
                                                             , "Wall.Exterior.Insulation.Thickness.2"
                                                             , "Wall.Exterior.Insulation.Condition.2"
                                                             , "Wall.Exterior.Insulation.Type.3"                                                  
                                                             , "Wall.Exterior.Insulation.Thickness.3"
                                                             , "Wall.Exterior.Insulation.Condition.3"
                                                             , "Furred.Wall?"
                                                             , "Furred.Wall.Insulated?"
                                                             , "Furred.Wall.Insulation.Type"                                   
                                                             , "Furred.Wall.Insulation.Thickness" ))]

prep.dat0 <- prep.dat[which(prep.dat$Category %in% c("Wall")),]
prep.dat0$`Wall.Exterior.Insulated?`[which(prep.dat0$`Wall.Exterior.Insulated?` != "Yes" & prep.dat0$Wall.Type %notin% c("Masonry", "Masonry (Basement)"))] <- "No" ###treat anything not Yes as No
prep.dat0$`Furred.Wall.Insulated?`  [which(prep.dat0$`Furred.Wall.Insulated?`   != "Yes" & prep.dat0$Wall.Type %in%    c("Masonry", "Masonry (Basement)"))] <- "No" ###treat anything not Yes as No

prep.dat0$Wall.Area <- as.numeric(as.character(prep.dat0$Wall.Area))
prep.dat1.0 <- prep.dat0[which(!is.na(prep.dat0$Wall.Area)),]

prep.dat1.1 <- prep.dat1.0[which(prep.dat1.0$`Wall.Cavity.Insulated?` %notin% c("Unknown")),]
prep.dat1 <- prep.dat1.1[-which(prep.dat1.1$`Wall.Cavity.Insulated?` == "Yes" & prep.dat1.1$Wall.Cavity.Insulation.Thickness.1 %in% c("Unknown",NA,"N/A")),]
# prep.dat1 <- prep.dat1.2[which(prep.dat1.2$Furred.Wall.Insulation.Thickness  %notin% c("Unknown",NA,"N/A")),]


#remove unneccesary wall types
prep.dat2 <- prep.dat1#[which(prep.dat1$Wall.Type %notin% c("Adiabatic", "Knee Wall")),] #"Masonry","Masonry (Basement)","ICF","Other","SIP","Log"
unique(prep.dat2$Wall.Type)

#create "Alternative" category
prep.dat2$Wall.Type[grep("alternative|other",prep.dat2$Wall.Type,ignore.case = T)] <- "Alternative"
length(unique(prep.dat2$CK_Cadmus_ID))
unique(prep.dat2$Wall.Type)

#assign new dataset
prep.dat3 <- prep.dat2

#######################################################
# Cleaning Steps
#######################################################
# replace datapoint not asked for with blank
for(i in 1:ncol(prep.dat3)){
  prep.dat3[,i] <- ifelse(prep.dat3[,i] %in% c("-- Datapoint not asked for --","Datapoint not asked for"), NA, prep.dat3[,i])
}

# replace Unknown or NA in Condition columns with 1 (or 100%)
for (i in grep("condition", names(prep.dat3), ignore.case = T)){
  prep.dat3[,i] <- ifelse(prep.dat3[,i] %in% c("Unknown","N/A"), 1, prep.dat3[,i])
}

# when cavity or exterior insulated columns = No, make condition 0%
prep.dat3$Wall.Cavity.Insulation.Condition.1[which(prep.dat3$`Wall.Cavity.Insulated?` == "No")] <- "0%"
prep.dat3$Wall.Exterior.Insulation.Condition.1[which(prep.dat3$`Wall.Exterior.Insulated?` == "No")] <- "0%"

# Make thickness columns numeric
for (i in grep("thickness", names(prep.dat3), ignore.case = T)){
  prep.dat3[,i] <- as.numeric(as.character(prep.dat3[,i]))
}

#######################################################
# Cleaning and re-naming inches and rvalue information
#######################################################
prep.dat4 <- prep.dat3
# make numeric
prep.dat4$cavity.inches1   <- prep.dat4$Wall.Cavity.Insulation.Thickness.1
prep.dat4$cavity.inches2   <- prep.dat4$Wall.Cavity.Insulation.Thickness.2
prep.dat4$cavity.inches3   <- prep.dat4$Wall.Cavity.Insulation.Thickness.3
prep.dat4$exterior.inches1 <- prep.dat4$Wall.Exterior.Insulation.Thickness.1
prep.dat4$exterior.inches2 <- prep.dat4$Wall.Exterior.Insulation.Thickness.2
prep.dat4$exterior.inches3 <- prep.dat4$Wall.Exterior.Insulation.Thickness.3
prep.dat4$furred.inches    <- prep.dat4$Furred.Wall.Insulation.Thickness
#update column names
prep.dat4$cavity.rvalues1   <- prep.dat4$Wall.Cavity.Insulation.Type.1
prep.dat4$cavity.rvalues2   <- prep.dat4$Wall.Cavity.Insulation.Type.2
prep.dat4$cavity.rvalues3   <- prep.dat4$Wall.Cavity.Insulation.Type.3
prep.dat4$exterior.rvalues1 <- prep.dat4$Wall.Exterior.Insulation.Type.1
prep.dat4$exterior.rvalues2 <- prep.dat4$Wall.Exterior.Insulation.Type.2
prep.dat4$exterior.rvalues3 <- prep.dat4$Wall.Exterior.Insulation.Type.3
prep.dat4$furred.rvalues    <- prep.dat4$Furred.Wall.Insulation.Type
#replace any inches that are NA with zeros
for(i in grep("inches|rval", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}

#fix names that are not in R value table
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "Unknown fiberglass")]               <- "Unknown"
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "-- Datapoint not asked for --")]    <- NA
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "None")]                             <- NA
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 %in% c("N/A","NA"))]                    <- NA
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"
prep.dat4$cavity.rvalues1[grep('unknown', prep.dat4$cavity.rvalues1, ignore.case = T)] <- "Unknown"
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"

prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 %in% c("N/A","NA"))]                    <- NA
prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "-- Datapoint not asked for --")]    <- NA
prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"
prep.dat4$cavity.rvalues2[grep('unknown', prep.dat4$cavity.rvalues2, ignore.case = T)] <- "Unknown"

prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "-- Datapoint not asked for --")]    <- NA
prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"
prep.dat4$exterior.rvalues1[grep('unknown', prep.dat4$exterior.rvalues1, ignore.case = T)] <- "Unknown"

#for ICF walls
prep.dat4$cavity.rvalues1[which(prep.dat4$Wall.Type == "ICF")] <- "Expanded polystyrene foam board"
prep.dat4$cavity.inches1[which(prep.dat4$Wall.Type == "ICF")]  <- 5
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1     == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2     == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$cavity.rvalues3[which(prep.dat4$cavity.rvalues3     == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$exterior.rvalues2[which(prep.dat4$exterior.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$exterior.rvalues3[which(prep.dat4$exterior.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$furred.rvalues[which(prep.dat4$furred.rvalues       == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################

prep.dat4$cavity.rvalues1[which(prep.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
prep.dat4$cavity.rvalues2[which(prep.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
prep.dat4$cavity.rvalues3[which(prep.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
prep.dat4$exterior.rvalues1[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.rvalues2[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.rvalues3[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$furred.rvalues[which(prep.dat4$`Furred.Wall.Insulated?` == "No")] <- 0

prep.dat4$cavity.inches1[which(prep.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
prep.dat4$cavity.inches2[which(prep.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
prep.dat4$cavity.inches3[which(prep.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
prep.dat4$exterior.inches1[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.inches2[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.inches3[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$furred.inches[which(prep.dat4$`Furred.Wall.Insulated?` == "No")] <- 0

#replace any inches and rvalues that are NA with zeros
for(i in grep("inches|rval", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}
#make all inches and rvalue columns numeric
for(i in grep("inches|rval", colnames(prep.dat4))){
  prep.dat4[,i] <- as.numeric(as.character(prep.dat4[,i]))
}
#replace any inches and rvalues that are NA with zeros
for(i in grep("inches|rval", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}
#make all inches and rvalue columns numeric
for(i in grep("inches|rval", colnames(prep.dat4))){
  prep.dat4[,i] <- as.numeric(as.character(prep.dat4[,i]))
}


prep.dat4.5 <- prep.dat4


#create total.r.value column
prep.dat4.5$total.r.val <- NA


#check uniques -- None should be NA
unique(prep.dat4.5$cavity.rvalues1)
unique(prep.dat4.5$cavity.rvalues2)
unique(prep.dat4.5$cavity.rvalues3)
unique(prep.dat4.5$exterior.rvalues1)
unique(prep.dat4.5$exterior.rvalues2)
unique(prep.dat4.5$exterior.rvalues3)
unique(prep.dat4.5$furred.rvalues)

#####################################################
# For condition information
#####################################################
prep.dat4.5$Wall.Cavity.Insulation.Condition.1 <- as.numeric(as.character(prep.dat4.5$Wall.Cavity.Insulation.Condition.1))
prep.dat4.5$Wall.Exterior.Insulation.Condition.1 <- as.numeric(as.character(prep.dat4.5$Wall.Exterior.Insulation.Condition.1))

# clean up condition information - 
# this creates a line item for any condition less than 100%, 
# making R value for 100% - cativty% = 0
prep.condition.sub1 <- prep.dat4.5[which(prep.dat4.5$Wall.Cavity.Insulation.Condition.1 %notin% c(1, NA, 0)),]
prep.condition.sub1$Wall.Cavity.Insulation.Condition.1 <- 1 - prep.condition.sub1$Wall.Cavity.Insulation.Condition.1
prep.condition.sub1$cavity.rvalues1 <- 0
prep.condition.sub1$total.r.val <- NA

# clean up condition information
# this creates a line item for any condition less than 100%, 
# making R value for 100% - exterior% = 0
prep.condition.sub2 <- prep.dat4.5[which(prep.dat4.5$Wall.Exterior.Insulation.Condition.1 %notin% c(1, NA, 0)),]
prep.condition.sub2$Wall.Exterior.Insulation.Condition.1 <- 1 - prep.condition.sub2$Wall.Exterior.Insulation.Condition.1
prep.condition.sub2$exterior.rvalues1 <- 0
prep.condition.sub2$total.r.val <- NA


prep.dat4.8 <- rbind.data.frame(prep.dat4.5
                              ,prep.condition.sub1
                              ,prep.condition.sub2
                                , stringsAsFactors = F)
prep.dat4.8$Wall.Cavity.Insulation.Condition.1[which(is.na(prep.dat4.8$Wall.Cavity.Insulation.Condition.1))] <- 1
prep.dat4.9 <- prep.dat4.8[which(!is.na(prep.dat4.8$Wall.Type)),]
names(prep.dat4.9)[which(names(prep.dat4.9) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"
# prep.dat4.9 <- prep.dat4.9[which(prep.dat4.9$CK_Cadmus_ID != "BUILDING"),]

###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################
#calculate the weighted r value
na.ind <- which(is.na(prep.dat4.9$total.r.val))
prep.dat4.9$total.r.val[na.ind] <- (prep.dat4.9$cavity.rvalues1[na.ind] * prep.dat4.9$cavity.inches1[na.ind]) +  
  (prep.dat4.9$cavity.rvalues2[na.ind]   * prep.dat4.9$cavity.inches2[na.ind]) +  
  (prep.dat4.9$cavity.rvalues3[na.ind]   * prep.dat4.9$cavity.inches3[na.ind]) + 
  (prep.dat4.9$exterior.rvalues1[na.ind] * prep.dat4.9$exterior.inches1[na.ind]) + 
  (prep.dat4.9$exterior.rvalues2[na.ind] * prep.dat4.9$exterior.inches2[na.ind]) + 
  (prep.dat4.9$exterior.rvalues3[na.ind] * prep.dat4.9$exterior.inches3[na.ind]) +
  (prep.dat4.9$furred.rvalues[na.ind]    * prep.dat4.9$furred.inches[na.ind])

#check -- NOTE -- NONE SHOULD BE NA
unique(prep.dat4.9$total.r.val)
# prep.dat4.9$total.r.val[which(prep.dat4.9$Wall.Type == "ICF")]
#caluclate u factors = inverse of Rvalue
prep.dat4.9$uvalue <- 1 / (prep.dat4.9$total.r.val)
prep.dat4.9$uvalue[which(prep.dat4.9$uvalue == "Inf")] <- 1
unique(prep.dat4.9$uvalue)

#make area numeric
prep.dat4.9$uvalue    <- as.numeric(as.character(prep.dat4.9$uvalue))
prep.dat4.9$Wall.Area <- as.numeric(as.character(prep.dat4.9$Wall.Area))
prep.dat5 <- prep.dat4.9[which(prep.dat4.9$Wall.Type %notin% c("Adiabatic", "Knee Wall")),]

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID)
                       ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)
unique(weightedU$aveUval)


#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
unique(weightedU$aveRval)
unique(weightedU$aveUval)

# get unique cadmus IDs and building types for this subset of data
wall.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
prep.dat7 <- left_join(prep.dat6, rbsa.dat.orig)
# prep.dat7.0 <- left_join(rbsa.dat, prep.dat5)
# prep.dat7 <- left_join(prep.dat6, prep.dat7.0)
prep.dat7$aveUval[which(is.na(prep.dat7$aveUval))] <- 0
prep.dat7$aveRval[which(is.na(prep.dat7$aveRval))] <- 0
###################################################################################################################
#
#
# End Prep
#
#
###################################################################################################################

rbsa.wall <- rbsa.dat.orig[which(colnames(rbsa.dat.orig) %in% c("CK_Building_ID","BuildingType","HomeYearBuilt"))]
wall.merge <- data.frame(left_join(rbsa.wall, prep.dat4.9, by = c("CK_Building_ID" = "CK_SiteID")),stringsAsFactors = F)
wall.merge <- wall.merge[which(!is.na(wall.merge$uvalue)),]
#########export rvalues
##  Write out confidence/precision info
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(wall.merge, paste(filepathCleaningDocs, "Insulation Exports", paste("Wall Insulation Values ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)














#############################################################################################
# Item 10: DISTRIBUTION OF FRAME WALL INSULATION LEVELS BY FRAMING TYPE (SF table 17)
#############################################################################################
item10.dat <- prep.dat5[grep("framed|alternative",prep.dat5$Wall.Type, ignore.case = T),]

#weight the u factor per home -- where weights are the wall area within home
item10.weightedU <- summarise(group_by(item10.dat, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
item10.weightedU$aveRval <- (1 / as.numeric(as.character(item10.weightedU$aveUval)))
item10.weightedU$aveRval[which(item10.weightedU$aveRval %in% c("NaN",1))] <- 0
item10.weightedU$aveUval[which(item10.weightedU$aveUval == "NaN")] <- 1
unique(item10.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
item10.wall.unique <- unique(item10.dat[which(colnames(item10.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
item10.dat1 <- left_join(item10.weightedU, item10.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item10.dat2 <- left_join(item10.dat1, rbsa.dat)
item10.dat2$aveUval[which(is.na(item10.dat2$aveUval))] <- 0
item10.dat2$aveRval[which(is.na(item10.dat2$aveRval))] <- 0


#Bin R values -- SF only
item10.dat2$rvalue.bins <- "Unknown"
item10.dat2$rvalue.bins[which(item10.dat2$aveRval ==  0)] <- "R0"
item10.dat2$rvalue.bins[which(item10.dat2$aveRval >   0  & item10.dat2$aveRval < 11)]  <- "R1.R10"
item10.dat2$rvalue.bins[which(item10.dat2$aveRval >= 11  & item10.dat2$aveRval < 17)]  <- "R11.R16"
item10.dat2$rvalue.bins[which(item10.dat2$aveRval >= 17  & item10.dat2$aveRval < 23)]  <- "R17.R22"
item10.dat2$rvalue.bins[which(item10.dat2$aveRval >= 22)] <- "RGT22"
unique(item10.dat2$rvalue.bins)

item10.dat2$count <- 1

item10.dat3 <- item10.dat2[which(item10.dat2$rvalue.bins != "Unknown"),]
colnames(item10.dat3)

item10.merge <- left_join(rbsa.dat, item10.dat3)
item10.merge <- item10.merge[which(!is.na(item10.merge$count)),]

item10.data <- weightedData(unique(item10.merge[-which(colnames(item10.merge) %in% c("Wall.Type"
                                                                              ,"aveUval"
                                                                              ,"aveRval"
                                                                              ,"rvalue.bins"
                                                                              ,"count"))]))
item10.data <- left_join(item10.data, item10.merge[which(colnames(item10.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins"
                                                                                       ,"count"))])



################################
# Weighted Analysis
################################
item10.summary <- proportionRowsAndColumns1(CustomerLevelData     = item10.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'Wall.Type'
                                            , rowVariable         = 'rvalue.bins'
                                            , aggregateColumnName = "All Frame Types"
)
item10.summary <- item10.summary[which(item10.summary$Wall.Type != "All Frame Types"),]
item10.summary <- item10.summary[which(item10.summary$rvalue.bins != "Total"),]

## Summary only for "All Frame Types"
item10.all.frame.types <- proportions_one_group(CustomerLevelData = item10.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Frame Types"
                                                ,columnName       = "Wall.Type"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item10.all.insul.levels <-  proportions_one_group(CustomerLevelData = item10.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "Wall.Type"
                                                  ,total.name       = "All Insulation Levels"
                                                  ,columnName       = "rvalue.bins"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item10.final <- rbind.data.frame(item10.summary
                                 , item10.all.frame.types
                                 , item10.all.insul.levels
                                 , stringsAsFactors = F)
item10.final$Wall.Type[which(item10.final$Wall.Type == "Total")] <- "All Frame Types"


##cast data
item10.cast <- dcast(setDT(item10.final),
                     formula   = BuildingType + Wall.Type ~ rvalue.bins,
                     value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

#join all insulation levels onto rvalue summary
item10.table <- data.frame("BuildingType"                   = item10.cast$BuildingType
                           ,"Wall.Type"                     = item10.cast$Wall.Type
                           ,"Percent.R0"                    = item10.cast$w.percent_R0
                           ,"SE.R0"                         = item10.cast$w.SE_R0
                           ,"Percent.R1.R10"                = item10.cast$w.percent_R1.R10
                           ,"SE.R1.R10"                     = item10.cast$w.SE_R1.R10
                           ,"Percent.R11.R16"               = item10.cast$w.percent_R11.R16
                           ,"SE.R11.R16"                    = item10.cast$w.SE_R11.R16
                           ,"Percent.R17.R22"               = item10.cast$w.percent_R17.R22
                           ,"SE.R17.R22"                    = item10.cast$w.SE_R17.R22
                           ,"Percent.RGT22"                 = item10.cast$w.percent_RGT22
                           ,"SE.RGT22"                      = item10.cast$w.SE_RGT22
                           ,"Percent_All Insulation Levels" = item10.cast$`w.percent_All Insulation Levels`
                           ,"SE.All Insulation Levels"      = item10.cast$`w.SE_All Insulation Levels`
                           ,"n"                             = item10.cast$`n_All Insulation Levels`
                           ,"EB.R0"                         = item10.cast$EB_R0
                           ,"EB.R1.R10"                     = item10.cast$EB_R1.R10
                           ,"EB.R11.R16"                    = item10.cast$EB_R11.R16
                           ,"EB.R17.R22"                    = item10.cast$EB_R17.R22
                           ,"EB.RGT22"                      = item10.cast$EB_RGT22
                           ,"EB.All Insulation Levels"      = item10.cast$`EB_All Insulation Levels`
                           )

# row ordering example code
unique(item10.table$Wall.Type)
rowOrder <- c("Framed 2x2"
              ,"Framed 2x3"
              ,"Framed 2x4"
              ,"Framed 2x6"
              ,"Framed 2x8"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"All Frame Types")
item10.table <- item10.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item10.table <- data.frame(item10.table)


item10.table.SF <- item10.table[which(item10.table$BuildingType == "Single Family"),-1]


#export table to correct workbook using exporting function
exportTable(item10.table.SF, "SF", "Table 17", weighted = TRUE)



################################
# Unweighted Analysis
################################
item10.summary <- proportions_two_groups_unweighted(CustomerLevelData = item10.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'Wall.Type'
                                            , rowVariable         = 'rvalue.bins'
                                            , aggregateColumnName = "All Frame Types"
)
item10.summary <- item10.summary[which(item10.summary$Wall.Type != "All Frame Types"),]

## Summary only for "All Frame Types"
item10.all.frame.types <- proportions_one_group(CustomerLevelData =  item10.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Frame Types"
                                                ,columnName       = "Wall.Type"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item10.all.insul.levels <-  proportions_one_group(CustomerLevelData = item10.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "Wall.Type"
                                                  ,total.name       = "All Insulation Levels"
                                                  ,columnName       = "rvalue.bins"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item10.final <- rbind.data.frame(item10.summary
                                 , item10.all.frame.types
                                 , item10.all.insul.levels
                                 , stringsAsFactors = F)
item10.final <- item10.final[which(item10.final$rvalue.bins != "Total"),]
item10.final$Wall.Type[which(item10.final$Wall.Type == "Total")] <- "All Frame Types"


##cast data
item10.cast <- dcast(setDT(item10.final),
                     formula   = BuildingType + Wall.Type ~ rvalue.bins,
                     value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item10.table <- data.frame("BuildingType"                   = item10.cast$BuildingType
                           ,"Wall.Type"                     = item10.cast$Wall.Type
                           ,"Percent.R0"                    = item10.cast$Percent_R0
                           ,"SE.R0"                         = item10.cast$SE_R0
                           ,"Percent.R1.R10"                = item10.cast$Percent_R1.R10
                           ,"SE.R1.R10"                     = item10.cast$SE_R1.R10
                           ,"Percent.R11.R16"               = item10.cast$Percent_R11.R16
                           ,"SE.R11.R16"                    = item10.cast$SE_R11.R16
                           ,"Percent.R17.R22"               = item10.cast$Percent_R17.R22
                           ,"SE.R17.R22"                    = item10.cast$SE_R17.R22
                           ,"Percent.RGT22"                 = item10.cast$Percent_RGT22
                           ,"SE.RGT22"                      = item10.cast$SE_RGT22
                           ,"Percent_All Insulation Levels" = item10.cast$`Percent_All Insulation Levels`
                           ,"SE.All Insulation Levels"      = item10.cast$`SE_All Insulation Levels`
                           ,"n"                             = item10.cast$`n_All Insulation Levels`
                           )

# row ordering example code
unique(item10.table$Wall.Type)
rowOrder <- c("Framed 2x2"
              ,"Framed 2x3"
              ,"Framed 2x4"
              ,"Framed 2x6"
              ,"Framed 2x8"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"All Frame Types")
item10.table <- item10.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item10.table <- data.frame(item10.table)


item10.table.SF <- item10.table[which(item10.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item10.table.SF, "SF", "Table 17", weighted = FALSE)






#############################################################################################
# Item 11: DISTRIBUTION OF WALL FRAMING TYPES BY VINTAGE (SF table 18)
#############################################################################################
## Note: For this table, you must run up to item10.dat3 for the cleaned data
item11.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Wall.Type"))]

item11.dat2 <- left_join(item11.dat1, rbsa.dat, by = c("CK_Cadmus_ID"))

#remove unneccesary wall types
item11.dat3 <- item11.dat2[grep("framed|alternative",item11.dat2$Wall.Type, ignore.case = T),]

#create "Alternative" category
item11.dat3$Wall.Type[grep("alternative",item11.dat3$Wall.Type, ignore.case = T)] <- "Alternative"
length(unique(item11.dat3$CK_Cadmus_ID))
unique(item11.dat3$Wall.Type)

                                                               
#cast out by wall frame types
item11.dat3$count <- 1
item11.customer <- summarise(group_by(item11.dat3, CK_Cadmus_ID, BuildingType, State, HomeYearBuilt_bins3, Wall.Type)
                             ,count = sum(count))

item11.merge <- left_join(rbsa.dat, item11.customer)
item11.merge <- item11.merge[which(!is.na(item11.merge$HomeYearBuilt)),]
item11.merge <- item11.merge[which(!is.na(item11.merge$Wall.Type)),]

item11.data <- weightedData(unique(item11.merge[-which(colnames(item11.merge) %in% c("Category"
                                                                                     ,"Wall.Type"
                                                                                     ,"count"))]))
item11.data <- left_join(item11.data, item11.merge[which(colnames(item11.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Category"
                                                                                       ,"Wall.Type"
                                                                                       ,"count"))])

item11.data$Ind <- 1

#############################
# Weighted Analysis
#############################
item11.by.vinage <- proportionRowsAndColumns1(CustomerLevelData = item11.data
                                          , valueVariable       = 'Ind'
                                          , columnVariable      = 'HomeYearBuilt_bins3'
                                          , rowVariable         = 'Wall.Type'
                                          , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item11.across.vintages <- proportions_one_group(CustomerLevelData = item11.data
                                                , valueVariable    = 'Ind'
                                                , groupingVariable = 'Wall.Type'
                                                , total.name       = 'All Housing Vintages'
                                                , columnName       = 'HomeYearBuilt_bins3'
                                                , weighted = TRUE
                                                ,two.prop.total = TRUE
                                                )


# row bind summaries
item11.final <- rbind.data.frame(item11.by.vinage, item11.across.vintages, stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item11.final <- item11.final[which(item11.final$HomeYearBuilt_bins3 != "Remove"),]

item11.cast <- dcast(setDT(item11.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ Wall.Type, sum,
                     value.var = c("w.percent", "w.SE", "count","n","N","EB"))

item11.table <- data.frame("BuildingType"     = item11.cast$BuildingType
                           ,"Housing.Vintage" = item11.cast$HomeYearBuilt_bins3
                           ,"Percent_2x4"     = item11.cast$`w.percent_Framed 2x4`
                           ,"SE_2x4"          = item11.cast$`w.SE_Framed 2x4`
                           ,"Percent_2x6"     = item11.cast$`w.percent_Framed 2x6`
                           ,"SE_2x6"          = item11.cast$`w.SE_Framed 2x6`
                           ,"Percent_2x8"     = item11.cast$`w.percent_Framed 2x8`
                           ,"SE_2x8"          = item11.cast$`w.SE_Framed 2x8`
                           ,"Percent_ALT"     = item11.cast$w.percent_Alternative
                           ,"SE_ALT"          = item11.cast$w.SE_Alternative
                           ,"Percent_Unknown" = item11.cast$`w.percent_Framed (Unknown)`
                           ,"SE_Unknown"      = item11.cast$`w.SE_Framed (Unknown)`
                           ,"n"               = item11.cast$n_Total
                           ,"EB_2x4"          = item11.cast$`EB_Framed 2x4`
                           ,"EB_2x6"          = item11.cast$`EB_Framed 2x6`
                           ,"EB_2x8"          = item11.cast$`EB_Framed 2x8`
                           ,"EB_ALT"          = item11.cast$EB_Alternative
                           ,"EB_Unknown"      = item11.cast$`EB_Framed (Unknown)`
                           )

# row ordering example code
unique(item11.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item11.table <- item11.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item11.table <- data.frame(item11.table)


item11.table.SF <- item11.table[which(item11.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item11.table.SF, "SF", "Table 18", weighted = TRUE)


#############################
# Unweighted Analysis
#############################
item11.by.vinage <- proportions_two_groups_unweighted(item11.data
                                              , valueVariable       = 'count'
                                              , columnVariable      = 'HomeYearBuilt_bins3'
                                              , rowVariable         = 'Wall.Type'
                                              , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item11.across.vintages <- proportions_one_group(item11.data
                                                , valueVariable    = 'count'
                                                , groupingVariable = 'Wall.Type'
                                                , total.name       = 'All Housing Vintages'
                                                , columnName       = 'HomeYearBuilt_bins3'
                                                , weighted = FALSE
                                                , two.prop.total = TRUE
)
# row bind summaries
item11.final <- rbind.data.frame(item11.by.vinage, item11.across.vintages, stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item11.final <- item11.final[which(item11.final$HomeYearBuilt_bins3 != "Remove"),]

item11.cast <- dcast(setDT(item11.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ Wall.Type, sum,
                     value.var = c("Percent", "SE", "Count","n"))

item11.table <- data.frame("BuildingType"     = item11.cast$BuildingType
                           ,"Housing.Vintage" = item11.cast$HomeYearBuilt_bins3
                           ,"Percent_2x4"     = item11.cast$`Percent_Framed 2x4`
                           ,"SE_2x4"          = item11.cast$`SE_Framed 2x4`
                           ,"Percent_2x6"     = item11.cast$`Percent_Framed 2x6`
                           ,"SE_2x6"          = item11.cast$`SE_Framed 2x6`
                           ,"Percent_2x8"     = item11.cast$`Percent_Framed 2x8`
                           ,"SE_2x8"          = item11.cast$`SE_Framed 2x8`
                           ,"Percent_ALT"     = item11.cast$Percent_Alternative
                           ,"SE_ALT"          = item11.cast$SE_Alternative
                           ,"Percent_Unknown" = item11.cast$`Percent_Framed (Unknown)`
                           ,"SE_Unknown"      = item11.cast$`SE_Framed (Unknown)`
                           ,"n"               = item11.cast$n_Total
)

# row ordering example code
unique(item11.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item11.table <- item11.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item11.table <- data.frame(item11.table)

item11.table.SF <- item11.table[which(item11.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item11.table.SF, "SF", "Table 18", weighted = FALSE)






#############################################################################################
# Item 12: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE  (SF table 19, MH table 16)
#############################################################################################
prep.item12.dat <- prep.dat5[-grep("basement",prep.dat5$Wall.Type, ignore.case = T),]

#weight the u factor per home -- where weights are the wall area within home
prep.item12.weightedU <- summarise(group_by(prep.item12.dat, CK_Cadmus_ID, Wall.Type)
                              ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
prep.item12.weightedU$aveRval <- (1 / as.numeric(as.character(prep.item12.weightedU$aveUval)))
prep.item12.weightedU$aveRval[which(prep.item12.weightedU$aveRval %in% c("NaN",1))] <- 0
prep.item12.weightedU$aveUval[which(prep.item12.weightedU$aveUval == "NaN")] <- 1
unique(prep.item12.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
prep.item12.wall.unique <- unique(prep.item12.dat[which(colnames(prep.item12.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.item12.dat1 <- left_join(prep.item12.weightedU, prep.item12.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
prep.item12.dat2 <- left_join(prep.item12.dat1, rbsa.dat)
prep.item12.dat2$aveUval[which(is.na(prep.item12.dat2$aveUval))] <- 0
prep.item12.dat2$aveRval[which(is.na(prep.item12.dat2$aveRval))] <- 0


## Note: For this table, you must run up to prep.dat7 for the cleaned data
item12.dat <- prep.item12.dat2

item12.dat1 <- item12.dat[which(!(is.na(item12.dat$HomeYearBuilt_bins3))),]

#Bin R values -- SF only
item12.dat1$rvalue.bins.SF <- "Unknown"
item12.dat1$rvalue.bins.SF[which(item12.dat1$aveRval < 1)] <- "R0"
item12.dat1$rvalue.bins.SF[which(item12.dat1$aveRval >=  1  & item12.dat1$aveRval < 11)]  <- "R1.R10"
item12.dat1$rvalue.bins.SF[which(item12.dat1$aveRval >= 11 & item12.dat1$aveRval  < 17)]  <- "R11.R16"
item12.dat1$rvalue.bins.SF[which(item12.dat1$aveRval >= 17 & item12.dat1$aveRval  < 23)]  <- "R17.R22"
item12.dat1$rvalue.bins.SF[which(item12.dat1$aveRval >= 22)] <- "RGT22"
unique(item12.dat1$rvalue.bins.SF)

#Bin R values -- MH only
item12.dat1$rvalue.bins.MH <- "Unknown"
item12.dat1$rvalue.bins.MH[which(item12.dat1$aveRval >= 0  & item12.dat1$aveRval <=  8)]  <- "R0.R8"
item12.dat1$rvalue.bins.MH[which(item12.dat1$aveRval > 8  & item12.dat1$aveRval <= 14)]  <- "R9.R14"
item12.dat1$rvalue.bins.MH[which(item12.dat1$aveRval > 14 & item12.dat1$aveRval <= 21)]  <- "R15.R21"
item12.dat1$rvalue.bins.MH[which(item12.dat1$aveRval > 21)]  <- "R22.R30"
unique(item12.dat1$rvalue.bins.MH)


############################################################################################################
# Apply weights
############################################################################################################
item12.dat1$count <- 1
colnames(item12.dat1)

item12.merge <- left_join(rbsa.dat, item12.dat1)
item12.merge <- item12.merge[which(!is.na(item12.merge$count)),]

item12.data <- weightedData(unique(item12.merge[-which(colnames(item12.merge) %in% c("Wall.Type"
                                                                                     ,"aveUval"
                                                                                     ,"aveRval"
                                                                                     ,"rvalue.bins.SF"
                                                                                     ,"rvalue.bins.MH"
                                                                                     ,"count"))]))
item12.data <- left_join(item12.data, item12.merge[which(colnames(item12.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins.SF"
                                                                                       ,"rvalue.bins.MH"
                                                                                       ,"count"))])
############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item12.summary <- proportionRowsAndColumns1(CustomerLevelData     = item12.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item12.summary <- item12.summary[which(item12.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]
item12.summary <- item12.summary[which(item12.summary$rvalue.bins.SF != "Total"),]

## Summary only for "All Frame Types"
item12.all.frame.types <- proportions_one_group(CustomerLevelData = item12.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)
item12.all.frame.types <- item12.all.frame.types[which(item12.all.frame.types$rvalue.bins.SF != "Total"),]

## Summary for only "All Insulation Levels"
item12.all.insul.levels <-  proportions_one_group(CustomerLevelData = item12.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Walls"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item12.final <- rbind.data.frame(item12.summary
                                 , item12.all.frame.types
                                 , item12.all.insul.levels
                                 , stringsAsFactors = F)
item12.final$HomeYearBuilt_bins3[which(item12.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"
item12.final$rvalue.bins.SF[which(item12.final$rvalue.bins.SF == "Total")] <- "All Walls"

item12.cast <- dcast(setDT(item12.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item12.cast)
item12.table <- data.frame("BuildingType"     = item12.cast$BuildingType
                           ,"Housing.Vintage" = item12.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item12.cast$w.percent_R0
                           ,"SE.R0"           = item12.cast$w.SE_R0
                           ,"Percent.R1.R10"  = item12.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item12.cast$w.SE_R1.R10
                           ,"Percent.R11.R16" = item12.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item12.cast$w.SE_R11.R16
                           ,"Percent.R17.R22" = item12.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item12.cast$w.SE_R17.R22
                           ,"Percent.RGT22"   = item12.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item12.cast$w.SE_RGT22
                           ,"n"               = item12.cast$`n_All Walls`
                           ,"EB.R0"           = item12.cast$EB_R0
                           ,"EB.R1.R10"       = item12.cast$EB_R1.R10
                           ,"EB.R11.R16"      = item12.cast$EB_R11.R16
                           ,"EB.R17.R22"      = item12.cast$EB_R17.R22
                           ,"EB.RGT22"        = item12.cast$EB_RGT22
                           )

# row ordering example code
unique(item12.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item12.table <- item12.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item12.table <- data.frame(item12.table)

item12.table.SF <- item12.table[which(item12.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item12.table.SF, "SF", "Table 19", weighted = TRUE)



################################
# Unweighted Analysis
################################
item12.summary <- proportions_two_groups_unweighted(CustomerLevelData = item12.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item12.summary <- item12.summary[which(item12.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item12.all.frame.types <- proportions_one_group(item12.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item12.all.insul.levels <-  proportions_one_group(item12.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item12.final <- rbind.data.frame(item12.summary
                                 , item12.all.frame.types
                                 , item12.all.insul.levels
                                 , stringsAsFactors = F)
item12.final <- item12.final[which(item12.final$rvalue.bins.SF != "Total"),]
item12.final$HomeYearBuilt_bins3[which(item12.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


##cast data
item12.cast <- dcast(setDT(item12.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))


item12.table <- data.frame("BuildingType"     = item12.cast$BuildingType
                           ,"Housing.Vintage" = item12.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item12.cast$Percent_R0
                           ,"SE.R0"           = item12.cast$SE_R0
                           ,"Percent.R1.R10"  = item12.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item12.cast$SE_R1.R10
                           ,"Percent.R11.R16" = item12.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item12.cast$SE_R11.R16
                           ,"Percent.R17.R22" = item12.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item12.cast$SE_R17.R22
                           ,"Percent.RGT22"   = item12.cast$Percent_RGT22
                           ,"SE.RGT22"        = item12.cast$SE_RGT22
                           ,"n"               = item12.cast$`n_All Housing Vintages`
                           )

# row ordering example code
unique(item12.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item12.table <- item12.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item12.table <- data.frame(item12.table)

item12.table.SF <- item12.table[which(item12.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item12.table.SF, "SF", "Table 19", weighted = FALSE)




############################################################################################################
# Weighted Analysis - Manufactured
############################################################################################################
item12.summary <- proportionRowsAndColumns1(CustomerLevelData     = item12.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins2'
                                            , rowVariable         = 'rvalue.bins.MH'
                                            , aggregateColumnName = "All Housing Vintages"
)
item12.summary <- item12.summary[which(item12.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]
item12.summary <- item12.summary[which(item12.summary$rvalue.bins.MH != "Total"),]

## Summary only for "All Frame Types"
item12.all.frame.types <- proportions_one_group(CustomerLevelData = item12.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.MH"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins2"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)
item12.all.frame.types <- item12.all.frame.types[which(item12.all.frame.types$rvalue.bins.MH != "Total"),]

## Summary for only "All Insulation Levels"
item12.all.insul.levels <-  proportions_one_group(CustomerLevelData = item12.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins2"
                                                  ,total.name       = "All Walls"
                                                  ,columnName       = "rvalue.bins.MH"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item12.final <- rbind.data.frame(item12.summary
                                 , item12.all.frame.types
                                 , item12.all.insul.levels
                                 , stringsAsFactors = F)
item12.final$HomeYearBuilt_bins2[which(item12.final$HomeYearBuilt_bins2 == "Total")] <- "All Housing Vintages"
item12.final$rvalue.bins.MH[which(item12.final$rvalue.bins.MH == "Total")] <- "All Walls"

item12.cast <- dcast(setDT(item12.final),
                     formula   = BuildingType + HomeYearBuilt_bins2 ~ rvalue.bins.MH,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item12.cast)

item12.table <- data.frame("BuildingType"       = item12.cast$BuildingType
                           ,"Housing.Vintage"   = item12.cast$HomeYearBuilt_bins2
                           ,"Percent.R0.R8"     = item12.cast$w.percent_R0.R8
                           ,"SE.R0.R8"          = item12.cast$w.SE_R0.R8
                           ,"Percent.R9.R14"    = item12.cast$w.percent_R9.R14
                           ,"SE.R9.R14"         = item12.cast$w.SE_R9.R14
                           ,"Percent.R15.R21"   = item12.cast$w.percent_R15.R21
                           ,"SE.R15.R21"        = item12.cast$w.SE_R15.R21
                           ,"Percent.R22.R30"   = item12.cast$w.percent_R22.R30
                           ,"SE.R22.R30"        = item12.cast$w.SE_R22.R30
                           ,"Percent.All.Walls" = item12.cast$`w.percent_All Walls`
                           ,"SE.All.Walls"      = item12.cast$`w.SE_All Walls`
                           ,"n"                 = item12.cast$`n_All Walls`
                           ,"EB.R0.R8"          = item12.cast$EB_R0.R8
                           ,"EB.R9.R14"         = item12.cast$EB_R9.R14
                           ,"EB.R15.R21"        = item12.cast$EB_R15.R21
                           ,"EB.R22.R30"        = item12.cast$EB_R22.R30
                           ,"EB.All.Walls"      = item12.cast$`EB_All Walls`
                           )

# row ordering example code
unique(item12.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item12.table <- item12.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item12.table <- data.frame(item12.table)

item12.table.MH <- item12.table[which(item12.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item12.table.MH, "MH", "Table 16", weighted = TRUE)


############################################################################################################
# Unweighted Analysis - Manufactured
############################################################################################################
item12.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item12.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins2'
                                            , rowVariable         = 'rvalue.bins.MH'
                                            , aggregateColumnName = "All Housing Vintages"
)
item12.summary <- item12.summary[which(item12.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]
item12.summary <- item12.summary[which(item12.summary$rvalue.bins.MH != "Total"),]

## Summary only for "All Frame Types"
item12.all.frame.types <- proportions_one_group(CustomerLevelData = item12.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.MH"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins2"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)
item12.all.frame.types <- item12.all.frame.types[which(item12.all.frame.types$rvalue.bins.MH != "Total"),]

## Summary for only "All Insulation Levels"
item12.all.insul.levels <-  proportions_one_group(CustomerLevelData = item12.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins2"
                                                  ,total.name       = "All Walls"
                                                  ,columnName       = "rvalue.bins.MH"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item12.final <- rbind.data.frame(item12.summary
                                 , item12.all.frame.types
                                 , item12.all.insul.levels
                                 , stringsAsFactors = F)
item12.final$HomeYearBuilt_bins2[which(item12.final$HomeYearBuilt_bins2 == "Total")] <- "All Housing Vintages"
item12.final$rvalue.bins.MH[which(item12.final$rvalue.bins.MH == "Total")] <- "All Walls"

item12.cast <- dcast(setDT(item12.final),
                     formula   = BuildingType + HomeYearBuilt_bins2 ~ rvalue.bins.MH,
                     value.var = c("Percent", "SE", "Count", "n"))

item12.table <- data.frame("BuildingType"       = item12.cast$BuildingType
                           ,"Housing.Vintage"   = item12.cast$HomeYearBuilt_bins2
                           ,"Percent.R0.R8"     = item12.cast$Percent_R0.R8
                           ,"SE.R0.R8"          = item12.cast$SE_R0.R8
                           ,"Percent.R9.R14"    = item12.cast$Percent_R9.R14
                           ,"SE.R9.R14"         = item12.cast$SE_R9.R14
                           ,"Percent.R15.R21"   = item12.cast$Percent_R15.R21
                           ,"SE.R15.R21"        = item12.cast$SE_R15.R21
                           ,"Percent.R22.R30"   = item12.cast$Percent_R22.R30
                           ,"SE.R22.R30"        = item12.cast$SE_R22.R30
                           ,"Percent.All.Walls" = item12.cast$`Percent_All Walls`
                           ,"SE.All.Walls"      = item12.cast$`SE_All Walls`
                           ,"n"                 = item12.cast$`n_All Walls`
)

# row ordering example code
unique(item12.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item12.table <- item12.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item12.table <- data.frame(item12.table)

item12.table.MH <- item12.table[which(item12.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item12.table.MH, "MH", "Table 16", weighted = FALSE)




#############################################################################################
# Table AP: DISTRIBUTION OF WALL INSULATION LEVELS BY STATE
#############################################################################################
prep.tableAP.dat <- prep.dat5[-grep("basement",prep.dat5$Wall.Type, ignore.case = T),]

#weight the u factor per home -- where weights are the wall area within home
prep.tableAP.weightedU <- summarise(group_by(prep.tableAP.dat, CK_Cadmus_ID)
                                   ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
prep.tableAP.weightedU$aveRval <- (1 / as.numeric(as.character(prep.tableAP.weightedU$aveUval)))
prep.tableAP.weightedU$aveRval[which(prep.tableAP.weightedU$aveRval %in% c("NaN",1))] <- 0
prep.tableAP.weightedU$aveUval[which(prep.tableAP.weightedU$aveUval == "NaN")] <- 1
unique(prep.tableAP.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
prep.tableAP.wall.unique <- unique(prep.tableAP.dat[which(colnames(prep.tableAP.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.tableAP.dat1 <- left_join(prep.tableAP.weightedU, prep.tableAP.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
prep.tableAP.dat2 <- left_join(prep.tableAP.dat1, rbsa.dat)
prep.tableAP.dat2$aveUval[which(is.na(prep.tableAP.dat2$aveUval))] <- 0
prep.tableAP.dat2$aveRval[which(is.na(prep.tableAP.dat2$aveRval))] <- 0


tableAP.dat <- prep.tableAP.dat2[which(!is.na(prep.tableAP.dat2$HomeYearBuilt)),]

tableAP.dat1 <- tableAP.dat

#Bin R values -- SF only
tableAP.dat1$rvalue.bins.SF <- "Unknown"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval < 1)] <- "R0"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval >=  1  & tableAP.dat1$aveRval < 11)]  <- "R1.R10"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval >= 11 & tableAP.dat1$aveRval  < 17)]  <- "R11.R16"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval >= 17 & tableAP.dat1$aveRval  < 23)]  <- "R17.R22"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval >= 22)] <- "RGT22"
unique(tableAP.dat1$rvalue.bins.SF)

#Bin R values -- MH only
tableAP.dat1$rvalue.bins.MH <- "Unknown"
tableAP.dat1$rvalue.bins.MH[which(tableAP.dat1$aveRval >= 0  & tableAP.dat1$aveRval <=  8)]  <- "R0.R8"
tableAP.dat1$rvalue.bins.MH[which(tableAP.dat1$aveRval > 8  & tableAP.dat1$aveRval <= 14)]  <- "R9.R14"
tableAP.dat1$rvalue.bins.MH[which(tableAP.dat1$aveRval > 14 & tableAP.dat1$aveRval <= 21)]  <- "R15.R21"
tableAP.dat1$rvalue.bins.MH[which(tableAP.dat1$aveRval > 21)]  <- "R22.R30"
unique(tableAP.dat1$rvalue.bins.MH)


############################################################################################################
# Apply weights
############################################################################################################
tableAP.dat1$count <- 1
colnames(tableAP.dat1)

tableAP.merge <- left_join(rbsa.dat, tableAP.dat1)
tableAP.merge <- tableAP.merge[which(!is.na(tableAP.merge$count)),]

tableAP.data <- weightedData(unique(tableAP.merge[-which(colnames(tableAP.merge) %in% c("Wall.Type"
                                                                                     ,"aveUval"
                                                                                     ,"aveRval"
                                                                                     ,"rvalue.bins.SF"
                                                                                     ,"rvalue.bins.MH"
                                                                                     ,"count"))]))
tableAP.data <- left_join(tableAP.data, tableAP.merge[which(colnames(tableAP.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins.SF"
                                                                                       ,"rvalue.bins.MH"
                                                                                       ,"count"))])

#############################################################################################
# Weighted Analysis - Single Family
#############################################################################################
tableAP.final <- proportionRowsAndColumns1(CustomerLevelData     = tableAP.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'State'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "Region"
)

tableAP.cast <- dcast(setDT(tableAP.final),
                     formula   = BuildingType + rvalue.bins.SF ~ State,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

tableAP.table <- data.frame("BuildingType"       = tableAP.cast$BuildingType
                            ,"Insulation.Levels" = tableAP.cast$rvalue.bins.SF
                            ,"Percent_ID"        = tableAP.cast$w.percent_ID
                            ,"SE_ID"             = tableAP.cast$w.SE_ID
                            ,"n_ID"              = tableAP.cast$n_ID
                            ,"Percent_MT"        = tableAP.cast$w.percent_MT
                            ,"SE_MT"             = tableAP.cast$w.SE_MT
                            ,"n_MT"              = tableAP.cast$n_MT
                            ,"Percent_OR"        = tableAP.cast$w.percent_OR
                            ,"SE_OR"             = tableAP.cast$w.SE_OR
                            ,"n_OR"              = tableAP.cast$n_OR
                            ,"Percent_WA"        = tableAP.cast$w.percent_WA
                            ,"SE_WA"             = tableAP.cast$w.SE_WA
                            ,"n_WA"              = tableAP.cast$n_WA
                            ,"Percent_Region"    = tableAP.cast$w.percent_Region
                            ,"SE_Region"         = tableAP.cast$w.SE_Region
                            ,"n_Region"          = tableAP.cast$n_Region
                            ,"EB_ID"             = tableAP.cast$EB_ID
                            ,"EB_MT"             = tableAP.cast$EB_MT
                            ,"EB_OR"             = tableAP.cast$EB_OR
                            ,"EB_WA"             = tableAP.cast$EB_WA
                            ,"EB_Region"         = tableAP.cast$EB_Region
)

tableAP.table.SF <- tableAP.table[which(tableAP.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(tableAP.table.SF, "SF", "Table AP", weighted = TRUE)



################################
# Unweighted Analysis
################################
tableAP.final <- proportions_two_groups_unweighted(CustomerLevelData     = tableAP.data
                                           , valueVariable       = 'count'
                                           , columnVariable      = 'State'
                                           , rowVariable         = 'rvalue.bins.SF'
                                           , aggregateColumnName = "Region"
)

tableAP.cast <- dcast(setDT(tableAP.final),
                      formula   = BuildingType + rvalue.bins.SF ~ State,
                      value.var = c("Percent", "SE", "Count", "n"))

tableAP.table <- data.frame("BuildingType"       = tableAP.cast$BuildingType
                            ,"Insulation.Levels" = tableAP.cast$rvalue.bins.SF
                            ,"Percent_ID"        = tableAP.cast$Percent_ID
                            ,"SE_ID"             = tableAP.cast$SE_ID
                            ,"n_ID"              = tableAP.cast$n_ID
                            ,"Percent_MT"        = tableAP.cast$Percent_MT
                            ,"SE_MT"             = tableAP.cast$SE_MT
                            ,"n_MT"              = tableAP.cast$n_MT
                            ,"Percent_OR"        = tableAP.cast$Percent_OR
                            ,"SE_OR"             = tableAP.cast$SE_OR
                            ,"n_OR"              = tableAP.cast$n_OR
                            ,"Percent_WA"        = tableAP.cast$Percent_WA
                            ,"SE_WA"             = tableAP.cast$SE_WA
                            ,"n_WA"              = tableAP.cast$n_WA
                            ,"Percent_Region"    = tableAP.cast$Percent_Region
                            ,"SE_Region"         = tableAP.cast$SE_Region
                            ,"n_Region"          = tableAP.cast$n_Region
)

tableAP.table.SF <- tableAP.table[which(tableAP.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(tableAP.table.SF, "SF", "Table AP", weighted = FALSE)


#############################################################################################
# Weighted Analysis - manufactured
#############################################################################################
tableAP.final <- proportionRowsAndColumns1(CustomerLevelData     = tableAP.data
                                           , valueVariable       = 'count'
                                           , columnVariable      = 'State'
                                           , rowVariable         = 'rvalue.bins.MH'
                                           , aggregateColumnName = "Region"
)

tableAP.cast <- dcast(setDT(tableAP.final),
                      formula   = BuildingType + rvalue.bins.MH ~ State,
                      value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

tableAP.table <- data.frame("BuildingType"       = tableAP.cast$BuildingType
                            ,"Insulation.Levels" = tableAP.cast$rvalue.bins.MH
                            ,"Percent_ID"        = tableAP.cast$w.percent_ID
                            ,"SE_ID"             = tableAP.cast$w.SE_ID
                            ,"n_ID"              = tableAP.cast$n_ID
                            ,"Percent_MT"        = tableAP.cast$w.percent_MT
                            ,"SE_MT"             = tableAP.cast$w.SE_MT
                            ,"n_MT"              = tableAP.cast$n_MT
                            ,"Percent_OR"        = tableAP.cast$w.percent_OR
                            ,"SE_OR"             = tableAP.cast$w.SE_OR
                            ,"n_OR"              = tableAP.cast$n_OR
                            ,"Percent_WA"        = tableAP.cast$w.percent_WA
                            ,"SE_WA"             = tableAP.cast$w.SE_WA
                            ,"n_WA"              = tableAP.cast$n_WA
                            ,"Percent_Region"    = tableAP.cast$w.percent_Region
                            ,"SE_Region"         = tableAP.cast$w.SE_Region
                            ,"n_Region"          = tableAP.cast$n_Region
                            ,"EB_ID"             = tableAP.cast$EB_ID
                            ,"EB_MT"             = tableAP.cast$EB_MT
                            ,"EB_OR"             = tableAP.cast$EB_OR
                            ,"EB_WA"             = tableAP.cast$EB_WA
                            ,"EB_Region"         = tableAP.cast$EB_Region
)

tableAP.table.MH <- tableAP.table[which(tableAP.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(tableAP.table.MH, "MH", "Table AP", weighted = TRUE)



################################
# Unweighted Analysis
################################
tableAP.final <- proportions_two_groups_unweighted(CustomerLevelData     = tableAP.data
                                                   , valueVariable       = 'count'
                                                   , columnVariable      = 'State'
                                                   , rowVariable         = 'rvalue.bins.MH'
                                                   , aggregateColumnName = "Region"
)

tableAP.cast <- dcast(setDT(tableAP.final),
                      formula   = BuildingType + rvalue.bins.MH ~ State,
                      value.var = c("Percent", "SE", "Count", "n"))

tableAP.table <- data.frame("BuildingType"       = tableAP.cast$BuildingType
                            ,"Insulation.Levels" = tableAP.cast$rvalue.bins.MH
                            ,"Percent_ID"        = tableAP.cast$Percent_ID
                            ,"SE_ID"             = tableAP.cast$SE_ID
                            ,"n_ID"              = tableAP.cast$n_ID
                            ,"Percent_MT"        = tableAP.cast$Percent_MT
                            ,"SE_MT"             = tableAP.cast$SE_MT
                            ,"n_MT"              = tableAP.cast$n_MT
                            ,"Percent_OR"        = tableAP.cast$Percent_OR
                            ,"SE_OR"             = tableAP.cast$SE_OR
                            ,"n_OR"              = tableAP.cast$n_OR
                            ,"Percent_WA"        = tableAP.cast$Percent_WA
                            ,"SE_WA"             = tableAP.cast$SE_WA
                            ,"n_WA"              = tableAP.cast$n_WA
                            ,"Percent_Region"    = tableAP.cast$Percent_Region
                            ,"SE_Region"         = tableAP.cast$SE_Region
                            ,"n_Region"          = tableAP.cast$n_Region
)

tableAP.table.MH <- tableAP.table[which(tableAP.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(tableAP.table.MH, "MH", "Table AP", weighted = FALSE)







#############################################################################################
# Item 13: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, IDAHO  (SF table 20)
#############################################################################################
## Note: For this table, you must run up to item12.dat1 for the cleaned data
item13.data <- item12.data[which(item12.data$State == "ID"),]


############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item13.summary <- proportionRowsAndColumns1(CustomerLevelData     = item13.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item13.summary <- item13.summary[which(item13.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item13.all.frame.types <- proportions_one_group(item13.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item13.all.insul.levels <-  proportions_one_group(item13.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item13.final <- rbind.data.frame(item13.summary
                                 , item13.all.frame.types
                                 , item13.all.insul.levels
                                 , stringsAsFactors = F)
item13.final <- item13.final[which(item13.final$rvalue.bins.SF != "Total"),]
item13.final$HomeYearBuilt_bins3[which(item13.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

item13.cast <- dcast(setDT(item13.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item13.table <- data.frame("BuildingType"     = item13.cast$BuildingType
                           ,"Housing.Vintage" = item13.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item13.cast$w.percent_R0
                           ,"SE.R0"           = item13.cast$w.SE_R0
                           ,"Percent.R1.R10"  = item13.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item13.cast$w.SE_R1.R10
                           ,"Percent.R11.R16" = item13.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item13.cast$w.SE_R11.R16
                           ,"Percent.R17.R22" = item13.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item13.cast$w.SE_R17.R22
                           ,"Percent.RGT22"   = item13.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item13.cast$w.SE_RGT22
                           ,"n"               = item13.cast$`n_All Housing Vintages`
                           ,"EB.R0"           = item13.cast$EB_R0
                           ,"EB.R1.R10"       = item13.cast$EB_R1.R10
                           ,"EB.R11.R16"      = item13.cast$EB_R11.R16
                           ,"EB.R17.R22"      = item13.cast$EB_R17.R22
                           ,"EB.RGT22"        = item13.cast$EB_RGT22
)

# row ordering example code
unique(item13.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item13.table <- item13.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item13.table <- data.frame(item13.table)

item13.table.SF <- item13.table[which(item13.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item13.table.SF, "SF", "Table 20", weighted = TRUE)



################################
# Unweighted Analysis
################################
item13.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item13.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item13.summary <- item13.summary[which(item13.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item13.all.frame.types <- proportions_one_group(item13.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted         = FALSE
                                                ,two.prop.total   = TRUE
)

## Summary for only "All Insulation Levels"
item13.all.insul.levels <-  proportions_one_group(item13.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted         = FALSE
                                                  ,two.prop.total   = TRUE
)


#merge together!
item13.final <- rbind.data.frame(item13.summary
                                 , item13.all.frame.types
                                 , item13.all.insul.levels
                                 , stringsAsFactors = F)
item13.final <- item13.final[which(item13.final$rvalue.bins.SF != "Total"),]
item13.final$HomeYearBuilt_bins3[which(item13.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


##cast data
item13.cast <- dcast(setDT(item13.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))


item13.table <- data.frame("BuildingType"     = item13.cast$BuildingType
                           ,"Housing.Vintage" = item13.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item13.cast$Percent_R0
                           ,"SE.R0"           = item13.cast$SE_R0
                           ,"Percent.R1.R10"  = item13.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item13.cast$SE_R1.R10
                           ,"Percent.R11.R16" = item13.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item13.cast$SE_R11.R16
                           ,"Percent.R17.R22" = item13.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item13.cast$SE_R17.R22
                           ,"Percent.RGT22"   = item13.cast$Percent_RGT22
                           ,"SE.RGT22"        = item13.cast$SE_RGT22
                           ,"n"               = item13.cast$`n_All Housing Vintages`
)

# row ordering example code
unique(item13.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item13.table <- item13.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item13.table <- data.frame(item13.table)

item13.table.SF <- item13.table[which(item13.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item13.table.SF, "SF", "Table 20", weighted = FALSE)






#############################################################################################
# Item 14: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, MONTANA  (SF table 21)
#############################################################################################
## Note: For this table, you must run up to item12.dat1 for the cleaned data
item14.data <- item12.data[which(item12.data$State == "MT"),]


############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item14.summary <- proportionRowsAndColumns1(CustomerLevelData     = item14.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item14.summary <- item14.summary[which(item14.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item14.all.frame.types <- proportions_one_group(item14.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted         = TRUE
                                                ,two.prop.total   = TRUE
)

## Summary for only "All Insulation Levels"
item14.all.insul.levels <-  proportions_one_group(item14.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted         = TRUE
                                                  ,two.prop.total   = TRUE
)


#merge together!
item14.final <- rbind.data.frame(item14.summary
                                 , item14.all.frame.types
                                 , item14.all.insul.levels
                                 , stringsAsFactors = F)
item14.final <- item14.final[which(item14.final$rvalue.bins.SF != "Total"),]
item14.final$HomeYearBuilt_bins3[which(item14.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

item14.cast <- dcast(setDT(item14.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item14.table <- data.frame("BuildingType"     = item14.cast$BuildingType
                           ,"Housing.Vintage" = item14.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item14.cast$w.percent_R0
                           ,"SE.R0"           = item14.cast$w.SE_R0
                           ,"Percent.R1.R10"  = item14.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item14.cast$w.SE_R1.R10
                           ,"Percent.R11.R16" = item14.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item14.cast$w.SE_R11.R16
                           ,"Percent.R17.R22" = item14.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item14.cast$w.SE_R17.R22
                           ,"Percent.RGT22"   = item14.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item14.cast$w.SE_RGT22
                           ,"n"               = item14.cast$`n_All Housing Vintages`
                           ,"EB.R0"           = item14.cast$EB_R0
                           ,"EB.R1.R10"       = item14.cast$EB_R1.R10
                           ,"EB.R11.R16"      = item14.cast$EB_R11.R16
                           ,"EB.R17.R22"      = item14.cast$EB_R17.R22
                           ,"EB.RGT22"        = item14.cast$EB_RGT22
)

# row ordering example code
unique(item14.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item14.table <- item14.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item14.table <- data.frame(item14.table)

item14.table.SF <- item14.table[which(item14.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item14.table.SF, "SF", "Table 21", weighted = TRUE)



################################
# Unweighted Analysis
################################
item14.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item14.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item14.summary <- item14.summary[which(item14.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item14.all.frame.types <- proportions_one_group(item14.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item14.all.insul.levels <-  proportions_one_group(item14.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item14.final <- rbind.data.frame(item14.summary
                                 , item14.all.frame.types
                                 , item14.all.insul.levels
                                 , stringsAsFactors = F)
item14.final <- item14.final[which(item14.final$rvalue.bins.SF != "Total"),]
item14.final$HomeYearBuilt_bins3[which(item14.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


##cast data
item14.cast <- dcast(setDT(item14.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))


item14.table <- data.frame("BuildingType"     = item14.cast$BuildingType
                           ,"Housing.Vintage" = item14.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item14.cast$Percent_R0
                           ,"SE.R0"           = item14.cast$SE_R0
                           ,"Percent.R1.R10"  = item14.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item14.cast$SE_R1.R10
                           ,"Percent.R11.R16" = item14.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item14.cast$SE_R11.R16
                           ,"Percent.R17.R22" = item14.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item14.cast$SE_R17.R22
                           ,"Percent.RGT22"   = item14.cast$Percent_RGT22
                           ,"SE.RGT22"        = item14.cast$SE_RGT22
                           ,"n"               = item14.cast$`n_All Housing Vintages`
)

# row ordering example code
unique(item14.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item14.table <- item14.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item14.table <- data.frame(item14.table)


item14.table.SF <- item14.table[which(item14.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item14.table.SF, "SF", "Table 21", weighted = FALSE)






#############################################################################################
# Item 15: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, OREGON  (SF table 22)
#############################################################################################
## Note: For this table, you must run up to item12.dat1 for the cleaned data
item15.data <- item12.data[which(item12.data$State == "OR"),]


############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item15.summary <- proportionRowsAndColumns1(CustomerLevelData     = item15.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item15.summary <- item15.summary[which(item15.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item15.all.frame.types <- proportions_one_group(item15.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item15.all.insul.levels <-  proportions_one_group(item15.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item15.final <- rbind.data.frame(item15.summary
                                 , item15.all.frame.types
                                 , item15.all.insul.levels
                                 , stringsAsFactors = F)
item15.final <- item15.final[which(item15.final$rvalue.bins.SF != "Total"),]
item15.final$HomeYearBuilt_bins3[which(item15.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

item15.cast <- dcast(setDT(item15.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item15.table <- data.frame("BuildingType"     = item15.cast$BuildingType
                           ,"Housing.Vintage" = item15.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item15.cast$w.percent_R0
                           ,"SE.R0"           = item15.cast$w.SE_R0
                           ,"Percent.R1.R10"  = item15.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item15.cast$w.SE_R1.R10
                           ,"Percent.R11.R16" = item15.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item15.cast$w.SE_R11.R16
                           ,"Percent.R17.R22" = item15.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item15.cast$w.SE_R17.R22
                           ,"Percent.RGT22"   = item15.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item15.cast$w.SE_RGT22
                           ,"n"               = item15.cast$`n_All Housing Vintages`
                           ,"EB.R0"           = item15.cast$EB_R0
                           ,"EB.R1.R10"       = item15.cast$EB_R1.R10
                           ,"EB.R11.R16"      = item15.cast$EB_R11.R16
                           ,"EB.R17.R22"      = item15.cast$EB_R17.R22
                           ,"EB.RGT22"        = item15.cast$EB_RGT22
)

# row ordering example code
unique(item15.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item15.table <- item15.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item15.table <- data.frame(item15.table)

item15.table.SF <- item15.table[which(item15.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item15.table.SF, "SF", "Table 22", weighted = TRUE)



################################
# Unweighted Analysis
################################
item15.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item15.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item15.summary <- item15.summary[which(item15.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item15.all.frame.types <- proportions_one_group(item15.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item15.all.insul.levels <-  proportions_one_group(item15.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item15.final <- rbind.data.frame(item15.summary
                                 , item15.all.frame.types
                                 , item15.all.insul.levels
                                 , stringsAsFactors = F)
item15.final <- item15.final[which(item15.final$rvalue.bins.SF != "Total"),]
item15.final$HomeYearBuilt_bins3[which(item15.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


##cast data
item15.cast <- dcast(setDT(item15.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))


item15.table <- data.frame("BuildingType"     = item15.cast$BuildingType
                           ,"Housing.Vintage" = item15.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item15.cast$Percent_R0
                           ,"SE.R0"           = item15.cast$SE_R0
                           ,"Percent.R1.R10"  = item15.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item15.cast$SE_R1.R10
                           ,"Percent.R11.R16" = item15.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item15.cast$SE_R11.R16
                           ,"Percent.R17.R22" = item15.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item15.cast$SE_R17.R22
                           ,"Percent.RGT22"   = item15.cast$Percent_RGT22
                           ,"SE.RGT22"        = item15.cast$SE_RGT22
                           ,"n"               = item15.cast$`n_All Housing Vintages`
)

# row ordering example code
unique(item15.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item15.table <- item15.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item15.table <- data.frame(item15.table)

item15.table.SF <- item15.table[which(item15.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item15.table.SF, "SF", "Table 22", weighted = FALSE)






#############################################################################################
# Item 16: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, WASHINGTON  (SF table 23)
#############################################################################################
## Note: For this table, you must run up to item12.dat1 for the cleaned data
item16.data <- item12.data[which(item12.data$State == "WA"),]


############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item16.summary <- proportionRowsAndColumns1(CustomerLevelData     = item16.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item16.summary <- item16.summary[which(item16.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item16.all.frame.types <- proportions_one_group(item16.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item16.all.insul.levels <-  proportions_one_group(item16.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item16.final <- rbind.data.frame(item16.summary
                                 , item16.all.frame.types
                                 , item16.all.insul.levels
                                 , stringsAsFactors = F)
item16.final <- item16.final[which(item16.final$rvalue.bins.SF != "Total"),]
item16.final$HomeYearBuilt_bins3[which(item16.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

item16.cast <- dcast(setDT(item16.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item16.table <- data.frame("BuildingType"     = item16.cast$BuildingType
                           ,"Housing.Vintage" = item16.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item16.cast$w.percent_R0
                           ,"SE.R0"           = item16.cast$w.SE_R0
                           ,"Percent.R1.R10"  = item16.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item16.cast$w.SE_R1.R10
                           ,"Percent.R11.R16" = item16.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item16.cast$w.SE_R11.R16
                           ,"Percent.R17.R22" = item16.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item16.cast$w.SE_R17.R22
                           ,"Percent.RGT22"   = item16.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item16.cast$w.SE_RGT22
                           ,"n"               = item16.cast$`n_All Housing Vintages`
                           ,"EB.R0"           = item16.cast$EB_R0
                           ,"EB.R1.R10"       = item16.cast$EB_R1.R10
                           ,"EB.R11.R16"      = item16.cast$EB_R11.R16
                           ,"EB.R17.R22"      = item16.cast$EB_R17.R22
                           ,"EB.RGT22"        = item16.cast$EB_RGT22
                           
)

# row ordering example code
unique(item16.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item16.table <- item16.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item16.table <- data.frame(item16.table)

item16.table.SF <- item16.table[which(item16.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item16.table.SF, "SF", "Table 23", weighted = TRUE)


################################
# Unweighted Analysis
################################
item16.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item16.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item16.summary <- item16.summary[which(item16.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item16.all.frame.types <- proportions_one_group(item16.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item16.all.insul.levels <-  proportions_one_group(item16.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item16.final <- rbind.data.frame(item16.summary
                                 , item16.all.frame.types
                                 , item16.all.insul.levels
                                 , stringsAsFactors = F)
item16.final <- item16.final[which(item16.final$rvalue.bins.SF != "Total"),]
item16.final$HomeYearBuilt_bins3[which(item16.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


##cast data
item16.cast <- dcast(setDT(item16.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))


item16.table <- data.frame("BuildingType"     = item16.cast$BuildingType
                           ,"Housing.Vintage" = item16.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item16.cast$Percent_R0
                           ,"SE.R0"           = item16.cast$SE_R0
                           ,"Percent.R1.R10"  = item16.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item16.cast$SE_R1.R10
                           ,"Percent.R11.R16" = item16.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item16.cast$SE_R11.R16
                           ,"Percent.R17.R22" = item16.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item16.cast$SE_R17.R22
                           ,"Percent.RGT22"   = item16.cast$Percent_RGT22
                           ,"SE.RGT22"        = item16.cast$SE_RGT22
                           ,"n"               = item16.cast$`n_All Housing Vintages`
)

# row ordering example code
unique(item16.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item16.table <- item16.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item16.table <- data.frame(item16.table)

item16.table.SF <- item16.table[which(item16.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item16.table.SF, "SF", "Table 23", weighted = FALSE)






#############################################################################################
# Item 18: DISTRIBUTION OF OBSERVED WALL SHEATHING INSULATION BY FRAMING TYPE (SF table 25)
#############################################################################################
item18.dat <- prep.dat4.9#[which(prep.dat4.9$Wall.Type %notin% c("Masonry", "Masonry (Basement)", "Adiabatic")),]
names(item18.dat)[which(names(item18.dat) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"
item18.dat <- item18.dat[which(!is.na(item18.dat$Wall.Type)),]

item18.dat$Insulation.Levels <- item18.dat$exterior.inches1
unique(item18.dat$Insulation.Levels)

item18.merge <- left_join(rbsa.dat, item18.dat)

item18.customer <- summarise(group_by(item18.merge, CK_Cadmus_ID, Wall.Type)
                             ,insulation.levels = sum(Insulation.Levels))

item18.customer$insulation.levels[which(item18.customer$insulation.levels == 0)] <- "None"

item18.merge <- left_join(rbsa.dat, item18.customer)
item18.merge <- item18.merge[which(!is.na(item18.merge$Wall.Type)),]
unique(item18.merge$Wall.Type)
item18.merge$count <- 1

item18.data <- weightedData(unique(item18.merge[-which(colnames(item18.merge) %in% c("Category"
                                                                                     ,"Wall.Type"
                                                                                     ,"count"
                                                                                     ,"insulation.levels"))]))
item18.data <- left_join(item18.data, item18.merge[which(colnames(item18.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Category"
                                                                                       ,"Wall.Type"
                                                                                       ,"count"
                                                                                       ,"insulation.levels"))])



############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item18.by.frame.type <- proportionRowsAndColumns1(item18.data
                                                  , valueVariable       = 'count'
                                                  , columnVariable      = 'Wall.Type'
                                                  , rowVariable         = 'insulation.levels'
                                                  , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item18.across.frame.types <- proportions_one_group(item18.data
                                                   , valueVariable    = 'count'
                                                   , groupingVariable = 'insulation.levels'
                                                   , total.name       = 'All Framing Types'
                                                   , columnName       = 'Wall.Type'
                                                   , weighted = TRUE
                                                   ,two.prop.total = TRUE
)
# row bind summaries
item18.final <- rbind.data.frame(item18.by.frame.type, item18.across.frame.types, stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item18.final <- item18.final[which(item18.final$Wall.Type != "Remove"),]

item18.cast <- dcast(setDT(item18.final),
                     formula   = BuildingType +  Wall.Type ~ insulation.levels,
                     value.var = c("w.percent", "w.SE", "count","n","N", "EB"))
names(item18.cast)

item18.table <- data.frame("BuildingType"       = item18.cast$BuildingType
                           ,"Wall.Type"         = item18.cast$Wall.Type
                           ,"Percent_0.25_inch" = item18.cast$w.percent_0.25
                           ,"SE_0.25_inch"      = item18.cast$w.SE_0.25
                           ,"Percent_0.5_inch"  = item18.cast$w.percent_0.5
                           ,"SE_0.5_inch"       = item18.cast$w.SE_0.5
                           ,"Percent_0.75_inch" = item18.cast$w.percent_0.75
                           ,"SE_0.75_inch"      = item18.cast$w.SE_0.75
                           ,"Percent_1_inch"    = item18.cast$w.percent_1
                           ,"SE_1_inch"         = item18.cast$w.SE_1
                           ,"Percent_1.5_inch"  = item18.cast$w.percent_1.5
                           ,"SE_1.5_inch"       = item18.cast$w.SE_1.5
                           ,"Percent_2_inch"    = item18.cast$w.percent_2
                           ,"SE_2_inch"         = item18.cast$w.SE_2
                           ,"Percent_3_inch"    = item18.cast$w.percent_3
                           ,"SE_3_inch"         = item18.cast$w.SE_3
                           ,"Percent_4_inch"    = item18.cast$w.percent_4
                           ,"SE_4_inch"         = item18.cast$w.SE_4
                           ,"Percent_None"      = item18.cast$w.percent_None
                           ,"SE_None"           = item18.cast$w.SE_None
                           ,"n"                 = item18.cast$n_Total
                           ,"EB_0.25_inch"      = item18.cast$EB_0.25
                           ,"EB_0.5_inch"       = item18.cast$EB_0.5
                           ,"EB_0.75_inch"      = item18.cast$EB_0.75
                           ,"EB_1_inch"         = item18.cast$EB_1
                           ,"EB_1.5_inch"       = item18.cast$EB_1.5
                           ,"EB_2_inch"         = item18.cast$EB_2
                           ,"EB_3_inch"         = item18.cast$EB_3
                           ,"EB_4_inch"         = item18.cast$EB_4
                           ,"EB_None"           = item18.cast$EB_None
)

# row ordering example code
levels(item18.table$Wall.Type)
rowOrder <- c("Framed 2x2"
              ,"Framed 2x3"
              ,"Framed 2x4"
              ,"Framed 2x6"
              ,"Framed 2x8"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"Masonry"
              ,"Masonry (Basement)"
              ,"ICF"
              ,"SIP"
              ,"Log"
              ,"Unknown"
              ,"Knee Wall"
              ,"Adiabatic"
              ,"All Framing Types")
item18.table <- item18.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item18.table <- data.frame(item18.table)


item18.table.SF <- item18.table[which(item18.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item18.table.SF, "SF", "Table 25", weighted = TRUE)


############################################################################################################
# Unweighted Analysis - Single Family
############################################################################################################
item18.by.frame.type <- proportions_two_groups_unweighted(item18.data
                                                          , valueVariable       = 'count'
                                                          , columnVariable      = 'Wall.Type'
                                                          , rowVariable         = 'insulation.levels'
                                                          , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item18.across.frame.types <- proportions_one_group(item18.data
                                                   , valueVariable    = 'count'
                                                   , groupingVariable = 'insulation.levels'
                                                   , total.name       = 'All Framing Types'
                                                   , columnName       = 'Wall.Type'
                                                   , weighted = FALSE
                                                   ,two.prop.total = TRUE
)
# row bind summaries
item18.final <- rbind.data.frame(item18.by.frame.type, item18.across.frame.types, stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item18.final <- item18.final[which(item18.final$Wall.Type != "Remove"),]



item18.cast <- dcast(setDT(item18.final),
                     formula   = BuildingType +  Wall.Type ~ insulation.levels, sum,
                     value.var = c("Percent", "SE", "Count","n"))

item18.table <- data.frame("BuildingType"       = item18.cast$BuildingType
                           ,"Wall.Type"         = item18.cast$Wall.Type
                           ,"Percent_0.25_inch" = item18.cast$Percent_0.25
                           ,"SE_0.25_inch"      = item18.cast$SE_0.25
                           ,"Percent_0.5_inch"  = item18.cast$Percent_0.5
                           ,"SE_0.5_inch"       = item18.cast$SE_0.5
                           ,"Percent_0.75_inch" = item18.cast$Percent_0.75
                           ,"SE_0.75_inch"      = item18.cast$SE_0.75
                           ,"Percent_1_inch"    = item18.cast$Percent_1
                           ,"SE_1_inch"         = item18.cast$SE_1
                           ,"Percent_1.5_inch"  = item18.cast$Percent_1.5
                           ,"SE_1.5_inch"       = item18.cast$SE_1.5
                           ,"Percent_2_inch"    = item18.cast$Percent_2
                           ,"SE_2_inch"         = item18.cast$SE_2
                           ,"Percent_3_inch"    = item18.cast$Percent_3
                           ,"SE_3_inch"         = item18.cast$SE_3
                           ,"Percent_4_inch"    = item18.cast$Percent_4
                           ,"SE_4_inch"         = item18.cast$SE_4
                           ,"Percent_None"      = item18.cast$Percent_None
                           ,"SE_None"           = item18.cast$SE_None
                           ,"n"                 = item18.cast$n_Total
)

# row ordering example code
levels(item18.table$Wall.Type)
rowOrder <- c("Framed 2x2"
              ,"Framed 2x3"
              ,"Framed 2x4"
              ,"Framed 2x6"
              ,"Framed 2x8"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"Masonry"
              ,"Masonry (Basement)"
              ,"ICF"
              ,"SIP"
              ,"Log"
              ,"Unknown"
              ,"Knee Wall"
              ,"Adiabatic"
              ,"All Framing Types")
item18.table <- item18.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item18.table <- data.frame(item18.table)

item18.table.SF <- item18.table[which(item18.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item18.table.SF, "SF", "Table 25", weighted = FALSE)






#############################################################################################
# Item 17: DISTRIBUTION OF Masonry Wall Insulation Levels by Vintage (SF table 24)
#############################################################################################
item17.dat <- prep.dat5[grep("masonry|icf",prep.dat5$Wall.Type, ignore.case = T),]

#weight the u factor per home -- where weights are the wall area within home
item17.weightedU <- summarise(group_by(item17.dat, CK_Cadmus_ID)
                              ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
item17.weightedU$aveRval <- (1 / as.numeric(as.character(item17.weightedU$aveUval)))
item17.weightedU$aveRval[which(item17.weightedU$aveRval %in% c("NaN",1))] <- 0
item17.weightedU$aveUval[which(item17.weightedU$aveUval == "NaN")] <- 1

unique(item17.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
item17.wall.unique <- unique(item17.dat[which(colnames(item17.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
item17.dat1 <- left_join(item17.weightedU, item17.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item17.dat2 <- left_join(item17.dat1, rbsa.dat)
item17.dat2$aveUval[which(is.na(item17.dat2$aveUval))] <- 0
item17.dat2$aveRval[which(is.na(item17.dat2$aveRval))] <- 0

#Bin R values -- SF only
item17.dat2$rvalue.bins <- "Unknown"
item17.dat2$rvalue.bins[which(item17.dat2$aveRval < 1)] <- "None"
item17.dat2$rvalue.bins[which(item17.dat2$aveRval >=  1 & item17.dat2$aveRval < 10)]  <- "R1.R9"
item17.dat2$rvalue.bins[which(item17.dat2$aveRval >= 10 & item17.dat2$aveRval < 16)]  <- "R10.R15"
item17.dat2$rvalue.bins[which(item17.dat2$aveRval >= 16 & item17.dat2$aveRval < 21)]  <- "R16.R20"
item17.dat2$rvalue.bins[which(item17.dat2$aveRval >= 21)] <- "RGT21"
unique(item17.dat2$rvalue.bins)

item17.dat2$count <- 1

item17.dat3 <- item17.dat2[which(item17.dat2$rvalue.bins != "Unknown"),]
colnames(item17.dat3)

item17.merge <- left_join(rbsa.dat, item17.dat3)
item17.merge <- item17.merge[which(!is.na(item17.merge$count)),]
item17.merge <- item17.merge[which(!is.na(item17.merge$HomeYearBuilt)),]

item17.data <- weightedData(unique(item17.merge[-which(colnames(item17.merge) %in% c("Wall.Type"
                                                                                     ,"aveUval"
                                                                                     ,"aveRval"
                                                                                     ,"rvalue.bins"
                                                                                     ,"count"))]))
item17.data <- left_join(item17.data, item17.merge[which(colnames(item17.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins"
                                                                                       ,"count"))])
################################################
# Weighted Analysis - Single Family
################################################
item17.summary <- proportionRowsAndColumns1(CustomerLevelData     = item17.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins'
                                            , aggregateColumnName = "All Housing Vintages"
)
item17.summary <- item17.summary[which(item17.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item17.all.frame.types <- proportions_one_group(item17.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item17.all.insul.levels <-  proportions_one_group(item17.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item17.final <- rbind.data.frame(item17.summary
                                 , item17.all.frame.types
                                 , item17.all.insul.levels
                                 , stringsAsFactors = F)
item17.final <- item17.final[which(item17.final$rvalue.bins != "Total"),]
item17.final$HomeYearBuilt_bins3[which(item17.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


item17.cast <- dcast(setDT(item17.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ rvalue.bins,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

#join all insulation levels onto rvalue summary
item17.table <- data.frame("BuildingType"     = item17.cast$BuildingType
                           ,"Housing.Vintage" = item17.cast$HomeYearBuilt_bins3
                           ,"Percent.None"    = item17.cast$w.percent_None
                           ,"SE.None"         = item17.cast$w.SE_None
                           ,"Percent.R1.R9"   = item17.cast$w.percent_R1.R9
                           ,"SE.R1.R9"        = item17.cast$w.SE_R1.R9
                           ,"Percent.R10.R15" = item17.cast$w.percent_R10.R15
                           ,"SE.R10.R15"      = item17.cast$w.SE_R10.R15
                           ,"Percent.R16.R20" = item17.cast$w.percent_R16.R20
                           ,"SE.R16.R20"      = item17.cast$w.SE_R16.R20
                           ,"Percent.RGT21"   = item17.cast$w.percent_RGT21
                           ,"SE.RGT21"        = item17.cast$w.SE_RGT21
                           ,"n"               = item17.cast$`n_All Housing Vintages`
                           ,'EB.None'         = item17.cast$EB_None
                           ,"EB.R1.R9"        = item17.cast$EB_R1.R9
                           ,"EB.R10.R15"      = item17.cast$EB_R10.R15
                           ,"EB.R16.R20"      = item17.cast$EB_R16.R20
                           ,"EB.R21"          = item17.cast$EB_RGT21
                           )

# row ordering example code
levels(item17.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item17.table <- item17.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item17.table <- data.frame(item17.table)


item17.table.SF <- item17.table[which(item17.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item17.table.SF, "SF", "Table 24", weighted = TRUE)


###############################################
# unweighted Analysis - Single Family
###############################################
item17.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item17.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins'
                                            , aggregateColumnName = "All Housing Vintages"
)
item17.summary <- item17.summary[which(item17.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item17.all.frame.types <- proportions_one_group(item17.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item17.all.insul.levels <-  proportions_one_group(item17.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item17.final <- rbind.data.frame(item17.summary
                                 , item17.all.frame.types
                                 , item17.all.insul.levels
                                 , stringsAsFactors = F)
item17.final <- item17.final[which(item17.final$rvalue.bins != "Total"),]
item17.final$HomeYearBuilt_bins3[which(item17.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


item17.cast <- dcast(setDT(item17.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ rvalue.bins,
                     value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item17.table <- data.frame("BuildingType"     = item17.cast$BuildingType
                           ,"Housing.Vintage" = item17.cast$HomeYearBuilt_bins3
                           ,"Percent.None"    = item17.cast$Percent_None
                           ,"SE.None"         = item17.cast$SE_None
                           ,"Percent.R1.R9"   = item17.cast$Percent_R1.R9
                           ,"SE.R1.R9"        = item17.cast$SE_R1.R9
                           ,"Percent.R10.R15" = item17.cast$Percent_R10.R15
                           ,"SE.R10.R15"      = item17.cast$SE_R10.R15
                           ,"Percent.R16.R20" = item17.cast$Percent_R16.R20
                           ,"SE.R16.R20"      = item17.cast$SE_R16.R20
                           ,"Percent.RGT21"   = item17.cast$Percent_RGT21
                           ,"SE.RGT21"        = item17.cast$SE_RGT21
                           ,"n"               = item17.cast$`n_All Housing Vintages`
)

# row ordering example code
levels(item17.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item17.table <- item17.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item17.table <- data.frame(item17.table)


item17.table.SF <- item17.table[which(item17.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item17.table.SF, "SF", "Table 24", weighted = FALSE)








#############################################################################################
# Item 175: DISTRIBUTION OF WALL U-VALUE BY STATE  (MH table 17)
#############################################################################################
## Note: For this table, you must run up to prep.dat7 for the cleaned data
item175.dat <- envelope.dat[which(names(envelope.dat) %in% c("CK_Cadmus_ID", "Wall.U-Value.-.For.Calcs"))]
item175.dat$`Wall.U-Value.-.For.Calcs` <- as.numeric(as.character(item175.dat$`Wall.U-Value.-.For.Calcs`))

item175.dat1 <- item175.dat[which(!is.na(item175.dat$`Wall.U-Value.-.For.Calcs`)),]

item175.summary <- data.frame(ddply(item175.dat1
                                    ,c("CK_Cadmus_ID"), summarise
                                    ,aveUval = mean(`Wall.U-Value.-.For.Calcs`)), stringsAsFactors = F)


item175.summary$count <- 1
colnames(item175.summary)

item175.merge <- left_join(rbsa.dat, item175.summary)
item175.merge <- item175.merge[which(!is.na(item175.merge$aveUval)),]



############################################################################################################
# Apply weights
############################################################################################################
item175.data <- weightedData(unique(item175.merge[-which(colnames(item175.merge) %in% c("aveUval"
                                                                                     ,"count"))]))
item175.data <- left_join(item175.data, item175.merge[which(colnames(item175.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"aveUval"
                                                                                       ,"count"))])


############################################################################################################
# Weighted Analysis - Manufactured
############################################################################################################
item175.final <- mean_one_group(CustomerLevelData = item175.data
                                ,valueVariable = 'aveUval'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")

item175.table.MH <- item175.final[which(item175.final$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item175.table.MH, "MH", "Table 17", weighted = TRUE)


############################################################################################################
# Unweighted Analysis - Manufactured
############################################################################################################
item175.final <- mean_one_group_unweighted(CustomerLevelData = item175.data
                                ,valueVariable = 'aveUval'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")

item175.table.MH <- item175.final[which(item175.final$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item175.table.MH, "MH", "Table 17", weighted = FALSE)












#############################################################################################
# Item 235: DISTRIBUTION OF WALL INSULATION BY WALL TYPE  (MF table 27)
#############################################################################################
#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_SiteID, Wall.Type)
                       ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)
unique(weightedU$aveUval)


#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
unique(weightedU$aveRval)
unique(weightedU$aveUval)

# get unique cadmus IDs and building types for this subset of data
wall.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_SiteID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.dat6 <- left_join(weightedU, wall.unique, by = "CK_SiteID")

#merge weighted u values onto cleaned RBSA data
prep.dat7 <- left_join(prep.dat6, rbsa.dat.MF, by = c("CK_SiteID" = "CK_Building_ID"))
item235.dat <- prep.dat7[which(!is.na(prep.dat7$CK_Cadmus_ID)),]
item235.dat$Wall.Type[grep("framed",item235.dat$Wall.Type,ignore.case = T)] <- "Frame"
item235.dat$Wall.Type[grep("masonry|concrete",item235.dat$Wall.Type,ignore.case = T)] <- "Masonry/Concrete"
item235.dat$Wall.Type[which(item235.dat$Wall.Type %notin% c("Frame","Masonry/Concrete"))] <- "Other"


#Bin R values -- MF only
item235.dat$rvalue.bins <- "Unknown"
item235.dat$rvalue.bins[which(item235.dat$aveRval >=  0  & item235.dat$aveRval < 8)]  <- "R0.R7"
item235.dat$rvalue.bins[which(item235.dat$aveRval >= 8   & item235.dat$aveRval < 14)]  <- "R8.R13"
item235.dat$rvalue.bins[which(item235.dat$aveRval >= 14  & item235.dat$aveRval < 21)]  <- "R14.R20"
item235.dat$rvalue.bins[which(item235.dat$aveRval >= 21  & item235.dat$aveRval < 24)]  <- "R21.R23"
item235.dat$rvalue.bins[which(item235.dat$aveRval >= 24)] <- "RGT23"
unique(item235.dat$rvalue.bins)

item235.dat$count <- 1

item235.dat1 <- item235.dat[which(item235.dat$rvalue.bins != "Unknown"),]
colnames(item235.dat1)

item235.merge <- left_join(rbsa.dat.MF, item235.dat1)
item235.merge <- item235.merge[which(!is.na(item235.merge$count)),]

item235.data <- weightedData(unique(item235.merge[-which(colnames(item235.merge) %in% c("CK_SiteID"
                                                                                        ,"Wall.Type"
                                                                                        ,"aveUval"
                                                                                        ,"aveRval"
                                                                                        ,"rvalue.bins"
                                                                                        ,"count"))]))
item235.data <- left_join(item235.data, item235.merge[which(colnames(item235.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"CK_SiteID"
                                                                                           ,"Wall.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"rvalue.bins"
                                                                                           ,"count"))])



################################
# Weighted Analysis
################################
item235.summary <- proportionRowsAndColumns1(CustomerLevelData     = item235.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'Wall.Type'
                                            , rowVariable         = 'rvalue.bins'
                                            , aggregateColumnName = "All Types"
)
item235.summary <- item235.summary[which(item235.summary$Wall.Type != "All Types"),]

## Summary only for "All Frame Types"
item235.all.frame.types <- proportions_one_group(CustomerLevelData = item235.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Types"
                                                ,columnName       = "Wall.Type"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

#merge together!
item235.final <- rbind.data.frame(item235.summary
                                 , item235.all.frame.types, stringsAsFactors = F)


##cast data
item235.cast <- dcast(setDT(item235.final),
                     formula   = BuildingType + Wall.Type ~ rvalue.bins,
                     value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

#join all insulation levels onto rvalue summary
item235.table <- data.frame("BuildingType"                  = item235.cast$BuildingType
                           ,"Wall.Type"                     = item235.cast$Wall.Type
                           ,"Percent.R0.R7"                 = item235.cast$w.percent_R0.R7
                           ,"SE.R0.R7"                      = item235.cast$w.SE_R0.R7
                           ,"Percent.R8.R13"                = item235.cast$w.percent_R8.R13
                           ,"SE.R8.R13"                     = item235.cast$w.SE_R8.R13
                           ,"Percent.R14.R20"               = item235.cast$w.percent_R14.R20
                           ,"SE.R14.R20"                    = item235.cast$w.SE_R14.R20
                           ,"Percent.R21.R23"               = item235.cast$w.percent_R21.R23
                           ,"SE.R21.R23"                    = item235.cast$w.SE_R21.R23
                           ,"Percent.RGT23"                 = item235.cast$w.percent_RGT23
                           ,"SE.RGT23"                      = item235.cast$w.SE_RGT23
                           ,"n"                             = item235.cast$n_Total
                           ,"EB.R0.R7"                      = item235.cast$EB_R0.R7
                           ,"EB.R8.R13"                     = item235.cast$EB_R8.R13
                           ,"EB.R14.R20"                    = item235.cast$EB_R14.R20
                           ,"EB.R21.R23"                    = item235.cast$EB_R21.R23
                           ,"EB.RGT23"                      = item235.cast$EB_RGT23
)

# row ordering example code
unique(item235.table$Wall.Type)
rowOrder <- c("Frame"
              ,"Other"
              ,"All Types")
item235.table <- item235.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item235.table <- data.frame(item235.table)


item235.table.MF <- item235.table[which(item235.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item235.table.MF, "MF", "Table 27", weighted = TRUE)



################################
# Unweighted Analysis
################################
item235.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item235.data
                                             , valueVariable       = 'count'
                                             , columnVariable      = 'Wall.Type'
                                             , rowVariable         = 'rvalue.bins'
                                             , aggregateColumnName = "All Types"
)
item235.summary <- item235.summary[which(item235.summary$Wall.Type != "All Types"),]

## Summary only for "All Frame Types"
item235.all.frame.types <- proportions_one_group(CustomerLevelData = item235.data
                                                 ,valueVariable    = "count"
                                                 ,groupingVariable = "rvalue.bins"
                                                 ,total.name       = "All Types"
                                                 ,columnName       = "Wall.Type"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE
)

#merge together!
item235.final <- rbind.data.frame(item235.summary
                                  , item235.all.frame.types, stringsAsFactors = F)


##cast data
item235.cast <- dcast(setDT(item235.final),
                      formula   = BuildingType + Wall.Type ~ rvalue.bins,
                      value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item235.table <- data.frame("BuildingType"                  = item235.cast$BuildingType
                            ,"Wall.Type"                     = item235.cast$Wall.Type
                            ,"Percent.R0.R7"                 = item235.cast$Percent_R0.R7
                            ,"SE.R0.R7"                      = item235.cast$SE_R0.R7
                            ,"Percent.R8.R13"                = item235.cast$Percent_R8.R13
                            ,"SE.R8.R13"                     = item235.cast$SE_R8.R13
                            ,"Percent.R14.R20"               = item235.cast$Percent_R14.R20
                            ,"SE.R14.R20"                    = item235.cast$SE_R14.R20
                            ,"Percent.R21.R23"               = item235.cast$Percent_R21.R23
                            ,"SE.R21.R23"                    = item235.cast$SE_R21.R23
                            ,"Percent.RGT23"                 = item235.cast$Percent_RGT23
                            ,"SE.RGT23"                      = item235.cast$SE_RGT23
                            ,"n"                             = item235.cast$n_Total
)

# row ordering example code
unique(item235.table$Wall.Type)
rowOrder <- c("Frame"
              ,"Other"
              ,"All Types")
item235.table <- item235.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item235.table <- data.frame(item235.table)


item235.table.MF <- item235.table[which(item235.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item235.table.MF, "MF", "Table 27", weighted = FALSE)




# #############################################################################################
# # Table JJ: all the insulation info
# #############################################################################################
# tableJJ.dat <- prep.dat7
# 
# tableJJ.2by6.dat <- item10.merge[which(item10.merge$Wall.Type == "Framed 2x6"),]
# 
# ############################
# # Apply weights
# ############################
# tableJJ.data <- weightedData(unique(tableJJ.dat[-which(colnames(tableJJ.dat) %in% c("aveUval"
#                                                                                     ,"aveRval"))]))
# tableJJ.data <- left_join(tableJJ.data, tableJJ.dat[which(colnames(tableJJ.dat) %in% c("CK_Cadmus_ID"
#                                                                                        ,"aveUval"
#                                                                                        ,"aveRval"))])
# 
# tableJJ.data$count <- 1
# 
# 
# tableJJ.2x6.data <- weightedData(unique(tableJJ.2by6.dat[-which(colnames(tableJJ.2by6.dat) %in% c("aveUval"
#                                                                                                   ,"aveRval"
#                                                                                                   ,"Wall.Type"
#                                                                                                   ,"rvalue.bins"
#                                                                                                   ,"count"))]))
# tableJJ.2x6.data <- left_join(tableJJ.2x6.data, tableJJ.2by6.dat[which(colnames(tableJJ.2by6.dat) %in% c("CK_Cadmus_ID"
#                                                                                                          ,"aveUval"
#                                                                                                          ,"aveRval"
#                                                                                                          ,"Wall.Type"
#                                                                                                          ,"rvalue.bins"
#                                                                                                          ,"count"))])
# 
# tableJJ.2x6.data$count <- 1
# 
# ##############################
# # Weighted Analysis
# ##############################
# tableJJ.wall.R <- mean_one_group(CustomerLevelData = tableJJ.data
#                                 ,valueVariable = 'aveRval'
#                                 ,byVariable = 'State'
#                                 ,aggregateRow = "Region")
# tableJJ.wall.R$Category <- "Wall R Value"
# 
# tableJJ.2by6.R <- mean_one_group(CustomerLevelData = tableJJ.2x6.data
#                                  ,valueVariable = "aveRval"
#                                  ,byVariable = "State"
#                                  ,aggregateRow = "Region")
# tableJJ.2by6.R$Category <- "2x6 Framed Wall R Value"
# 
# wall.table.JJ <- rbind.data.frame(tableJJ.wall.R, tableJJ.2by6.R, stringsAsFactors = F)
# 
# wall.cast <- dcast(setDT(wall.table.JJ)
#                    ,formula = BuildingType + State ~ Category
#                    ,value.var = c("Mean", "SE","n", "n_h", "N_h","EB"))
# wall.cast <- data.frame(wall.cast, stringsAsFactors = F)
# 
# wall.table <- data.frame("BuildingType"                = wall.cast$BuildingType
#                          ,"State"                      = wall.cast$State
#                          ,"Wall.Insulation"            = wall.cast$Mean_Wall.R.Value
#                          ,"Wall.Insulation.SE"         = wall.cast$SE_Wall.R.Value
#                          ,"Wall.Insulation.n"          = wall.cast$n_Wall.R.Value
#                          ,"R.Value.Framed.2x6.Wall"    = wall.cast$Mean_2x6.Framed.Wall.R.Value
#                          ,"R.Value.Framed.2x6.Wall.SE" = wall.cast$SE_2x6.Framed.Wall.R.Value
#                          ,"R.Value.Framed.2x6.Wall.n"  = wall.cast$n_2x6.Framed.Wall.R.Value
#                          ,"Wall.Insulation.EB"         = wall.cast$EB_Wall.R.Value
#                          ,"R.Value.Framed.2x6.Wall.EB" = wall.cast$EB_2x6.Framed.Wall.R.Value
#                          )
# levels(wall.table$State)
# rowOrder <- c("ID"
#               ,"MT"
#               ,"OR"
#               ,"WA"
#               ,"Region")
# wall.table <- wall.table %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
# wall.table <- data.frame(wall.table)
# 
# wall.table.JJ.SF <- wall.table[which(wall.table$BuildingType == "Single Family")
#                                   ,which(colnames(wall.table) %notin% c("BuildingType"))]
# wall.table.JJ.MH <- wall.table[which(wall.table$BuildingType == "Manufactured")
#                                   ,which(colnames(wall.table) %notin% c("BuildingType"))]
# 
# # exportTable(wall.table.JJ.SF, "SF","Table JJ",weighted = TRUE)
# exportTable(wall.table.JJ.MH, "MH","Table JJ",weighted = TRUE)
# 
# ##############################
# # Unweighted Analysis
# ##############################
# tableJJ.wall.R <- mean_one_group_unweighted(CustomerLevelData = tableJJ.data
#                                  ,valueVariable = 'aveRval'
#                                  ,byVariable = 'State'
#                                  ,aggregateRow = "Region")
# tableJJ.wall.R$Category <- "Wall R Value"
# 
# tableJJ.2by6.R <- mean_one_group_unweighted(CustomerLevelData = tableJJ.2x6.data
#                                  ,valueVariable = "aveRval"
#                                  ,byVariable = "State"
#                                  ,aggregateRow = "Region")
# tableJJ.2by6.R$Category <- "2x6 Framed Wall R Value"
# 
# wall.table.JJ <- rbind.data.frame(tableJJ.wall.R, tableJJ.2by6.R, stringsAsFactors = F)
# 
# wall.cast <- dcast(setDT(wall.table.JJ)
#                    ,formula = BuildingType + State ~ Category
#                    ,value.var = c("Mean", "SE","n"))
# wall.cast <- data.frame(wall.cast, stringsAsFactors = F)
# 
# wall.table <- data.frame("BuildingType" = wall.cast$BuildingType
#                          ,"State" = wall.cast$State
#                          ,"Wall.Insulation" = wall.cast$Mean_Wall.R.Value
#                          ,"Wall.Insulation.SE" = wall.cast$SE_Wall.R.Value
#                          ,"Wall.Insulation.n" = wall.cast$n_2x6.Framed.Wall.R.Value
#                          ,"R.Value.Framed.2x6.Wall" = wall.cast$Mean_2x6.Framed.Wall.R.Value
#                          ,"R.Value.Framed.2x6.Wall.SE" = wall.cast$SE_2x6.Framed.Wall.R.Value
#                          ,"R.Value.Framed.2x6.Wall.n" = wall.cast$n_2x6.Framed.Wall.R.Value)
# levels(wall.table$State)
# rowOrder <- c("ID"
#               ,"MT"
#               ,"OR"
#               ,"WA"
#               ,"Region")
# wall.table <- wall.table %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
# wall.table <- data.frame(wall.table)
# 
# wall.table.JJ.SF <- wall.table[which(wall.table$BuildingType == "Single Family")
#                                ,which(colnames(wall.table) %notin% c("BuildingType"))]
# wall.table.JJ.MH <- wall.table[which(wall.table$BuildingType == "Manufactured")
#                                ,which(colnames(wall.table) %notin% c("BuildingType"))]
# 
# # exportTable(wall.table.JJ.SF, "SF","Table JJ",weighted = FALSE)
# exportTable(wall.table.JJ.MH, "MH","Table JJ",weighted = FALSE)

































#################################################################################################################################
#
#
# OVERSAMPLE TABLES
#
#
#################################################################################################################################
# Read in clean os data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#############################################################################################
# Item 10: DISTRIBUTION OF FRAME WALL INSULATION LEVELS BY FRAMING TYPE (SF table 17)
#############################################################################################
item10.os.dat <- prep.dat5[grep("framed|alternative",prep.dat5$Wall.Type, ignore.case = T),]

#weight the u factor per home -- where weights are the wall area within home
item10.os.weightedU <- summarise(group_by(item10.os.dat, CK_Cadmus_ID, Wall.Type)
                              ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
item10.os.weightedU$aveRval <- (1 / as.numeric(as.character(item10.os.weightedU$aveUval)))
item10.os.weightedU$aveRval[which(item10.os.weightedU$aveRval %in% c("NaN",1))] <- 0
item10.os.weightedU$aveUval[which(item10.os.weightedU$aveUval == "NaN")] <- 1
unique(item10.os.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
item10.os.wall.unique <- unique(item10.os.dat[which(colnames(item10.os.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
item10.os.dat1 <- left_join(item10.os.weightedU, item10.os.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned scl data
item10.os.dat2 <- left_join(item10.os.dat1, os.dat)
item10.os.dat2$aveUval[which(is.na(item10.os.dat2$aveUval))] <- 0
item10.os.dat2$aveRval[which(is.na(item10.os.dat2$aveRval))] <- 0
item10.os.dat2 <- item10.os.dat2[which(item10.os.dat2$CK_Building_ID == subset.ind),]

#Bin R values -- SF only
item10.os.dat2$rvalue.bins <- "Unknown"
item10.os.dat2$rvalue.bins[which(item10.os.dat2$aveRval ==  0)] <- "R0"
item10.os.dat2$rvalue.bins[which(item10.os.dat2$aveRval >   0  & item10.os.dat2$aveRval < 11)]  <- "R1.R10"
item10.os.dat2$rvalue.bins[which(item10.os.dat2$aveRval >= 11  & item10.os.dat2$aveRval < 17)]  <- "R11.R16"
item10.os.dat2$rvalue.bins[which(item10.os.dat2$aveRval >= 17  & item10.os.dat2$aveRval < 23)]  <- "R17.R22"
item10.os.dat2$rvalue.bins[which(item10.os.dat2$aveRval >= 22)] <- "RGT22"
unique(item10.os.dat2$rvalue.bins)

item10.os.dat2$count <- 1

item10.os.dat3 <- item10.os.dat2[which(item10.os.dat2$rvalue.bins != "Unknown"),]
colnames(item10.os.dat3)

item10.os.merge <- left_join(os.dat, item10.os.dat3)
item10.os.merge <- item10.os.merge[which(!is.na(item10.os.merge$count)),]

item10.os.data <- weightedData(unique(item10.os.merge[-which(colnames(item10.os.merge) %in% c("Wall.Type"
                                                                                     ,"aveUval"
                                                                                     ,"aveRval"
                                                                                     ,"rvalue.bins"
                                                                                     ,"count"))]))
item10.os.data <- left_join(item10.os.data, item10.os.merge[which(colnames(item10.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins"
                                                                                       ,"count"))])



################################
# Weighted Analysis
################################
item10.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item10.os.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'Wall.Type'
                                            , rowVariable         = 'rvalue.bins'
                                            , aggregateColumnName = "All Frame Types"
)
item10.os.summary <- item10.os.summary[which(item10.os.summary$Wall.Type != "All Frame Types"),]
item10.os.summary <- item10.os.summary[which(item10.os.summary$rvalue.bins != "Total"),]

## Summary only for "All Frame Types"
item10.os.all.frame.types <- proportions_one_group(CustomerLevelData = item10.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Frame Types"
                                                ,columnName       = "Wall.Type"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item10.os.data$Wall.Category <- item10.os.data$Wall.Type
item10.os.all.insul.levels <-  proportions_one_group(CustomerLevelData = item10.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "Wall.Category"
                                                  ,total.name       = "All Insulation Levels"
                                                  ,columnName       = "rvalue.bins"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)
names(item10.os.all.insul.levels)[which(names(item10.os.all.insul.levels) == "Wall.Category")] <- "Wall.Type"

#merge together!
item10.os.final <- rbind.data.frame(item10.os.summary
                                 , item10.os.all.frame.types
                                 , item10.os.all.insul.levels
                                 , stringsAsFactors = F)
item10.os.final$Wall.Type[which(item10.os.final$Wall.Type == "Total")] <- "All Frame Types"


##cast data
item10.os.cast <- dcast(setDT(item10.os.final),
                     formula   = BuildingType + Wall.Type ~ rvalue.bins,
                     value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
names(item10.os.cast)
#join all insulation levels onto rvalue summary
item10.os.table <- data.frame("BuildingType"                   = item10.os.cast$BuildingType
                              ,"Wall.Type"                     = item10.os.cast$Wall.Type
                              ,"Percent.R0"                    = item10.os.cast$w.percent_R0
                              ,"SE.R0"                         = item10.os.cast$w.SE_R0
                              ,"Percent.R1.R10"                = item10.os.cast$w.percent_R1.R10
                              ,"SE.R1.R10"                     = item10.os.cast$w.SE_R1.R10
                              ,"Percent.R11.R16"               = item10.os.cast$w.percent_R11.R16
                              ,"SE.R11.R16"                    = item10.os.cast$w.SE_R11.R16
                              ,"Percent.R17.R22"               = item10.os.cast$w.percent_R17.R22
                              ,"SE.R17.R22"                    = item10.os.cast$w.SE_R17.R22
                              ,"Percent.RGT22"                 = item10.os.cast$w.percent_RGT22
                              ,"SE.RGT22"                      = item10.os.cast$w.SE_RGT22
                              ,"Percent_All Insulation Levels" = item10.os.cast$`w.percent_All Insulation Levels`
                              ,"SE.All Insulation Levels"      = item10.os.cast$`w.SE_All Insulation Levels`
                              ,"n"                             = item10.os.cast$`n_All Insulation Levels`
                              ,"EB.R0"                         = item10.os.cast$EB_R0
                              ,"EB.R1.R10"                     = item10.os.cast$EB_R1.R10
                              ,"EB.R11.R16"                    = item10.os.cast$EB_R11.R16
                              ,"EB.R17.R22"                    = item10.os.cast$EB_R17.R22
                              ,"EB.RGT22"                      = item10.os.cast$EB_RGT22
                              ,"EB.All Insulation Levels"      = item10.os.cast$`EB_All Insulation Levels`
)

# row ordering example code
unique(item10.os.table$Wall.Type)
rowOrder <- c("Framed 2x4"
              ,"Framed 2x6"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"All Frame Types")
item10.os.table <- item10.os.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item10.os.table <- data.frame(item10.os.table)

item10.os.table.SF <- item10.os.table[which(item10.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item10.os.table.SF, "SF", "Table 17", weighted = TRUE, osIndicator = export.ind, OS = T)



################################
# Unweighted Analysis
################################
item10.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = item10.os.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'Wall.Type'
                                                    , rowVariable         = 'rvalue.bins'
                                                    , aggregateColumnName = "All Frame Types"
)
item10.os.summary <- item10.os.summary[which(item10.os.summary$Wall.Type != "All Frame Types"),]

## Summary only for "All Frame Types"
item10.os.all.frame.types <- proportions_one_group(CustomerLevelData =  item10.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Frame Types"
                                                ,columnName       = "Wall.Type"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item10.os.data$Wall.Category <- item10.os.data$Wall.Type
item10.os.all.insul.levels <-  proportions_one_group(CustomerLevelData = item10.os.data
                                                     ,valueVariable    = "count"
                                                     ,groupingVariable = "Wall.Category"
                                                     ,total.name       = "All Insulation Levels"
                                                     ,columnName       = "rvalue.bins"
                                                     ,weighted = FALSE
                                                     ,two.prop.total = TRUE
)
names(item10.os.all.insul.levels)[which(names(item10.os.all.insul.levels) == "Wall.Category")] <- "Wall.Type"

#merge together!
item10.os.final <- rbind.data.frame(item10.os.summary
                                 , item10.os.all.frame.types
                                 , item10.os.all.insul.levels
                                 , stringsAsFactors = F)
item10.os.final <- item10.os.final[which(item10.os.final$rvalue.bins != "Total"),]
item10.os.final$Wall.Type[which(item10.os.final$Wall.Type == "Total")] <- "All Frame Types"


##cast data
item10.os.cast <- dcast(setDT(item10.os.final),
                     formula   = BuildingType + Wall.Type ~ rvalue.bins,
                     value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item10.os.table <- data.frame("BuildingType"                   = item10.os.cast$BuildingType
                              ,"Wall.Type"                     = item10.os.cast$Wall.Type
                              ,"Percent.R0"                    = item10.os.cast$Percent_R0
                              ,"SE.R0"                         = item10.os.cast$SE_R0
                              ,"Percent.R1.R10"                = item10.os.cast$Percent_R1.R10
                              ,"SE.R1.R10"                     = item10.os.cast$SE_R1.R10
                              ,"Percent.R11.R16"               = item10.os.cast$Percent_R11.R16
                              ,"SE.R11.R16"                    = item10.os.cast$SE_R11.R16
                              ,"Percent.R17.R22"               = item10.os.cast$Percent_R17.R22
                              ,"SE.R17.R22"                    = item10.os.cast$SE_R17.R22
                              ,"Percent.RGT22"                 = item10.os.cast$Percent_RGT22
                              ,"SE.RGT22"                      = item10.os.cast$SE_RGT22
                              ,"Percent_All Insulation Levels" = item10.os.cast$`Percent_All Insulation Levels`
                              ,"SE.All Insulation Levels"      = item10.os.cast$`SE_All Insulation Levels`
                              ,"n"                             = item10.os.cast$`n_All Insulation Levels`
)

# row ordering example code
unique(item10.os.table$Wall.Type)
rowOrder <- c("Framed 2x4"
              ,"Framed 2x6"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"All Frame Types")
item10.os.table <- item10.os.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item10.os.table <- data.frame(item10.os.table)

item10.os.table.SF <- item10.os.table[which(item10.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item10.os.table.SF, "SF", "Table 17", weighted = FALSE, osIndicator = export.ind, OS = T)






#############################################################################################
# Item 11: DISTRIBUTION OF WALL FRAMING TYPES BY VINTAGE (SF table 18)
#############################################################################################
## Note: For this table, you must run up to item10.os.dat3 for the cleaned data
item11.os.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                , "Category"
                                                                , "Wall.Type"))]

item11.os.dat2 <- left_join(item11.os.dat1, os.dat, by = c("CK_Cadmus_ID"))
item11.os.dat2 <- item11.os.dat2[which(item11.os.dat2$CK_Building_ID == subset.ind),]
#remove unneccesary wall types
item11.os.dat3 <- item11.os.dat2[grep("framed|alternative",item11.os.dat2$Wall.Type, ignore.case = T),]

#create "Alternative" category
item11.os.dat3$Wall.Type[grep("alternative",item11.os.dat3$Wall.Type, ignore.case = T)] <- "Alternative"
length(unique(item11.os.dat3$CK_Cadmus_ID))
unique(item11.os.dat3$Wall.Type)


#cast out by wall frame types
item11.os.dat3$count <- 1
item11.os.customer <- summarise(group_by(item11.os.dat3, CK_Cadmus_ID, CK_Building_ID, HomeYearBuilt_bins3, Wall.Type)
                             ,count = sum(unique(count)))

item11.os.merge <- left_join(os.dat, item11.os.customer)
item11.os.merge <- item11.os.merge[which(!is.na(item11.os.merge$HomeYearBuilt)),]
item11.os.merge <- item11.os.merge[which(!is.na(item11.os.merge$Wall.Type)),]

item11.os.data <- weightedData(unique(item11.os.merge[-which(colnames(item11.os.merge) %in% c("Category"
                                                                                     ,"Wall.Type"
                                                                                     ,"count"))]))
item11.os.data <- left_join(item11.os.data, item11.os.merge[which(colnames(item11.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Category"
                                                                                       ,"Wall.Type"
                                                                                       ,"count"))])

item11.os.data$Ind <- 1

#############################
# Weighted Analysis
#############################
item11.os.by.vinage <- proportionRowsAndColumns1(CustomerLevelData = item11.os.data
                                              , valueVariable       = 'Ind'
                                              , columnVariable      = 'HomeYearBuilt_bins3'
                                              , rowVariable         = 'Wall.Type'
                                              , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item11.os.across.vintages <- proportions_one_group(CustomerLevelData = item11.os.data
                                                , valueVariable    = 'Ind'
                                                , groupingVariable = 'Wall.Type'
                                                , total.name       = 'All Housing Vintages'
                                                , columnName       = 'HomeYearBuilt_bins3'
                                                , weighted = TRUE
                                                ,two.prop.total = TRUE
)


# row bind summaries
item11.os.final <- rbind.data.frame(item11.os.by.vinage, item11.os.across.vintages, stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item11.os.final <- item11.os.final[which(item11.os.final$HomeYearBuilt_bins3 != "Remove"),]



item11.os.cast <- dcast(setDT(item11.os.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ Wall.Type, sum,
                     value.var = c("w.percent", "w.SE", "count","n","N","EB"))
names(item11.os.cast)
item11.os.table <- data.frame("BuildingType"  = item11.os.cast$BuildingType
                           ,"Housing.Vintage" = item11.os.cast$HomeYearBuilt_bins3
                           ,"Percent_2x4"     = item11.os.cast$`w.percent_Framed 2x4`
                           ,"SE_2x4"          = item11.os.cast$`w.SE_Framed 2x4`
                           ,"Percent_2x6"     = item11.os.cast$`w.percent_Framed 2x6`
                           ,"SE_2x6"          = item11.os.cast$`w.SE_Framed 2x6`
                           # ,"Percent_2x8"     = item11.os.cast$`w.percent_Framed 2x8`
                           # ,"SE_2x8"          = item11.os.cast$`w.SE_Framed 2x8`
                           ,"Percent_ALT"     = item11.os.cast$w.percent_Alternative
                           ,"SE_ALT"          = item11.os.cast$w.SE_Alternative
                           ,"Percent_Unknown" = item11.os.cast$`w.percent_Framed (Unknown)`
                           ,"SE_Unknown"      = item11.os.cast$`w.SE_Framed (Unknown)`
                           ,"n"               = item11.os.cast$n_Total
                           ,"EB_2x4"          = item11.os.cast$`EB_Framed 2x4`
                           ,"EB_2x6"          = item11.os.cast$`EB_Framed 2x6`
                           # ,"EB_2x6"          = item11.os.cast$`EB_Framed 2x8`
                           ,"EB_ALT"          = item11.os.cast$EB_Alternative
                           ,"EB_Unknown"      = item11.os.cast$`EB_Framed (Unknown)`
)

# row ordering example code
unique(item11.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item11.os.table <- item11.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item11.os.table <- data.frame(item11.os.table)

item11.os.table.SF <- item11.os.table[which(item11.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item11.os.table.SF, "SF", "Table 18", weighted = TRUE, osIndicator = export.ind, OS = T)




#############################
# Unweighted Analysis
#############################
item11.os.by.vinage <- proportions_two_groups_unweighted(item11.os.data
                                                      , valueVariable       = 'count'
                                                      , columnVariable      = 'HomeYearBuilt_bins3'
                                                      , rowVariable         = 'Wall.Type'
                                                      , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item11.os.across.vintages <- proportions_one_group(item11.os.data
                                                , valueVariable    = 'count'
                                                , groupingVariable = 'Wall.Type'
                                                , total.name       = 'All Housing Vintages'
                                                , columnName       = 'HomeYearBuilt_bins3'
                                                , weighted = FALSE
                                                , two.prop.total = TRUE
)
# row bind summaries
item11.os.final <- rbind.data.frame(item11.os.by.vinage, item11.os.across.vintages, stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item11.os.final <- item11.os.final[which(item11.os.final$HomeYearBuilt_bins3 != "Remove"),]



item11.os.cast <- dcast(setDT(item11.os.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ Wall.Type, sum,
                     value.var = c("Percent", "SE", "Count","n"))



item11.os.table <- data.frame("BuildingType"  = item11.os.cast$BuildingType
                           ,"Housing.Vintage" = item11.os.cast$HomeYearBuilt_bins3
                           ,"Percent_2x4"     = item11.os.cast$`Percent_Framed 2x4`
                           ,"SE_2x4"          = item11.os.cast$`SE_Framed 2x4`
                           ,"Percent_2x6"     = item11.os.cast$`Percent_Framed 2x6`
                           ,"SE_2x6"          = item11.os.cast$`SE_Framed 2x6`
                           # ,"Percent_2x8"     = item11.os.cast$`Percent_Framed 2x8`
                           # ,"SE_2x8"          = item11.os.cast$`SE_Framed 2x8`
                           ,"Percent_ALT"     = item11.os.cast$Percent_Alternative
                           ,"SE_ALT"          = item11.os.cast$SE_Alternative
                           ,"Percent_Unknown" = item11.os.cast$`Percent_Framed (Unknown)`
                           ,"SE_Unknown"      = item11.os.cast$`SE_Framed (Unknown)`
                           ,"n"               = item11.os.cast$n_Total
)

# row ordering example code
unique(item11.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item11.os.table <- item11.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item11.os.table <- data.frame(item11.os.table)

item11.os.table.SF <- item11.os.table[which(item11.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item11.os.table.SF, "SF", "Table 18", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
# Item 11A: DISTRIBUTION OF WALL FRAMING TYPES BY SAMPLE (SF table 18A)
#############################################################################################
## Note: For this table, you must run up to item10.os.dat3 for the cleaned data
item11A.os.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                   , "Category"
                                                                   , "Wall.Type"))]

item11A.os.dat2 <- left_join(item11A.os.dat1, os.dat, by = c("CK_Cadmus_ID"))

#remove unneccesary wall types
item11A.os.dat3 <- item11A.os.dat2[grep("framed|alternative",item11A.os.dat2$Wall.Type, ignore.case = T),]

#create "Alternative" category
item11A.os.dat3$Wall.Type[grep("alternative",item11A.os.dat3$Wall.Type, ignore.case = T)] <- "Alternative"
length(unique(item11A.os.dat3$CK_Cadmus_ID))
unique(item11A.os.dat3$Wall.Type)


#cast out by wall frame types
item11A.os.dat3$count <- 1
item11A.os.customer <- summarise(group_by(item11A.os.dat3, CK_Cadmus_ID, CK_Building_ID, HomeYearBuilt_bins3, Wall.Type)
                                ,count = sum(count))

item11A.os.merge <- left_join(os.dat, item11A.os.customer)
item11A.os.merge <- item11A.os.merge[which(!is.na(item11A.os.merge$Wall.Type)),]

item11A.os.data <- weightedData(unique(item11A.os.merge[-which(colnames(item11A.os.merge) %in% c("Category"
                                                                                              ,"Wall.Type"
                                                                                              ,"count"))]))
item11A.os.data <- left_join(item11A.os.data, item11A.os.merge[which(colnames(item11A.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"Category"
                                                                                                   ,"Wall.Type"
                                                                                                   ,"count"))])

item11A.os.data$Ind <- 1
item11A.os.data$Count <- 1
#############################
# Weighted Analysis
#############################
item11A.os.final <- proportionRowsAndColumns1(CustomerLevelData = item11A.os.data
                                                 , valueVariable       = 'Count'
                                                 , columnVariable      = 'CK_Building_ID'
                                                 , rowVariable         = 'Wall.Type'
                                                 , aggregateColumnName = "Remove"
)
item11A.os.final <- item11A.os.final[which(item11A.os.final$CK_Building_ID != "Remove"),]

item11A.os.cast <- dcast(setDT(item11A.os.final),
                        formula   = BuildingType + Wall.Type ~ CK_Building_ID, sum,
                        value.var = c("w.percent", "w.SE", "count","n","N","EB"))


if(os.ind == "scl"){
  item11A.os.table <- data.frame("BuildingType"    = item11A.os.cast$BuildingType
                                 ,"Wall.Type"          = item11A.os.cast$Wall.Type
                                 ,"Percent_SCL.GenPop"   = item11A.os.cast$`w.percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item11A.os.cast$`w.SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item11A.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item11A.os.cast$`w.percent_SCL LI`
                                 ,"SE_SCL.LI"            = item11A.os.cast$`w.SE_SCL LI`
                                 ,"n_SCL.LI"             = item11A.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item11A.os.cast$`w.percent_SCL EH`
                                 ,"SE_SCL.EH"            = item11A.os.cast$`w.SE_SCL EH`
                                 ,"n_SCL.EH"             = item11A.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item11A.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item11A.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item11A.os.cast$`n_2017 RBSA PS`
                                 ,"EB_SCL.GenPop"        = item11A.os.cast$`EB_SCL GenPop`
                                 ,"EB_SCL.LI"            = item11A.os.cast$`EB_SCL LI`
                                 ,"EB_SCL.EH"            = item11A.os.cast$`EB_SCL EH`
                                 ,"EB_2017.RBSA.PS"      = item11A.os.cast$`EB_2017 RBSA PS`
  )
  
}else if(os.ind == "snopud"){
  item11A.os.table <- data.frame("BuildingType"    = item11A.os.cast$BuildingType
                                 ,"Wall.Type"          = item11A.os.cast$Wall.Type
                                 ,"Percent_SnoPUD"          = item11A.os.cast$`w.percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item11A.os.cast$`w.SE_SnoPUD`
                                 ,"n_SnoPUD"                = item11A.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item11A.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item11A.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item11A.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item11A.os.cast$`w.percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item11A.os.cast$`w.SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item11A.os.cast$`n_2017 RBSA NW`
                                 ,"EB_SnoPUD"               = item11A.os.cast$`EB_SnoPUD`
                                 ,"EB_2017.RBSA.PS"         = item11A.os.cast$`EB_2017 RBSA PS`
                                 ,"EB_RBSA.NW"              = item11A.os.cast$`EB_2017 RBSA NW`
  )
  
}

# row ordering example code
unique(item11A.os.table$Wall.Type)
rowOrder <- c("Framed 2x4"
              ,"Framed 2x6"
              ,"Framed 2x8"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"Total")
item11A.os.table <- item11A.os.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item11A.os.table <- data.frame(item11A.os.table)


item11A.os.table.SF <- item11A.os.table[which(item11A.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item11A.os.table.SF, "SF", "Table 18A", weighted = TRUE, osIndicator = export.ind, OS = T)




#############################
# Unweighted Analysis
#############################
item11A.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item11A.os.data
                                              , valueVariable       = 'Count'
                                              , columnVariable      = 'CK_Building_ID'
                                              , rowVariable         = 'Wall.Type'
                                              , aggregateColumnName = "Remove"
)
item11A.os.final <- item11A.os.final[which(item11A.os.final$CK_Building_ID != "Remove"),]

item11A.os.cast <- dcast(setDT(item11A.os.final),
                         formula   = BuildingType + Wall.Type ~ CK_Building_ID, sum,
                         value.var = c("Percent", "SE","n"))


if(os.ind == "scl"){
  item11A.os.table <- data.frame("BuildingType"    = item11A.os.cast$BuildingType
                                 ,"Wall.Type"          = item11A.os.cast$Wall.Type
                                 ,"Percent_SCL.GenPop"   = item11A.os.cast$`Percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item11A.os.cast$`SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item11A.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item11A.os.cast$`Percent_SCL LI`
                                 ,"SE_SCL.LI"            = item11A.os.cast$`SE_SCL LI`
                                 ,"n_SCL.LI"             = item11A.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item11A.os.cast$`Percent_SCL EH`
                                 ,"SE_SCL.EH"            = item11A.os.cast$`SE_SCL EH`
                                 ,"n_SCL.EH"             = item11A.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item11A.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item11A.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item11A.os.cast$`n_2017 RBSA PS`
  )
  
}else if(os.ind == "snopud"){
  item11A.os.table <- data.frame("BuildingType"    = item11A.os.cast$BuildingType
                                 ,"Wall.Type"          = item11A.os.cast$Wall.Type
                                 ,"Percent_SnoPUD"          = item11A.os.cast$`Percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item11A.os.cast$`SE_SnoPUD`
                                 ,"n_SnoPUD"                = item11A.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item11A.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item11A.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item11A.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item11A.os.cast$`Percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item11A.os.cast$`SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item11A.os.cast$`n_2017 RBSA NW`
  )
  
}


# row ordering example code
unique(item11A.os.table$Wall.Type)
rowOrder <- c("Framed 2x4"
              ,"Framed 2x6"
              ,"Framed 2x8"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"Total")
item11A.os.table <- item11A.os.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item11A.os.table <- data.frame(item11A.os.table)

item11A.os.table.SF <- item11A.os.table[which(item11A.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item11A.os.table.SF, "SF", "Table 18A", weighted = FALSE, osIndicator = export.ind, OS = T)






#############################################################################################
# Table AP: DISTRIBUTION OF WALL INSULATION LEVELS BY CK_Building_ID
#############################################################################################
prep.tableAP.dat <- prep.dat5[-grep("basement",prep.dat5$Wall.Type, ignore.case = T),]
which(duplicated(prep.tableAP.dat$CK_Cadmus_ID))
#weight the u factor per home -- where weights are the wall area within home
prep.tableAP.weightedU <- summarise(group_by(prep.tableAP.dat, CK_Cadmus_ID)
                                    ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
prep.tableAP.weightedU$aveRval <- (1 / as.numeric(as.character(prep.tableAP.weightedU$aveUval)))
prep.tableAP.weightedU$aveRval[which(prep.tableAP.weightedU$aveRval %in% c("NaN",1))] <- 0
prep.tableAP.weightedU$aveUval[which(prep.tableAP.weightedU$aveUval == "NaN")] <- 1
unique(prep.tableAP.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
prep.tableAP.wall.unique <- unique(prep.tableAP.dat[which(colnames(prep.tableAP.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.tableAP.dat1 <- left_join(prep.tableAP.weightedU, prep.tableAP.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned scl data
prep.tableAP.dat2 <- left_join(prep.tableAP.dat1, os.dat)
prep.tableAP.dat2$aveUval[which(is.na(prep.tableAP.dat2$aveUval))] <- 0
prep.tableAP.dat2$aveRval[which(is.na(prep.tableAP.dat2$aveRval))] <- 0


tableAP.dat <- prep.tableAP.dat2

tableAP.dat1 <- tableAP.dat

#Bin R values -- SF only
tableAP.dat1$rvalue.bins.SF <- "Unknown"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval < 1)] <- "R0"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval >=  1  & tableAP.dat1$aveRval < 11)]  <- "R1.R10"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval >= 11 & tableAP.dat1$aveRval  < 17)]  <- "R11.R16"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval >= 17 & tableAP.dat1$aveRval  < 23)]  <- "R17.R22"
tableAP.dat1$rvalue.bins.SF[which(tableAP.dat1$aveRval >= 22)] <- "RGT22"
unique(tableAP.dat1$rvalue.bins.SF)

############################################################################################################
# Apply weights
############################################################################################################
tableAP.dat1$count <- 1
colnames(tableAP.dat1)

tableAP.merge <- left_join(os.dat, tableAP.dat1)
tableAP.merge <- tableAP.merge[which(!is.na(tableAP.merge$count)),]

tableAP.data <- weightedData(unique(tableAP.merge[-which(colnames(tableAP.merge) %in% c("Wall.Type"
                                                                                        ,"aveUval"
                                                                                        ,"aveRval"
                                                                                        ,"rvalue.bins.SF"
                                                                                        ,"count"))]))
tableAP.data <- left_join(tableAP.data, unique(tableAP.merge[which(colnames(tableAP.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Wall.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"rvalue.bins.SF"
                                                                                           ,"count"))]))

#############################################################################################
# Weighted Analysis - Single Family
#############################################################################################
tableAP.final <- proportionRowsAndColumns1(CustomerLevelData     = tableAP.data
                                           , valueVariable       = 'count'
                                           , columnVariable      = 'CK_Building_ID'
                                           , rowVariable         = 'rvalue.bins.SF'
                                           , aggregateColumnName = "Remove"
)
tableAP.final <- tableAP.final[which(tableAP.final$CK_Building_ID != "Remove"),]

tableAP.cast <- dcast(setDT(tableAP.final),
                      formula   = BuildingType + rvalue.bins.SF ~ CK_Building_ID,
                      value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

if(os.ind == "scl"){
  tableAP.table <- data.frame("BuildingType"        = tableAP.cast$BuildingType
                              ,"Insulation.Levels"    = tableAP.cast$rvalue.bins.SF
                              ,"Percent_SCL.GenPop"   = tableAP.cast$`w.percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = tableAP.cast$`w.SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = tableAP.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = tableAP.cast$`w.percent_SCL LI`
                              ,"SE_SCL.LI"            = tableAP.cast$`w.SE_SCL LI`
                              ,"n_SCL.LI"             = tableAP.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = tableAP.cast$`w.percent_SCL EH`
                              ,"SE_SCL.EH"            = tableAP.cast$`w.SE_SCL EH`
                              ,"n_SCL.EH"             = tableAP.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = tableAP.cast$`w.percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = tableAP.cast$`w.SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = tableAP.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = tableAP.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = tableAP.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = tableAP.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = tableAP.cast$`EB_2017 RBSA PS`)
  
}else if(os.ind == "snopud"){
  tableAP.table <- data.frame("BuildingType"        = tableAP.cast$BuildingType
                              ,"Insulation.Levels"    = tableAP.cast$rvalue.bins.SF
                              ,"Percent_SnoPUD"          = tableAP.cast$`w.percent_SnoPUD`
                              ,"SE_SnoPUD"               = tableAP.cast$`w.SE_SnoPUD`
                              ,"n_SnoPUD"                = tableAP.cast$`n_SnoPUD`
                              ,"Percent_2017.RBSA.PS"    = tableAP.cast$`w.percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"         = tableAP.cast$`w.SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"          = tableAP.cast$`n_2017 RBSA PS`
                              ,"Percent_RBSA.NW"         = tableAP.cast$`w.percent_2017 RBSA NW`
                              ,"SE_RBSA.NW"              = tableAP.cast$`w.SE_2017 RBSA NW`
                              ,"n_RBSA.NW"               = tableAP.cast$`n_2017 RBSA NW`
                              ,"EB_SnoPUD"               = tableAP.cast$`EB_SnoPUD`
                              ,"EB_2017.RBSA.PS"         = tableAP.cast$`EB_2017 RBSA PS`
                              ,"EB_RBSA.NW"              = tableAP.cast$`EB_2017 RBSA NW`)
  
}


tableAP.table.SF <- tableAP.table[which(tableAP.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(tableAP.table.SF, "SF", "Table 19", weighted = TRUE, osIndicator = export.ind, OS = T)



################################
# Unweighted Analysis
################################
tableAP.final <- proportions_two_groups_unweighted(CustomerLevelData     = tableAP.data
                                                   , valueVariable       = 'count'
                                                   , columnVariable      = 'CK_Building_ID'
                                                   , rowVariable         = 'rvalue.bins.SF'
                                                   , aggregateColumnName = "Remove"
)
tableAP.final <- tableAP.final[which(tableAP.final$CK_Building_ID != "Remove"),]

tableAP.cast <- dcast(setDT(tableAP.final),
                      formula   = BuildingType + rvalue.bins.SF ~ CK_Building_ID,
                      value.var = c("Percent", "SE", "Count", "n"))

if(os.ind == "scl"){
  tableAP.table <- data.frame("BuildingType"           = tableAP.cast$BuildingType
                              ,"Insulation.Levels"    = tableAP.cast$rvalue.bins.SF
                              ,"Percent_SCL.GenPop"   = tableAP.cast$`Percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = tableAP.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = tableAP.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = tableAP.cast$`Percent_SCL LI`
                              ,"SE_SCL.LI"            = tableAP.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = tableAP.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = tableAP.cast$`Percent_SCL EH`
                              ,"SE_SCL.EH"            = tableAP.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = tableAP.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = tableAP.cast$`Percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = tableAP.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = tableAP.cast$`n_2017 RBSA PS`)
  
}else if(os.ind == "snopud"){
  tableAP.table <- data.frame("BuildingType"           = tableAP.cast$BuildingType
                              ,"Insulation.Levels"       = tableAP.cast$rvalue.bins.SF
                              ,"Percent_SnoPUD"          = tableAP.cast$`Percent_SnoPUD`
                              ,"SE_SnoPUD"               = tableAP.cast$`SE_SnoPUD`
                              ,"n_SnoPUD"                = tableAP.cast$`n_SnoPUD`
                              ,"Percent_2017.RBSA.PS"    = tableAP.cast$`Percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"         = tableAP.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"          = tableAP.cast$`n_2017 RBSA PS`
                              ,"Percent_RBSA.NW"         = tableAP.cast$`Percent_2017 RBSA NW`
                              ,"SE_RBSA.NW"              = tableAP.cast$`SE_2017 RBSA NW`
                              ,"n_RBSA.NW"               = tableAP.cast$`n_2017 RBSA NW`)
  
}


tableAP.table.SF <- tableAP.table[which(tableAP.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(tableAP.table.SF, "SF", "Table 19", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
# Item 12: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE  (SF table 19, MH table 16)
#############################################################################################
prep.item12.os.dat <- prep.dat5[-grep("basement",prep.dat5$Wall.Type, ignore.case = T),]

#weight the u factor per home -- where weights are the wall area within home
prep.item12.os.weightedU <- summarise(group_by(prep.item12.os.dat, CK_Cadmus_ID, Wall.Type)
                                   ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
prep.item12.os.weightedU$aveRval <- (1 / as.numeric(as.character(prep.item12.os.weightedU$aveUval)))
prep.item12.os.weightedU$aveRval[which(prep.item12.os.weightedU$aveRval %in% c("NaN",1))] <- 0
prep.item12.os.weightedU$aveUval[which(prep.item12.os.weightedU$aveUval == "NaN")] <- 1
unique(prep.item12.os.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
prep.item12.os.wall.unique <- unique(prep.item12.os.dat[which(colnames(prep.item12.os.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.item12.os.dat1 <- left_join(prep.item12.os.weightedU, prep.item12.os.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned scl data
prep.item12.os.dat2 <- left_join(prep.item12.os.dat1, os.dat)
prep.item12.os.dat2$aveUval[which(is.na(prep.item12.os.dat2$aveUval))] <- 0
prep.item12.os.dat2$aveRval[which(is.na(prep.item12.os.dat2$aveRval))] <- 0
## Note: For this table, you must run up to prep.dat7 for the cleaned data
item12.os.dat <- prep.item12.os.dat2

item12.os.dat1 <- item12.os.dat[which(!(is.na(item12.os.dat$HomeYearBuilt_bins3))),]

#Bin R values -- SF only
item12.os.dat1$rvalue.bins.SF <- "Unknown"
item12.os.dat1$rvalue.bins.SF[which(item12.os.dat1$aveRval < 1)] <- "R0"
item12.os.dat1$rvalue.bins.SF[which(item12.os.dat1$aveRval >=  1  & item12.os.dat1$aveRval < 11)]  <- "R1.R10"
item12.os.dat1$rvalue.bins.SF[which(item12.os.dat1$aveRval >= 11 & item12.os.dat1$aveRval  < 17)]  <- "R11.R16"
item12.os.dat1$rvalue.bins.SF[which(item12.os.dat1$aveRval >= 17 & item12.os.dat1$aveRval  < 23)]  <- "R17.R22"
item12.os.dat1$rvalue.bins.SF[which(item12.os.dat1$aveRval >= 22)] <- "RGT22"
unique(item12.os.dat1$rvalue.bins.SF)

############################################################################################################
# Apply weights
############################################################################################################
item12.os.dat1$count <- 1
colnames(item12.os.dat1)

item12.os.merge <- left_join(os.dat, item12.os.dat1)
item12.os.merge <- item12.os.merge[which(!is.na(item12.os.merge$count)),]

item12.os.data <- weightedData(unique(item12.os.merge[-which(colnames(item12.os.merge) %in% c("Wall.Type"
                                                                                     ,"aveUval"
                                                                                     ,"aveRval"
                                                                                     ,"rvalue.bins.SF"
                                                                                     ,"count"))]))
item12.os.data <- left_join(item12.os.data, item12.os.merge[which(colnames(item12.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins.SF"
                                                                                       ,"count"))])

item12.os.data1 <- item12.os.data[which(item12.os.data$CK_Building_ID == subset.ind),]
################################################
# Weighted Analysis - Single Family
################################################
item12.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item12.os.data1
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item12.os.summary <- item12.os.summary[which(item12.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]
item12.os.summary <- item12.os.summary[which(item12.os.summary$rvalue.bins.SF != "Total"),]

## Summary only for "All Frame Types"
item12.os.all.frame.types <- proportions_one_group(CustomerLevelData = item12.os.data1
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)
item12.os.all.frame.types <- item12.os.all.frame.types[which(item12.os.all.frame.types$rvalue.bins.SF != "Total"),]

## Summary for only "All Insulation Levels"
item12.os.all.insul.levels <-  proportions_one_group(CustomerLevelData = item12.os.data1
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Walls"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item12.os.final <- rbind.data.frame(item12.os.summary
                                 , item12.os.all.frame.types
                                 , item12.os.all.insul.levels
                                 , stringsAsFactors = F)
item12.os.final$HomeYearBuilt_bins3[which(item12.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"
item12.os.final$rvalue.bins.SF[which(item12.os.final$rvalue.bins.SF == "Total")] <- "All Walls"

item12.os.cast <- dcast(setDT(item12.os.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item12.os.cast)
item12.os.table <- data.frame("BuildingType"     = item12.os.cast$BuildingType
                           ,"Housing.Vintage" = item12.os.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item12.os.cast$w.percent_R0
                           ,"SE.R0"           = item12.os.cast$w.SE_R0
                           ,"Percent.R1.R10"  = item12.os.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item12.os.cast$w.SE_R1.R10
                           ,"Percent.R11.R16" = item12.os.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item12.os.cast$w.SE_R11.R16
                           ,"Percent.R17.R22" = item12.os.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item12.os.cast$w.SE_R17.R22
                           ,"Percent.RGT22"   = item12.os.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item12.os.cast$w.SE_RGT22
                           ,"n"               = item12.os.cast$`n_All Walls`
                           ,"EB.R0"           = item12.os.cast$EB_R0
                           ,"EB.R1.R10"       = item12.os.cast$EB_R1.R10
                           ,"EB.R11.R16"      = item12.os.cast$EB_R11.R16
                           ,"EB.R17.R22"      = item12.os.cast$EB_R17.R22
                           ,"EB.RGT22"        = item12.os.cast$EB_RGT22
)

# row ordering example code
unique(item12.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item12.os.table <- item12.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item12.os.table <- data.frame(item12.os.table)

item12.os.table.SF <- item12.os.table[which(item12.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item12.os.table.SF, "SF", "Table 20", weighted = TRUE, osIndicator = export.ind, OS = T)



################################
# Unweighted Analysis
################################
item12.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = item12.os.data1
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item12.os.summary <- item12.os.summary[which(item12.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item12.os.all.frame.types <- proportions_one_group(item12.os.data1
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item12.os.all.insul.levels <-  proportions_one_group(item12.os.data1
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item12.os.final <- rbind.data.frame(item12.os.summary
                                 , item12.os.all.frame.types
                                 , item12.os.all.insul.levels
                                 , stringsAsFactors = F)
item12.os.final <- item12.os.final[which(item12.os.final$rvalue.bins.SF != "Total"),]
item12.os.final$HomeYearBuilt_bins3[which(item12.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


##cast data
item12.os.cast <- dcast(setDT(item12.os.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))


item12.os.table <- data.frame("BuildingType"     = item12.os.cast$BuildingType
                              ,"Housing.Vintage" = item12.os.cast$HomeYearBuilt_bins3
                              ,"Percent.R0"      = item12.os.cast$Percent_R0
                              ,"SE.R0"           = item12.os.cast$SE_R0
                              ,"Percent.R1.R10"  = item12.os.cast$Percent_R1.R10
                              ,"SE.R1.R10"       = item12.os.cast$SE_R1.R10
                              ,"Percent.R11.R16" = item12.os.cast$Percent_R11.R16
                              ,"SE.R11.R16"      = item12.os.cast$SE_R11.R16
                              ,"Percent.R17.R22" = item12.os.cast$Percent_R17.R22
                              ,"SE.R17.R22"      = item12.os.cast$SE_R17.R22
                              ,"Percent.RGT22"   = item12.os.cast$Percent_RGT22
                              ,"SE.RGT22"        = item12.os.cast$SE_RGT22
                              ,"n"               = item12.os.cast$`n_All Housing Vintages`
)

# row ordering example code
unique(item12.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item12.os.table <- item12.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item12.os.table <- data.frame(item12.os.table)

item12.os.table.SF <- item12.os.table[which(item12.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item12.os.table.SF, "SF", "Table 20", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
# Item 14: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, SCL LOW INCOME  (SF table 21)
#############################################################################################
## Note: For this table, you must run up to item12.os.dat1 for the cleaned data
if(os.ind == "scl"){
  item14.os.data <- item12.os.data[which(item12.os.data$CK_Building_ID == "SCL LI"),]
}else if(os.ind == "snopud"){
  item14.os.data <- item12.os.data[which(item12.os.data$CK_Building_ID == "2017 RBSA PS"),]
}


##############################################
# Weighted Analysis - Single Family
##############################################
item14.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item14.os.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item14.os.summary <- item14.os.summary[which(item14.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item14.os.all.frame.types <- proportions_one_group(item14.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item14.os.all.insul.levels <-  proportions_one_group(item14.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)
#merge together!
item14.os.final <- rbind.data.frame(item14.os.summary
                                 , item14.os.all.frame.types
                                 , item14.os.all.insul.levels
                                 , stringsAsFactors = F)
item14.os.final <- item14.os.final[which(item14.os.final$rvalue.bins.SF != "Total"),]
item14.os.final$HomeYearBuilt_bins3[which(item14.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

item14.os.cast <- dcast(setDT(item14.os.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item14.os.cast)
item14.os.table <- data.frame("BuildingType"     = item14.os.cast$BuildingType
                              ,"Housing.Vintage" = item14.os.cast$HomeYearBuilt_bins3
                              ,"Percent.R0"      = item14.os.cast$w.percent_R0
                              ,"SE.R0"           = item14.os.cast$w.SE_R0
                              ,"Percent.R1.R10"  = item14.os.cast$w.percent_R1.R10
                              ,"SE.R1.R10"       = item14.os.cast$w.SE_R1.R10
                              ,"Percent.R11.R16" = item14.os.cast$w.percent_R11.R16
                              ,"SE.R11.R16"      = item14.os.cast$w.SE_R11.R16
                              ,"Percent.R17.R22" = item14.os.cast$w.percent_R17.R22
                              ,"SE.R17.R22"      = item14.os.cast$w.SE_R17.R22
                              ,"Percent.RGT22"   = NA#item14.os.cast$w.percent_RGT22
                              ,"SE.RGT22"        = NA#item14.os.cast$w.SE_RGT22
                              ,"n"               = item14.os.cast$`n_All Housing Vintages`
                              ,"EB.R0"           = item14.os.cast$EB_R0
                              ,"EB.R1.R10"       = item14.os.cast$EB_R1.R10
                              ,"EB.R11.R16"      = item14.os.cast$EB_R11.R16
                              ,"EB.R17.R22"      = item14.os.cast$EB_R17.R22
                              ,"EB.RGT22"        = NA#item14.os.cast$EB_RGT22
)

# row ordering example code
unique(item14.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item14.os.table <- item14.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item14.os.table <- data.frame(item14.os.table)

item14.os.table.SF <- item14.os.table[which(item14.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item14.os.table.SF, "SF", "Table 21", weighted = TRUE, osIndicator = export.ind, OS = T)



################################
# Unweighted Analysis
################################
item14.os.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item14.os.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item14.os.summary <- item14.os.summary[which(item14.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item14.os.all.frame.types <- proportions_one_group(item14.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item14.os.all.insul.levels <-  proportions_one_group(item14.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item14.os.final <- rbind.data.frame(item14.os.summary
                                 , item14.os.all.frame.types
                                 , item14.os.all.insul.levels
                                 , stringsAsFactors = F)
item14.os.final <- item14.os.final[which(item14.os.final$rvalue.bins.SF != "Total"),]
item14.os.final$HomeYearBuilt_bins3[which(item14.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


##cast data
item14.os.cast <- dcast(setDT(item14.os.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))


item14.os.table <- data.frame("BuildingType"     = item14.os.cast$BuildingType
                              ,"Housing.Vintage" = item14.os.cast$HomeYearBuilt_bins3
                              ,"Percent.R0"      = item14.os.cast$Percent_R0
                              ,"SE.R0"           = item14.os.cast$SE_R0
                              ,"Percent.R1.R10"  = item14.os.cast$Percent_R1.R10
                              ,"SE.R1.R10"       = item14.os.cast$SE_R1.R10
                              ,"Percent.R11.R16" = item14.os.cast$Percent_R11.R16
                              ,"SE.R11.R16"      = item14.os.cast$SE_R11.R16
                              ,"Percent.R17.R22" = item14.os.cast$Percent_R17.R22
                              ,"SE.R17.R22"      = item14.os.cast$SE_R17.R22
                              ,"Percent.RGT22"   = NA#item14.os.cast$Percent_RGT22
                              ,"SE.RGT22"        = NA#item14.os.cast$SE_RGT22
                              ,"n"               = item14.os.cast$`n_All Housing Vintages`
)

# row ordering example code
unique(item14.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item14.os.table <- item14.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item14.os.table <- data.frame(item14.os.table)


item14.os.table.SF <- item14.os.table[which(item14.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item14.os.table.SF, "SF", "Table 21", weighted = FALSE, osIndicator = export.ind, OS = T)







#############################################################################################
# Item 15: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, SCL ELECTRICALLY HEATED HOMES  (SF table 22)
#############################################################################################
## Note: For this table, you must run up to item12.os.dat1 for the cleaned data
if(os.ind == "scl"){
  item15.os.data <- item12.os.data[which(item12.os.data$CK_Building_ID == "SCL EH"),]
}else if(os.ind == "snopud"){
  item15.os.data <- item12.os.data[which(item12.os.data$CK_Building_ID == "2017 RBSA NW"),]
}


############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item15.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item15.os.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item15.os.summary <- item15.os.summary[which(item15.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item15.os.all.frame.types <- proportions_one_group(item15.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item15.os.all.insul.levels <-  proportions_one_group(item15.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item15.os.final <- rbind.data.frame(item15.os.summary
                                 , item15.os.all.frame.types
                                 , item15.os.all.insul.levels
                                 , stringsAsFactors = F)
item15.os.final <- item15.os.final[which(item15.os.final$rvalue.bins.SF != "Total"),]
item15.os.final$HomeYearBuilt_bins3[which(item15.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

item15.os.cast <- dcast(setDT(item15.os.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item15.os.cast)
item15.os.table <- data.frame("BuildingType"     = item15.os.cast$BuildingType
                              ,"Housing.Vintage" = item15.os.cast$HomeYearBuilt_bins3
                              ,"Percent.R0"      = item15.os.cast$w.percent_R0
                              ,"SE.R0"           = item15.os.cast$w.SE_R0
                              ,"Percent.R1.R10"  = item15.os.cast$w.percent_R1.R10
                              ,"SE.R1.R10"       = item15.os.cast$w.SE_R1.R10
                              ,"Percent.R11.R16" = item15.os.cast$w.percent_R11.R16
                              ,"SE.R11.R16"      = item15.os.cast$w.SE_R11.R16
                              ,"Percent.R17.R22" = item15.os.cast$w.percent_R17.R22
                              ,"SE.R17.R22"      = item15.os.cast$w.SE_R17.R22
                              ,"Percent.RGT22"   = NA#item15.os.cast$w.percent_RGT22
                              ,"SE.RGT22"        = NA#item15.os.cast$w.SE_RGT22
                              ,"n"               = item15.os.cast$`n_All Housing Vintages`
                              ,"EB.R0"           = item15.os.cast$EB_R0
                              ,"EB.R1.R10"       = item15.os.cast$EB_R1.R10
                              ,"EB.R11.R16"      = item15.os.cast$EB_R11.R16
                              ,"EB.R17.R22"      = item15.os.cast$EB_R17.R22
                              ,"EB.RGT22"        = NA#item15.os.cast$EB_RGT22
)

# row ordering example code
unique(item15.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item15.os.table <- item15.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item15.os.table <- data.frame(item15.os.table)

item15.os.table.SF <- item15.os.table[which(item15.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item15.os.table.SF, "SF", "Table 22", weighted = TRUE, osIndicator = export.ind, OS = T)



################################
# Unweighted Analysis
################################
item15.os.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item15.os.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item15.os.summary <- item15.os.summary[which(item15.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item15.os.all.frame.types <- proportions_one_group(item15.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item15.os.all.insul.levels <-  proportions_one_group(item15.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item15.os.final <- rbind.data.frame(item15.os.summary
                                 , item15.os.all.frame.types
                                 , item15.os.all.insul.levels
                                 , stringsAsFactors = F)
item15.os.final <- item15.os.final[which(item15.os.final$rvalue.bins.SF != "Total"),]
item15.os.final$HomeYearBuilt_bins3[which(item15.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


##cast data
item15.os.cast <- dcast(setDT(item15.os.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))


item15.os.table <- data.frame("BuildingType"     = item15.os.cast$BuildingType
                              ,"Housing.Vintage" = item15.os.cast$HomeYearBuilt_bins3
                              ,"Percent.R0"      = item15.os.cast$Percent_R0
                              ,"SE.R0"           = item15.os.cast$SE_R0
                              ,"Percent.R1.R10"  = item15.os.cast$Percent_R1.R10
                              ,"SE.R1.R10"       = item15.os.cast$SE_R1.R10
                              ,"Percent.R11.R16" = item15.os.cast$Percent_R11.R16
                              ,"SE.R11.R16"      = item15.os.cast$SE_R11.R16
                              ,"Percent.R17.R22" = item15.os.cast$Percent_R17.R22
                              ,"SE.R17.R22"      = item15.os.cast$SE_R17.R22
                              ,"Percent.RGT22"   = NA#item15.os.cast$Percent_RGT22
                              ,"SE.RGT22"        = NA#item15.os.cast$SE_RGT22
                              ,"n"               = item15.os.cast$`n_All Housing Vintages`
)

# row ordering example code
unique(item15.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item15.os.table <- item15.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item15.os.table <- data.frame(item15.os.table)

item15.os.table.SF <- item15.os.table[which(item15.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item15.os.table.SF, "SF", "Table 22", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
# Item 16: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, 2017 scl PUGET SOUND  (SF table 23)
#############################################################################################
## Note: For this table, you must run up to item12.os.dat1 for the cleaned data
stopifnot(subset.ind == "SCL")
item16.os.data <- item12.os.data[which(item12.os.data$CK_Building_ID == "2017 RBSA PS"),]


##############################################
# Weighted Analysis - Single Family
##############################################
item16.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item16.os.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item16.os.summary <- item16.os.summary[which(item16.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item16.os.all.frame.types <- proportions_one_group(item16.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item16.os.all.insul.levels <-  proportions_one_group(item16.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item16.os.final <- rbind.data.frame(item16.os.summary
                                 , item16.os.all.frame.types
                                 , item16.os.all.insul.levels
                                 , stringsAsFactors = F)
item16.os.final <- item16.os.final[which(item16.os.final$rvalue.bins.SF != "Total"),]
item16.os.final$HomeYearBuilt_bins3[which(item16.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

item16.os.cast <- dcast(setDT(item16.os.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item16.os.cast)
item16.os.table <- data.frame("BuildingType"     = item16.os.cast$BuildingType
                              ,"Housing.Vintage" = item16.os.cast$HomeYearBuilt_bins3
                              ,"Percent.R0"      = item16.os.cast$w.percent_R0
                              ,"SE.R0"           = item16.os.cast$w.SE_R0
                              ,"Percent.R1.R10"  = item16.os.cast$w.percent_R1.R10
                              ,"SE.R1.R10"       = item16.os.cast$w.SE_R1.R10
                              ,"Percent.R11.R16" = item16.os.cast$w.percent_R11.R16
                              ,"SE.R11.R16"      = item16.os.cast$w.SE_R11.R16
                              ,"Percent.R17.R22" = item16.os.cast$w.percent_R17.R22
                              ,"SE.R17.R22"      = item16.os.cast$w.SE_R17.R22
                              ,"Percent.RGT22"   = item16.os.cast$w.percent_RGT22
                              ,"SE.RGT22"        = item16.os.cast$w.SE_RGT22
                              ,"n"               = item16.os.cast$`n_All Housing Vintages`
                              ,"EB.R0"           = item16.os.cast$EB_R0
                              ,"EB.R1.R10"       = item16.os.cast$EB_R1.R10
                              ,"EB.R11.R16"      = item16.os.cast$EB_R11.R16
                              ,"EB.R17.R22"      = item16.os.cast$EB_R17.R22
                              ,"EB.RGT22"        = item16.os.cast$EB_RGT22
                           
)

# row ordering example code
unique(item16.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item16.os.table <- item16.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item16.os.table <- data.frame(item16.os.table)

item16.os.table.SF <- item16.os.table[which(item16.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item16.os.table.SF, "SF", "Table 23", weighted = TRUE, osIndicator = export.ind, OS = T)



################################
# Unweighted Analysis
################################
item16.os.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item16.os.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item16.os.summary <- item16.os.summary[which(item16.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item16.os.all.frame.types <- proportions_one_group(item16.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item16.os.all.insul.levels <-  proportions_one_group(item16.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item16.os.final <- rbind.data.frame(item16.os.summary
                                 , item16.os.all.frame.types
                                 , item16.os.all.insul.levels
                                 , stringsAsFactors = F)
item16.os.final <- item16.os.final[which(item16.os.final$rvalue.bins.SF != "Total"),]
item16.os.final$HomeYearBuilt_bins3[which(item16.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


##cast data
item16.os.cast <- dcast(setDT(item16.os.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))

item16.os.table <- data.frame("BuildingType"     = item16.os.cast$BuildingType
                              ,"Housing.Vintage" = item16.os.cast$HomeYearBuilt_bins3
                              ,"Percent.R0"      = item16.os.cast$Percent_R0
                              ,"SE.R0"           = item16.os.cast$SE_R0
                              ,"Percent.R1.R10"  = item16.os.cast$Percent_R1.R10
                              ,"SE.R1.R10"       = item16.os.cast$SE_R1.R10
                              ,"Percent.R11.R16" = item16.os.cast$Percent_R11.R16
                              ,"SE.R11.R16"      = item16.os.cast$SE_R11.R16
                              ,"Percent.R17.R22" = item16.os.cast$Percent_R17.R22
                              ,"SE.R17.R22"      = item16.os.cast$SE_R17.R22
                              ,"Percent.RGT22"   = item16.os.cast$Percent_RGT22
                              ,"SE.RGT22"        = item16.os.cast$SE_RGT22
                              ,"n"               = item16.os.cast$`n_All Housing Vintages`
)

# row ordering example code
unique(item16.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item16.os.table <- item16.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item16.os.table <- data.frame(item16.os.table)

item16.os.table.SF <- item16.os.table[which(item16.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item16.os.table.SF, "SF", "Table 23", weighted = FALSE, osIndicator = export.ind, OS = T)






#############################################################################################
# Item 18: DISTRIBUTION OF OBSERVED WALL SHEATHING INSULATION BY FRAMING TYPE (SF table 25)
#############################################################################################
item18.os.dat <- prep.dat4.9#[-grep("knee|adiabatic", prep.dat4.9$Wall.Type, ignore.case = T),]
names(item18.os.dat)[which(names(item18.os.dat) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"
item18.os.dat <- item18.os.dat[which(!is.na(item18.os.dat$Wall.Type)),]

item18.os.dat$Insulation.Levels <- item18.os.dat$exterior.inches1
unique(item18.os.dat$Insulation.Levels)

item18.os.merge <- left_join(os.dat, item18.os.dat)
item18.os.merge <- item18.os.merge[which(item18.os.merge$CK_Building_ID == subset.ind),]
item18.os.customer <- summarise(group_by(item18.os.merge, CK_Cadmus_ID, Wall.Type)
                             ,insulation.levels = sum(Insulation.Levels))

item18.os.customer$insulation.levels[which(item18.os.customer$insulation.levels == 0)] <- "None"

item18.os.merge <- left_join(os.dat, item18.os.customer)
item18.os.merge <- item18.os.merge[which(!is.na(item18.os.merge$Wall.Type)),]
unique(item18.os.merge$Wall.Type)
item18.os.merge$count <- 1

item18.os.data <- weightedData(unique(item18.os.merge[-which(colnames(item18.os.merge) %in% c("Category"
                                                                                     ,"Wall.Type"
                                                                                     ,"count"
                                                                                     ,"insulation.levels"))]))
item18.os.data <- left_join(item18.os.data, item18.os.merge[which(colnames(item18.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Category"
                                                                                       ,"Wall.Type"
                                                                                       ,"count"
                                                                                       ,"insulation.levels"))])



############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item18.os.by.frame.type <- proportionRowsAndColumns1(item18.os.data
                                                  , valueVariable       = 'count'
                                                  , columnVariable      = 'Wall.Type'
                                                  , rowVariable         = 'insulation.levels'
                                                  , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item18.os.across.frame.types <- proportions_one_group(item18.os.data
                                                   , valueVariable    = 'count'
                                                   , groupingVariable = 'insulation.levels'
                                                   , total.name       = 'All Framing Types'
                                                   , columnName       = 'Wall.Type'
                                                   , weighted = TRUE
                                                   ,two.prop.total = TRUE
)
# row bind summaries
item18.os.final <- rbind.data.frame(item18.os.by.frame.type, item18.os.across.frame.types, stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item18.os.final <- item18.os.final[which(item18.os.final$Wall.Type != "Remove"),]



item18.os.cast <- dcast(setDT(item18.os.final),
                     formula   = BuildingType +  Wall.Type ~ insulation.levels,
                     value.var = c("w.percent", "w.SE", "count","n","N", "EB"))
names(item18.os.cast)

item18.os.table <- data.frame("BuildingType"       = item18.os.cast$BuildingType
                              ,"Wall.Type"         = item18.os.cast$Wall.Type
                              ,"Percent_1_inch"    = item18.os.cast$w.percent_1
                              ,"SE_1_inch"         = item18.os.cast$w.SE_1
                              ,"Percent_1.5_inch"  = item18.os.cast$w.percent_1.5
                              ,"SE_1.5_inch"       = item18.os.cast$w.SE_1.5
                              ,"Percent_2_inch"    = item18.os.cast$w.percent_2
                              ,"SE_2_inch"         = item18.os.cast$w.SE_2
                              ,"Percent_None"      = item18.os.cast$w.percent_None
                              ,"SE_None"           = item18.os.cast$w.SE_None
                              ,"n"                 = item18.os.cast$n_Total
                              ,"EB_1_inch"         = item18.os.cast$EB_1
                              ,"EB_1.5_inch"       = item18.os.cast$EB_1.5
                              ,"EB_2_inch"         = item18.os.cast$EB_2
                              ,"EB_None"           = item18.os.cast$EB_None
)

# row ordering example code
levels(item18.os.table$Wall.Type)
rowOrder <- c("Framed 2x4"
              ,"Framed 2x6"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"Masonry"
              ,"Masonry (Basement)"
              ,"ICF"
              ,"SIP"
              ,"Log"
              ,"Unknown"
              ,"Knee Wall"
              ,"Adiabatic"
              ,"All Framing Types")
item18.os.table <- item18.os.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item18.os.table <- data.frame(item18.os.table)


item18.os.table.SF <- item18.os.table[which(item18.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item18.os.table.SF, "SF", "Table 25", weighted = TRUE, osIndicator = export.ind, OS = T)


############################################################################################################
# Unweighted Analysis - Single Family
############################################################################################################
item18.os.by.frame.type <- proportions_two_groups_unweighted(item18.os.data
                                                          , valueVariable       = 'count'
                                                          , columnVariable      = 'Wall.Type'
                                                          , rowVariable         = 'insulation.levels'
                                                          , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item18.os.across.frame.types <- proportions_one_group(item18.os.data
                                                   , valueVariable    = 'count'
                                                   , groupingVariable = 'insulation.levels'
                                                   , total.name       = 'All Framing Types'
                                                   , columnName       = 'Wall.Type'
                                                   , weighted = FALSE
                                                   ,two.prop.total = TRUE
)
# row bind summaries
item18.os.final <- rbind.data.frame(item18.os.by.frame.type, item18.os.across.frame.types, stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item18.os.final <- item18.os.final[which(item18.os.final$Wall.Type != "Remove"),]



item18.os.cast <- dcast(setDT(item18.os.final),
                     formula   = BuildingType +  Wall.Type ~ insulation.levels, sum,
                     value.var = c("Percent", "SE", "Count","n"))

item18.os.table <- data.frame("BuildingType"       = item18.os.cast$BuildingType
                              ,"Wall.Type"         = item18.os.cast$Wall.Type
                              ,"Percent_1_inch"    = item18.os.cast$Percent_1
                              ,"SE_1_inch"         = item18.os.cast$SE_1
                              ,"Percent_1.5_inch"  = item18.os.cast$Percent_1.5
                              ,"SE_1.5_inch"       = item18.os.cast$SE_1.5
                              ,"Percent_2_inch"    = item18.os.cast$Percent_2
                              ,"SE_2_inch"         = item18.os.cast$SE_2
                              ,"Percent_None"      = item18.os.cast$Percent_None
                              ,"SE_None"           = item18.os.cast$SE_None
                              ,"n"                 = item18.os.cast$n_Total
)

# row ordering example code
levels(item18.os.table$Wall.Type)
rowOrder <- c("Framed 2x4"
              ,"Framed 2x6"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"Masonry"
              ,"Masonry (Basement)"
              ,"ICF"
              ,"SIP"
              ,"Log"
              ,"Unknown"
              ,"Knee Wall"
              ,"Adiabatic"
              ,"All Framing Types")
item18.os.table <- item18.os.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item18.os.table <- data.frame(item18.os.table)

item18.os.table.SF <- item18.os.table[which(item18.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item18.os.table.SF, "SF", "Table 25", weighted = FALSE, osIndicator = export.ind, OS = T)








#############################################################################################
# Item 17: DISTRIBUTION OF Masonry Wall Insulation Levels by Vintage (SF table 24)
#############################################################################################
item17.os.dat <- prep.dat5[grep("masonry|icf",prep.dat5$Wall.Type, ignore.case = T),]

#weight the u factor per home -- where weights are the wall area within home
item17.os.weightedU <- summarise(group_by(item17.os.dat, CK_Cadmus_ID)
                              ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
item17.os.weightedU$aveRval <- (1 / as.numeric(as.character(item17.os.weightedU$aveUval)))
item17.os.weightedU$aveRval[which(item17.os.weightedU$aveRval %in% c("NaN",1))] <- 0
item17.os.weightedU$aveUval[which(item17.os.weightedU$aveUval == "NaN")] <- 1

unique(item17.os.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
item17.os.wall.unique <- unique(item17.os.dat[which(colnames(item17.os.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
item17.os.dat1 <- left_join(item17.os.weightedU, item17.os.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned scl data
item17.os.dat2 <- left_join(item17.os.dat1, os.dat)
item17.os.dat2$aveUval[which(is.na(item17.os.dat2$aveUval))] <- 0
item17.os.dat2$aveRval[which(is.na(item17.os.dat2$aveRval))] <- 0
item17.os.dat2 <- item17.os.dat2[which(item17.os.dat2$CK_Building_ID == subset.ind),]

#Bin R values -- SF only
item17.os.dat2$rvalue.bins <- "Unknown"
item17.os.dat2$rvalue.bins[which(item17.os.dat2$aveRval < 1)] <- "None"
item17.os.dat2$rvalue.bins[which(item17.os.dat2$aveRval >=  1 & item17.os.dat2$aveRval < 10)]  <- "R1.R9"
item17.os.dat2$rvalue.bins[which(item17.os.dat2$aveRval >= 10 & item17.os.dat2$aveRval < 16)]  <- "R10.R15"
item17.os.dat2$rvalue.bins[which(item17.os.dat2$aveRval >= 16 & item17.os.dat2$aveRval < 21)]  <- "R16.R20"
item17.os.dat2$rvalue.bins[which(item17.os.dat2$aveRval >= 21)] <- "RGT21"
unique(item17.os.dat2$rvalue.bins)

item17.os.dat2$count <- 1

item17.os.dat3 <- item17.os.dat2[which(item17.os.dat2$rvalue.bins != "Unknown"),]
colnames(item17.os.dat3)

item17.os.merge <- left_join(os.dat, item17.os.dat3)
item17.os.merge <- item17.os.merge[which(!is.na(item17.os.merge$count)),]
item17.os.merge <- item17.os.merge[which(!is.na(item17.os.merge$HomeYearBuilt)),]

item17.os.data <- weightedData(unique(item17.os.merge[-which(colnames(item17.os.merge) %in% c("Wall.Type"
                                                                                     ,"aveUval"
                                                                                     ,"aveRval"
                                                                                     ,"rvalue.bins"
                                                                                     ,"count"))]))
item17.os.data <- left_join(item17.os.data, item17.os.merge[which(colnames(item17.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins"
                                                                                       ,"count"))])
############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item17.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item17.os.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins'
                                            , aggregateColumnName = "All Housing Vintages"
)
item17.os.summary <- item17.os.summary[which(item17.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item17.os.all.frame.types <- proportions_one_group(item17.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item17.os.all.insul.levels <-  proportions_one_group(item17.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item17.os.final <- rbind.data.frame(item17.os.summary
                                 , item17.os.all.frame.types
                                 , item17.os.all.insul.levels
                                 , stringsAsFactors = F)
item17.os.final <- item17.os.final[which(item17.os.final$rvalue.bins != "Total"),]
item17.os.final$HomeYearBuilt_bins3[which(item17.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


item17.os.cast <- dcast(setDT(item17.os.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ rvalue.bins,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item17.os.cast)
#join all insulation levels onto rvalue summary
item17.os.table <- data.frame("BuildingType"     = item17.os.cast$BuildingType
                              ,"Housing.Vintage" = item17.os.cast$HomeYearBuilt_bins3
                              ,"Percent.None"    = item17.os.cast$w.percent_None
                              ,"SE.None"         = item17.os.cast$w.SE_None
                              ,"Percent.R1.R9"   = item17.os.cast$w.percent_R1.R9
                              ,"SE.R1.R9"        = item17.os.cast$w.SE_R1.R9
                              ,"Percent.R10.R15" = item17.os.cast$w.percent_R10.R15
                              ,"SE.R10.R15"      = item17.os.cast$w.SE_R10.R15
                              ,"Percent.R16.R20" = item17.os.cast$w.percent_R16.R20
                              ,"SE.R16.R20"      = item17.os.cast$w.SE_R16.R20
                              ,"Percent.RGT21"   = item17.os.cast$w.percent_RGT21
                              ,"SE.RGT21"        = item17.os.cast$w.SE_RGT21
                              ,"n"               = item17.os.cast$`n_All Housing Vintages`
                              ,'EB.None'         = item17.os.cast$EB_None
                              ,"EB.R1.R9"        = item17.os.cast$EB_R1.R9
                              ,"EB.R10.R15"      = item17.os.cast$EB_R10.R15
                              ,"EB.R16.R20"      = item17.os.cast$EB_R16.R20
                              ,"EB.RGT21"          = item17.os.cast$EB_RGT21
)

# row ordering example code
levels(item17.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item17.os.table <- item17.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item17.os.table <- data.frame(item17.os.table)


item17.os.table.SF <- item17.os.table[which(item17.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item17.os.table.SF, "SF", "Table 24", weighted = TRUE, osIndicator = export.ind, OS = T)

############################################################################################################
# unweighted Analysis - Single Family
############################################################################################################
item17.os.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item17.os.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item17.os.summary <- item17.os.summary[which(item17.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item17.os.all.frame.types <- proportions_one_group(item17.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item17.os.all.insul.levels <-  proportions_one_group(item17.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item17.os.final <- rbind.data.frame(item17.os.summary
                                 , item17.os.all.frame.types
                                 , item17.os.all.insul.levels
                                 , stringsAsFactors = F)
item17.os.final <- item17.os.final[which(item17.os.final$rvalue.bins != "Total"),]
item17.os.final$HomeYearBuilt_bins3[which(item17.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


item17.os.cast <- dcast(setDT(item17.os.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ rvalue.bins,
                     value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item17.os.table <- data.frame("BuildingType"     = item17.os.cast$BuildingType
                              ,"Housing.Vintage" = item17.os.cast$HomeYearBuilt_bins3
                              ,"Percent.None"    = item17.os.cast$Percent_None
                              ,"SE.None"         = item17.os.cast$SE_None
                              ,"Percent.R1.R9"   = item17.os.cast$Percent_R1.R9
                              ,"SE.R1.R9"        = item17.os.cast$SE_R1.R9
                              ,"Percent.R10.R15" = item17.os.cast$Percent_R10.R15
                              ,"SE.R10.R15"      = item17.os.cast$SE_R10.R15
                              ,"Percent.R16.R20" = item17.os.cast$Percent_R16.R20
                              ,"SE.R16.R20"      = item17.os.cast$SE_R16.R20
                              ,"Percent.RGT21"   = item17.os.cast$Percent_RGT21
                              ,"SE.RGT21"        = item17.os.cast$SE_RGT21
                              ,"n"               = item17.os.cast$`n_All Housing Vintages`
)

# row ordering example code
levels(item17.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item17.os.table <- item17.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item17.os.table <- data.frame(item17.os.table)


item17.os.table.SF <- item17.os.table[which(item17.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item17.os.table.SF, "SF", "Table 24", weighted = FALSE, osIndicator = export.ind, OS = T)






#############################################################################################
# Item 17: DISTRIBUTION OF Masonry Wall Insulation Levels by Vintage (SF table 24)
#############################################################################################
item17A.os.dat <- prep.dat5[grep("masonry|icf",prep.dat5$Wall.Type, ignore.case = T),]

#weight the u factor per home -- where weights are the wall area within home
item17A.os.weightedU <- summarise(group_by(item17A.os.dat, CK_Cadmus_ID)
                                 ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
item17A.os.weightedU$aveRval <- (1 / as.numeric(as.character(item17A.os.weightedU$aveUval)))
item17A.os.weightedU$aveRval[which(item17A.os.weightedU$aveRval %in% c("NaN",1))] <- 0
item17A.os.weightedU$aveUval[which(item17A.os.weightedU$aveUval == "NaN")] <- 1

unique(item17A.os.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
item17A.os.wall.unique <- unique(item17A.os.dat[which(colnames(item17A.os.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
item17A.os.dat1 <- left_join(item17A.os.weightedU, item17A.os.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned scl data
item17A.os.dat2 <- left_join(item17A.os.dat1, os.dat)
item17A.os.dat2$aveUval[which(is.na(item17A.os.dat2$aveUval))] <- 0
item17A.os.dat2$aveRval[which(is.na(item17A.os.dat2$aveRval))] <- 0

#Bin R values -- SF only
item17A.os.dat2$rvalue.bins <- "Unknown"
item17A.os.dat2$rvalue.bins[which(item17A.os.dat2$aveRval < 1)] <- "None"
item17A.os.dat2$rvalue.bins[which(item17A.os.dat2$aveRval >=  1 & item17A.os.dat2$aveRval < 10)]  <- "R1.R9"
item17A.os.dat2$rvalue.bins[which(item17A.os.dat2$aveRval >= 10 & item17A.os.dat2$aveRval < 16)]  <- "R10.R15"
item17A.os.dat2$rvalue.bins[which(item17A.os.dat2$aveRval >= 16 & item17A.os.dat2$aveRval < 21)]  <- "R16.R20"
item17A.os.dat2$rvalue.bins[which(item17A.os.dat2$aveRval >= 21)] <- "RGT21"
unique(item17A.os.dat2$rvalue.bins)

item17A.os.dat2$count <- 1

item17A.os.dat3 <- item17A.os.dat2[which(item17A.os.dat2$rvalue.bins != "Unknown"),]
colnames(item17A.os.dat3)

item17A.os.merge <- left_join(os.dat, item17A.os.dat3)
item17A.os.merge <- item17A.os.merge[which(!is.na(item17A.os.merge$count)),]

item17A.os.data <- weightedData(unique(item17A.os.merge[-which(colnames(item17A.os.merge) %in% c("Wall.Type"
                                                                                              ,"aveUval"
                                                                                              ,"aveRval"
                                                                                              ,"rvalue.bins"
                                                                                              ,"count"))]))
item17A.os.data <- left_join(item17A.os.data, item17A.os.merge[which(colnames(item17A.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"Wall.Type"
                                                                                                   ,"aveUval"
                                                                                                   ,"aveRval"
                                                                                                   ,"rvalue.bins"
                                                                                                   ,"count"))])
############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
item17A.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item17A.os.data
                                               , valueVariable       = 'count'
                                               , columnVariable      = 'CK_Building_ID'
                                               , rowVariable         = 'rvalue.bins'
                                               , aggregateColumnName = "All Samples"
)
item17A.os.summary <- item17A.os.summary[which(item17A.os.summary$CK_Building_ID != "All Samples"),]

## Summary only for "All Frame Types"
item17A.os.all.frame.types <- proportions_one_group(item17A.os.data
                                                   ,valueVariable    = "count"
                                                   ,groupingVariable = "rvalue.bins"
                                                   ,total.name       = "All Samples"
                                                   ,columnName       = "CK_Building_ID"
                                                   ,weighted = TRUE
                                                   ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item17A.os.all.insul.levels <-  proportions_one_group(item17A.os.data
                                                     ,valueVariable    = "count"
                                                     ,groupingVariable = "CK_Building_ID"
                                                     ,total.name       = "All Insulation Levels"
                                                     ,columnName       = "rvalue.bins"
                                                     ,weighted = TRUE
                                                     ,two.prop.total = TRUE
)


#merge together!
item17A.os.final <- rbind.data.frame(item17A.os.summary
                                    , item17A.os.all.frame.types
                                    , item17A.os.all.insul.levels
                                    , stringsAsFactors = F)
item17A.os.final <- item17A.os.final[which(item17A.os.final$rvalue.bins != "Total"),]
item17A.os.final <- item17A.os.final[which(item17A.os.final$CK_Building_ID != "All Samples"),]


item17A.os.cast <- dcast(setDT(item17A.os.final),
                        formula   = BuildingType + rvalue.bins ~ CK_Building_ID,
                        value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

if(os.ind == "scl"){
  #join all insulation levels onto rvalue summary
  item17A.os.table <- data.frame("BuildingType"     = item17A.os.cast$BuildingType
                                 ,"Insulation.Level" = item17A.os.cast$rvalue.bins
                                 ,"Percent_SCL.GenPop"   = item17A.os.cast$`w.percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item17A.os.cast$`w.SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item17A.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item17A.os.cast$`w.percent_SCL LI`
                                 ,"SE_SCL.LI"            = item17A.os.cast$`w.SE_SCL LI`
                                 ,"n_SCL.LI"             = item17A.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item17A.os.cast$`w.percent_SCL EH`
                                 ,"SE_SCL.EH"            = item17A.os.cast$`w.SE_SCL EH`
                                 ,"n_SCL.EH"             = item17A.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item17A.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item17A.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item17A.os.cast$`n_2017 RBSA PS`
                                 ,"EB_SCL.GenPop"        = item17A.os.cast$`EB_SCL GenPop`
                                 ,"EB_SCL.LI"            = item17A.os.cast$`EB_SCL LI`
                                 ,"EB_SCL.EH"            = item17A.os.cast$`EB_SCL EH`
                                 ,"EB_2017.RBSA.PS"      = item17A.os.cast$`EB_2017 RBSA PS`
  )
  
}else if(os.ind == "snopud"){
  #join all insulation levels onto rvalue summary
  item17A.os.table <- data.frame("BuildingType"     = item17A.os.cast$BuildingType
                                 ,"Insulation.Level" = item17A.os.cast$rvalue.bins
                                 ,"Percent_SnoPUD"          = item17A.os.cast$`w.percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item17A.os.cast$`w.SE_SnoPUD`
                                 ,"n_SnoPUD"                = item17A.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item17A.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item17A.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item17A.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item17A.os.cast$`w.percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item17A.os.cast$`w.SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item17A.os.cast$`n_2017 RBSA NW`
                                 ,"EB_SnoPUD"               = item17A.os.cast$`EB_SnoPUD`
                                 ,"EB_2017.RBSA.PS"         = item17A.os.cast$`EB_2017 RBSA PS`
                                 ,"EB_RBSA.NW"              = item17A.os.cast$`EB_2017 RBSA NW`
  )
  
}

# row ordering example code
levels(item17A.os.table$Insulation.Level)
rowOrder <- c("None"
              ,"R1.R9"
              ,"R10.R15"
              ,"R16.R20"
              ,"RGT21"
              ,"All Insulation Levels")
item17A.os.table <- item17A.os.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)  
item17A.os.table <- data.frame(item17A.os.table)


item17A.os.table.SF <- item17A.os.table[which(item17A.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item17A.os.table.SF, "SF", "Table 24A", weighted = TRUE, osIndicator = export.ind, OS = T)

############################################################################################################
# unweighted Analysis - Single Family
############################################################################################################
item17A.os.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item17A.os.data
                                                       , valueVariable       = 'count'
                                                       , columnVariable      = 'CK_Building_ID'
                                                       , rowVariable         = 'rvalue.bins'
                                                       , aggregateColumnName = "All Samples"
)
item17A.os.summary <- item17A.os.summary[which(item17A.os.summary$CK_Building_ID != "All Samples"),]
item17A.os.summary <- item17A.os.summary[which(item17A.os.summary$rvalue.bins != "Total"),]

## Summary only for "All Frame Types"
item17A.os.all.frame.types <- proportions_one_group(item17A.os.data
                                                   ,valueVariable    = "count"
                                                   ,groupingVariable = "rvalue.bins"
                                                   ,total.name       = "All Samples"
                                                   ,columnName       = "CK_Building_ID"
                                                   ,weighted = FALSE
                                                   ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item17A.os.all.insul.levels <-  proportions_one_group(item17A.os.data
                                                     ,valueVariable    = "count"
                                                     ,groupingVariable = "CK_Building_ID"
                                                     ,total.name       = "All Insulation Levels"
                                                     ,columnName       = "rvalue.bins"
                                                     ,weighted = FALSE
                                                     ,two.prop.total = TRUE
)
item17A.os.all.insul.levels <- item17A.os.all.insul.levels[which(item17A.os.all.insul.levels$CK_Building_ID != "Total"),]

#merge together!
item17A.os.final <- rbind.data.frame(item17A.os.summary
                                    , item17A.os.all.frame.types
                                    , item17A.os.all.insul.levels
                                    , stringsAsFactors = F)
item17A.os.final <- item17A.os.final[which(item17A.os.final$rvalue.bins != "Total"),]
item17A.os.final <- item17A.os.final[which(item17A.os.final$CK_Building_ID != "All Samples"),]


item17A.os.cast <- dcast(setDT(item17A.os.final),
                        formula   = BuildingType + rvalue.bins ~ CK_Building_ID,
                        value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item17A.os.table <- data.frame("BuildingType"     = item17A.os.cast$BuildingType
                               ,"Insulation.Level" = item17A.os.cast$rvalue.bins
                               ,"Percent_SCL.GenPop"   = item17A.os.cast$`Percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item17A.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item17A.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = item17A.os.cast$`Percent_SCL LI`
                               ,"SE_SCL.LI"            = item17A.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = item17A.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = item17A.os.cast$`Percent_SCL EH`
                               ,"SE_SCL.EH"            = item17A.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = item17A.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = item17A.os.cast$`Percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item17A.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item17A.os.cast$`n_2017 RBSA PS`
)

# row ordering example code
levels(item17A.os.table$Insulation.Level)
rowOrder <- c("None"
              ,"R1.R9"
              ,"R10.R15"
              ,"R16.R20"
              ,"RGT21"
              ,"All Insulation Levels")
item17A.os.table <- item17A.os.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)  
item17A.os.table <- data.frame(item17A.os.table)

item17A.os.table.SF <- item17A.os.table[which(item17A.os.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item17A.os.table.SF, "SF", "Table 24A", weighted = FALSE, osIndicator = export.ind, OS = T)
