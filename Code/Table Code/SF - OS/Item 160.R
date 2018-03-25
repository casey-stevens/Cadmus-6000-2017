#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())
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

# Read in data for analysis
# envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Read in data for analysis
# mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]


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
length(unique(item160.dat.12$CK_Cadmus_ID))
#check uniques
unique(item160.dat.12$Primary.Heating.System)
item160.dat.12$count <- 1

item160.dat.13 <- unique(item160.dat.12[which(item160.dat.12$Heating.Fuel == "Electric"),])

item160.sum <- summarise(group_by(item160.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item160.sum$Count <- 1
which(duplicated(item160.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item160.sum$Heating.Fuel)

item160.merge <- left_join(rbsa.dat, item160.sum)
item160.merge <- item160.merge[which(!is.na(item160.merge$Heating.Fuel)),]

item160.mechanical <- item160.merge





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
prep.dat7 <- left_join(prep.dat6, rbsa.dat)
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










#############################################################################################
# Item 160: DISTRIBUTION OF FRAME WALL INSULATION LEVELS, ELECTRICALLY HEATED HOMES  (SF table B-5)
#############################################################################################
item160.dat <- prep.dat5[grep("framed|alternative",prep.dat5$Wall.Type, ignore.case = T),]

#weight the u factor per home -- where weights are the wall area within home
item160.weightedU <- summarise(group_by(item160.dat, CK_Cadmus_ID, Wall.Type)
                              ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)

#back-calculate the weight r values
item160.weightedU$aveRval <- (1 / as.numeric(as.character(item160.weightedU$aveUval)))
item160.weightedU$aveRval[which(item160.weightedU$aveRval %in% c("NaN",1))] <- 0
item160.weightedU$aveUval[which(item160.weightedU$aveUval == "NaN")] <- 1
unique(item160.weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
item160.wall.unique <- unique(item160.dat[which(colnames(item160.dat) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
item160.dat1 <- left_join(item160.weightedU, item160.wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item160.dat2 <- left_join(item160.dat1, rbsa.dat)
item160.dat2$aveUval[which(is.na(item160.dat2$aveUval))] <- 0
item160.dat2$aveRval[which(is.na(item160.dat2$aveRval))] <- 0

#Bin R values -- SF only
item160.dat2$rvalue.bins <- "Unknown"
item160.dat2$rvalue.bins[which(item160.dat2$aveRval ==  0)] <- "R0"
item160.dat2$rvalue.bins[which(item160.dat2$aveRval >   0 & item160.dat2$aveRval < 11)]  <- "R1.R10"
item160.dat2$rvalue.bins[which(item160.dat2$aveRval >= 11 & item160.dat2$aveRval < 17)]  <- "R11.R16"
item160.dat2$rvalue.bins[which(item160.dat2$aveRval >= 17 & item160.dat2$aveRval < 23)]  <- "R17.R22"
item160.dat2$rvalue.bins[which(item160.dat2$aveRval >= 22)] <- "RGT22"
unique(item160.dat2$rvalue.bins)

item160.dat2$count <- 1
colnames(item160.dat2)

item160.merge <- left_join(item160.mechanical, item160.dat2)
item160.merge <- item160.merge[which(!is.na(item160.merge$Wall.Type)),]

item160.data <- weightedData(unique(item160.merge[which(colnames(item160.merge) %in% colnames(rbsa.dat))]))
item160.data <- left_join(item160.data, item160.merge[which(colnames(item160.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Heating.Fuel"
                                                                                       ,"Count"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins"
                                                                                       ,"count"))])



################################
# Weighted Analysis
################################
item160.summary <- proportionRowsAndColumns1(CustomerLevelData     = item160.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'Wall.Type'
                                            , rowVariable         = 'rvalue.bins'
                                            , aggregateColumnName = "All Frame Types"
)
item160.summary <- item160.summary[which(item160.summary$Wall.Type != "All Frame Types"),]

## Summary only for "All Frame Types"
item160.all.frame.types <- proportions_one_group(item160.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Frame Types"
                                                ,columnName       = "Wall.Type"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE)

## Summary for only "All Insulation Levels"
item160.all.insul.levels <-  proportions_one_group(item160.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "Wall.Type"
                                                  ,total.name       = "All Insulation Levels"
                                                  ,columnName       = "rvalue.bins"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE)
item160.all.insul.levels$Wall.Type[which(item160.all.insul.levels$Wall.Type == "Total")] <- "All Frame Types"


#merge together!
item160.final <- rbind.data.frame(item160.summary
                                 , item160.all.frame.types
                                 , item160.all.insul.levels
                                 , stringsAsFactors = F)

##cast data
item160.cast <- dcast(setDT(item160.final),
                     formula   = BuildingType + Wall.Type ~ rvalue.bins,
                     value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

#join all insulation levels onto rvalue summary
item160.table <- data.frame("BuildingType"    = item160.cast$BuildingType
                           ,"Wall.Type"       = item160.cast$Wall.Type
                           ,"Percent.R0"      = item160.cast$w.percent_R0
                           ,"SE.R0"           = item160.cast$w.SE_R0
                           # ,"count.R0"        = item160.cast$count_R0
                           ,"Percent.R1.R10"  = item160.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item160.cast$w.SE_R1.R10
                           # ,"count.R1.R10"    = item160.cast$count_R1.R10
                           ,"Percent.R11.R16" = item160.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item160.cast$w.SE_R11.R16
                           # ,"count.R11.R16"   = item160.cast$count_R11.R16
                           ,"Percent.R17.R22" = item160.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item160.cast$w.SE_R17.R22
                           # ,"count.R17.R22"   = item160.cast$count_R17.R22
                           ,"Percent.RGT22"   = item160.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item160.cast$w.SE_RGT22
                           # ,"count.RGT22"     = item160.cast$count_RGT22
                           ,"Percent_All Insulation Levels" = item160.cast$`w.percent_All Insulation Levels`
                           ,"SE.All Insulation Levels"      = item160.cast$`w.SE_All Insulation Levels`
                           # ,"count.All Insulation Levels"   = item160.cast$`count_All Insulation Levels`
                           ,"n"                    = item160.cast$`n_All Insulation Levels`
                           ,"EB.R0"           = item160.cast$EB_R0
                           ,"EB.R1.R10"       = item160.cast$EB_R1.R10
                           ,"EB.R11.R16"      = item160.cast$EB_R11.R16
                           ,"EB.R17.R22"      = item160.cast$EB_R17.R22
                           ,"EB.RGT22"        = item160.cast$EB_RGT22
                           ,"EB.All Insulation Levels"      = item160.cast$`EB_All Insulation Levels`
)

# row ordering example code
unique(item160.table$Wall.Type)
rowOrder <- c("Framed 2x2"
              ,"Framed 2x3"
              ,"Framed 2x4"
              ,"Framed 2x6"
              ,"Framed 2x8"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"All Frame Types")
item160.table <- item160.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item160.table <- data.frame(item160.table)

item160.table.SF <- item160.table[which(item160.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item160.table.SF, "SF", "Table B-5", weighted = TRUE)



################################
# Unweighted Analysis
################################
item160.summary <- proportions_two_groups_unweighted(CustomerLevelData = item160.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'Wall.Type'
                                                    , rowVariable         = 'rvalue.bins'
                                                    , aggregateColumnName = "All Frame Types"
)
item160.summary <- item160.summary[which(item160.summary$Wall.Type != "All Frame Types"),]

## Summary only for "All Frame Types"
item160.all.frame.types <- proportions_one_group(CustomerLevelData =  item160.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Frame Types"
                                                ,columnName       = "Wall.Type"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE)

## Summary for only "All Insulation Levels"
item160.all.insul.levels <-  proportions_one_group(item160.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "Wall.Type"
                                                  ,total.name       = "All Insulation Levels"
                                                  ,columnName       = "rvalue.bins"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE)
item160.all.insul.levels$Wall.Type[which(item160.all.insul.levels$Wall.Type == "Total")] <- "All Frame Types"


#merge together!
item160.final <- rbind.data.frame(item160.summary
                                 , item160.all.frame.types
                                 , item160.all.insul.levels
                                 , stringsAsFactors = F)

##cast data
item160.cast <- dcast(setDT(item160.final),
                     formula   = BuildingType + Wall.Type ~ rvalue.bins,
                     value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item160.table <- data.frame("BuildingType"     = item160.cast$BuildingType
                           ,"Wall.Type"       = item160.cast$Wall.Type
                           ,"Percent.R0"      = item160.cast$Percent_R0
                           ,"SE.R0"           = item160.cast$SE_R0
                           # ,"Count.R0"        = item160.cast$Count_R0
                           ,"Percent.R1.R10"  = item160.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item160.cast$SE_R1.R10
                           # ,"Count.R1.R10"    = item160.cast$Count_R1.R10
                           ,"Percent.R11.R16" = item160.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item160.cast$SE_R11.R16
                           ,"Count.R11.R16"   = item160.cast$Count_R11.R16
                           ,"Percent.R17.R22" = item160.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item160.cast$SE_R17.R22
                           # ,"Count.R17.R22"   = item160.cast$Count_R17.R22
                           ,"Percent.RGT22"   = item160.cast$Percent_RGT22
                           ,"SE.RGT22"        = item160.cast$SE_RGT22
                           # ,"Count.RGT22"     = item160.cast$Count_RGT22
                           ,"Percent_All Insulation Levels" = item160.cast$`Percent_All Insulation Levels`
                           ,"SE.All Insulation Levels"      = item160.cast$`SE_All Insulation Levels`
                           # ,"Count.All Insulation Levels"   = item160.cast$`Count_All Insulation Levels`
                           ,"n"                    = item160.cast$`n_All Insulation Levels`
)

# row ordering example code
unique(item160.table$Wall.Type)
rowOrder <- c("Framed 2x2"
              ,"Framed 2x3"
              ,"Framed 2x4"
              ,"Framed 2x6"
              ,"Framed 2x8"
              ,"Framed (Unknown)"
              ,"Alternative"
              ,"All Frame Types")
item160.table <- item160.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)  
item160.table <- data.frame(item160.table)

item160.table.SF <- item160.table[which(item160.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item160.table.SF, "SF", "Table B-5", weighted = FALSE)

