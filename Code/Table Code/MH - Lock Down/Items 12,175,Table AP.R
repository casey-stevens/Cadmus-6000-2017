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
rbsa.dat.orig <- read.xlsx(xlsxFile = file.path(filepathCleanData ,paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat <- rbsa.dat.orig[grep("site",rbsa.dat.orig$CK_Building_ID, ignore.case = T),]
rbsa.dat.MF <- rbsa.dat.orig[grep("bldg", rbsa.dat.orig$CK_Building_ID, ignore.case = T),]


#Read in data for analysis
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Envelope.xlsx', envelope.export, mode = 'wb')
# envelope.dat <- read.xlsx(envelope.export)
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]
rvals$Type.of.Insulation <- trimws(tolower(rvals$Type.of.Insulation))
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
prep.dat0$Furred.Wall.Insulation.Thickness <- as.numeric(prep.dat0$Furred.Wall.Insulation.Thickness)
prep.dat0$Wall.Cavity.Insulation.Thickness.1 <- as.numeric(prep.dat0$Wall.Cavity.Insulation.Thickness.1)
prep.dat0$Wall.Exterior.Insulation.Thickness.1 <- as.numeric(prep.dat0$Wall.Exterior.Insulation.Thickness.1)

# prep.dat0 <- prep.dat0[which(!(prep.dat0$`Furred.Wall.Insulated?` %in% c("Unknown","N/A") &prep.dat0$`Wall.Cavity.Insulated?` %in% c("Unknown","N/A") &prep.dat0$`Wall.Exterior.Insulated?` %in% c("Unknown","N/A"))),]
# prep.dat0 <- prep.dat0[which(!(is.na(prep.dat0$Furred.Wall.Insulation.Thickness) & is.na(prep.dat0$Wall.Cavity.Insulation.Thickness.1) & is.na(prep.dat0$Wall.Exterior.Insulation.Thickness.1))),]


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

# replace Unknown or NA in Condition columns with 1 (or 100%)
for (i in grep("insulation.type", names(prep.dat3), ignore.case = T)){
  prep.dat3[,i] <- trimws(tolower(prep.dat3[,i]))
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
  prep.dat4[,i] <- ifelse(prep.dat4[,i] %in% c("N/A", NA), 0, prep.dat4[,i])
}

#fix names that are not in R value table
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "fiberglass or mineral wool batts")] <- "mineral wool batts"
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "unknown fiberglass")]               <- "unknown"
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "datapoint not asked for")]          <- NA
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "none")]                             <- NA
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 %in% c("n/a","na"))]                    <- NA
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "expanded polystyrene foam board (white)")] <- "expanded polystyrene foam board"
prep.dat4$cavity.rvalues1[grep('unknown', prep.dat4$cavity.rvalues1, ignore.case = T)] <- "unknown"
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "extruded polystyrene foam board (pink or blue)")] <- "extruded polystyrene foam board"

prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "extruded polystyrene (blue)")]      <- "extruded polystyrene foam board"
prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 %in% c("n/a","na"))]                    <- NA
prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "datapoint not asked for")]    <- NA
prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "expanded polystyrene foam board (white)")] <- "expanded polystyrene foam board"
prep.dat4$cavity.rvalues2[grep('unknown', prep.dat4$cavity.rvalues2, ignore.case = T)] <- "unknown"

prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "datapoint not asked for")]    <- NA
prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "extruded polystyrene foam board (pink or blue)")] <- "extruded polystyrene foam board"
prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "expanded polystyrene foam board (white)")] <- "expanded polystyrene foam board"
prep.dat4$exterior.rvalues1[grep('unknown', prep.dat4$exterior.rvalues1, ignore.case = T)] <- "unknown"

#for ICF walls
prep.dat4$cavity.rvalues1[which(prep.dat4$Wall.Type == "ICF")] <- "expanded polystyrene foam board"
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

# prep.dat4.8 <- prep.dat4.5
prep.dat4.8 <- rbind.data.frame(prep.dat4.5
                                ,prep.condition.sub1
                                ,prep.condition.sub2
                                , stringsAsFactors = F)
prep.dat4.8$Wall.Cavity.Insulation.Condition.1[which(is.na(prep.dat4.8$Wall.Cavity.Insulation.Condition.1))] <- 1
prep.dat4.9 <- prep.dat4.8[which(!is.na(prep.dat4.8$Wall.Type)),]
names(prep.dat4.9)[which(names(prep.dat4.9) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"
# prep.dat4.9 <- prep.dat4.9[which(prep.dat4.9$CK_Cadmus_ID != "BUILDING"),]
sort(unique(prep.dat4.9$cavity.rvalues1))
sort(unique(prep.dat4.9$exterior.rvalues1))

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
sort(unique(prep.dat4.9$total.r.val))
# prep.dat4.9$total.r.val[which(prep.dat4.9$Wall.Type == "ICF")]
#caluclate u factors = inverse of Rvalue
prep.dat4.9$uvalue <- 1 / (prep.dat4.9$total.r.val)
prep.dat4.9$uvalue[which(prep.dat4.9$uvalue == "Inf")] <- 1
sort(unique(prep.dat4.9$uvalue))

#make area numeric
prep.dat4.9$uvalue    <- as.numeric(as.character(prep.dat4.9$uvalue))
prep.dat4.9$Wall.Area <- as.numeric(as.character(prep.dat4.9$Wall.Area))
prep.dat5 <- prep.dat4.9[which(prep.dat4.9$Wall.Type %notin% c("Adiabatic", "Knee Wall")),]

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID)
                       ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)
sort(unique(weightedU$aveUval))


#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
sort(unique(weightedU$aveRval))
sort(unique(weightedU$aveUval))

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

# rbsa.wall <- rbsa.dat.orig[which(colnames(rbsa.dat.orig) %in% c("CK_Building_ID","BuildingType","HomeYearBuilt"))]
# wall.merge <- data.frame(left_join(rbsa.wall, prep.dat4.9, by = c("CK_Building_ID" = "CK_SiteID")),stringsAsFactors = F)
# wall.merge <- wall.merge[which(!is.na(wall.merge$uvalue)),]
#########export rvalues
##  Write out confidence/precision info
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(wall.merge, paste(filepathCleaningDocs, "Insulation Exports", paste("Wall Insulation Values ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)


rbsa.wall <- rbsa.dat.orig[which(colnames(rbsa.dat) %in% c("CK_Building_ID","BuildingType","HomeYearBuilt"))]
wall.merge <- data.frame(left_join(rbsa.wall, prep.dat4.9, by = c("CK_Building_ID" = "CK_SiteID")),stringsAsFactors = F)
wall.merge <- wall.merge[which(!is.na(wall.merge$uvalue)),]
#########export rvalues
##  Write out confidence/precision info
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(wall.merge, paste(filepathCleaningDocs, "Insulation Exports", paste("Wall Insulation Values ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)













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
