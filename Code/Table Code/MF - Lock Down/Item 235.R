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
envelope.dat <- read.xlsx(envelope.export)
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

prep.dat0 <- prep.dat0[which(!(prep.dat0$`Furred.Wall.Insulated?` %in% c("Unknown","N/A") &prep.dat0$`Wall.Cavity.Insulated?` %in% c("Unknown","N/A") &prep.dat0$`Wall.Exterior.Insulated?` %in% c("Unknown","N/A"))),]
prep.dat0 <- prep.dat0[which(!(is.na(prep.dat0$Furred.Wall.Insulation.Thickness) & is.na(prep.dat0$Wall.Cavity.Insulation.Thickness.1) & is.na(prep.dat0$Wall.Exterior.Insulation.Thickness.1))),]


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
prep.dat7 <- prep.dat7[grep("3 or fewer floors", prep.dat7$BuildingTypeXX),]
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
item235.data <- left_join(item235.data, unique(item235.merge[which(colnames(item235.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"CK_SiteID"
                                                                                           ,"Wall.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"rvalue.bins"
                                                                                           ,"count"))]))



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
              ,"Masonry/Concrete"
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
              ,"Masonry/Concrete"
              ,"Other"
              ,"All Types")
item235.table <- item235.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)
item235.table <- data.frame(item235.table)


item235.table.MF <- item235.table[which(item235.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item235.table.MF, "MF", "Table 27", weighted = FALSE)

