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
rbsa.dat.orig <- read.xlsx(xlsxFile = file.path(filepathCleanData ,paste("clean.pse.data", rundate, ".xlsx", sep = "")))
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
# Item 235A: DISTRIBUTION OF WALL INSULATION BY WALL TYPE  (MF table 27)
#############################################################################################
#weight the u factor per home -- where weights are the wall area within home
item235A.prep <- prep.dat5[which(prep.dat5$Category == "PSE"),]
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
item235A.dat <- prep.dat7[which(!is.na(prep.dat7$CK_Cadmus_ID)),]
item235A.dat$Wall.Type[grep("framed",item235A.dat$Wall.Type,ignore.case = T)] <- "Frame"
item235A.dat$Wall.Type[grep("masonry|concrete",item235A.dat$Wall.Type,ignore.case = T)] <- "Masonry/Concrete"
item235A.dat$Wall.Type[which(item235A.dat$Wall.Type %notin% c("Frame","Masonry/Concrete"))] <- "Other"


#Bin R values -- MF only
item235A.dat$rvalue.bins <- "Unknown"
item235A.dat$rvalue.bins[which(item235A.dat$aveRval >=  0  & item235A.dat$aveRval < 8)]  <- "R0.R7"
item235A.dat$rvalue.bins[which(item235A.dat$aveRval >= 8   & item235A.dat$aveRval < 14)]  <- "R8.R13"
item235A.dat$rvalue.bins[which(item235A.dat$aveRval >= 14  & item235A.dat$aveRval < 21)]  <- "R14.R20"
item235A.dat$rvalue.bins[which(item235A.dat$aveRval >= 21  & item235A.dat$aveRval < 24)]  <- "R21.R23"
item235A.dat$rvalue.bins[which(item235A.dat$aveRval >= 24)] <- "RGT23"
unique(item235A.dat$rvalue.bins)

item235A.dat$count <- 1

item235A.dat1 <- item235A.dat[which(item235A.dat$rvalue.bins != "Unknown"),]
colnames(item235A.dat1)

item235A.merge <- left_join(rbsa.dat.MF, item235A.dat1)
item235A.merge <- item235A.merge[which(!is.na(item235A.merge$count)),]

item235A.data <- weightedData(unique(item235A.merge[-which(colnames(item235A.merge) %in% c("CK_SiteID"
                                                                                        ,"Wall.Type"
                                                                                        ,"aveUval"
                                                                                        ,"aveRval"
                                                                                        ,"rvalue.bins"
                                                                                        ,"count"
                                                                                        ,"Category"))]))
item235A.data <- left_join(item235A.data, unique(item235A.merge[which(colnames(item235A.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"CK_SiteID"
                                                                                           ,"Wall.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"rvalue.bins"
                                                                                           ,"count"
                                                                                           ,"Category"))]))



################################
# Weighted Analysis
################################
item235A.summary <- proportionRowsAndColumns1(CustomerLevelData     = item235A.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'Wall.Type'
                                            , rowVariable         = 'rvalue.bins'
                                            , aggregateColumnName = "All Types"
)
item235A.summary <- item235A.summary[which(item235A.summary$Wall.Type != "All Types"),]

## Summary only for "All Frame Types"
item235A.all.frame.types <- proportions_one_group(CustomerLevelData = item235A.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Types"
                                                ,columnName       = "Wall.Type"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

#merge together!
item235A.final <- rbind.data.frame(item235A.summary
                                 , item235A.all.frame.types, stringsAsFactors = F)


##cast data
item235A.cast <- dcast(setDT(item235A.final),
                     formula   = BuildingType + Wall.Type ~ rvalue.bins,
                     value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
names(item235A.cast)

#join all insulation levels onto rvalue summary
item235A.table <- data.frame("BuildingType"                  = item235A.cast$BuildingType
                           ,"Wall.Type"                     = item235A.cast$Wall.Type
                           ,"Percent.R0.R7"                 = item235A.cast$w.percent_R0.R7
                           ,"SE.R0.R7"                      = item235A.cast$w.SE_R0.R7
                           ,"Percent.R8.R13"                = item235A.cast$w.percent_R8.R13
                           ,"SE.R8.R13"                     = item235A.cast$w.SE_R8.R13
                           ,"Percent.R14.R20"               = item235A.cast$w.percent_R14.R20
                           ,"SE.R14.R20"                    = item235A.cast$w.SE_R14.R20
                           ,"Percent.R21.R23"               = NA#item235A.cast$w.percent_R21.R23
                           ,"SE.R21.R23"                    = NA#item235A.cast$w.SE_R21.R23
                           ,"Percent.RGT23"                 = item235A.cast$w.percent_RGT23
                           ,"SE.RGT23"                      = item235A.cast$w.SE_RGT23
                           ,"n"                             = item235A.cast$n_Total
                           ,"EB.R0.R7"                      = item235A.cast$EB_R0.R7
                           ,"EB.R8.R13"                     = item235A.cast$EB_R8.R13
                           ,"EB.R14.R20"                    = item235A.cast$EB_R14.R20
                           ,"EB.R21.R23"                    = NA#item235A.cast$EB_R21.R23
                           ,"EB.RGT23"                      = item235A.cast$EB_RGT23
)

# row ordering example code
unique(item235A.table$Wall.Type)
rowOrder <- c("Frame"
              ,"Masonry/Concrete"
              ,"Other"
              ,"All Types")
item235A.table <- item235A.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)
item235A.table <- data.frame(item235A.table)


item235A.table.MF <- item235A.table[which(item235A.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item235A.table.MF, "MF", "Table 27A", weighted = TRUE,OS = T, osIndicator = "PSE")



################################
# Unweighted Analysis
################################
item235A.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item235A.data
                                                     , valueVariable       = 'count'
                                                     , columnVariable      = 'Wall.Type'
                                                     , rowVariable         = 'rvalue.bins'
                                                     , aggregateColumnName = "All Types"
)
item235A.summary <- item235A.summary[which(item235A.summary$Wall.Type != "All Types"),]

## Summary only for "All Frame Types"
item235A.all.frame.types <- proportions_one_group(CustomerLevelData = item235A.data
                                                 ,valueVariable    = "count"
                                                 ,groupingVariable = "rvalue.bins"
                                                 ,total.name       = "All Types"
                                                 ,columnName       = "Wall.Type"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE
)

#merge together!
item235A.final <- rbind.data.frame(item235A.summary
                                  , item235A.all.frame.types, stringsAsFactors = F)


##cast data
item235A.cast <- dcast(setDT(item235A.final),
                      formula   = BuildingType + Wall.Type ~ rvalue.bins,
                      value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item235A.table <- data.frame("BuildingType"                  = item235A.cast$BuildingType
                            ,"Wall.Type"                     = item235A.cast$Wall.Type
                            ,"Percent.R0.R7"                 = item235A.cast$Percent_R0.R7
                            ,"SE.R0.R7"                      = item235A.cast$SE_R0.R7
                            ,"Percent.R8.R13"                = item235A.cast$Percent_R8.R13
                            ,"SE.R8.R13"                     = item235A.cast$SE_R8.R13
                            ,"Percent.R14.R20"               = item235A.cast$Percent_R14.R20
                            ,"SE.R14.R20"                    = item235A.cast$SE_R14.R20
                            ,"Percent.R21.R23"               = NA#item235A.cast$Percent_R21.R23
                            ,"SE.R21.R23"                    = NA#item235A.cast$SE_R21.R23
                            ,"Percent.RGT23"                 = item235A.cast$Percent_RGT23
                            ,"SE.RGT23"                      = item235A.cast$SE_RGT23
                            ,"n"                             = item235A.cast$n_Total
)

# row ordering example code
unique(item235A.table$Wall.Type)
rowOrder <- c("Frame"
              ,"Masonry/Concrete"
              ,"Other"
              ,"All Types")
item235A.table <- item235A.table %>% mutate(Wall.Type = factor(Wall.Type, levels = rowOrder)) %>% arrange(Wall.Type)
item235A.table <- data.frame(item235A.table)


item235A.table.MF <- item235A.table[which(item235A.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item235A.table.MF, "MF", "Table 27A", weighted = FALSE,OS = T, osIndicator = "PSE")





#############################################################################################
# Item 235B: DISTRIBUTION OF WALL INSULATION BY WALL TYPE  (MF table 27)
#############################################################################################
#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_SiteID,Category, Wall.Type)
                       ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
)
sort(unique(weightedU$aveUval))


#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
unique(weightedU$aveRval)
unique(weightedU$aveUval)

# get unique cadmus IDs and building types for this subset of data
wall.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_SiteID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.dat6 <- data.frame(left_join(weightedU, wall.unique, by = "CK_SiteID"),stringsAsFactors = F)
names(prep.dat6)[which(names(prep.dat6) == "Category")] <- "Wall.Category"
#merge weighted u values onto cleaned RBSA data
prep.dat7 <- left_join(prep.dat6, rbsa.dat.MF, by = c("CK_SiteID" = "CK_Building_ID"))
prep.dat7 <- prep.dat7[grep("3 or fewer floors", prep.dat7$BuildingTypeXX),]
item235B.dat <- prep.dat7[which(!is.na(prep.dat7$CK_Cadmus_ID)),]
item235B.dat$Wall.Type[grep("framed",item235B.dat$Wall.Type,ignore.case = T)] <- "Frame"
item235B.dat$Wall.Type[grep("masonry|concrete",item235B.dat$Wall.Type,ignore.case = T)] <- "Masonry/Concrete"
item235B.dat$Wall.Type[which(item235B.dat$Wall.Type %notin% c("Frame","Masonry/Concrete"))] <- "Other"


#Bin R values -- MF only
item235B.dat$rvalue.bins <- "Unknown"
item235B.dat$rvalue.bins[which(item235B.dat$aveRval >=  0  & item235B.dat$aveRval < 8)]  <- "R0.R7"
item235B.dat$rvalue.bins[which(item235B.dat$aveRval >= 8   & item235B.dat$aveRval < 14)]  <- "R8.R13"
item235B.dat$rvalue.bins[which(item235B.dat$aveRval >= 14  & item235B.dat$aveRval < 21)]  <- "R14.R20"
item235B.dat$rvalue.bins[which(item235B.dat$aveRval >= 21  & item235B.dat$aveRval < 24)]  <- "R21.R23"
item235B.dat$rvalue.bins[which(item235B.dat$aveRval >= 24)] <- "RGT23"
unique(item235B.dat$rvalue.bins)

item235B.dat$count <- 1

item235B.dat1 <- item235B.dat[which(item235B.dat$rvalue.bins != "Unknown"),]
colnames(item235B.dat1)

item235B.merge <- left_join(rbsa.dat.MF, item235B.dat1)
item235B.merge <- item235B.merge[which(!is.na(item235B.merge$count)),]

item235B.data <- weightedData(unique(item235B.merge[-which(colnames(item235B.merge) %in% c("CK_SiteID"
                                                                                           ,"Wall.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"rvalue.bins"
                                                                                           ,"count"
                                                                                           ,"Wall.Category"
                                                                                           ,"Category"))]))
item235B.data <- left_join(item235B.data, unique(item235B.merge[which(colnames(item235B.merge) %in% c("CK_Cadmus_ID"
                                                                                                      ,"CK_SiteID"
                                                                                                      ,"Wall.Type"
                                                                                                      ,"aveUval"
                                                                                                      ,"aveRval"
                                                                                                      ,"rvalue.bins"
                                                                                                      ,"count"
                                                                                                      ,"Wall.Category"
                                                                                                      ,"Category"))]))



################################
# Weighted Analysis
################################
item235B.summary <- proportionRowsAndColumns1(CustomerLevelData     = item235B.data
                                              , valueVariable       = 'count'
                                              , columnVariable      = 'Category'
                                              , rowVariable         = 'rvalue.bins'
                                              , aggregateColumnName = "Remove"
)
item235B.summary <- item235B.summary[which(item235B.summary$Category != "Remove"),]

##cast data
item235B.cast <- dcast(setDT(item235B.summary),
                       formula   = BuildingType + rvalue.bins ~ Category,
                       value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

#join all insulation levels onto rvalue summary
item235B.table <- data.frame("Insulation.Level"                  = item235B.cast$rvalue.bins
                             ,"PSE.Percent"                 = item235B.cast$w.percent_PSE
                             ,"PSE.SE"                      = item235B.cast$w.SE_PSE
                             ,"PSE.n"                       = item235B.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item235B.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item235B.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item235B.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item235B.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item235B.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item235B.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item235B.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item235B.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"              = item235B.cast$`n_2017 RBSA PS`
                             ,"PSE.EB"                      = item235B.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item235B.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item235B.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item235B.cast$`EB_2017 RBSA PS`
)

# row ordering example code
unique(item235B.table$Insulation.Level)
rowOrder <- c("R0.R7"
              ,"R8.R13"
              ,"R14.R20"
              ,"RGT23"
              ,"Total")
item235B.table <- item235B.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)
item235B.table <- data.frame(item235B.table)


# item235B.table.MF <- item235B.table[which(item235B.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item235B.table, "MF", "Table 27B", weighted = TRUE,OS = T, osIndicator = "PSE")



################################
# Unweighted Analysis
################################
item235B.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item235B.data
                                              , valueVariable       = 'count'
                                              , columnVariable      = 'Category'
                                              , rowVariable         = 'rvalue.bins'
                                              , aggregateColumnName = "Remove"
)
item235B.summary <- item235B.summary[which(item235B.summary$Category != "Remove"),]

##cast data
item235B.cast <- dcast(setDT(item235B.summary),
                       formula   = BuildingType + rvalue.bins ~ Category,
                       value.var = c("Percent", "SE","n"))

#join all insulation levels onto rvalue summary
item235B.table <- data.frame("Insulation.Level"                  = item235B.cast$rvalue.bins
                             ,"PSE.Percent"                 = item235B.cast$Percent_PSE
                             ,"PSE.SE"                      = item235B.cast$SE_PSE
                             ,"PSE.n"                       = item235B.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item235B.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item235B.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item235B.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item235B.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item235B.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item235B.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item235B.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item235B.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"              = item235B.cast$`n_2017 RBSA PS`
)

# row ordering example code
unique(item235B.table$Insulation.Level)
rowOrder <- c("R0.R7"
              ,"R8.R13"
              ,"R14.R20"
              ,"RGT23"
              ,"Total")
item235B.table <- item235B.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)
item235B.table <- data.frame(item235B.table)

#export table to correct workbook using exporting function
exportTable(item235B.table, "MF", "Table 27B", weighted = FALSE,OS = T, osIndicator = "PSE")

