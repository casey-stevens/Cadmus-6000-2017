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

###################################################################################################################
#
#
# This will be used for items 10, 12, 13, 14, 15, and 16
#
#
###################################################################################################################
#subset envelope data to necessary columns
prep.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
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
                                                               , "Wall.Exterior.Insulation.Condition.3"))]
prep.dat0 <- prep.dat[which(prep.dat$`Wall.Cavity.Insulated?` %in% c("Yes", "No", "-- Datapoint not asked for --")),]
prep.dat0$`Wall.Exterior.Insulated?`[which(prep.dat0$`Wall.Exterior.Insulated?` != "Yes" & prep.dat0$Wall.Type %notin% c("Masonry", "Masonry (Basement)"))] <- "No" ###treat anything not Yes as No
prep.dat0.1 <- prep.dat0[which(!(is.na(prep.dat0$Wall.Area))),]
prep.dat1.0 <- prep.dat0.1[which(prep.dat0.1$Wall.Area != "Unknown"),]
prep.dat1.1 <- prep.dat1.0[which(prep.dat1.0$Wall.Cavity.Insulation.Thickness.1 != "Unknown"),]
prep.dat1.2 <- prep.dat1.1[-which(prep.dat1.1$Wall.Exterior.Insulation.Thickness.1 == "Unknown"),]

#review types
unique(prep.dat1.2$Wall.Cavity.Insulation.Type.1)
unique(prep.dat1.2$Wall.Cavity.Insulation.Type.2)
unique(prep.dat1.2$Wall.Cavity.Insulation.Type.3) #nothing in this column
unique(prep.dat1.2$Wall.Exterior.Insulation.Type.1)
unique(prep.dat1.2$Wall.Exterior.Insulation.Type.2) #nothing in this column
unique(prep.dat1.2$Wall.Exterior.Insulation.Type.3) #nothing in this column

#review insulation thicknesses
unique(prep.dat1.2$Wall.Cavity.Insulation.Thickness.1)
unique(prep.dat1.2$Wall.Cavity.Insulation.Thickness.2)
unique(prep.dat1.2$Wall.Cavity.Insulation.Thickness.3)
unique(prep.dat1.2$Wall.Exterior.Insulation.Thickness.1)
unique(prep.dat1.2$Wall.Exterior.Insulation.Thickness.2)
unique(prep.dat1.2$Wall.Exterior.Insulation.Thickness.3)

#review conditions
unique(prep.dat1.2$Wall.Cavity.Insulation.Condition.1)
unique(prep.dat1.2$Wall.Cavity.Insulation.Condition.2)
unique(prep.dat1.2$Wall.Cavity.Insulation.Condition.3)
unique(prep.dat1.2$Wall.Exterior.Insulation.Condition.1)
unique(prep.dat1.2$Wall.Exterior.Insulation.Condition.2)
unique(prep.dat1.2$Wall.Exterior.Insulation.Condition.3)


#remove unneccesary wall types
prep.dat2 <- prep.dat1.2[which(prep.dat1.2$Wall.Type %notin% c("Masonry","Masonry (Basement)","Log","Adiabatic", "Knee Wall", "Other", "ICF", "SIP")),]
unique(prep.dat2$Wall.Type)

#create "Alternative" category
prep.dat2$Wall.Type[grep("alternative",prep.dat2$Wall.Type,ignore.case = T)] <- "Alternative"
length(unique(prep.dat2$CK_Cadmus_ID))
unique(prep.dat2$Wall.Type)

#assign new dataset
prep.dat3 <- prep.dat2

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################

for(i in 1:ncol(prep.dat3)){
  prep.dat3[,i] <- ifelse(prep.dat3[,i] == "-- Datapoint not asked for --", NA, prep.dat3[,i])
}

#cleaning for wall.cavity
prep.dat3$Wall.Cavity.Insulation.Thickness.1[which(prep.dat3$Wall.Cavity.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
prep.dat3$Wall.Cavity.Insulation.Thickness.1[which(is.na(prep.dat3$Wall.Cavity.Insulation.Thickness.1))] <- "N/A N/A"
prep.dat3$Wall.Cavity.Insulation.Thickness.2[which(prep.dat3$Wall.Cavity.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Wall.Cavity.Insulation.Thickness.2[which(prep.dat3$Wall.Cavity.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
prep.dat3$Wall.Cavity.Insulation.Thickness.2[which(is.na(prep.dat3$Wall.Cavity.Insulation.Thickness.2))] <- "N/A N/A"
prep.dat3$Wall.Cavity.Insulation.Thickness.3[which(prep.dat3$Wall.Cavity.Insulation.Thickness.3 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Wall.Cavity.Insulation.Thickness.3[which(prep.dat3$Wall.Cavity.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
prep.dat3$Wall.Cavity.Insulation.Thickness.3[which(is.na(prep.dat3$Wall.Cavity.Insulation.Thickness.3))] <- "N/A N/A"
unique(prep.dat3$Wall.Cavity.Insulation.Thickness.1)
unique(prep.dat3$Wall.Cavity.Insulation.Thickness.2)
unique(prep.dat3$Wall.Cavity.Insulation.Thickness.3)

#cleaning for wall exterior
prep.dat3$Wall.Exterior.Insulation.Thickness.1[which(prep.dat3$Wall.Exterior.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.1[which(is.na(prep.dat3$Wall.Exterior.Insulation.Thickness.1))] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.1[which(prep.dat3$Wall.Exterior.Insulation.Thickness.1 == "Unknown")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.2[which(prep.dat3$Wall.Exterior.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.2[which(is.na(prep.dat3$Wall.Exterior.Insulation.Thickness.2))] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.2[which(prep.dat3$Wall.Exterior.Insulation.Thickness.2 == "Unknown")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.3[which(prep.dat3$Wall.Exterior.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.3[which(is.na(prep.dat3$Wall.Exterior.Insulation.Thickness.3))] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.1[which(prep.dat3$Wall.Exterior.Insulation.Thickness.1 == "1.5")] <- "1.5 inches"
prep.dat3$Wall.Exterior.Insulation.Thickness.3[which(prep.dat3$Wall.Exterior.Insulation.Thickness.3 == "Unknown")] <- "N/A N/A"
unique(prep.dat3$Wall.Exterior.Insulation.Thickness.1)
unique(prep.dat3$Wall.Exterior.Insulation.Thickness.2)
unique(prep.dat3$Wall.Exterior.Insulation.Thickness.3)


#Clean Condition unknown values
prep.dat3$Wall.Cavity.Insulation.Condition.1[which(prep.dat3$Wall.Cavity.Insulation.Condition.1 == "Unknown")] <- "100%"
prep.dat3$Wall.Cavity.Insulation.Condition.2[which(prep.dat3$Wall.Cavity.Insulation.Condition.2 == "Unknown")] <- "100%"
prep.dat3$Wall.Cavity.Insulation.Condition.3[which(prep.dat3$Wall.Cavity.Insulation.Condition.3 == "Unknown")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.1[which(prep.dat3$Wall.Exterior.Insulation.Condition.1 == "Unknown")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.2[which(prep.dat3$Wall.Exterior.Insulation.Condition.2 == "Unknown")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.3[which(prep.dat3$Wall.Exterior.Insulation.Condition.3 == "Unknown")] <- "100%"

prep.dat3$Wall.Cavity.Insulation.Condition.1[which(is.na(prep.dat3$Wall.Cavity.Insulation.Condition.1) & prep.dat3$Wall.Cavity.Insulation.Thickness.1 != "N/A N/A")] <- "100%"
prep.dat3$Wall.Cavity.Insulation.Condition.2[which(is.na(prep.dat3$Wall.Cavity.Insulation.Condition.2) & prep.dat3$Wall.Cavity.Insulation.Thickness.2 != "N/A N/A")] <- "100%"
prep.dat3$Wall.Cavity.Insulation.Condition.3[which(is.na(prep.dat3$Wall.Cavity.Insulation.Condition.3) & prep.dat3$Wall.Cavity.Insulation.Thickness.3 != "N/A N/A")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.1[which(is.na(prep.dat3$Wall.Exterior.Insulation.Condition.1) & prep.dat3$Wall.Exterior.Insulation.Thickness.1 != "N/A N/A")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.2[which(is.na(prep.dat3$Wall.Exterior.Insulation.Condition.2) & prep.dat3$Wall.Exterior.Insulation.Thickness.2 != "N/A N/A")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.3[which(is.na(prep.dat3$Wall.Exterior.Insulation.Condition.3) & prep.dat3$Wall.Exterior.Insulation.Thickness.3 != "N/A N/A")] <- "100%"

prep.dat3$Wall.Cavity.Insulation.Condition.1[which(prep.dat3$`Wall.Cavity.Insulated?` == "No")] <- "0%"
prep.dat3$Wall.Exterior.Insulation.Condition.1[which(prep.dat3$`Wall.Exterior.Insulated?` == "No")] <- "0%"


#remove percent signs and make numeric
prep.dat3$Wall.Cavity.Insulation.Condition.1 <- gsub("%", "", prep.dat3$Wall.Cavity.Insulation.Condition.1)
prep.dat3$Wall.Cavity.Insulation.Condition.1 <- as.numeric(as.character(prep.dat3$Wall.Cavity.Insulation.Condition.1))
prep.dat3$Wall.Cavity.Insulation.Condition.2 <- gsub("%", "", prep.dat3$Wall.Cavity.Insulation.Condition.2)
prep.dat3$Wall.Cavity.Insulation.Condition.2 <- as.numeric(as.character(prep.dat3$Wall.Cavity.Insulation.Condition.2))
prep.dat3$Wall.Cavity.Insulation.Condition.3 <- gsub("%", "", prep.dat3$Wall.Cavity.Insulation.Condition.3)
prep.dat3$Wall.Cavity.Insulation.Condition.3 <- as.numeric(as.character(prep.dat3$Wall.Cavity.Insulation.Condition.3))

prep.dat3$Wall.Exterior.Insulation.Condition.1 <- gsub("%", "", prep.dat3$Wall.Exterior.Insulation.Condition.1)
prep.dat3$Wall.Exterior.Insulation.Condition.1 <- as.numeric(as.character(prep.dat3$Wall.Exterior.Insulation.Condition.1))
prep.dat3$Wall.Exterior.Insulation.Condition.2 <- gsub("%", "", prep.dat3$Wall.Exterior.Insulation.Condition.2)
prep.dat3$Wall.Exterior.Insulation.Condition.2 <- as.numeric(as.character(prep.dat3$Wall.Exterior.Insulation.Condition.2))
prep.dat3$Wall.Exterior.Insulation.Condition.3 <- gsub("%", "", prep.dat3$Wall.Exterior.Insulation.Condition.3)
prep.dat3$Wall.Exterior.Insulation.Condition.3 <- as.numeric(as.character(prep.dat3$Wall.Exterior.Insulation.Condition.3))


# add new ID variable for merging -- don't know if we need this
prep.dat3$count <- 1
prep.dat3$TMP_ID <- cumsum(prep.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(prep.dat3$Wall.Cavity.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(prep.dat3$Wall.Cavity.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul3 <- unlist(strsplit(prep.dat3$Wall.Cavity.Insulation.Thickness.3, " "))
clean.insul3.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul3, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul3.1)

clean.insul1.0 <- unlist(strsplit(prep.dat3$Wall.Exterior.Insulation.Thickness.1, " "))
clean.insul1.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul1.0, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul1.2)

clean.insul2.0 <- unlist(strsplit(prep.dat3$Wall.Exterior.Insulation.Thickness.2, " "))
clean.insul2.00 <- as.data.frame(matrix(clean.insul2.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul2.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul2.00)
dim(clean.insul2.2)

clean.insul3.0 <- unlist(strsplit(prep.dat3$Wall.Exterior.Insulation.Thickness.3, " "))
clean.insul3.00 <- as.data.frame(matrix(clean.insul3.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul3.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul3.00)
dim(clean.insul3.2)

clean.insul.join1 <- left_join(clean.insul1.1,    clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join2 <- left_join(clean.insul.join1, clean.insul3.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join3 <- left_join(clean.insul.join2, clean.insul1.2, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join4 <- left_join(clean.insul.join3, clean.insul2.2, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join5 <- left_join(clean.insul.join4, clean.insul3.2, by = c("CK_Cadmus_ID", "TMP_ID"))

colnames(clean.insul.join5) <- c("CK_Cadmus_ID"
                                 ,"TMP_ID"
                                 ,"cavity.inches1"
                                 ,"Remove.1"
                                 ,"cavity.inches2"
                                 ,"Remove.2"
                                 ,"cavity.inches3"
                                 ,"Remove.3"
                                 ,"exterior.inches1"
                                 ,"Remove.1"
                                 ,"exterior.inches2"
                                 ,"Remove.2"
                                 ,"exterior.inches3"
                                 ,"Remove.3")

clean.thickness.data <- clean.insul.join5[-grep("Remove", colnames(clean.insul.join5))]

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
prep.dat4$cavity.inches1   <- as.numeric(as.character(prep.dat4$cavity.inches1)) # warning here is OK
prep.dat4$cavity.inches2   <- as.numeric(as.character(prep.dat4$cavity.inches2)) # warning here is OK
prep.dat4$cavity.inches3   <- as.numeric(as.character(prep.dat4$cavity.inches3)) # warning here is OK
prep.dat4$exterior.inches1 <- as.numeric(as.character(prep.dat4$exterior.inches1)) # warning here is OK
prep.dat4$exterior.inches2 <- as.numeric(as.character(prep.dat4$exterior.inches2)) # warning here is OK
prep.dat4$exterior.inches3 <- as.numeric(as.character(prep.dat4$exterior.inches3)) # warning here is OK

#replace any inches that are NA with zeros
for(i in grep("inches|rval", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}

#update column names
prep.dat4$cavity.rvalues1 <- prep.dat4$Wall.Cavity.Insulation.Type.1
prep.dat4$cavity.rvalues2 <- prep.dat4$Wall.Cavity.Insulation.Type.2
prep.dat4$cavity.rvalues3 <- prep.dat4$Wall.Cavity.Insulation.Type.3
prep.dat4$exterior.rvalues1 <- prep.dat4$Wall.Exterior.Insulation.Type.1
prep.dat4$exterior.rvalues2 <- prep.dat4$Wall.Exterior.Insulation.Type.2
prep.dat4$exterior.rvalues3 <- prep.dat4$Wall.Exterior.Insulation.Type.3

#fix names that are not in R value table
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "Unknown fiberglass")]               <- "Unknown"
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "-- Datapoint not asked for --")]    <- NA
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "None")]                             <- NA
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "N/A")]                              <- NA
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"
prep.dat4$cavity.rvalues1[grep('unknown', prep.dat4$cavity.rvalues1, ignore.case = T)] <- "Unknown"
prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"

prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "N/A")]                              <- NA
prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "-- Datapoint not asked for --")]    <- NA
prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"
prep.dat4$cavity.rvalues2[grep('unknown', prep.dat4$cavity.rvalues2, ignore.case = T)] <- "Unknown"

prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "-- Datapoint not asked for --")]    <- NA
prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"
prep.dat4$exterior.rvalues1[grep('unknown', prep.dat4$exterior.rvalues1, ignore.case = T)] <- "Unknown"

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  prep.dat4$cavity.rvalues1[which(prep.dat4$cavity.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$cavity.rvalues2[which(prep.dat4$cavity.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$cavity.rvalues3[which(prep.dat4$cavity.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$exterior.rvalues2[which(prep.dat4$exterior.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$exterior.rvalues3[which(prep.dat4$exterior.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
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
prep.dat4$cavity.inches1[which(prep.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
prep.dat4$cavity.inches2[which(prep.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
prep.dat4$cavity.inches3[which(prep.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
prep.dat4$exterior.inches1[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.inches2[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.inches3[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0

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

prep.dat4.5$Wall.Cavity.Insulation.Condition.1   <- prep.dat4.5$Wall.Cavity.Insulation.Condition.1 / 100
prep.dat4.5$Wall.Exterior.Insulation.Condition.1 <- prep.dat4.5$Wall.Exterior.Insulation.Condition.1 / 100


# clean up condition information
prep.condition.sub1 <- prep.dat4.5[which(prep.dat4.5$Wall.Cavity.Insulation.Condition.1 %notin% c(1, NA)),]
prep.condition.sub1$Wall.Cavity.Insulation.Condition.1 <- 1 - prep.condition.sub1$Wall.Cavity.Insulation.Condition.1
prep.condition.sub1$total.r.val <- NA

# clean up condition information
prep.condition.sub2 <- prep.dat4.5[which(prep.dat4.5$Wall.Exterior.Insulation.Condition.1 %notin% c(1, NA)),]
prep.condition.sub2$Wall.Exterior.Insulation.Condition.1 <- 1 - prep.condition.sub2$Wall.Exterior.Insulation.Condition.1
prep.condition.sub2$total.r.val <- NA


prep.dat4.9 <- rbind.data.frame(prep.dat4.5
                              ,prep.condition.sub1
                              ,prep.condition.sub2
                                , stringsAsFactors = F)
prep.dat5 <- prep.dat4.9[which(!is.na(prep.dat4.9$Wall.Type)),]

###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################


#calculate the weighted r value
na.ind <- which(is.na(prep.dat5$total.r.val))
prep.dat5$total.r.val[na.ind] <- (prep.dat5$cavity.rvalues1[na.ind] * prep.dat5$cavity.inches1[na.ind]) +  
  (prep.dat5$cavity.rvalues2[na.ind] * prep.dat5$cavity.inches2[na.ind]) +  
  (prep.dat5$cavity.rvalues3[na.ind] * prep.dat5$cavity.inches3[na.ind]) + 
  (prep.dat5$exterior.rvalues1[na.ind] * prep.dat5$exterior.inches1[na.ind]) + 
  (prep.dat5$exterior.rvalues2[na.ind] * prep.dat5$exterior.inches2[na.ind]) + 
  (prep.dat5$exterior.rvalues3[na.ind] * prep.dat5$exterior.inches3[na.ind])

#check -- NOTE -- NONE SHOULD BE NA
unique(prep.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
prep.dat5$uvalue <- 1 / (1 + prep.dat5$total.r.val)
unique(prep.dat5$uvalue)

#make area numeric
prep.dat5$uvalue    <- as.numeric(as.character(prep.dat5$uvalue))
prep.dat5$Wall.Area <- as.numeric(as.character(prep.dat5$Wall.Area))

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * Wall.Cavity.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Cavity.Insulation.Condition.1)
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



#########export rvalues
##  Write out confidence/precision info
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(prep.dat5, paste(filepathCleaningDocs, "Insulation Exports", paste("all.insulation.values ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)
# write.xlsx(prep.dat7, paste(filepathCleaningDocs, "Insulation Exports", paste("one.line.insulation ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)








#############################################################################################
# Item 10: DISTRIBUTION OF FRAME WALL INSULATION LEVELS BY FRAMING TYPE (SF table 17)
#############################################################################################
item10.dat <- prep.dat7[grep("framed|alternative",prep.dat7$Wall.Type, ignore.case = T),]

#Bin R values -- SF only
item10.dat$rvalue.bins <- "Unknown"
item10.dat$rvalue.bins[which(item10.dat$aveRval ==  0)] <- "R0"
item10.dat$rvalue.bins[which(item10.dat$aveRval >   0  & item10.dat$aveRval < 11)]  <- "R1.R10"
item10.dat$rvalue.bins[which(item10.dat$aveRval >= 11  & item10.dat$aveRval < 17)]  <- "R11.R16"
item10.dat$rvalue.bins[which(item10.dat$aveRval >= 17  & item10.dat$aveRval < 23)]  <- "R17.R22"
item10.dat$rvalue.bins[which(item10.dat$aveRval >= 22)] <- "RGT22"
unique(item10.dat$rvalue.bins)

item10.dat$count <- 1

item10.dat1 <- item10.dat[which(item10.dat$rvalue.bins != "Unknown"),]
colnames(item10.dat1)

item10.merge <- left_join(rbsa.dat, item10.dat1)
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

## Summary only for "All Frame Types"
item10.all.frame.types <- proportions_one_group(CustomerLevelData     = item10.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins"
                                                ,total.name       = "All Frame Types"
                                                ,columnName       = "Wall.Type"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item10.all.insul.levels <-  proportions_one_group(CustomerLevelData     = item10.data
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
item10.final <- item10.final[which(item10.final$rvalue.bins != "Total"),]
item10.final$Wall.Type[which(item10.final$Wall.Type == "Total")] <- "All Frame Types"


##cast data
item10.cast <- dcast(setDT(item10.final),
                     formula   = BuildingType + Wall.Type ~ rvalue.bins,
                     value.var = c("w.percent", "w.SE", "count", "n", "N"))

#join all insulation levels onto rvalue summary
item10.table <- data.frame("BuildingType"     = item10.cast$BuildingType
                           ,"Wall.Type"       = item10.cast$Wall.Type
                           ,"Percent.R0"      = item10.cast$w.percent_R0
                           ,"SE.R0"           = item10.cast$w.SE_R0
                           ,"count.R0"        = item10.cast$count_R0
                           ,"Percent.R1.R10"  = item10.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item10.cast$w.SE_R1.R10
                           ,"count.R1.R10"    = item10.cast$count_R1.R10
                           ,"Percent.R11.R16" = item10.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item10.cast$w.SE_R11.R16
                           ,"count.R11.R16"   = item10.cast$count_R11.R16
                           ,"Percent.R17.R22" = item10.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item10.cast$w.SE_R17.R22
                           ,"count.R17.R22"   = item10.cast$count_R17.R22
                           ,"Percent.RGT22"   = item10.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item10.cast$w.SE_RGT22
                           ,"count.RGT22"     = item10.cast$count_RGT22
                           ,"Percent_All Insulation Levels" = item10.cast$`w.percent_All Insulation Levels`
                           ,"SE.All Insulation Levels"      = item10.cast$`w.SE_All Insulation Levels`
                           ,"count.All Insulation Levels"   = item10.cast$`count_All Insulation Levels`
                           )

item10.table.SF <- item10.table[which(item10.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item10.table.SF, "SF", "Table 17"
            , weighted = TRUE)



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
item10.all.insul.levels <-  proportions_one_group(item10.data
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
                     value.var = c("Percent", "SE", "Count", "SampleSize"))

#join all insulation levels onto rvalue summary
item10.table <- data.frame("BuildingType"     = item10.cast$BuildingType
                           ,"Wall.Type"       = item10.cast$Wall.Type
                           ,"Percent.R0"      = item10.cast$Percent_R0
                           ,"SE.R0"           = item10.cast$SE_R0
                           ,"Count.R0"        = item10.cast$Count_R0
                           ,"Percent.R1.R10"  = item10.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item10.cast$SE_R1.R10
                           ,"Count.R1.R10"    = item10.cast$Count_R1.R10
                           ,"Percent.R11.R16" = item10.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item10.cast$SE_R11.R16
                           ,"Count.R11.R16"   = item10.cast$Count_R11.R16
                           ,"Percent.R17.R22" = item10.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item10.cast$SE_R17.R22
                           ,"Count.R17.R22"   = item10.cast$Count_R17.R22
                           ,"Percent.RGT22"   = item10.cast$Percent_RGT22
                           ,"SE.RGT22"        = item10.cast$SE_RGT22
                           ,"Count.RGT22"     = item10.cast$Count_RGT22
                           ,"Percent_All Insulation Levels" = item10.cast$`Percent_All Insulation Levels`
                           ,"SE.All Insulation Levels"      = item10.cast$`SE_All Insulation Levels`
                           ,"Count.All Insulation Levels"   = item10.cast$`Count_All Insulation Levels`
                           )

item10.table.SF <- item10.table[which(item10.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item10.table.SF, "SF", "Table 17"
            , weighted = FALSE)






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



#############################
# Weighted Analysis
#############################

##### CASEY: What we want for sample size is the total number of sites in each housing bin


item11.by.vinage <- proportionRowsAndColumns1(item11.data
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
                                                , weighted = TRUE
                                                ,two.prop.total = TRUE
                                                )
# row bind summaries
item11.final <- rbind.data.frame(item11.by.vinage, item11.across.vintages, stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item11.final <- item11.final[which(item11.final$HomeYearBuilt_bins3 != "Remove"),]



item11.cast <- dcast(setDT(item11.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ Wall.Type, sum,
                     value.var = c("w.percent", "w.SE", "count","n","N"))



item11.table <- data.frame("BuildingType"     = item11.cast$BuildingType
                           ,"Housing.Vintage" = item11.cast$HomeYearBuilt_bins3
                           # ,"Percent_2x2"     = item11.cast$`w.percent_Framed 2x2`
                           # ,"SE_2x2"          = item11.cast$`w.SE_Framed 2x2`
                           # ,"Count_2x2"       = item11.cast$`count_Framed 2x2`
                           # ,"Percent_2x3"     = item11.cast$`w.percent_Framed 2x3`
                           # ,"SE_2x3"          = item11.cast$`w.SE_Framed 2x3`
                           # ,"Count_2x3"       = item11.cast$`count_Framed 2x3`
                           ,"Percent_2x4"     = item11.cast$`w.percent_Framed 2x4`
                           ,"SE_2x4"          = item11.cast$`w.SE_Framed 2x4`
                           ,"Count_2x4"       = item11.cast$`count_Framed 2x4`
                           ,"Percent_2x6"     = item11.cast$`w.percent_Framed 2x6`
                           ,"SE_2x6"          = item11.cast$`w.SE_Framed 2x6`
                           ,"Count_2x6"       = item11.cast$`count_Framed 2x6`
                           ,"Percent_2x8"     = item11.cast$`w.percent_Framed 2x8`
                           ,"SE_2x8"          = item11.cast$`w.SE_Framed 2x8`
                           ,"Count_2x8"       = item11.cast$`count_Framed 2x8`
                           ,"Percent_ALT"     = item11.cast$w.percent_Alternative
                           ,"SE_ALT"          = item11.cast$w.SE_Alternative
                           ,"Count_ALT"       = item11.cast$count_Alternative
                           ,"Percent_Unknown" = item11.cast$`w.percent_Framed (Unknown)`
                           ,"SE_Unknown"      = item11.cast$`w.SE_Framed (Unknown)`
                           ,"Count_Unknown"   = item11.cast$`count_Framed (Unknown)`
                           )


item11.table.SF            <- item11.table[which(item11.table$BuildingType == "Single Family"),-1]
# item11.table.SF$SampleSize <- item11.table.SF$SampleSize[which(item11.table.SF$Housing.Vintage == "All Housing Vintages")]

#export table to correct workbook using exporting function
exportTable(item11.table.SF, "SF", "Table 18"
            , weighted = TRUE)




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
                     value.var = c("Percent", "SE", "Count","SampleSize"))



item11.table <- data.frame("BuildingType"     = item11.cast$BuildingType
                           ,"Housing.Vintage" = item11.cast$HomeYearBuilt_bins3
                           # ,"Percent_2x2"     = item11.cast$`Percent_Framed 2x2`
                           # ,"SE_2x2"          = item11.cast$`SE_Framed 2x2`
                           # ,"Count_2x2"       = item11.cast$`Count_Framed 2x2`
                           # ,"Percent_2x3"     = item11.cast$`Percent_Framed 2x3`
                           # ,"SE_2x3"          = item11.cast$`SE_Framed 2x3`
                           # ,"Count_2x3"       = item11.cast$`Count_Framed 2x3`
                           ,"Percent_2x4"     = item11.cast$`Percent_Framed 2x4`
                           ,"SE_2x4"          = item11.cast$`SE_Framed 2x4`
                           ,"Count_2x4"       = item11.cast$`Count_Framed 2x4`
                           ,"Percent_2x6"     = item11.cast$`Percent_Framed 2x6`
                           ,"SE_2x6"          = item11.cast$`SE_Framed 2x6`
                           ,"Count_2x6"       = item11.cast$`Count_Framed 2x6`
                           ,"Percent_2x8"     = item11.cast$`Percent_Framed 2x8`
                           ,"SE_2x8"          = item11.cast$`SE_Framed 2x8`
                           ,"Count_2x8"       = item11.cast$`Count_Framed 2x8`
                           ,"Percent_ALT"     = item11.cast$Percent_Alternative
                           ,"SE_ALT"          = item11.cast$SE_Alternative
                           ,"Count_ALT"       = item11.cast$Count_Alternative
                           ,"Percent_Unknown" = item11.cast$`Percent_Framed (Unknown)`
                           ,"SE_Unknown"      = item11.cast$`SE_Framed (Unknown)`
                           ,"Count_Unknown"   = item11.cast$`Count_Framed (Unknown)`
)


item11.table.SF <- item11.table[which(item11.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item11.table.SF, "SF", "Table 18"
            , weighted = FALSE)








#############################################################################################
# Item 12: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE  (SF table 19, MH table 16)
#############################################################################################
## Note: For this table, you must run up to prep.dat7 for the cleaned data
item12.dat <- prep.dat7

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
item12.dat1$rvalue.bins.MH[which(item12.dat1$aveRval >= 0  & item12.dat1$aveRval <  9)]  <- "R0.R8"
item12.dat1$rvalue.bins.MH[which(item12.dat1$aveRval >= 9  & item12.dat1$aveRval < 15)]  <- "R9.R14"
item12.dat1$rvalue.bins.MH[which(item12.dat1$aveRval >= 15 & item12.dat1$aveRval < 22)]  <- "R15.R21"
item12.dat1$rvalue.bins.MH[which(item12.dat1$aveRval >= 22)]  <- "R22.R30"
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

## Summary only for "All Frame Types"
item12.all.frame.types <- proportions_one_group(item12.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item12.all.insul.levels <-  proportions_one_group(item12.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item12.final <- rbind.data.frame(item12.summary
                                 , item12.all.frame.types
                                 , item12.all.insul.levels
                                 , stringsAsFactors = F)
item12.final <- item12.final[which(item12.final$rvalue.bins.SF != "Total"),]
item12.final$HomeYearBuilt_bins3[which(item12.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

item12.cast <- dcast(setDT(item12.final),
                     formula   = BuildingType + HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N"))

item12.table <- data.frame("BuildingType"     = item12.cast$BuildingType
                           ,"Housing.Vintage" = item12.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item12.cast$w.percent_R0
                           ,"SE.R0"           = item12.cast$w.SE_R0
                           ,"Count.R0"        = item12.cast$count_R0
                           ,"Percent.R1.R10"  = item12.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item12.cast$w.SE_R1.R10
                           ,"Count.R1.R10"    = item12.cast$count_R1.R10
                           ,"Percent.R11.R16" = item12.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item12.cast$w.SE_R11.R16
                           ,"Count.R11.R16"   = item12.cast$count_R11.R16
                           ,"Percent.R17.R22" = item12.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item12.cast$w.SE_R17.R22
                           ,"Count.R17.R22"   = item12.cast$count_R17.R22
                           ,"Percent.RGT22"   = item12.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item12.cast$w.SE_RGT22
                           ,"Count.RGT22"     = item12.cast$count_RGT22
                           )


item12.table.SF <- item12.table[which(item12.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item12.table.SF, "SF", "Table 19"
            , weighted = TRUE)



################################
# Unweighted Analysis
################################
item12.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item12.data
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
                     value.var = c("Percent", "SE", "Count", "SampleSize"))


item12.table <- data.frame("BuildingType"     = item12.cast$BuildingType
                           ,"Housing.Vintage" = item12.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item12.cast$Percent_R0
                           ,"SE.R0"           = item12.cast$SE_R0
                           ,"Count.R0"        = item12.cast$Count_R0
                           ,"Percent.R1.R10"  = item12.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item12.cast$SE_R1.R10
                           ,"Count.R1.R10"    = item12.cast$Count_R1.R10
                           ,"Percent.R11.R16" = item12.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item12.cast$SE_R11.R16
                           ,"Count.R11.R16"   = item12.cast$Count_R11.R16
                           ,"Percent.R17.R22" = item12.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item12.cast$SE_R17.R22
                           ,"Count.R17.R22"   = item12.cast$Count_R17.R22
                           ,"Percent.RGT22"   = item12.cast$Percent_RGT22
                           ,"SE.RGT22"        = item12.cast$SE_RGT22
                           ,"Count.RGT22"     = item12.cast$Count_RGT22
                           )


item12.table.SF <- item12.table[which(item12.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item12.table.SF, "SF", "Table 19"
            , weighted = FALSE)







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

## Summary only for "All Frame Types"
item12.all.frame.types <- proportions_one_group(item12.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.MH"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins2"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item12.all.insul.levels <-  proportions_one_group(item12.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins2"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.MH"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item12.final <- rbind.data.frame(item12.summary
                                 , item12.all.frame.types
                                 , item12.all.insul.levels
                                 , stringsAsFactors = F)
item12.final <- item12.final[which(item12.final$rvalue.bins.MH != "Total"),]
item12.final$HomeYearBuilt_bins2[which(item12.final$HomeYearBuilt_bins2 == "Total")] <- "All Housing Vintages"

item12.cast <- dcast(setDT(item12.final),
                     formula   = BuildingType + HomeYearBuilt_bins2 ~ rvalue.bins.MH,
                     value.var = c("w.percent", "w.SE", "count", "n", "N"))

item12.table <- data.frame("BuildingType"     = item12.cast$BuildingType
                           ,"Housing.Vintage" = item12.cast$HomeYearBuilt_bins2
                           ,"Percent.R0.R8"   = item12.cast$w.percent_R0.R8
                           ,"SE.R0.R8"        = item12.cast$w.SE_R0.R8
                           ,"n.R0.R8"         = item12.cast$count_R0.R8
                           ,"Percent.R9.R14"  = item12.cast$w.percent_R9.R14
                           ,"SE.R9.R14"       = item12.cast$w.SE_R9.R14
                           ,"n.R9.R14"        = item12.cast$count_R9.R14
                           ,"Percent.R15.R21" = item12.cast$w.percent_R15.R21
                           ,"SE.R15.R21"      = item12.cast$w.SE_R15.R21
                           ,"n.R15.R21"       = item12.cast$count_R15.R21
                           ,"Percent.R22.R30" = item12.cast$w.percent_R22.R30
                           ,"SE.R22.R30"      = item12.cast$w.SE_R22.R30
                           ,"n.R22.R30"       = item12.cast$count_R22.R30
                           )


item12.table.MH <- item12.table[which(item12.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item12.table.MH, "MH", "Table 16"
            , weighted = TRUE)


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

## Summary only for "All Frame Types"
item12.all.frame.types <- proportions_one_group(item12.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.MH"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins2"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item12.all.insul.levels <-  proportions_one_group(item12.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins2"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.MH"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item12.final <- rbind.data.frame(item12.summary
                                 , item12.all.frame.types
                                 , item12.all.insul.levels
                                 , stringsAsFactors = F)
item12.final <- item12.final[which(item12.final$rvalue.bins.MH != "Total"),]
item12.final$HomeYearBuilt_bins2[which(item12.final$HomeYearBuilt_bins2 == "Total")] <- "All Housing Vintages"

item12.cast <- dcast(setDT(item12.final),
                     formula   = BuildingType + HomeYearBuilt_bins2 ~ rvalue.bins.MH,
                     value.var = c("Percent", "SE", "Count", "SampleSize"))

item12.table <- data.frame("BuildingType"     = item12.cast$BuildingType
                           ,"Housing.Vintage" = item12.cast$HomeYearBuilt_bins2
                           ,"Percent.R0.R8"   = item12.cast$Percent_R0.R8
                           ,"SE.R0.R8"        = item12.cast$SE_R0.R8
                           ,"Count.R0.R8"     = item12.cast$Count_R0.R8
                           ,"Percent.R9.R14"  = item12.cast$Percent_R9.R14
                           ,"SE.R9.R14"       = item12.cast$SE_R9.R14
                           ,"Count.R9.R14"    = item12.cast$Count_R9.R14
                           ,"Percent.R15.R21" = item12.cast$Percent_R15.R21
                           ,"SE.R15.R21"      = item12.cast$SE_R15.R21
                           ,"Count.R15.R21"   = item12.cast$Count_R15.R21
                           ,"Percent.R22.R30" = item12.cast$Percent_R22.R30
                           ,"SE.R22.R30"      = item12.cast$SE_R22.R30
                           ,"Count.R22.R30"   = item12.cast$Count_R22.R30
)


item12.table.MH <- item12.table[which(item12.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item12.table.MH, "MH", "Table 16"
            , weighted = FALSE)








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
                     value.var = c("w.percent", "w.SE", "count", "n", "N"))

item13.table <- data.frame("BuildingType"     = item13.cast$BuildingType
                           ,"Housing.Vintage" = item13.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item13.cast$w.percent_R0
                           ,"SE.R0"           = item13.cast$w.SE_R0
                           ,"Count.R0"        = item13.cast$count_R0
                           ,"Percent.R1.R10"  = item13.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item13.cast$w.SE_R1.R10
                           ,"Count.R1.R10"    = item13.cast$count_R1.R10
                           ,"Percent.R11.R16" = item13.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item13.cast$w.SE_R11.R16
                           ,"Count.R11.R16"   = item13.cast$count_R11.R16
                           ,"Percent.R17.R22" = item13.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item13.cast$w.SE_R17.R22
                           ,"Count.R17.R22"   = item13.cast$count_R17.R22
                           ,"Percent.RGT22"   = item13.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item13.cast$w.SE_RGT22
                           ,"Count.RGT22"     = item13.cast$count_RGT22
)


item13.table.SF <- item13.table[which(item13.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item13.table.SF, "SF", "Table 20"
            , weighted = TRUE)



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
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item13.all.insul.levels <-  proportions_one_group(item13.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
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
                     value.var = c("Percent", "SE", "Count", "SampleSize"))


item13.table <- data.frame("BuildingType"     = item13.cast$BuildingType
                           ,"Housing.Vintage" = item13.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item13.cast$Percent_R0
                           ,"SE.R0"           = item13.cast$SE_R0
                           ,"Count.R0"        = item13.cast$Count_R0
                           ,"Percent.R1.R10"  = item13.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item13.cast$SE_R1.R10
                           ,"Count.R1.R10"    = item13.cast$Count_R1.R10
                           ,"Percent.R11.R16" = item13.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item13.cast$SE_R11.R16
                           ,"Count.R11.R16"   = item13.cast$Count_R11.R16
                           ,"Percent.R17.R22" = item13.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item13.cast$SE_R17.R22
                           ,"Count.R17.R22"   = item13.cast$Count_R17.R22
                           ,"Percent.RGT22"   = item13.cast$Percent_RGT22
                           ,"SE.RGT22"        = item13.cast$SE_RGT22
                           ,"Count.RGT22"     = item13.cast$Count_RGT22
)


item13.table.SF <- item13.table[which(item13.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item13.table.SF, "SF", "Table 20"
            , weighted = FALSE)










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
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item14.all.insul.levels <-  proportions_one_group(item14.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
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
                     value.var = c("w.percent", "w.SE", "count", "n", "N"))

item14.table <- data.frame("BuildingType"     = item14.cast$BuildingType
                           ,"Housing.Vintage" = item14.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item14.cast$w.percent_R0
                           ,"SE.R0"           = item14.cast$w.SE_R0
                           ,"Count.R0"        = item14.cast$count_R0
                           ,"Percent.R1.R10"  = item14.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item14.cast$w.SE_R1.R10
                           ,"Count.R1.R10"    = item14.cast$count_R1.R10
                           ,"Percent.R11.R16" = item14.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item14.cast$w.SE_R11.R16
                           ,"Count.R11.R16"   = item14.cast$count_R11.R16
                           ,"Percent.R17.R22" = item14.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item14.cast$w.SE_R17.R22
                           ,"Count.R17.R22"   = item14.cast$count_R17.R22
                           ,"Percent.RGT22"   = item14.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item14.cast$w.SE_RGT22
                           ,"Count.RGT22"     = item14.cast$count_RGT22
)


item14.table.SF <- item14.table[which(item14.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item14.table.SF, "SF", "Table 21"
            , weighted = TRUE)



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
                     value.var = c("Percent", "SE", "Count", "SampleSize"))


item14.table <- data.frame("BuildingType"     = item14.cast$BuildingType
                           ,"Housing.Vintage" = item14.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item14.cast$Percent_R0
                           ,"SE.R0"           = item14.cast$SE_R0
                           ,"Count.R0"        = item14.cast$Count_R0
                           ,"Percent.R1.R10"  = item14.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item14.cast$SE_R1.R10
                           ,"Count.R1.R10"    = item14.cast$Count_R1.R10
                           ,"Percent.R11.R16" = item14.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item14.cast$SE_R11.R16
                           ,"Count.R11.R16"   = item14.cast$Count_R11.R16
                           ,"Percent.R17.R22" = item14.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item14.cast$SE_R17.R22
                           ,"Count.R17.R22"   = item14.cast$Count_R17.R22
                           ,"Percent.RGT22"   = item14.cast$Percent_RGT22
                           ,"SE.RGT22"        = item14.cast$SE_RGT22
                           ,"Count.RGT22"     = item14.cast$Count_RGT22
)


item14.table.SF <- item14.table[which(item14.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item14.table.SF, "SF", "Table 21"
            , weighted = FALSE)







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
                     value.var = c("w.percent", "w.SE", "count", "n", "N"))

item15.table <- data.frame("BuildingType"     = item15.cast$BuildingType
                           ,"Housing.Vintage" = item15.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item15.cast$w.percent_R0
                           ,"SE.R0"           = item15.cast$w.SE_R0
                           ,"Count.R0"        = item15.cast$count_R0
                           ,"Percent.R1.R10"  = item15.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item15.cast$w.SE_R1.R10
                           ,"Count.R1.R10"    = item15.cast$count_R1.R10
                           ,"Percent.R11.R16" = item15.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item15.cast$w.SE_R11.R16
                           ,"Count.R11.R16"   = item15.cast$count_R11.R16
                           ,"Percent.R17.R22" = item15.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item15.cast$w.SE_R17.R22
                           ,"Count.R17.R22"   = item15.cast$count_R17.R22
                           ,"Percent.RGT22"   = item15.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item15.cast$w.SE_RGT22
                           ,"Count.RGT22"     = item15.cast$count_RGT22
)


item15.table.SF <- item15.table[which(item15.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item15.table.SF, "SF", "Table 22"
            , weighted = TRUE)



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
                     value.var = c("Percent", "SE", "Count", "SampleSize"))


item15.table <- data.frame("BuildingType"     = item15.cast$BuildingType
                           ,"Housing.Vintage" = item15.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item15.cast$Percent_R0
                           ,"SE.R0"           = item15.cast$SE_R0
                           ,"Count.R0"        = item15.cast$Count_R0
                           ,"Percent.R1.R10"  = item15.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item15.cast$SE_R1.R10
                           ,"Count.R1.R10"    = item15.cast$Count_R1.R10
                           ,"Percent.R11.R16" = item15.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item15.cast$SE_R11.R16
                           ,"Count.R11.R16"   = item15.cast$Count_R11.R16
                           ,"Percent.R17.R22" = item15.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item15.cast$SE_R17.R22
                           ,"Count.R17.R22"   = item15.cast$Count_R17.R22
                           ,"Percent.RGT22"   = item15.cast$Percent_RGT22
                           ,"SE.RGT22"        = item15.cast$SE_RGT22
                           ,"Count.RGT22"     = item15.cast$Count_RGT22
)


item15.table.SF <- item15.table[which(item15.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item15.table.SF, "SF", "Table 22"
            , weighted = FALSE)









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
                     value.var = c("w.percent", "w.SE", "count", "n", "N"))

item16.table <- data.frame("BuildingType"     = item16.cast$BuildingType
                           ,"Housing.Vintage" = item16.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item16.cast$w.percent_R0
                           ,"SE.R0"           = item16.cast$w.SE_R0
                           ,"Count.R0"        = item16.cast$count_R0
                           ,"Percent.R1.R10"  = item16.cast$w.percent_R1.R10
                           ,"SE.R1.R10"       = item16.cast$w.SE_R1.R10
                           ,"Count.R1.R10"    = item16.cast$count_R1.R10
                           ,"Percent.R11.R16" = item16.cast$w.percent_R11.R16
                           ,"SE.R11.R16"      = item16.cast$w.SE_R11.R16
                           ,"Count.R11.R16"   = item16.cast$count_R11.R16
                           ,"Percent.R17.R22" = item16.cast$w.percent_R17.R22
                           ,"SE.R17.R22"      = item16.cast$w.SE_R17.R22
                           ,"Count.R17.R22"   = item16.cast$count_R17.R22
                           ,"Percent.RGT22"   = item16.cast$w.percent_RGT22
                           ,"SE.RGT22"        = item16.cast$w.SE_RGT22
                           ,"Count.RGT22"     = item16.cast$count_RGT22
)


item16.table.SF <- item16.table[which(item16.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item16.table.SF, "SF", "Table 23"
            , weighted = TRUE)



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
                     value.var = c("Percent", "SE", "Count", "SampleSize"))


item16.table <- data.frame("BuildingType"     = item16.cast$BuildingType
                           ,"Housing.Vintage" = item16.cast$HomeYearBuilt_bins3
                           ,"Percent.R0"      = item16.cast$Percent_R0
                           ,"SE.R0"           = item16.cast$SE_R0
                           ,"Count.R0"        = item16.cast$Count_R0
                           ,"Percent.R1.R10"  = item16.cast$Percent_R1.R10
                           ,"SE.R1.R10"       = item16.cast$SE_R1.R10
                           ,"Count.R1.R10"    = item16.cast$Count_R1.R10
                           ,"Percent.R11.R16" = item16.cast$Percent_R11.R16
                           ,"SE.R11.R16"      = item16.cast$SE_R11.R16
                           ,"Count.R11.R16"   = item16.cast$Count_R11.R16
                           ,"Percent.R17.R22" = item16.cast$Percent_R17.R22
                           ,"SE.R17.R22"      = item16.cast$SE_R17.R22
                           ,"Count.R17.R22"   = item16.cast$Count_R17.R22
                           ,"Percent.RGT22"   = item16.cast$Percent_RGT22
                           ,"SE.RGT22"        = item16.cast$SE_RGT22
                           ,"Count.RGT22"     = item16.cast$Count_RGT22
)


item16.table.SF <- item16.table[which(item16.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item16.table.SF, "SF", "Table 23"
            , weighted = FALSE)






#############################################################################################
# Item 18: DISTRIBUTION OF OBSERVED WALL SHEATHING INSULATION BY FRAMING TYPE (SF table 25)
#############################################################################################
item18.dat <- prep.dat4.9#[which(prep.dat4.9$Wall.Type %notin% c("Masonry", "Masonry (Basement)", "Adiabatic")),]
item18.dat <- item18.dat[which(!is.na(item18.dat$Wall.Type)),]

item18.dat$Insulation.Levels <- item18.dat$exterior.inches1
unique(item18.dat$Insulation.Levels)

item18.merge <- left_join(rbsa.dat, item18.dat)

item18.customer <- summarise(group_by(item18.merge, CK_Cadmus_ID, BuildingType, State, Wall.Type)
                             ,insulation.levels = sum(Insulation.Levels))

item18.customer$insulation.levels[which(item18.customer$insulation.levels == 0)] <- "None"

item18.merge <- left_join(rbsa.dat, item18.customer)
item18.merge <- item18.merge[which(!is.na(item18.merge$Wall.Type)),]
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
                     formula   = BuildingType +  Wall.Type ~ insulation.levels, sum,
                     value.var = c("w.percent", "w.SE", "count","n","N"))
names(item18.cast)


item18.table <- data.frame("BuildingType"       = item18.cast$BuildingType
                           ,"Housing.Vintage"   = item18.cast$Wall.Type
                           ,"Percent_0.25_inch" = item18.cast$w.percent_0.25
                           ,"SE_0.25_inch"      = item18.cast$w.SE_0.25
                           ,"Count_0.25_inch"   = item18.cast$count_0.25
                           ,"Percent_0.5_inch"  = item18.cast$w.percent_0.5
                           ,"SE_0.5_inch"       = item18.cast$w.SE_0.5
                           ,"Count_0.5_inch"    = item18.cast$count_0.5
                           ,"Percent_0.75_inch" = item18.cast$w.percent_0.75
                           ,"SE_0.75_inch"      = item18.cast$w.SE_0.75
                           ,"Count_0.75_inch"   = item18.cast$count_0.75
                           ,"Percent_1_inch"    = item18.cast$w.percent_1
                           ,"SE_1_inch"         = item18.cast$w.SE_1
                           ,"Count_1_inch"      = item18.cast$count_1
                           ,"Percent_1.5_inch"  = item18.cast$w.percent_1.5
                           ,"SE_1.5_inch"       = item18.cast$w.SE_1.5
                           ,"Count_1.5_inch"    = item18.cast$count_1.5
                           ,"Percent_2_inch"    = item18.cast$w.percent_2
                           ,"SE_2_inch"         = item18.cast$w.SE_2
                           ,"Count_2_inch"      = item18.cast$count_2
                           # ,"Percent_2.5_inch"  = item18.cast$w.percent_2.5
                           # ,"SE_2.5_inch"       = item18.cast$w.SE_2.5
                           # ,"Count_2.5_inch"    = item18.cast$count_2.5
                           ,"Percent_3_inch"    = item18.cast$w.percent_3
                           ,"SE_3_inch"         = item18.cast$w.SE_3
                           ,"Count_3_inch"      = item18.cast$count_3
                           ,"Percent_None"      = item18.cast$w.percent_None
                           ,"SE_None"           = item18.cast$w.SE_None
                           ,"Count_None"        = item18.cast$count_None
                           # ,"SampleSize"        = item18.cast$count_Total
)


item18.table.SF <- item18.table[which(item18.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item18.table.SF, "SF", "Table 25"
            , weighted = TRUE)


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
                     value.var = c("Percent", "SE", "Count","SampleSize"))



item18.table <- data.frame("BuildingType"       = item18.cast$BuildingType
                           ,"Housing.Vintage"   = item18.cast$Wall.Type
                           ,"Percent_0.25_inch" = item18.cast$Percent_0.25
                           ,"SE_0.25_inch"      = item18.cast$SE_0.25
                           ,"Count_0.25_inch"   = item18.cast$Count_0.25
                           ,"Percent_0.5_inch"  = item18.cast$Percent_0.5
                           ,"SE_0.5_inch"       = item18.cast$SE_0.5
                           ,"Count_0.5_inch"    = item18.cast$Count_0.5
                           ,"Percent_0.75_inch" = item18.cast$Percent_0.75
                           ,"SE_0.75_inch"      = item18.cast$SE_0.75
                           ,"Count_0.75_inch"   = item18.cast$Count_0.75
                           ,"Percent_1_inch"    = item18.cast$Percent_1
                           ,"SE_1_inch"         = item18.cast$SE_1
                           ,"Count_1_inch"      = item18.cast$Count_1
                           ,"Percent_1.5_inch"  = item18.cast$Percent_1.5
                           ,"SE_1.5_inch"       = item18.cast$SE_1.5
                           ,"Count_1.5_inch"    = item18.cast$Count_1.5
                           ,"Percent_2_inch"    = item18.cast$Percent_2
                           ,"SE_2_inch"         = item18.cast$SE_2
                           ,"Count_2_inch"      = item18.cast$Count_2
                           # ,"Percent_2.5_inch"  = item18.cast$Percent_2.5
                           # ,"SE_2.5_inch"       = item18.cast$SE_2.5
                           # ,"Count_2.5_inch"    = item18.cast$Count_2.5
                           ,"Percent_3_inch"    = item18.cast$Percent_3
                           ,"SE_3_inch"         = item18.cast$SE_3
                           ,"Count_3_inch"      = item18.cast$Count_3
                           ,"Percent_None"      = item18.cast$Percent_None
                           ,"SE_None"           = item18.cast$SE_None
                           ,"Count_None"        = item18.cast$Count_None
                           # ,"SampleSize"        = item18.cast$Count_Total
)

item18.table.SF <- item18.table[which(item18.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item18.table.SF, "SF", "Table 25"
            , weighted = FALSE)








#############################################################################################
# Item 17: DISTRIBUTION OF Masonry Wall Insulation Levels by Vintage (SF table 24)
#############################################################################################
###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################
#remove unneccesary wall types
prep.dat2 <- prep.dat1.2[grep("masonry",prep.dat1.2$Wall.Type, ignore.case = T),-grep("cavity",colnames(prep.dat1.2), ignore.case = T)]
unique(prep.dat2$Wall.Type)

#assign new dataset
prep.dat3 <- prep.dat2

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################

for(i in 1:ncol(prep.dat3)){
  prep.dat3[,i] <- ifelse(prep.dat3[,i] == "-- Datapoint not asked for --", NA, prep.dat3[,i])
}

#cleaning for wall exterior
prep.dat3$Wall.Exterior.Insulation.Thickness.1[which(prep.dat3$Wall.Exterior.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.1[which(is.na(prep.dat3$Wall.Exterior.Insulation.Thickness.1))] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.1[which(prep.dat3$Wall.Exterior.Insulation.Thickness.1 == "Unknown")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.2[which(prep.dat3$Wall.Exterior.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.2[which(is.na(prep.dat3$Wall.Exterior.Insulation.Thickness.2))] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.2[which(prep.dat3$Wall.Exterior.Insulation.Thickness.2 == "Unknown")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.3[which(prep.dat3$Wall.Exterior.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.3[which(is.na(prep.dat3$Wall.Exterior.Insulation.Thickness.3))] <- "N/A N/A"
prep.dat3$Wall.Exterior.Insulation.Thickness.1[which(prep.dat3$Wall.Exterior.Insulation.Thickness.1 == "1.5")] <- "1.5 inches"
prep.dat3$Wall.Exterior.Insulation.Thickness.3[which(prep.dat3$Wall.Exterior.Insulation.Thickness.3 == "Unknown")] <- "N/A N/A"
unique(prep.dat3$Wall.Exterior.Insulation.Thickness.1)
unique(prep.dat3$Wall.Exterior.Insulation.Thickness.2)
unique(prep.dat3$Wall.Exterior.Insulation.Thickness.3)


#Clean Condition unknown values
prep.dat3$Wall.Exterior.Insulation.Condition.1[which(prep.dat3$Wall.Exterior.Insulation.Condition.1 == "Unknown")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.2[which(prep.dat3$Wall.Exterior.Insulation.Condition.2 == "Unknown")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.3[which(prep.dat3$Wall.Exterior.Insulation.Condition.3 == "Unknown")] <- "100%"

prep.dat3$Wall.Exterior.Insulation.Condition.1[which(is.na(prep.dat3$Wall.Exterior.Insulation.Condition.1) & prep.dat3$Wall.Exterior.Insulation.Thickness.1 != "N/A N/A")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.2[which(is.na(prep.dat3$Wall.Exterior.Insulation.Condition.2) & prep.dat3$Wall.Exterior.Insulation.Thickness.2 != "N/A N/A")] <- "100%"
prep.dat3$Wall.Exterior.Insulation.Condition.3[which(is.na(prep.dat3$Wall.Exterior.Insulation.Condition.3) & prep.dat3$Wall.Exterior.Insulation.Thickness.3 != "N/A N/A")] <- "100%"

prep.dat3$Wall.Exterior.Insulation.Condition.1[which(prep.dat3$`Wall.Exterior.Insulated?` == "No")] <- "0%"


#remove percent signs and make numeric
prep.dat3$Wall.Exterior.Insulation.Condition.1 <- gsub("%", "", prep.dat3$Wall.Exterior.Insulation.Condition.1)
prep.dat3$Wall.Exterior.Insulation.Condition.1 <- as.numeric(as.character(prep.dat3$Wall.Exterior.Insulation.Condition.1))
prep.dat3$Wall.Exterior.Insulation.Condition.2 <- gsub("%", "", prep.dat3$Wall.Exterior.Insulation.Condition.2)
prep.dat3$Wall.Exterior.Insulation.Condition.2 <- as.numeric(as.character(prep.dat3$Wall.Exterior.Insulation.Condition.2))
prep.dat3$Wall.Exterior.Insulation.Condition.3 <- gsub("%", "", prep.dat3$Wall.Exterior.Insulation.Condition.3)
prep.dat3$Wall.Exterior.Insulation.Condition.3 <- as.numeric(as.character(prep.dat3$Wall.Exterior.Insulation.Condition.3))


# add new ID variable for merging -- don't know if we need this
prep.dat3$count <- 1
prep.dat3$TMP_ID <- cumsum(prep.dat3$count)

## r-values ##
clean.insul1.0 <- unlist(strsplit(prep.dat3$Wall.Exterior.Insulation.Thickness.1, " "))
clean.insul1.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul1.0, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul1.2)

clean.insul2.0 <- unlist(strsplit(prep.dat3$Wall.Exterior.Insulation.Thickness.2, " "))
clean.insul2.00 <- as.data.frame(matrix(clean.insul2.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul2.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul2.00)
dim(clean.insul2.2)

clean.insul3.0 <- unlist(strsplit(prep.dat3$Wall.Exterior.Insulation.Thickness.3, " "))
clean.insul3.00 <- as.data.frame(matrix(clean.insul3.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul3.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul3.00)
dim(clean.insul3.2)

clean.insul.join4 <- left_join(clean.insul1.2, clean.insul2.2, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join5 <- left_join(clean.insul.join4, clean.insul3.2, by = c("CK_Cadmus_ID", "TMP_ID"))

colnames(clean.insul.join5) <- c("CK_Cadmus_ID"
                                 ,"TMP_ID"
                                 ,"exterior.inches1"
                                 ,"Remove.1"
                                 ,"exterior.inches2"
                                 ,"Remove.2"
                                 ,"exterior.inches3"
                                 ,"Remove.3")

clean.thickness.data <- clean.insul.join5[-grep("Remove", colnames(clean.insul.join5))]

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
prep.dat4$exterior.inches1 <- as.numeric(as.character(prep.dat4$exterior.inches1)) # warning here is OK
prep.dat4$exterior.inches2 <- as.numeric(as.character(prep.dat4$exterior.inches2)) # warning here is OK
prep.dat4$exterior.inches3 <- as.numeric(as.character(prep.dat4$exterior.inches3)) # warning here is OK

#replace any inches that are NA with zeros
for(i in grep("inches|rval", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}

#update column names
prep.dat4$exterior.rvalues1 <- prep.dat4$Wall.Exterior.Insulation.Type.1
prep.dat4$exterior.rvalues2 <- prep.dat4$Wall.Exterior.Insulation.Type.2
prep.dat4$exterior.rvalues3 <- prep.dat4$Wall.Exterior.Insulation.Type.3

#fix names that are not in R value table
prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "-- Datapoint not asked for --")]    <- NA
prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"
prep.dat4$exterior.rvalues1[grep('unknown', prep.dat4$exterior.rvalues1, ignore.case = T)] <- "Unknown"

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  prep.dat4$exterior.rvalues1[which(prep.dat4$exterior.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$exterior.rvalues2[which(prep.dat4$exterior.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$exterior.rvalues3[which(prep.dat4$exterior.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################

prep.dat4$exterior.rvalues1[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.rvalues2[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.rvalues3[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.inches1[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.inches2[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
prep.dat4$exterior.inches3[which(prep.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0

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
unique(prep.dat4.5$exterior.rvalues1)
unique(prep.dat4.5$exterior.rvalues2)
unique(prep.dat4.5$exterior.rvalues3)

prep.dat4.5$Wall.Exterior.Insulation.Condition.1 <- prep.dat4.5$Wall.Exterior.Insulation.Condition.1 / 100


# clean up condition information
prep.condition.sub2 <- prep.dat4.5[which(prep.dat4.5$Wall.Exterior.Insulation.Condition.1 %notin% c(1, NA)),]
prep.condition.sub2$Wall.Exterior.Insulation.Condition.1 <- 1 - prep.condition.sub2$Wall.Exterior.Insulation.Condition.1
prep.condition.sub2$total.r.val <- NA


prep.dat4.9 <- rbind.data.frame(prep.dat4.5
                                ,prep.condition.sub2
                                , stringsAsFactors = F)
prep.dat5 <- prep.dat4.9[which(!is.na(prep.dat4.9$Wall.Type)),]

#calculate the weighted r value
na.ind <- which(is.na(prep.dat5$total.r.val))
prep.dat5$total.r.val[na.ind] <- (prep.dat5$exterior.rvalues1[na.ind] * prep.dat5$exterior.inches1[na.ind]) + 
  (prep.dat5$exterior.rvalues2[na.ind] * prep.dat5$exterior.inches2[na.ind]) + 
  (prep.dat5$exterior.rvalues3[na.ind] * prep.dat5$exterior.inches3[na.ind])

#check -- NOTE -- NONE SHOULD BE NA
unique(prep.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
prep.dat5$uvalue <- 1 / (1 + prep.dat5$total.r.val)
unique(prep.dat5$uvalue)

#make area numeric
prep.dat5$uvalue    <- as.numeric(as.character(prep.dat5$uvalue))
prep.dat5$Wall.Area <- as.numeric(as.character(prep.dat5$Wall.Area))

prep.dat5$Wall.Exterior.Insulation.Condition.1[which(is.na(prep.dat5$Wall.Exterior.Insulation.Condition.1))] <- 0

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * Wall.Exterior.Insulation.Condition.1 * uvalue) / sum(Wall.Area * Wall.Exterior.Insulation.Condition.1)
)
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 0

#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval))) - 1
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0
unique(weightedU$aveRval)


# get unique cadmus IDs and building types for this subset of data
wall.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
prep.dat7 <- left_join(prep.dat6, rbsa.dat)
prep.dat7$aveUval[which(is.na(prep.dat7$aveUval))] <- 0
prep.dat7$aveRval[which(is.na(prep.dat7$aveRval))] <- 0


item17.dat <- prep.dat7

#Bin R values -- SF only
item17.dat$rvalue.bins <- "Unknown"
item17.dat$rvalue.bins[which(item17.dat$aveRval < 1)] <- "None"
item17.dat$rvalue.bins[which(item17.dat$aveRval >=  1  & item17.dat$aveRval < 10)]  <- "R1.R9"
item17.dat$rvalue.bins[which(item17.dat$aveRval >= 10 & item17.dat$aveRval < 16)]  <- "R10.R15"
item17.dat$rvalue.bins[which(item17.dat$aveRval >= 16 & item17.dat$aveRval < 21)]  <- "R16.R20"
item17.dat$rvalue.bins[which(item17.dat$aveRval >= 21)] <- "RGT21"
unique(item17.dat$rvalue.bins)

item17.dat$count <- 1

item17.dat1 <- item17.dat[which(item17.dat$rvalue.bins != "Unknown"),]
colnames(item17.dat1)

item17.merge <- left_join(rbsa.dat, item17.dat1)
item17.merge <- item17.merge[which(!is.na(item17.merge$count)),]

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
############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
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
                     value.var = c("w.percent", "w.SE", "count", "n", "N"))

#join all insulation levels onto rvalue summary
item17.table <- data.frame("BuildingType"     = item17.cast$BuildingType
                           ,"Housing.Vintage" = item17.cast$HomeYearBuilt_bins3
                           ,"Percent.None"    = item17.cast$w.percent_None
                           ,"SE.None"         = item17.cast$w.SE_None
                           ,"Count.None"      = item17.cast$count_None
                           ,"Percent.R1.R9"   = item17.cast$w.percent_R1.R9
                           ,"SE.R1.R9"        = item17.cast$w.SE_R1.R9
                           ,"Count.R1.R9"     = item17.cast$count_R1.R9
                           ,"Percent.R10.R15" = item17.cast$w.percent_R10.R15
                           ,"SE.R10.R15"      = item17.cast$w.SE_R10.R15
                           ,"Count.R10.R15"   = item17.cast$count_R10.R15
                           ,"Percent.R16.R20" = NA #item17.cast$w.percent_R16.R20
                           ,"SE.R16.R20"      = NA #item17.cast$w.SE_R16.R20
                           ,"Count.R16.R20"   = NA #item17.cast$count_R16.R20
                           ,"Percent.RGT21"   = NA #item17.cast$w.percent_RGT21
                           ,"SE.RGT21"        = NA #item17.cast$w.SE_RGT21
                           ,"Count.RGT21"     = NA #item17.cast$count_RGT21
                           ,"SampleSize"      = NA #item17.cast$count_Total
                           )

item17.table.SF <- item17.table[which(item17.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item17.table.SF, "SF", "Table 24"
            , weighted = TRUE)

############################################################################################################
# Weighted Analysis - Single Family
############################################################################################################
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
                     value.var = c("Percent", "SE", "Count", "SampleSize"))

#join all insulation levels onto rvalue summary
item17.table <- data.frame("BuildingType"     = item17.cast$BuildingType
                           ,"Housing.Vintage" = item17.cast$HomeYearBuilt_bins3
                           ,"Percent.None"    = item17.cast$Percent_None
                           ,"SE.None"         = item17.cast$SE_None
                           ,"Count.None"      = item17.cast$Count_None
                           ,"Percent.R1.R9"   = item17.cast$Percent_R1.R9
                           ,"SE.R1.R9"        = item17.cast$SE_R1.R9
                           ,"Count.R1.R9"     = item17.cast$Count_R1.R9
                           ,"Percent.R10.R15" = item17.cast$Percent_R10.R15
                           ,"SE.R10.R15"      = item17.cast$SE_R10.R15
                           ,"Count.R10.R15"   = item17.cast$Count_R10.R15
                           ,"Percent.R16.R20" = NA #item17.cast$Percent_R16.R20
                           ,"SE.R16.R20"      = NA #item17.cast$SE_R16.R20
                           ,"Count.R16.R20"   = NA #item17.cast$Count_R16.R20
                           ,"Percent.RGT21"   = NA #item17.cast$Percent_RGT21
                           ,"SE.RGT21"        = NA #item17.cast$SE_RGT21
                           ,"Count.RGT21"     = NA #item17.cast$Count_RGT21
                           ,"SampleSize"      = NA #item17.cast$Count_Total
)

item17.table.SF <- item17.table[which(item17.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item17.table.SF, "SF", "Table 24"
            , weighted = FALSE)




















#############################################################################################
# Item 175: DISTRIBUTION OF WALL U-VALUE BY STATE  (MH table 17)
#############################################################################################
## Note: For this table, you must run up to prep.dat7 for the cleaned data
item175.dat <- prep.dat7

item175.dat1 <- item175.dat

############################################################################################################
# Apply weights
############################################################################################################
item175.dat1$count <- 1
colnames(item175.dat1)

item175.merge <- left_join(rbsa.dat, item175.dat1)
item175.merge <- item175.merge[which(!is.na(item175.merge$count)),]

item175.data <- weightedData(unique(item175.merge[-which(colnames(item175.merge) %in% c("Wall.Type"
                                                                                     ,"aveUval"
                                                                                     ,"aveRval"
                                                                                     ,"count"))]))
item175.data <- left_join(item175.data, item175.merge[which(colnames(item175.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"count"))])


############################################################################################################
# Weighted Analysis - Manufactured
############################################################################################################
item175.final <- mean_one_group(CustomerLevelData = item175.data
                                ,valueVariable = 'aveUval'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")

item175.table.MH <- item175.final[which(item175.final$BuildingType == "Manufactured"),-1]
item175.table.MH$n[which(item175.table.MH$State == "Region")] <- item175.table.MH$n_h[which(item175.table.MH$State == "Region")]
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
