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



#############################################################################################
# Item 10: DISTRIBUTION OF FRAME WALL INSULATION LEVELS BY FRAMING TYPE (SF table 17)
#############################################################################################

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]

#subset envelope data to necessary columns
item10.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
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
item10.dat0 <- item10.dat[which(item10.dat$`Wall.Cavity.Insulated?` %in% c("Yes", "No")),]
item10.dat0$`Wall.Exterior.Insulated?`[which(item10.dat0$`Wall.Exterior.Insulated?` != "Yes")] <- "No" ###treat anything not Yes as No
item10.dat1.0 <- item10.dat0[which(!(is.na(item10.dat0$Wall.Area))),]
item10.dat1.1 <- item10.dat1.0[which(item10.dat1.0$Wall.Cavity.Insulation.Thickness.1 != "Unknown"),]
item10.dat1.2 <- item10.dat1.1[-which(item10.dat1.1$Wall.Exterior.Insulation.Thickness.1 == "Unknown"),]

#review types
unique(item10.dat1.2$Wall.Cavity.Insulation.Type.1)
unique(item10.dat1.2$Wall.Cavity.Insulation.Type.2)
unique(item10.dat1.2$Wall.Cavity.Insulation.Type.3) #nothing in this column
unique(item10.dat1.2$Wall.Exterior.Insulation.Type.1)
unique(item10.dat1.2$Wall.Exterior.Insulation.Type.2) #nothing in this column
unique(item10.dat1.2$Wall.Exterior.Insulation.Type.3) #nothing in this column

#review insulation thicknesses
unique(item10.dat1.2$Wall.Cavity.Insulation.Thickness.1)
unique(item10.dat1.2$Wall.Cavity.Insulation.Thickness.2)
unique(item10.dat1.2$Wall.Cavity.Insulation.Thickness.3)
unique(item10.dat1.2$Wall.Exterior.Insulation.Thickness.1)
unique(item10.dat1.2$Wall.Exterior.Insulation.Thickness.2)
unique(item10.dat1.2$Wall.Exterior.Insulation.Thickness.3)

#review conditions
unique(item10.dat1.2$Wall.Cavity.Insulation.Condition.1)
unique(item10.dat1.2$Wall.Cavity.Insulation.Condition.2)
unique(item10.dat1.2$Wall.Cavity.Insulation.Condition.3)
unique(item10.dat1.2$Wall.Exterior.Insulation.Condition.1)
unique(item10.dat1.2$Wall.Exterior.Insulation.Condition.2)
unique(item10.dat1.2$Wall.Exterior.Insulation.Condition.3)


#remove unneccesary wall types
item10.dat2 <- item10.dat1.2[which(!(item10.dat1.2$Wall.Type %in% c("Masonry","Masonry (Basement)","Log","Adiabatic"))),]

#create "Alternative" category
item10.dat2$Wall.Type[which(item10.dat2$Wall.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
length(unique(item10.dat2$CK_Cadmus_ID))#473
unique(item10.dat2$Wall.Type)

#assign new dataset
item10.dat3 <- item10.dat2

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################

for(i in 1:ncol(item10.dat3)){
  item10.dat3[,i] <- ifelse(item10.dat3[,i] == "-- Datapoint not asked for --", NA, item10.dat3[,i])
}

#cleaning for wall.cavity
item10.dat3$Wall.Cavity.Insulation.Thickness.1[which(item10.dat3$Wall.Cavity.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item10.dat3$Wall.Cavity.Insulation.Thickness.1[which(is.na(item10.dat3$Wall.Cavity.Insulation.Thickness.1))] <- "N/A N/A"
item10.dat3$Wall.Cavity.Insulation.Thickness.2[which(item10.dat3$Wall.Cavity.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item10.dat3$Wall.Cavity.Insulation.Thickness.2[which(item10.dat3$Wall.Cavity.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item10.dat3$Wall.Cavity.Insulation.Thickness.2[which(is.na(item10.dat3$Wall.Cavity.Insulation.Thickness.2))] <- "N/A N/A"
item10.dat3$Wall.Cavity.Insulation.Thickness.3[which(item10.dat3$Wall.Cavity.Insulation.Thickness.3 == "Unknown")] <- "Unknown Unknown"
item10.dat3$Wall.Cavity.Insulation.Thickness.3[which(item10.dat3$Wall.Cavity.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
item10.dat3$Wall.Cavity.Insulation.Thickness.3[which(is.na(item10.dat3$Wall.Cavity.Insulation.Thickness.3))] <- "N/A N/A"
unique(item10.dat3$Wall.Cavity.Insulation.Thickness.1)
unique(item10.dat3$Wall.Cavity.Insulation.Thickness.2)
unique(item10.dat3$Wall.Cavity.Insulation.Thickness.3)

#cleaning for wall exterior
item10.dat3$Wall.Exterior.Insulation.Thickness.1[which(item10.dat3$Wall.Exterior.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item10.dat3$Wall.Exterior.Insulation.Thickness.1[which(is.na(item10.dat3$Wall.Exterior.Insulation.Thickness.1))] <- "N/A N/A"
item10.dat3$Wall.Exterior.Insulation.Thickness.2[which(item10.dat3$Wall.Exterior.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item10.dat3$Wall.Exterior.Insulation.Thickness.2[which(is.na(item10.dat3$Wall.Exterior.Insulation.Thickness.2))] <- "N/A N/A"
item10.dat3$Wall.Exterior.Insulation.Thickness.3[which(item10.dat3$Wall.Exterior.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
item10.dat3$Wall.Exterior.Insulation.Thickness.3[which(is.na(item10.dat3$Wall.Exterior.Insulation.Thickness.3))] <- "N/A N/A"
unique(item10.dat3$Wall.Exterior.Insulation.Thickness.1)
unique(item10.dat3$Wall.Exterior.Insulation.Thickness.2)
unique(item10.dat3$Wall.Exterior.Insulation.Thickness.3)


#Clean Condition unknown values
item10.dat3$Wall.Cavity.Insulation.Condition.1[which(item10.dat3$Wall.Cavity.Insulation.Condition.1 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.2[which(item10.dat3$Wall.Cavity.Insulation.Condition.2 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.3[which(item10.dat3$Wall.Cavity.Insulation.Condition.3 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.1[which(item10.dat3$Wall.Cavity.Insulation.Condition.1 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.2[which(item10.dat3$Wall.Cavity.Insulation.Condition.2 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.3[which(item10.dat3$Wall.Cavity.Insulation.Condition.3 == "Unknown")] <- "100%"


# add new ID variable for merging -- don't know if we need this
item10.dat3$count <- 1
item10.dat3$TMP_ID <- cumsum(item10.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item10.dat3$Wall.Cavity.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item10.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item10.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item10.dat3$Wall.Cavity.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item10.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item10.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul3 <- unlist(strsplit(item10.dat3$Wall.Cavity.Insulation.Thickness.3, " "))
clean.insul3.1 <- cbind.data.frame("CK_Cadmus_ID" = item10.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item10.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul3, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul3.1)

clean.insul1.0 <- unlist(strsplit(item10.dat3$Wall.Exterior.Insulation.Thickness.1, " "))
clean.insul1.00 <- as.data.frame(matrix(clean.insul1.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.2 <- cbind.data.frame("CK_Cadmus_ID" = item10.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item10.dat3$TMP_ID
                                   , clean.insul1.00)
dim(clean.insul1.2)

clean.insul2.0 <- unlist(strsplit(item10.dat3$Wall.Exterior.Insulation.Thickness.2, " "))
clean.insul2.00 <- as.data.frame(matrix(clean.insul2.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul2.2 <- cbind.data.frame("CK_Cadmus_ID" = item10.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item10.dat3$TMP_ID
                                   , clean.insul2.00)
dim(clean.insul2.2)

clean.insul3.0 <- unlist(strsplit(item10.dat3$Wall.Exterior.Insulation.Thickness.3, " "))
clean.insul3.00 <- as.data.frame(matrix(clean.insul3.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul3.2 <- cbind.data.frame("CK_Cadmus_ID" = item10.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item10.dat3$TMP_ID
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
item10.dat4 <- as.data.frame(left_join(item10.dat3, clean.thickness.data, by = c("CK_Cadmus_ID", "TMP_ID"))
                             , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# make numeric
item10.dat4$cavity.inches1   <- as.numeric(as.character(item10.dat4$cavity.inches1)) # warning here is OK
item10.dat4$cavity.inches2   <- as.numeric(as.character(item10.dat4$cavity.inches2)) # warning here is OK
item10.dat4$cavity.inches3   <- as.numeric(as.character(item10.dat4$cavity.inches3)) # warning here is OK
item10.dat4$exterior.inches1 <- as.numeric(as.character(item10.dat4$exterior.inches1)) # warning here is OK
item10.dat4$exterior.inches2 <- as.numeric(as.character(item10.dat4$exterior.inches2)) # warning here is OK
item10.dat4$exterior.inches3 <- as.numeric(as.character(item10.dat4$exterior.inches3)) # warning here is OK

#replace any inches that are NA with zeros
for(i in 28:33){
  item10.dat4[,i] <- ifelse(is.na(item10.dat4[,i]), 0, item10.dat4[,i])
}

#update column names
item10.dat4$cavity.rvalues1 <- item10.dat4$Wall.Cavity.Insulation.Type.1
item10.dat4$cavity.rvalues2 <- item10.dat4$Wall.Cavity.Insulation.Type.2
item10.dat4$cavity.rvalues3 <- item10.dat4$Wall.Cavity.Insulation.Type.3
item10.dat4$exterior.rvalues1 <- item10.dat4$Wall.Exterior.Insulation.Type.1
item10.dat4$exterior.rvalues2 <- item10.dat4$Wall.Exterior.Insulation.Type.2
item10.dat4$exterior.rvalues3 <- item10.dat4$Wall.Exterior.Insulation.Type.3

#fix names that are not in R value table
item10.dat4$cavity.rvalues1[which(item10.dat4$cavity.rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item10.dat4$cavity.rvalues1[which(item10.dat4$cavity.rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item10.dat4$cavity.rvalues1[which(item10.dat4$cavity.rvalues1 == "-- Datapoint not asked for --")]    <- NA
item10.dat4$cavity.rvalues1[which(item10.dat4$cavity.rvalues1 == "None")]                             <- NA
item10.dat4$cavity.rvalues2[which(item10.dat4$cavity.rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item10.dat4$cavity.rvalues2[which(item10.dat4$cavity.rvalues2 == "N/A")]                              <- NA
item10.dat4$cavity.rvalues2[which(item10.dat4$cavity.rvalues2 == "-- Datapoint not asked for --")]    <- NA
item10.dat4$exterior.rvalues1[which(item10.dat4$exterior.rvalues1 == "-- Datapoint not asked for --")]    <- NA
item10.dat4$exterior.rvalues1[which(item10.dat4$exterior.rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
item10.dat4$exterior.rvalues1[which(item10.dat4$exterior.rvalues1 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item10.dat4$cavity.rvalues1[which(item10.dat4$cavity.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item10.dat4$cavity.rvalues2[which(item10.dat4$cavity.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item10.dat4$cavity.rvalues3[which(item10.dat4$cavity.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item10.dat4$exterior.rvalues1[which(item10.dat4$exterior.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item10.dat4$exterior.rvalues2[which(item10.dat4$exterior.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item10.dat4$exterior.rvalues3[which(item10.dat4$exterior.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################

item10.dat4$cavity.rvalues1[which(item10.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item10.dat4$cavity.rvalues2[which(item10.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item10.dat4$cavity.rvalues3[which(item10.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item10.dat4$exterior.rvalues1[which(item10.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
item10.dat4$exterior.rvalues2[which(item10.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
item10.dat4$exterior.rvalues3[which(item10.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
item10.dat4$cavity.inches1[which(item10.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item10.dat4$cavity.inches2[which(item10.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item10.dat4$cavity.inches3[which(item10.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item10.dat4$exterior.inches1[which(item10.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
item10.dat4$exterior.inches2[which(item10.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
item10.dat4$exterior.inches3[which(item10.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0

#replace any inches and rvalues that are NA with zeros
for(i in 28:39){
  item10.dat4[,i] <- ifelse(is.na(item10.dat4[,i]), 0, item10.dat4[,i])
}


#make all inches and rvalue columns numeric
for(i in 28:39){
  item10.dat4[,i] <- as.numeric(as.character(item10.dat4[,i]))
}

item10.dat5 <- item10.dat4


#check uniques -- None should be NA
unique(item10.dat4$cavity.rvalues1)
unique(item10.dat4$cavity.rvalues2)
unique(item10.dat4$cavity.rvalues3)
unique(item10.dat4$exterior.rvalues1)
unique(item10.dat4$exterior.rvalues2)
unique(item10.dat4$exterior.rvalues3)



###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################

#create total.r.value column
item10.dat5$total.r.val <- NA

#calculate the weighted r value
item10.dat5$total.r.val <- (item10.dat5$cavity.rvalues1 * item10.dat5$cavity.inches1) +  
    (item10.dat5$cavity.rvalues2 * item10.dat5$cavity.inches2) + 
    (item10.dat5$exterior.rvalues1 * item10.dat5$exterior.inches3)

#check -- NOTE -- NONE SHOULD BE NA
unique(item10.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item10.dat5$uvalue <- 1 / item10.dat5$total.r.val
unique(item10.dat5$uvalue)

# replace inf with 0
item10.dat5$uvalue[which(item10.dat5$uvalue == "Inf")] <- 0

#make area numeric
item10.dat5$uvalue <- as.numeric(as.character(item10.dat5$uvalue))
item10.dat5$Wall.Area <- as.numeric(as.character(item10.dat5$Wall.Area))

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(item10.dat5, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * uvalue) / sum(Wall.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
unique(weightedU$aveRval)
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

wall.unique <- unique(item10.dat5[which(colnames(item10.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item10.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item10.dat7 <- left_join(item10.dat6, rbsa.dat, by = "CK_Cadmus_ID")


#Bin R values -- SF only
item10.dat7$rvalue.bins <- "Unknown"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval == 0)] <- "R0"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >  0  & item10.dat7$aveRval < 11)]  <- "R1.R10"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >= 11 & item10.dat7$aveRval < 17)]  <- "R11.R16"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >= 17 & item10.dat7$aveRval < 23)]  <- "R17.R22"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >= 22)] <- "RGT22"
unique(item10.dat7$rvalue.bins)

item10.dat7$count <- 1

item10.dat8 <- item10.dat7[which(item10.dat7$rvalue.bins != "Unknown"),]
colnames(item10.dat8)

item10.merge <- left_join(rbsa.dat, item10.dat8)
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

item10.final <- proportionRowsAndColumns1(item10.data
                                          , valueVariable       = 'count'
                                          , columnVariable      = 'rvalue.bins'
                                          , rowVariable         = 'Wall.Type'
                                          , aggregateColumnName = "All Insulation Levels"
                                          )


# #summarise by wall frame types
# #summarise by r value bins
# item10.sum1 <- summarise(group_by(item10.dat8, BuildingType, Wall.Type, rvalue.bins)
#                          ,SampleSize = ""
#                          ,Count = sum(count))
# #summarise across r value bins
# item10.sum2 <- summarise(group_by(item10.dat8, BuildingType, Wall.Type)
#                          ,rvalue.bins = "All Insulation Levels"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count))
# 
# 
# #summarise across wall frame types
# #summarise by r value bins
# item10.sum3 <- summarise(group_by(item10.dat8, BuildingType, rvalue.bins)
#                          , Wall.Type = "All Frame Types"
#                          ,SampleSize = ""
#                          ,Count = sum(count))
# #summarise across r value bins
# item10.sum4 <- summarise(group_by(item10.dat8, BuildingType)
#                          ,rvalue.bins = "All Insulation Levels"
#                          , Wall.Type = "All Frame Types"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count))
# 
# item10.merge <- rbind.data.frame(item10.sum1,item10.sum2,item10.sum3,item10.sum4,stringsAsFactors = F)
# 
# item10.tot.counts <- rbind.data.frame(item10.sum2, item10.sum4, stringsAsFactors = F)
# item10.tot.counts <- item10.tot.counts[which(colnames(item10.tot.counts) %in% c("BuildingType"
#                                                                                 , "Wall.Type"
#                                                                                 , "Count"))]
# item10.sampleSizes <- item10.merge[which(item10.merge$SampleSize != ""),which(colnames(item10.merge) %in% c("BuildingType"
#                                                                                                             ,"SampleSize"
#                                                                                                             ,"Wall.Type"))]
# 
# item10.merge1 <- left_join(item10.merge, item10.tot.counts, by = c("BuildingType", "Wall.Type"))
# item10.merge2 <- left_join(item10.merge1, item10.sampleSizes, by = c("BuildingType", "Wall.Type"))
# colnames(item10.merge2) <- c("BuildingType", "Wall.Type", "rvalue.bins", "Remove", "Count", "TotalCount", "SampleSize")
# 
# building.type <- rbind.data.frame("Manufactured"
#                                   , "Multifamily - High Rise"
#                                   , "Multifamily - Low Rise"
#                                   , "Single Family"
#                                   , stringsAsFactors = F)
# 
# for(i in 1:4){
#   item10.merge2$TotalCount[which(item10.merge2$BuildingType == building.type[i,] & item10.merge2$rvalue.bins == "All Insulation Levels")] <-
#     item10.merge2$TotalCount[which(item10.merge2$BuildingType == building.type[i,] & item10.merge2$rvalue.bins == "All Insulation Levels" & item10.merge2$Wall.Type == "All Frame Types")]
# }
# 
# item10.final <- item10.merge2
# item10.final$SampleSize <- as.numeric(as.character(item10.final$SampleSize))
# 
# item10.final$Percent <- item10.final$Count / item10.final$TotalCount
# item10.final$SE <- sqrt(item10.final$Percent * (1 - item10.final$Percent) / item10.final$SampleSize)

##cast data
item10.cast <- dcast(setDT(item10.final),
                      formula   = BuildingType +  Wall.Type ~ rvalue.bins,
                      value.var = c("w.percent", "w.SE", "count", "n", "N"))

#join all insulation levels onto rvalue summary
item10.table <- data.frame("BuildingType"     = item10.cast$BuildingType
                            ,"Wall.Type"       = item10.cast$Wall.Type
                            ,"Percent.R0"      = item10.cast$w.percent_R0
                            ,"SE.R0"           = item10.cast$w.SE_R0
                            ,"Count.R0"        = item10.cast$count_R0
                            ,"Percent.R1.R10"  = item10.cast$w.percent_R1.R10
                            ,"SE.R1.R10"       = item10.cast$w.SE_R1.R10
                            ,"Count.R1.R10"    = item10.cast$count_R1.R10
                            ,"Percent.R11.R16" = item10.cast$w.percent_R11.R16
                            ,"SE.R11.R16"      = item10.cast$w.SE_R11.R16
                            ,"Count.R11.R16"   = item10.cast$count_R11.R16
                            ,"Percent.R17.R22" = item10.cast$w.percent_R17.R22
                            ,"SE.R17.R22"      = item10.cast$w.SE_R17.R22
                            ,"Count.R17.R22"   = item10.cast$count_R17.R22
                            ,"Percent.RGT22"   = item10.cast$w.percent_RGT22
                            ,"SE.RGT22"        = item10.cast$w.SE_RGT22
                            ,"Count.RGT22"     = item10.cast$count_RGT22
                            ,"Percent_All Insulation Levels" = item10.cast$`w.percent_All Insulation Levels`
                            ,"SE_All Insulation Levels"      = item10.cast$`w.SE_All Insulation Levels`
                            ,"SampleSize"      = item10.cast$`count_All Insulation Levels`)

item10.table.SF <- item10.table[which(item10.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item10.table.SF, "SF", "Table 17")







#############################################################################################
# Item 11: DISTRIBUTION OF WALL FRAMING TYPES BY VINTAGE (SF table 18)
#############################################################################################
## Note: For this table, you must run up to item10.dat3 for the cleaned data
item11.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Wall.Type"))]

item11.dat2 <- left_join(item11.dat1, rbsa.dat, by = c("CK_Cadmus_ID"))

#remove unneccesary wall types
item11.dat3 <- item11.dat2[which(!(item11.dat2$Wall.Type %in% c("Masonry","Masonry (Basement)","Log","Adiabatic"))),]

#create "Alternative" category
item11.dat3$Wall.Type[which(item11.dat3$Wall.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
length(unique(item11.dat3$CK_Cadmus_ID))#506
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



item11.final <- proportionRowsAndColumns1(item11.data
                                          , valueVariable       = 'count'
                                          , columnVariable      = 'Wall.Type'
                                          , rowVariable         = 'HomeYearBuilt_bins3'
                                          , aggregateColumnName = "All Home Vintages"
)


item11.cast <- dcast(setDT(item11.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ Wall.Type, sum,
                     value.var = c("w.percent", "w.SE", "count","n","N"))

item11.sum1 <- summarise(group_by(item11.cast, BuildingType, HomeYearBuilt_bins4)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count_2x4 = sum(`Framed 2x4`)
                         ,Count_2x6 = sum(`Framed 2x6`)
                         ,Count_ALT = sum(Alternative)
)

item11.sum2 <- summarise(group_by(item11.cast, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Home Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count_2x4 = sum(`Framed 2x4`)
                         ,Count_2x6 = sum(`Framed 2x6`)
                         ,Count_ALT = sum(Alternative)
)

item11.dat4 <- rbind.data.frame(item11.sum1, item11.sum2, stringsAsFactors = F)
item11.dat4$TotalCount <- item11.dat4$Count_2x4 + item11.dat4$Count_2x6 + item11.dat4$Count_ALT
item11.dat4$Percent_2x4 <- item11.dat4$Count_2x4 / item11.dat4$TotalCount
item11.dat4$SE_2x4 <- sqrt(item11.dat4$Percent_2x4 * (1 - item11.dat4$Percent_2x4) / item11.dat4$SampleSize)
item11.dat4$Percent_2x6 <- item11.dat4$Count_2x6 / item11.dat4$TotalCount
item11.dat4$SE_2x6 <- sqrt(item11.dat4$Percent_2x6 * (1 - item11.dat4$Percent_2x6) / item11.dat4$SampleSize)
item11.dat4$Percent_ALT <- item11.dat4$Count_ALT / item11.dat4$TotalCount
item11.dat4$SE_ALT <- sqrt(item11.dat4$Percent_ALT * (1 - item11.dat4$Percent_ALT) / item11.dat4$SampleSize)


item11.final <- data.frame("BuildingType" = item11.dat4$BuildingType
                           ,"Housing.Vintage" = item11.dat4$HomeYearBuilt_bins4
                           ,"Percent_2x4" = item11.dat4$Percent_2x4
                           ,"SE_2x4" = item11.dat4$SE_2x4
                           ,"Percent_2x6" = item11.dat4$Percent_2x6
                           ,"SE_2x6" = item11.dat4$SE_2x6
                           ,"Percent_ALT" = item11.dat4$Percent_ALT
                           ,"SE_ALT" = item11.dat4$SE_ALT
                           ,"SampleSize" = item11.dat4$SampleSize)


item11.table1 <- item11.final[which(!(is.na(item11.final$Housing.Vintage))),]
item11.table2 <- item11.table1[which(item11.table1$BuildingType %in% c("Single Family")),]










#############################################################################################
# Item 12: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE  (SF table 19, MH table 16)
#############################################################################################
## Note: For this table, you must run up to item10.dat7 for the cleaned data
item12.dat <- item10.dat8

item12.dat1 <- item12.dat[which(!(is.na(item12.dat$HomeYearBuilt_bins4))),]

############################################################################################################
## For single family
############################################################################################################
#summarise by vintage bins
#summarise by r value bins
item12.sum1 <- summarise(group_by(item12.dat1, BuildingType, HomeYearBuilt_bins4, rvalue.bins)
                         ,Count = sum(count))
#summarise across r value bins
item12.sum2 <- summarise(group_by(item12.dat1, BuildingType, HomeYearBuilt_bins4)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item12.merge1 <- left_join(item12.sum1, item12.sum2, by = c("BuildingType", "HomeYearBuilt_bins4"))

#summarise across wall frame types
#summarise by r value bins
item12.sum3 <- summarise(group_by(item12.dat1, BuildingType, rvalue.bins)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,Count = sum(count))
#summarise across r value bins
item12.sum4 <- summarise(group_by(item12.dat1, BuildingType)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item12.merge2 <- left_join(item12.sum3, item12.sum4, by = c("BuildingType", "HomeYearBuilt_bins4"))


item12.final <- rbind.data.frame(item12.merge1, item12.merge2, stringsAsFactors = F)

#calculate percent and SE
item12.final$Percent <- item12.final$Count / item12.final$TotalCount
item12.final$SE <- sqrt(item12.final$Percent * (1 - item12.final$Percent) / item12.final$SampleSize)

##cast data
item12.cast <- dcast(setDT(item12.final),
                        formula   = BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins,
                        value.var = c("Percent", "SE", "SampleSize"))
SampleSize <- rbind.data.frame(item12.sum2, item12.sum4, stringsAsFactors = F)

item12.table <- left_join(item12.cast, SampleSize, by = c("BuildingType", "HomeYearBuilt_bins4"))

#join all insulation levels onto rvalue summary
item12.table.SF <- data.frame("BuildingType" = item12.table$BuildingType
                            ,"Housing.Vintage" = item12.table$HomeYearBuilt_bins4
                            ,"Percent.R0" = item12.table$Percent_R0
                            ,"SE.R0" = item12.table$SE_R0
                            ,"Percent.R1.R10" = item12.table$Percent_R1.R10
                            ,"SE.R1.R10" = item12.table$SE_R1.R10
                            ,"Percent.R11.R16" = item12.table$Percent_R11.R16
                            ,"SE.R11.R16" = item12.table$SE_R11.R16
                            ,"Percent.R17.R22" = item12.table$Percent_R17.R22
                            ,"SE.R17.R22" = item12.table$SE_R17.R22
                            ,"Percent.RGT22" = item12.table$Percent_RGT22
                            ,"SE.RGT22" = item12.table$SE_RGT22
                            ,"SampleSize" = item12.table$SampleSize)

item12.table.SF1 <- item12.table.SF[which(item12.table.SF$BuildingType == "Single Family"),]

############################################################################################################
## For manufactured homes
############################################################################################################

item12.MH.dat <- item10.dat6

#merge weighted u values onto cleaned RBSA data
item12.MH.dat0 <- left_join(item12.MH.dat, rbsa.dat, by = "CK_Cadmus_ID")
item12.MH.dat1 <- item12.MH.dat0[which(!(is.na(item12.MH.dat0$HomeYearBuiltXX))),]


# Bin R values -- SF only
item12.MH.dat1$rvalue.bins <- "Unknown"
item12.MH.dat1$rvalue.bins[which(item12.MH.dat1$aveRval >= 0  & item12.MH.dat1$aveRval <  9)]  <- "R0.R8"
item12.MH.dat1$rvalue.bins[which(item12.MH.dat1$aveRval >= 9  & item12.MH.dat1$aveRval < 15)]  <- "R9.R14"
item12.MH.dat1$rvalue.bins[which(item12.MH.dat1$aveRval >= 15 & item12.MH.dat1$aveRval < 22)]  <- "R15.R21"
item12.MH.dat1$rvalue.bins[which(item12.MH.dat1$aveRval >= 22 & item12.MH.dat1$aveRval < 31)]  <- "R22.R30"
unique(item12.MH.dat1$rvalue.bins)

item12.MH.dat1$count <- 1
#summarise by vintage bins
#summarise by r value bins
item12.MH.sum1 <- summarise(group_by(item12.MH.dat1, BuildingType, HomeYearBuilt_bins4, rvalue.bins)
                         ,Count = sum(count))
#summarise across r value bins
item12.MH.sum2 <- summarise(group_by(item12.MH.dat1, BuildingType, HomeYearBuilt_bins4)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item12.MH.merge1 <- left_join(item12.MH.sum1, item12.MH.sum2, by = c("BuildingType", "HomeYearBuilt_bins4"))

#summarise across wall frame types
#summarise by r value bins
item12.MH.sum3 <- summarise(group_by(item12.MH.dat1, BuildingType, rvalue.bins)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,Count = sum(count))
#summarise across r value bins
item12.MH.sum4 <- summarise(group_by(item12.MH.dat1, BuildingType)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item12.MH.merge2 <- left_join(item12.MH.sum3, item12.MH.sum4, by = c("BuildingType", "HomeYearBuilt_bins4"))


item12.MH.final <- rbind.data.frame(item12.MH.merge1, item12.MH.merge2, stringsAsFactors = F)

item12.MH.final$Percent <- item12.MH.final$Count / item12.MH.final$TotalCount
item12.MH.final$SE <- sqrt(item12.MH.final$Percent * (1 - item12.MH.final$Percent) / item12.MH.final$SampleSize)

##cast data
item12.MH.cast <- dcast(setDT(item12.MH.final),
                      formula   = BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins,
                      value.var = c("Percent", "SE", "SampleSize"))
SampleSize <- rbind.data.frame(item12.sum2, item12.sum4, stringsAsFactors = F)

item12.MH.table <- left_join(item12.MH.cast, SampleSize, by = c("BuildingType", "HomeYearBuilt_bins4"))

#join all insulation levels onto rvalue summary
item12.MH.table1 <- data.frame("BuildingType"    = item12.MH.table$BuildingType
                              ,"Housing.Vintage" = item12.MH.table$HomeYearBuilt_bins4
                              ,"Percent.R0.R8"   = item12.MH.table$Percent_R0.R8
                              ,"SE.R0.R8"        = item12.MH.table$SE_R0.R8
                              ,"Percent.R9.R14"  = item12.MH.table$Percent_R9.R14
                              ,"SE.R9.R14"       = item12.MH.table$SE_R9.R14
                              ,"Percent.R15.R21" = item12.MH.table$Percent_R15.R21
                              ,"SE.R15.R21"      = item12.MH.table$SE_R15.R21
                              ,"Percent.R22.R30" = item12.MH.table$Percent_R22.R30
                              ,"SE.R22.R30"      = item12.MH.table$SE_R22.R30
                              ,"SampleSize"      = item12.MH.table$SampleSize)

item12.MH.table2 <- item12.MH.table1[which(item12.MH.table1$BuildingType == "Manufactured"),]
View(item12.MH.table2)






#############################################################################################
# Item 13: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, IDAHO  (SF table 20)
#############################################################################################
## Note: For this table, you must run up to item12.dat1 for the cleaned data
item13.dat <- item12.dat1[which(item12.dat1$State == "ID"),]

#summarise by vintage bins
#summarise by r value bins
item13.sum1 <- summarise(group_by(item13.dat, BuildingType, HomeYearBuilt_bins4, rvalue.bins)
                         ,Count = sum(count))
#summarise across r value bins
item13.sum2 <- summarise(group_by(item13.dat, BuildingType, HomeYearBuilt_bins4)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item13.merge1 <- left_join(item13.sum1, item13.sum2, by = c("BuildingType", "HomeYearBuilt_bins4"))

#summarise across wall frame types
#summarise by r value bins
item13.sum3 <- summarise(group_by(item13.dat, BuildingType, rvalue.bins)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,Count = sum(count))
#summarise across r value bins
item13.sum4 <- summarise(group_by(item13.dat, BuildingType)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item13.merge2 <- left_join(item13.sum3, item13.sum4, by = c("BuildingType", "HomeYearBuilt_bins4"))


item13.final <- rbind.data.frame(item13.merge1, item13.merge2, stringsAsFactors = F)

#calculate percent and SE
item13.final$Percent <- item13.final$Count / item13.final$TotalCount
item13.final$SE <- sqrt(item13.final$Percent * (1 - item13.final$Percent) / item13.final$SampleSize)

##cast data
item13.cast <- dcast(setDT(item13.final),
                     formula   = BuildingType + HomeYearBuilt_bins4 ~ rvalue.bins,
                     value.var = c("Percent", "SE", "SampleSize"))
SampleSize <- rbind.data.frame(item13.sum2, item13.sum4, stringsAsFactors = F)

item13.table <- left_join(item13.cast, SampleSize, by = c("BuildingType", "HomeYearBuilt_bins4"))

#join all insulation levels onto rvalue summary
item13.table.SF <- data.frame("BuildingType" = item13.table$BuildingType
                              ,"Housing.Vintage" = item13.table$HomeYearBuilt_bins4
                              ,"Percent.R0" = item13.table$Percent_R0
                              ,"SE.R0" = item13.table$SE_R0
                              ,"Percent.R1.R10" = item13.table$Percent_R1.R10
                              ,"SE.R1.R10" = item13.table$SE_R1.R10
                              ,"Percent.R11.R16" = item13.table$Percent_R11.R16
                              ,"SE.R11.R16" = item13.table$SE_R11.R16
                              ,"Percent.R17.R22" = item13.table$Percent_R17.R22
                              ,"SE.R17.R22" = item13.table$SE_R17.R22
                              ,"Percent.RGT22" = item13.table$Percent_RGT22
                              ,"SE.RGT22" = item13.table$SE_RGT22
                              ,"SampleSize" = item13.table$SampleSize)

item13.table.SF1 <- item13.table.SF[which(item13.table.SF$BuildingType == "Single Family"),]










#############################################################################################
# Item 14: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, MONTANA  (SF table 21)
#############################################################################################
## Note: For this table, you must run up to item12.dat1 for the cleaned data
item14.dat <- item12.dat1[which(item12.dat1$State == "MT"),]

#summarise by vintage bins
#summarise by r value bins
item14.sum1 <- summarise(group_by(item14.dat, BuildingType, HomeYearBuilt_bins4, rvalue.bins)
                         ,Count = sum(count))
#summarise across r value bins
item14.sum2 <- summarise(group_by(item14.dat, BuildingType, HomeYearBuilt_bins4)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item14.merge1 <- left_join(item14.sum1, item14.sum2, by = c("BuildingType", "HomeYearBuilt_bins4"))

#summarise across wall frame types
#summarise by r value bins
item14.sum3 <- summarise(group_by(item14.dat, BuildingType, rvalue.bins)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,Count = sum(count))
#summarise across r value bins
item14.sum4 <- summarise(group_by(item14.dat, BuildingType)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item14.merge2 <- left_join(item14.sum3, item14.sum4, by = c("BuildingType", "HomeYearBuilt_bins4"))


item14.final <- rbind.data.frame(item14.merge1, item14.merge2, stringsAsFactors = F)

#calculate percent and SE
item14.final$Percent <- item14.final$Count / item14.final$TotalCount
item14.final$SE <- sqrt(item14.final$Percent * (1 - item14.final$Percent) / item14.final$SampleSize)

##cast data
item14.cast <- dcast(setDT(item14.final),
                     formula   = BuildingType + HomeYearBuilt_bins4 ~ rvalue.bins,
                     value.var = c("Percent", "SE", "SampleSize"))
SampleSize <- rbind.data.frame(item14.sum2, item14.sum4, stringsAsFactors = F)

item14.table <- left_join(item14.cast, SampleSize, by = c("BuildingType", "HomeYearBuilt_bins4"))

#join all insulation levels onto rvalue summary
item14.table1 <- data.frame("BuildingType" = item14.table$BuildingType
                              ,"Housing.Vintage" = item14.table$HomeYearBuilt_bins4
                              ,"Percent.R0" = 0#item14.table$Percent_R0
                              ,"SE.R0" = 0#item14.table$SE_R0
                              ,"Percent.R1.R10" = item14.table$Percent_R1.R10
                              ,"SE.R1.R10" = item14.table$SE_R1.R10
                              ,"Percent.R11.R16" = item14.table$Percent_R11.R16
                              ,"SE.R11.R16" = item14.table$SE_R11.R16
                              ,"Percent.R17.R22" = item14.table$Percent_R17.R22
                              ,"SE.R17.R22" = item14.table$SE_R17.R22
                              ,"Percent.RGT22" = item14.table$Percent_RGT22
                              ,"SE.RGT22" = item14.table$SE_RGT22
                              ,"SampleSize" = item14.table$SampleSize)

item14.table.SF1 <- item14.table1[which(item14.table1$BuildingType == "Single Family"),]







#############################################################################################
# Item 15: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, OREGON  (SF table 22)
#############################################################################################
## Note: For this table, you must run up to item12.dat1 for the cleaned data
item15.dat <- item12.dat1[which(item12.dat1$State == "OR"),]

#summarise by vintage bins
#summarise by r value bins
item15.sum1 <- summarise(group_by(item15.dat, BuildingType, HomeYearBuilt_bins4, rvalue.bins)
                         ,Count = sum(count))
#summarise across r value bins
item15.sum2 <- summarise(group_by(item15.dat, BuildingType, HomeYearBuilt_bins4)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item15.merge1 <- left_join(item15.sum1, item15.sum2, by = c("BuildingType", "HomeYearBuilt_bins4"))

#summarise across wall frame types
#summarise by r value bins
item15.sum3 <- summarise(group_by(item15.dat, BuildingType, rvalue.bins)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,Count = sum(count))
#summarise across r value bins
item15.sum4 <- summarise(group_by(item15.dat, BuildingType)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item15.merge2 <- left_join(item15.sum3, item15.sum4, by = c("BuildingType", "HomeYearBuilt_bins4"))


item15.final <- rbind.data.frame(item15.merge1, item15.merge2, stringsAsFactors = F)

#calculate percent and SE
item15.final$Percent <- item15.final$Count / item15.final$TotalCount
item15.final$SE <- sqrt(item15.final$Percent * (1 - item15.final$Percent) / item15.final$SampleSize)

##cast data
item15.cast <- dcast(setDT(item15.final),
                     formula   = BuildingType + HomeYearBuilt_bins4 ~ rvalue.bins,
                     value.var = c("Percent", "SE", "SampleSize"))
SampleSize <- rbind.data.frame(item15.sum2, item15.sum4, stringsAsFactors = F)

item15.table <- left_join(item15.cast, SampleSize, by = c("BuildingType", "HomeYearBuilt_bins4"))

#join all insulation levels onto rvalue summary
item15.table1 <- data.frame("BuildingType" = item15.table$BuildingType
                            ,"Housing.Vintage" = item15.table$HomeYearBuilt_bins4
                            ,"Percent.R0" = item15.table$Percent_R0
                            ,"SE.R0" = item15.table$SE_R0
                            ,"Percent.R1.R10" = item15.table$Percent_R1.R10
                            ,"SE.R1.R10" = item15.table$SE_R1.R10
                            ,"Percent.R11.R16" = item15.table$Percent_R11.R16
                            ,"SE.R11.R16" = item15.table$SE_R11.R16
                            ,"Percent.R17.R22" = item15.table$Percent_R17.R22
                            ,"SE.R17.R22" = item15.table$SE_R17.R22
                            ,"Percent.RGT22" = item15.table$Percent_RGT22
                            ,"SE.RGT22" = item15.table$SE_RGT22
                            ,"SampleSize" = item15.table$SampleSize)

item15.table2 <- item15.table1[which(item15.table1$BuildingType == "Single Family"),]









#############################################################################################
# Item 16: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, WASHINGTON  (SF table 23)
#############################################################################################
# Note: For this table, you must run up to item12.dat1 for the cleaned data
item16.dat <- item12.dat1[which(item12.dat1$State == "WA"),]

#summarise by vintage bins
#summarise by r value bins
item16.sum1 <- summarise(group_by(item16.dat, BuildingType, HomeYearBuilt_bins4, rvalue.bins)
                         ,Count = sum(count))
#summarise across r value bins
item16.sum2 <- summarise(group_by(item16.dat, BuildingType, HomeYearBuilt_bins4)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item16.merge1 <- left_join(item16.sum1, item16.sum2, by = c("BuildingType", "HomeYearBuilt_bins4"))

#summarise across wall frame types
#summarise by r value bins
item16.sum3 <- summarise(group_by(item16.dat, BuildingType, rvalue.bins)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,Count = sum(count))
#summarise across r value bins
item16.sum4 <- summarise(group_by(item16.dat, BuildingType)
                         , HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,TotalCount = sum(count))
#merge
item16.merge2 <- left_join(item16.sum3, item16.sum4, by = c("BuildingType", "HomeYearBuilt_bins4"))


item16.final <- rbind.data.frame(item16.merge1, item16.merge2, stringsAsFactors = F)

#calculate percent and SE
item16.final$Percent <- item16.final$Count / item16.final$TotalCount
item16.final$SE <- sqrt(item16.final$Percent * (1 - item16.final$Percent) / item16.final$SampleSize)

##cast data
item16.cast <- dcast(setDT(item16.final),
                     formula   = BuildingType + HomeYearBuilt_bins4 ~ rvalue.bins,
                     value.var = c("Percent", "SE", "SampleSize"))
SampleSize <- rbind.data.frame(item16.sum2, item16.sum4, stringsAsFactors = F)

item16.table <- left_join(item16.cast, SampleSize, by = c("BuildingType", "HomeYearBuilt_bins4"))

#join all insulation levels onto rvalue summary
item16.table1 <- data.frame("BuildingType" = item16.table$BuildingType
                            ,"Housing.Vintage" = item16.table$HomeYearBuilt_bins4
                            ,"Percent.R0" = item16.table$Percent_R0
                            ,"SE.R0" = item16.table$SE_R0
                            ,"Percent.R1.R10" = item16.table$Percent_R1.R10
                            ,"SE.R1.R10" = item16.table$SE_R1.R10
                            ,"Percent.R11.R16" = item16.table$Percent_R11.R16
                            ,"SE.R11.R16" = item16.table$SE_R11.R16
                            ,"Percent.R17.R22" = item16.table$Percent_R17.R22
                            ,"SE.R17.R22" = item16.table$SE_R17.R22
                            ,"Percent.RGT22" = item16.table$Percent_RGT22
                            ,"SE.RGT22" = item16.table$SE_RGT22
                            ,"SampleSize" = item16.table$SampleSize)

item16.table2 <- item16.table1[which(item16.table1$BuildingType == "Single Family"),]










