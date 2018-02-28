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
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]


#############################################################################################
#Item 175: DISTRIBUTION OF WALL U-VALUE BY STATE (MH TABLE 17)
#############################################################################################
#subset envelope data to necessary columns
item175.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Wall.Type"
                                                               , "Wall.Area"
                                                               , "Wall.Framing.Size"
                                                               , "Wall.Cavity.Insulated?"
                                                               , "Wall.Cavity.Insulation.Type.1"
                                                               , "Wall.Cavity.Insulation.Thickness.1"
                                                               , "Wall.Cavity.Insulation.Type.2"                                                  
                                                               , "Wall.Cavity.Insulation.Thickness.2"
                                                               , "Wall.Exterior.Insulated?"
                                                               , "Wall.Exterior.Insulation.Type.1"
                                                               , "Wall.Exterior.Insulation.Thickness.1"
                                                               , "Wall.Exterior.Insulation.Type.2"                                                  
                                                               , "Wall.Exterior.Insulation.Thickness.2"))]
length(unique(item175.dat$CK_Cadmus_ID))#584

item175.dat0 <- item175.dat[which(item175.dat$`Wall.Cavity.Insulated?` %in% c("Yes", "No")),]
item175.dat1 <- item175.dat0[which(item175.dat0$`Wall.Exterior.Insulated?` %in% c("Yes", "No")),]
item175.dat1.1 <- item175.dat1[which(item175.dat1$Wall.Cavity.Insulation.Thickness.1 != "Unknown"),]
item175.dat1.2 <- item175.dat1.1[-which(item175.dat1.1$Wall.Exterior.Insulation.Thickness.1 == "Unknown"),]

unique(item175.dat1.2$Wall.Cavity.Insulation.Type.1)
unique(item175.dat1.2$Wall.Cavity.Insulation.Type.2)
unique(item175.dat1.2$Wall.Cavity.Insulation.Type.3) #nothing in this column
unique(item175.dat1.2$Wall.Exterior.Insulation.Type.1)
unique(item175.dat1.2$Wall.Exterior.Insulation.Type.2) #nothing in this column
unique(item175.dat1.2$Wall.Exterior.Insulation.Type.3) #nothing in this column

unique(item175.dat1.2$Wall.Cavity.Insulation.Thickness.1)
unique(item175.dat1.2$Wall.Cavity.Insulation.Thickness.2)
unique(item175.dat1.2$Wall.Exterior.Insulation.Thickness.1)

#remove unneccesary wall types
item175.dat2 <- item175.dat1.2[which(!(item175.dat1.2$Wall.Type %in% c("Masonry","Masonry (Basement)","Log","Adiabatic"))),]

#create "Alternative" category
item175.dat2$Wall.Type[which(item175.dat2$Wall.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
length(unique(item175.dat2$CK_Cadmus_ID))#473
unique(item175.dat2$Wall.Type)

#remove items have the datapoint was not asked for
item175.dat3 <- item175.dat2[which(item175.dat2$Wall.Framing.Size != "-- Datapoint not asked for --"),]
length(unique(item175.dat3$CK_Cadmus_ID))#368
unique(item175.dat3$Wall.Framing.Size)

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
#cleaning for wall.cavity
item175.dat3$Wall.Cavity.Insulation.Thickness.1[which(item175.dat3$Wall.Cavity.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item175.dat3$Wall.Cavity.Insulation.Thickness.1[which(item175.dat3$Wall.Cavity.Insulation.Thickness.1 == "-- Datapoint not asked for --")] <- "N/A N/A"
item175.dat3$Wall.Cavity.Insulation.Thickness.1[which(is.na(item175.dat3$Wall.Cavity.Insulation.Thickness.1))] <- "N/A N/A"
item175.dat3$Wall.Cavity.Insulation.Thickness.2[which(item175.dat3$Wall.Cavity.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item175.dat3$Wall.Cavity.Insulation.Thickness.2[which(item175.dat3$Wall.Cavity.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item175.dat3$Wall.Cavity.Insulation.Thickness.2[which(item175.dat3$Wall.Cavity.Insulation.Thickness.2 == "-- Datapoint not asked for --")] <- "N/A N/A"
item175.dat3$Wall.Cavity.Insulation.Thickness.2[which(is.na(item175.dat3$Wall.Cavity.Insulation.Thickness.2))] <- "N/A N/A"
unique(item175.dat3$Wall.Cavity.Insulation.Thickness.1)
unique(item175.dat3$Wall.Cavity.Insulation.Thickness.2)

#cleaning for wall exterior
item175.dat3$Wall.Exterior.Insulation.Thickness.1[which(item175.dat3$Wall.Exterior.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item175.dat3$Wall.Exterior.Insulation.Thickness.1[which(item175.dat3$Wall.Exterior.Insulation.Thickness.1 == "-- Datapoint not asked for --")] <- "N/A N/A"
item175.dat3$Wall.Exterior.Insulation.Thickness.1[which(is.na(item175.dat3$Wall.Exterior.Insulation.Thickness.1))] <- "N/A N/A"
unique(item175.dat3$Wall.Exterior.Insulation.Thickness.1)

# add new ID variable for merging -- don't know if we need this
item175.dat3$count <- 1
item175.dat3$TMP_ID <- cumsum(item175.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item175.dat3$Wall.Cavity.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item175.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item175.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item175.dat3$Wall.Cavity.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item175.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item175.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul3 <- unlist(strsplit(item175.dat3$Wall.Exterior.Insulation.Thickness.1, " "))
clean.insul3 <- as.data.frame(matrix(clean.insul3, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul3.1 <- cbind.data.frame("CK_Cadmus_ID" = item175.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item175.dat3$TMP_ID
                                   , clean.insul3)
dim(clean.insul3.1)

clean.insul4 <- unlist(strsplit(item175.dat3$Wall.Cavity.Insulation.Thickness.2, " "))
clean.insul4.1 <- cbind.data.frame("CK_Cadmus_ID" = item175.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item175.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul4, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul4.1)

clean.insul.join1 <- left_join(clean.insul1.1,    clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join2 <- left_join(clean.insul.join1, clean.insul3.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join3 <- left_join(clean.insul.join2, clean.insul4.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
item175.dat4 <- as.data.frame(left_join(item175.dat3, clean.insul.join3, by = c("CK_Cadmus_ID", "TMP_ID"))
                             , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item175.dat4$inches1 <- as.numeric(as.character(item175.dat4$V1.x)) # warning here is OK
item175.dat4$inches2 <- as.numeric(as.character(item175.dat4$V1.y)) # warning here is OK
item175.dat4$inches3 <- as.numeric(as.character(item175.dat4$V1.x.x)) # warning here is OK

item175.dat4$rvalues1 <- item175.dat4$Wall.Cavity.Insulation.Type.1
item175.dat4$rvalues2 <- item175.dat4$Wall.Cavity.Insulation.Type.2
item175.dat4$rvalues3 <- item175.dat4$Wall.Exterior.Insulation.Type.1

#fix names that are not in R value table
item175.dat4$rvalues1[which(item175.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item175.dat4$rvalues1[which(item175.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item175.dat4$rvalues1[which(item175.dat4$rvalues1 == "-- Datapoint not asked for --")]    <- NA
item175.dat4$rvalues1[which(item175.dat4$rvalues1 == "None")]                             <- NA
item175.dat4$rvalues2[which(item175.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item175.dat4$rvalues2[which(item175.dat4$rvalues2 == "N/A")]                              <- NA
item175.dat4$rvalues2[which(item175.dat4$rvalues2 == "-- Datapoint not asked for --")]    <- NA
item175.dat4$rvalues3[which(item175.dat4$rvalues3 == "-- Datapoint not asked for --")]    <- NA
item175.dat4$rvalues3[which(item175.dat4$rvalues3 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
item175.dat4$rvalues3[which(item175.dat4$rvalues3 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item175.dat4$rvalues1[which(item175.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item175.dat4$rvalues2[which(item175.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item175.dat4$rvalues3[which(item175.dat4$rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################

item175.dat4$rvalues1[which(item175.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item175.dat4$rvalues2[which(item175.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item175.dat4$rvalues3[which(item175.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
item175.dat4$inches1[which(item175.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item175.dat4$inches2[which(item175.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
item175.dat4$inches3[which(item175.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0

item175.dat5 <- item175.dat4
item175.dat5$rvalues1 <- as.numeric(as.character(item175.dat5$rvalues1))
item175.dat5$rvalues2 <- as.numeric(as.character(item175.dat5$rvalues2))
item175.dat5$rvalues3 <- as.numeric(as.character(item175.dat5$rvalues3))


#check uniques
unique(item175.dat4$rvalues1)
unique(item175.dat4$rvalues2) #only NA values here
unique(item175.dat4$rvalues3)

#identify which rows that do not contain NAs for any rvalues
Non_NA_ind <- which(!(is.na(item175.dat5$rvalues2)))






###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################

#create total.r.value column
item175.dat5$total.r.val <- NA


#calculate the weighted r value where wall cavity insulation type is not NA
for (i in Non_NA_ind){
item175.dat5$total.r.val[i] <- (item175.dat5$rvalues1[i] * item175.dat5$inches1[i]) +  
  (item175.dat5$rvalues2[i] * item175.dat5$inches2[i]) + 
  (item175.dat5$rvalues3[i] * item175.dat5$inches3[i])
}


#calculate the weighted r value where wall cavity insulation type is NA
item175.dat5$total.r.val[which(is.na(item175.dat5$total.r.val))] <- 
  (item175.dat5$rvalues1[which(is.na(item175.dat5$total.r.val))] *
     item175.dat5$inches1[which(is.na(item175.dat5$total.r.val))]) +  
  (item175.dat5$rvalues3[which(is.na(item175.dat5$total.r.val))] * 
     item175.dat5$inches3[which(is.na(item175.dat5$total.r.val))])

#check -- NOTE -- NONE SHOULD BE NA
unique(item175.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item175.dat5$uvalue <- 1 / item175.dat5$total.r.val

# replace inf with 0
item175.dat5$uvalue[which(item175.dat5$uvalue == "Inf")] <- 0

#make area numeric
item175.dat5$uvalue <- as.numeric(as.character(item175.dat5$uvalue))
item175.dat5$Wall.Area <- as.numeric(as.character(item175.dat5$Wall.Area))

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(item175.dat5, CK_Cadmus_ID)
                       ,aveUval = sum(Wall.Area * as.numeric(as.character(uvalue))) / sum(Wall.Area)
)

###########################
# End Analysis
###########################


wall.unique <- unique(item175.dat5[which(colnames(item175.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item175.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item175.dat7 <- left_join(item175.dat6, rbsa.dat, by = "CK_Cadmus_ID")

item175.dat8 <- item175.dat7[which(!(is.na(item175.dat7$aveUval))),]

#summarise by state
item175.state <- summarise(group_by(item175.dat8, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(aveUval)
                           ,SE = sd(aveUval) / sqrt(SampleSize))
# summarise by region
item175.region <- summarise(group_by(item175.dat8, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(aveUval)
                            ,SE = sd(aveUval) / sqrt(SampleSize))
#merge together
item175.final <- rbind.data.frame(item175.state, item175.region, stringsAsFactors = F)

item175.table <- data.frame("BuildingType" = item175.final$BuildingType
                            ,"State" = item175.final$State
                            ,"Mean" = item175.final$Mean
                            ,"SE" = item175.final$SE
                            ,"SampleSize" = item175.final$SampleSize)

item175.table1 <- item175.table[which(item175.table$BuildingType == "Manufactured"),]
