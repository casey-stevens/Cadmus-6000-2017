#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

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
item10.dat1.1 <- item10.dat0[which(item10.dat0$Wall.Cavity.Insulation.Thickness.1 != "Unknown"),]
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

#Clean Condition unknown values
item10.dat3$Wall.Cavity.Insulation.Condition.1[which(item10.dat3$Wall.Cavity.Insulation.Condition.1 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.2[which(item10.dat3$Wall.Cavity.Insulation.Condition.2 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.3[which(item10.dat3$Wall.Cavity.Insulation.Condition.3 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.1[which(item10.dat3$Wall.Cavity.Insulation.Condition.1 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.2[which(item10.dat3$Wall.Cavity.Insulation.Condition.2 == "Unknown")] <- "100%"
item10.dat3$Wall.Cavity.Insulation.Condition.3[which(item10.dat3$Wall.Cavity.Insulation.Condition.3 == "Unknown")] <- "100%"

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

#replace any inches that are NA with zeros
for(i in 28:33){
  item10.dat4[,i] <- ifelse(is.na(item10.dat4[,i]), 0, item10.dat4[,i])
}


#replace any rvalues that are NA with zeros
for(i in 28:39){
  item10.dat4[,i] <- as.numeric(as.character(item10.dat4[,i]))
}

item10.dat5 <- item10.dat4
item10.dat5$cavity.rvalues1 <- as.numeric(as.character(item10.dat5$cavity.rvalues1))
item10.dat5$cavity.rvalues2 <- as.numeric(as.character(item10.dat5$cavity.rvalues2))
item10.dat5$exterior.rvalues1 <- as.numeric(as.character(item10.dat5$exterior.rvalues1))


#check uniques
unique(item10.dat4$cavity.rvalues1)
unique(item10.dat4$cavity.rvalues2) #only NA values here
unique(item10.dat4$exterior.rvalues1)



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

# replace inf with 0
item10.dat5$uvalue[which(item10.dat5$uvalue == "Inf")] <- 0

#make area numeric
item10.dat5$uvalue <- as.numeric(as.character(item10.dat5$uvalue))
item10.dat5$Wall.Area <- as.numeric(as.character(item10.dat5$Wall.Area))

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(item10.dat5, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * as.numeric(as.character(uvalue))) / sum(Wall.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

wall.unique <- unique(item10.dat5[which(colnames(item10.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item10.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item10.dat7 <- left_join(item10.dat6, rbsa.dat, by = "CK_Cadmus_ID")

item10.dat8 <- item10.dat7[which(!(is.na(item10.dat7$aveUval))),]


#Bin R values -- SF only
item10.dat7$rvalue.bins <- "Unknown"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval == 0)] <- "R0"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval > 0  & item10.dat7$aveRval < 11)]  <- "R1.R10"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >= 11 & item10.dat7$aveRval < 17)]  <- "R11.R16"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >= 17 & item10.dat7$aveRval < 23)]  <- "R17.R22"
item10.dat7$rvalue.bins[which(item10.dat7$aveRval >= 22)] <- "RGT22"
unique(item10.dat7$rvalue.bins)

item10.dat7$count <- 1

item10.dat8 <- item10.dat7[which(item10.dat7$rvalue.bins != "Unknown"),]

#summarise by wall frame types
#summarise by r value bins
item10.sum1 <- summarise(group_by(item10.dat8, BuildingType, Wall.Type, rvalue.bins)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))
#summarise across r value bins
item10.sum2 <- summarise(group_by(item10.dat8, BuildingType, Wall.Type)
                         ,rvalue.bins = "All Insulation Levels"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))


#summarise across wall frame types
#summarise by r value bins
item10.sum3 <- summarise(group_by(item10.dat8, BuildingType, rvalue.bins)
                         , Wall.Type = "All Frame Types"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))
#summarise across r value bins
item10.sum4 <- summarise(group_by(item10.dat8, BuildingType)
                         ,rvalue.bins = "All Insulation Levels"
                         , Wall.Type = "All Frame Types"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item10.merge <- rbind.data.frame(item10.sum1,item10.sum2,item10.sum3,item10.sum4,stringsAsFactors = F)

item10.tot.counts <- rbind.data.frame(item10.sum2, item10.sum4, stringsAsFactors = F)
item10.tot.counts <- item10.tot.counts[which(colnames(item10.tot.counts) %in% c("BuildingType"
                                                                                , "Wall.Type"
                                                                                , "Count"))]

item10.merge1 <- left_join(item10.merge, item10.tot.counts, by = c("BuildingType", "Wall.Type"))
colnames(item10.merge1) <- c("BuildingType", "Wall.Type", "rvalue.bins", "SampleSize", "Count", "TotalCount")

building.type <- rbind.data.frame("Manufactured"
                                  , "Multifamily - High Rise"
                                  , "Multifamily - Low Rise"
                                  , "Single Family"
                                  , stringsAsFactors = F)
i=1
for(i in 1:4){
  item10.merge1$TotalCount[which(item10.merge1$BuildingType == building.type[i,] & item10.merge1$rvalue.bins == "All Insulation Levels")] <-
    item10.merge1$TotalCount[which(item10.merge1$BuildingType == building.type[i,] & item10.merge1$rvalue.bins == "All Insulation Levels" & item10.merge1$Wall.Type == "All Frame Types")]
}

item10.final <- item10.merge1

item10.final$Percent <- item10.final$Count / item10.final$TotalCount
item10.final$SE <- sqrt(item10.final$Percent * (1 - item10.final$Percent) / item10.final$SampleSize)

##cast data
item10.table <- dcast(setDT(item10.final),
                      formula   = BuildingType +  Wall.Type ~ rvalue.bins,
                      value.var = c("Percent", "SE", "SampleSize"))

#join all insulation levels onto rvalue summary
item10.table1 <- data.frame("BuildingType" = item10.table$BuildingType
                            ,"Wall.Type" = item10.table$Wall.Type
                            ,"Percent.R0" = item10.table$Percent_R0
                            ,"SE.R0" = item10.table$SE_R0
                            ,"Percent.R1.R10" = item10.table$Percent_R1.R10
                            ,"SE.R1.R10" = item10.table$SE_R1.R10
                            ,"Percent.R11.R16" = item10.table$Percent_R11.R16
                            ,"SE.R11.R16" = item10.table$SE_R11.R16
                            ,"Percent.R17.R22" = item10.table$Percent_R17.R22
                            ,"SE.R17.R22" = item10.table$SE_R17.R22
                            ,"Percent.RGT22" = item10.table$Percent_RGT22
                            ,"SE.RGT22" = item10.table$SE_RGT22
                            ,"Percent_All Insulation Levels" = item10.table$`Percent_All Insulation Levels`
                            ,"SE_All Insulation Levels"   = item10.table$`SE_All Insulation Levels`
                            ,"SampleSize" = item10.table$`SampleSize_All Insulation Levels`)

item10.table2 <- item10.table1[which(item10.table1$BuildingType == "Single Family"),]


######################################################################################################
# Previously built code
# produces slightly different estimates
# can't validate this is correct
######################################################################################################
# ##cast data
# item10.dat7$count <- 1
# item10.dat.cast <- dcast(setDT(item10.dat7),
#                          formula   = CK_Cadmus_ID + BuildingType +  Wall.Type ~ rvalue.bins, sum,
#                          value.var = 'count')
# 
# for (i in 3:length(item10.dat.cast)){
#   item10.dat.cast[i][which(is.na(item10.dat.cast[i])),] <- 0
# }
# 
# head(item10.dat.cast)
# 
# 
# ## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
# item10.SF.dat <- subset(item10.dat.cast, item10.dat.cast$BuildingType == "Single Family")
# 
# #summarize by frame type
# item10.sum1 <- summarise(group_by(item10.SF.dat, BuildingType, Wall.Type)
#                         ,sampleSize      = length(unique(CK_Cadmus_ID))
#                         ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
#                         ,r0.percent      = sum(R0) / sampleSizeNoNA ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
#                         ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
#                         ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
#                         ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
#                         ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
#                         ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
#                         ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
#                         ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
#                         ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
#                         ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA)
# )
# # sum across frame types
# item10.sum2 <- summarise(group_by(item10.SF.dat, BuildingType)
#                          ,Wall.Type = "All Frame Types"
#                          ,sampleSize      = length(unique(CK_Cadmus_ID))
#                          ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
#                          ,r0.percent      = sum(R0) / sampleSizeNoNA ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
#                          ,r0.se           = sd(R0) / sqrt(sampleSizeNoNA)
#                          ,r1.r10.percent  = sum(R1.R10)  / sampleSizeNoNA
#                          ,r1.r10.se       = sd(R1.R10) / sqrt(sampleSizeNoNA)
#                          ,r11.r16.percent = sum(R11.R16) / sampleSizeNoNA
#                          ,r11.r16.se      = sd(R11.R16) / sqrt(sampleSizeNoNA)
#                          ,r17.r22.percent = sum(R17.R22) / sampleSizeNoNA
#                          ,r17.r22.se      = sd(R17.R22) / sqrt(sampleSizeNoNA)
#                          ,rGT22.percent   = sum(RGT22) / sampleSizeNoNA
#                          ,rGT22.se        = sd(RGT22) / sqrt(sampleSizeNoNA))
# 
# item10.frame.types <- rbind.data.frame(item10.sum1, item10.sum2, stringsAsFactors = F)
# 
# 
# item10.SF.dat$count <- 1
# #across R levels, by frame type
# item10.sum.allLevels1 <- summarise(group_by(item10.SF.dat, BuildingType, Wall.Type)
#                                   ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
#                                   ,WallTypeCount = sum(count)
#                                   ,TotalCount = sum(item10.SF.dat$count)
#                                   ,AllInsulationLevelPercent = WallTypeCount / TotalCount
#                                   ,AllInsulationSE = sqrt((AllInsulationLevelPercent * (1 - AllInsulationLevelPercent)) / WallTypeCount)
# )
# #across R levels, across frame types
# item10.sum.allLevels2 <- summarise(group_by(item10.SF.dat, BuildingType)
#                                    ,Wall.Type = "All Frame Types"
#                                   ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
#                                   ,WallTypeCount = sum(count)
#                                   ,TotalCount = sum(item10.SF.dat$count)
#                                   ,AllInsulationLevelPercent = WallTypeCount / TotalCount
#                                   ,AllInsulationSE = sqrt((AllInsulationLevelPercent * (1 - AllInsulationLevelPercent)) / WallTypeCount)
# )
# item10.sum.allLevels <- rbind.data.frame(item10.sum.allLevels1, item10.sum.allLevels2, stringsAsFactors = F)
# 
# #Check to make sure they add to 2
# sum(item10.sum.allLevels$AllInsulationLevelPercent)
# 
# #join all insulation levels onto rvalue summary
# item10.final.XXX <- data.frame("BuildingType" = item10.frame.types$BuildingType
#                             ,"Wall.Type" = item10.frame.types$Wall.Type
#                             ,"Percent.R0" = item10.frame.types$r0.percent
#                             ,"SE.R0" = item10.frame.types$r0.se
#                             ,"Percent.R1.R10" = item10.frame.types$r1.r10.percent
#                             ,"SE.R1.R10" = item10.frame.types$r1.r10.se
#                             ,"Percent.R11.R16" = item10.frame.types$r11.r16.percent
#                             ,"SE.R11.R16" = item10.frame.types$r11.r16.se
#                             ,"Percent.R17.R22" = item10.frame.types$r17.r22.percent
#                             ,"SE.R17.R22" = item10.frame.types$r17.r22.se
#                             ,"Percent.RGT22" = item10.frame.types$rGT22.percent
#                             ,"SE.RGT22" = item10.frame.types$rGT22.se
#                             ,"Percent_All Insulation Levels" = item10.sum.allLevels$AllInsulationLevelPercent
#                             ,"SE_All Insulation Levels"   = item10.sum.allLevels$AllInsulationSE
#                             ,"SampleSize" = item10.sum.allLevels$sampleSizeNoNA)
# 
# 








#############################################################################################
# Item 11: DISTRIBUTION OF WALL FRAMING TYPES BY VINTAGE (SF table 18)
#############################################################################################
## Note: For this table, you must run up to item10.dat3 for the cleaned data
item11.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Wall.Type"))]
item11.dat1 <- item11.dat[which(!(is.na(item11.dat$Wall.Type))),]

item11.dat2 <- left_join(item11.dat1, rbsa.dat, by = c("CK_Cadmus_ID"))

#remove unneccesary wall types
item11.dat3 <- item11.dat2[which(!(item11.dat2$Wall.Type %in% c("Masonry","Masonry (Basement)","Log","Adiabatic"))),]

#create "Alternative" category
item11.dat3$Wall.Type[which(item11.dat3$Wall.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
length(unique(item11.dat3$CK_Cadmus_ID))#506
unique(item11.dat3$Wall.Type)

                                                               
#cast out by wall frame types
item11.dat3$count <- 1
item11.cast <- dcast(setDT(item11.dat3),
                     formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ Wall.Type, sum,
                     value.var = 'count')

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
# Item 12: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE  (SF table 19)
#############################################################################################
## Note: For this table, you must run up to item10.dat5 for the cleaned data
item12.dat <- item10.dat8

############################################################################################################
## For single family
############################################################################################################

item12.dat$count <- 1
item12.SF.dat.cast <- dcast(setDT(item12.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item12.SF.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item12.SF.dat <- subset(item12.SF.dat.cast, item12.SF.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item12.SF.sum <- summarise(group_by(item12.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,r0.percent      = sum(R0) / SampleSize 
                        ,r0.se           = sd(R0) / sqrt(SampleSize)
                        ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                        ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                        ,r11.r16.percent = sum(R11.R16) / SampleSize
                        ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                        ,r17.r22.percent = sum(R17.R22) / SampleSize
                        ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                        ,rGT22.percent   = sum(RGT22) / SampleSize
                        ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)

item12.SF.sum1 <- summarise(group_by(item12.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         ,r0.percent      = sum(R0) / SampleSize
                         ,r0.se           = sd(R0) / sqrt(SampleSize)
                         ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                         ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                         ,r11.r16.percent = sum(R11.R16) / SampleSize
                         ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                         ,r17.r22.percent = sum(R17.R22) / SampleSize
                         ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                         ,rGT22.percent   = sum(RGT22) / SampleSize
                         ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)


#join all insulation levels onto rvalue summary
item12.SF.final <- rbind.data.frame(item12.SF.sum, item12.SF.sum1, stringsAsFactors = F)
item12.SF.final$check  <- item12.SF.final$r0.percent + item12.SF.final$r1.r10.percent + item12.SF.final$r11.r16.percent + item12.SF.final$r17.r22.percent + item12.SF.final$rGT22.percent


item12.SF.table <- data.frame("BuildingType" = item12.SF.final$BuildingType
                              ,"Housing.Vintage" = item12.SF.final$HomeYearBuilt_bins4
                              ,"Percent_R0" = item12.SF.final$r0.percent
                              ,"SE_R0" = item12.SF.final$r0.se
                              ,"Percent_R1_R10" = item12.SF.final$r1.r10.percent
                              ,"SE_R1_R10" = item12.SF.final$r1.r10.se
                              ,"Percent_R11_R16" = item12.SF.final$r11.r16.percent
                              ,"SE_R11_R16" = item12.SF.final$r11.r16.se
                              ,"Percent_R17_R22" = item12.SF.final$r17.r22.percent
                              ,"SE_R17_R22" = item12.SF.final$r17.r22.se
                              ,"Percent_RGT22" = item12.SF.final$rGT22.percent
                              ,"SE_RGT22" = item12.SF.final$rGT22.se
                              ,"SampleSize" = item12.SF.final$SampleSize)
item12.SF.table1 <- item12.SF.table[which(!(is.na(item12.SF.table$Housing.Vintage))),]

############################################################################################################
## For manufactured homes
############################################################################################################

item12.MH.dat <- item10.dat6

#merge weighted u values onto cleaned RBSA data
item12.MH.dat2 <- left_join(item12.MH.dat, rbsa.dat, by = "CK_Cadmus_ID")


# Bin R values -- SF only
item12.MH.dat2$rvalue.bins <- "Unknown"
item12.MH.dat2$rvalue.bins[which(item12.MH.dat2$aveRval >= 0  & item12.MH.dat2$aveRval < 9)]  <- "R0.R8"
item12.MH.dat2$rvalue.bins[which(item12.MH.dat2$aveRval >= 9 & item12.MH.dat2$aveRval < 15)]  <- "R9.R14"
item12.MH.dat2$rvalue.bins[which(item12.MH.dat2$aveRval >= 15 & item12.MH.dat2$aveRval < 22)]  <- "R15.R21"
item12.MH.dat2$rvalue.bins[which(item12.MH.dat2$aveRval >= 22 & item12.MH.dat2$aveRval < 31)]  <- "R22.R30"
unique(item12.MH.dat2$rvalue.bins)


item12.MH.dat2$count <- 1
item12.MH.dat.cast <- dcast(setDT(item12.MH.dat2),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item12.MH.dat.cast)


## Manufactured Homes
item12.MH.dat <- subset(item12.MH.dat.cast, item12.MH.dat.cast$BuildingType == "Manufactured")

#summarize
item12.MH.sum <- summarise(group_by(item12.MH.dat, BuildingType, HomeYearBuilt_bins4)
                           ,sampleSize  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                           ,r1.r8.percent  = sum(R0.R8)  / sampleSize
                           ,r1.r8.se       = sd(R0.R8) / sqrt(sampleSize)
                           ,r9.r14.percent = sum(R9.R14) / sampleSize
                           ,r9.r14.se      = sd(R9.R14) / sqrt(sampleSize)
                           ,r15.r21.percent = sum(R15.R21) / sampleSize
                           ,r15.r21.se      = sd(R15.R21) / sqrt(sampleSize)
                           ,r22.r30.percent = sum(R22.R30) / sampleSize
                           ,r22.r30.se      = sd(R22.R30) / sqrt(sampleSize)
)

item12.MH.sum1 <- summarise(group_by(item12.MH.dat, BuildingType)
                            ,HomeYearBuilt_bins4 = "All Vintages"
                            ,sampleSize  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                            ,r1.r8.percent  = sum(R0.R8)  / sampleSize
                            ,r1.r8.se       = sd(R0.R8) / sqrt(sampleSize)
                            ,r9.r14.percent = sum(R9.R14) / sampleSize
                            ,r9.r14.se      = sd(R9.R14) / sqrt(sampleSize)
                            ,r15.r21.percent = sum(R15.R21) / sampleSize
                            ,r15.r21.se      = sd(R15.R21) / sqrt(sampleSize)
                            ,r22.r30.percent = sum(R22.R30) / sampleSize
                            ,r22.r30.se      = sd(R22.R30) / sqrt(sampleSize)
)


#join all insulation levels onto rvalue summary
item12.MH.final <- rbind.data.frame(item12.MH.sum, item12.MH.sum1, stringsAsFactors = F)
check  <- item12.MH.final$r1.r8.percent +
  item12.MH.final$r9.r14.percent +
  item12.MH.final$r15.r21.percent +
  item12.MH.final$r22.r30.percent



item12.MH.table <- data.frame("BuildingType" = item12.MH.final$BuildingType
                              ,"Housing.Vintage" = item12.MH.final$HomeYearBuilt_bins4
                              ,"Percent_R0_R8" = item12.MH.final$r1.r8.percent
                              ,"SE_R0_R8" = item12.MH.final$r1.r8.se
                              ,"Percent_R9_R14" = item12.MH.final$r9.r14.percent
                              ,"SE_R9_R14" = item12.MH.final$r9.r14.se
                              ,"Percent_R15_R21" = item12.MH.final$r15.r21.percent
                              ,"SE_R15_R21" = item12.MH.final$r15.r21.se
                              ,"Percent_R22_R30" = item12.MH.final$r22.r30.percent
                              ,"SE_R22_R30" = item12.MH.final$r22.r30.se
                              ,"SampleSize" = item12.MH.final$sampleSize)
item12.MH.table1 <- item12.MH.table[which(!(is.na(item12.MH.table$Housing.Vintage))),]








#############################################################################################
# Item 13: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, IDAHO  (SF table 20)
#############################################################################################
## Note: For this table, you must run up to item12.SF.dat2 for the cleaned data
item13.dat <- item12.dat[which(item12.dat$State == "ID"),]


item13.dat.cast <- dcast(setDT(item13.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item13.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item13.SF.dat <- subset(item13.dat.cast, item13.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item13.sum <- summarise(group_by(item13.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,SampleSize  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                        ,r0.percent      = sum(R0) / SampleSize 
                        ,r0.se           = sd(R0) / sqrt(SampleSize)
                        ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                        ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                        ,r11.r16.percent = sum(R11.R16) / SampleSize
                        ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                        ,r17.r22.percent = sum(R17.R22) / SampleSize
                        ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                        ,rGT22.percent   = sum(RGT22) / SampleSize
                        ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)

item13.sum1 <- summarise(group_by(item13.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         ,SampleSize  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
                         ,r0.percent      = sum(R0) / SampleSize
                         ,r0.se           = sd(R0) / sqrt(SampleSize)
                         ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                         ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                         ,r11.r16.percent = sum(R11.R16) / SampleSize
                         ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                         ,r17.r22.percent = sum(R17.R22) / SampleSize
                         ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                         ,rGT22.percent   = sum(RGT22) / SampleSize
                         ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)


#join all insulation levels onto rvalue summary
item13.final <- rbind.data.frame(item13.sum, item13.sum1, stringsAsFactors = F)
item13.final$check  <- item13.final$r0.percent + item13.final$r1.r10.percent + item13.final$r11.r16.percent + item13.final$r17.r22.percent + item13.final$rGT22.percent


item13.table <- data.frame("BuildingType" = item13.final$BuildingType
                              ,"Housing.Vintage" = item13.final$HomeYearBuilt_bins4
                              ,"Percent_R0" = item13.final$r0.percent
                              ,"SE_R0" = item13.final$r0.se
                              ,"Percent_R1_R10" = item13.final$r1.r10.percent
                              ,"SE_R1_R10" = item13.final$r1.r10.se
                              ,"Percent_R11_R16" = item13.final$r11.r16.percent
                              ,"SE_R11_R16" = item13.final$r11.r16.se
                              ,"Percent_R17_R22" = item13.final$r17.r22.percent
                              ,"SE_R17_R22" = item13.final$r17.r22.se
                              ,"Percent_RGT22" = item13.final$rGT22.percent
                              ,"SE_RGT22" = item13.final$rGT22.se
                              ,"SampleSize" = item13.final$SampleSize)
item13.table1 <- item13.table[which(!(is.na(item13.table$Housing.Vintage))),]










#############################################################################################
# Item 14: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, MONTANA  (SF table 21)
#############################################################################################
## Note: For this table, you must run up to item12.dat2 for the cleaned data
item14.dat <- item12.dat[which(item12.dat$State == "MT"),]


item14.dat.cast <- dcast(setDT(item14.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item14.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item14.SF.dat <- subset(item14.dat.cast, item14.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item14.sum <- summarise(group_by(item14.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        # ,r0.percent      = sum(R0) / SampleSize
                        # ,r0.se           = sd(R0) / sqrt(SampleSize)
                        ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                        ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                        ,r11.r16.percent = sum(R11.R16) / SampleSize
                        ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                        ,r17.r22.percent = sum(R17.R22) / SampleSize
                        ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                        ,rGT22.percent   = sum(RGT22) / SampleSize
                        ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)

item14.sum1 <- summarise(group_by(item14.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         # ,r0.percent      = sum(R0) / SampleSize
                         # ,r0.se           = sd(R0) / sqrt(SampleSize)
                         ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                         ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                         ,r11.r16.percent = sum(R11.R16) / SampleSize
                         ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                         ,r17.r22.percent = sum(R17.R22) / SampleSize
                         ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                         ,rGT22.percent   = sum(RGT22) / SampleSize
                         ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)


#join all insulation levels onto rvalue summary
item14.final <- rbind.data.frame(item14.sum, item14.sum1, stringsAsFactors = F)
# item14.final$check  <- item14.final$r0.percent + item14.final$r1.r10.percent + item14.final$r11.r16.percent + item14.final$r17.r22.percent + item14.final$rGT22.percent



item14.table <- data.frame("BuildingType" = item14.final$BuildingType
                           ,"Housing.Vintage" = item14.final$HomeYearBuilt_bins4
                           # ,"Percent_R0" = item14.final$r0.percent
                           # ,"SE_R0" = item14.final$r0.se
                           ,"Percent_R1_R10" = item14.final$r1.r10.percent
                           ,"SE_R1_R10" = item14.final$r1.r10.se
                           ,"Percent_R11_R16" = item14.final$r11.r16.percent
                           ,"SE_R11_R16" = item14.final$r11.r16.se
                           ,"Percent_R17_R22" = item14.final$r17.r22.percent
                           ,"SE_R17_R22" = item14.final$r17.r22.se
                           ,"Percent_RGT22" = item14.final$rGT22.percent
                           ,"SE_RGT22" = item14.final$rGT22.se
                           ,"SampleSize" = item14.final$SampleSize)
item14.table1 <- item14.table[which(!(is.na(item14.table$Housing.Vintage))),]







#############################################################################################
# Item 15: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, OREGON  (SF table 22)
#############################################################################################
## Note: For this table, you must run up to item12.dat2 for the cleaned data
item15.dat <- item12.dat[which(item12.dat$State == "OR"),]


item15.dat.cast <- dcast(setDT(item15.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item15.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item15.SF.dat <- subset(item15.dat.cast, item15.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item15.sum <- summarise(group_by(item15.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,r0.percent      = sum(R0) / SampleSize 
                        ,r0.se           = sd(R0) / sqrt(SampleSize)
                        ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                        ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                        ,r11.r16.percent = sum(R11.R16) / SampleSize
                        ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                        ,r17.r22.percent = sum(R17.R22) / SampleSize
                        ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                        ,rGT22.percent   = sum(RGT22) / SampleSize
                        ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)

item15.sum1 <- summarise(group_by(item15.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         ,r0.percent      = sum(R0) / SampleSize
                         ,r0.se           = sd(R0) / sqrt(SampleSize)
                         ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                         ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                         ,r11.r16.percent = sum(R11.R16) / SampleSize
                         ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                         ,r17.r22.percent = sum(R17.R22) / SampleSize
                         ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                         ,rGT22.percent   = sum(RGT22) / SampleSize
                         ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)


#join all insulation levels onto rvalue summary
item15.final <- rbind.data.frame(item15.sum, item15.sum1, stringsAsFactors = F)
item15.final$check  <- item15.final$r0.percent + item15.final$r1.r10.percent + item15.final$r11.r16.percent + item15.final$r17.r22.percent + item15.final$rGT22.percent


item15.table <- data.frame("BuildingType" = item15.final$BuildingType
                           ,"Housing.Vintage" = item15.final$HomeYearBuilt_bins4
                           ,"Percent_R0" = item15.final$r0.percent
                           ,"SE_R0" = item15.final$r0.se
                           ,"Percent_R1_R10" = item15.final$r1.r10.percent
                           ,"SE_R1_R10" = item15.final$r1.r10.se
                           ,"Percent_R11_R16" = item15.final$r11.r16.percent
                           ,"SE_R11_R16" = item15.final$r11.r16.se
                           ,"Percent_R17_R22" = item15.final$r17.r22.percent
                           ,"SE_R17_R22" = item15.final$r17.r22.se
                           ,"Percent_RGT22" = item15.final$rGT22.percent
                           ,"SE_RGT22" = item15.final$rGT22.se
                           ,"SampleSize" = item15.final$SampleSize)
item15.table1 <- item15.table[which(!(is.na(item15.table$Housing.Vintage))),]









#############################################################################################
# Item 16: DISTRIBUTION OF WALL INSULATION LEVELS BY HOME VINTAGE, WASHINGTON  (SF table 23)
#############################################################################################
## Note: For this table, you must run up to item12.dat2 for the cleaned data
item16.dat <- item12.dat[which(item12.dat$State == "WA"),]


item16.dat.cast <- dcast(setDT(item16.dat),
                         formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
                         value.var = 'count')
head(item16.dat.cast)


## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
item16.SF.dat <- subset(item16.dat.cast, item16.dat.cast$BuildingType == "Single Family")

#summarize --SF only
item16.sum <- summarise(group_by(item16.SF.dat, BuildingType, HomeYearBuilt_bins4)
                        ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                        ,r0.percent      = sum(R0) / SampleSize 
                        ,r0.se           = sd(R0) / sqrt(SampleSize)
                        ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                        ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                        ,r11.r16.percent = sum(R11.R16) / SampleSize
                        ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                        ,r17.r22.percent = sum(R17.R22) / SampleSize
                        ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                        ,rGT22.percent   = sum(RGT22) / SampleSize
                        ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)

item16.sum1 <- summarise(group_by(item16.SF.dat, BuildingType)
                         ,HomeYearBuilt_bins4 = "All Vintages"
                         ,SampleSize      = sum(length(unique(CK_Cadmus_ID)))
                         ,r0.percent      = sum(R0) / SampleSize
                         ,r0.se           = sd(R0) / sqrt(SampleSize)
                         ,r1.r10.percent  = sum(R1.R10)  / SampleSize
                         ,r1.r10.se       = sd(R1.R10) / sqrt(SampleSize)
                         ,r11.r16.percent = sum(R11.R16) / SampleSize
                         ,r11.r16.se      = sd(R11.R16) / sqrt(SampleSize)
                         ,r17.r22.percent = sum(R17.R22) / SampleSize
                         ,r17.r22.se      = sd(R17.R22) / sqrt(SampleSize)
                         ,rGT22.percent   = sum(RGT22) / SampleSize
                         ,rGT22.se        = sd(RGT22) / sqrt(SampleSize)
)


#join all insulation levels onto rvalue summary
item16.final <- rbind.data.frame(item16.sum, item16.sum1, stringsAsFactors = F)
item16.final$check  <- item16.final$r0.percent + item16.final$r1.r10.percent + item16.final$r11.r16.percent + item16.final$r17.r22.percent + item16.final$rGT22.percent


item16.table <- data.frame("BuildingType" = item16.final$BuildingType
                           ,"Housing.Vintage" = item16.final$HomeYearBuilt_bins4
                           ,"Percent_R0" = item16.final$r0.percent
                           ,"SE_R0" = item16.final$r0.se
                           ,"Percent_R1_R10" = item16.final$r1.r10.percent
                           ,"SE_R1_R10" = item16.final$r1.r10.se
                           ,"Percent_R11_R16" = item16.final$r11.r16.percent
                           ,"SE_R11_R16" = item16.final$r11.r16.se
                           ,"Percent_R17_R22" = item16.final$r17.r22.percent
                           ,"SE_R17_R22" = item16.final$r17.r22.se
                           ,"Percent_RGT22" = item16.final$rGT22.percent
                           ,"SE_RGT22" = item16.final$rGT22.se
                           ,"SampleSize" = item16.final$SampleSize)
item16.table1 <- item16.table[which(!(is.na(item16.table$Housing.Vintage))),]










