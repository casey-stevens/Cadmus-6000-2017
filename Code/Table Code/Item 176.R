#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]


#############################################################################################
#Item 176: DISTRIBUTION OF FLOOR U-VALUE BY STATE (MH TABLE 19)
#############################################################################################
#subset envelope data to necessary columns
item176.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Floor.Type"
                                                               , "Floor.Sub-Type"
                                                               , "Floor.Area"
                                                               , "Floor.Insulated?"
                                                               , "Floor.Insulation.Type.1"
                                                               , "Floor.Insulation.Thickness.1"
                                                               , "Floor.Insulation.Type.2"                                                  
                                                               , "Floor.Insulation.Thickness.2"
                                                               # , "Floor.Insulation.Type.3"                                                  
                                                               # , "Floor.Insulation.Thickness.3"
                                                               , "Slab.Insulated?"
                                                               , "Slab.Insulation.Type.1"
                                                               , "Slab.Insulation.Thickness.1"
                                                               , "Slab.Insulation.Type.2"                                                  
                                                               , "Slab.Insulation.Thickness.2"
                                                               # , "Slab.Insulation.Type.3"                                                  
                                                               # , "Slab.Insulation.Thickness.3"
))]

unique(item176.dat$Floor.Insulation.Type.1)
unique(item176.dat$Floor.Insulation.Type.2)
unique(item176.dat$Floor.Insulation.Type.3) #nothing in this column
unique(item176.dat$Slab.Insulation.Type.1) #nothing in this column
unique(item176.dat$Slab.Insulation.Type.2) #nothing in this column
unique(item176.dat$Slab.Insulation.Type.3) #nothing in this column

#subset to only wall information
item176.dat1 <- item176.dat[which(item176.dat$Category == "Floor"),]

#remove unneccesary floor types
item176.dat2 <- left_join(rbsa.dat, item176.dat1, by = "CK_Cadmus_ID")

#remove items have the datapoint was not asked for
item176.dat3 <- item176.dat2[which(item176.dat2$`Floor.Insulated?` %in% c("Yes", "No")),]
length(unique(item176.dat3$CK_Cadmus_ID))#258
unique(item176.dat3$`Floor.Insulated?`)


###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
unique(item176.dat3$Floor.Insulation.Thickness.1)
item176.dat3$Floor.Insulation.Thickness.1[which(item176.dat3$Floor.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
item176.dat3$Floor.Insulation.Thickness.1[which(item176.dat3$Floor.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
item176.dat3$Floor.Insulation.Thickness.1[which(item176.dat3$Floor.Insulation.Thickness.1 == "-- Datapoint not asked for --")] <- "N/A N/A"
item176.dat3$Floor.Insulation.Thickness.1[which(is.na(item176.dat3$Floor.Insulation.Thickness.1))] <- "N/A N/A"
item176.dat3$Floor.Insulation.Thickness.2[which(item176.dat3$Floor.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
item176.dat3$Floor.Insulation.Thickness.2[which(item176.dat3$Floor.Insulation.Thickness.2 == "200024390821")] <- "Unknown Unknown"
item176.dat3$Floor.Insulation.Thickness.2[which(item176.dat3$Floor.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
item176.dat3$Floor.Insulation.Thickness.2[which(item176.dat3$Floor.Insulation.Thickness.2 == "-- Datapoint not asked for --")] <- "N/A N/A"
item176.dat3$Floor.Insulation.Thickness.2[which(is.na(item176.dat3$Floor.Insulation.Thickness.2))] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
item176.dat3$count <- 1
item176.dat3$TMP_ID <- cumsum(item176.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(item176.dat3$Floor.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = item176.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item176.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(item176.dat3$Floor.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = item176.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = item176.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
item176.dat4 <- as.data.frame(left_join(item176.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                             , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
item176.dat4$inches1 <- as.numeric(as.character(item176.dat4$V1.x)) # warning here is OK
item176.dat4$inches2 <- as.numeric(as.character(item176.dat4$V1.y)) # warning here is OK

item176.dat4$rvalues1 <- item176.dat4$Floor.Insulation.Type.1
item176.dat4$rvalues2 <- item176.dat4$Floor.Insulation.Type.2

#check uniques
unique(item176.dat4$rvalues1)
unique(item176.dat4$rvalues2)

#fix names that are not in R value table
item176.dat4$rvalues1[which(item176.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
item176.dat4$rvalues1[which(item176.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
item176.dat4$rvalues1[which(item176.dat4$rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
item176.dat4$rvalues2[which(item176.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
item176.dat4$rvalues2[which(item176.dat4$rvalues2 == "N/A")]                              <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  item176.dat4$rvalues1[which(item176.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  item176.dat4$rvalues2[which(item176.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

item176.dat5 <- item176.dat4

###########################
# Cleaning step (No Insulation to zero)
###########################
item176.dat5$rvalues1[which(item176.dat5$`Floor.Insulated?` == "No")] <- 0
item176.dat5$rvalues2[which(item176.dat5$`Floor.Insulated?` == "No")] <- 0
item176.dat5$inches1[which(item176.dat5$`Floor.Insulated?` == "No")] <- 0
item176.dat5$inches2[which(item176.dat5$`Floor.Insulated?` == "No")] <- 0
###########################
# End Cleaning step
###########################
item176.dat5$rvalues1 <- as.numeric(as.character(item176.dat5$rvalues1))
item176.dat5$rvalues2 <- as.numeric(as.character(item176.dat5$rvalues2))


#identify which rows that do not contain NAs for any rvalues
Non_NA_ind <- which(!(is.na(item176.dat5$rvalues2)))






###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################

#create total.r.value column
item176.dat5$total.r.val <- NA


#calculate the weighted r value where wall cavity insulation type is not NA
for (i in Non_NA_ind){
  item176.dat5$total.r.val[i] <- (item176.dat5$rvalues1[i] * item176.dat5$inches1[i]) +  
    (item176.dat5$rvalues2[i] * item176.dat5$inches2[i])
}


#calculate the weighted r value where wall cavity insulation type is NA
item176.dat5$total.r.val[which(is.na(item176.dat5$total.r.val))] <- 
  (item176.dat5$rvalues1[which(is.na(item176.dat5$total.r.val))] *
     item176.dat5$inches1[which(is.na(item176.dat5$total.r.val))])

#check -- NOTE -- NONE SHOULD BE NA
unique(item176.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
item176.dat5$ufactor <- 1 / item176.dat5$total.r.val

# replace inf with 0
item176.dat5$ufactor[which(item176.dat5$ufactor == "Inf")] <- 0

#make area numeric
item176.dat5$ufactor <- as.numeric(as.character(item176.dat5$ufactor))
item176.dat5$Floor.Area <- as.numeric(as.character(item176.dat5$Floor.Area))

#weight the u factor per home
weightedU <- summarise(group_by(item176.dat5, CK_Cadmus_ID, Floor.Type)
                       ,aveUval = sum(Floor.Area * ufactor) / sum(Floor.Area)
)

###########################
# End Analysis
###########################


wall.unique <- unique(item176.dat5[which(colnames(item176.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

item176.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item176.dat7 <- left_join(item176.dat6, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))

item176.dat8 <- item176.dat7[which(!(is.na(item176.dat7$aveUval))),]

#summarise by state
item176.state <- summarise(group_by(item176.dat8, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(aveUval)
                           ,SE = sd(aveUval) / sqrt(SampleSize))
# summarise by region
item176.region <- summarise(group_by(item176.dat8, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(aveUval)
                            ,SE = sd(aveUval) / sqrt(SampleSize))
#merge together
item176.final <- rbind.data.frame(item176.state, item176.region, stringsAsFactors = F)

item176.table <- data.frame("BuildingType" = item176.final$BuildingType
                            ,"State" = item176.final$State
                            ,"Mean" = item176.final$Mean
                            ,"SE" = item176.final$SE
                            ,"SampleSize" = item176.final$SampleSize)

item176.table1 <- item176.table[which(item176.table$BuildingType == "Manufactured"),]
