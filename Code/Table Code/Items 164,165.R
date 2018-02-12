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

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]


#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item164.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item164.dat.11 <- item164.dat.1[which(item164.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item164.dat.12 <- item164.dat.11[which(item164.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item164.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item164.dat.12$Primary.Heating.System)
item164.dat.12$count <- 1

item164.dat.13 <- unique(item164.dat.12[which(item164.dat.12$Heating.Fuel == "Electric"),])

item164.sum1 <- summarise(group_by(item164.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item164.sum1$Count <- 1
which(duplicated(item164.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item164.sum1$Heating.Fuel)

item164.mechanical <- item164.sum1






#############################################################################################
# Similar to Item 26
#############################################################################################
#
#For Envelope information
#
#############################################################################################
#subset envelope data to necessary columns
prep.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                             ,"CK_SiteID"
                                                             ,"PK_Envelope_ID"
                                                             , "Category"
                                                             , "Ceiling.Type"
                                                             , "Ceiling.Sub-Type"
                                                             , "Ceiling.Area"
                                                             , "Ceiling.Insulated?"
                                                             , "Ceiling.Insulation.Type.1"
                                                             , "Ceiling.Insulation.Thickness.1"
                                                             , "Ceiling.Insulation.Condition.1"
                                                             , "Ceiling.Insulation.Type.2"                                                  
                                                             , "Ceiling.Insulation.Thickness.2"
                                                             , "Ceiling.Insulation.Condition.2"
                                                             , "Ceiling.Insulation.Type.3"
                                                             , "Ceiling.Insulation.Thickness.3"
                                                             , "Ceiling.Insulation.Condition.3"))]
prep.dat0 <- prep.dat[which(prep.dat$`Ceiling.Insulated?` %in% c("Yes", "No", "Datapoint not asked for")),]
prep.dat1.0 <- prep.dat0[which(!(is.na(as.numeric(as.character(prep.dat0$Ceiling.Area))))),]
prep.dat1.2 <- prep.dat1.0[which(prep.dat1.0$Ceiling.Insulation.Thickness.1 != "Unknown"),]

#assign new dataset
prep.dat3 <- prep.dat1.2

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

# when Ceiling or slab insulated columns = No, make condition 0%
prep.dat3$Ceiling.Insulation.Condition.1[which(prep.dat3$`Ceiling.Insulated?` == "No")] <- "0%"

# Make thickness columns numeric
for (i in grep("thickness", names(prep.dat3), ignore.case = T)){
  prep.dat3[,i] <- as.numeric(as.character(prep.dat3[,i]))
}

#######################################################
# Cleaning and re-naming inches and rvalue information
#######################################################
prep.dat4 <- prep.dat3
# make numeric
prep.dat4$ceiling.inches1 <- prep.dat4$Ceiling.Insulation.Thickness.1
prep.dat4$ceiling.inches2 <- prep.dat4$Ceiling.Insulation.Thickness.2
prep.dat4$ceiling.inches3 <- prep.dat4$Ceiling.Insulation.Thickness.3
#update column names
prep.dat4$ceiling.rvalues1 <- prep.dat4$Ceiling.Insulation.Type.1
prep.dat4$ceiling.rvalues2 <- prep.dat4$Ceiling.Insulation.Type.2
prep.dat4$ceiling.rvalues3 <- prep.dat4$Ceiling.Insulation.Type.3

#replace any inches that are NA with zeros
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}

#fix names that are not in R value table
prep.dat4$ceiling.rvalues1[grep("extruded", prep.dat4$ceiling.rvalues1, ignore.case = T)] <- "Extruded polystyrene foam board"
prep.dat4$ceiling.rvalues1[grep("Expanded", prep.dat4$ceiling.rvalues1, ignore.case = T)] <- "Expanded polystyrene foam board"
prep.dat4$ceiling.rvalues1[grep("other|unknown", prep.dat4$ceiling.rvalues1, ignore.case = T)] <- "Unknown"
prep.dat4$ceiling.rvalues1[grep("wood", prep.dat4$ceiling.rvalues1, ignore.case = T)] <- "Wood shavings"
prep.dat4$ceiling.rvalues1[grep("rock", prep.dat4$ceiling.rvalues1, ignore.case = T)] <- "Rock wool"
unique(prep.dat4$ceiling.rvalues1)
prep.dat4$ceiling.rvalues2[grep("extruded", prep.dat4$ceiling.rvalues2, ignore.case = T)] <- "Extruded polystyrene foam board"
prep.dat4$ceiling.rvalues2[grep("Expanded", prep.dat4$ceiling.rvalues2, ignore.case = T)] <- "Expanded polystyrene foam board"
prep.dat4$ceiling.rvalues2[grep("other|unknown", prep.dat4$ceiling.rvalues2, ignore.case = T)] <- "Unknown"
prep.dat4$ceiling.rvalues2[grep("wood", prep.dat4$ceiling.rvalues2, ignore.case = T)] <- "Wood shavings"
prep.dat4$ceiling.rvalues2[grep("rock|wool", prep.dat4$ceiling.rvalues2, ignore.case = T)] <- "Rock Wool"
prep.dat4$ceiling.rvalues2[grep("None", prep.dat4$ceiling.rvalues2, ignore.case = T)] <- "0"
prep.dat4$ceiling.rvalues2[which(prep.dat4$ceiling.rvalues2 %in% c("Foil-faced polyiscyanurate foam board", "Foil-faced fiberglass insulation"))] <- "Foil-faced polyisocyanurate foam board"
unique(prep.dat4$ceiling.rvalues2)
prep.dat4$ceiling.rvalues3[which(prep.dat4$ceiling.rvalues3 %in% c("Foil-faced polyiscyanurate foam board", "Foil-faced fiberglass insulation"))] <- "Foil-faced polyisocyanurate foam board"

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  prep.dat4$ceiling.rvalues1[which(prep.dat4$ceiling.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$ceiling.rvalues2[which(prep.dat4$ceiling.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$ceiling.rvalues3[which(prep.dat4$ceiling.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################
unique(prep.dat4$ceiling.rvalues1)
unique(prep.dat4$ceiling.rvalues2)
unique(prep.dat4$ceiling.rvalues3)

prep.dat4$ceiling.rvalues1[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0
prep.dat4$ceiling.rvalues2[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0
prep.dat4$ceiling.rvalues3[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0

prep.dat4$ceiling.inches1[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0
prep.dat4$ceiling.inches2[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0
prep.dat4$ceiling.inches3[which(prep.dat4$`Ceiling.Insulated?` == "No")] <- 0

#replace any inches and rvalues that are NA with zeros
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}
#make all inches and rvalue columns numeric
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- as.numeric(as.character(prep.dat4[,i]))
}
#replace any inches and rvalues that are NA with zeros
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}
#make all inches and rvalue columns numeric
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- as.numeric(as.character(prep.dat4[,i]))
}

prep.dat4.5 <- prep.dat4

#create total.r.value column
prep.dat4.5$total.r.val <- NA

#check uniques -- None should be NA
unique(prep.dat4.5$ceiling.rvalues1)
unique(prep.dat4.5$ceiling.rvalues2)
unique(prep.dat4.5$ceiling.rvalues3)
unique(prep.dat4.5$ceiling.inches1)
unique(prep.dat4.5$ceiling.inches2)
unique(prep.dat4.5$ceiling.inches3)

prep.dat4.5$Ceiling.Insulation.Condition.1 <- as.numeric(as.character(prep.dat4.5$Ceiling.Insulation.Condition.1))

prep.condition.sub1 <- prep.dat4.5[which(prep.dat4.5$Ceiling.Insulation.Condition.1 %notin% c(1, NA, 0)),]
prep.condition.sub1$Ceiling.Insulation.Condition.1 <- 1 - prep.condition.sub1$Ceiling.Insulation.Condition.1
prep.condition.sub1$ceiling.rvalues1 <- 0
prep.condition.sub1$ceiling.rvalues2 <- 0
prep.condition.sub1$ceiling.rvalues3 <- 0
prep.condition.sub1$total.r.val <- NA

prep.dat5 <- rbind.data.frame(prep.dat4.5
                              ,prep.condition.sub1
                              , stringsAsFactors = F)

prep.dat5$Ceiling.Insulation.Condition.1[which(is.na(prep.dat5$Ceiling.Insulation.Condition.1))] <- 1 

###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################
#calculate the weighted r value
na.ind <- which(is.na(prep.dat5$total.r.val))
prep.dat5$total.r.val[na.ind] <- (prep.dat5$ceiling.rvalues1[na.ind] * prep.dat5$ceiling.inches1[na.ind]) +  
  (prep.dat5$ceiling.rvalues2[na.ind] * prep.dat5$ceiling.inches2[na.ind]) +  
  (prep.dat5$ceiling.rvalues3[na.ind] * prep.dat5$ceiling.inches3[na.ind]) 

#check -- NOTE -- NONE SHOULD BE NA
unique(prep.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
prep.dat5$uvalue <- 1 / (prep.dat5$total.r.val)
prep.dat5$uvalue[which(prep.dat5$uvalue == "Inf")] <- 1
unique(prep.dat5$uvalue)

#make area numeric
prep.dat5$uvalue       <- as.numeric(as.character(prep.dat5$uvalue))
prep.dat5$Ceiling.Area <- as.numeric(as.character(prep.dat5$Ceiling.Area))
names(prep.dat5)[which(names(prep.dat5) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"
#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID, Ceiling.Type)
                       ,aveUval = sum(Ceiling.Area * Ceiling.Insulation.Condition.1 * uvalue) / sum(Ceiling.Area * Ceiling.Insulation.Condition.1)
)

#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
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







#############################################################################################
# Item 164: DISTRIBUTION OF ATTIC INSULATION, ELECTRICALLY HEATED HOMES (SF table B-9)
#############################################################################################
item164.dat <- prep.dat7[which(prep.dat7$Ceiling.Type == "Attic"),]

item164.merge <- left_join(item164.mechanical, item164.dat)
item164.merge <- left_join(rbsa.dat, item164.merge)
item164.merge <- item164.merge[which(!is.na(item164.merge$aveRval)),]

item164.data <- weightedData(item164.merge[-which(colnames(item164.merge) %in% c("Ceiling.Type"
                                                                          ,"aveUval"
                                                                          ,"aveRval"
                                                                          ,"Heating.Fuel"
                                                                          ,"Count"))])
item164.data <- left_join(item164.data, item164.merge[which(colnames(item164.merge) %in% c("CK_Cadmus_ID"
                                                                                   ,"Ceiling.Type"
                                                                                   ,"aveUval"
                                                                                   ,"aveRval"
                                                                                   ,"Heating.Fuel"
                                                                                   ,"Count"))])




#Bin R values -- SF only
item164.data$rvalue.bins <- "Unknown"
item164.data$rvalue.bins[which(item164.data$aveRval == 0)] <- "R0"
item164.data$rvalue.bins[which(item164.data$aveRval >  0  & item164.data$aveRval < 11)]  <- "R1.R10"
item164.data$rvalue.bins[which(item164.data$aveRval >= 11 & item164.data$aveRval < 16)]  <- "R11.R15"
item164.data$rvalue.bins[which(item164.data$aveRval >= 16 & item164.data$aveRval < 21)]  <- "R16.R20"
item164.data$rvalue.bins[which(item164.data$aveRval >= 21 & item164.data$aveRval < 26)]  <- "R21.R25"
item164.data$rvalue.bins[which(item164.data$aveRval >= 26 & item164.data$aveRval < 31)]  <- "R26.R30"
item164.data$rvalue.bins[which(item164.data$aveRval >= 31 & item164.data$aveRval < 41)]  <- "R31.R40"
item164.data$rvalue.bins[which(item164.data$aveRval >= 41 & item164.data$aveRval < 51)]  <- "R41.R50"
item164.data$rvalue.bins[which(item164.data$aveRval >= 51)] <- "RGT50"
unique(item164.data$rvalue.bins)

item164.data$count <- 1

##############################
# Weighted Analysis
##############################
item164.final <- proportions_one_group(CustomerLevelData = item164.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = TRUE)
item164.final.SF <- item164.final[which(item164.final$BuildingType == "Single Family")
                                  ,-which(colnames(item164.final) %in% c("BuildingType"))]
exportTable(item164.final.SF, "SF", "Table B-9", weighted = TRUE)

##############################
# Unweighted Analysis
##############################
item164.final <- proportions_one_group(CustomerLevelData = item164.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = FALSE)
item164.final.SF <- item164.final[which(item164.final$BuildingType == "Single Family")
                                ,-which(colnames(item164.final) %in% c("BuildingType"))]
exportTable(item164.final.SF, "SF", "Table B-9", weighted = FALSE)









#############################################################################################
# Item 165: DISTRIBUTION OF VAULT CEILING INSULATION LEVEL, ELECTRICALLY HEATED HOMES (SF table B-10)
#############################################################################################
item165.dat <- prep.dat7[which(prep.dat7$Ceiling.Type == "Sloped / Vaulted (no attic)"),]

item165.merge <- left_join(item164.mechanical, item165.dat)
item165.merge <- left_join(rbsa.dat, item165.merge)
item165.merge <- item165.merge[which(!is.na(item165.merge$aveRval)),]

item165.data <- weightedData(item165.merge[-which(colnames(item165.merge) %in% c("Ceiling.Type"
                                                                                 ,"aveUval"
                                                                                 ,"aveRval"
                                                                                 ,"Heating.Fuel"
                                                                                 ,"Count"))])
item165.data <- left_join(item165.data, item165.merge[which(colnames(item165.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ceiling.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"Heating.Fuel"
                                                                                           ,"Count"))])

item165.data$count <- 1
#Bin R values -- SF only
item165.data$rvalue.bins <- "Unknown"
item165.data$rvalue.bins[which(item165.data$aveRval == 0)] <- "R0"
item165.data$rvalue.bins[which(item165.data$aveRval >  0 & item165.data$aveRval < 16)]   <- "R1.R15"
item165.data$rvalue.bins[which(item165.data$aveRval >= 16 & item165.data$aveRval < 21)]  <- "R16.R20"
item165.data$rvalue.bins[which(item165.data$aveRval >= 21 & item165.data$aveRval < 25)]  <- "R21.R25"
item165.data$rvalue.bins[which(item165.data$aveRval >= 25 & item165.data$aveRval < 31)]  <- "R26.R30"
item165.data$rvalue.bins[which(item165.data$aveRval >= 31 & item165.data$aveRval < 41)]  <- "R31.R40"
item165.data$rvalue.bins[which(item165.data$aveRval >= 41)]  <- "R41.R50"
unique(item165.data$rvalue.bins)


##############################
# Weighted Analysis
##############################
item165.final <- proportions_one_group(CustomerLevelData = item165.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = TRUE)
item165.final.SF <- item165.final[which(item165.final$BuildingType == "Single Family")
                                ,-which(colnames(item165.final) %in% c("BuildingType"))]
exportTable(item165.final.SF, "SF", "Table B-10", weighted = TRUE)

##############################
# Unweighted Analysis
##############################
item165.final <- proportions_one_group(CustomerLevelData = item165.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = FALSE)
item165.final.SF <- item165.final[which(item165.final$BuildingType == "Single Family")
                                ,-which(colnames(item165.final) %in% c("BuildingType"))]
exportTable(item165.final.SF, "SF", "Table B-10", weighted = FALSE)
