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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))

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
# This will be used for item 23
#
#
###################################################################################################################


#############################################################################################
# Item 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (SF table 30) (also need MH)
#############################################################################################
#subset envelope data to necessary columns
prep.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                             , "CK_SiteID"
                                                             , "PK_Envelope_ID"
                                                             , "Category"
                                                             , "Floor.Type"
                                                             , "Floor.Sub-Type"
                                                             , "Floor.Area"
                                                             , "Floor.Insulated?"
                                                             , "Floor.Insulation.Type.1"
                                                             , "Floor.Insulation.Thickness.1"
                                                             , "Floor.Insulation.Condition.1"
                                                             , "Floor.Insulation.Type.2"                                                  
                                                             , "Floor.Insulation.Thickness.2"
                                                             , "Floor.Insulation.Condition.2"
                                                             , "Floor.Insulation.Type.3"
                                                             , "Floor.Insulation.Thickness.3"
                                                             , "Floor.Insulation.Condition.3"
                                                             , "Slab.Insulated?"
                                                             , "Slab.Insulation.Type.1"
                                                             , "Slab.Insulation.Thickness.1"
                                                             , "Slab.Insulation.Condition.1"
                                                             , "Slab.Insulation.Type.2"                                                  
                                                             , "Slab.Insulation.Thickness.2"
                                                             , "Slab.Insulation.Condition.2"
                                                             , "Slab.Insulation.Type.3"
                                                             , "Slab.Insulation.Thickness.3"
                                                             , "Slab.Insulation.Condition.3"
))]

prep.dat0 <- prep.dat[which(prep.dat$`Floor.Insulated?` %in% c("Yes", "No")),]
unique(prep.dat0$`Floor.Insulated?`)

prep.dat0$`Slab.Insulated?`[which(prep.dat0$`Slab.Insulated?` != "Yes")] <- "No" ###treat anything not Yes as No

prep.dat1.0 <- prep.dat0[which(!is.na(as.numeric(as.character(prep.dat0$Floor.Area)))),]

prep.dat1.1 <- prep.dat1.0[which(prep.dat1.0$Floor.Insulation.Thickness.1 %notin% c("Unknown")),]
# prep.dat1.2 <- prep.dat1.1[-which(prep.dat1.1$Slab.Insulation.Thickness.1 == "Unknown"),]

#assign new dataset
prep.dat3 <- prep.dat1.1

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


# when floor or slab insulated columns = No, make condition 0%
prep.dat3$Floor.Insulation.Condition.1[which(prep.dat3$`Floor.Insulated?` == "No")] <- "0%"
prep.dat3$Slab.Insulation.Condition.1[which(prep.dat3$`Slab.Insulated?` == "No")] <- "0%"

# Make thickness columns numeric
for (i in grep("thickness", names(prep.dat3), ignore.case = T)){
  prep.dat3[,i] <- as.numeric(as.character(prep.dat3[,i]))
}

#######################################################
# Cleaning and re-naming inches and rvalue information
#######################################################
prep.dat4 <- prep.dat3
# make numeric
prep.dat4$floor.inches1 <- prep.dat4$Floor.Insulation.Thickness.1
prep.dat4$floor.inches2 <- prep.dat4$Floor.Insulation.Thickness.2
prep.dat4$floor.inches3 <- prep.dat4$Floor.Insulation.Thickness.3
prep.dat4$slab.inches1  <- prep.dat4$Slab.Insulation.Thickness.1
prep.dat4$slab.inches2  <- prep.dat4$Slab.Insulation.Thickness.2
prep.dat4$slab.inches3  <- prep.dat4$Slab.Insulation.Thickness.3
#update column names
prep.dat4$floor.rvalues1 <- prep.dat4$Floor.Insulation.Type.1
prep.dat4$floor.rvalues2 <- prep.dat4$Floor.Insulation.Type.2
prep.dat4$floor.rvalues3 <- prep.dat4$Floor.Insulation.Type.3
prep.dat4$slab.rvalues1  <- prep.dat4$Slab.Insulation.Type.1
prep.dat4$slab.rvalues2  <- prep.dat4$Slab.Insulation.Type.2
prep.dat4$slab.rvalues3  <- prep.dat4$Slab.Insulation.Type.3

#replace any inches that are NA with zeros
for(i in grep("inches|rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}

#fix names that are not in R value table
prep.dat4$floor.rvalues1[which(prep.dat4$floor.rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
prep.dat4$floor.rvalues2[which(prep.dat4$floor.rvalues2 == "N/A")]                              <- NA

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  prep.dat4$floor.rvalues1[which(prep.dat4$floor.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$floor.rvalues2[which(prep.dat4$floor.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$floor.rvalues3[which(prep.dat4$floor.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$slab.rvalues1[which(prep.dat4$slab.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$slab.rvalues2[which(prep.dat4$slab.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$slab.rvalues3[which(prep.dat4$slab.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################

prep.dat4$floor.rvalues1[which(prep.dat4$`Floor.Insulated?` == "No")] <- 0
prep.dat4$floor.rvalues2[which(prep.dat4$`Floor.Insulated?` == "No")] <- 0
prep.dat4$floor.rvalues3[which(prep.dat4$`Floor.Insulated?` == "No")] <- 0
prep.dat4$slab.rvalues1[which(prep.dat4$`Slab.Insulated?`   == "No")] <- 0
prep.dat4$slab.rvalues2[which(prep.dat4$`Slab.Insulated?`   == "No")] <- 0
prep.dat4$slab.rvalues3[which(prep.dat4$`Slab.Insulated?`   == "No")] <- 0

prep.dat4$floor.inches1[which(prep.dat4$`Floor.Insulated?`  == "No")] <- 0
prep.dat4$floor.inches2[which(prep.dat4$`Floor.Insulated?`  == "No")] <- 0
prep.dat4$floor.inches3[which(prep.dat4$`Floor.Insulated?`  == "No")] <- 0
prep.dat4$slab.inches1[which(prep.dat4$`Slab.Insulated?`    == "No")] <- 0
prep.dat4$slab.inches2[which(prep.dat4$`Slab.Insulated?`    == "No")] <- 0
prep.dat4$slab.inches3[which(prep.dat4$`Slab.Insulated?`    == "No")] <- 0

#replace any inches and rvalues that are NA with zeros
for(i in grep(".inches|.rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}
#make all inches and rvalue columns numeric
for(i in grep(".inches|.rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- as.numeric(as.character(prep.dat4[,i]))
}
#replace any inches and rvalues that are NA with zeros
for(i in grep(".inches|.rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}
#make all inches and rvalue columns numeric
for(i in grep(".inches|.rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- as.numeric(as.character(prep.dat4[,i]))
}

prep.dat4.5 <- prep.dat4

#create total.r.value column
prep.dat4.5$total.r.val <- NA


#check uniques -- None should be NA
unique(prep.dat4.5$floor.rvalues1)
unique(prep.dat4.5$floor.rvalues2)
unique(prep.dat4.5$floor.rvalues3)
unique(prep.dat4.5$slab.rvalues1)
unique(prep.dat4.5$slab.rvalues2)
unique(prep.dat4.5$slab.rvalues3)

prep.dat4.5$Floor.Insulation.Condition.1 <- as.numeric(as.character(prep.dat4.5$Floor.Insulation.Condition.1))
prep.dat4.5$Slab.Insulation.Condition.1 <- as.numeric(as.character(prep.dat4.5$Slab.Insulation.Condition.1))

# clean up condition information
prep.condition.sub1 <- prep.dat4.5[which(prep.dat4.5$Floor.Insulation.Condition.1 %notin% c(1, NA, 0)),]
prep.condition.sub1$Floor.Insulation.Condition.1 <- 1 - prep.condition.sub1$Floor.Insulation.Condition.1
prep.condition.sub1$floor.rvalues1 <- 0
prep.condition.sub1$floor.rvalues2 <- 0
prep.condition.sub1$floor.rvalues3 <- 0
prep.condition.sub1$total.r.val <- NA

# clean up condition information
prep.condition.sub2 <- prep.dat4.5[which(prep.dat4.5$Slab.Insulation.Condition.1 %notin% c(1, NA, 0)),]

prep.dat5 <- rbind.data.frame(prep.dat4.5
                              ,prep.condition.sub1
                              , stringsAsFactors = F)

###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################
#calculate the weighted r value
na.ind <- which(is.na(prep.dat5$total.r.val))
prep.dat5$total.r.val[na.ind] <- (prep.dat5$floor.rvalues1[na.ind] * prep.dat5$floor.inches1[na.ind]) +  
  (prep.dat5$floor.rvalues2[na.ind] * prep.dat5$floor.inches2[na.ind]) +  
  (prep.dat5$floor.rvalues3[na.ind] * prep.dat5$floor.inches3[na.ind]) + 
  (prep.dat5$slab.rvalues1[na.ind] * prep.dat5$slab.inches1[na.ind]) + 
  (prep.dat5$slab.rvalues2[na.ind] * prep.dat5$slab.inches2[na.ind]) + 
  (prep.dat5$slab.rvalues3[na.ind] * prep.dat5$slab.inches3[na.ind])

#check -- NOTE -- NONE SHOULD BE NA
unique(prep.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
prep.dat5$uvalue <- 1 / (prep.dat5$total.r.val)
prep.dat5$uvalue[which(prep.dat5$uvalue == "Inf")] <- 1
unique(prep.dat5$uvalue)

#make area numeric
prep.dat5$uvalue    <- as.numeric(as.character(prep.dat5$uvalue))
prep.dat5$Floor.Area <- as.numeric(as.character(prep.dat5$Floor.Area))
prep.dat5$Floor.Insulation.Condition.1[which(is.na(prep.dat5$Floor.Insulation.Condition.1))] <- 1

#weight the u factor per home -- where weights are the wall area within home
names(prep.dat5)[which(names(prep.dat5) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID)
                       ,aveUval = sum(Floor.Area * Floor.Insulation.Condition.1 * uvalue) / sum(Floor.Area * Floor.Insulation.Condition.1)
)

#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
unique(weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
Floor.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.dat6 <- left_join(weightedU, Floor.unique, by = "CK_Cadmus_ID")

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





rbsa.floor <- rbsa.dat[which(colnames(rbsa.dat) %in% c("CK_Building_ID","BuildingType","HomeYearBuilt"))]
floor.merge <- left_join(rbsa.floor, prep.dat5, by = c("CK_Building_ID" = "CK_SiteID"))
floor.merge <- floor.merge[which(!is.na(floor.merge$uvalue)),]
#########export rvalues
##  Write out confidence/precision info
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(floor.merge, paste(filepathCleaningDocs, "Insulation Exports", paste("Floor Insulation Values ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)











############################################################################################################
# ITEM 239: DISTRIBUTION OF FLOOR INSULATION LEVELS BY FLOOR TYPE (MF Table 31)
############################################################################################################
#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_SiteID, Floor.Type)
                       ,aveUval = sum(Floor.Area * Floor.Insulation.Condition.1 * uvalue) / sum(Floor.Area * Floor.Insulation.Condition.1)
)
unique(weightedU$aveUval)


#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
unique(weightedU$aveRval)
unique(weightedU$aveUval)

# get unique cadmus IDs and building types for this subset of data
Floor.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_SiteID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.dat6 <- left_join(weightedU, Floor.unique, by = "CK_SiteID")

#merge weighted u values onto cleaned RBSA data
prep.dat7 <- left_join(prep.dat6, rbsa.dat.MF, by = c("CK_SiteID" = "CK_Building_ID"))
prep.dat7 <- prep.dat7[grep("3 or fewer floors", prep.dat7$BuildingTypeXX),]
item239.dat <- prep.dat7[which(!is.na(prep.dat7$CK_Cadmus_ID)),]
unique(item239.dat$Floor.Type)

#Bin R values -- MF only
item239.dat$rvalue.bins <- "Unknown"
item239.dat$rvalue.bins[which(item239.dat$aveRval ==  0)] <- "None"
item239.dat$rvalue.bins[which(item239.dat$aveRval >  0   & item239.dat$aveRval < 4)]   <- "R0.R3"
item239.dat$rvalue.bins[which(item239.dat$aveRval >= 4   & item239.dat$aveRval < 11)]  <- "R4.R10"
item239.dat$rvalue.bins[which(item239.dat$aveRval >= 11  & item239.dat$aveRval < 16)]  <- "R11.R15"
item239.dat$rvalue.bins[which(item239.dat$aveRval >= 16  & item239.dat$aveRval < 23)]  <- "R16.R22"
item239.dat$rvalue.bins[which(item239.dat$aveRval >= 23  & item239.dat$aveRval < 28)]  <- "R23.R27"
item239.dat$rvalue.bins[which(item239.dat$aveRval >= 28  & item239.dat$aveRval < 35)]  <- "R28.R35"
unique(item239.dat$rvalue.bins)

item239.dat$count <- 1

item239.dat1 <- item239.dat[which(item239.dat$rvalue.bins != "Unknown"),]
colnames(item239.dat1)

item239.merge <- left_join(rbsa.dat.MF, item239.dat1)
item239.merge <- item239.merge[which(!is.na(item239.merge$count)),]

item239.data <- weightedData(unique(item239.merge[-which(colnames(item239.merge) %in% c("CK_SiteID"
                                                                                        ,"Floor.Type"
                                                                                        ,"aveUval"
                                                                                        ,"aveRval"
                                                                                        ,"rvalue.bins"
                                                                                        ,"count"))]))
item239.data <- left_join(item239.data, item239.merge[which(colnames(item239.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"CK_SiteID"
                                                                                           ,"Floor.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"rvalue.bins"
                                                                                           ,"count"))])



################################
# Weighted Analysis
################################
item239.summary <- proportionRowsAndColumns1(CustomerLevelData     = item239.data
                                             , valueVariable       = 'count'
                                             , columnVariable      = 'Floor.Type'
                                             , rowVariable         = 'rvalue.bins'
                                             , aggregateColumnName = "All Types"
)
item239.summary <- item239.summary[which(item239.summary$Floor.Type != "All Types"),]

## Summary only for "All Frame Types"
item239.all.frame.types <- proportions_one_group(CustomerLevelData = item239.data
                                                 ,valueVariable    = "count"
                                                 ,groupingVariable = "rvalue.bins"
                                                 ,total.name       = "All Types"
                                                 ,columnName       = "Floor.Type"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE
)

#merge together!
item239.final <- rbind.data.frame(item239.summary
                                  , item239.all.frame.types, stringsAsFactors = F)


##cast data
item239.cast <- dcast(setDT(item239.final),
                      formula   = BuildingType + Floor.Type ~ rvalue.bins,
                      value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

#join all insulation levels onto rvalue summary
item239.table <- data.frame("BuildingType"                  = item239.cast$BuildingType
                            ,"Floor.Type"                   = item239.cast$Floor.Type
                            ,"Percent.None"                 = item239.cast$w.percent_None
                            ,"SE.None"                      = item239.cast$w.SE_None
                            ,"Percent.R0.R3"                = item239.cast$w.percent_R0.R3
                            ,"SE.R0.R3"                     = item239.cast$w.SE_R0.R3
                            ,"Percent.R4.R10"               = item239.cast$w.percent_R4.R10
                            ,"SE.R4.R10"                    = item239.cast$w.SE_R4.R10
                            ,"Percent.R11.R15"              = item239.cast$w.percent_R11.R15
                            ,"SE.R11.R15"                   = item239.cast$w.SE_R11.R15
                            ,"Percent.R16.R22"              = item239.cast$w.percent_R16.R22
                            ,"SE.R16.R22"                   = item239.cast$w.SE_R16.R22
                            ,"Percent.R23.R27"              = item239.cast$w.percent_R23.R27
                            ,"SE.R23.R27"                   = item239.cast$w.SE_R23.R27
                            ,"Percent.R28.R35"              = item239.cast$w.percent_R28.R35
                            ,"SE.R28.R35"                   = item239.cast$w.SE_R28.R35
                            ,"n"                            = item239.cast$n_Total
                            ,"EB.None"                      = item239.cast$EB_None
                            ,"EB.R0.R3"                     = item239.cast$EB_R0.R3
                            ,"EB.R4.R10"                    = item239.cast$EB_R4.R10
                            ,"EB.R11.R15"                   = item239.cast$EB_R11.R15
                            ,"EB.R16.R22"                   = item239.cast$EB_R16.R22
                            ,"EB.R23.R27"                   = item239.cast$EB_R23.R27
                            ,"EB.R28.R35"                   = item239.cast$EB_R28.R35
)
# row ordering example code
unique(item239.table$Floor.Type)
rowOrder <- c("Crawlspace"
              ,"Cantilever"
              ,"Floor over other area"
              ,"Basement"
              ,"All Types")
item239.table <- item239.table %>% mutate(Floor.Type = factor(Floor.Type, levels = rowOrder)) %>% arrange(Floor.Type)
item239.table <- data.frame(item239.table)


item239.table.MF <- item239.table[which(item239.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item239.table.MF, "MF", "Table 31", weighted = TRUE)



################################
# Unweighted Analysis
################################
item239.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item239.data
                                                     , valueVariable       = 'count'
                                                     , columnVariable      = 'Floor.Type'
                                                     , rowVariable         = 'rvalue.bins'
                                                     , aggregateColumnName = "All Types"
)
item239.summary <- item239.summary[which(item239.summary$Floor.Type != "All Types"),]

## Summary only for "All Frame Types"
item239.all.frame.types <- proportions_one_group(CustomerLevelData = item239.data
                                                 ,valueVariable    = "count"
                                                 ,groupingVariable = "rvalue.bins"
                                                 ,total.name       = "All Types"
                                                 ,columnName       = "Floor.Type"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE
)

#merge together!
item239.final <- rbind.data.frame(item239.summary
                                  , item239.all.frame.types, stringsAsFactors = F)


##cast data
item239.cast <- dcast(setDT(item239.final),
                      formula   = BuildingType + Floor.Type ~ rvalue.bins,
                      value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item239.table <- data.frame("BuildingType"                  = item239.cast$BuildingType
                            ,"Floor.Type"                   = item239.cast$Floor.Type
                            ,"Percent.None"                 = item239.cast$Percent_None
                            ,"SE.None"                      = item239.cast$SE_None
                            ,"Percent.R0.R3"                = item239.cast$Percent_R0.R3
                            ,"SE.R0.R3"                     = item239.cast$SE_R0.R3
                            ,"Percent.R4.R10"               = item239.cast$Percent_R4.R10
                            ,"SE.R4.R10"                    = item239.cast$SE_R4.R10
                            ,"Percent.R11.R15"              = item239.cast$Percent_R11.R15
                            ,"SE.R11.R15"                   = item239.cast$SE_R11.R15
                            ,"Percent.R16.R22"              = item239.cast$Percent_R16.R22
                            ,"SE.R16.R22"                   = item239.cast$SE_R16.R22
                            ,"Percent.R23.R27"              = item239.cast$Percent_R23.R27
                            ,"SE.R23.R27"                   = item239.cast$SE_R23.R27
                            ,"Percent.R28.R35"              = item239.cast$Percent_R28.R35
                            ,"SE.R28.R35"                   = item239.cast$SE_R28.R35
                            ,"n"                            = item239.cast$n_Total
)
# row ordering example code
unique(item239.table$Floor.Type)
rowOrder <- c("Crawlspace"
              ,"Cantilever"
              ,"Floor over other area"
              ,"Basement"
              ,"All Types")
item239.table <- item239.table %>% mutate(Floor.Type = factor(Floor.Type, levels = rowOrder)) %>% arrange(Floor.Type)
item239.table <- data.frame(item239.table)


item239.table.MF <- item239.table[which(item239.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item239.table.MF, "MF", "Table 31", weighted = FALSE)
