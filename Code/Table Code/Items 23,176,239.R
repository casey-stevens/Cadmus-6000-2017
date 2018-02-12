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
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Envelope.xlsx', envelope.export, mode = 'wb')
envelope.dat <- read.xlsx(envelope.export)
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]

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
                                                             ,"CK_SiteID"
                                                             ,"PK_Envelope_ID"
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
prep.dat1.1 <- prep.dat1.0[which(prep.dat1.0$Floor.Insulation.Thickness.1 != "Unknown"),]
prep.dat1.2 <- prep.dat1.1[-which(prep.dat1.1$Slab.Insulation.Thickness.1 == "Unknown"),]

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
# ITEM 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (SF Table 30, MH Table 18)
############################################################################################################
item23.dat <- prep.dat7[grep("site", prep.dat7$CK_Building_ID, ignore.case = T),]

item23.dat1 <- item23.dat[which(!(is.na(item23.dat$HomeYearBuilt_bins3))),]

#Bin R values -- SF only
item23.dat1$rvalue.bins.SF <- "Unknown"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval == 0)] <- "None"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >  0  & item23.dat1$aveRval  < 4)]   <- "R1.R3"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 4  & item23.dat1$aveRval  < 11)]  <- "R4.R10"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 11 & item23.dat1$aveRval  < 16)]  <- "R11.R15"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 16 & item23.dat1$aveRval  < 23)]  <- "R16.R22"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 23 & item23.dat1$aveRval  < 28)]  <- "R23.R27"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 28 & item23.dat1$aveRval  < 36)]  <- "R28.R35"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 36)] <- "RGT36"
unique(item23.dat1$rvalue.bins.SF)

#Bin R values -- MH only
item23.dat1$rvalue.bins.MH <- "Unknown"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval >= 0  & item23.dat1$aveRval < 9)]    <- "R0.R8"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval >= 9 & item23.dat1$aveRval < 15)]   <- "R9.R14"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval >= 15 & item23.dat1$aveRval < 22)]  <- "R15.R21"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval >= 22 & item23.dat1$aveRval < 31)]  <- "R22.R30"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval >= 31)]  <- "R31.R40"
unique(item23.dat1$rvalue.bins.MH)


######################
# Apply weights
######################
item23.dat1$count <- 1
colnames(item23.dat1)

item23.merge <- left_join(rbsa.dat, item23.dat1)
item23.merge <- item23.merge[which(!is.na(item23.merge$count)),]

item23.data <- weightedData(unique(item23.merge[which(colnames(item23.merge) %notin% c("Wall.Type"
                                                                                     ,"aveUval"
                                                                                     ,"aveRval"
                                                                                     ,"rvalue.bins.SF"
                                                                                     ,"rvalue.bins.MH"
                                                                                     ,"count"
                                                                                     ,"Floor.Type"))]))
item23.data <- left_join(item23.data, item23.merge[which(colnames(item23.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins.SF"
                                                                                       ,"rvalue.bins.MH"
                                                                                       ,"count"
                                                                                       ,"Floor.Type"))])
######################
# Weighted - Single Family
######################
item23.summary <- proportionRowsAndColumns1(CustomerLevelData     = item23.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item23.summary <- item23.summary[which(item23.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item23.all.frame.types <- proportions_one_group(item23.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item23.all.insul.levels <-  proportions_one_group(item23.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item23.final <- rbind.data.frame(item23.summary
                                 , item23.all.frame.types
                                 , item23.all.insul.levels
                                 , stringsAsFactors = F)
item23.final <- item23.final[which(item23.final$rvalue.bins.SF != "Total"),]
item23.final$HomeYearBuilt_bins3[which(item23.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


item23.cast <- dcast(setDT(item23.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item23.cast)
item23.table <- data.frame("BuildingType"     = item23.cast$BuildingType
                           ,"Housing.Vintage" = item23.cast$HomeYearBuilt_bins3
                           ,"Percent.None"    = item23.cast$w.percent_None
                           ,"SE.None"         = item23.cast$w.SE_None
                           ,"Percent.R1.R3"   = item23.cast$w.percent_R1.R3  
                           ,"SE.R1.R3"        = item23.cast$w.SE_R1.R3
                           ,"Percent.R4.R10"  = item23.cast$w.percent_R4.R10  
                           ,"SE.R4.R10"       = item23.cast$w.SE_R4.R10
                           ,"Percent.R11.R15" = item23.cast$w.percent_R11.R15
                           ,"SE.R11.R15"      = item23.cast$w.SE_R11.R15
                           ,"Percent.R16.R22" = item23.cast$w.percent_R16.R22
                           ,"SE.R16.R22"      = item23.cast$w.SE_R16.R22
                           ,"Percent.R23.R27" = item23.cast$w.percent_R23.R27
                           ,"SE.R23.R27"      = item23.cast$w.SE_R23.R27
                           ,"Percent.R28.R35" = item23.cast$w.percent_R28.R35
                           ,"SE.R28.R35"      = item23.cast$w.SE_R28.R35
                           ,"Percent.RGT36"   = item23.cast$w.percent_RGT36
                           ,"SE.RGT36"        = item23.cast$w.SE_RGT36
                           ,"n"               = item23.cast$`n_All Housing Vintages`
                           ,'EB.None'         = item23.cast$EB_None
                           ,'EB.R1.R3'        = item23.cast$EB_R1.R3
                           ,'EB.R4.R10'       = item23.cast$EB_R4.R10
                           ,'EB.R11.R15'      = item23.cast$EB_R11.R15
                           ,'EB.R16.R22'      = item23.cast$EB_R16.R22
                           ,'EB.R23.R27'      = item23.cast$EB_R23.R27
                           ,'EB.R28.R35'      = item23.cast$EB_R28.R35
                           ,'EB.RGT36'        = item23.cast$EB_RGT36
                           )

# row ordering example code
levels(item23.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item23.table <- item23.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item23.table <- data.frame(item23.table)


item23.table.SF <- item23.table[which(item23.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item23.table.SF, "SF", "Table 30", weighted = TRUE)

######################
# Unweighted - Single Family
######################
item23.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item23.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item23.summary <- item23.summary[which(item23.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item23.all.frame.types <- proportions_one_group(item23.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item23.all.insul.levels <-  proportions_one_group(item23.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item23.final <- rbind.data.frame(item23.summary
                                 , item23.all.frame.types
                                 , item23.all.insul.levels
                                 , stringsAsFactors = F)
item23.final <- item23.final[which(item23.final$rvalue.bins.SF != "Total"),]
item23.final$HomeYearBuilt_bins3[which(item23.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


item23.cast <- dcast(setDT(item23.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))

item23.table <- data.frame("BuildingType"     = item23.cast$BuildingType
                           ,"Housing.Vintage" = item23.cast$HomeYearBuilt_bins3
                           ,"Percent.None"    = item23.cast$Percent_None
                           ,"SE.None"         = item23.cast$SE_None
                           ,"Percent.R1.R3"   = item23.cast$Percent_R1.R3  
                           ,"SE.R1.R3"        = item23.cast$SE_R1.R3
                           ,"Percent.R4.R10"  = item23.cast$Percent_R4.R10  
                           ,"SE.R4.R10"       = item23.cast$SE_R4.R10
                           ,"Percent.R11.R15" = item23.cast$Percent_R11.R15
                           ,"SE.R11.R15"      = item23.cast$SE_R11.R15
                           ,"Percent.R16.R22" = item23.cast$Percent_R16.R22
                           ,"SE.R16.R22"      = item23.cast$SE_R16.R22
                           ,"Percent.R23.R27" = item23.cast$Percent_R23.R27
                           ,"SE.R23.R27"      = item23.cast$SE_R23.R27
                           ,"Percent.R28.R35" = item23.cast$Percent_R28.R35
                           ,"SE.R28.R35"      = item23.cast$SE_R28.R35
                           ,"Percent.RGT36"   = item23.cast$Percent_RGT36
                           ,"SE.RGT36"        = item23.cast$SE_RGT36
                           ,"n"               = item23.cast$`n_All Housing Vintages`
)

# row ordering example code
levels(item23.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")

item23.table <- item23.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item23.table <- data.frame(item23.table)

item23.table.SF <- item23.table[which(item23.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item23.table.SF, "SF", "Table 30", weighted = FALSE)



######################
# Weighted - Manufactured
######################
item23.summary <- proportionRowsAndColumns1(CustomerLevelData     = item23.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins2'
                                            , rowVariable         = 'rvalue.bins.MH'
                                            , aggregateColumnName = "All Housing Vintages"
)
item23.summary <- item23.summary[which(item23.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]
item23.summary <- item23.summary[which(item23.summary$rvalue.bins.MH != "Total"),]

## Summary only for "All Frame Types"
item23.all.frame.types <- proportions_one_group(item23.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.MH"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins2"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)
item23.all.frame.types <- item23.all.frame.types[which(item23.all.frame.types$rvalue.bins.MH != "Total"),]

## Summary for only "All Insulation Levels"
item23.all.insul.levels <-  proportions_one_group(item23.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins2"
                                                  ,total.name       = "All Floors"
                                                  ,columnName       = "rvalue.bins.MH"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)
item23.all.insul.levels$HomeYearBuilt_bins2[which(item23.all.insul.levels$HomeYearBuilt_bins2 == "Total")] <- "All Housing Vintages"

#merge together!
item23.final <- rbind.data.frame(item23.summary
                                 , item23.all.frame.types
                                 , item23.all.insul.levels
                                 , stringsAsFactors = F)


item23.cast <- dcast(setDT(item23.final),
                     formula   = BuildingType +  HomeYearBuilt_bins2 ~ rvalue.bins.MH,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item23.table <- data.frame("BuildingType"        = item23.cast$BuildingType
                           ,"Housing.Vintage"    = item23.cast$HomeYearBuilt_bins2
                           ,"Percent.R0.R8"      = item23.cast$w.percent_R0.R8  
                           ,"SE.R0.R8"           = item23.cast$w.SE_R0.R8
                           ,"Percent.R9.R14"     = item23.cast$w.percent_R9.R14  
                           ,"SE.R9.R14"          = item23.cast$w.SE_R9.R14
                           ,"Percent.R15.R21"    = item23.cast$w.percent_R15.R21
                           ,"SE.R15.R21"         = item23.cast$w.SE_R15.R21
                           ,"Percent.R22.R30"    = item23.cast$w.percent_R22.R30
                           ,"SE.R22.R30"         = item23.cast$w.SE_R22.R30
                           ,"Percent.R31.R40"    = item23.cast$w.percent_R31.R40
                           ,"SE.R31.R40"         = item23.cast$w.SE_R31.R40
                           ,"Percent.All.Floors" = item23.cast$`w.percent_All Floors`
                           ,"SE.All.Floors"      = item23.cast$`w.SE_All Floors`
                           ,"n"                  = item23.cast$`n_All Floors`
                           ,'EB.R0.R8'           = item23.cast$EB_R0.R8
                           ,'EB.R9.R14'          = item23.cast$EB_R9.R14
                           ,'EB.R15.R21'         = item23.cast$EB_R15.R21
                           ,'EB.R22.R30'         = item23.cast$EB_R22.R30
                           ,'EB.R31.R40'         = item23.cast$EB_R31.R40
                           ,"EB.All.Floors"      = item23.cast$`EB_All Floors`
                           )

# row ordering example code
levels(item23.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")

item23.table <- item23.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item23.table <- data.frame(item23.table)

item23.table.MH <- item23.table[which(item23.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
# exportTable(item23.table.MH, "MH", "Table 18", weighted = TRUE)


######################
# Unweighted - Manufactured
######################
item23.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item23.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins2'
                                            , rowVariable         = 'rvalue.bins.MH'
                                            , aggregateColumnName = "All Housing Vintages"
)
item23.summary <- item23.summary[which(item23.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]
item23.summary <- item23.summary[which(item23.summary$rvalue.bins.MH != "Total"),]

## Summary only for "All Frame Types"
item23.all.frame.types <- proportions_one_group(item23.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.MH"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins2"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)
item23.all.frame.types <- item23.all.frame.types[which(item23.all.frame.types$rvalue.bins.MH != "Total"),]

## Summary for only "All Insulation Levels"
item23.all.insul.levels <-  proportions_one_group(item23.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins2"
                                                  ,total.name       = "All Floors"
                                                  ,columnName       = "rvalue.bins.MH"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)
item23.all.insul.levels$HomeYearBuilt_bins2[which(item23.all.insul.levels$HomeYearBuilt_bins2 == "Total")] <- "All Housing Vintages"

#merge together!
item23.final <- rbind.data.frame(item23.summary
                                 , item23.all.frame.types
                                 , item23.all.insul.levels
                                 , stringsAsFactors = F)

item23.cast <- dcast(setDT(item23.final),
                     formula   = BuildingType +  HomeYearBuilt_bins2 ~ rvalue.bins.MH,
                     value.var = c("Percent", "SE", "Count", "n"))

item23.table <- data.frame("BuildingType"        = item23.cast$BuildingType
                           ,"Housing.Vintage"    = item23.cast$HomeYearBuilt_bins2
                           ,"Percent.R0.R8"      = item23.cast$Percent_R0.R8  
                           ,"SE.R0.R8"           = item23.cast$SE_R0.R8
                           ,"Percent.R9.R14"     = item23.cast$Percent_R9.R14  
                           ,"SE.R9.R14"          = item23.cast$SE_R9.R14
                           ,"Percent.R15.R21"    = item23.cast$Percent_R15.R21
                           ,"SE.R15.R21"         = item23.cast$SE_R15.R21
                           ,"Percent.R22.R30"    = item23.cast$Percent_R22.R30
                           ,"SE.R22.R30"         = item23.cast$SE_R22.R30
                           ,"Percent.R31.R40"    = item23.cast$Percent_R31.R40
                           ,"SE.R31.R40"         = item23.cast$SE_R31.R40
                           ,"Percent.All.Floors" = item23.cast$`Percent_All Floors`
                           ,"SE.All.Floors"      = item23.cast$`SE_All Floors`
                           ,"n"                  = item23.cast$`n_All Floors`
)

# row ordering example code
levels(item23.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")

item23.table <- item23.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item23.table <- data.frame(item23.table)

item23.table.MH <- item23.table[which(item23.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
# exportTable(item23.table.MH, "MH", "Table 18", weighted = FALSE)





#########################################################################################
# ITEM 176: DISTRIBUTION OF FLOOR U-VALUE BY STATE (MH Table 19)
#########################################################################################
item176.dat1 <- prep.dat7

######################
# Apply weights
######################
item176.dat1$count <- 1
colnames(item176.dat1)

item176.merge <- left_join(rbsa.dat, item176.dat1)
item176.merge <- item176.merge[which(!is.na(item176.merge$count)),]

item176.data <- weightedData(unique(item176.merge[which(colnames(item176.merge) %notin% c("aveUval"
                                                                                          ,"aveRval"
                                                                                          ,"count"
                                                                                          ,"Floor.Type"))]))
item176.data <- left_join(item176.data, item176.merge[which(colnames(item176.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"count"
                                                                                           ,"Floor.Type"))])
######################
# Weighted - MH
######################
item176.final <- mean_one_group(CustomerLevelData = item176.data
                                ,valueVariable = 'aveUval'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")

item176.table.MH <- item176.final[which(item176.final$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item176.table.MH, "MH", "Table 19", weighted = TRUE)


######################
# Unweighted - MH
######################
item176.final <- mean_one_group_unweighted(CustomerLevelData = item176.data
                                           ,valueVariable = 'aveUval'
                                           ,byVariable = 'State'
                                           ,aggregateRow = "Region")

item176.table.MH <- item176.final[which(item176.final$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item176.table.MH, "MH", "Table 19", weighted = FALSE)






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
                            ,"Percent.R0.R3"                = NA#item239.cast$w.percent_R0.R3
                            ,"SE.R0.R3"                     = NA#item239.cast$w.SE_R0.R3
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
                            ,"EB.R0.R3"                     = NA#item239.cast$EB_R0.R3
                            ,"EB.R4.R10"                    = item239.cast$EB_R4.R10
                            ,"EB.R11.R15"                   = item239.cast$EB_R11.R15
                            ,"EB.R16.R22"                   = item239.cast$EB_R16.R22
                            ,"EB.R23.R27"                   = item239.cast$EB_R23.R27
                            ,"EB.R28.R35"                   = item239.cast$EB_R28.R35
)
# row ordering example code
unique(item239.table$Floor.Type)
rowOrder <- c("Crawlspace"
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
                            ,"Percent.R0.R3"                = NA#item239.cast$Percent_R0.R3
                            ,"SE.R0.R3"                     = NA#item239.cast$SE_R0.R3
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
              ,"Floor over other area"
              ,"Basement"
              ,"All Types")
item239.table <- item239.table %>% mutate(Floor.Type = factor(Floor.Type, levels = rowOrder)) %>% arrange(Floor.Type)  
item239.table <- data.frame(item239.table)


item239.table.MF <- item239.table[which(item239.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item239.table.MF, "MF", "Table 31", weighted = FALSE)




















############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

############################################################################################################
# ITEM 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (SF Table 30, MH Table 18)
############################################################################################################
item23.os.dat <- prep.dat7

item23.os.dat1 <- item23.os.dat[which(!(is.na(item23.os.dat$HomeYearBuilt_bins3))),]

#Bin R values -- SF only
item23.os.dat1$rvalue.bins.SF <- "Unknown"
item23.os.dat1$rvalue.bins.SF[which(item23.os.dat1$aveRval == 0)] <- "None"
item23.os.dat1$rvalue.bins.SF[which(item23.os.dat1$aveRval >  0  & item23.os.dat1$aveRval  < 4)]   <- "R1.R3"
item23.os.dat1$rvalue.bins.SF[which(item23.os.dat1$aveRval >= 4  & item23.os.dat1$aveRval  < 11)]  <- "R4.R10"
item23.os.dat1$rvalue.bins.SF[which(item23.os.dat1$aveRval >= 11 & item23.os.dat1$aveRval  < 16)]  <- "R11.R15"
item23.os.dat1$rvalue.bins.SF[which(item23.os.dat1$aveRval >= 16 & item23.os.dat1$aveRval  < 23)]  <- "R16.R22"
item23.os.dat1$rvalue.bins.SF[which(item23.os.dat1$aveRval >= 23 & item23.os.dat1$aveRval  < 28)]  <- "R23.R27"
item23.os.dat1$rvalue.bins.SF[which(item23.os.dat1$aveRval >= 28 & item23.os.dat1$aveRval  < 36)]  <- "R28.R35"
item23.os.dat1$rvalue.bins.SF[which(item23.os.dat1$aveRval >= 36)] <- "RGT36"
unique(item23.os.dat1$rvalue.bins.SF)

######################
# Apply weights
######################
item23.os.dat1$count <- 1
colnames(item23.os.dat1)
item23.os.dat1 <- item23.os.dat1[which(names(item23.os.dat1) != "CK_Building_ID")]

item23.os.merge <- left_join(scl.dat, item23.os.dat1)
item23.os.merge <- item23.os.merge[which(item23.os.merge$CK_Building_ID == "SCL GenPop"),]
item23.os.merge <- item23.os.merge[which(!is.na(item23.os.merge$count)),]

item23.os.data <- weightedData(unique(item23.os.merge[which(colnames(item23.os.merge) %notin% c("Wall.Type"
                                                                                                ,"aveUval"
                                                                                                ,"aveRval"
                                                                                                ,"rvalue.bins.SF"
                                                                                                ,"count"
                                                                                                ,"Floor.Type"))]))
item23.os.data <- left_join(item23.os.data, item23.os.merge[which(colnames(item23.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"Wall.Type"
                                                                                                   ,"aveUval"
                                                                                                   ,"aveRval"
                                                                                                   ,"rvalue.bins.SF"
                                                                                                   ,"count"
                                                                                                   ,"Floor.Type"))])
######################
# Weighted - Single Family
######################
item23.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item23.os.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item23.os.summary <- item23.os.summary[which(item23.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item23.os.all.frame.types <- proportions_one_group(item23.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item23.os.all.insul.levels <-  proportions_one_group(item23.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE
)


#merge together!
item23.os.final <- rbind.data.frame(item23.os.summary
                                 , item23.os.all.frame.types
                                 , item23.os.all.insul.levels
                                 , stringsAsFactors = F)
item23.os.final <- item23.os.final[which(item23.os.final$rvalue.bins.SF != "Total"),]
item23.os.final$HomeYearBuilt_bins3[which(item23.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


item23.os.cast <- dcast(setDT(item23.os.final),
                     formula   = HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item23.os.cast)
item23.os.table <- data.frame("Housing.Vintage"  = item23.os.cast$HomeYearBuilt_bins3
                              ,"Percent.None"    = item23.os.cast$w.percent_None
                              ,"SE.None"         = item23.os.cast$w.SE_None
                              ,"Percent.R1.R3"   = item23.os.cast$w.percent_R1.R3  
                              ,"SE.R1.R3"        = item23.os.cast$w.SE_R1.R3
                              ,"Percent.R4.R10"  = item23.os.cast$w.percent_R4.R10  
                              ,"SE.R4.R10"       = item23.os.cast$w.SE_R4.R10
                              ,"Percent.R11.R15" = item23.os.cast$w.percent_R11.R15
                              ,"SE.R11.R15"      = item23.os.cast$w.SE_R11.R15
                              ,"Percent.R16.R22" = item23.os.cast$w.percent_R16.R22
                              ,"SE.R16.R22"      = item23.os.cast$w.SE_R16.R22
                              ,"Percent.R23.R27" = item23.os.cast$w.percent_R23.R27
                              ,"SE.R23.R27"      = item23.os.cast$w.SE_R23.R27
                              ,"Percent.R28.R35" = item23.os.cast$w.percent_R28.R35
                              ,"SE.R28.R35"      = item23.os.cast$w.SE_R28.R35
                              ,"Percent.RGT36"   = item23.os.cast$w.percent_RGT36
                              ,"SE.RGT36"        = item23.os.cast$w.SE_RGT36
                              ,"n"               = item23.os.cast$`n_All Housing Vintages`
                              ,'EB.None'         = item23.os.cast$EB_None
                              ,'EB.R1.R3'        = item23.os.cast$EB_R1.R3
                              ,'EB.R4.R10'       = item23.os.cast$EB_R4.R10
                              ,'EB.R11.R15'      = item23.os.cast$EB_R11.R15
                              ,'EB.R16.R22'      = item23.os.cast$EB_R16.R22
                              ,'EB.R23.R27'      = item23.os.cast$EB_R23.R27
                              ,'EB.R28.R35'      = item23.os.cast$EB_R28.R35
                              ,'EB.RGT36'        = item23.os.cast$EB_RGT36
)

# row ordering example code
levels(item23.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item23.os.table <- item23.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item23.os.table <- data.frame(item23.os.table)

#export table to correct workbook using exporting function
exportTable(item23.os.table, "SF", "Table 30", weighted = TRUE, osIndicator = "SCL", OS = T)

######################
# Unweighted - Single Family
######################
item23.os.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item23.os.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item23.os.summary <- item23.os.summary[which(item23.os.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item23.os.all.frame.types <- proportions_one_group(item23.os.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item23.os.all.insul.levels <-  proportions_one_group(item23.os.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Housing Vintages"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)


#merge together!
item23.os.final <- rbind.data.frame(item23.os.summary
                                 , item23.os.all.frame.types
                                 , item23.os.all.insul.levels
                                 , stringsAsFactors = F)
item23.os.final <- item23.os.final[which(item23.os.final$rvalue.bins.SF != "Total"),]
item23.os.final$HomeYearBuilt_bins3[which(item23.os.final$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

item23.os.cast <- dcast(setDT(item23.os.final),
                     formula   = HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))
names(item23.os.cast)
item23.os.table <- data.frame("Housing.Vintage"  = item23.os.cast$HomeYearBuilt_bins3
                              ,"Percent.None"    = item23.os.cast$Percent_None
                              ,"SE.None"         = item23.os.cast$SE_None
                              ,"Percent.R1.R3"   = item23.os.cast$Percent_R1.R3  
                              ,"SE.R1.R3"        = item23.os.cast$SE_R1.R3
                              ,"Percent.R4.R10"  = item23.os.cast$Percent_R4.R10  
                              ,"SE.R4.R10"       = item23.os.cast$SE_R4.R10
                              ,"Percent.R11.R15" = item23.os.cast$Percent_R11.R15
                              ,"SE.R11.R15"      = item23.os.cast$SE_R11.R15
                              ,"Percent.R16.R22" = item23.os.cast$Percent_R16.R22
                              ,"SE.R16.R22"      = item23.os.cast$SE_R16.R22
                              ,"Percent.R23.R27" = item23.os.cast$Percent_R23.R27
                              ,"SE.R23.R27"      = item23.os.cast$SE_R23.R27
                              ,"Percent.R28.R35" = item23.os.cast$Percent_R28.R35
                              ,"SE.R28.R35"      = item23.os.cast$SE_R28.R35
                              ,"Percent.RGT36"   = item23.os.cast$Percent_RGT36
                              ,"SE.RGT36"        = item23.os.cast$SE_RGT36
                              ,"n"               = item23.os.cast$`n_All Housing Vintages`
)

# row ordering example code
levels(item23.os.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")

item23.os.table <- item23.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item23.os.table <- data.frame(item23.os.table)

#export table to correct workbook using exporting function
exportTable(item23.os.table, "SF", "Table 30", weighted = FALSE, osIndicator = "SCL", OS = T)




############################################################################################################
# ITEM 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (SF Table 30, MH Table 18)
############################################################################################################
item23A.os.dat1 <- prep.dat7

#Bin R values -- SF only
item23A.os.dat1$rvalue.bins.SF <- "Unknown"
item23A.os.dat1$rvalue.bins.SF[which(item23A.os.dat1$aveRval == 0)] <- "None"
item23A.os.dat1$rvalue.bins.SF[which(item23A.os.dat1$aveRval >  0  & item23A.os.dat1$aveRval  < 4)]   <- "R1.R3"
item23A.os.dat1$rvalue.bins.SF[which(item23A.os.dat1$aveRval >= 4  & item23A.os.dat1$aveRval  < 11)]  <- "R4.R10"
item23A.os.dat1$rvalue.bins.SF[which(item23A.os.dat1$aveRval >= 11 & item23A.os.dat1$aveRval  < 16)]  <- "R11.R15"
item23A.os.dat1$rvalue.bins.SF[which(item23A.os.dat1$aveRval >= 16 & item23A.os.dat1$aveRval  < 23)]  <- "R16.R22"
item23A.os.dat1$rvalue.bins.SF[which(item23A.os.dat1$aveRval >= 23 & item23A.os.dat1$aveRval  < 28)]  <- "R23.R27"
item23A.os.dat1$rvalue.bins.SF[which(item23A.os.dat1$aveRval >= 28 & item23A.os.dat1$aveRval  < 36)]  <- "R28.R35"
item23A.os.dat1$rvalue.bins.SF[which(item23A.os.dat1$aveRval >= 36)] <- "RGT36"
unique(item23A.os.dat1$rvalue.bins.SF)

######################
# Apply weights
######################
item23A.os.dat1$count <- 1
colnames(item23A.os.dat1)
item23A.os.dat1 <- item23A.os.dat1[which(names(item23A.os.dat1) != "CK_Building_ID")]

item23A.os.merge <- left_join(scl.dat, item23A.os.dat1)
item23A.os.merge <- item23A.os.merge[which(!is.na(item23A.os.merge$count)),]

item23A.os.data <- weightedData(unique(item23A.os.merge[which(colnames(item23A.os.merge) %notin% c("Wall.Type"
                                                                                                ,"aveUval"
                                                                                                ,"aveRval"
                                                                                                ,"rvalue.bins.SF"
                                                                                                ,"count"
                                                                                                ,"Floor.Type"))]))
item23A.os.data <- left_join(item23A.os.data, item23A.os.merge[which(colnames(item23A.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"Wall.Type"
                                                                                                   ,"aveUval"
                                                                                                   ,"aveRval"
                                                                                                   ,"rvalue.bins.SF"
                                                                                                   ,"count"
                                                                                                   ,"Floor.Type"))])
######################
# Weighted - Single Family
######################
item23A.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item23A.os.data
                                               , valueVariable       = 'count'
                                               , columnVariable      = 'CK_Building_ID'
                                               , rowVariable         = 'rvalue.bins.SF'
                                               , aggregateColumnName = "Remove"
)
item23A.os.summary <- item23A.os.summary[which(item23A.os.summary$CK_Building_ID != "Remove"),]
item23A.os.summary <- item23A.os.summary[which(item23A.os.summary$rvalue.bins.SF != "Total"),]

## Summary for only "All Insulation Levels"
item23A.os.all.insul.levels <-  proportions_one_group(item23A.os.data
                                                     ,valueVariable    = "count"
                                                     ,groupingVariable = "CK_Building_ID"
                                                     ,total.name       = "Total"
                                                     ,columnName       = "rvalue.bins.SF"
                                                     ,weighted = TRUE
                                                     ,two.prop.total = TRUE
)
item23A.os.all.insul.levels <- item23A.os.all.insul.levels[which(item23A.os.all.insul.levels$CK_Building_ID != "Total"),]

#merge together!
item23A.os.final <- rbind.data.frame(item23A.os.summary
                                    , item23A.os.all.insul.levels
                                    , stringsAsFactors = F)

item23A.os.cast <- dcast(setDT(item23A.os.final),
                        formula   = rvalue.bins.SF ~ CK_Building_ID,
                        value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item23A.os.cast)
item23A.os.table <- data.frame("Insulation.Level"      = item23A.os.cast$rvalue.bins.SF
                               ,"Percent_SCL.GenPop"   = item23A.os.cast$`w.percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item23A.os.cast$`w.SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item23A.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = item23A.os.cast$`w.percent_SCL LI`
                               ,"SE_SCL.LI"            = item23A.os.cast$`w.SE_SCL LI`
                               ,"n_SCL.LI"             = item23A.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = item23A.os.cast$`w.percent_SCL EH`
                               ,"SE_SCL.EH"            = item23A.os.cast$`w.SE_SCL EH`
                               ,"n_SCL.EH"             = item23A.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = item23A.os.cast$`w.percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item23A.os.cast$`w.SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item23A.os.cast$`n_2017 RBSA PS`
                               ,"EB_SCL.GenPop"        = item23A.os.cast$`EB_SCL GenPop`
                               ,"EB_SCL.LI"            = item23A.os.cast$`EB_SCL LI`
                               ,"EB_SCL.EH"            = item23A.os.cast$`EB_SCL EH`
                               ,"EB_2017.RBSA.PS"      = item23A.os.cast$`EB_2017 RBSA PS`
)

# row ordering example code
levels(item23A.os.table$Insulation.Level)
rowOrder <- c("None"
              ,"R1.R3"
              ,"R4.R10"
              ,"R11.R15"
              ,"R16.R22"
              ,"R23.R27"
              ,"R28.R35"
              ,"RGT36"
              ,"Total")
item23A.os.table <- item23A.os.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)  
item23A.os.table <- data.frame(item23A.os.table)

#export table to correct workbook using exporting function
exportTable(item23A.os.table, "SF", "Table 30A", weighted = TRUE, osIndicator = "SCL", OS = T)

######################
# Unweighted - Single Family
######################
item23A.os.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item23A.os.data
                                                        , valueVariable       = 'count'
                                                        , columnVariable      = 'CK_Building_ID'
                                                        , rowVariable         = 'rvalue.bins.SF'
                                                        , aggregateColumnName = "Remove"
)
item23A.os.summary <- item23A.os.summary[which(item23A.os.summary$CK_Building_ID != "Remove"),]
item23A.os.summary <- item23A.os.summary[which(item23A.os.summary$rvalue.bins.SF != "Total"),]

## Summary for only "All Insulation Levels"
item23A.os.all.insul.levels <-  proportions_one_group(item23A.os.data
                                                      ,valueVariable    = "count"
                                                      ,groupingVariable = "CK_Building_ID"
                                                      ,total.name       = "Total"
                                                      ,columnName       = "rvalue.bins.SF"
                                                      ,weighted = FALSE
                                                      ,two.prop.total = TRUE
)
item23A.os.all.insul.levels <- item23A.os.all.insul.levels[which(item23A.os.all.insul.levels$CK_Building_ID != "Total"),]

#merge together!
item23A.os.final <- rbind.data.frame(item23A.os.summary
                                     , item23A.os.all.insul.levels
                                     , stringsAsFactors = F)

item23A.os.cast <- dcast(setDT(item23A.os.final),
                        formula   = rvalue.bins.SF ~ CK_Building_ID,
                        value.var = c("Percent", "SE", "Count", "n"))
names(item23A.os.cast)
item23A.os.table <- data.frame("Insulation.Level" = item23A.os.cast$rvalue.bins.SF
                               ,"Percent_SCL.GenPop"   = item23A.os.cast$`Percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item23A.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item23A.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = item23A.os.cast$`Percent_SCL LI`
                               ,"SE_SCL.LI"            = item23A.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = item23A.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = item23A.os.cast$`Percent_SCL EH`
                               ,"SE_SCL.EH"            = item23A.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = item23A.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = item23A.os.cast$`Percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item23A.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item23A.os.cast$`n_2017 RBSA PS`
)

# row ordering example code
levels(item23A.os.table$Insulation.Level)
rowOrder <- c("None"
              ,"R1.R3"
              ,"R4.R10"
              ,"R11.R15"
              ,"R16.R22"
              ,"R23.R27"
              ,"R28.R35"
              ,"RGT36"
              ,"Total")
item23A.os.table <- item23A.os.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)  
item23A.os.table <- data.frame(item23A.os.table)
#export table to correct workbook using exporting function
exportTable(item23A.os.table, "SF", "Table 30A", weighted = FALSE, osIndicator = "SCL", OS = T)

