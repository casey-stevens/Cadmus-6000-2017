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

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]

#############################################################################################
# Item 163: DISTRIBUTION OF FLOOR INSULATION, ELECTRICALLY HEATED HOMES (SF table B-8)
#############################################################################################
#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item163.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item163.dat.11 <- item163.dat.1[which(item163.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item163.dat.12 <- item163.dat.11[which(item163.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item163.dat.12$CK_Cadmus_ID))
#check uniques
unique(item163.dat.12$Primary.Heating.System)
item163.dat.12$count <- 1

item163.dat.13 <- unique(item163.dat.12[which(item163.dat.12$Heating.Fuel == "Electric"),])

item163.sum1 <- summarise(group_by(item163.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item163.sum1$Count <- 1
which(duplicated(item163.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item163.sum1$Heating.Fuel)

item163.mechanical <- item163.sum1






#############################################################################################
# Similar to Item 163
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


item163.dat <- prep.dat7

item163.dat1 <- item163.dat[which(!(is.na(item163.dat$HomeYearBuilt_bins3))),]

#Bin R values -- SF only
item163.dat1$rvalue.bins.SF <- "Unknown"
item163.dat1$rvalue.bins.SF[which(item163.dat1$aveRval == 0)] <- "None"
item163.dat1$rvalue.bins.SF[which(item163.dat1$aveRval >  0  & item163.dat1$aveRval  < 4)]   <- "R1.R3"
item163.dat1$rvalue.bins.SF[which(item163.dat1$aveRval >= 4  & item163.dat1$aveRval  < 11)]  <- "R4.R10"
item163.dat1$rvalue.bins.SF[which(item163.dat1$aveRval >= 11 & item163.dat1$aveRval  < 16)]  <- "R11.R15"
item163.dat1$rvalue.bins.SF[which(item163.dat1$aveRval >= 16 & item163.dat1$aveRval  < 163)]  <- "R16.R22"
item163.dat1$rvalue.bins.SF[which(item163.dat1$aveRval >= 23 & item163.dat1$aveRval  < 28)]  <- "R23.R27"
item163.dat1$rvalue.bins.SF[which(item163.dat1$aveRval >= 28 & item163.dat1$aveRval  < 36)]  <- "R28.R35"
item163.dat1$rvalue.bins.SF[which(item163.dat1$aveRval >= 36)] <- "RGT36"
unique(item163.dat1$rvalue.bins.SF)

#Bin R values -- MH only
item163.dat1$rvalue.bins.MH <- "Unknown"
item163.dat1$rvalue.bins.MH[which(item163.dat1$aveRval >= 0  & item163.dat1$aveRval < 9)]    <- "R0.R8"
item163.dat1$rvalue.bins.MH[which(item163.dat1$aveRval >= 9 & item163.dat1$aveRval < 15)]   <- "R9.R14"
item163.dat1$rvalue.bins.MH[which(item163.dat1$aveRval >= 15 & item163.dat1$aveRval < 22)]  <- "R15.R21"
item163.dat1$rvalue.bins.MH[which(item163.dat1$aveRval >= 22 & item163.dat1$aveRval < 31)]  <- "R22.R30"
item163.dat1$rvalue.bins.MH[which(item163.dat1$aveRval >= 31)]  <- "R31.R40"
unique(item163.dat1$rvalue.bins.MH)


############################################################################################################
# Apply weights
############################################################################################################
item163.dat1$count <- 1

item163.merge <- left_join(item163.mechanical, item163.dat1)
item163.merge <- left_join(rbsa.dat, item163.merge)
item163.merge <- item163.merge[which(!is.na(item163.merge$aveRval)),]
colnames(item163.merge)

item163.data <- weightedData(unique(item163.merge[which(colnames(item163.merge) %notin% c("Heating.Fuel"
                                                                                          ,"Count"
                                                                                          ,"Floor.Type"         
                                                                                          ,"aveUval"
                                                                                          ,"aveRval"
                                                                                          ,"rvalue.bins.SF"
                                                                                          ,"rvalue.bins.MH"
                                                                                          ,"count"))]))
item163.data <- left_join(item163.data, item163.merge[which(colnames(item163.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Heating.Fuel"
                                                                                       ,"Count"
                                                                                       ,"Floor.Type"         
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins.SF"
                                                                                       ,"rvalue.bins.MH"
                                                                                       ,"count"))])
############################################################################################################
# Weighted - Single Family
############################################################################################################
item163.summary <- proportionRowsAndColumns1(CustomerLevelData     = item163.data
                                            , valueVariable       = 'count'
                                            , columnVariable      = 'HomeYearBuilt_bins3'
                                            , rowVariable         = 'rvalue.bins.SF'
                                            , aggregateColumnName = "All Housing Vintages"
)
item163.summary <- item163.summary[which(item163.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item163.all.frame.types <- proportions_one_group(item163.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE)

## Summary for only "All Insulation Levels"
item163.all.insul.levels <-  proportions_one_group(item163.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Insulation Levels"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE)
item163.all.insul.levels$HomeYearBuilt_bins3[which(item163.all.insul.levels$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"

#merge together!
item163.final <- rbind.data.frame(item163.summary
                                 , item163.all.frame.types
                                 , item163.all.insul.levels
                                 , stringsAsFactors = F)

item163.cast <- dcast(setDT(item163.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item163.table <- data.frame("BuildingType"     = item163.cast$BuildingType
                           ,"Housing.Vintage" = item163.cast$HomeYearBuilt_bins3
                           ,"Percent.None"    = item163.cast$w.percent_None
                           ,"SE.None"         = item163.cast$w.SE_None
                           # ,"Count.None"      = item163.cast$count_None
                           ,"Percent.R1.R3"   = item163.cast$w.percent_R1.R3  
                           ,"SE.R1.R3"        = item163.cast$w.SE_R1.R3
                           # ,"Count.R1.R3"     = item163.cast$count_R1.R3
                           ,"Percent.R4.R10"  = item163.cast$w.percent_R4.R10  
                           ,"SE.R4.R10"       = item163.cast$w.SE_R4.R10
                           # ,"Count.R4.R10"    = item163.cast$count_R4.R10
                           ,"Percent.R11.R15" = item163.cast$w.percent_R11.R15
                           ,"SE.R11.R15"      = item163.cast$w.SE_R11.R15
                           # ,"Count.R11.R15"   = item163.cast$count_R11.R15
                           ,"Percent.R16.R22" = item163.cast$w.percent_R16.R22
                           ,"SE.R16.R22"      = item163.cast$w.SE_R16.R22
                           # ,"Count.R16.R22"   = item163.cast$count_R16.R22
                           ,"Percent.R23.R27" = item163.cast$w.percent_R23.R27
                           ,"SE.R23.R27"      = item163.cast$w.SE_R23.R27
                           # ,"Count.R23.R27"   = item163.cast$count_R23.R27
                           ,"Percent.R28.R35" = item163.cast$w.percent_R28.R35
                           ,"SE.R28.R35"      = item163.cast$w.SE_R28.R35
                           # ,"Count.R28.R35"   = item163.cast$count_R28.R35
                           ,"Percent.RGT36"   = item163.cast$w.percent_RGT36
                           ,"SE.RGT36"        = item163.cast$w.SE_RGT36
                           # ,"Count.RGT36"     = item163.cast$count_RGT36
                           ,"n" = item163.cast$`n_All Insulation Levels`
                           ,"EB.None"         = item163.cast$EB_None
                           ,"EB.R1.R3"        = item163.cast$EB_R1.R3
                           ,"EB.R4.R10"       = item163.cast$EB_R4.R10
                           ,"EB.R11.R15"      = item163.cast$EB_R11.R15
                           ,"EB.R16.R22"      = item163.cast$EB_R16.R22
                           ,"EB.R23.R27"      = item163.cast$EB_R23.R27
                           ,"EB.R28.R35"      = item163.cast$EB_R28.R35
                           ,"EB.RGT36"        = item163.cast$EB_RGT36
)

# row ordering example code
levels(item163.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item163.table <- item163.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item163.table <- data.frame(item163.table)


item163.table.SF <- item163.table[which(item163.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item163.table.SF, "SF", "Table B-8", weighted = TRUE)

############################################################################################################
# Unweighted - Single Family
############################################################################################################
item163.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item163.data
                                                    , valueVariable       = 'count'
                                                    , columnVariable      = 'HomeYearBuilt_bins3'
                                                    , rowVariable         = 'rvalue.bins.SF'
                                                    , aggregateColumnName = "All Housing Vintages"
)
item163.summary <- item163.summary[which(item163.summary$HomeYearBuilt_bins3 != "All Housing Vintages"),]

## Summary only for "All Frame Types"
item163.all.frame.types <- proportions_one_group(item163.data
                                                ,valueVariable    = "count"
                                                ,groupingVariable = "rvalue.bins.SF"
                                                ,total.name       = "All Housing Vintages"
                                                ,columnName       = "HomeYearBuilt_bins3"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE
)

## Summary for only "All Insulation Levels"
item163.all.insul.levels <-  proportions_one_group(item163.data
                                                  ,valueVariable    = "count"
                                                  ,groupingVariable = "HomeYearBuilt_bins3"
                                                  ,total.name       = "All Insulation Levels"
                                                  ,columnName       = "rvalue.bins.SF"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE
)
item163.all.insul.levels$HomeYearBuilt_bins3[which(item163.all.insul.levels$HomeYearBuilt_bins3 == "Total")] <- "All Housing Vintages"


#merge together!
item163.final <- rbind.data.frame(item163.summary
                                 , item163.all.frame.types
                                 , item163.all.insul.levels
                                 , stringsAsFactors = F)

item163.cast <- dcast(setDT(item163.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 ~ rvalue.bins.SF,
                     value.var = c("Percent", "SE", "Count", "n"))

item163.table <- data.frame("BuildingType"     = item163.cast$BuildingType
                           ,"Housing.Vintage" = item163.cast$HomeYearBuilt_bins3
                           ,"Percent.None"    = item163.cast$Percent_None
                           ,"SE.None"         = item163.cast$SE_None
                           # ,"Count.None"      = item163.cast$Count_None
                           ,"Percent.R1.R3"   = item163.cast$Percent_R1.R3  
                           ,"SE.R1.R3"        = item163.cast$SE_R1.R3
                           # ,"Count.R1.R3"     = item163.cast$Count_R1.R3
                           ,"Percent.R4.R10"  = item163.cast$Percent_R4.R10  
                           ,"SE.R4.R10"       = item163.cast$SE_R4.R10
                           # ,"Count.R4.R10"    = item163.cast$Count_R4.R10
                           ,"Percent.R11.R15" = item163.cast$Percent_R11.R15
                           ,"SE.R11.R15"      = item163.cast$SE_R11.R15
                           # ,"Count.R11.R15"   = item163.cast$Count_R11.R15
                           ,"Percent.R16.R22" = item163.cast$Percent_R16.R22
                           ,"SE.R16.R22"      = item163.cast$SE_R16.R22
                           # ,"Count.R16.R22"   = item163.cast$Count_R16.R22
                           ,"Percent.R23.R27" = item163.cast$Percent_R23.R27
                           ,"SE.R23.R27"      = item163.cast$SE_R23.R27
                           # ,"Count.R23.R27"   = item163.cast$Count_R23.R27
                           ,"Percent.R28.R35" = item163.cast$Percent_R28.R35
                           ,"SE.R28.R35"      = item163.cast$SE_R28.R35
                           # ,"Count.R28.R35"   = item163.cast$Count_R28.R35
                           ,"Percent.RGT36"   = item163.cast$Percent_RGT36
                           ,"SE.RGT36"        = item163.cast$SE_RGT36
                           # ,"Count.RGT36"     = item163.cast$Count_RGT36
                           ,"n" = item163.cast$`n_All Insulation Levels`
)

# row ordering example code
levels(item163.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item163.table <- item163.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item163.table <- data.frame(item163.table)

item163.table.SF <- item163.table[which(item163.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item163.table.SF, "SF", "Table B-8", weighted = FALSE)
