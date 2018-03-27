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




#############################################################################################
#
# PREP FOR ITEMS 26, 30, 31
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
prep.dat0 <- prep.dat[which(prep.dat$`Ceiling.Insulated?` %in% c("Yes", "No")),]
prep.dat1.0 <- prep.dat0[which(!(is.na(as.numeric(as.character(prep.dat0$Ceiling.Area))))),]
prep.dat1.2 <- prep.dat1.0[which(prep.dat1.0$Ceiling.Insulation.Thickness.1  %notin% c("Unknown")),]

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

# replace Unknown or NA in Condition columns with 1 (or 100%)
for (i in grep("insulation.type", names(prep.dat3), ignore.case = T)){
  prep.dat3[,i] <- trimws(tolower(prep.dat3[,i]))
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
# prep.dat5 <- prep.dat4.5
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






rbsa.ceiling <- rbsa.dat[which(colnames(rbsa.dat) %in% c("CK_Building_ID","BuildingType","HomeYearBuilt"))]
ceiling.merge <- left_join(rbsa.ceiling, prep.dat5, by = c("CK_Building_ID" = "CK_SiteID"))
ceiling.merge <- ceiling.merge[which(!is.na(ceiling.merge$uvalue)),]
#########export rvalues
##  Write out confidence/precision info
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(ceiling.merge, paste(filepathCleaningDocs, "Insulation Exports", paste("Ceiling Insulation Values ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)















#############################################################################################
#Item 237: DISTRIBUTION OF CEILING INSULATION BY CEILING TYPE (MF TABLE 29)
#############################################################################################
#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_SiteID, Ceiling.Type)
                       ,aveUval = sum(Ceiling.Area * Ceiling.Insulation.Condition.1 * uvalue) / sum(Ceiling.Area * Ceiling.Insulation.Condition.1)
)
unique(weightedU$aveUval)


#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
unique(weightedU$aveRval)
unique(weightedU$aveUval)

# get unique cadmus IDs and building types for this subset of data
Ceiling.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_SiteID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.dat6 <- left_join(weightedU, Ceiling.unique, by = "CK_SiteID")

#merge weighted u values onto cleaned RBSA data
prep.dat7 <- left_join(prep.dat6, rbsa.dat.MF, by = c("CK_SiteID" = "CK_Building_ID"))
prep.dat7 <- prep.dat7[grep("3 or fewer floors", prep.dat7$BuildingTypeXX),]
item237.dat <- prep.dat7[which(!is.na(prep.dat7$CK_Cadmus_ID)),]

unique(item237.dat$Ceiling.Type)

#Bin R values -- MF only
item237.dat$rvalue.bins <- "Unknown"
item237.dat$rvalue.bins[which(item237.dat$aveRval >=  0  & item237.dat$aveRval < 11)]  <- "R0.R10"
item237.dat$rvalue.bins[which(item237.dat$aveRval >= 11  & item237.dat$aveRval < 16)]  <- "R11.R15"
item237.dat$rvalue.bins[which(item237.dat$aveRval >= 16  & item237.dat$aveRval < 21)]  <- "R16.R20"
item237.dat$rvalue.bins[which(item237.dat$aveRval >= 21  & item237.dat$aveRval < 26)]  <- "R21.R25"
item237.dat$rvalue.bins[which(item237.dat$aveRval >= 26  & item237.dat$aveRval < 31)]  <- "R26.R30"
item237.dat$rvalue.bins[which(item237.dat$aveRval >= 31  & item237.dat$aveRval < 41)]  <- "R31.R40"
item237.dat$rvalue.bins[which(item237.dat$aveRval >= 41  & item237.dat$aveRval < 51)]  <- "R41.R50"
item237.dat$rvalue.bins[which(item237.dat$aveRval >= 51)] <- "RGT50"
unique(item237.dat$rvalue.bins)

item237.dat$count <- 1

item237.dat1 <- item237.dat[which(item237.dat$rvalue.bins != "Unknown"),]
colnames(item237.dat1)

item237.merge <- left_join(rbsa.dat.MF, item237.dat1)
item237.merge <- item237.merge[which(!is.na(item237.merge$count)),]

item237.data <- weightedData(unique(item237.merge[-which(colnames(item237.merge) %in% c("CK_SiteID"
                                                                                        ,"Ceiling.Type"
                                                                                        ,"aveUval"
                                                                                        ,"aveRval"
                                                                                        ,"rvalue.bins"
                                                                                        ,"count"))]))
item237.data <- left_join(item237.data, item237.merge[which(colnames(item237.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"CK_SiteID"
                                                                                           ,"Ceiling.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"rvalue.bins"
                                                                                           ,"count"))])



################################
# Weighted Analysis
################################
item237.summary <- proportionRowsAndColumns1(CustomerLevelData     = item237.data
                                             , valueVariable       = 'count'
                                             , columnVariable      = 'Ceiling.Type'
                                             , rowVariable         = 'rvalue.bins'
                                             , aggregateColumnName = "All Types"
)
item237.summary <- item237.summary[which(item237.summary$Ceiling.Type != "All Types"),]

## Summary only for "All Frame Types"
item237.all.frame.types <- proportions_one_group(CustomerLevelData = item237.data
                                                 ,valueVariable    = "count"
                                                 ,groupingVariable = "rvalue.bins"
                                                 ,total.name       = "All Types"
                                                 ,columnName       = "Ceiling.Type"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE
)

#merge together!
item237.final <- rbind.data.frame(item237.summary
                                  , item237.all.frame.types, stringsAsFactors = F)


##cast data
item237.cast <- dcast(setDT(item237.final),
                      formula   = BuildingType + Ceiling.Type ~ rvalue.bins,
                      value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

#join all insulation levels onto rvalue summary
item237.table <- data.frame("BuildingType"                   = item237.cast$BuildingType
                            ,"Ceiling.Type"                  = item237.cast$Ceiling.Type
                            ,"Percent.R0.R10"                = item237.cast$w.percent_R0.R10
                            ,"SE.R0.R10"                     = item237.cast$w.SE_R0.R10
                            ,"Percent.R11.R15"               = item237.cast$w.percent_R11.R15
                            ,"SE.R11.R15"                    = item237.cast$w.SE_R11.R15
                            ,"Percent.R16.R20"               = item237.cast$w.percent_R16.R20
                            ,"SE.R16.R20"                    = item237.cast$w.SE_R16.R20
                            ,"Percent.R21.R25"               = item237.cast$w.percent_R21.R25
                            ,"SE.R21.R25"                    = item237.cast$w.SE_R21.R25
                            ,"Percent.R26.R30"               = item237.cast$w.percent_R26.R30
                            ,"SE.R26.R30"                    = item237.cast$w.SE_R26.R30
                            ,"Percent.R31.R40"               = item237.cast$w.percent_R31.R40
                            ,"SE.R31.R40"                    = item237.cast$w.SE_R31.R40
                            ,"Percent.R41.R50"               = item237.cast$w.percent_R41.R50
                            ,"SE.R41.R50"                    = item237.cast$w.SE_R41.R50
                            ,"Percent.RGT50"                 = item237.cast$w.percent_RGT50
                            ,"SE.RGT50"                      = item237.cast$w.SE_RGT50
                            ,"n"                             = item237.cast$n_Total
                            ,"EB.R0.R10"                     = item237.cast$EB_R0.R10
                            ,"EB.R11.R15"                    = item237.cast$EB_R11.R15
                            ,"EB.R16.R20"                    = item237.cast$EB_R16.R20
                            ,"EB.R21.R25"                    = item237.cast$EB_R21.R25
                            ,"EB.R26.R30"                    = item237.cast$EB_R26.R30
                            ,"EB.R31.R40"                    = item237.cast$EB_R31.R40
                            ,"EB.R41.R50"                    = item237.cast$EB_R41.R50
                            ,"EB.RGT50"                      = item237.cast$EB_RGT50
)

# row ordering example code
unique(item237.table$Ceiling.Type)
rowOrder <- c("Attic"
              ,"Roof Deck"
              ,"Sloped / Vaulted (no attic)"
              ,"Other"
              ,"All Types")
item237.table <- item237.table %>% mutate(Ceiling.Type = factor(Ceiling.Type, levels = rowOrder)) %>% arrange(Ceiling.Type)
item237.table <- data.frame(item237.table)


item237.table.MF <- item237.table[which(item237.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item237.table.MF, "MF", "Table 29", weighted = TRUE)



################################
# Unweighted Analysis
################################
item237.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item237.data
                                                     , valueVariable       = 'count'
                                                     , columnVariable      = 'Ceiling.Type'
                                                     , rowVariable         = 'rvalue.bins'
                                                     , aggregateColumnName = "All Types"
)
item237.summary <- item237.summary[which(item237.summary$Ceiling.Type != "All Types"),]

## Summary only for "All Frame Types"
item237.all.frame.types <- proportions_one_group(CustomerLevelData = item237.data
                                                 ,valueVariable    = "count"
                                                 ,groupingVariable = "rvalue.bins"
                                                 ,total.name       = "All Types"
                                                 ,columnName       = "Ceiling.Type"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE
)

#merge together!
item237.final <- rbind.data.frame(item237.summary
                                  , item237.all.frame.types, stringsAsFactors = F)


##cast data
item237.cast <- dcast(setDT(item237.final),
                      formula   = BuildingType + Ceiling.Type ~ rvalue.bins,
                      value.var = c("Percent", "SE", "Count", "n"))

#join all insulation levels onto rvalue summary
item237.table <- data.frame("BuildingType"                   = item237.cast$BuildingType
                            ,"Ceiling.Type"                  = item237.cast$Ceiling.Type
                            ,"Percent.R0.R10"                = item237.cast$Percent_R0.R10
                            ,"SE.R0.R10"                     = item237.cast$SE_R0.R10
                            ,"Percent.R11.R15"               = item237.cast$Percent_R11.R15
                            ,"SE.R11.R15"                    = item237.cast$SE_R11.R15
                            ,"Percent.R16.R20"               = item237.cast$Percent_R16.R20
                            ,"SE.R16.R20"                    = item237.cast$SE_R16.R20
                            ,"Percent.R21.R25"               = item237.cast$Percent_R21.R25
                            ,"SE.R21.R25"                    = item237.cast$SE_R21.R25
                            ,"Percent.R26.R30"               = item237.cast$Percent_R26.R30
                            ,"SE.R26.R30"                    = item237.cast$SE_R26.R30
                            ,"Percent.R31.R40"               = item237.cast$Percent_R31.R40
                            ,"SE.R31.R40"                    = item237.cast$SE_R31.R40
                            ,"Percent.R41.R50"               = item237.cast$Percent_R41.R50
                            ,"SE.R41.R50"                    = item237.cast$SE_R41.R50
                            ,"Percent.RGT50"                 = item237.cast$Percent_RGT50
                            ,"SE.RGT50"                      = item237.cast$SE_RGT50
                            ,"n"                             = item237.cast$n_Total
)

# row ordering example code
unique(item237.table$Ceiling.Type)
rowOrder <- c("Attic"
              ,"Roof Deck"
              ,"Sloped / Vaulted (no attic)"
              ,"Other"
              ,"All Types")
item237.table <- item237.table %>% mutate(Ceiling.Type = factor(Ceiling.Type, levels = rowOrder)) %>% arrange(Ceiling.Type)
item237.table <- data.frame(item237.table)


item237.table.MF <- item237.table[which(item237.table$BuildingType == "Multifamily"),-1]


#export table to correct workbook using exporting function
exportTable(item237.table.MF, "MF", "Table 29", weighted = FALSE)


