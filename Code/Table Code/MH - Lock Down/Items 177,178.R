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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
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
#Item 177: DISTRIBUTION OF CEILING INSULATION (MH TABLE 20)
#############################################################################################
item177.dat <- prep.dat7[which(prep.dat7$BuildingType == "Manufactured"),]

#
item177.dat$rvalue.bins <- "Unknown"
item177.dat$rvalue.bins[which(item177.dat$aveRval >= 0  & item177.dat$aveRval <=  8)]  <- "R0.R8"
item177.dat$rvalue.bins[which(item177.dat$aveRval > 8  & item177.dat$aveRval <= 14)]  <- "R9.R14"
item177.dat$rvalue.bins[which(item177.dat$aveRval > 14 & item177.dat$aveRval <= 21)]  <- "R15.R21"
item177.dat$rvalue.bins[which(item177.dat$aveRval > 21  & item177.dat$aveRval <= 30)]  <- "R22.R30"
item177.dat$rvalue.bins[which(item177.dat$aveRval > 30)]  <- "R31.R40"
unique(item177.dat$rvalue.bins)

item177.merge <- left_join(rbsa.dat, item177.dat)
item177.merge <- item177.merge[which(!is.na(item177.merge$rvalue.bins)),]
item177.merge <- item177.merge[which(!is.na(item177.merge$HomeYearBuilt_bins2)),]


##########################################
# add pop and sample sizes by strata
##########################################
item177.data <- weightedData(item177.merge[-which(colnames(item177.merge) %in% c("Ceiling.Type"
                                                                                 ,"aveUval"
                                                                                 ,"aveRval"
                                                                                 ,"rvalue.bins"))])
item177.data <- left_join(item177.data, item177.merge[which(colnames(item177.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ceiling.Type"
                                                                                           ,"aveUval"
                                                                                           ,"aveRval"
                                                                                           ,"rvalue.bins"))])
item177.data$count <- 1
##############################
# Weighted Analysis
##############################
item177.summary <- proportionRowsAndColumns1(CustomerLevelData     = item177.data
                                             , valueVariable       = 'count'
                                             , columnVariable      = 'HomeYearBuilt_bins2'
                                             , rowVariable         = 'rvalue.bins'
                                             , aggregateColumnName = "All Housing Vintages"
)
item177.summary <- item177.summary[which(item177.summary$rvalue.bins != "Total"),]
item177.summary <- item177.summary[which(item177.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]

## Summary only for "All Vintages"
item177.all.vintages <- proportions_one_group(item177.data
                                              ,valueVariable    = "count"
                                              ,groupingVariable = "rvalue.bins"
                                              ,total.name       = "All Housing Vintages"
                                              ,columnName       = "HomeYearBuilt_bins2"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
item177.all.vintages$rvalue.bins[which(item177.all.vintages$rvalue.bins == "Total")] <- "All Ceilings"

## Summary for only "All Ceilings"
item177.all.ceilings <-  proportions_one_group(item177.data
                                               ,valueVariable    = "count"
                                               ,groupingVariable = "HomeYearBuilt_bins2"
                                               ,total.name       = "All Ceilings"
                                               ,columnName       = "rvalue.bins"
                                               ,weighted = TRUE
                                               ,two.prop.total = TRUE)
item177.all.ceilings <-  item177.all.ceilings[which(item177.all.ceilings$HomeYearBuilt_bins2 != "Total"),]


#merge together!
item177.final <- rbind.data.frame(item177.summary
                                  , item177.all.ceilings
                                  , item177.all.vintages
                                  , stringsAsFactors = F)

item177.cast <- dcast(setDT(item177.final),
                      formula   = BuildingType +  HomeYearBuilt_bins2 ~ rvalue.bins,
                      value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item177.table <- data.frame("BuildingType"          = item177.cast$BuildingType
                            ,"Housing.Vintage"      = item177.cast$HomeYearBuilt_bins2
                            ,"Percent.R0.R8"        = item177.cast$w.percent_R0.R8
                            ,"SE.R0.R8"             = item177.cast$w.SE_R0.R8
                            ,"Percent.R9.R14"       = item177.cast$w.percent_R9.R14
                            ,"SE.R9.R14"            = item177.cast$w.SE_R9.R14
                            ,"Percent.R15.R21"      = item177.cast$w.percent_R15.R21
                            ,"SE.R15.R21"           = item177.cast$w.SE_R15.R21
                            ,"Percent.R22.R30"      = item177.cast$w.percent_R22.R30
                            ,"SE.R22.R30"           = item177.cast$w.SE_R22.R30
                            ,"Percent.R31.R40"      = item177.cast$w.percent_R31.R40
                            ,"SE.R31.R40"           = item177.cast$w.SE_R31.R40
                            ,"Percent.All.Ceilings" = item177.cast$`w.percent_All Ceilings`
                            ,"SE.All.Ceilings"      = item177.cast$`w.SE_All Ceilings`
                            ,"n" = item177.cast$`n_All Ceilings`
                            ,"EB.R0.R8"             = item177.cast$EB_R0.R8
                            ,"EB.R9.R14"            = item177.cast$EB_R9.R14
                            ,"EB.R15.R21"           = item177.cast$EB_R15.R21
                            ,"EB.R22.R30"           = item177.cast$EB_R22.R30
                            ,"EB.R31.R40"           = item177.cast$EB_R31.R40
                            ,"EB.All.Ceilings"      = item177.cast$`EB_All Ceilings`
)

levels(item177.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item177.table <- item177.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)
item177.table <- data.frame(item177.table)

item177.table.MH <- item177.table[which(item177.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item177.table.MH, "MH", "Table 20", weighted = TRUE)

############################################################################################################
# Unweighted - Manufactured
############################################################################################################
item177.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item177.data
                                                     , valueVariable       = 'count'
                                                     , columnVariable      = 'HomeYearBuilt_bins2'
                                                     , rowVariable         = 'rvalue.bins'
                                                     , aggregateColumnName = "All Housing Vintages"
)
item177.summary <- item177.summary[which(item177.summary$rvalue.bins != "Total"),]
item177.summary <- item177.summary[which(item177.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]

## Summary only for "All Vintages"
item177.all.vintages <- proportions_one_group(item177.data
                                              ,valueVariable    = "count"
                                              ,groupingVariable = "rvalue.bins"
                                              ,total.name       = "All Housing Vintages"
                                              ,columnName       = "HomeYearBuilt_bins2"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item177.all.vintages$rvalue.bins[which(item177.all.vintages$rvalue.bins == "Total")] <- "All Ceilings"

## Summary for only "All Ceilings"
item177.all.ceilings <-  proportions_one_group(item177.data
                                               ,valueVariable    = "count"
                                               ,groupingVariable = "HomeYearBuilt_bins2"
                                               ,total.name       = "All Ceilings"
                                               ,columnName       = "rvalue.bins"
                                               ,weighted = FALSE
                                               ,two.prop.total = TRUE)
item177.all.ceilings <-  item177.all.ceilings[which(item177.all.ceilings$HomeYearBuilt_bins2 != "Total"),]


#merge together!
item177.final <- rbind.data.frame(item177.summary
                                  , item177.all.ceilings
                                  , item177.all.vintages
                                  , stringsAsFactors = F)

item177.cast <- dcast(setDT(item177.final),
                      formula   = BuildingType +  HomeYearBuilt_bins2 ~ rvalue.bins,
                      value.var = c("Percent", "SE", "Count", "n"))

item177.table <- data.frame("BuildingType"          = item177.cast$BuildingType
                            ,"Housing.Vintage"      = item177.cast$HomeYearBuilt_bins2
                            ,"Percent.R0.R8"        = item177.cast$Percent_R0.R8
                            ,"SE.R0.R8"             = item177.cast$SE_R0.R8
                            ,"Percent.R9.R14"       = item177.cast$Percent_R9.R14
                            ,"SE.R9.R14"            = item177.cast$SE_R9.R14
                            ,"Percent.R15.R21"      = item177.cast$Percent_R15.R21
                            ,"SE.R15.R21"           = item177.cast$SE_R15.R21
                            ,"Percent.R22.R30"      = item177.cast$Percent_R22.R30
                            ,"SE.R22.R30"           = item177.cast$SE_R22.R30
                            ,"Percent.R31.R40"      = item177.cast$Percent_R31.R40
                            ,"SE.R31.R40"           = item177.cast$SE_R31.R40
                            ,"Percent.All.Ceilings" = item177.cast$`Percent_All Ceilings`
                            ,"SE.All.Ceilings"      = item177.cast$`SE_All Ceilings`
                            ,"n" = item177.cast$`n_All Ceilings`
)

levels(item177.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Housing Vintages")
item177.table <- item177.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)
item177.table <- data.frame(item177.table)

item177.table.MH <- item177.table[which(item177.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item177.table.MH, "MH", "Table 20", weighted = FALSE)







#############################################################################################
#Item 178: DISTRIBUTION OF CEILING U-VALUE BY STATE (MH TABLE 21)
#############################################################################################
item178.dat <- envelope.dat[which(names(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.U-Value.-.For.Calcs"))]
item178.dat$`Ceiling.U-Value.-.For.Calcs` <- as.numeric(as.character(item178.dat$`Ceiling.U-Value.-.For.Calcs`))

item178.dat1 <- item178.dat[which(!is.na(item178.dat$`Ceiling.U-Value.-.For.Calcs`)),]

item178.summary <- data.frame(ddply(item178.dat1
                                    ,c("CK_Cadmus_ID"), summarise
                                    ,aveUval = mean(`Ceiling.U-Value.-.For.Calcs`)), stringsAsFactors = F)


item178.summary$count <- 1
colnames(item178.summary)

item178.merge <- left_join(rbsa.dat, item178.summary)
item178.merge <- item178.merge[which(!is.na(item178.merge$aveUval)),]

################################################
# Adding pop and sample sizes for weights
################################################
item178.data <- weightedData(item178.merge[-which(colnames(item178.merge) %in% c("aveUval"
                                                                                 ,"count"))])
item178.data <- left_join(item178.data, item178.merge[which(colnames(item178.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"aveUval"
                                                                                           ,"count"))])

#######################
# Weighted Analysis
#######################
item178.final <- mean_one_group(item178.data
                                ,valueVariable = 'aveUval'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item178.final.MH <- item178.final[which(item178.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item178.final) %in% c("BuildingType"))]

exportTable(item178.final.MH, "MH", "Table 21", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item178.final <- mean_one_group_unweighted(item178.data
                                           ,valueVariable = 'aveUval'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item178.final.MH <- item178.final[which(item178.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item178.final) %in% c("BuildingType"))]

exportTable(item178.final.MH, "MH", "Table 21", weighted = FALSE)
