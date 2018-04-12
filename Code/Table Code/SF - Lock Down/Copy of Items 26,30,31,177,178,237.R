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
# #weight the u factor per home -- where weights are the wall area within home
# weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID, Ceiling.Type)
#                        ,aveUval = sum(Ceiling.Area * Ceiling.Insulation.Condition.1 * uvalue) / sum(Ceiling.Area * Ceiling.Insulation.Condition.1)
# )
# 
# #back-calculate the weight r values
# weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
# weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
# weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
# unique(weightedU$aveRval)
# 
# # get unique cadmus IDs and building types for this subset of data
# wall.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])
# 
# # merge on ID and building types to weighted uFactor and rValue data
# prep.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")
# 
# #merge weighted u values onto cleaned RBSA data
# prep.dat7 <- left_join(prep.dat6, rbsa.dat)
# prep.dat7$aveUval[which(is.na(prep.dat7$aveUval))] <- 0
# prep.dat7$aveRval[which(is.na(prep.dat7$aveRval))] <- 0
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














############################################################################################################
## Item 26
############################################################################################################
item26.dat <- prep.dat5[which(prep.dat5$Ceiling.Type == "Attic"),]

item26.merge <- left_join(rbsa.dat, item26.dat)
item26.merge <- item26.merge[which(!is.na(item26.merge$total.r.val)),]

item26.merge$total.r.val <- item26.merge$total.r.val #* item26.merge$Ceiling.Insulation.Condition.1


#Bin R values -- SF only
item26.merge$rvalue.bins <- "Unknown"
item26.merge$rvalue.bins[which(item26.merge$total.r.val == 0)] <- "R0"
item26.merge$rvalue.bins[which(item26.merge$total.r.val >  0  & item26.merge$total.r.val < 11)]  <- "R1.R10"
item26.merge$rvalue.bins[which(item26.merge$total.r.val >= 11 & item26.merge$total.r.val < 16)]  <- "R11.R15"
item26.merge$rvalue.bins[which(item26.merge$total.r.val >= 16 & item26.merge$total.r.val < 21)]  <- "R16.R20"
item26.merge$rvalue.bins[which(item26.merge$total.r.val >= 21 & item26.merge$total.r.val < 26)]  <- "R21.R25"
item26.merge$rvalue.bins[which(item26.merge$total.r.val >= 26 & item26.merge$total.r.val < 31)]  <- "R26.R30"
item26.merge$rvalue.bins[which(item26.merge$total.r.val >= 31 & item26.merge$total.r.val < 41)]  <- "R31.R40"
item26.merge$rvalue.bins[which(item26.merge$total.r.val >= 41 & item26.merge$total.r.val < 51)]  <- "R41.R50"
item26.merge$rvalue.bins[which(item26.merge$total.r.val >= 51)] <- "RGT50"
unique(item26.merge$rvalue.bins)

item26.merge$count <- 1


item26.data <- weightedData(item26.merge[-which(colnames(item26.merge) %in% c("CK_SiteID"
                                                                              ,"PK_Envelope_ID"
                                                                              ,"Category"
                                                                              ,"Ceiling.Type"
                                                                              ,"Ceiling.Area"
                                                                              ,"Ceiling.Insulated?"
                                                                              ,"Ceiling.Insulation.Type.1"
                                                                              ,"Ceiling.Insulation.Thickness.1"
                                                                              ,"Ceiling.Insulation.Condition.1"
                                                                              ,"Ceiling.Insulation.Type.2"
                                                                              ,"Ceiling.Insulation.Thickness.2"
                                                                              ,"Ceiling.Insulation.Condition.2"
                                                                              ,"Ceiling.Insulation.Type.3"
                                                                              ,"Ceiling.Insulation.Thickness.3"
                                                                              ,"Ceiling.Insulation.Condition.3"
                                                                              ,"ceiling.inches1"
                                                                              ,"ceiling.inches2"
                                                                              ,"ceiling.inches3"
                                                                              ,"ceiling.rvalues1"
                                                                              ,"ceiling.rvalues2"
                                                                              ,"ceiling.rvalues3"
                                                                              ,"total.r.val"
                                                                              ,"uvalue"
                                                                              ,"rvalue.bins"
                                                                              ,"count"))])
item26.data <- left_join(item26.data, item26.merge[which(colnames(item26.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"CK_Building_ID"
                                                                                       ,"CK_SiteID"
                                                                                       ,"PK_Envelope_ID"
                                                                                       ,"Category"
                                                                                       ,"Ceiling.Type"
                                                                                       ,"Ceiling.Area"
                                                                                       ,"Ceiling.Insulated?"
                                                                                       ,"Ceiling.Insulation.Type.1"
                                                                                       ,"Ceiling.Insulation.Thickness.1"
                                                                                       ,"Ceiling.Insulation.Condition.1"
                                                                                       ,"Ceiling.Insulation.Type.2"
                                                                                       ,"Ceiling.Insulation.Thickness.2"
                                                                                       ,"Ceiling.Insulation.Condition.2"
                                                                                       ,"Ceiling.Insulation.Type.3"
                                                                                       ,"Ceiling.Insulation.Thickness.3"
                                                                                       ,"Ceiling.Insulation.Condition.3"
                                                                                       ,"ceiling.inches1"
                                                                                       ,"ceiling.inches2"
                                                                                       ,"ceiling.inches3"
                                                                                       ,"ceiling.rvalues1"
                                                                                       ,"ceiling.rvalues2"
                                                                                       ,"ceiling.rvalues3"
                                                                                       ,"total.r.val"
                                                                                       ,"uvalue"
                                                                                       ,"rvalue.bins"
                                                                                       ,"count"))])






##############################
# Weighted Analysis
##############################
item26.final <- proportions_one_group(CustomerLevelData = item26.data
                                      ,valueVariable    = 'Ceiling.Area'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = TRUE)
item26.final.SF <- item26.final[which(item26.final$BuildingType == "Single Family")
                                ,-which(colnames(item26.final) %in% c("BuildingType"))]
exportTable(item26.final.SF, "SF", "Table 33", weighted = TRUE)

##############################
# Unweighted Analysis
##############################
item26.final <- proportions_one_group(CustomerLevelData = item26.data
                                      ,valueVariable    = 'Ceiling.Area'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = FALSE)
item26.final.SF <- item26.final[which(item26.final$BuildingType == "Single Family")
                                ,-which(colnames(item26.final) %in% c("BuildingType"))]
exportTable(item26.final.SF, "SF", "Table 33", weighted = FALSE)








############################################################################################################
## Item 30
############################################################################################################
item30.dat <- prep.dat7[which(prep.dat7$Ceiling.Type == "Sloped / Vaulted (no attic)"),]

item30.data <- weightedData(item30.dat[-which(colnames(item30.dat) %in% c("Ceiling.Type"
                                                                          ,"aveUval"
                                                                          ,"aveRval"))])
item30.data <- left_join(item30.data, item30.dat[which(colnames(item30.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"Ceiling.Type"
                                                                                   ,"aveUval"
                                                                                   ,"aveRval"))])


#Bin R values -- SF only
item30.data$rvalue.bins <- "Unknown"
item30.data$rvalue.bins[which(item30.data$aveRval == 0)] <- "R0"
item30.data$rvalue.bins[which(item30.data$aveRval >  0 & item30.data$aveRval < 16)]   <- "R1.R15"
item30.data$rvalue.bins[which(item30.data$aveRval >= 16 & item30.data$aveRval < 21)]  <- "R16.R20"
item30.data$rvalue.bins[which(item30.data$aveRval >= 21 & item30.data$aveRval < 26)]  <- "R21.R25"
item30.data$rvalue.bins[which(item30.data$aveRval >= 26 & item30.data$aveRval < 31)]  <- "R26.R30"
item30.data$rvalue.bins[which(item30.data$aveRval >= 31 & item30.data$aveRval < 41)]  <- "R31.R40"
item30.data$rvalue.bins[which(item30.data$aveRval >= 41)]  <- "R41.R50"
unique(item30.data$rvalue.bins)

item30.data$count <- 1



##############################
# Weighted Analysis
##############################
item30.final <- proportions_one_group(CustomerLevelData = item30.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = TRUE)
item30.final.SF <- item30.final[which(item30.final$BuildingType == "Single Family")
                                ,-which(colnames(item30.final) %in% c("BuildingType"))]
exportTable(item30.final.SF, "SF", "Table 37", weighted = TRUE)

##############################
# Unweighted Analysis
##############################
item30.final <- proportions_one_group(CustomerLevelData = item30.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'rvalue.bins'
                                      ,total.name       = "Total"
                                      ,weighted         = FALSE)


item30.final.SF <- item30.final[which(item30.final$BuildingType == "Single Family")
                                ,-which(colnames(item30.final) %in% c("BuildingType"))]
exportTable(item30.final.SF, "SF", "Table 37", weighted = FALSE)







# ############################################################################################################
# ## Item 31
# ############################################################################################################
# item31.dat <- prep.dat5[which(prep.dat5$Ceiling.Type == "Roof Deck"),]
# item31.dat$count <- 1
# 
# item31.dat0 <- item31.dat[which(item31.dat$Ceiling.Insulation.Thickness.1 != "N/A N/A"),]
# 
# item31.dat1 <- left_join(rbsa.dat, item31.dat0)
# item31.dat2 <- item31.dat1[which(!is.na(item31.dat1$uvalue)),]
# 
# item31.data <- weightedData(item31.dat2[-c(grep("Ceiling", colnames(item31.dat2),ignore.case = T)
#                                            ,which(colnames(item31.dat2) %in% c("Category"
#                                                                                ,"CK_SiteID"
#                                                                                ,"count"
#                                                                                ,"uvalue"
#                                                                                ,"total.r.val"
#                                                                                ,"TMP_ID"
#                                                                                ,"PK_Envelope_ID"
#                                                                                ,"")))])
# item31.data <- left_join(item31.data,item31.dat2[c(grep("Ceiling|ceiling", colnames(item31.dat2))
#                                                    ,which(colnames(item31.dat2) %in% c("CK_Cadmus_ID"
#                                                                                        ,"CK_Site_ID"
#                                                                                        ,"Category"
#                                                                                        ,"count"
#                                                                                        ,"uvalue"
#                                                                                        ,"total.r.val"
#                                                                                        ,"TMP_ID"
#                                                                                        ,"PK_Envelope_ID")))] )
# 
# ##############################
# # Weighted Analysis
# ##############################
# item31.final <- proportions_one_group(CustomerLevelData = item31.data
#                                       ,valueVariable    = 'count'
#                                       ,groupingVariable = 'Ceiling.Insulation.Thickness.1'
#                                       ,total.name       = "Total"
#                                       ,weighted         = TRUE)
# item31.final.SF <- item31.final[which(item31.final$BuildingType == "Single Family")
#                                 , -which(colnames(item31.final) %in% c("BuildingType"))]
# exportTable(item31.final.SF, "SF", "Table 38", weighted = TRUE)
# 
# ##############################
# # Unweighted Analysis
# ##############################
# item31.final <- proportions_one_group(CustomerLevelData = item31.data
#                                       ,valueVariable    = 'count'
#                                       ,groupingVariable = 'Ceiling.Insulation.Thickness.1'
#                                       ,total.name       = "Total"
#                                       ,weighted         = FALSE)
# item31.final.SF <- item31.final[which(item31.final$BuildingType == "Single Family")
#                                 , -which(colnames(item31.final) %in% c("BuildingType"))]
# exportTable(item31.final.SF, "SF", "Table 38", weighted = FALSE)
















# #############################################################################################
# #Item 177: DISTRIBUTION OF CEILING INSULATION (MH TABLE 20)
# #############################################################################################
# item177.dat <- prep.dat7[which(prep.dat7$BuildingType == "Manufactured"),]
# 
# #
# item177.dat$rvalue.bins <- "Unknown"
# item177.dat$rvalue.bins[which(item177.dat$aveRval >= 0  & item177.dat$aveRval <=  8)]  <- "R0.R8"
# item177.dat$rvalue.bins[which(item177.dat$aveRval > 8  & item177.dat$aveRval <= 14)]  <- "R9.R14"
# item177.dat$rvalue.bins[which(item177.dat$aveRval > 14 & item177.dat$aveRval <= 21)]  <- "R15.R21"
# item177.dat$rvalue.bins[which(item177.dat$aveRval > 21  & item177.dat$aveRval <= 30)]  <- "R22.R30"
# item177.dat$rvalue.bins[which(item177.dat$aveRval > 30)]  <- "R31.R40"
# unique(item177.dat$rvalue.bins)
# 
# item177.merge <- left_join(rbsa.dat, item177.dat)
# item177.merge <- item177.merge[which(!is.na(item177.merge$rvalue.bins)),]
# item177.merge <- item177.merge[which(!is.na(item177.merge$HomeYearBuilt_bins2)),]
# 
# 
# ##########################################
# # add pop and sample sizes by strata
# ##########################################
# item177.data <- weightedData(item177.merge[-which(colnames(item177.merge) %in% c("Ceiling.Type"
#                                                                                  ,"aveUval"
#                                                                                  ,"aveRval"
#                                                                                  ,"rvalue.bins"))])
# item177.data <- left_join(item177.data, item177.merge[which(colnames(item177.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ceiling.Type"
#                                                                                            ,"aveUval"
#                                                                                            ,"aveRval"
#                                                                                            ,"rvalue.bins"))])
# item177.data$count <- 1
# ##############################
# # Weighted Analysis
# ##############################
# item177.summary <- proportionRowsAndColumns1(CustomerLevelData     = item177.data
#                                              , valueVariable       = 'count'
#                                              , columnVariable      = 'HomeYearBuilt_bins2'
#                                              , rowVariable         = 'rvalue.bins'
#                                              , aggregateColumnName = "All Housing Vintages"
# )
# item177.summary <- item177.summary[which(item177.summary$rvalue.bins != "Total"),]
# item177.summary <- item177.summary[which(item177.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]
# 
# ## Summary only for "All Vintages"
# item177.all.vintages <- proportions_one_group(item177.data
#                                               ,valueVariable    = "count"
#                                               ,groupingVariable = "rvalue.bins"
#                                               ,total.name       = "All Housing Vintages"
#                                               ,columnName       = "HomeYearBuilt_bins2"
#                                               ,weighted = TRUE
#                                               ,two.prop.total = TRUE)
# item177.all.vintages$rvalue.bins[which(item177.all.vintages$rvalue.bins == "Total")] <- "All Ceilings"
# 
# ## Summary for only "All Ceilings"
# item177.all.ceilings <-  proportions_one_group(item177.data
#                                                ,valueVariable    = "count"
#                                                ,groupingVariable = "HomeYearBuilt_bins2"
#                                                ,total.name       = "All Ceilings"
#                                                ,columnName       = "rvalue.bins"
#                                                ,weighted = TRUE
#                                                ,two.prop.total = TRUE)
# item177.all.ceilings <-  item177.all.ceilings[which(item177.all.ceilings$HomeYearBuilt_bins2 != "Total"),]
# 
# 
# #merge together!
# item177.final <- rbind.data.frame(item177.summary
#                                   , item177.all.ceilings
#                                   , item177.all.vintages
#                                   , stringsAsFactors = F)
# 
# item177.cast <- dcast(setDT(item177.final),
#                       formula   = BuildingType +  HomeYearBuilt_bins2 ~ rvalue.bins,
#                       value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# item177.table <- data.frame("BuildingType"          = item177.cast$BuildingType
#                             ,"Housing.Vintage"      = item177.cast$HomeYearBuilt_bins2
#                             ,"Percent.R0.R8"        = item177.cast$w.percent_R0.R8
#                             ,"SE.R0.R8"             = item177.cast$w.SE_R0.R8
#                             ,"Percent.R9.R14"       = item177.cast$w.percent_R9.R14
#                             ,"SE.R9.R14"            = item177.cast$w.SE_R9.R14
#                             ,"Percent.R15.R21"      = item177.cast$w.percent_R15.R21
#                             ,"SE.R15.R21"           = item177.cast$w.SE_R15.R21
#                             ,"Percent.R22.R30"      = item177.cast$w.percent_R22.R30
#                             ,"SE.R22.R30"           = item177.cast$w.SE_R22.R30
#                             ,"Percent.R31.R40"      = item177.cast$w.percent_R31.R40
#                             ,"SE.R31.R40"           = item177.cast$w.SE_R31.R40
#                             ,"Percent.All.Ceilings" = item177.cast$`w.percent_All Ceilings`
#                             ,"SE.All.Ceilings"      = item177.cast$`w.SE_All Ceilings`
#                             ,"n" = item177.cast$`n_All Ceilings`
#                             ,"EB.R0.R8"             = item177.cast$EB_R0.R8
#                             ,"EB.R9.R14"            = item177.cast$EB_R9.R14
#                             ,"EB.R15.R21"           = item177.cast$EB_R15.R21
#                             ,"EB.R22.R30"           = item177.cast$EB_R22.R30
#                             ,"EB.R31.R40"           = item177.cast$EB_R31.R40
#                             ,"EB.All.Ceilings"      = item177.cast$`EB_All Ceilings`
# )
# 
# levels(item177.table$Housing.Vintage)
# rowOrder <- c("Pre 1951"
#               ,"1951-1960"
#               ,"1961-1970"
#               ,"1971-1980"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"All Housing Vintages")
# item177.table <- item177.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
# item177.table <- data.frame(item177.table)
# 
# item177.table.MH <- item177.table[which(item177.table$BuildingType == "Manufactured"),-1]
# 
# #export table to correct workbook using exporting function
# exportTable(item177.table.MH, "MH", "Table 20", weighted = TRUE)
# 
# ############################################################################################################
# # Unweighted - Manufactured
# ############################################################################################################
# item177.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item177.data
#                                                      , valueVariable       = 'count'
#                                                      , columnVariable      = 'HomeYearBuilt_bins2'
#                                                      , rowVariable         = 'rvalue.bins'
#                                                      , aggregateColumnName = "All Housing Vintages"
# )
# item177.summary <- item177.summary[which(item177.summary$rvalue.bins != "Total"),]
# item177.summary <- item177.summary[which(item177.summary$HomeYearBuilt_bins2 != "All Housing Vintages"),]
# 
# ## Summary only for "All Vintages"
# item177.all.vintages <- proportions_one_group(item177.data
#                                               ,valueVariable    = "count"
#                                               ,groupingVariable = "rvalue.bins"
#                                               ,total.name       = "All Housing Vintages"
#                                               ,columnName       = "HomeYearBuilt_bins2"
#                                               ,weighted = FALSE
#                                               ,two.prop.total = TRUE)
# item177.all.vintages$rvalue.bins[which(item177.all.vintages$rvalue.bins == "Total")] <- "All Ceilings"
# 
# ## Summary for only "All Ceilings"
# item177.all.ceilings <-  proportions_one_group(item177.data
#                                                ,valueVariable    = "count"
#                                                ,groupingVariable = "HomeYearBuilt_bins2"
#                                                ,total.name       = "All Ceilings"
#                                                ,columnName       = "rvalue.bins"
#                                                ,weighted = FALSE
#                                                ,two.prop.total = TRUE)
# item177.all.ceilings <-  item177.all.ceilings[which(item177.all.ceilings$HomeYearBuilt_bins2 != "Total"),]
# 
# 
# #merge together!
# item177.final <- rbind.data.frame(item177.summary
#                                   , item177.all.ceilings
#                                   , item177.all.vintages
#                                   , stringsAsFactors = F)
# 
# item177.cast <- dcast(setDT(item177.final),
#                       formula   = BuildingType +  HomeYearBuilt_bins2 ~ rvalue.bins,
#                       value.var = c("Percent", "SE", "Count", "n"))
# 
# item177.table <- data.frame("BuildingType"          = item177.cast$BuildingType
#                             ,"Housing.Vintage"      = item177.cast$HomeYearBuilt_bins2
#                             ,"Percent.R0.R8"        = item177.cast$Percent_R0.R8
#                             ,"SE.R0.R8"             = item177.cast$SE_R0.R8
#                             ,"Percent.R9.R14"       = item177.cast$Percent_R9.R14
#                             ,"SE.R9.R14"            = item177.cast$SE_R9.R14
#                             ,"Percent.R15.R21"      = item177.cast$Percent_R15.R21
#                             ,"SE.R15.R21"           = item177.cast$SE_R15.R21
#                             ,"Percent.R22.R30"      = item177.cast$Percent_R22.R30
#                             ,"SE.R22.R30"           = item177.cast$SE_R22.R30
#                             ,"Percent.R31.R40"      = item177.cast$Percent_R31.R40
#                             ,"SE.R31.R40"           = item177.cast$SE_R31.R40
#                             ,"Percent.All.Ceilings" = item177.cast$`Percent_All Ceilings`
#                             ,"SE.All.Ceilings"      = item177.cast$`SE_All Ceilings`
#                             ,"n" = item177.cast$`n_All Ceilings`
# )
# 
# levels(item177.table$Housing.Vintage)
# rowOrder <- c("Pre 1951"
#               ,"1951-1960"
#               ,"1961-1970"
#               ,"1971-1980"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"All Housing Vintages")
# item177.table <- item177.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
# item177.table <- data.frame(item177.table)
# 
# item177.table.MH <- item177.table[which(item177.table$BuildingType == "Manufactured"),-1]
# 
# #export table to correct workbook using exporting function
# exportTable(item177.table.MH, "MH", "Table 20", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 178: DISTRIBUTION OF CEILING U-VALUE BY STATE (MH TABLE 21)
# #############################################################################################
# item178.dat <- envelope.dat[which(names(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.U-Value.-.For.Calcs"))]
# item178.dat$`Ceiling.U-Value.-.For.Calcs` <- as.numeric(as.character(item178.dat$`Ceiling.U-Value.-.For.Calcs`))
# 
# item178.dat1 <- item178.dat[which(!is.na(item178.dat$`Ceiling.U-Value.-.For.Calcs`)),]
# 
# item178.summary <- data.frame(ddply(item178.dat1
#                                     ,c("CK_Cadmus_ID"), summarise
#                                     ,aveUval = mean(`Ceiling.U-Value.-.For.Calcs`)), stringsAsFactors = F)
# 
# 
# item178.summary$count <- 1
# colnames(item178.summary)
# 
# item178.merge <- left_join(rbsa.dat, item178.summary)
# item178.merge <- item178.merge[which(!is.na(item178.merge$aveUval)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item178.data <- weightedData(item178.merge[-which(colnames(item178.merge) %in% c("aveUval"
#                                                                                  ,"count"))])
# item178.data <- left_join(item178.data, item178.merge[which(colnames(item178.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"aveUval"
#                                                                                            ,"count"))])
# 
# #######################
# # Weighted Analysis
# #######################
# item178.final <- mean_one_group(item178.data
#                                 ,valueVariable = 'aveUval'
#                                 ,byVariable = 'State'
#                                 ,aggregateRow = 'Region')
# 
# item178.final.MH <- item178.final[which(item178.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item178.final) %in% c("BuildingType"))]
# 
# exportTable(item178.final.MH, "MH", "Table 21", weighted = TRUE)
# 
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item178.final <- mean_one_group_unweighted(item178.data
#                                            ,valueVariable = 'aveUval'
#                                            ,byVariable = 'State'
#                                            ,aggregateRow = 'Region')
# 
# item178.final.MH <- item178.final[which(item178.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item178.final) %in% c("BuildingType"))]
# 
# exportTable(item178.final.MH, "MH", "Table 21", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 237: DISTRIBUTION OF CEILING INSULATION BY CEILING TYPE (MF TABLE 29)
# #############################################################################################
# #weight the u factor per home -- where weights are the wall area within home
# weightedU <- summarise(group_by(prep.dat5, CK_SiteID, Ceiling.Type)
#                        ,aveUval = sum(Ceiling.Area * Ceiling.Insulation.Condition.1 * uvalue) / sum(Ceiling.Area * Ceiling.Insulation.Condition.1)
# )
# unique(weightedU$aveUval)
# 
# 
# #back-calculate the weight r values
# weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval)))
# weightedU$aveRval[which(weightedU$aveRval %in% c("NaN",1))] <- 0
# weightedU$aveUval[which(weightedU$aveUval == "NaN")] <- 1
# unique(weightedU$aveRval)
# unique(weightedU$aveUval)
# 
# # get unique cadmus IDs and building types for this subset of data
# Ceiling.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_SiteID","BuildingType"))])
# 
# # merge on ID and building types to weighted uFactor and rValue data
# prep.dat6 <- left_join(weightedU, Ceiling.unique, by = "CK_SiteID")
# 
# #merge weighted u values onto cleaned RBSA data
# prep.dat7 <- left_join(prep.dat6, rbsa.dat.MF, by = c("CK_SiteID" = "CK_Building_ID"))
# prep.dat7 <- prep.dat7[grep("3 or fewer floors", prep.dat7$BuildingTypeXX),]
# item237.dat <- prep.dat7[which(!is.na(prep.dat7$CK_Cadmus_ID)),]
# 
# unique(item237.dat$Ceiling.Type)
# 
# #Bin R values -- MF only
# item237.dat$rvalue.bins <- "Unknown"
# item237.dat$rvalue.bins[which(item237.dat$aveRval >=  0  & item237.dat$aveRval < 11)]  <- "R0.R10"
# item237.dat$rvalue.bins[which(item237.dat$aveRval >= 11  & item237.dat$aveRval < 16)]  <- "R11.R15"
# item237.dat$rvalue.bins[which(item237.dat$aveRval >= 16  & item237.dat$aveRval < 21)]  <- "R16.R20"
# item237.dat$rvalue.bins[which(item237.dat$aveRval >= 21  & item237.dat$aveRval < 26)]  <- "R21.R25"
# item237.dat$rvalue.bins[which(item237.dat$aveRval >= 26  & item237.dat$aveRval < 31)]  <- "R26.R30"
# item237.dat$rvalue.bins[which(item237.dat$aveRval >= 31  & item237.dat$aveRval < 41)]  <- "R31.R40"
# item237.dat$rvalue.bins[which(item237.dat$aveRval >= 41  & item237.dat$aveRval < 51)]  <- "R41.R50"
# item237.dat$rvalue.bins[which(item237.dat$aveRval >= 51)] <- "RGT50"
# unique(item237.dat$rvalue.bins)
# 
# item237.dat$count <- 1
# 
# item237.dat1 <- item237.dat[which(item237.dat$rvalue.bins != "Unknown"),]
# colnames(item237.dat1)
# 
# item237.merge <- left_join(rbsa.dat.MF, item237.dat1)
# item237.merge <- item237.merge[which(!is.na(item237.merge$count)),]
# 
# item237.data <- weightedData(unique(item237.merge[-which(colnames(item237.merge) %in% c("CK_SiteID"
#                                                                                         ,"Ceiling.Type"
#                                                                                         ,"aveUval"
#                                                                                         ,"aveRval"
#                                                                                         ,"rvalue.bins"
#                                                                                         ,"count"))]))
# item237.data <- left_join(item237.data, item237.merge[which(colnames(item237.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"CK_SiteID"
#                                                                                            ,"Ceiling.Type"
#                                                                                            ,"aveUval"
#                                                                                            ,"aveRval"
#                                                                                            ,"rvalue.bins"
#                                                                                            ,"count"))])
# 
# 
# 
# ################################
# # Weighted Analysis
# ################################
# item237.summary <- proportionRowsAndColumns1(CustomerLevelData     = item237.data
#                                              , valueVariable       = 'count'
#                                              , columnVariable      = 'Ceiling.Type'
#                                              , rowVariable         = 'rvalue.bins'
#                                              , aggregateColumnName = "All Types"
# )
# item237.summary <- item237.summary[which(item237.summary$Ceiling.Type != "All Types"),]
# 
# ## Summary only for "All Frame Types"
# item237.all.frame.types <- proportions_one_group(CustomerLevelData = item237.data
#                                                  ,valueVariable    = "count"
#                                                  ,groupingVariable = "rvalue.bins"
#                                                  ,total.name       = "All Types"
#                                                  ,columnName       = "Ceiling.Type"
#                                                  ,weighted = TRUE
#                                                  ,two.prop.total = TRUE
# )
# 
# #merge together!
# item237.final <- rbind.data.frame(item237.summary
#                                   , item237.all.frame.types, stringsAsFactors = F)
# 
# 
# ##cast data
# item237.cast <- dcast(setDT(item237.final),
#                       formula   = BuildingType + Ceiling.Type ~ rvalue.bins,
#                       value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# #join all insulation levels onto rvalue summary
# item237.table <- data.frame("BuildingType"                   = item237.cast$BuildingType
#                             ,"Ceiling.Type"                  = item237.cast$Ceiling.Type
#                             ,"Percent.R0.R10"                = item237.cast$w.percent_R0.R10
#                             ,"SE.R0.R10"                     = item237.cast$w.SE_R0.R10
#                             ,"Percent.R11.R15"               = item237.cast$w.percent_R11.R15
#                             ,"SE.R11.R15"                    = item237.cast$w.SE_R11.R15
#                             ,"Percent.R16.R20"               = item237.cast$w.percent_R16.R20
#                             ,"SE.R16.R20"                    = item237.cast$w.SE_R16.R20
#                             ,"Percent.R21.R25"               = item237.cast$w.percent_R21.R25
#                             ,"SE.R21.R25"                    = item237.cast$w.SE_R21.R25
#                             ,"Percent.R26.R30"               = item237.cast$w.percent_R26.R30
#                             ,"SE.R26.R30"                    = item237.cast$w.SE_R26.R30
#                             ,"Percent.R31.R40"               = item237.cast$w.percent_R31.R40
#                             ,"SE.R31.R40"                    = item237.cast$w.SE_R31.R40
#                             ,"Percent.R41.R50"               = item237.cast$w.percent_R41.R50
#                             ,"SE.R41.R50"                    = item237.cast$w.SE_R41.R50
#                             ,"Percent.RGT50"                 = item237.cast$w.percent_RGT50
#                             ,"SE.RGT50"                      = item237.cast$w.SE_RGT50
#                             ,"n"                             = item237.cast$n_Total
#                             ,"EB.R0.R10"                     = item237.cast$EB_R0.R10
#                             ,"EB.R11.R15"                    = item237.cast$EB_R11.R15
#                             ,"EB.R16.R20"                    = item237.cast$EB_R16.R20
#                             ,"EB.R21.R25"                    = item237.cast$EB_R21.R25
#                             ,"EB.R26.R30"                    = item237.cast$EB_R26.R30
#                             ,"EB.R31.R40"                    = item237.cast$EB_R31.R40
#                             ,"EB.R41.R50"                    = item237.cast$EB_R41.R50
#                             ,"EB.RGT50"                      = item237.cast$EB_RGT50
# )
# 
# # row ordering example code
# unique(item237.table$Ceiling.Type)
# rowOrder <- c("Attic"
#               ,"Roof Deck"
#               ,"Sloped / Vaulted (no attic)"
#               ,"Other"
#               ,"All Types")
# item237.table <- item237.table %>% mutate(Ceiling.Type = factor(Ceiling.Type, levels = rowOrder)) %>% arrange(Ceiling.Type)  
# item237.table <- data.frame(item237.table)
# 
# 
# item237.table.MF <- item237.table[which(item237.table$BuildingType == "Multifamily"),-1]
# 
# 
# #export table to correct workbook using exporting function
# exportTable(item237.table.MF, "MF", "Table 29", weighted = TRUE)
# 
# 
# 
# ################################
# # Unweighted Analysis
# ################################
# item237.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item237.data
#                                                      , valueVariable       = 'count'
#                                                      , columnVariable      = 'Ceiling.Type'
#                                                      , rowVariable         = 'rvalue.bins'
#                                                      , aggregateColumnName = "All Types"
# )
# item237.summary <- item237.summary[which(item237.summary$Ceiling.Type != "All Types"),]
# 
# ## Summary only for "All Frame Types"
# item237.all.frame.types <- proportions_one_group(CustomerLevelData = item237.data
#                                                  ,valueVariable    = "count"
#                                                  ,groupingVariable = "rvalue.bins"
#                                                  ,total.name       = "All Types"
#                                                  ,columnName       = "Ceiling.Type"
#                                                  ,weighted = FALSE
#                                                  ,two.prop.total = TRUE
# )
# 
# #merge together!
# item237.final <- rbind.data.frame(item237.summary
#                                   , item237.all.frame.types, stringsAsFactors = F)
# 
# 
# ##cast data
# item237.cast <- dcast(setDT(item237.final),
#                       formula   = BuildingType + Ceiling.Type ~ rvalue.bins,
#                       value.var = c("Percent", "SE", "Count", "n"))
# 
# #join all insulation levels onto rvalue summary
# item237.table <- data.frame("BuildingType"                   = item237.cast$BuildingType
#                             ,"Ceiling.Type"                  = item237.cast$Ceiling.Type
#                             ,"Percent.R0.R10"                = item237.cast$Percent_R0.R10
#                             ,"SE.R0.R10"                     = item237.cast$SE_R0.R10
#                             ,"Percent.R11.R15"               = item237.cast$Percent_R11.R15
#                             ,"SE.R11.R15"                    = item237.cast$SE_R11.R15
#                             ,"Percent.R16.R20"               = item237.cast$Percent_R16.R20
#                             ,"SE.R16.R20"                    = item237.cast$SE_R16.R20
#                             ,"Percent.R21.R25"               = item237.cast$Percent_R21.R25
#                             ,"SE.R21.R25"                    = item237.cast$SE_R21.R25
#                             ,"Percent.R26.R30"               = item237.cast$Percent_R26.R30
#                             ,"SE.R26.R30"                    = item237.cast$SE_R26.R30
#                             ,"Percent.R31.R40"               = item237.cast$Percent_R31.R40
#                             ,"SE.R31.R40"                    = item237.cast$SE_R31.R40
#                             ,"Percent.R41.R50"               = item237.cast$Percent_R41.R50
#                             ,"SE.R41.R50"                    = item237.cast$SE_R41.R50
#                             ,"Percent.RGT50"                 = item237.cast$Percent_RGT50
#                             ,"SE.RGT50"                      = item237.cast$SE_RGT50
#                             ,"n"                             = item237.cast$n_Total
# )
# 
# # row ordering example code
# unique(item237.table$Ceiling.Type)
# rowOrder <- c("Attic"
#               ,"Roof Deck"
#               ,"Sloped / Vaulted (no attic)"
#               ,"Other"
#               ,"All Types")
# item237.table <- item237.table %>% mutate(Ceiling.Type = factor(Ceiling.Type, levels = rowOrder)) %>% arrange(Ceiling.Type)  
# item237.table <- data.frame(item237.table)
# 
# 
# item237.table.MF <- item237.table[which(item237.table$BuildingType == "Multifamily"),-1]
# 
# 
# #export table to correct workbook using exporting function
# exportTable(item237.table.MF, "MF", "Table 29", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ############################################################################################################
# #
# #
# # OVERSAMPLE ANALYSIS
# #
# #
# ############################################################################################################
# # Read in clean os data
# os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
# length(unique(os.dat$CK_Cadmus_ID))
# os.dat$CK_Building_ID <- os.dat$Category
# os.dat <- os.dat[which(names(os.dat) != "Category")]
# 
# 
# ############################################################################################################
# # ITEM 26: DISTRIBUTION OF CEILING INSULATION BY HOME VINTAGE
# ############################################################################################################
# item26.os.dat1 <- prep.dat7
# 
# #Bin R values -- SF only
# item26.os.dat1$rvalue.bins.SF <- "Unknown"
# item26.os.dat1$rvalue.bins.SF[which(item26.os.dat1$aveRval == 0)] <- "R0"
# item26.os.dat1$rvalue.bins.SF[which(item26.os.dat1$aveRval >  0  & item26.os.dat1$aveRval < 11)]  <- "R1.R10"
# item26.os.dat1$rvalue.bins.SF[which(item26.os.dat1$aveRval >= 11 & item26.os.dat1$aveRval < 16)]  <- "R11.R15"
# item26.os.dat1$rvalue.bins.SF[which(item26.os.dat1$aveRval >= 16 & item26.os.dat1$aveRval < 21)]  <- "R16.R20"
# item26.os.dat1$rvalue.bins.SF[which(item26.os.dat1$aveRval >= 21 & item26.os.dat1$aveRval < 26)]  <- "R21.R25"
# item26.os.dat1$rvalue.bins.SF[which(item26.os.dat1$aveRval >= 26 & item26.os.dat1$aveRval < 31)]  <- "R26.R30"
# item26.os.dat1$rvalue.bins.SF[which(item26.os.dat1$aveRval >= 31 & item26.os.dat1$aveRval < 41)]  <- "R31.R40"
# item26.os.dat1$rvalue.bins.SF[which(item26.os.dat1$aveRval >= 41 & item26.os.dat1$aveRval < 51)]  <- "R41.R50"
# item26.os.dat1$rvalue.bins.SF[which(item26.os.dat1$aveRval >= 51)] <- "RGT50"
# unique(item26.os.dat1$rvalue.bins.SF)
# 
# item26.os.dat1$count <- 1
# 
# ######################
# # Apply weights
# ######################
# item26.os.dat1$count <- 1
# colnames(item26.os.dat1)
# item26.os.dat1 <- item26.os.dat1[which(names(item26.os.dat1) != "CK_Building_ID")]
# 
# item26.os.merge <- left_join(os.dat, item26.os.dat1)
# item26.os.merge <- item26.os.merge[which(!is.na(item26.os.merge$count)),]
# 
# item26.os.data <- weightedData(unique(item26.os.merge[which(colnames(item26.os.merge) %notin% c("aveUval"
#                                                                                                 ,"aveRval"
#                                                                                                 ,"rvalue.bins.SF"
#                                                                                                 ,"count"
#                                                                                                 ,"Ceiling.Type"))]))
# item26.os.data <- left_join(item26.os.data, unique(item26.os.merge[which(colnames(item26.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                                           ,"aveUval"
#                                                                                                           ,"aveRval"
#                                                                                                           ,"rvalue.bins.SF"
#                                                                                                           ,"count"
#                                                                                                           ,"Ceiling.Type"))]))
# 
# ######################
# # Weighted - Single Family
# ######################
# item26.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item26.os.data
#                                                , valueVariable       = 'count'
#                                                , columnVariable      = 'CK_Building_ID'
#                                                , rowVariable         = 'rvalue.bins.SF'
#                                                , aggregateColumnName = "Remove"
# )
# item26.os.summary <- item26.os.summary[which(item26.os.summary$CK_Building_ID != "Remove"),]
# item26.os.summary <- item26.os.summary[which(item26.os.summary$rvalue.bins.SF != "Total"),]
# 
# ## Summary for only "All Insulation Levels"
# item26.os.all.insul.levels <-  proportions_one_group(item26.os.data
#                                                      ,valueVariable    = "count"
#                                                      ,groupingVariable = "CK_Building_ID"
#                                                      ,total.name       = "Total"
#                                                      ,columnName       = "rvalue.bins.SF"
#                                                      ,weighted = TRUE
#                                                      ,two.prop.total = TRUE
# )
# item26.os.all.insul.levels <- item26.os.all.insul.levels[which(item26.os.all.insul.levels$CK_Building_ID != "Total"),]
# 
# #merge together!
# item26.os.final <- rbind.data.frame(item26.os.summary
#                                     , item26.os.all.insul.levels
#                                     , stringsAsFactors = F)
# 
# item26.os.cast <- dcast(setDT(item26.os.final),
#                         formula   = rvalue.bins.SF ~ CK_Building_ID,
#                         value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
# names(item26.os.cast)
# 
# if(os.ind == "scl"){
#   item26.os.table <- data.frame("Insulation.Level"      = item26.os.cast$rvalue.bins.SF
#                                 ,"Percent_SCL.GenPop"   = item26.os.cast$`w.percent_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item26.os.cast$`w.SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item26.os.cast$`n_SCL GenPop`
#                                 ,"Percent_SCL.LI"       = item26.os.cast$`w.percent_SCL LI`
#                                 ,"SE_SCL.LI"            = item26.os.cast$`w.SE_SCL LI`
#                                 ,"n_SCL.LI"             = item26.os.cast$`n_SCL LI`
#                                 ,"Percent_SCL.EH"       = item26.os.cast$`w.percent_SCL EH`
#                                 ,"SE_SCL.EH"            = item26.os.cast$`w.SE_SCL EH`
#                                 ,"n_SCL.EH"             = item26.os.cast$`n_SCL EH`
#                                 ,"Percent_2017.RBSA.PS" = item26.os.cast$`w.percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item26.os.cast$`w.SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item26.os.cast$`n_2017 RBSA PS`
#                                 ,"EB_SCL.GenPop"        = item26.os.cast$`EB_SCL GenPop`
#                                 ,"EB_SCL.LI"            = item26.os.cast$`EB_SCL LI`
#                                 ,"EB_SCL.EH"            = item26.os.cast$`EB_SCL EH`
#                                 ,"EB_2017.RBSA.PS"      = item26.os.cast$`EB_2017 RBSA PS`
#   )
#   
# }else if(os.ind == "snopud"){
#   item26.os.table <- data.frame("Insulation.Level"      = item26.os.cast$rvalue.bins.SF
#                                 ,"Percent_SnoPUD"          = item26.os.cast$`w.percent_SnoPUD`
#                                 ,"SE_SnoPUD"               = item26.os.cast$`w.SE_SnoPUD`
#                                 ,"n_SnoPUD"                = item26.os.cast$`n_SnoPUD`
#                                 ,"Percent_2017.RBSA.PS"    = item26.os.cast$`w.percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"         = item26.os.cast$`w.SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"          = item26.os.cast$`n_2017 RBSA PS`
#                                 ,"Percent_RBSA.NW"         = item26.os.cast$`w.percent_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"              = item26.os.cast$`w.SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"               = item26.os.cast$`n_2017 RBSA NW`
#                                 ,"EB_SnoPUD"               = item26.os.cast$`EB_SnoPUD`
#                                 ,"EB_2017.RBSA.PS"         = item26.os.cast$`EB_2017 RBSA PS`
#                                 ,"EB_RBSA.NW"              = item26.os.cast$`EB_2017 RBSA NW`
#   )
#   
# }
# 
# # row ordering example code
# levels(item26.os.table$Insulation.Level)
# rowOrder <- c("R0"
#               ,"R1.R10"
#               ,"R11.R15"
#               ,"R16.R20"
#               ,"R21.R25"
#               ,"R26.R30"
#               ,"R31.R40"
#               ,"R41.R50"
#               ,"RGT50"
#               ,"Total")
# item26.os.table <- item26.os.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)  
# item26.os.table <- data.frame(item26.os.table)
# 
# #export table to correct workbook using exporting function
# exportTable(item26.os.table, "SF", "Table 33", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# ######################
# # Unweighted - Single Family
# ######################
# item26.os.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item26.os.data
#                                                        , valueVariable       = 'count'
#                                                        , columnVariable      = 'CK_Building_ID'
#                                                        , rowVariable         = 'rvalue.bins.SF'
#                                                        , aggregateColumnName = "Remove"
# )
# item26.os.summary <- item26.os.summary[which(item26.os.summary$CK_Building_ID != "Remove"),]
# item26.os.summary <- item26.os.summary[which(item26.os.summary$rvalue.bins.SF != "Total"),]
# 
# ## Summary for only "All Insulation Levels"
# item26.os.all.insul.levels <-  proportions_one_group(item26.os.data
#                                                      ,valueVariable    = "count"
#                                                      ,groupingVariable = "CK_Building_ID"
#                                                      ,total.name       = "Total"
#                                                      ,columnName       = "rvalue.bins.SF"
#                                                      ,weighted = FALSE
#                                                      ,two.prop.total = TRUE
# )
# item26.os.all.insul.levels <- item26.os.all.insul.levels[which(item26.os.all.insul.levels$CK_Building_ID != "Total"),]
# 
# #merge together!
# item26.os.final <- rbind.data.frame(item26.os.summary
#                                     , item26.os.all.insul.levels
#                                     , stringsAsFactors = F)
# 
# item26.os.cast <- dcast(setDT(item26.os.final),
#                         formula   = rvalue.bins.SF ~ CK_Building_ID,
#                         value.var = c("Percent", "SE","n"))
# names(item26.os.cast)
# 
# if(os.ind == "scl"){
#   item26.os.table <- data.frame("Insulation.Level"      = item26.os.cast$rvalue.bins.SF
#                                 ,"Percent_SCL.GenPop"   = item26.os.cast$`Percent_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item26.os.cast$`SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item26.os.cast$`n_SCL GenPop`
#                                 ,"Percent_SCL.LI"       = item26.os.cast$`Percent_SCL LI`
#                                 ,"SE_SCL.LI"            = item26.os.cast$`SE_SCL LI`
#                                 ,"n_SCL.LI"             = item26.os.cast$`n_SCL LI`
#                                 ,"Percent_SCL.EH"       = item26.os.cast$`Percent_SCL EH`
#                                 ,"SE_SCL.EH"            = item26.os.cast$`SE_SCL EH`
#                                 ,"n_SCL.EH"             = item26.os.cast$`n_SCL EH`
#                                 ,"Percent_2017.RBSA.PS" = item26.os.cast$`Percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item26.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item26.os.cast$`n_2017 RBSA PS`
#   )
# }else if(os.ind == "snopud"){
#   item26.os.table <- data.frame("Insulation.Level"      = item26.os.cast$rvalue.bins.SF
#                                 ,"Percent_SnoPUD"          = item26.os.cast$`Percent_SnoPUD`
#                                 ,"SE_SnoPUD"               = item26.os.cast$`SE_SnoPUD`
#                                 ,"n_SnoPUD"                = item26.os.cast$`n_SnoPUD`
#                                 ,"Percent_2017.RBSA.PS"    = item26.os.cast$`Percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"         = item26.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"          = item26.os.cast$`n_2017 RBSA PS`
#                                 ,"Percent_RBSA.NW"         = item26.os.cast$`Percent_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"              = item26.os.cast$`SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"               = item26.os.cast$`n_2017 RBSA NW`
#   )
# }
# 
# 
# # row ordering example code
# levels(item26.os.table$Insulation.Level)
# rowOrder <- c("R0"
#               ,"R1.R10"
#               ,"R11.R15"
#               ,"R16.R20"
#               ,"R21.R25"
#               ,"R26.R30"
#               ,"R31.R40"
#               ,"R41.R50"
#               ,"RGT50"
#               ,"Total")
# item26.os.table <- item26.os.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)  
# item26.os.table <- data.frame(item26.os.table)
# 
# #export table to correct workbook using exporting function
# exportTable(item26.os.table, "SF", "Table 33", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# ############################################################################################################
# ## Item 30
# ############################################################################################################
# item30.os.dat1 <- prep.dat7[which(prep.dat7$Ceiling.Type == "Sloped / Vaulted (no attic)"),]
# 
# 
# #Bin R values -- SF only
# item30.os.dat1$rvalue.bins <- "Unknown"
# item30.os.dat1$rvalue.bins[which(item30.os.dat1$aveRval == 0)] <- "R0"
# item30.os.dat1$rvalue.bins[which(item30.os.dat1$aveRval >  0 & item30.os.dat1$aveRval < 16)]   <- "R1.R15"
# item30.os.dat1$rvalue.bins[which(item30.os.dat1$aveRval >= 16 & item30.os.dat1$aveRval < 21)]  <- "R16.R20"
# item30.os.dat1$rvalue.bins[which(item30.os.dat1$aveRval >= 21 & item30.os.dat1$aveRval < 26)]  <- "R21.R25"
# item30.os.dat1$rvalue.bins[which(item30.os.dat1$aveRval >= 26 & item30.os.dat1$aveRval < 31)]  <- "R26.R30"
# item30.os.dat1$rvalue.bins[which(item30.os.dat1$aveRval >= 31 & item30.os.dat1$aveRval < 41)]  <- "R31.R40"
# item30.os.dat1$rvalue.bins[which(item30.os.dat1$aveRval >= 41)]  <- "R41.R50"
# unique(item30.os.dat1$rvalue.bins)
# 
# item30.os.dat1$count <- 1
# colnames(item30.os.dat1)
# item30.os.dat1 <- item30.os.dat1[which(names(item30.os.dat1) != "CK_Building_ID")]
# 
# item30.os.merge <- left_join(os.dat, item30.os.dat1)
# item30.os.merge <- item30.os.merge[which(!is.na(item30.os.merge$count)),]
# unique(item30.os.merge$rvalue.bins)
# 
# 
# item30.os.data <- weightedData(item30.os.merge[-which(colnames(item30.os.merge) %in% c("Ceiling.Type"
#                                                                                        ,"aveUval"
#                                                                                        ,"aveRval"
#                                                                                        ,"rvalue.bins"
#                                                                                        ,"count"))])
# item30.os.data <- left_join(item30.os.data, unique(item30.os.merge[which(colnames(item30.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                                           ,"Ceiling.Type"
#                                                                                                           ,"aveUval"
#                                                                                                           ,"aveRval"
#                                                                                                           ,"rvalue.bins"
#                                                                                                           ,"count"))]))
# ##############################
# # Weighted Analysis
# ##############################
# item30.os.summary <- proportionRowsAndColumns1(CustomerLevelData     = item30.os.data
#                                                , valueVariable       = 'count'
#                                                , columnVariable      = 'CK_Building_ID'
#                                                , rowVariable         = 'rvalue.bins'
#                                                , aggregateColumnName = "Remove"
# )
# item30.os.summary <- item30.os.summary[which(item30.os.summary$CK_Building_ID != "Remove"),]
# item30.os.summary <- item30.os.summary[which(item30.os.summary$rvalue.bins != "Total"),]
# 
# ## Summary for only "All Insulation Levels"
# item30.os.all.insul.levels <-  proportions_one_group(item30.os.data
#                                                      ,valueVariable    = "count"
#                                                      ,groupingVariable = "CK_Building_ID"
#                                                      ,total.name       = "Total"
#                                                      ,columnName       = "rvalue.bins"
#                                                      ,weighted = TRUE
#                                                      ,two.prop.total = TRUE
# )
# item30.os.all.insul.levels <- item30.os.all.insul.levels[which(item30.os.all.insul.levels$CK_Building_ID != "Total"),]
# 
# #merge together!
# item30.os.final <- rbind.data.frame(item30.os.summary
#                                     , item30.os.all.insul.levels
#                                     , stringsAsFactors = F)
# 
# item30.os.cast <- dcast(setDT(item30.os.final),
#                         formula   = rvalue.bins ~ CK_Building_ID,
#                         value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
# names(item30.os.cast)
# 
# if(os.ind == "scl"){
#   item30.os.table <- data.frame("Insulation.Level"      = item30.os.cast$rvalue.bins
#                                 ,"Percent_SCL.GenPop"   = item30.os.cast$`w.percent_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item30.os.cast$`w.SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item30.os.cast$`n_SCL GenPop`
#                                 ,"Percent_SCL.LI"       = item30.os.cast$`w.percent_SCL LI`
#                                 ,"SE_SCL.LI"            = item30.os.cast$`w.SE_SCL LI`
#                                 ,"n_SCL.LI"             = item30.os.cast$`n_SCL LI`
#                                 ,"Percent_SCL.EH"       = item30.os.cast$`w.percent_SCL EH`
#                                 ,"SE_SCL.EH"            = item30.os.cast$`w.SE_SCL EH`
#                                 ,"n_SCL.EH"             = item30.os.cast$`n_SCL EH`
#                                 ,"Percent_2017.RBSA.PS" = item30.os.cast$`w.percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item30.os.cast$`w.SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item30.os.cast$`n_2017 RBSA PS`
#                                 ,"EB_SCL.GenPop"        = item30.os.cast$`EB_SCL GenPop`
#                                 ,"EB_SCL.LI"            = item30.os.cast$`EB_SCL LI`
#                                 ,"EB_SCL.EH"            = item30.os.cast$`EB_SCL EH`
#                                 ,"EB_2017.RBSA.PS"      = item30.os.cast$`EB_2017 RBSA PS`
#   )
#   
# }else if(os.ind == "snopud"){
#   item30.os.table <- data.frame("Insulation.Level"      = item30.os.cast$rvalue.bins
#                                 ,"Percent_SnoPUD"          = item30.os.cast$`w.percent_SnoPUD`
#                                 ,"SE_SnoPUD"               = item30.os.cast$`w.SE_SnoPUD`
#                                 ,"n_SnoPUD"                = item30.os.cast$`n_SnoPUD`
#                                 ,"Percent_2017.RBSA.PS"    = item30.os.cast$`w.percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"         = item30.os.cast$`w.SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"          = item30.os.cast$`n_2017 RBSA PS`
#                                 ,"Percent_RBSA.NW"         = item30.os.cast$`w.percent_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"              = item30.os.cast$`w.SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"               = item30.os.cast$`n_2017 RBSA NW`
#                                 ,"EB_SnoPUD"               = item30.os.cast$`EB_SnoPUD`
#                                 ,"EB_2017.RBSA.PS"         = item30.os.cast$`EB_2017 RBSA PS`
#                                 ,"EB_RBSA.NW"              = item30.os.cast$`EB_2017 RBSA NW`
#   )
#   
# }
# 
# # row ordering example code
# levels(item30.os.table$Insulation.Level)
# rowOrder <- c("R0"
#               ,"R1.R15"
#               ,"R16.R20"
#               ,"R21.R25"
#               ,"R26.R30"
#               ,"R31.R40"
#               ,"R41.R50"
#               ,"RGT50"
#               ,"Total")
# item30.os.table <- item30.os.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)  
# item30.os.table <- data.frame(item30.os.table)
# 
# exportTable(item30.os.table, "SF", "Table 37", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# ##############################
# # Unweighted Analysis
# ##############################
# item30.os.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item30.os.data
#                                                        , valueVariable       = 'count'
#                                                        , columnVariable      = 'CK_Building_ID'
#                                                        , rowVariable         = 'rvalue.bins'
#                                                        , aggregateColumnName = "Remove"
# )
# item30.os.summary <- item30.os.summary[which(item30.os.summary$CK_Building_ID != "Remove"),]
# item30.os.summary <- item30.os.summary[which(item30.os.summary$rvalue.bins != "Total"),]
# 
# ## Summary for only "All Insulation Levels"
# item30.os.all.insul.levels <-  proportions_one_group(item30.os.data
#                                                      ,valueVariable    = "count"
#                                                      ,groupingVariable = "CK_Building_ID"
#                                                      ,total.name       = "Total"
#                                                      ,columnName       = "rvalue.bins"
#                                                      ,weighted = FALSE
#                                                      ,two.prop.total = TRUE
# )
# item30.os.all.insul.levels <- item30.os.all.insul.levels[which(item30.os.all.insul.levels$CK_Building_ID != "Total"),]
# 
# #merge together!
# item30.os.final <- rbind.data.frame(item30.os.summary
#                                     , item30.os.all.insul.levels
#                                     , stringsAsFactors = F)
# 
# item30.os.cast <- dcast(setDT(item30.os.final),
#                         formula   = rvalue.bins ~ CK_Building_ID,
#                         value.var = c("Percent", "SE","n"))
# names(item30.os.cast)
# 
# if(os.ind == "scl"){
#   item30.os.table <- data.frame("Insulation.Level"      = item30.os.cast$rvalue.bins
#                                 ,"Percent_SCL.GenPop"   = item30.os.cast$`Percent_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item30.os.cast$`SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item30.os.cast$`n_SCL GenPop`
#                                 ,"Percent_SCL.LI"       = item30.os.cast$`Percent_SCL LI`
#                                 ,"SE_SCL.LI"            = item30.os.cast$`SE_SCL LI`
#                                 ,"n_SCL.LI"             = item30.os.cast$`n_SCL LI`
#                                 ,"Percent_SCL.EH"       = item30.os.cast$`Percent_SCL EH`
#                                 ,"SE_SCL.EH"            = item30.os.cast$`SE_SCL EH`
#                                 ,"n_SCL.EH"             = item30.os.cast$`n_SCL EH`
#                                 ,"Percent_2017.RBSA.PS" = item30.os.cast$`Percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item30.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item30.os.cast$`n_2017 RBSA PS`
#   )
# }else if(os.ind == "snopud"){
#   item30.os.table <- data.frame("Insulation.Level"      = item30.os.cast$rvalue.bins
#                                 ,"Percent_SnoPUD"          = item30.os.cast$`Percent_SnoPUD`
#                                 ,"SE_SnoPUD"               = item30.os.cast$`SE_SnoPUD`
#                                 ,"n_SnoPUD"                = item30.os.cast$`n_SnoPUD`
#                                 ,"Percent_2017.RBSA.PS"    = item30.os.cast$`Percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"         = item30.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"          = item30.os.cast$`n_2017 RBSA PS`
#                                 ,"Percent_RBSA.NW"         = item30.os.cast$`Percent_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"              = item30.os.cast$`SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"               = item30.os.cast$`n_2017 RBSA NW`
#   )
# }
# 
# 
# # row ordering example code
# levels(item30.os.table$Insulation.Level)
# rowOrder <- c("R0"
#               ,"R1.R15"
#               ,"R16.R20"
#               ,"R21.R25"
#               ,"R26.R30"
#               ,"R31.R40"
#               ,"R41.R50"
#               ,"RGT50"
#               ,"Total")
# item30.os.table <- item30.os.table %>% mutate(Insulation.Level = factor(Insulation.Level, levels = rowOrder)) %>% arrange(Insulation.Level)  
# item30.os.table <- data.frame(item30.os.table)
# 
# exportTable(item30.os.table, "SF", "Table 37", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# ############################################################################################################
# ## Item 31
# ############################################################################################################
# item31.os.dat <- prep.dat5[which(prep.dat5$Ceiling.Type == "Roof Deck"),]
# item31.os.dat$count <- 1
# 
# item31.os.dat0 <- item31.os.dat[which(item31.os.dat$Ceiling.Insulation.Thickness.1 != "N/A N/A"),]
# # item31.os.dat0 <- item31.os.dat0[which(names(item31.os.dat0) != "CK_Building_ID")]
# 
# item31.os.dat1 <- left_join(os.dat, item31.os.dat0)
# item31.os.dat2 <- item31.os.dat1[which(!is.na(item31.os.dat1$uvalue)),]
# 
# item31.os.data <- weightedData(item31.os.dat2[-c(grep("Ceiling", colnames(item31.os.dat2),ignore.case = T)
#                                                  ,which(colnames(item31.os.dat2) %in% c("Category"
#                                                                                         ,"count"
#                                                                                         ,"uvalue"
#                                                                                         ,"total.r.val"
#                                                                                         ,"TMP_ID"
#                                                                                         ,"PK_Envelope_ID"
#                                                                                         ,"CK_SiteID"
#                                                                                         ,"CK_Cadmus_ID.y")))])
# item31.os.data <- left_join(item31.os.data,unique(item31.os.dat2[c(grep("Ceiling|ceiling", colnames(item31.os.dat2))
#                                                                    ,which(colnames(item31.os.dat2) %in% c("CK_Cadmus_ID"
#                                                                                                           ,"Category"
#                                                                                                           ,"count"
#                                                                                                           ,"uvalue"
#                                                                                                           ,"total.r.val")))]))
# 
# ##############################
# # Weighted Analysis
# ##############################
# item31.os.final <- proportionRowsAndColumns1(CustomerLevelData = item31.os.data
#                                              ,valueVariable = 'count'
#                                              ,columnVariable = 'CK_Building_ID'
#                                              ,rowVariable = "Ceiling.Insulation.Thickness.1"
#                                              ,aggregateColumnName = "Remove")
# item31.os.final <- item31.os.final[which(item31.os.final$CK_Building_ID != "Remove"),]
# 
# item31.os.cast <- dcast(setDT(item31.os.final)
#                         ,formula = Ceiling.Insulation.Thickness.1 ~ CK_Building_ID
#                         ,value.var = c("w.percent","w.SE","n","EB"))
# names(item31.os.cast)
# 
# if(os.ind == "scl"){
#   item31.os.table <- data.frame("Insulation.Level"      = item31.os.cast$Ceiling.Insulation.Thickness.1
#                                 ,"Percent_SCL.GenPop"   = item31.os.cast$`w.percent_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item31.os.cast$`w.SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item31.os.cast$`n_SCL GenPop`
#                                 ,"Percent_SCL.LI"       = NA#item31.os.cast$`w.percent_SCL LI`
#                                 ,"SE_SCL.LI"            = NA#item31.os.cast$`w.SE_SCL LI`
#                                 ,"n_SCL.LI"             = NA#item31.os.cast$`n_SCL LI`
#                                 ,"Percent_SCL.EH"       = NA#item31.os.cast$`w.percent_SCL EH`
#                                 ,"SE_SCL.EH"            = NA#item31.os.cast$`w.SE_SCL EH`
#                                 ,"n_SCL.EH"             = NA#item31.os.cast$`n_SCL EH`
#                                 ,"Percent_2017.RBSA.PS" = item31.os.cast$`w.percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item31.os.cast$`w.SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item31.os.cast$`n_2017 RBSA PS`
#                                 ,"EB_SCL.GenPop"        = item31.os.cast$`EB_SCL GenPop`
#                                 ,"EB_SCL.LI"            = NA#item31.os.cast$`EB_SCL LI`
#                                 ,"EB_SCL.EH"            = NA#item31.os.cast$`EB_SCL EH`
#                                 ,"EB_2017.RBSA.PS"      = item31.os.cast$`EB_2017 RBSA PS`)
#   
# }else if(os.ind == "snopud"){
#   item31.os.table <- data.frame("Insulation.Level"         = item31.os.cast$Ceiling.Insulation.Thickness.1
#                                 ,"Percent_SnoPUD"          = item31.os.cast$`w.percent_SnoPUD`
#                                 ,"SE_SnoPUD"               = item31.os.cast$`w.SE_SnoPUD`
#                                 ,"n_SnoPUD"                = item31.os.cast$`n_SnoPUD`
#                                 ,"Percent_2017.RBSA.PS"    = item31.os.cast$`w.percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"         = item31.os.cast$`w.SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"          = item31.os.cast$`n_2017 RBSA PS`
#                                 ,"Percent_RBSA.NW"         = item31.os.cast$`w.percent_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"              = item31.os.cast$`w.SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"               = item31.os.cast$`n_2017 RBSA NW`
#                                 ,"EB_SnoPUD"               = item31.os.cast$`EB_SnoPUD`
#                                 ,"EB_2017.RBSA.PS"         = item31.os.cast$`EB_2017 RBSA PS`
#                                 ,"EB_RBSA.NW"              = item31.os.cast$`EB_2017 RBSA NW`)
#   
# }
# item31.os.table
# 
# exportTable(item31.os.table, "SF", "Table 38", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# ##############################
# # Unweighted Analysis
# ##############################
# item31.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item31.os.data
#                                                      ,valueVariable = 'count'
#                                                      ,columnVariable = 'CK_Building_ID'
#                                                      ,rowVariable = "Ceiling.Insulation.Thickness.1"
#                                                      ,aggregateColumnName = "Remove")
# item31.os.final <- item31.os.final[which(item31.os.final$CK_Building_ID != "Remove"),]
# 
# item31.os.cast <- dcast(setDT(item31.os.final)
#                         ,formula = Ceiling.Insulation.Thickness.1 ~ CK_Building_ID
#                         ,value.var = c("Percent","SE","n"))
# names(item31.os.cast)
# 
# if(os.ind == "scl"){
#   item31.os.table <- data.frame("Insulation.Level"      = item31.os.cast$Ceiling.Insulation.Thickness.1
#                                 ,"Percent_SCL.GenPop"   = item31.os.cast$`Percent_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item31.os.cast$`SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item31.os.cast$`n_SCL GenPop`
#                                 ,"Percent_SCL.LI"       = NA#item31.os.cast$`Percent_SCL LI`
#                                 ,"SE_SCL.LI"            = NA#item31.os.cast$`SE_SCL LI`
#                                 ,"n_SCL.LI"             = NA#item31.os.cast$`n_SCL LI`
#                                 ,"Percent_SCL.EH"       = NA#item31.os.cast$`Percent_SCL EH`
#                                 ,"SE_SCL.EH"            = NA#item31.os.cast$`SE_SCL EH`
#                                 ,"n_SCL.EH"             = NA#item31.os.cast$`n_SCL EH`
#                                 ,"Percent_2017.RBSA.PS" = item31.os.cast$`Percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item31.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item31.os.cast$`n_2017 RBSA PS`)
#   
# }else if(os.ind == "snopud"){
#   item31.os.table <- data.frame("Insulation.Level"         = item31.os.cast$Ceiling.Insulation.Thickness.1
#                                 ,"Percent_SnoPUD"          = item31.os.cast$`Percent_SnoPUD`
#                                 ,"SE_SnoPUD"               = item31.os.cast$`SE_SnoPUD`
#                                 ,"n_SnoPUD"                = item31.os.cast$`n_SnoPUD`
#                                 ,"Percent_2017.RBSA.PS"    = item31.os.cast$`Percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"         = item31.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"          = item31.os.cast$`n_2017 RBSA PS`
#                                 ,"Percent_RBSA.NW"         = item31.os.cast$`Percent_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"              = item31.os.cast$`SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"               = item31.os.cast$`n_2017 RBSA NW`)
#   
# }
# 
# item31.os.table
# 
# exportTable(item31.os.table, "SF", "Table 38", weighted = FALSE, osIndicator = export.ind, OS = T)
