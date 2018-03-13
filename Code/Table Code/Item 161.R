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
# envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Read in data for analysis
# mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
# Item 161: DISTRIBUTION OF FRAME WALL INSULATION LEVELS, ELECTRICALLY HEATED HOMES (SF table B-6)
#############################################################################################
#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item161.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item161.dat.11 <- item161.dat.1[which(item161.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item161.dat.12 <- item161.dat.11[which(item161.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item161.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item161.dat.12$Primary.Heating.System)
item161.dat.12$count <- 1

item161.dat.13 <- unique(item161.dat.12[which(item161.dat.12$Heating.Fuel == "Electric"),])

item161.sum <- summarise(group_by(item161.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item161.sum$Count <- 1
which(duplicated(item161.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item161.sum$Heating.Fuel)

item161.merge <- left_join(rbsa.dat, item161.sum)
item161.merge <- item161.merge[which(!is.na(item161.merge$Heating.Fuel)),]

item161.mechanical <- item161.merge






#############################################################################################
#
#For Envelope information
#
#############################################################################################
item161.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Floor.Type"
                                                               ,"Floor.Sub-Type"))]

item161.dat$Ind <- 0
item161.dat$Ind[which(item161.dat$Floor.Type == "Basement")] <- 1

item161.dat1 <- unique(item161.dat[which(item161.dat$Ind == 1),])
which(duplicated(item161.dat3$CK_Cadmus_ID))

item161.merge <- left_join(rbsa.dat, item161.dat1)
item161.merge$Ind[which(is.na(item161.merge$Ind))] <- 0



#############################################################################################
#
#Merge mechanical and envelope data together
#
#############################################################################################

item161.merge1 <- left_join(item161.merge, item161.mechanical)
item161.merge2 <- unique(item161.merge1[which(item161.merge1$Heating.Fuel == "Electric"),])

item161.merge3 <- left_join(rbsa.dat, item161.merge2)
item161.merge <- item161.merge3[which(!is.na(item161.merge3$Heating.Fuel)),]



################################################
# Adding pop and sample sizes for weights
################################################
item161.data <- weightedData(item161.merge[-which(colnames(item161.merge) %in% c("Heating.Fuel"
                                                                                 ,"Count"
                                                                                 ,"Site.SQFT"
                                                                                 ,"Floor.Type"
                                                                                 ,"Floor.Sub-Type"
                                                                                 ,"Ind"))])
item161.data <- left_join(item161.data, item161.merge[which(colnames(item161.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating.Fuel"
                                                                                           ,"Count"
                                                                                           ,"Site.SQFT"
                                                                                           ,"Floor.Type"
                                                                                           ,"Floor.Sub-Type"
                                                                                           ,"Ind"))])
#######################
# Weighted Analysis
#######################
item161.final <- proportions_one_group(CustomerLevelData = item161.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE
                                       ,two.prop.total = NA)

item161.final.SF <- item161.final[which(item161.final$BuildingType == "Single Family")
                                  ,-which(colnames(item161.final) %in% c("BuildingType"))]

exportTable(item161.final.SF, "SF", "Table B-6", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item161.final <- proportions_one_group(CustomerLevelData = item161.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE
                                       ,two.prop.total = NA)

item161.final.SF <- item161.final[which(item161.final$BuildingType == "Single Family")
                                  ,-which(colnames(item161.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item161.final.SF, "SF", "Table B-6", weighted = FALSE)

