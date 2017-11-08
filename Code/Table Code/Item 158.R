#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

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



#############################################################################################
# Item 158: AVERAGE CONDITIONED FLOOR AREA BY STATE, ELECTRICALLY HEATED HOMES (SF table B-4)
#############################################################################################

#############################################################################################
#
#For Envelope information
#
#############################################################################################

#subset to columns need in table
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                            , "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
names(env.dat) <- c("CK_Cadmus_ID", "Floor.Area")
env.dat1 <- env.dat[which(!(is.na(env.dat$Floor.Area))),]

env.dat2 <- env.dat1[which(!(env.dat1$Floor.Area %in% c("0", "Unknown"))),]
env.dat2$Floor.Area <- as.numeric(as.character(env.dat2$Floor.Area))

env.sum <- summarise(group_by(env.dat2, CK_Cadmus_ID)
                     ,Site.SQFT = sum(Floor.Area))

item158.envelope <- env.sum



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item158.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item158.dat.11 <- item158.dat.1[which(item158.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item158.dat.12 <- item158.dat.11[which(item158.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item158.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item158.dat.12$Primary.Heating.System)
item158.dat.12$count <- 1

item158.dat.13 <- unique(item158.dat.12[which(item158.dat.12$Heating.Fuel == "Electric"),])

item158.sum <- summarise(group_by(item158.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item158.sum$Count <- 1
which(duplicated(item158.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item158.sum$Heating.Fuel)

item158.merge <- left_join(rbsa.dat, item158.sum)
item158.merge <- item158.merge[which(!is.na(item158.merge$Heating.Fuel)),]

item158.mechanical <- item158.merge



#############################################################################################
#
#Merge mechanical and envelope data together
#
#############################################################################################

item158.merge <- left_join(item158.envelope, item158.mechanical)

item158.merge1 <- item158.merge[which(!(is.na(item158.merge$Heating.Fuel))),]

item158.merge2 <- left_join(rbsa.dat, item158.merge1)
item158.merge <- item158.merge2[which(!is.na(item158.merge2$Heating.Fuel)),]


################################################
# Adding pop and sample sizes for weights
################################################
item158.data <- weightedData(item158.merge[-which(colnames(item158.merge) %in% c("Heating.Fuel"
                                                                                 ,"Count"
                                                                                 ,"Site.SQFT"))])
item158.data <- left_join(item158.data, item158.merge[which(colnames(item158.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating.Fuel"
                                                                                           ,"Count"
                                                                                           ,"Site.SQFT"))])
#######################
# Weighted Analysis
#######################
item158.final <- mean_one_group(item158.data
                                ,valueVariable = 'Site.SQFT'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item158.final.SF <- item158.final[which(item158.final$BuildingType == "Single Family")
                                  ,-which(colnames(item158.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item158.final.SF, "SF", "Table B-3", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item158.final <- mean_one_group_unweighted(item158.data
                                           ,valueVariable = 'Site.SQFT'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item158.final.SF <- item158.final[which(item158.final$BuildingType == "Single Family")
                                  ,-which(colnames(item158.final) %in% c("BuildingType"))]

exportTable(item158.final.SF, "SF", "Table B-3", weighted = FALSE)

