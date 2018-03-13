#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list = ls())
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
# envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Read in data for analysis
# mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
# Item 159: DISTRIBUTION OF ELECTRICALLY HEATED HOMES BY GROUND CONTACT TYPE AND STATE (SF table B-2)
#############################################################################################

#############################################################################################
#
#For Envelope information
#
#############################################################################################

#subset to columns need in table
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
names(env.dat) <- c("CK_Cadmus_ID", "Floor.Area")
env.dat1 <- env.dat[which(!(is.na(env.dat$Floor.Area))),]

env.dat2 <- env.dat1[which(!(env.dat1$Floor.Area %in% c("0", "Unknown"))),]
env.dat2$Floor.Area <- as.numeric(as.character(env.dat2$Floor.Area))

env.sum <- summarise(group_by(env.dat2, CK_Cadmus_ID)
                     ,Site.SQFT = sum(Floor.Area))

item159.envelope <- env.sum



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item159.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item159.dat.11 <- item159.dat.1[which(item159.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item159.dat.12 <- item159.dat.11[which(item159.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item159.dat.12$CK_Cadmus_ID))
#check uniques
unique(item159.dat.12$Primary.Heating.System)
item159.dat.12$count <- 1

item159.dat.13 <- unique(item159.dat.12[which(item159.dat.12$Heating.Fuel == "Electric"),])

item159.sum <- summarise(group_by(item159.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item159.sum$Count <- 1
which(duplicated(item159.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item159.sum$Heating.Fuel)

item159.merge <- left_join(rbsa.dat, item159.sum)
item159.merge <- item159.merge[which(!is.na(item159.merge$Heating.Fuel)),]

item159.mechanical <- item159.merge



#############################################################################################
#
#Merge mechanical and envelope data together
#
#############################################################################################

item159.merge <- left_join(item159.envelope, item159.mechanical)

item159.merge1 <- item159.merge[which(!(is.na(item159.merge$Heating.Fuel))),]

item159.merge2 <- left_join(rbsa.dat, item159.merge1)
item159.merge <- item159.merge2[which(!is.na(item159.merge2$Site.SQFT)),]
item159.merge <- item159.merge[which(!is.na(item159.merge$HomeYearBuilt_bins2)),]



################################################
# Adding pop and sample sizes for weights
################################################
item159.data <- weightedData(item159.merge[-which(colnames(item159.merge) %in% c("Heating.Fuel"
                                                                                 ,"Count"
                                                                                 ,"Site.SQFT"))])
item159.data <- left_join(item159.data, item159.merge[which(colnames(item159.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating.Fuel"
                                                                                           ,"Count"
                                                                                           ,"Site.SQFT"))])
#######################
# Weighted Analysis
#######################
item159.cast <- mean_two_groups(CustomerLevelData = item159.data
                                ,valueVariable    = 'Site.SQFT'
                                ,byVariableRow    = 'HomeYearBuilt_bins2'
                                ,byVariableColumn = 'State'
                                ,columnAggregate  = "Region"
                                ,rowAggregate     = "All Vintages")

item159.table <- data.frame("BuildingType"    = item159.cast$BuildingType
                            ,"Housing.Vintage"= item159.cast$HomeYearBuilt_bins2
                            ,"Mean_ID"        = item159.cast$Mean_ID
                            ,"SE_ID"          = item159.cast$SE_ID
                            ,"n_ID"           = item159.cast$n_ID
                            ,"Mean_MT"        = item159.cast$Mean_MT
                            ,"SE_MT"          = item159.cast$SE_MT
                            ,"n_MT"           = item159.cast$n_MT
                            ,"Mean_OR"        = item159.cast$Mean_OR
                            ,"SE_OR"          = item159.cast$SE_OR
                            ,"n_OR"           = item159.cast$n_OR
                            ,"Mean_WA"        = item159.cast$Mean_WA
                            ,"SE_WA"          = item159.cast$SE_WA
                            ,"n_WA"           = item159.cast$n_WA
                            ,"Mean_Region"    = item159.cast$Mean_Region
                            ,"SE_Region"      = item159.cast$SE_Region
                            ,"n_Region"       = item159.cast$n_Region
                            ,"EB_ID"          = item159.cast$EB_ID
                            ,"EB_MT"          = item159.cast$EB_MT
                            ,"EB_OR"          = item159.cast$EB_OR
                            ,"EB_WA"          = item159.cast$EB_WA
                            ,"EB_Region"      = item159.cast$EB_Region
)

# If final table have <NA> something was named incorrectly
levels(item159.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item159.table <- item159.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item159.table <- data.frame(item159.table)

item159.final.SF <- item159.table[which(item159.table$BuildingType == "Single Family")
                                  ,-which(colnames(item159.table) %in% c("BuildingType"))]

exportTable(item159.final.SF, "SF", "Table B-4", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item159.cast <-  mean_two_groups_unweighted(CustomerLevelData = item159.data
                                             ,valueVariable    = 'Site.SQFT'
                                             ,byVariableRow    = 'HomeYearBuilt_bins2'
                                             ,byVariableColumn = 'State'
                                             ,columnAggregate  = "Region"
                                             ,rowAggregate     = "All Vintages")

item159.table <- data.frame("BuildingType"    = item159.cast$BuildingType
                            ,"Housing.Vintage"= item159.cast$HomeYearBuilt_bins2
                            ,"Mean_ID"        = item159.cast$Mean_ID
                            ,"SE_ID"          = item159.cast$SE_ID
                            ,"n_ID"           = item159.cast$n_ID
                            ,"Mean_MT"        = item159.cast$Mean_MT
                            ,"SE_MT"          = item159.cast$SE_MT
                            ,"n_MT"           = item159.cast$n_MT
                            ,"Mean_OR"        = item159.cast$Mean_OR
                            ,"SE_OR"          = item159.cast$SE_OR
                            ,"n_OR"           = item159.cast$n_OR
                            ,"Mean_WA"        = item159.cast$Mean_WA
                            ,"SE_WA"          = item159.cast$SE_WA
                            ,"n_WA"           = item159.cast$n_WA
                            ,"Mean_Region"    = item159.cast$Mean_Region
                            ,"SE_Region"      = item159.cast$SE_Region
                            ,"n_Region"       = item159.cast$n_Region
)

# If final table have <NA> something was named incorrectly
levels(item159.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item159.table <- item159.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item159.table <- data.frame(item159.table)

item159.final.SF <- item159.table[which(item159.table$BuildingType == "Single Family")
                                  ,-which(colnames(item159.table) %in% c("BuildingType"))]

exportTable(item159.final.SF, "SF", "Table B-4", weighted = FALSE)
