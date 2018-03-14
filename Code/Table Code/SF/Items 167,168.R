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
# mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

#bring in one-line file for heat loss info
# one.line.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.export), sheet = "Site One Line Summary", startRow = 2)
one.line.dat$CK_Cadmus_ID <- trimws(toupper(one.line.dat$Cadmus.ID))



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item167.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item167.dat.11 <- item167.dat.1[which(item167.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item167.dat.12 <- item167.dat.11[which(item167.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item167.dat.12$CK_Cadmus_ID))

#check uniques
unique(item167.dat.12$Primary.Heating.System)
item167.dat.12$count <- 1

#subset to only electric primary heating systems
item167.dat.13 <- unique(item167.dat.12[which(item167.dat.12$Heating.Fuel == "Electric"),])

#summaryise up to the customer level - keeping heating fuel information
item167.sum <- summarise(group_by(item167.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item167.sum$Count <- 1

#check uniques
which(duplicated(item167.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item167.sum$Heating.Fuel)

#merge with cleaned rbsa data and subset to complete data
item167.merge <- left_join(rbsa.dat, item167.sum)
item167.merge <- item167.merge[which(!is.na(item167.merge$Heating.Fuel)),]

#re-assign mechanical data for later use
item167.mechanical <- item167.merge


# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(item167.data, paste(filepathCleaningDocs, "Insulation Exports", paste("UA Cadmus IDs ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)



#############################################################################################
# 
# Heat Loss Data
# 
#############################################################################################
item167.dat <- one.line.dat[which(colnames(one.line.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Whole.House.UA"))]
item167.dat1 <- left_join(item167.dat, item167.mechanical)
item167.dat2 <- left_join(rbsa.dat, item167.dat1)

item167.dat2$Whole.House.UA <- as.numeric(as.character(item167.dat2$Whole.House.UA))
item167.dat2$Conditioned.Area <- as.numeric(as.character(item167.dat2$Conditioned.Area))

item167.dat2 <- item167.dat2[which(!is.na(item167.dat2$Whole.House.UA)),]
item167.dat3 <- item167.dat2[grep("site",item167.dat2$CK_Building_ID, ignore.case = T),]

which(duplicated(item167.dat3$CK_Cadmus_ID))

item167.dat4 <- item167.dat3[which(item167.dat3$Conditioned.Area > 0),]

item167.dat4$Normalized.Heat.Loss.Rate <- item167.dat4$Whole.House.UA / item167.dat4$Conditioned.Area

item167.dat5 <- item167.dat4[which(!is.na(item167.dat4$HomeYearBuilt_bins3)),]


################################################
# Adding pop and sample sizes for weights
################################################
item167.data <- weightedData(item167.dat5[-which(colnames(item167.dat5) %in% c("Whole.House.UA"
                                                                            ,"Normalized.Heat.Loss.Rate"
                                                                            ,"Count"
                                                                            ,"Heating.Fuel"))])
item167.data <- left_join(item167.data, item167.dat5[which(colnames(item167.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Whole.House.UA"
                                                                                     ,"Normalized.Heat.Loss.Rate"
                                                                                     ,"Count"
                                                                                     ,"Heating.Fuel"))])
item167.data$count <- 1
#######################
# Weighted Analysis
#######################
item167.cast <- mean_two_groups(CustomerLevelData = item167.data
                               ,valueVariable = "Normalized.Heat.Loss.Rate"
                               ,byVariableRow = "HomeYearBuilt_bins3"
                               ,byVariableColumn = "State"
                               ,columnAggregate = "Region"
                               ,rowAggregate = "All Vintages")

item167.table <- data.frame("BuildingType"     = item167.cast$BuildingType
                           ,"Housing.Vintage" = item167.cast$HomeYearBuilt_bins3
                           ,"ID"              = item167.cast$Mean_ID
                           ,"ID.SE"           = item167.cast$SE_ID
                           ,"ID.n"            = item167.cast$n_ID
                           ,"MT"              = item167.cast$Mean_MT
                           ,"MT.SE"           = item167.cast$SE_MT
                           ,"MT.n"            = item167.cast$n_MT
                           ,"OR"              = item167.cast$Mean_OR
                           ,"OR.SE"           = item167.cast$SE_OR
                           ,"OR.n"            = item167.cast$n_OR
                           ,"WA"              = item167.cast$Mean_WA
                           ,"WA.SE"           = item167.cast$SE_WA
                           ,"WA.n"            = item167.cast$n_WA
                           ,"Region"          = item167.cast$Mean_Region
                           ,"Region.SE"       = item167.cast$SE_Region
                           ,"Region.n"        = item167.cast$n_Region
                           ,"ID.EB"           = item167.cast$EB_ID
                           ,"MT.EB"           = item167.cast$EB_MT
                           ,"OR.EB"           = item167.cast$EB_OR
                           ,"WA.EB"           = item167.cast$EB_WA
                           ,"Region.EB"       = item167.cast$EB_Region)

levels(item167.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item167.table <- item167.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item167.table <- data.frame(item167.table)


item167.table.SF <- item167.table[which(item167.table$BuildingType == "Single Family"),
                                -which(colnames(item167.table) == "BuildingType")]

exportTable(item167.table.SF, "SF","Table B-12",weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item167.cast <- mean_two_groups_unweighted(CustomerLevelData = item167.data
                                          ,valueVariable = "Normalized.Heat.Loss.Rate"
                                          ,byVariableRow = "HomeYearBuilt_bins3"
                                          ,byVariableColumn = "State"
                                          ,columnAggregate = "Region"
                                          ,rowAggregate = "All Vintages")

item167.table <- data.frame("BuildingType"     = item167.cast$BuildingType
                           ,"Housing.Vintage" = item167.cast$HomeYearBuilt_bins3
                           ,"ID"              = item167.cast$Mean_ID
                           ,"ID.SE"           = item167.cast$SE_ID
                           ,"ID.n"            = item167.cast$n_ID
                           ,"MT"              = item167.cast$Mean_MT
                           ,"MT.SE"           = item167.cast$SE_MT
                           ,"MT.n"            = item167.cast$n_MT
                           ,"OR"              = item167.cast$Mean_OR
                           ,"OR.SE"           = item167.cast$SE_OR
                           ,"OR.n"            = item167.cast$n_OR
                           ,"WA"              = item167.cast$Mean_WA
                           ,"WA.SE"           = item167.cast$SE_WA
                           ,"WA.n"            = item167.cast$n_WA
                           ,"Region"          = item167.cast$Mean_Region
                           ,"Region.SE"       = item167.cast$SE_Region
                           ,"Region.n"        = item167.cast$n_Region)

levels(item167.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item167.table <- item167.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item167.table <- data.frame(item167.table)


item167.table.SF <- item167.table[which(item167.table$BuildingType == "Single Family"),
                                -which(colnames(item167.table) == "BuildingType")]

exportTable(item167.table.SF, "SF","Table B-12",weighted = FALSE)






#############################################################################################
#Item 37: AVERAGE HEAT-LOSS RATE BY VINTAGE AND STATE (SF table 44, MH table 26)
#############################################################################################
item168.data <- item167.data

#######################
# Weighted Analysis
#######################
item168.cast <- mean_two_groups(CustomerLevelData = item168.data
                               ,valueVariable    = "Whole.House.UA"
                               ,byVariableRow    = "HomeYearBuilt_bins3"
                               ,byVariableColumn = "State"
                               ,columnAggregate  = "Region"
                               ,rowAggregate     = "All Vintages")

item168.table <- data.frame("BuildingType"     = item168.cast$BuildingType
                           ,"Housing.Vintage" = item168.cast$HomeYearBuilt_bins3
                           ,"ID"              = item168.cast$Mean_ID
                           ,"ID.SE"           = item168.cast$SE_ID
                           ,"ID.n"            = item168.cast$n_ID
                           ,"MT"              = item168.cast$Mean_MT
                           ,"MT.SE"           = item168.cast$SE_MT
                           ,"MT.n"            = item168.cast$n_MT
                           ,"OR"              = item168.cast$Mean_OR
                           ,"OR.SE"           = item168.cast$SE_OR
                           ,"OR.n"            = item168.cast$n_OR
                           ,"WA"              = item168.cast$Mean_WA
                           ,"WA.SE"           = item168.cast$SE_WA
                           ,"WA.n"            = item168.cast$n_WA
                           ,"Region"          = item168.cast$Mean_Region
                           ,"Region.SE"       = item168.cast$SE_Region
                           ,"Region.n"        = item168.cast$n_Region
                           ,"ID.EB"           = item168.cast$EB_ID
                           ,"MT.EB"           = item168.cast$EB_MT
                           ,"OR.EB"           = item168.cast$EB_OR
                           ,"WA.EB"           = item168.cast$EB_WA
                           ,"Region.EB"       = item168.cast$EB_Region)

levels(item168.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item168.table <- item168.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item168.table <- data.frame(item168.table)


item168.table.SF <- item168.table[which(item168.table$BuildingType == "Single Family"),
                                -which(colnames(item168.table) == "BuildingType")]

exportTable(item168.table.SF, "SF","Table B-13",weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item168.cast <- mean_two_groups_unweighted(CustomerLevelData = item168.data
                                          ,valueVariable    = "Whole.House.UA"
                                          ,byVariableRow    = "HomeYearBuilt_bins3"
                                          ,byVariableColumn = "State"
                                          ,columnAggregate  = "Region"
                                          ,rowAggregate     = "All Vintages")

item168.table <- data.frame("BuildingType"     = item168.cast$BuildingType
                           ,"Housing.Vintage" = item168.cast$HomeYearBuilt_bins3
                           ,"ID"              = item168.cast$Mean_ID
                           ,"ID.SE"           = item168.cast$SE_ID
                           ,"ID.n"            = item168.cast$n_ID
                           ,"MT"              = item168.cast$Mean_MT
                           ,"MT.SE"           = item168.cast$SE_MT
                           ,"MT.n"            = item168.cast$n_MT
                           ,"OR"              = item168.cast$Mean_OR
                           ,"OR.SE"           = item168.cast$SE_OR
                           ,"OR.n"            = item168.cast$n_OR
                           ,"WA"              = item168.cast$Mean_WA
                           ,"WA.SE"           = item168.cast$SE_WA
                           ,"WA.n"            = item168.cast$n_WA
                           ,"Region"          = item168.cast$Mean_Region
                           ,"Region.SE"       = item168.cast$SE_Region
                           ,"Region.n"        = item168.cast$n_Region)

levels(item168.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item168.table <- item168.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item168.table <- data.frame(item168.table)


item168.table.SF <- item168.table[which(item168.table$BuildingType == "Single Family"),
                                -which(colnames(item168.table) == "BuildingType")]

exportTable(item168.table.SF, "SF","Table B-13",weighted = FALSE)
