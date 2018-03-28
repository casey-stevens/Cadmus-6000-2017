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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site",rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg",rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

one.line.bldg.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), startRow = 2)

one.line.bldg.dat1 <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("PK_BuildingID"
                                                                                 ,"Primary.Heating.System"
                                                                                 ,"Primary.Heating.Fuel"
                                                                                 ,"Primary.Cooling.System"
                                                                                 ,"Central.Building.Heat"))]

one.line.bldg.dat2  <- left_join(rbsa.dat.bldg, one.line.bldg.dat1, by = c("CK_Building_ID" = "PK_BuildingID"))
one.line.bldg.dat2 <- one.line.bldg.dat2[which(!is.na(one.line.bldg.dat2$Primary.Heating.System)),]
length(unique(one.line.bldg.dat2$CK_Cadmus_ID))

# Mechanical
# mechanical.dat <- read.xlsx(mechanical.export)
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"CK_SiteID"
                                                                        ,"System.Type"
                                                                        ,"Heating.Fuel",
                                                                        "Heat.Iteration",
                                                                        "Cool.Iteration",
                                                                        "Primary.Heating.System",
                                                                        "Primary.Cooling.System"
                                                                        ,"Serves.Common.Areas?"
                                                                        ,"Central.for.Building"))]

mechanical.dat2  <- left_join(rbsa.dat, mechanical.dat1, by = c("CK_Building_ID" = "CK_SiteID"))
length(unique(mechanical.dat2$CK_Cadmus_ID))
names(mechanical.dat2)[which(names(mechanical.dat2) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"

#Subset to MF
mechanical.dat.MF <- mechanical.dat2[grep("Multifamily", mechanical.dat2$BuildingType),]



#############################################################################################
#Item 248: DISTRIBUTION OF UNIT COOLING SYSTEMS / Table 40
#############################################################################################
item248.dat <- unique(mechanical.dat.MF[which(colnames(mechanical.dat.MF) %in% c("CK_Cadmus_ID",
                                                                                 "Cool.Iteration",
                                                                                 "CK_Building_ID",
                                                                                 "System.Type"
                                                                                 ,"Primary.Cooling.System"
))])
item248.dat <- item248.dat[grep("site", item248.dat$Cool.Iteration, ignore.case = T),]

item248.dat$CoolingInd <- 1

unique(item248.dat$System.Type)
item248.dat$System.Type[grep("central", item248.dat$System.Type, ignore.case = T)] <- "Central Ac"

item248.dat1 <- data.frame(summarise(group_by(item248.dat,CK_Cadmus_ID,System.Type),
                                     CoolingInd = sum(unique(CoolingInd), na.rm = T)), stringsAsFactors = F)

item248.dat1 <- left_join(rbsa.dat.site, item248.dat1)

item248.dat1$CoolingInd[which(is.na(item248.dat1$CoolingInd))] <- 0
item248.dat1$System.Type[which(item248.dat1$CoolingInd == 0)] <- "No Cooling"
item248.dat1 <- item248.dat1[which(item248.dat1$Category == "PSE"),]
################################################
# Adding pop and sample sizes for weights
################################################
item248.data <- weightedData(item248.dat1[-which(colnames(item248.dat1) %in% c("System.Type"
                                                                               ,"CoolingInd"
                                                                               ,"Category"))])
item248.data <- left_join(item248.data, item248.dat1[which(colnames(item248.dat1) %in% c("CK_Cadmus_ID"
                                                                                         ,"System.Type"
                                                                                         ,"CoolingInd"
                                                                                         ,"Category"))])

item248.data$count <- 1

#######################
# Weighted Analysis
#######################
item248.final <- proportions_one_group(CustomerLevelData = item248.data
                                       ,valueVariable = 'count'
                                       ,groupingVariable = 'System.Type'
                                       ,total.name = "All Systems"
)
item248.final.MF <- item248.final[which(colnames(item248.final) %notin% c("BuildingType"))]

exportTable(item248.final.MF, "MF", "Table 40", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
item248.final <- proportions_one_group(CustomerLevelData = item248.data
                                       ,valueVariable = 'count'
                                       ,groupingVariable = 'System.Type'
                                       ,total.name = "All Systems"
                                       ,weighted = FALSE
)
item248.final.MF <- item248.final[which(colnames(item248.final) %notin% c("BuildingType"))]

exportTable(item248.final.MF, "MF", "Table 40", weighted = FALSE,OS = T, osIndicator = "PSE")
