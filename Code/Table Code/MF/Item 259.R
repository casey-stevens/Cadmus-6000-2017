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
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
# lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))




#############################################################################################
#item 259: DISTRIBUTION OF COMMON AREA LAMPS BY EISA CATEGORY (MF table 51)
#############################################################################################
one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = "Building One Line Summary", startRow = 3)
one.line.bldg.dat <- one.line.bldg.dat[which(one.line.bldg.dat$Area.of.Conditioned.Common.Space > 0),]
one.line.bldg.dat$CK_Building_ID <- one.line.bldg.dat$PK_BuildingID

one.line.bldg.dat <- one.line.bldg.dat[names(one.line.bldg.dat) %in% c("CK_Building_ID", "Area.of.Conditioned.Common.Space")]

rbsa.merge <- left_join(rbsa.dat.bldg, one.line.bldg.dat)
rbsa.merge <- rbsa.merge[which(!is.na(rbsa.merge$Area.of.Conditioned.Common.Space)),]



#subset to columns needed for analysis
item259.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Lamp.Category"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Status"
                                                                ,""
                                                                ,""))]
item259.dat$count <- 1

item259.dat1 <- left_join(rbsa.dat, item259.dat, by = c("CK_Building_ID" = "CK_SiteID"))
names(item259.dat1)[which(names(item259.dat1) %in% c("CK_Cadmus_ID.x"))] <- "CK_Cadmus_ID"

item259.dat2 <- item259.dat1[grep("BLDG", item259.dat1$CK_Building_ID),]

#clean fixture and bulbs per fixture
item259.dat2$Fixture.Qty <- as.numeric(as.character(item259.dat2$Fixture.Qty))
item259.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item259.dat2$LIGHTING_BulbsPerFixture))

item259.dat2$Lamps <- item259.dat2$Fixture.Qty * item259.dat2$LIGHTING_BulbsPerFixture
unique(item259.dat2$Lamps)

item259.dat3 <- item259.dat2[which(item259.dat2$Lamps %notin% c("N/A",NA)),]
unique(item259.dat3$Status)
item259.dat4 <- item259.dat3[which(item259.dat3$Status %notin% c("Empty Socket", "Unknown")),]

item259.merge <- left_join(rbsa.merge, item259.dat4)
item259.merge <- item259.merge[grep("3 or fewer floors", item259.merge$BuildingTypeXX, ignore.case = T),]
item259.merge <- item259.merge[which(!is.na(item259.merge$Lamps)),]
length(unique(item259.merge$CK_Cadmus_ID))
################################################
# Adding pop and sample sizes for weights
################################################
item259.data <- weightedData(item259.merge[-which(colnames(item259.merge) %in% c("CK_Cadmus_ID.y"
                                                                                 ,"Fixture.Qty"
                                                                                 ,"LIGHTING_BulbsPerFixture"
                                                                                 ,"Lamp.Category"
                                                                                 ,"Lamps"
                                                                                 ,"count"
                                                                                 ,"Area.of.Conditioned.Common.Space"
                                                                                 ,"Status"))])
item259.data <- left_join(item259.data, item259.merge[which(colnames(item259.merge) %in% c("CK_Cadmus_ID"               
                                                                                           ,"Fixture.Qty"
                                                                                           ,"LIGHTING_BulbsPerFixture"
                                                                                           ,"Lamp.Category"
                                                                                           ,"Lamps"
                                                                                           ,"count"
                                                                                           ,"Area.of.Conditioned.Common.Space"
                                                                                           ,"Status"))])
item259.data$count <- 1
#######################
# Weighted Analysis
#######################
item259.table.MF <- proportions_one_group(CustomerLevelData = item259.data
                                          ,valueVariable    = 'Lamps'
                                          ,groupingVariable = 'Status'
                                          ,total.name       = "Remove"
                                          ,weighted         = TRUE)
item259.table.MF <- item259.table.MF[which(item259.table.MF$Status != "Total"),]

item259.final.MF <- item259.table.MF[which(item259.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(item259.table.MF) %in% c("BuildingType"))]

exportTable(item259.final.MF, "MF", "Table 51", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item259.table.MF <- proportions_one_group(CustomerLevelData = item259.data
                                          ,valueVariable    = 'Lamps'
                                          ,groupingVariable = 'Status'
                                          ,total.name       = "Remove"
                                          ,weighted         = FALSE)
item259.table.MF <- item259.table.MF[which(item259.table.MF$Status != "Total"),]

item259.final.MF <- item259.table.MF[which(item259.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(item259.table.MF) %in% c("BuildingType"))]
exportTable(item259.final.MF, "MF", "Table 51", weighted = FALSE)



