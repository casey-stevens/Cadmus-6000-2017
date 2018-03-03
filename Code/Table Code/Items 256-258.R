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
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))

#Read in data for analysis
buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Building_ID <- trimws(toupper(buildings.interview.dat$CK_BuildingID))


#############################################################################################
#Item 256: AVERAGE NUMBER OF COMMON AREA LAMPS PER UNIT BY BUILDING SIZE (MF Table 48)
#############################################################################################
item256.buildings.int <- unique(buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Building_ID"
                                                                                                ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding"))])

which(duplicated(item256.buildings.int$CK_Cadmus_ID))

#subset to columns needed for analysis
item256.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"))]
item256.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item256.dat1 <- left_join(rbsa.dat, item256.dat, by = c("CK_Building_ID" = "CK_SiteID"))
names(item256.dat1)[which(names(item256.dat1) %in% c("CK_Cadmus_ID.x"))] <- "CK_Cadmus_ID"

#subset to building info
item256.dat2 <- item256.dat1[grep("BLDG", item256.dat1$CK_Building_ID),]

#clean fixture and bulbs per fixture
item256.dat2$Fixture.Qty <- as.numeric(as.character(item256.dat2$Fixture.Qty))
item256.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item256.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item256.dat2$Lamps <- item256.dat2$Fixture.Qty * item256.dat2$LIGHTING_BulbsPerFixture
unique(item256.dat2$Lamps)

#remove missing lamp quantities
item256.dat3 <- item256.dat2[which(!(is.na(item256.dat2$Lamps))),]

#Subset to Multifamily
item256.dat4 <- item256.dat3[grep("Multifamily", item256.dat3$BuildingType),]

#summarise up to the site level
item256.dat5 <- summarise(group_by(item256.dat4, CK_Cadmus_ID, CK_Building_ID)
                          ,SiteCount = sum(Lamps))


item256.dat6 <- left_join(item256.dat5, item256.buildings.int)
item256.dat6$INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding <- as.numeric(as.character(item256.dat6$INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding))
item256.dat6 <- item256.dat6[which(!is.na(item256.dat6$INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding)),]

item256.dat6$LampsPerUnit <- item256.dat6$SiteCount / item256.dat6$INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding

#subset to remove any missing number of units, or unit size equal to 1 (doesn't make sense)
item256.dat7 <- item256.dat6[which(item256.dat6$INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding > 1),]

item256.merge <- left_join(rbsa.dat, item256.dat7)
item256.merge <- item256.merge[which(!is.na(item256.merge$LampsPerUnit)),]


######################################
#Pop and Sample Sizes for weights
######################################
item256.data <- weightedData(item256.merge[which(colnames(item256.merge) %notin% c("SiteCount"
                                                                                   ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding"
                                                                                   ,"LampsPerUnit"))])

item256.data <- left_join(item256.data, item256.merge[which(colnames(item256.merge) %in% c("CK_Cadmus_ID"
                                                                                              ,"SiteCount"
                                                                                              ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding"
                                                                                              ,"LampsPerUnit"))])
item256.data$count <- 1


######################
# weighted analysis
######################
item256.final <- mean_one_group(CustomerLevelData = item256.data
                                ,valueVariable = 'LampsPerUnit'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

item256.final.MF <- item256.final[which(colnames(item256.final) %notin% c("BuildingType","n"))]

exportTable(item256.final.MF, "MF", "Table 48", weighted = TRUE)

######################
# unweighted analysis
######################
item256.final <- mean_one_group_unweighted(CustomerLevelData = item256.data
                                ,valueVariable = 'LampsPerUnit'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

item256.final.MF <- item256.final[which(colnames(item256.final) %notin% c("BuildingType","n"))]

exportTable(item256.final.MF, "MF", "Table 48", weighted = FALSE)







#############################################################################################
#Item 257: DISTRIBUTION OF COMMON AREA LAMPS BY LAMP TYPE AND BUILDING SIZE (MF Table 49)
#############################################################################################
one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = "Building One Line Summary", startRow = 3)
one.line.bldg.dat <- one.line.bldg.dat[which(one.line.bldg.dat$Area.of.Conditioned.Common.Space > 0),]
one.line.bldg.dat$CK_Building_ID <- one.line.bldg.dat$PK_BuildingID

one.line.bldg.dat <- one.line.bldg.dat[names(one.line.bldg.dat) %in% c("CK_Building_ID", "Area.of.Conditioned.Common.Space")]

rbsa.merge <- left_join(rbsa.dat.bldg, one.line.bldg.dat)
rbsa.merge <- rbsa.merge[which(!is.na(rbsa.merge$Area.of.Conditioned.Common.Space)),]

#subset to columns needed for analysis
item257.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Lamp.Category"))]
item257.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item257.dat1 <- left_join(rbsa.dat, item257.dat, by = c("CK_Building_ID" = "CK_SiteID"))
names(item257.dat1)[which(names(item257.dat1) %in% c("CK_Cadmus_ID.x"))] <- "CK_Cadmus_ID"

#remove building info
item257.dat2 <- item257.dat1[grep("BLDG", item257.dat1$CK_Building_ID),]

#clean fixture and bulbs per fixture
item257.dat2$Fixture.Qty <- as.numeric(as.character(item257.dat2$Fixture.Qty))
item257.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item257.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item257.dat2$Lamps <- item257.dat2$Fixture.Qty * item257.dat2$LIGHTING_BulbsPerFixture
unique(item257.dat2$Lamps)

#remove missing lamp quantities
item257.dat3 <- item257.dat2[which(!(is.na(item257.dat2$Lamps))),]

#Subset to Multifamily
item257.dat4 <- item257.dat3[grep("Multifamily", item257.dat3$BuildingType),]

#summarise up to the site level
item257.dat5 <- summarise(group_by(item257.dat4, CK_Cadmus_ID, CK_Building_ID, Lamp.Category)
                          ,SiteCount = sum(Lamps))
unique(item257.dat5$Lamp.Category)

item257.dat6 <- item257.dat5[which(item257.dat5$Lamp.Category != "Unknown"),]

item257.merge <- left_join(rbsa.merge, item257.dat6)
item257.merge <- item257.merge[which(!is.na(item257.merge$SiteCount)),]
length(unique(item257.merge$CK_Cadmus_ID))
######################################
#Pop and Sample Sizes for weights
######################################
item257.data <- weightedData(item257.merge[which(colnames(item257.merge) %notin% c("Lamp.Category"
                                                                                   ,"SiteCount"
                                                                                   ,"Area.of.Conditioned.Common.Space"))])

item257.data <- left_join(item257.data, item257.merge[which(colnames(item257.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Lamp.Category"
                                                                                           ,"SiteCount"
                                                                                           ,"Area.of.Conditioned.Common.Space"))])
item257.data$count <- 1
length(unique(item257.data$CK_Cadmus_ID))
##############################
# weighted analysis
##############################
item257.final <- proportionRowsAndColumns1(CustomerLevelData = item257.data
                                           ,valueVariable = 'SiteCount'
                                           ,columnVariable = 'HomeType'
                                           ,rowVariable = 'Lamp.Category'
                                           ,aggregateColumnName = "Remove")
item257.final <- item257.final[which(item257.final$HomeType != "Remove"),]
# item257.final <- item257.final[which(item257.final$Lamp.Category != "Total"),]

item257.all.sizes <- proportions_one_group(CustomerLevelData = item257.data
                                              ,valueVariable = 'SiteCount'
                                              ,groupingVariable = 'Lamp.Category'
                                              ,total.name = 'All Sizes'
                                              ,columnName = "HomeType"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
# item257.all.sizes <- item257.all.sizes[which(item257.all.sizes$Lamp.Category != "Total"),]


item257.final <- rbind.data.frame(item257.final, item257.all.sizes, stringsAsFactors = F)

item257.cast <- dcast(setDT(item257.final)
                      ,formula = HomeType ~ Lamp.Category
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))


item257.table <- data.frame("Building.Size"            = item257.cast$HomeType
                            ,"Compact.Fluorescent"     = item257.cast$`w.percent_Compact Fluorescent`
                            ,"Compact.Fluorescent.SE"  = item257.cast$`w.SE_Compact Fluorescent`
                            ,"Halogen"                 = item257.cast$w.percent_Halogen
                            ,"Halogen.SE"              = item257.cast$w.SE_Halogen
                            ,"Incandescent"            = item257.cast$w.percent_Incandescent
                            ,"Incandescent.SE"         = item257.cast$w.SE_Incandescent
                            ,"Incandescent.Halogen"    = item257.cast$`w.percent_Incandescent / Halogen`
                            ,"Incandescent.Halogen.SE" = item257.cast$`w.SE_Incandescent / Halogen`
                            ,"Linear.Fluorescent"      = item257.cast$`w.percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE"   = item257.cast$`w.SE_Linear Fluorescent`
                            ,"LED"                     = item257.cast$`w.percent_Light Emitting Diode`
                            ,"LED.SE"                  = item257.cast$`w.SE_Light Emitting Diode`
                            ,"Other"                   = item257.cast$w.percent_Other
                            ,"Other.SE"                = item257.cast$w.SE_Other
                            ,"n"                       = item257.cast$n_Total
                            ,"Compact.Fluorescent.EB"  = item257.cast$`EB_Compact Fluorescent`
                            ,"Halogen.EB"              = item257.cast$EB_Halogen
                            ,"Incandescent.EB"         = item257.cast$EB_Incandescent
                            ,"Incandescent.Halogen.EB" = item257.cast$`EB_Incandescent / Halogen`
                            ,"Linear.Fluorescent.EB"   = item257.cast$`EB_Linear Fluorescent`
                            ,"LED.EB"                  = item257.cast$`EB_Light Emitting Diode`
                            ,"Other.EB"                = item257.cast$EB_Other
                            )

levels(item257.table$Building.Size)
rowOrder <- c("Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"All Sizes")
item257.table <- item257.table %>% mutate(Building.Size = factor(Building.Size, levels = rowOrder)) %>% arrange(Building.Size)  
item257.table <- data.frame(item257.table)

exportTable(item257.table, "MF", "Table 49", weighted = TRUE)


##############################
# unweighted analysis
##############################
item257.final <- proportions_two_groups_unweighted(CustomerLevelData = item257.data
                                           ,valueVariable = 'SiteCount'
                                           ,columnVariable = 'HomeType'
                                           ,rowVariable = 'Lamp.Category'
                                           ,aggregateColumnName = "Remove")
item257.final <- item257.final[which(item257.final$HomeType != "Remove"),]
# item257.final <- item257.final[which(item257.final$Lamp.Category != "Total"),]

item257.all.sizes <- proportions_one_group(CustomerLevelData = item257.data
                                              ,valueVariable = 'SiteCount'
                                              ,groupingVariable = 'Lamp.Category'
                                              ,total.name = 'All Sizes'
                                              ,columnName = "HomeType"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
# item257.all.sizes <- item257.all.sizes[which(item257.all.sizes$Lamp.Category != "Total"),]


item257.final <- rbind.data.frame(item257.final, item257.all.sizes, stringsAsFactors = F)

item257.cast <- dcast(setDT(item257.final)
                      ,formula = HomeType ~ Lamp.Category
                      ,value.var = c("Percent", "SE", "Count", "n"))


item257.table <- data.frame("Building.Size"            = item257.cast$HomeType
                            ,"Compact.Fluorescent"     = item257.cast$`Percent_Compact Fluorescent`
                            ,"Compact.Fluorescent.SE"  = item257.cast$`SE_Compact Fluorescent`
                            ,"Halogen"                 = item257.cast$Percent_Halogen
                            ,"Halogen.SE"              = item257.cast$SE_Halogen
                            ,"Incandescent"            = item257.cast$Percent_Incandescent
                            ,"Incandescent.SE"         = item257.cast$SE_Incandescent
                            ,"Incandescent.Halogen"    = item257.cast$`Percent_Incandescent / Halogen`
                            ,"Incandescent.Halogen.SE" = item257.cast$`SE_Incandescent / Halogen`
                            ,"Linear.Fluorescent"      = item257.cast$`Percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE"   = item257.cast$`SE_Linear Fluorescent`
                            ,"LED"                     = item257.cast$`Percent_Light Emitting Diode`
                            ,"LED.SE"                  = item257.cast$`SE_Light Emitting Diode`
                            ,"Other"                   = item257.cast$Percent_Other
                            ,"Other.SE"                = item257.cast$SE_Other
                            ,"n"                       = item257.cast$n_Total)
levels(item257.table$Building.Size)
rowOrder <- c("Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"All Sizes")
item257.table <- item257.table %>% mutate(Building.Size = factor(Building.Size, levels = rowOrder)) %>% arrange(Building.Size)  
item257.table <- data.frame(item257.table)

exportTable(item257.table, "MF", "Table 49", weighted = FALSE)






#############################################################################################
#Item 258: DISTRIBUTION OF COMMON AREA LAMPS BY LAMP TYPE AND BUILDING SIZE (MF Table 50)
#############################################################################################
#subset to columns needed for analysis
item258.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]
item258.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item258.dat1 <- left_join(rbsa.dat, item258.dat, by = c("CK_Building_ID" = "CK_SiteID"))
names(item258.dat1)[which(names(item258.dat1) %in% c("CK_Cadmus_ID.x"))] <- "CK_Cadmus_ID"

#remove building info
item258.dat2 <- item258.dat1[grep("BLDG", item258.dat1$CK_Building_ID),]

#clean fixture and bulbs per fixture
item258.dat2$Fixture.Qty <- as.numeric(as.character(item258.dat2$Fixture.Qty))
item258.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item258.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item258.dat2$Lamps <- item258.dat2$Fixture.Qty * item258.dat2$LIGHTING_BulbsPerFixture
unique(item258.dat2$Lamps)

#remove missing lamp quantities
item258.dat3 <- item258.dat2[which(!(is.na(item258.dat2$Lamps))),]

#Subset to Multifamily
item258.dat4 <- item258.dat3[grep("Multifamily", item258.dat3$BuildingType),]

#summarise up to the site level
item258.dat5 <- summarise(group_by(item258.dat4, CK_Cadmus_ID, Clean.Room, Lamp.Category)
                          ,SiteCount = sum(Lamps))
unique(item258.dat5$Lamp.Category)

item258.dat6 <- item258.dat5[which(item258.dat5$Lamp.Category != "Unknown"),]


item258.merge <- left_join(rbsa.merge, item258.dat6)
item258.merge <- item258.merge[which(!is.na(item258.merge$SiteCount)),]

######################################
#Pop and Sample Sizes for weights
######################################
item258.data <- weightedData(item258.merge[which(colnames(item258.merge) %notin% c("Lamp.Category"
                                                                                   ,"SiteCount"
                                                                                   ,"Clean.Room"
                                                                                   ,"Area.of.Conditioned.Common.Space"))])

item258.data <- left_join(item258.data, item258.merge[which(colnames(item258.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Lamp.Category"
                                                                                           ,"SiteCount"
                                                                                           ,"Clean.Room"
                                                                                           ,"Area.of.Conditioned.Common.Space"))])
item258.data$count <- 1

##############################
# weighted analysis
##############################
item258.final <- proportionRowsAndColumns1(CustomerLevelData = item258.data
                                           ,valueVariable = 'SiteCount'
                                           ,columnVariable = 'Clean.Room'
                                           ,rowVariable = 'Lamp.Category'
                                           ,aggregateColumnName = "Remove")
item258.final <- item258.final[which(item258.final$Clean.Room != "Remove"),]
# item258.final <- item258.final[which(item258.final$Lamp.Category != "Total"),]

item258.all.sizes <- proportions_one_group(CustomerLevelData = item258.data
                                              ,valueVariable = 'SiteCount'
                                              ,groupingVariable = 'Lamp.Category'
                                              ,total.name = 'All Sizes'
                                              ,columnName = "Clean.Room"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
# item258.all.sizes <- item258.all.sizes[which(item258.all.sizes$Lamp.Category != "Total"),]


item258.final <- rbind.data.frame(item258.final, item258.all.sizes, stringsAsFactors = F)

item258.cast <- dcast(setDT(item258.final)
                      ,formula = Clean.Room ~ Lamp.Category
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

names(item258.cast)
item258.table <- data.frame("Room.Type"                = item258.cast$Clean.Room
                            ,"Compact.Fluorescent"     = item258.cast$`w.percent_Compact Fluorescent`
                            ,"Compact.Fluorescent.SE"  = item258.cast$`w.SE_Compact Fluorescent`
                            ,"Halogen"                 = item258.cast$w.percent_Halogen
                            ,"Halogen.SE"              = item258.cast$w.SE_Halogen
                            ,"Incandescent"            = item258.cast$w.percent_Incandescent
                            ,"Incandescent.SE"         = item258.cast$w.SE_Incandescent
                            ,"Incandescent.Halogen"    = item258.cast$`w.percent_Incandescent / Halogen`
                            ,"Incandescent.Halogen.SE" = item258.cast$`w.SE_Incandescent / Halogen`
                            ,"Linear.Fluorescent"      = item258.cast$`w.percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE"   = item258.cast$`w.SE_Linear Fluorescent`
                            ,"LED"                     = item258.cast$`w.percent_Light Emitting Diode`
                            ,"LED.SE"                  = item258.cast$`w.SE_Light Emitting Diode`
                            ,"Other"                   = item258.cast$w.percent_Other
                            ,"Other.SE"                = item258.cast$w.SE_Other
                            ,"n"                       = item258.cast$n_Total
                            ,"Compact.Fluorescent.EB"  = item258.cast$`EB_Compact Fluorescent`
                            ,"Halogen.EB"              = item258.cast$EB_Halogen
                            ,"Incandescent.EB"         = item258.cast$EB_Incandescent
                            ,"Incandescent.Halogen.EB" = item258.cast$`EB_Incandescent / Halogen`
                            ,"Linear.Fluorescent.EB"   = item258.cast$`EB_Linear Fluorescent`
                            ,"LED.EB"                  = item258.cast$`EB_Light Emitting Diode`
                            ,"Other.EB"                = item258.cast$EB_Other
                            )

exportTable(item258.table, "MF", "Table 50", weighted = TRUE)


##############################
# weighted analysis
##############################
item258.final <- proportions_two_groups_unweighted(CustomerLevelData = item258.data
                                                   ,valueVariable = 'SiteCount'
                                                   ,columnVariable = 'Clean.Room'
                                                   ,rowVariable = 'Lamp.Category'
                                                   ,aggregateColumnName = "Remove")
item258.final <- item258.final[which(item258.final$Clean.Room != "Remove"),]
# item258.final <- item258.final[which(item258.final$Lamp.Category != "Total"),]

item258.all.sizes <- proportions_one_group(CustomerLevelData = item258.data
                                              ,valueVariable = 'SiteCount'
                                              ,groupingVariable = 'Lamp.Category'
                                              ,total.name = 'All Sizes'
                                              ,columnName = "Clean.Room"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
# item258.all.sizes <- item258.all.sizes[which(item258.all.sizes$Lamp.Category != "Total"),]


item258.final <- rbind.data.frame(item258.final, item258.all.sizes, stringsAsFactors = F)

item258.cast <- dcast(setDT(item258.final)
                      ,formula = Clean.Room ~ Lamp.Category
                      ,value.var = c("Percent", "SE", "Count", "n"))


item258.table <- data.frame("Room.Type"                = item258.cast$Clean.Room
                            ,"Compact.Fluorescent"     = item258.cast$`Percent_Compact Fluorescent`
                            ,"Compact.Fluorescent.SE"  = item258.cast$`SE_Compact Fluorescent`
                            ,"Halogen"                 = item258.cast$Percent_Halogen
                            ,"Halogen.SE"              = item258.cast$SE_Halogen
                            ,"Incandescent"            = item258.cast$Percent_Incandescent
                            ,"Incandescent.SE"         = item258.cast$SE_Incandescent
                            ,"Incandescent.Halogen"    = item258.cast$`Percent_Incandescent / Halogen`
                            ,"Incandescent.Halogen.SE" = item258.cast$`SE_Incandescent / Halogen`
                            ,"Linear.Fluorescent"      = item258.cast$`Percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE"   = item258.cast$`SE_Linear Fluorescent`
                            ,"LED"                     = item258.cast$`Percent_Light Emitting Diode`
                            ,"LED.SE"                  = item258.cast$`SE_Light Emitting Diode`
                            ,"Other"                   = item258.cast$Percent_Other
                            ,"Other.SE"                = item258.cast$SE_Other
                            ,"n"                       = item258.cast$n_Total)

exportTable(item258.table, "MF", "Table 50", weighted = FALSE)

