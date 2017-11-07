#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################
##  Clear variables
rm(list=ls())
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
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export))
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))


#############################################################################################
#Item 68: DISTRIBUTION OF LAMPS BY EISA CATEGORY AND STATE (SF table 75, MH table 54)
#############################################################################################
#subset to columns needed for analysis
item68.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,""
                                                               ,""
                                                               ,""
                                                               ,""
                                                               ,""))]
item68.dat$count <- 1

item68.dat1 <- left_join(item68.dat, rbsa.dat, by = "CK_Cadmus_ID")

item68.dat2 <- item68.dat1[-grep("BLDG", item68.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item68.dat2$Fixture.Qty <- as.numeric(as.character(item68.dat2$Fixture.Qty))
item68.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item68.dat2$LIGHTING_BulbsPerFixture))

item68.dat2$Lamps <- item68.dat2$Fixture.Qty * item68.dat2$LIGHTING_BulbsPerFixture
unique(item68.dat2$Lamps)

item68.dat3 <- item68.dat2[which(!(is.na(item68.dat2$Lamps))),]






















#############################################################################################
#Item 69: DISTRIBUTION OF LAMPS BY TYPE AND STATE (SF table 76, MH table 55)
#############################################################################################
#subset to columns needed for analysis
item69.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"))]
item69.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item69.dat1 <- left_join(rbsa.dat, item69.dat, by = "CK_Cadmus_ID")

#remove building info
item69.dat2 <- item69.dat1[grep("SITE", item69.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item69.dat2$Fixture.Qty <- as.numeric(as.character(item69.dat2$Fixture.Qty))
item69.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item69.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item69.dat2$Lamps <- item69.dat2$Fixture.Qty * item69.dat2$LIGHTING_BulbsPerFixture
unique(item69.dat2$Lamps)

#remove missing lamp quantities
item69.dat3 <- item69.dat2[which(!(is.na(item69.dat2$Lamp.Category))),]
item69.dat3 <- item69.dat3[which(item69.dat3$Lamp.Category != "Unknown"),]

#check lamp types
unique(item69.dat3$Lamp.Category)


item69.merge <- left_join(rbsa.dat, item69.dat3)
item69.merge <- item69.merge[which(!is.na(item69.merge$Lamp.Category))]


################################################
# Adding pop and sample sizes for weights
################################################
item69.data <- weightedData(item69.merge[-which(colnames(item69.merge) %in% c("CK_SiteID"               
                                                                              ,"Fixture.Qty"
                                                                              ,"LIGHTING_BulbsPerFixture"
                                                                              ,"Lamp.Category"
                                                                              ,"count"
                                                                              ,"Lamps"))])
item69.data <- left_join(item69.data, item69.merge[which(colnames(item69.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"CK_SiteID"               
                                                                                       ,"Fixture.Qty"
                                                                                       ,"LIGHTING_BulbsPerFixture"
                                                                                       ,"Lamp.Category"
                                                                                       ,"count"
                                                                                       ,"Lamps"))])
item69.data$count <- 1
#######################
# Weighted Analysis
#######################
item69.final <- proportionRowsAndColumns1(CustomerLevelData = item69.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'State'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "Region")

item69.cast <- dcast(setDT(item69.final)
                     , formula = BuildingType + Lamp.Category ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item69.table <- data.frame("BuildingType"    = item69.cast$BuildingType
                           ,"Lamp.Type"      = item69.cast$Lamp.Category
                           ,"Percent_ID"     = item69.cast$w.percent_ID
                           ,"SE_ID"          = item69.cast$w.SE_ID
                           ,"Count_ID"       = item69.cast$count_ID
                           ,"Percent_MT"     = item69.cast$w.percent_MT
                           ,"SE_MT"          = item69.cast$w.SE_MT
                           ,"Count_MT"       = item69.cast$count_MT
                           ,"Percent_OR"     = item69.cast$w.percent_OR
                           ,"SE_OR"          = item69.cast$w.SE_OR
                           ,"Count_OR"       = item69.cast$count_OR
                           ,"Percent_WA"     = item69.cast$w.percent_WA
                           ,"SE_WA"          = item69.cast$w.SE_WA
                           ,"Count_WA"       = item69.cast$count_WA
                           ,"Percent_Region" = item69.cast$w.percent_Region
                           ,"SE_Region"      = item69.cast$w.SE_Region
                           ,"Count_Region"   = item69.cast$count_Region
                           # ,"SampleSize"     = item69.cast$SampleSize_Region
)


item69.final.SF <- item69.table[which(item69.table$BuildingType == "Single Family")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]
item69.final.MH <- item69.table[which(item69.table$BuildingType == "Manufactured")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]

exportTable(item69.final.SF, "SF", "Table 76", weighted = TRUE)
exportTable(item69.final.MH, "MH", "Table 55", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item69.final <- proportions_two_groups_unweighted(CustomerLevelData = item69.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'State'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "Region")

item69.cast <- dcast(setDT(item69.final)
                     , formula = BuildingType + Lamp.Category ~ State
                     , value.var = c("Percent", "SE", "Count", "SampleSize"))


item69.table <- data.frame("BuildingType"    = item69.cast$BuildingType
                           ,"Lamp.Type"      = item69.cast$Lamp.Category
                           ,"Percent_ID"     = item69.cast$Percent_ID
                           ,"SE_ID"          = item69.cast$SE_ID
                           ,"Count_ID"       = item69.cast$Count_ID
                           ,"Percent_MT"     = item69.cast$Percent_MT
                           ,"SE_MT"          = item69.cast$SE_MT
                           ,"Count_MT"       = item69.cast$Count_MT
                           ,"Percent_OR"     = item69.cast$Percent_OR
                           ,"SE_OR"          = item69.cast$SE_OR
                           ,"Count_OR"       = item69.cast$Count_OR
                           ,"Percent_WA"     = item69.cast$Percent_WA
                           ,"SE_WA"          = item69.cast$SE_WA
                           ,"Count_WA"       = item69.cast$Count_WA
                           ,"Percent_Region" = item69.cast$Percent_Region
                           ,"SE_Region"      = item69.cast$SE_Region
                           ,"Count_Region"   = item69.cast$Count_Region
                           # ,"SampleSize"     = item69.cast$SampleSize_Region
)

item69.final.SF <- item69.table[which(item69.table$BuildingType == "Single Family")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]
item69.final.MH <- item69.table[which(item69.table$BuildingType == "Manufactured")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]

exportTable(item69.final.SF, "SF", "Table 76", weighted = FALSE)
exportTable(item69.final.MH, "MH", "Table 55", weighted = FALSE)






#############################################################################################
#Item 70: DISTRIBUTION OF LAMPS BY TYPE AND ROOM (SF table 77, MH table 56, MF table 84)
#############################################################################################
#subset to columns needed for analysis
item70.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"))]
item70.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item70.dat1 <- left_join(rbsa.dat, item70.dat, by = "CK_Cadmus_ID")

item70.dat1.1 <- item70.dat1[which(!(item70.dat1$Clean.Room %in% c("Basement","Storage"))),]
item70.dat1.1$Clean.Room[which(item70.dat1.1$Clean.Room == "Mechanical")] <- "Other"
#remove building info
item70.dat2 <- item70.dat1.1[grep("SITE", item70.dat1.1$CK_SiteID),]

#clean fixture and bulbs per fixture
item70.dat2$Fixture.Qty <- as.numeric(as.character(item70.dat2$Fixture.Qty))
item70.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item70.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item70.dat2$Lamps <- item70.dat2$Fixture.Qty * item70.dat2$LIGHTING_BulbsPerFixture
unique(item70.dat2$Lamps)

#remove missing lamp quantities
item70.dat3 <- item70.dat2[which(!(is.na(item70.dat2$Lamps))),]

#check lamp types
unique(item70.dat3$Lamp.Category)
item70.dat4 <- item70.dat3[which(item70.dat3$Lamp.Category != "Unknown"),]


item70.merge <- left_join(rbsa.dat, item70.dat4)


################################################
# Adding pop and sample sizes for weights
################################################
item70.data <- weightedData(item70.merge[-which(colnames(item70.merge) %in% c("CK_SiteID"               
                                                                              ,"Fixture.Qty"
                                                                              ,"LIGHTING_BulbsPerFixture"
                                                                              ,"Lamp.Category"
                                                                              ,"count"
                                                                              ,"Lamps"
                                                                              ,"Clean.Room"))])
item70.data <- left_join(item70.data, item70.merge[which(colnames(item70.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"CK_SiteID"               
                                                                                       ,"Fixture.Qty"
                                                                                       ,"LIGHTING_BulbsPerFixture"
                                                                                       ,"Lamp.Category"
                                                                                       ,"count"
                                                                                       ,"Lamps"
                                                                                       ,"Clean.Room"))])
item70.data <- item70.data[which(!is.na(item70.data$Lamp.Category)),]


#######################
# Weighted Analysis
#######################
item70.final <- proportionRowsAndColumns1(CustomerLevelData = item70.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'Clean.Room'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "All Room Types")
item70.final <- item70.final[which(item70.final$Clean.Room != "All Room Types"),]


item70.all.room.types <- proportions_one_group(CustomerLevelData = item70.data
                                               ,valueVariable = 'count'
                                               ,groupingVariable = "Lamp.Category"
                                               ,total.name = "All Room Types"
                                               ,columnName = "Clean.Room"
                                               ,weighted = TRUE)
item70.all.room.types <- item70.all.room.types[which(item70.all.room.types$Lamp.Category != "Total"),]


item70.final <- rbind.data.frame(item70.final, item70.all.room.types, stringsAsFactors = F)

item70.cast <- dcast(setDT(item70.final)
                     , formula = BuildingType + Clean.Room ~ Lamp.Category
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item70.table <- data.frame("BuildingType"                  = item70.cast$BuildingType
                           ,"Room.Type"                    = item70.cast$Clean.Room
                           ,"Percent_CFL"                  = item70.cast$`w.percent_Compact Fluorescent`
                           ,"SE_CFL"                       = item70.cast$`w.SE_Compact Fluorescent`
                           ,"Count_CFL"                    = item70.cast$`count_Compact Fluorescent`
                           ,"Percent_Halogen"              = item70.cast$w.percent_Halogen
                           ,"SE_Halogen"                   = item70.cast$w.SE_Halogen
                           ,"Count_Halogen"                = item70.cast$count_Halogen
                           ,"Percent_Incandescent"         = item70.cast$w.percent_Incandescent
                           ,"SE_Incandescent"              = item70.cast$w.SE_Incandescent
                           ,"Count_Incandescent"           = item70.cast$count_Incandescent
                           ,"Percent_Incandescent.Halogen" = item70.cast$`w.percent_Incandescent / Halogen`
                           ,"SE_Incandescent.Halogen"      = item70.cast$`w.SE_Incandescent / Halogen`
                           ,"Count_Incandescent.Halogen"   = item70.cast$`count_Incandescent / Halogen`
                           ,"Percent_LED"                  = item70.cast$`w.percent_Light Emitting Diode`
                           ,"SE_LED"                       = item70.cast$`w.SE_Light Emitting Diode`
                           ,"Count_LED"                    = item70.cast$`count_Light Emitting Diode`
                           ,"Percent_LF"                   = item70.cast$`w.percent_Linear Fluorescent`
                           ,"SE_LF"                        = item70.cast$`w.SE_Linear Fluorescent`
                           ,"Count_LF"                     = item70.cast$`count_Linear Fluorescent`
                           ,"Percent_Other"                = item70.cast$w.percent_Other
                           ,"SE_Other"                     = item70.cast$w.SE_Other
                           ,"Count_Other"                  = item70.cast$count_Other
                           # ,"Percent_Total"                = item70.cast$w.percent_Total
                           # ,"SE_Total"                     = item70.cast$w.SE_Total
                           ,"Count_Total"                  = item70.cast$count_Total
                           # ,"SampleSize"     = item70.cast$SampleSize_Region
)


item70.final.SF <- item70.table[which(item70.table$BuildingType == "Single Family")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MH <- item70.table[which(item70.table$BuildingType == "Manufactured")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]

exportTable(item70.final.SF, "SF", "Table 77", weighted = TRUE)
exportTable(item70.final.MH, "MH", "Table 56", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item70.final <- proportions_two_groups_unweighted(CustomerLevelData = item70.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'Clean.Room'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "All Room Types")
item70.final <- item70.final[which(item70.final$Clean.Room != "All Room Types"),]


item70.all.room.types <- proportions_one_group(CustomerLevelData = item70.data
                                               ,valueVariable = 'count'
                                               ,groupingVariable = "Lamp.Category"
                                               ,total.name = "All Room Types"
                                               ,columnName = "Clean.Room"
                                               ,weighted = FALSE)
item70.all.room.types <- item70.all.room.types[which(item70.all.room.types$Lamp.Category != "Total"),]


item70.final <- rbind.data.frame(item70.final, item70.all.room.types, stringsAsFactors = F)

item70.cast <- dcast(setDT(item70.final)
                     , formula = BuildingType + Clean.Room ~ Lamp.Category
                     , value.var = c("Percent", "SE", "Count", "SampleSize"))

item70.table <- data.frame("BuildingType"                  = item70.cast$BuildingType
                           ,"Room.Type"                    = item70.cast$Clean.Room
                           ,"Percent_CFL"                  = item70.cast$`Percent_Compact Fluorescent`
                           ,"SE_CFL"                       = item70.cast$`SE_Compact Fluorescent`
                           ,"Count_CFL"                    = item70.cast$`Count_Compact Fluorescent`
                           ,"Percent_Halogen"              = item70.cast$Percent_Halogen
                           ,"SE_Halogen"                   = item70.cast$SE_Halogen
                           ,"Count_Halogen"                = item70.cast$Count_Halogen
                           ,"Percent_Incandescent"         = item70.cast$Percent_Incandescent
                           ,"SE_Incandescent"              = item70.cast$SE_Incandescent
                           ,"Count_Incandescent"           = item70.cast$Count_Incandescent
                           ,"Percent_Incandescent.Halogen" = item70.cast$`Percent_Incandescent / Halogen`
                           ,"SE_Incandescent.Halogen"      = item70.cast$`SE_Incandescent / Halogen`
                           ,"Count_Incandescent.Halogen"   = item70.cast$`Count_Incandescent / Halogen`
                           ,"Percent_LED"                  = item70.cast$`Percent_Light Emitting Diode`
                           ,"SE_LED"                       = item70.cast$`SE_Light Emitting Diode`
                           ,"Count_LED"                    = item70.cast$`Count_Light Emitting Diode`
                           ,"Percent_LF"                   = item70.cast$`Percent_Linear Fluorescent`
                           ,"SE_LF"                        = item70.cast$`SE_Linear Fluorescent`
                           ,"Count_LF"                     = item70.cast$`Count_Linear Fluorescent`
                           ,"Percent_Other"                = item70.cast$Percent_Other
                           ,"SE_Other"                     = item70.cast$SE_Other
                           ,"Count_Other"                  = item70.cast$Count_Other
                           # ,"Percent_Total"                = item70.cast$Percent_Total
                           # ,"SE_Total"                     = item70.cast$SE_Total
                           ,"Count_Total"                  = item70.cast$Count_Total
                           # ,"SampleSize"     = item70.cast$SampleSize_Region
)


item70.final.SF <- item70.table[which(item70.table$BuildingType == "Single Family")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MH <- item70.table[which(item70.table$BuildingType == "Manufactured")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]

exportTable(item70.final.SF, "SF", "Table 77", weighted = FALSE)
exportTable(item70.final.MH, "MH", "Table 56", weighted = FALSE)

