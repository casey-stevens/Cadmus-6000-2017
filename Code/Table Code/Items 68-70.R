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
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))


#############################################################################################
#Item 68: DISTRIBUTION OF LAMPS BY EISA CATEGORY AND STATE (SF table 75, MH table 54, MF table 81)
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
#Item 69: DISTRIBUTION OF LAMPS BY TYPE AND STATE (SF table 76, MH table 55, MF table 83)
#############################################################################################
#subset to columns needed for analysis
item69.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"Clean.Room"))]
item69.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item69.dat1 <- left_join(rbsa.dat, item69.dat, by = "CK_Cadmus_ID")

#remove building info
item69.dat1.5 <- item69.dat1[which(item69.dat1$Clean.Room != "Storage"),]
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
item69.merge <- item69.merge[which(!is.na(item69.merge$Lamp.Category)),]
item69.merge <- item69.merge[which(!is.na(item69.merge$Lamps)),]


################################################
# Adding pop and sample sizes for weights
################################################
item69.data <- weightedData(item69.merge[-which(colnames(item69.merge) %in% c("CK_SiteID"               
                                                                              ,"Fixture.Qty"
                                                                              ,"LIGHTING_BulbsPerFixture"
                                                                              ,"Lamp.Category"
                                                                              ,"count"
                                                                              ,"Lamps"
                                                                              ,"Clean.Room"))])
item69.data <- left_join(item69.data, item69.merge[which(colnames(item69.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"CK_SiteID"               
                                                                                       ,"Fixture.Qty"
                                                                                       ,"LIGHTING_BulbsPerFixture"
                                                                                       ,"Lamp.Category"
                                                                                       ,"count"
                                                                                       ,"Lamps"
                                                                                       ,"Clean.Room"))])
item69.data$count <- 1
#######################
# Weighted Analysis
#######################
item69.summary <- proportionRowsAndColumns1(CustomerLevelData = item69.data
                                          ,valueVariable    = 'Lamps'
                                          ,columnVariable   = 'State'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "Region")

item69.cast <- dcast(setDT(item69.summary)
                     , formula = BuildingType + Lamp.Category ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item69.table <- data.frame("BuildingType"    = item69.cast$BuildingType
                           ,"Lamp.Type"      = item69.cast$Lamp.Category
                           ,"Percent_ID"     = item69.cast$w.percent_ID
                           ,"SE_ID"          = item69.cast$w.SE_ID
                           ,"n_ID"           = item69.cast$n_ID
                           ,"Percent_MT"     = item69.cast$w.percent_MT
                           ,"SE_MT"          = item69.cast$w.SE_MT
                           ,"n_MT"           = item69.cast$n_MT
                           ,"Percent_OR"     = item69.cast$w.percent_OR
                           ,"SE_OR"          = item69.cast$w.SE_OR
                           ,"n_OR"           = item69.cast$n_OR
                           ,"Percent_WA"     = item69.cast$w.percent_WA
                           ,"SE_WA"          = item69.cast$w.SE_WA
                           ,"n_WA"           = item69.cast$n_WA
                           ,"Percent_Region" = item69.cast$w.percent_Region
                           ,"SE_Region"      = item69.cast$w.SE_Region
                           ,"n_Region"       = item69.cast$n_Region
)


item69.final.SF <- item69.table[which(item69.table$BuildingType == "Single Family")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]
item69.final.MH <- item69.table[which(item69.table$BuildingType == "Manufactured")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]
item69.final.MF <- item69.table[which(item69.table$BuildingType == "Multifamily")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]

exportTable(item69.final.SF, "SF", "Table 76", weighted = TRUE)
exportTable(item69.final.MH, "MH", "Table 55", weighted = TRUE)
exportTable(item69.final.MF, "MF", "Table 83", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item69.summary <- proportions_two_groups_unweighted(CustomerLevelData = item69.data
                                          ,valueVariable    = 'Lamps'
                                          ,columnVariable   = 'State'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "Region")

item69.cast <- dcast(setDT(item69.summary)
                     , formula = BuildingType + Lamp.Category ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))


item69.table <- data.frame("BuildingType"    = item69.cast$BuildingType
                           ,"Lamp.Type"      = item69.cast$Lamp.Category
                           ,"Percent_ID"     = item69.cast$Percent_ID
                           ,"SE_ID"          = item69.cast$SE_ID
                           ,"n_ID"           = item69.cast$n_ID
                           ,"Percent_MT"     = item69.cast$Percent_MT
                           ,"SE_MT"          = item69.cast$SE_MT
                           ,"n_MT"           = item69.cast$n_MT
                           ,"Percent_OR"     = item69.cast$Percent_OR
                           ,"SE_OR"          = item69.cast$SE_OR
                           ,"n_OR"           = item69.cast$n_OR
                           ,"Percent_WA"     = item69.cast$Percent_WA
                           ,"SE_WA"          = item69.cast$SE_WA
                           ,"n_WA"           = item69.cast$n_WA
                           ,"Percent_Region" = item69.cast$Percent_Region
                           ,"SE_Region"      = item69.cast$SE_Region
                           ,"n_Region"       = item69.cast$n_Region
)

item69.final.SF <- item69.table[which(item69.table$BuildingType == "Single Family")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]
item69.final.MH <- item69.table[which(item69.table$BuildingType == "Manufactured")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]
item69.final.MF <- item69.table[which(item69.table$BuildingType == "Multifamily")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]

exportTable(item69.final.SF, "SF", "Table 76", weighted = FALSE)
exportTable(item69.final.MH, "MH", "Table 55", weighted = FALSE)
exportTable(item69.final.MF, "MF", "Table 83", weighted = FALSE)






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

item70.dat1.1 <- item70.dat1[which(!(item70.dat1$Clean.Room %in% c("Storage"))),]
item70.dat1.1$Clean.Room[which(item70.dat1.1$Clean.Room %in% c("Attic"
                                                               ,"Basement"
                                                               ,"Crawlspace"
                                                               ,"Crawl Space"
                                                               ,"Mechanical"
                                                               ,"Grow Room"))] <- "Other"
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
item70.merge <- item70.merge[which(!is.na(item70.merge$Lamp.Category)),]
item70.merge <- item70.merge[which(!is.na(item70.merge$Lamps)),]
#clean room types
unique(item70.merge$Clean.Room)
item70.merge$Clean.Room[which(item70.merge$Clean.Room %in% c("Attic"
                                                           ,"Basement"
                                                           ,"Crawlspace"
                                                           ,"Crawl Space"
                                                           ,"Mechanical"
                                                           ,"Grow Room"))] <- "Other"

unique(item70.merge$Clean.Room)


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


#######################
# Weighted Analysis
#######################
item70.summary <- proportionRowsAndColumns1(CustomerLevelData = item70.data
                                          ,valueVariable    = 'Lamps'
                                          ,columnVariable   = 'Clean.Room'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "All Room Types")
item70.summary <- item70.summary[which(item70.summary$Clean.Room != "All Room Types"),]
# item70.summary <- item70.summary[which(colnames(item70.summary)     != "n")]
# colnames(item70.summary)[which(colnames(item70.summary) == "n_hj")] <- "n"

item70.all.room.types <- proportions_one_group(CustomerLevelData = item70.data
                                               ,valueVariable = 'Lamps'
                                               ,groupingVariable = "Lamp.Category"
                                               ,total.name = "All Room Types"
                                               ,columnName = "Clean.Room"
                                               ,weighted = TRUE
                                               ,two.prop.total = TRUE)
item70.all.room.types$Lamp.Category[which(item70.all.room.types$Lamp.Category == "Total")] <- "All Categories"

item70.samplesize <- proportions_one_group(CustomerLevelData = item70.data
                                           ,valueVariable = 'Lamps'
                                           ,groupingVariable = 'Clean.Room'
                                           ,total.name = 'All Categories'
                                           ,columnName = 'Lamp.Category'
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
item70.samplesize <- item70.samplesize[which(item70.samplesize$Clean.Room != "Total"),]

item70.final <- rbind.data.frame(item70.summary, item70.all.room.types, item70.samplesize, stringsAsFactors = F)

item70.cast <- dcast(setDT(item70.final)
                     , formula = BuildingType + Clean.Room ~ Lamp.Category
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item70.table <- data.frame("BuildingType"                  = item70.cast$BuildingType
                           ,"Room.Type"                    = item70.cast$Clean.Room
                           ,"Percent_CFL"                  = item70.cast$`w.percent_Compact Fluorescent`
                           ,"SE_CFL"                       = item70.cast$`w.SE_Compact Fluorescent`
                           # ,"n_CFL"                        = item70.cast$`n_Compact Fluorescent`
                           ,"Percent_Halogen"              = item70.cast$w.percent_Halogen
                           ,"SE_Halogen"                   = item70.cast$w.SE_Halogen
                           # ,"n_Halogen"                    = item70.cast$n_Halogen
                           ,"Percent_Incandescent"         = item70.cast$w.percent_Incandescent
                           ,"SE_Incandescent"              = item70.cast$w.SE_Incandescent
                           # ,"n_Incandescent"               = item70.cast$n_Incandescent
                           ,"Percent_Incandescent.Halogen" = item70.cast$`w.percent_Incandescent / Halogen`
                           ,"SE_Incandescent.Halogen"      = item70.cast$`w.SE_Incandescent / Halogen`
                           # ,"n_Incandescent.Halogen"       = item70.cast$`n_Incandescent / Halogen`
                           ,"Percent_LED"                  = item70.cast$`w.percent_Light Emitting Diode`
                           ,"SE_LED"                       = item70.cast$`w.SE_Light Emitting Diode`
                           # ,"n_LED"                        = item70.cast$`n_Light Emitting Diode`
                           ,"Percent_LF"                   = item70.cast$`w.percent_Linear Fluorescent`
                           ,"SE_LF"                        = item70.cast$`w.SE_Linear Fluorescent`
                           # ,"n_LF"                         = item70.cast$`n_Linear Fluorescent`
                           ,"Percent_Other"                = item70.cast$w.percent_Other
                           ,"SE_Other"                     = item70.cast$w.SE_Other
                           # ,"n_Other"                      = item70.cast$n_Other
                           ,"n"                            = item70.cast$`n_All Categories`
)


item70.final.SF <- item70.table[which(item70.table$BuildingType == "Single Family")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MH <- item70.table[which(item70.table$BuildingType == "Manufactured")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MF <- item70.table[which(item70.table$BuildingType == "Multifamily")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]

exportTable(item70.final.SF, "SF", "Table 77", weighted = TRUE)
exportTable(item70.final.MH, "MH", "Table 56", weighted = TRUE)
exportTable(item70.final.MF, "MF", "Table 84", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item70.summary <- proportions_two_groups_unweighted(CustomerLevelData = item70.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'Clean.Room'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "All Room Types")
item70.summary <- item70.summary[which(item70.summary$Clean.Room != "All Room Types"),]


item70.all.room.types <- proportions_one_group(CustomerLevelData = item70.data
                                               ,valueVariable = 'Lamps'
                                               ,groupingVariable = "Lamp.Category"
                                               ,total.name = "All Room Types"
                                               ,columnName = "Clean.Room"
                                               ,weighted = FALSE
                                               ,two.prop.total = TRUE)
item70.all.room.types$Lamp.Category[which(item70.all.room.types$Lamp.Category == "Total")] <- "All Categories"

item70.samplesize <- proportions_one_group(CustomerLevelData = item70.data
                                           ,valueVariable = 'Lamps'
                                           ,groupingVariable = 'Clean.Room'
                                           ,total.name = 'All Categories'
                                           ,columnName = 'Lamp.Category'
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
item70.samplesize <- item70.samplesize[which(item70.samplesize$Clean.Room != "Total"),]

item70.final <- rbind.data.frame(item70.summary, item70.all.room.types, item70.samplesize, stringsAsFactors = F)

item70.cast <- dcast(setDT(item70.final)
                     , formula = BuildingType + Clean.Room ~ Lamp.Category
                     , value.var = c("Percent", "SE", "Count", "n"))

item70.table <- data.frame("BuildingType"                  = item70.cast$BuildingType
                           ,"Room.Type"                    = item70.cast$Clean.Room
                           ,"Percent_CFL"                  = item70.cast$`Percent_Compact Fluorescent`
                           ,"SE_CFL"                       = item70.cast$`SE_Compact Fluorescent`
                           # ,"n_CFL"                        = item70.cast$`n_Compact Fluorescent`
                           ,"Percent_Halogen"              = item70.cast$Percent_Halogen
                           ,"SE_Halogen"                   = item70.cast$SE_Halogen
                           # ,"n_Halogen"                    = item70.cast$n_Halogen
                           ,"Percent_Incandescent"         = item70.cast$Percent_Incandescent
                           ,"SE_Incandescent"              = item70.cast$SE_Incandescent
                           # ,"n_Incandescent"               = item70.cast$n_Incandescent
                           ,"Percent_Incandescent.Halogen" = item70.cast$`Percent_Incandescent / Halogen`
                           ,"SE_Incandescent.Halogen"      = item70.cast$`SE_Incandescent / Halogen`
                           # ,"n_Incandescent.Halogen"       = item70.cast$`n_Incandescent / Halogen`
                           ,"Percent_LED"                  = item70.cast$`Percent_Light Emitting Diode`
                           ,"SE_LED"                       = item70.cast$`SE_Light Emitting Diode`
                           # ,"n_LED"                        = item70.cast$`n_Light Emitting Diode`
                           ,"Percent_LF"                   = item70.cast$`Percent_Linear Fluorescent`
                           ,"SE_LF"                        = item70.cast$`SE_Linear Fluorescent`
                           # ,"n_LF"                         = item70.cast$`n_Linear Fluorescent`
                           ,"Percent_Other"                = item70.cast$Percent_Other
                           ,"SE_Other"                     = item70.cast$SE_Other
                           # ,"n_Other"                      = item70.cast$n_Other
                           ,"n"                            = item70.cast$`n_All Categories`
)


item70.final.SF <- item70.table[which(item70.table$BuildingType == "Single Family")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MH <- item70.table[which(item70.table$BuildingType == "Manufactured")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MF <- item70.table[which(item70.table$BuildingType == "Multifamily")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]

exportTable(item70.final.SF, "SF", "Table 77", weighted = FALSE)
exportTable(item70.final.MH, "MH", "Table 56", weighted = FALSE)
exportTable(item70.final.MF, "MF", "Table 84", weighted = FALSE)

