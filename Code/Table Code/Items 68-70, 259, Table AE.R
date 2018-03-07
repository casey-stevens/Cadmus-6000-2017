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
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))


#############################################################################################
#Item 68: DISTRIBUTION OF LAMPS BY EISA CATEGORY AND STATE (SF table 75, MH table 54, MF table 81)
#############################################################################################
#subset to columns needed for analysis
item68.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Lamp.Category"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"Status"
                                                               ,""
                                                               ,""))]
item68.dat$count <- 1

item68.dat1 <- left_join(rbsa.dat, item68.dat, by = "CK_Cadmus_ID")

item68.dat2 <- item68.dat1[-grep("BLDG", item68.dat1$CK_Building_ID),]

#clean fixture and bulbs per fixture
item68.dat2$Fixture.Qty <- as.numeric(as.character(item68.dat2$Fixture.Qty))
item68.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item68.dat2$LIGHTING_BulbsPerFixture))

item68.dat2$Lamps <- item68.dat2$Fixture.Qty * item68.dat2$LIGHTING_BulbsPerFixture
unique(item68.dat2$Lamps)

item68.dat3 <- item68.dat2[which(!(is.na(item68.dat2$Lamps))),]
unique(item68.dat3$Status)
item68.dat4 <- item68.dat3[which(item68.dat3$Status %notin% c("Empty Socket", "Unknown")),]


################################################
# Adding pop and sample sizes for weights
################################################
item68.data <- weightedData(item68.dat4[-which(colnames(item68.dat4) %in% c("Fixture.Qty"
                                                                            ,"LIGHTING_BulbsPerFixture"
                                                                            ,"Lamp.Category"
                                                                            ,"Lamps"
                                                                            ,"count"
                                                                            ,"Status"))])
item68.data <- left_join(item68.data, item68.dat4[which(colnames(item68.dat4) %in% c("CK_Cadmus_ID"               
                                                                                     ,"Fixture.Qty"
                                                                                     ,"LIGHTING_BulbsPerFixture"
                                                                                     ,"Lamp.Category"
                                                                                     ,"Lamps"
                                                                                     ,"count"
                                                                                     ,"Status"))])
item68.data$count <- 1
#######################
# Weighted Analysis
#######################
item68.summary <- proportionRowsAndColumns1(CustomerLevelData = item68.data
                                            ,valueVariable    = 'Lamps'
                                            ,columnVariable   = 'State'
                                            ,rowVariable      = 'Status'
                                            ,aggregateColumnName = "Region")

item68.cast <- dcast(setDT(item68.summary)
                     , formula = BuildingType + Status ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item68.table <- data.frame("BuildingType"    = item68.cast$BuildingType
                           ,"EISA.Category"  = item68.cast$Status
                           ,"Percent_ID"     = item68.cast$w.percent_ID
                           ,"SE_ID"          = item68.cast$w.SE_ID
                           ,"n_ID"           = item68.cast$n_ID
                           ,"Percent_MT"     = item68.cast$w.percent_MT
                           ,"SE_MT"          = item68.cast$w.SE_MT
                           ,"n_MT"           = item68.cast$n_MT
                           ,"Percent_OR"     = item68.cast$w.percent_OR
                           ,"SE_OR"          = item68.cast$w.SE_OR
                           ,"n_OR"           = item68.cast$n_OR
                           ,"Percent_WA"     = item68.cast$w.percent_WA
                           ,"SE_WA"          = item68.cast$w.SE_WA
                           ,"n_WA"           = item68.cast$n_WA
                           ,"Percent_Region" = item68.cast$w.percent_Region
                           ,"SE_Region"      = item68.cast$w.SE_Region
                           ,"n_Region"       = item68.cast$n_Region
                           ,"EB_ID"          = item68.cast$EB_ID
                           ,"EB_MT"          = item68.cast$EB_MT
                           ,"EB_OR"          = item68.cast$EB_OR
                           ,"EB_WA"          = item68.cast$EB_WA
                           ,"EB_Region"      = item68.cast$EB_Region
)

levels(item68.table$EISA.Category)
rowOrder <- c("Exempt"
              ,"Noncompliant"
              ,"Compliant"
              ,"Total")
item68.table <- item68.table %>% mutate(EISA.Category = factor(EISA.Category, levels = rowOrder)) %>% arrange(EISA.Category)  
item68.table <- data.frame(item68.table)


item68.final.SF <- item68.table[which(item68.table$BuildingType == "Single Family")
                                ,-which(colnames(item68.table) %in% c("BuildingType"))]
item68.final.MH <- item68.table[which(item68.table$BuildingType == "Manufactured")
                                ,-which(colnames(item68.table) %in% c("BuildingType"))]

# exportTable(item68.final.SF, "SF", "Table 75", weighted = TRUE)
exportTable(item68.final.MH, "MH", "Table 54", weighted = TRUE)


################################################################################
# For Multifamily
################################################################################
item68.table.MF <- proportions_one_group(CustomerLevelData    = item68.data
                                            ,valueVariable    = 'Lamps'
                                            ,groupingVariable = 'Status'
                                            ,total.name       = "Remove")
item68.table.MF <- item68.table.MF[which(item68.table.MF$Status != "Total"),]

item68.final.MF <- item68.table.MF[which(item68.table.MF$BuildingType == "Multifamily")
                                ,-which(colnames(item68.table.MF) %in% c("BuildingType"))]
# exportTable(item68.final.MF, "MF", "Table 81", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item68.summary <- proportions_two_groups_unweighted(CustomerLevelData = item68.data
                                                    ,valueVariable    = 'Lamps'
                                                    ,columnVariable   = 'State'
                                                    ,rowVariable      = 'Status'
                                                    ,aggregateColumnName = "Region")

item68.cast <- dcast(setDT(item68.summary)
                     , formula = BuildingType + Status ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))


item68.table <- data.frame("BuildingType"    = item68.cast$BuildingType
                           ,"EISA.Category"  = item68.cast$Status
                           ,"Percent_ID"     = item68.cast$Percent_ID
                           ,"SE_ID"          = item68.cast$SE_ID
                           ,"n_ID"           = item68.cast$n_ID
                           ,"Percent_MT"     = item68.cast$Percent_MT
                           ,"SE_MT"          = item68.cast$SE_MT
                           ,"n_MT"           = item68.cast$n_MT
                           ,"Percent_OR"     = item68.cast$Percent_OR
                           ,"SE_OR"          = item68.cast$SE_OR
                           ,"n_OR"           = item68.cast$n_OR
                           ,"Percent_WA"     = item68.cast$Percent_WA
                           ,"SE_WA"          = item68.cast$SE_WA
                           ,"n_WA"           = item68.cast$n_WA
                           ,"Percent_Region" = item68.cast$Percent_Region
                           ,"SE_Region"      = item68.cast$SE_Region
                           ,"n_Region"       = item68.cast$n_Region
)

item68.final.SF <- item68.table[which(item68.table$BuildingType == "Single Family")
                                ,-which(colnames(item68.table) %in% c("BuildingType"))]
item68.final.MH <- item68.table[which(item68.table$BuildingType == "Manufactured")
                                ,-which(colnames(item68.table) %in% c("BuildingType"))]

# exportTable(item68.final.SF, "SF", "Table 75", weighted = FALSE)
exportTable(item68.final.MH, "MH", "Table 54", weighted = FALSE)


################################################################################
# For Multifamily
################################################################################
item68.table.MF <- proportions_one_group(CustomerLevelData = item68.data
                                         ,valueVariable    = 'Lamps'
                                         ,groupingVariable = 'Status'
                                         ,total.name       = "Remove"
                                         ,weighted         = FALSE)
item68.table.MF <- item68.table.MF[which(item68.table.MF$Status != "Total"),]

item68.final.MF <- item68.table.MF[which(item68.table.MF$BuildingType == "Multifamily")
                                   ,-which(colnames(item68.table.MF) %in% c("BuildingType"))]
# exportTable(item68.final.MF, "MF", "Table 81", weighted = FALSE)







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
item69.dat3 <- item69.dat2[which(item69.dat2$Lamp.Category %notin% c("N/A", NA)),]

#check lamp types
unique(item69.dat3$Lamp.Category)


item69.merge <- left_join(rbsa.dat, item69.dat3)
item69.merge <- item69.merge[which(item69.merge$Lamp.Category %notin% c("N/A", NA)),]
item69.merge <- item69.merge[which(item69.merge$Lamps %notin% c("Unknown", "N/A", NA)),]


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
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

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
                           ,"EB_ID"          = item69.cast$EB_ID
                           ,"EB_MT"          = item69.cast$EB_MT
                           ,"EB_OR"          = item69.cast$EB_OR
                           ,"EB_WA"          = item69.cast$EB_WA
                           ,"EB_Region"      = item69.cast$EB_Region
)


item69.final.SF <- item69.table[which(item69.table$BuildingType == "Single Family")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]
item69.final.MH <- item69.table[which(item69.table$BuildingType == "Manufactured")
                                ,-which(colnames(item69.table) %in% c("BuildingType"))]

# exportTable(item69.final.SF, "SF", "Table 76", weighted = TRUE)
exportTable(item69.final.MH, "MH", "Table 55", weighted = TRUE)

################################################################################
# For Multifamily
################################################################################
item69.table.MF <- proportions_one_group(CustomerLevelData = item69.data
                                         ,valueVariable    = 'Lamps'
                                         ,groupingVariable = 'Lamp.Category'
                                         ,total.name       = "All Lamp Types"
                                         ,weighted         = TRUE)
# item69.table.MF <- item69.table.MF[which(item69.table.MF$Lamp.Category != "Total"),]

item69.final.MF <- item69.table.MF[which(item69.table.MF$BuildingType == "Multifamily")
                                   ,-which(colnames(item69.table.MF) %in% c("BuildingType"))]
# exportTable(item69.final.MF, "MF", "Table 83", weighted = TRUE)



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

# exportTable(item69.final.SF, "SF", "Table 76", weighted = FALSE)
exportTable(item69.final.MH, "MH", "Table 55", weighted = FALSE)

################################################################################
# For Multifamily
################################################################################
item69.table.MF <- proportions_one_group(CustomerLevelData = item69.data
                                         ,valueVariable    = 'Lamps'
                                         ,groupingVariable = 'Lamp.Category'
                                         ,total.name       = "All Lamp Types"
                                         ,weighted         = FALSE)
# item69.table.MF <- item69.table.MF[which(item69.table.MF$Lamp.Category != "Total"),]

item69.final.MF <- item69.table.MF[which(item69.table.MF$BuildingType == "Multifamily")
                                   ,-which(colnames(item69.table.MF) %in% c("BuildingType"))]
# exportTable(item69.final.MF, "MF", "Table 83", weighted = FALSE)






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
item70.dat3 <- item70.dat2[which(item70.dat2$Lamps %notin% c("N/A",NA)),]

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
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
names(item70.cast)
item70.table <- data.frame("BuildingType"                  = item70.cast$BuildingType
                           ,"Room.Type"                    = item70.cast$Clean.Room
                           ,"Percent_CFL"                  = item70.cast$`w.percent_Compact Fluorescent`
                           ,"SE_CFL"                       = item70.cast$`w.SE_Compact Fluorescent`
                           ,"Percent_Halogen"              = item70.cast$w.percent_Halogen
                           ,"SE_Halogen"                   = item70.cast$w.SE_Halogen
                           ,"Percent_Incandescent"         = item70.cast$w.percent_Incandescent
                           ,"SE_Incandescent"              = item70.cast$w.SE_Incandescent
                           ,"Percent_Incandescent.Halogen" = item70.cast$`w.percent_Incandescent / Halogen`
                           ,"SE_Incandescent.Halogen"      = item70.cast$`w.SE_Incandescent / Halogen`
                           ,"Percent_LED"                  = item70.cast$`w.percent_Light Emitting Diode`
                           ,"SE_LED"                       = item70.cast$`w.SE_Light Emitting Diode`
                           ,"Percent_LF"                   = item70.cast$`w.percent_Linear Fluorescent`
                           ,"SE_LF"                        = item70.cast$`w.SE_Linear Fluorescent`
                           ,"Percent_Other"                = item70.cast$w.percent_Other
                           ,"SE_Other"                     = item70.cast$w.SE_Other
                           ,"n"                            = item70.cast$`n_All Categories`
                           ,"EB_CFL"                       = item70.cast$`EB_Compact Fluorescent`
                           ,"EB_Halogen"                   = item70.cast$EB_Halogen
                           ,"EB_Incandescent"              = item70.cast$EB_Incandescent
                           ,"EB_Incandescent.Halogen"      = item70.cast$`EB_Incandescent / Halogen`
                           ,"EB_LED"                       = item70.cast$`EB_Light Emitting Diode`
                           ,"EB_LF"                        = item70.cast$`EB_Linear Fluorescent`
                           ,"EB_Other"                     = item70.cast$EB_Other
)

# row ordering example code
levels(item70.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
item70.table <- item70.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)  
item70.table <- data.frame(item70.table)


item70.final.SF <- item70.table[which(item70.table$BuildingType == "Single Family")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MH <- item70.table[which(item70.table$BuildingType == "Manufactured")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MF <- item70.table[which(item70.table$BuildingType == "Multifamily")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]

# exportTable(item70.final.SF, "SF", "Table 77", weighted = TRUE)
exportTable(item70.final.MH, "MH", "Table 56", weighted = TRUE)
# exportTable(item70.final.MF, "MF", "Table 84", weighted = TRUE)


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


# row ordering example code
levels(item70.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
item70.table <- item70.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)  
item70.table <- data.frame(item70.table)


item70.final.SF <- item70.table[which(item70.table$BuildingType == "Single Family")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MH <- item70.table[which(item70.table$BuildingType == "Manufactured")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]
item70.final.MF <- item70.table[which(item70.table$BuildingType == "Multifamily")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]

# exportTable(item70.final.SF, "SF", "Table 77", weighted = FALSE)
exportTable(item70.final.MH, "MH", "Table 56", weighted = FALSE)
# exportTable(item70.final.MF, "MF", "Table 84", weighted = FALSE)








#############################################################################################
# Table AE: DISTRIBUTION OF LAMPS BY TYPE AND ROOM - DIFF BINS
#############################################################################################
tableAE.merge <- item70.merge
tableAE.merge$Clean.Room[which(tableAE.merge$Clean.Room %in% c("Dining Room","Living Room","Family Room"))] <- "Living Room"

################################################
# Adding pop and sample sizes for weights
################################################
tableAE.data <- weightedData(tableAE.merge[-which(colnames(tableAE.merge) %in% c("CK_SiteID"
                                                                              ,"Fixture.Qty"
                                                                              ,"LIGHTING_BulbsPerFixture"
                                                                              ,"Lamp.Category"
                                                                              ,"count"
                                                                              ,"Lamps"
                                                                              ,"Clean.Room"))])
tableAE.data <- left_join(tableAE.data, tableAE.merge[which(colnames(tableAE.merge) %in% c("CK_Cadmus_ID"
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
tableAE.summary <- proportionRowsAndColumns1(CustomerLevelData = tableAE.data
                                            ,valueVariable    = 'Lamps'
                                            ,columnVariable   = 'Clean.Room'
                                            ,rowVariable      = 'Lamp.Category'
                                            ,aggregateColumnName = "All Room Types")
tableAE.summary <- tableAE.summary[which(tableAE.summary$Clean.Room != "All Room Types"),]
# tableAE.summary <- tableAE.summary[which(colnames(tableAE.summary)     != "n")]
# colnames(tableAE.summary)[which(colnames(tableAE.summary) == "n_hj")] <- "n"

tableAE.all.room.types <- proportions_one_group(CustomerLevelData = tableAE.data
                                               ,valueVariable = 'Lamps'
                                               ,groupingVariable = "Lamp.Category"
                                               ,total.name = "All Room Types"
                                               ,columnName = "Clean.Room"
                                               ,weighted = TRUE
                                               ,two.prop.total = TRUE)
tableAE.all.room.types$Lamp.Category[which(tableAE.all.room.types$Lamp.Category == "Total")] <- "All Categories"

tableAE.samplesize <- proportions_one_group(CustomerLevelData = tableAE.data
                                           ,valueVariable = 'Lamps'
                                           ,groupingVariable = 'Clean.Room'
                                           ,total.name = 'All Categories'
                                           ,columnName = 'Lamp.Category'
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
tableAE.samplesize <- tableAE.samplesize[which(tableAE.samplesize$Clean.Room != "Total"),]

tableAE.final <- rbind.data.frame(tableAE.summary, tableAE.all.room.types, tableAE.samplesize, stringsAsFactors = F)

tableAE.cast <- dcast(setDT(tableAE.final)
                     , formula = BuildingType + Clean.Room ~ Lamp.Category
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAE.table <- data.frame("BuildingType"                  = tableAE.cast$BuildingType
                           ,"Room.Type"                    = tableAE.cast$Clean.Room
                           ,"Percent_CFL"                  = tableAE.cast$`w.percent_Compact Fluorescent`
                           ,"SE_CFL"                       = tableAE.cast$`w.SE_Compact Fluorescent`
                           # ,"n_CFL"                        = tableAE.cast$`n_Compact Fluorescent`
                           ,"Percent_Halogen"              = tableAE.cast$w.percent_Halogen
                           ,"SE_Halogen"                   = tableAE.cast$w.SE_Halogen
                           # ,"n_Halogen"                    = tableAE.cast$n_Halogen
                           ,"Percent_Incandescent"         = tableAE.cast$w.percent_Incandescent
                           ,"SE_Incandescent"              = tableAE.cast$w.SE_Incandescent
                           # ,"n_Incandescent"               = tableAE.cast$n_Incandescent
                           ,"Percent_Incandescent.Halogen" = tableAE.cast$`w.percent_Incandescent / Halogen`
                           ,"SE_Incandescent.Halogen"      = tableAE.cast$`w.SE_Incandescent / Halogen`
                           # ,"n_Incandescent.Halogen"       = tableAE.cast$`n_Incandescent / Halogen`
                           ,"Percent_LED"                  = tableAE.cast$`w.percent_Light Emitting Diode`
                           ,"SE_LED"                       = tableAE.cast$`w.SE_Light Emitting Diode`
                           # ,"n_LED"                        = tableAE.cast$`n_Light Emitting Diode`
                           ,"Percent_LF"                   = tableAE.cast$`w.percent_Linear Fluorescent`
                           ,"SE_LF"                        = tableAE.cast$`w.SE_Linear Fluorescent`
                           # ,"n_LF"                         = tableAE.cast$`n_Linear Fluorescent`
                           ,"Percent_Other"                = tableAE.cast$w.percent_Other
                           ,"SE_Other"                     = tableAE.cast$w.SE_Other
                           # ,"n_Other"                      = tableAE.cast$n_Other
                           ,"n"                            = tableAE.cast$`n_All Categories`
                           ,"EB_CFL"                       = tableAE.cast$`EB_Compact Fluorescent`
                           ,"EB_Halogen"                   = tableAE.cast$EB_Halogen
                           ,"EB_Incandescent"              = tableAE.cast$EB_Incandescent
                           ,"EB_Incandescent.Halogen"      = tableAE.cast$`EB_Incandescent / Halogen`
                           ,"EB_LED"                       = tableAE.cast$`EB_Light Emitting Diode`
                           ,"EB_LF"                        = tableAE.cast$`EB_Linear Fluorescent`
                           ,"EB_Other"                     = tableAE.cast$EB_Other
)

# row ordering example code
levels(tableAE.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
tableAE.table <- tableAE.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)
tableAE.table <- data.frame(tableAE.table)


tableAE.final.SF <- tableAE.table[which(tableAE.table$BuildingType == "Single Family")
                                ,-which(colnames(tableAE.table) %in% c("BuildingType"))]
tableAE.final.MH <- tableAE.table[which(tableAE.table$BuildingType == "Manufactured")
                                ,-which(colnames(tableAE.table) %in% c("BuildingType"))]
tableAE.final.MF <- tableAE.table[which(tableAE.table$BuildingType == "Multifamily")
                                ,-which(colnames(tableAE.table) %in% c("BuildingType"))]

# exportTable(tableAE.final.SF, "SF", "Table AE", weighted = TRUE)
exportTable(tableAE.final.MH, "MH", "Table AE", weighted = TRUE)
exportTable(tableAE.final.MF, "MF", "Table AE", weighted = TRUE)


#######################
# # Unweighted Analysis
# #######################
tableAE.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableAE.data
                                                    ,valueVariable    = 'count'
                                                    ,columnVariable   = 'Clean.Room'
                                                    ,rowVariable      = 'Lamp.Category'
                                                    ,aggregateColumnName = "All Room Types")
tableAE.summary <- tableAE.summary[which(tableAE.summary$Clean.Room != "All Room Types"),]


tableAE.all.room.types <- proportions_one_group(CustomerLevelData = tableAE.data
                                               ,valueVariable = 'Lamps'
                                               ,groupingVariable = "Lamp.Category"
                                               ,total.name = "All Room Types"
                                               ,columnName = "Clean.Room"
                                               ,weighted = FALSE
                                               ,two.prop.total = TRUE)
tableAE.all.room.types$Lamp.Category[which(tableAE.all.room.types$Lamp.Category == "Total")] <- "All Categories"

tableAE.samplesize <- proportions_one_group(CustomerLevelData = tableAE.data
                                           ,valueVariable = 'Lamps'
                                           ,groupingVariable = 'Clean.Room'
                                           ,total.name = 'All Categories'
                                           ,columnName = 'Lamp.Category'
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
tableAE.samplesize <- tableAE.samplesize[which(tableAE.samplesize$Clean.Room != "Total"),]

tableAE.final <- rbind.data.frame(tableAE.summary, tableAE.all.room.types, tableAE.samplesize, stringsAsFactors = F)

tableAE.cast <- dcast(setDT(tableAE.final)
                     , formula = BuildingType + Clean.Room ~ Lamp.Category
                     , value.var = c("Percent", "SE", "Count", "n"))

tableAE.table <- data.frame("BuildingType"                  = tableAE.cast$BuildingType
                           ,"Room.Type"                    = tableAE.cast$Clean.Room
                           ,"Percent_CFL"                  = tableAE.cast$`Percent_Compact Fluorescent`
                           ,"SE_CFL"                       = tableAE.cast$`SE_Compact Fluorescent`
                           # ,"n_CFL"                        = tableAE.cast$`n_Compact Fluorescent`
                           ,"Percent_Halogen"              = tableAE.cast$Percent_Halogen
                           ,"SE_Halogen"                   = tableAE.cast$SE_Halogen
                           # ,"n_Halogen"                    = tableAE.cast$n_Halogen
                           ,"Percent_Incandescent"         = tableAE.cast$Percent_Incandescent
                           ,"SE_Incandescent"              = tableAE.cast$SE_Incandescent
                           # ,"n_Incandescent"               = tableAE.cast$n_Incandescent
                           ,"Percent_Incandescent.Halogen" = tableAE.cast$`Percent_Incandescent / Halogen`
                           ,"SE_Incandescent.Halogen"      = tableAE.cast$`SE_Incandescent / Halogen`
                           # ,"n_Incandescent.Halogen"       = tableAE.cast$`n_Incandescent / Halogen`
                           ,"Percent_LED"                  = tableAE.cast$`Percent_Light Emitting Diode`
                           ,"SE_LED"                       = tableAE.cast$`SE_Light Emitting Diode`
                           # ,"n_LED"                        = tableAE.cast$`n_Light Emitting Diode`
                           ,"Percent_LF"                   = tableAE.cast$`Percent_Linear Fluorescent`
                           ,"SE_LF"                        = tableAE.cast$`SE_Linear Fluorescent`
                           # ,"n_LF"                         = tableAE.cast$`n_Linear Fluorescent`
                           ,"Percent_Other"                = tableAE.cast$Percent_Other
                           ,"SE_Other"                     = tableAE.cast$SE_Other
                           # ,"n_Other"                      = tableAE.cast$n_Other
                           ,"n"                            = tableAE.cast$`n_All Categories`
)


# row ordering example code
levels(tableAE.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
tableAE.table <- tableAE.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)
tableAE.table <- data.frame(tableAE.table)


tableAE.final.SF <- tableAE.table[which(tableAE.table$BuildingType == "Single Family")
                                ,-which(colnames(tableAE.table) %in% c("BuildingType"))]
tableAE.final.MH <- tableAE.table[which(tableAE.table$BuildingType == "Manufactured")
                                ,-which(colnames(tableAE.table) %in% c("BuildingType"))]
tableAE.final.MF <- tableAE.table[which(tableAE.table$BuildingType == "Multifamily")
                                ,-which(colnames(tableAE.table) %in% c("BuildingType"))]

# exportTable(tableAE.final.SF, "SF", "Table AE", weighted = FALSE)
exportTable(tableAE.final.MH, "MH", "Table AE", weighted = FALSE)
exportTable(tableAE.final.MF, "MF", "Table AE", weighted = FALSE)
























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





























############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#############################################################################################
#Item 68: DISTRIBUTION OF LAMPS BY EISA CATEGORY AND CK_Building_ID (SF table 75, MH table 54, MF table 81)
#############################################################################################
#subset to columns needed for analysis
item68.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Lamp.Category"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"Status"
                                                               ,""
                                                               ,""))]
item68.os.dat$count <- 1

item68.os.dat2 <- left_join(os.dat, item68.os.dat, by = "CK_Cadmus_ID")

#clean fixture and bulbs per fixture
item68.os.dat2$Fixture.Qty <- as.numeric(as.character(item68.os.dat2$Fixture.Qty))
item68.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item68.os.dat2$LIGHTING_BulbsPerFixture))

item68.os.dat2$Lamps <- item68.os.dat2$Fixture.Qty * item68.os.dat2$LIGHTING_BulbsPerFixture
unique(item68.os.dat2$Lamps)

item68.os.dat3 <- item68.os.dat2[which(!(is.na(item68.os.dat2$Lamps))),]
unique(item68.os.dat3$Status)
item68.os.dat4 <- item68.os.dat3[which(item68.os.dat3$Status %notin% c("Empty Socket", "Unknown")),]


################################################
# Adding pop and sample sizes for weights
################################################
item68.os.data <- weightedData(item68.os.dat4[-which(colnames(item68.os.dat4) %in% c("Fixture.Qty"
                                                                            ,"LIGHTING_BulbsPerFixture"
                                                                            ,"Lamp.Category"
                                                                            ,"Lamps"
                                                                            ,"count"
                                                                            ,"Status"))])
item68.os.data <- left_join(item68.os.data, unique(item68.os.dat4[which(colnames(item68.os.dat4) %in% c("CK_Cadmus_ID"               
                                                                                     ,"Fixture.Qty"
                                                                                     ,"LIGHTING_BulbsPerFixture"
                                                                                     ,"Lamp.Category"
                                                                                     ,"Lamps"
                                                                                     ,"count"
                                                                                     ,"Status"))]))
item68.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item68.os.summary <- proportionRowsAndColumns1(CustomerLevelData = item68.os.data
                                            ,valueVariable    = 'Lamps'
                                            ,columnVariable   = 'CK_Building_ID'
                                            ,rowVariable      = 'Status'
                                            ,aggregateColumnName = "Remove")

item68.os.cast <- dcast(setDT(item68.os.summary)
                     , formula = BuildingType + Status ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

names(item68.os.cast)
if(os.ind == "scl"){
  item68.os.table <- data.frame("BuildingType"    = item68.os.cast$BuildingType
                                ,"EISA.Category"  = item68.os.cast$Status
                                ,"Percent_SCL.GenPop"   = item68.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item68.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item68.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item68.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item68.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item68.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item68.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item68.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item68.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item68.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item68.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item68.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item68.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item68.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item68.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item68.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item68.os.table <- data.frame("BuildingType"    = item68.os.cast$BuildingType
                                ,"EISA.Category"  = item68.os.cast$Status
                                ,"Percent_SnoPUD"          = item68.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item68.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item68.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item68.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item68.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item68.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item68.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item68.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item68.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item68.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item68.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item68.os.cast$`EB_2017 RBSA NW`)
}





levels(item68.os.table$EISA.Category)
rowOrder <- c("Exempt"
              ,"Noncompliant"
              ,"Compliant"
              ,"Total")
item68.os.table <- item68.os.table %>% mutate(EISA.Category = factor(EISA.Category, levels = rowOrder)) %>% arrange(EISA.Category)  
item68.os.table <- data.frame(item68.os.table)


item68.os.final.SF <- item68.os.table[which(item68.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item68.os.table) %in% c("BuildingType"))]

exportTable(item68.os.final.SF, "SF", "Table 75", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item68.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = item68.os.data
                                                    ,valueVariable    = 'Lamps'
                                                    ,columnVariable   = 'CK_Building_ID'
                                                    ,rowVariable      = 'Status'
                                                    ,aggregateColumnName = "Remove")

item68.os.cast <- dcast(setDT(item68.os.summary)
                     , formula = BuildingType + Status ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))

names(item68.os.cast)
if(os.ind == "scl"){
  item68.os.table <- data.frame("BuildingType"    = item68.os.cast$BuildingType
                                ,"EISA.Category"  = item68.os.cast$Status
                                ,"Percent_SCL.GenPop"   = item68.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item68.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item68.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item68.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item68.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item68.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item68.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item68.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item68.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item68.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item68.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item68.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item68.os.table <- data.frame("BuildingType"    = item68.os.cast$BuildingType
                                ,"EISA.Category"  = item68.os.cast$Status
                                ,"Percent_SnoPUD"          = item68.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item68.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item68.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item68.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item68.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item68.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item68.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item68.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item68.os.cast$`n_2017 RBSA NW`)
}




item68.os.final.SF <- item68.os.table[which(item68.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item68.os.table) %in% c("BuildingType"))]

exportTable(item68.os.final.SF, "SF", "Table 75", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 69: DISTRIBUTION OF LAMPS BY TYPE AND CK_Building_ID (SF table 76, MH table 55, MF table 83)
#############################################################################################
#subset to columns needed for analysis
item69.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"Clean.Room"))]
item69.os.dat$count <- 1

#join clean scl data onto lighting analysis data
item69.os.dat1 <- left_join(os.dat, item69.os.dat, by = "CK_Cadmus_ID")

#remove building info
item69.os.dat1.5 <- item69.os.dat1[which(item69.os.dat1$Clean.Room != "Storage"),]
item69.os.dat2 <- item69.os.dat1[grep("SITE", item69.os.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item69.os.dat2$Fixture.Qty <- as.numeric(as.character(item69.os.dat2$Fixture.Qty))
item69.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item69.os.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item69.os.dat2$Lamps <- item69.os.dat2$Fixture.Qty * item69.os.dat2$LIGHTING_BulbsPerFixture
unique(item69.os.dat2$Lamps)

#remove missing lamp quantities
item69.os.dat3 <- item69.os.dat2[which(item69.os.dat2$Lamp.Category %notin% c("N/A", NA)),]

#check lamp types
unique(item69.os.dat3$Lamp.Category)


item69.os.merge <- left_join(os.dat, item69.os.dat3)
item69.os.merge <- item69.os.merge[which(item69.os.merge$Lamp.Category %notin% c("N/A", NA, "Unknown")),]
item69.os.merge <- item69.os.merge[which(item69.os.merge$Lamps %notin% c("Unknown", "N/A", NA)),]


################################################
# Adding pop and sample sizes for weights
################################################
item69.os.data <- weightedData(item69.os.merge[-which(colnames(item69.os.merge) %in% c("CK_SiteID"               
                                                                              ,"Fixture.Qty"
                                                                              ,"LIGHTING_BulbsPerFixture"
                                                                              ,"Lamp.Category"
                                                                              ,"count"
                                                                              ,"Lamps"
                                                                              ,"Clean.Room"))])
item69.os.data <- left_join(item69.os.data, unique(item69.os.merge[which(colnames(item69.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"CK_SiteID"               
                                                                                       ,"Fixture.Qty"
                                                                                       ,"LIGHTING_BulbsPerFixture"
                                                                                       ,"Lamp.Category"
                                                                                       ,"count"
                                                                                       ,"Lamps"
                                                                                       ,"Clean.Room"))]))
item69.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item69.os.summary <- proportionRowsAndColumns1(CustomerLevelData = item69.os.data
                                            ,valueVariable    = 'Lamps'
                                            ,columnVariable   = 'CK_Building_ID'
                                            ,rowVariable      = 'Lamp.Category'
                                            ,aggregateColumnName = "Remove")

item69.os.cast <- dcast(setDT(item69.os.summary)
                     , formula = BuildingType + Lamp.Category ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

names(item69.os.cast)
if(os.ind == "scl"){
  item69.os.table <- data.frame("BuildingType"    = item69.os.cast$BuildingType
                                ,"Lamp.Type"      = item69.os.cast$Lamp.Category
                                ,"Percent_SCL.GenPop"   = item69.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item69.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item69.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item69.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item69.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item69.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item69.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item69.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item69.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item69.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item69.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item69.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item69.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item69.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item69.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item69.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item69.os.table <- data.frame("BuildingType"    = item69.os.cast$BuildingType
                                ,"Lamp.Type"      = item69.os.cast$Lamp.Category
                                ,"Percent_SnoPUD"          = item69.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item69.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item69.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item69.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item69.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item69.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item69.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item69.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item69.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item69.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item69.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item69.os.cast$`EB_2017 RBSA NW`)
}





item69.os.final.SF <- item69.os.table[which(item69.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item69.os.table) %in% c("BuildingType"))]

exportTable(item69.os.final.SF, "SF", "Table 76", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item69.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = item69.os.data
                                                    ,valueVariable    = 'Lamps'
                                                    ,columnVariable   = 'CK_Building_ID'
                                                    ,rowVariable      = 'Lamp.Category'
                                                    ,aggregateColumnName = "Remove")

item69.os.cast <- dcast(setDT(item69.os.summary)
                     , formula = BuildingType + Lamp.Category ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))

names(item69.os.cast)
if(os.ind == "scl"){
  item69.os.table <- data.frame("BuildingType"    = item69.os.cast$BuildingType
                                ,"Lamp.Type"      = item69.os.cast$Lamp.Category
                                ,"Percent_SCL.GenPop"   = item69.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item69.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item69.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item69.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item69.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item69.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item69.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item69.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item69.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item69.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item69.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item69.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item69.os.table <- data.frame("BuildingType"    = item69.os.cast$BuildingType
                                ,"Lamp.Type"      = item69.os.cast$Lamp.Category
                                ,"Percent_SnoPUD"          = item69.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item69.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item69.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item69.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item69.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item69.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item69.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item69.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item69.os.cast$`n_2017 RBSA NW`)
}



item69.os.final.SF <- item69.os.table[which(item69.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item69.os.table) %in% c("BuildingType"))]

exportTable(item69.os.final.SF, "SF", "Table 76", weighted = FALSE, osIndicator = export.ind, OS = T)



#############################################################################################
#Item 70: DISTRIBUTION OF LAMPS BY TYPE AND ROOM FOR SCL GENERAL POPULATION
#############################################################################################
#subset to columns needed for analysis
item70.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"))]
item70.os.dat$count <- 1

#join clean scl data onto lighting analysis data
item70.os.dat1 <- left_join(os.dat, item70.os.dat, by = "CK_Cadmus_ID")
item70.os.dat1 <- item70.os.dat1[which(item70.os.dat1$CK_Building_ID == subset.ind),]

item70.os.dat1.1 <- item70.os.dat1[which(!(item70.os.dat1$Clean.Room %in% c("Storage"))),]
item70.os.dat1.1$Clean.Room[which(item70.os.dat1.1$Clean.Room %in% c("Attic"
                                                               ,"Basement"
                                                               ,"Crawlspace"
                                                               ,"Crawl Space"
                                                               ,"Mechanical"
                                                               ,"Grow Room"))] <- "Other"
item70.os.dat2 <- item70.os.dat1.1[grep("SITE", item70.os.dat1.1$CK_SiteID),]

#clean fixture and bulbs per fixture
item70.os.dat2$Fixture.Qty <- as.numeric(as.character(item70.os.dat2$Fixture.Qty))
item70.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item70.os.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item70.os.dat2$Lamps <- item70.os.dat2$Fixture.Qty * item70.os.dat2$LIGHTING_BulbsPerFixture
unique(item70.os.dat2$Lamps)

#remove missing lamp quantities
item70.os.dat3 <- item70.os.dat2[which(item70.os.dat2$Lamps %notin% c("N/A",NA)),]

#check lamp types
unique(item70.os.dat3$Lamp.Category)
item70.os.dat4 <- item70.os.dat3[which(item70.os.dat3$Lamp.Category != "Unknown"),]


item70.os.merge <- left_join(os.dat, item70.os.dat4)
item70.os.merge <- item70.os.merge[which(!is.na(item70.os.merge$Lamp.Category)),]
item70.os.merge <- item70.os.merge[which(!is.na(item70.os.merge$Lamps)),]
#clean room types
unique(item70.os.merge$Clean.Room)
item70.os.merge$Clean.Room[which(item70.os.merge$Clean.Room %in% c("Attic"
                                                             ,"Basement"
                                                             ,"Crawlspace"
                                                             ,"Crawl Space"
                                                             ,"Mechanical"
                                                             ,"Grow Room"))] <- "Other"

unique(item70.os.merge$Clean.Room)


################################################
# Adding pop and sample sizes for weights
################################################
item70.os.data <- weightedData(item70.os.merge[-which(colnames(item70.os.merge) %in% c("CK_SiteID"               
                                                                              ,"Fixture.Qty"
                                                                              ,"LIGHTING_BulbsPerFixture"
                                                                              ,"Lamp.Category"
                                                                              ,"count"
                                                                              ,"Lamps"
                                                                              ,"Clean.Room"))])
item70.os.data <- left_join(item70.os.data, unique(item70.os.merge[which(colnames(item70.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"CK_SiteID"               
                                                                                       ,"Fixture.Qty"
                                                                                       ,"LIGHTING_BulbsPerFixture"
                                                                                       ,"Lamp.Category"
                                                                                       ,"count"
                                                                                       ,"Lamps"
                                                                                       ,"Clean.Room"))]))


#######################
# Weighted Analysis
#######################
item70.os.summary <- proportionRowsAndColumns1(CustomerLevelData = item70.os.data
                                            ,valueVariable    = 'Lamps'
                                            ,columnVariable   = 'Clean.Room'
                                            ,rowVariable      = 'Lamp.Category'
                                            ,aggregateColumnName = "All Room Types")
item70.os.summary <- item70.os.summary[which(item70.os.summary$Clean.Room != "All Room Types"),]
# item70.os.summary <- item70.os.summary[which(colnames(item70.os.summary)     != "n")]
# colnames(item70.os.summary)[which(colnames(item70.os.summary) == "n_hj")] <- "n"

item70.os.all.room.types <- proportions_one_group(CustomerLevelData = item70.os.data
                                               ,valueVariable = 'Lamps'
                                               ,groupingVariable = "Lamp.Category"
                                               ,total.name = "All Room Types"
                                               ,columnName = "Clean.Room"
                                               ,weighted = TRUE
                                               ,two.prop.total = TRUE)
item70.os.all.room.types$Lamp.Category[which(item70.os.all.room.types$Lamp.Category == "Total")] <- "All Categories"

item70.os.samplesize <- proportions_one_group(CustomerLevelData = item70.os.data
                                           ,valueVariable = 'Lamps'
                                           ,groupingVariable = 'Clean.Room'
                                           ,total.name = 'All Categories'
                                           ,columnName = 'Lamp.Category'
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
item70.os.samplesize <- item70.os.samplesize[which(item70.os.samplesize$Clean.Room != "Total"),]

item70.os.final <- rbind.data.frame(item70.os.summary, item70.os.all.room.types, item70.os.samplesize, stringsAsFactors = F)

item70.os.cast <- dcast(setDT(item70.os.final)
                     , formula = BuildingType + Clean.Room ~ Lamp.Category
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

names(item70.os.cast)

item70.os.table <- data.frame("BuildingType"                  = item70.os.cast$BuildingType
                                ,"Room.Type"                    = item70.os.cast$Clean.Room
                                ,"Percent_CFL"                  = item70.os.cast$`w.percent_Compact Fluorescent`
                                ,"SE_CFL"                       = item70.os.cast$`w.SE_Compact Fluorescent`
                                ,"Percent_Halogen"              = item70.os.cast$w.percent_Halogen
                                ,"SE_Halogen"                   = item70.os.cast$w.SE_Halogen
                                ,"Percent_Incandescent"         = item70.os.cast$w.percent_Incandescent
                                ,"SE_Incandescent"              = item70.os.cast$w.SE_Incandescent
                                ,"Percent_Incandescent.Halogen" = item70.os.cast$`w.percent_Incandescent / Halogen`
                                ,"SE_Incandescent.Halogen"      = item70.os.cast$`w.SE_Incandescent / Halogen`
                                ,"Percent_LED"                  = item70.os.cast$`w.percent_Light Emitting Diode`
                                ,"SE_LED"                       = item70.os.cast$`w.SE_Light Emitting Diode`
                                ,"Percent_LF"                   = item70.os.cast$`w.percent_Linear Fluorescent`
                                ,"SE_LF"                        = item70.os.cast$`w.SE_Linear Fluorescent`
                                ,"Percent_Other"                = item70.os.cast$w.percent_Other
                                ,"SE_Other"                     = item70.os.cast$w.SE_Other
                                ,"n"                            = item70.os.cast$`n_All Categories`
                                ,"EB_CFL"                       = item70.os.cast$`EB_Compact Fluorescent`
                                ,"EB_Halogen"                   = item70.os.cast$EB_Halogen
                                ,"EB_Incandescent"              = item70.os.cast$EB_Incandescent
                                ,"EB_Incandescent.Halogen"      = item70.os.cast$`EB_Incandescent / Halogen`
                                ,"EB_LED"                       = item70.os.cast$`EB_Light Emitting Diode`
                                ,"EB_LF"                        = item70.os.cast$`EB_Linear Fluorescent`
                                ,"EB_Other"                     = item70.os.cast$EB_Other)





# row ordering example code
levels(item70.os.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
item70.os.table <- item70.os.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)  
item70.os.table <- data.frame(item70.os.table)


item70.os.final.SF <- item70.os.table[which(item70.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item70.os.table) %in% c("BuildingType"))]

exportTable(item70.os.final.SF, "SF", "Table 77", weighted = TRUE, osIndicator = export.ind, OS = T)


#######################
# Unweighted Analysis
#######################
item70.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = item70.os.data
                                                    ,valueVariable    = 'count'
                                                    ,columnVariable   = 'Clean.Room'
                                                    ,rowVariable      = 'Lamp.Category'
                                                    ,aggregateColumnName = "All Room Types")
item70.os.summary <- item70.os.summary[which(item70.os.summary$Clean.Room != "All Room Types"),]


item70.os.all.room.types <- proportions_one_group(CustomerLevelData = item70.os.data
                                               ,valueVariable = 'Lamps'
                                               ,groupingVariable = "Lamp.Category"
                                               ,total.name = "All Room Types"
                                               ,columnName = "Clean.Room"
                                               ,weighted = FALSE
                                               ,two.prop.total = TRUE)
item70.os.all.room.types$Lamp.Category[which(item70.os.all.room.types$Lamp.Category == "Total")] <- "All Categories"

item70.os.samplesize <- proportions_one_group(CustomerLevelData = item70.os.data
                                           ,valueVariable = 'Lamps'
                                           ,groupingVariable = 'Clean.Room'
                                           ,total.name = 'All Categories'
                                           ,columnName = 'Lamp.Category'
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
item70.os.samplesize <- item70.os.samplesize[which(item70.os.samplesize$Clean.Room != "Total"),]

item70.os.final <- rbind.data.frame(item70.os.summary, item70.os.all.room.types, item70.os.samplesize, stringsAsFactors = F)

item70.os.cast <- dcast(setDT(item70.os.final)
                     , formula = BuildingType + Clean.Room ~ Lamp.Category
                     , value.var = c("Percent", "SE", "Count", "n"))


names(item70.os.cast)

item70.os.table <- data.frame("BuildingType"                  = item70.os.cast$BuildingType
                           ,"Room.Type"                    = item70.os.cast$Clean.Room
                           ,"Percent_CFL"                  = item70.os.cast$`Percent_Compact Fluorescent`
                           ,"SE_CFL"                       = item70.os.cast$`SE_Compact Fluorescent`
                           ,"Percent_Halogen"              = item70.os.cast$Percent_Halogen
                           ,"SE_Halogen"                   = item70.os.cast$SE_Halogen
                           ,"Percent_Incandescent"         = item70.os.cast$Percent_Incandescent
                           ,"SE_Incandescent"              = item70.os.cast$SE_Incandescent
                           ,"Percent_Incandescent.Halogen" = item70.os.cast$`Percent_Incandescent / Halogen`
                           ,"SE_Incandescent.Halogen"      = item70.os.cast$`SE_Incandescent / Halogen`
                           ,"Percent_LED"                  = item70.os.cast$`Percent_Light Emitting Diode`
                           ,"SE_LED"                       = item70.os.cast$`SE_Light Emitting Diode`
                           ,"Percent_LF"                   = item70.os.cast$`Percent_Linear Fluorescent`
                           ,"SE_LF"                        = item70.os.cast$`SE_Linear Fluorescent`
                           ,"Percent_Other"                = item70.os.cast$Percent_Other
                           ,"SE_Other"                     = item70.os.cast$SE_Other
                           ,"n"                            = item70.os.cast$`n_All Categories`
)


# row ordering example code
levels(item70.os.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
item70.os.table <- item70.os.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)  
item70.os.table <- data.frame(item70.os.table)


item70.os.final.SF <- item70.os.table[which(item70.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item70.os.table) %in% c("BuildingType"))]

exportTable(item70.os.final.SF, "SF", "Table 77", weighted = FALSE, osIndicator = export.ind, OS = T)










#############################################################################################
# Table AE: DISTRIBUTION OF LAMPS BY TYPE AND ROOM - DIFF BINS
#############################################################################################
tableAE.os.merge <- item70.merge
tableAE.os.merge$Clean.Room[which(tableAE.os.merge$Clean.Room %in% c("Dining Room","Living Room","Family Room"))] <- "Living Room"

################################################
# Adding pop and sample sizes for weights
################################################
tableAE.os.data <- weightedData(tableAE.os.merge[-which(colnames(tableAE.os.merge) %in% c("CK_SiteID"
                                                                                 ,"Fixture.Qty"
                                                                                 ,"LIGHTING_BulbsPerFixture"
                                                                                 ,"Lamp.Category"
                                                                                 ,"count"
                                                                                 ,"Lamps"
                                                                                 ,"Clean.Room"))])
tableAE.os.data <- left_join(tableAE.os.data, tableAE.os.merge[which(colnames(tableAE.os.merge) %in% c("CK_Cadmus_ID"
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
tableAE.os.summary <- proportionRowsAndColumns1(CustomerLevelData = tableAE.os.data
                                             ,valueVariable    = 'Lamps'
                                             ,columnVariable   = 'Clean.Room'
                                             ,rowVariable      = 'Lamp.Category'
                                             ,aggregateColumnName = "All Room Types")
tableAE.os.summary <- tableAE.os.summary[which(tableAE.os.summary$Clean.Room != "All Room Types"),]
# tableAE.os.summary <- tableAE.os.summary[which(colnames(tableAE.os.summary)     != "n")]
# colnames(tableAE.os.summary)[which(colnames(tableAE.os.summary) == "n_hj")] <- "n"

tableAE.os.all.room.types <- proportions_one_group(CustomerLevelData = tableAE.os.data
                                                ,valueVariable = 'Lamps'
                                                ,groupingVariable = "Lamp.Category"
                                                ,total.name = "All Room Types"
                                                ,columnName = "Clean.Room"
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE)
tableAE.os.all.room.types$Lamp.Category[which(tableAE.os.all.room.types$Lamp.Category == "Total")] <- "All Categories"

tableAE.os.samplesize <- proportions_one_group(CustomerLevelData = tableAE.os.data
                                            ,valueVariable = 'Lamps'
                                            ,groupingVariable = 'Clean.Room'
                                            ,total.name = 'All Categories'
                                            ,columnName = 'Lamp.Category'
                                            ,weighted = TRUE
                                            ,two.prop.total = TRUE)
tableAE.os.samplesize <- tableAE.os.samplesize[which(tableAE.os.samplesize$Clean.Room != "Total"),]

tableAE.os.final <- rbind.data.frame(tableAE.os.summary, tableAE.os.all.room.types, tableAE.os.samplesize, stringsAsFactors = F)

tableAE.os.cast <- dcast(setDT(tableAE.os.final)
                      , formula = BuildingType + Clean.Room ~ Lamp.Category
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAE.os.table <- data.frame("BuildingType"                  = tableAE.os.cast$BuildingType
                            ,"Room.Type"                    = tableAE.os.cast$Clean.Room
                            ,"Percent_CFL"                  = tableAE.os.cast$`w.percent_Compact Fluorescent`
                            ,"SE_CFL"                       = tableAE.os.cast$`w.SE_Compact Fluorescent`
                            # ,"n_CFL"                        = tableAE.os.cast$`n_Compact Fluorescent`
                            ,"Percent_Halogen"              = tableAE.os.cast$w.percent_Halogen
                            ,"SE_Halogen"                   = tableAE.os.cast$w.SE_Halogen
                            # ,"n_Halogen"                    = tableAE.os.cast$n_Halogen
                            ,"Percent_Incandescent"         = tableAE.os.cast$w.percent_Incandescent
                            ,"SE_Incandescent"              = tableAE.os.cast$w.SE_Incandescent
                            # ,"n_Incandescent"               = tableAE.os.cast$n_Incandescent
                            ,"Percent_Incandescent.Halogen" = tableAE.os.cast$`w.percent_Incandescent / Halogen`
                            ,"SE_Incandescent.Halogen"      = tableAE.os.cast$`w.SE_Incandescent / Halogen`
                            # ,"n_Incandescent.Halogen"       = tableAE.os.cast$`n_Incandescent / Halogen`
                            ,"Percent_LED"                  = tableAE.os.cast$`w.percent_Light Emitting Diode`
                            ,"SE_LED"                       = tableAE.os.cast$`w.SE_Light Emitting Diode`
                            # ,"n_LED"                        = tableAE.os.cast$`n_Light Emitting Diode`
                            ,"Percent_LF"                   = tableAE.os.cast$`w.percent_Linear Fluorescent`
                            ,"SE_LF"                        = tableAE.os.cast$`w.SE_Linear Fluorescent`
                            # ,"n_LF"                         = tableAE.os.cast$`n_Linear Fluorescent`
                            ,"Percent_Other"                = tableAE.os.cast$w.percent_Other
                            ,"SE_Other"                     = tableAE.os.cast$w.SE_Other
                            # ,"n_Other"                      = tableAE.os.cast$n_Other
                            ,"n"                            = tableAE.os.cast$`n_All Categories`
                            ,"EB_CFL"                       = tableAE.os.cast$`EB_Compact Fluorescent`
                            ,"EB_Halogen"                   = tableAE.os.cast$EB_Halogen
                            ,"EB_Incandescent"              = tableAE.os.cast$EB_Incandescent
                            ,"EB_Incandescent.Halogen"      = tableAE.os.cast$`EB_Incandescent / Halogen`
                            ,"EB_LED"                       = tableAE.os.cast$`EB_Light Emitting Diode`
                            ,"EB_LF"                        = tableAE.os.cast$`EB_Linear Fluorescent`
                            ,"EB_Other"                     = tableAE.os.cast$EB_Other
)

# row ordering example code
levels(tableAE.os.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
tableAE.os.table <- tableAE.os.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)
tableAE.os.table <- data.frame(tableAE.os.table)


tableAE.os.final.SF <- tableAE.os.table[which(tableAE.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAE.os.table) %in% c("BuildingType"))]

exportTable(tableAE.os.final.SF, "SF", "Table AE", weighted = TRUE, osIndicator = export.ind, OS = TRUE)


#######################
# # Unweighted Analysis
# #######################
tableAE.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableAE.os.data
                                                     ,valueVariable    = 'count'
                                                     ,columnVariable   = 'Clean.Room'
                                                     ,rowVariable      = 'Lamp.Category'
                                                     ,aggregateColumnName = "All Room Types")
tableAE.os.summary <- tableAE.os.summary[which(tableAE.os.summary$Clean.Room != "All Room Types"),]


tableAE.os.all.room.types <- proportions_one_group(CustomerLevelData = tableAE.os.data
                                                ,valueVariable = 'Lamps'
                                                ,groupingVariable = "Lamp.Category"
                                                ,total.name = "All Room Types"
                                                ,columnName = "Clean.Room"
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE)
tableAE.os.all.room.types$Lamp.Category[which(tableAE.os.all.room.types$Lamp.Category == "Total")] <- "All Categories"

tableAE.os.samplesize <- proportions_one_group(CustomerLevelData = tableAE.os.data
                                            ,valueVariable = 'Lamps'
                                            ,groupingVariable = 'Clean.Room'
                                            ,total.name = 'All Categories'
                                            ,columnName = 'Lamp.Category'
                                            ,weighted = FALSE
                                            ,two.prop.total = TRUE)
tableAE.os.samplesize <- tableAE.os.samplesize[which(tableAE.os.samplesize$Clean.Room != "Total"),]

tableAE.os.final <- rbind.data.frame(tableAE.os.summary, tableAE.os.all.room.types, tableAE.os.samplesize, stringsAsFactors = F)

tableAE.os.cast <- dcast(setDT(tableAE.os.final)
                      , formula = BuildingType + Clean.Room ~ Lamp.Category
                      , value.var = c("Percent", "SE", "Count", "n"))

tableAE.os.table <- data.frame("BuildingType"                  = tableAE.os.cast$BuildingType
                            ,"Room.Type"                    = tableAE.os.cast$Clean.Room
                            ,"Percent_CFL"                  = tableAE.os.cast$`Percent_Compact Fluorescent`
                            ,"SE_CFL"                       = tableAE.os.cast$`SE_Compact Fluorescent`
                            # ,"n_CFL"                        = tableAE.os.cast$`n_Compact Fluorescent`
                            ,"Percent_Halogen"              = tableAE.os.cast$Percent_Halogen
                            ,"SE_Halogen"                   = tableAE.os.cast$SE_Halogen
                            # ,"n_Halogen"                    = tableAE.os.cast$n_Halogen
                            ,"Percent_Incandescent"         = tableAE.os.cast$Percent_Incandescent
                            ,"SE_Incandescent"              = tableAE.os.cast$SE_Incandescent
                            # ,"n_Incandescent"               = tableAE.os.cast$n_Incandescent
                            ,"Percent_Incandescent.Halogen" = tableAE.os.cast$`Percent_Incandescent / Halogen`
                            ,"SE_Incandescent.Halogen"      = tableAE.os.cast$`SE_Incandescent / Halogen`
                            # ,"n_Incandescent.Halogen"       = tableAE.os.cast$`n_Incandescent / Halogen`
                            ,"Percent_LED"                  = tableAE.os.cast$`Percent_Light Emitting Diode`
                            ,"SE_LED"                       = tableAE.os.cast$`SE_Light Emitting Diode`
                            # ,"n_LED"                        = tableAE.os.cast$`n_Light Emitting Diode`
                            ,"Percent_LF"                   = tableAE.os.cast$`Percent_Linear Fluorescent`
                            ,"SE_LF"                        = tableAE.os.cast$`SE_Linear Fluorescent`
                            # ,"n_LF"                         = tableAE.os.cast$`n_Linear Fluorescent`
                            ,"Percent_Other"                = tableAE.os.cast$Percent_Other
                            ,"SE_Other"                     = tableAE.os.cast$SE_Other
                            # ,"n_Other"                      = tableAE.os.cast$n_Other
                            ,"n"                            = tableAE.os.cast$`n_All Categories`
)


# row ordering example code
levels(tableAE.os.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
tableAE.os.table <- tableAE.os.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)
tableAE.os.table <- data.frame(tableAE.os.table)


tableAE.os.final.SF <- tableAE.os.table[which(tableAE.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAE.os.table) %in% c("BuildingType"))]

exportTable(tableAE.os.final.SF, "SF", "Table AE", weighted = FALSE, osIndicator = export.ind, OS = TRUE)
