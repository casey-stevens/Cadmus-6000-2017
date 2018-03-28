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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]

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

item68.dat2 <- item68.dat1[grep("SITE", item68.dat1$CK_Building_ID),]

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
                                                                            ,"Status"
                                                                            ,"Category"))])
item68.data <- left_join(item68.data, item68.dat4[which(colnames(item68.dat4) %in% c("CK_Cadmus_ID"               
                                                                                     ,"Fixture.Qty"
                                                                                     ,"LIGHTING_BulbsPerFixture"
                                                                                     ,"Lamp.Category"
                                                                                     ,"Lamps"
                                                                                     ,"count"
                                                                                     ,"Status"
                                                                                     ,"Category"))])
item68.data$count <- 1
#######################
# Weighted Analysis
#######################
item68.summary <- proportionRowsAndColumns1(CustomerLevelData = item68.data
                                            ,valueVariable    = 'Lamps'
                                            ,columnVariable   = 'Category'
                                            ,rowVariable      = 'Status'
                                            ,aggregateColumnName = "Remove")

item68.cast <- dcast(setDT(item68.summary)
                     , formula = BuildingType + Status ~ Category
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item68.table <- data.frame("BuildingType"    = item68.cast$BuildingType
                           ,"EISA.Category"  = item68.cast$Status
                           ,"PSE.Percent"                 = item68.cast$w.percent_PSE
                           ,"PSE.SE"                      = item68.cast$w.SE_PSE
                           ,"PSE.n"                       = item68.cast$n_PSE
                           ,"PSE.King.County.Percent"     = item68.cast$`w.percent_PSE KING COUNTY`
                           ,"PSE.King.County.SE"          = item68.cast$`w.SE_PSE KING COUNTY`
                           ,"PSE.King.County.n"           = item68.cast$`n_PSE KING COUNTY`
                           ,"PSE.Non.King.County.Percent" = item68.cast$`w.percent_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.SE"      = item68.cast$`w.SE_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.n"       = item68.cast$`n_PSE NON-KING COUNTY`
                           ,"2017.RBSA.PS.Percent"        = item68.cast$`w.percent_2017 RBSA PS`
                           ,"2017.RBSA.PS.SE"             = item68.cast$`w.SE_2017 RBSA PS`
                           ,"2017.RBSA.PS.n"              = item68.cast$`n_2017 RBSA PS`
                           ,"PSE_EB"                      = item68.cast$EB_PSE
                           ,"PSE.King.County_EB"          = item68.cast$`EB_PSE KING COUNTY`
                           ,"PSE.Non.King.County_EB"      = item68.cast$`EB_PSE NON-KING COUNTY`
                           ,"2017.RBSA.PS_EB"             = item68.cast$`EB_2017 RBSA PS`
)

levels(item68.table$EISA.Category)
rowOrder <- c("Exempt"
              ,"Noncompliant"
              ,"Compliant"
              ,"Total")
item68.table <- item68.table %>% mutate(EISA.Category = factor(EISA.Category, levels = rowOrder)) %>% arrange(EISA.Category)
item68.table <- data.frame(item68.table)

item68.final.MF <- item68.table[which(item68.table$BuildingType == "Multifamily")
                                ,-which(colnames(item68.table) %in% c("BuildingType"))]
exportTable(item68.final.MF, "MF", "Table 81", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
item68.summary <- proportions_two_groups_unweighted(CustomerLevelData = item68.data
                                                    ,valueVariable    = 'Lamps'
                                                    ,columnVariable   = 'Category'
                                                    ,rowVariable      = 'Status'
                                                    ,aggregateColumnName = "Region")

item68.cast <- dcast(setDT(item68.summary)
                     , formula = BuildingType + Status ~ Category
                     , value.var = c("Percent", "SE", "Count", "n"))


item68.table <- data.frame("BuildingType"    = item68.cast$BuildingType
                           ,"EISA.Category"  = item68.cast$Status
                           ,"PSE.Percent"                 = item68.cast$Percent_PSE
                           ,"PSE.SE"                      = item68.cast$SE_PSE
                           ,"PSE.n"                       = item68.cast$n_PSE
                           ,"PSE.King.County.Percent"     = item68.cast$`Percent_PSE KING COUNTY`
                           ,"PSE.King.County.SE"          = item68.cast$`SE_PSE KING COUNTY`
                           ,"PSE.King.County.n"           = item68.cast$`n_PSE KING COUNTY`
                           ,"PSE.Non.King.County.Percent" = item68.cast$`Percent_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.SE"      = item68.cast$`SE_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.n"       = item68.cast$`n_PSE NON-KING COUNTY`
                           ,"2017.RBSA.PS.Percent"        = item68.cast$`Percent_2017 RBSA PS`
                           ,"2017.RBSA.PS.SE"             = item68.cast$`SE_2017 RBSA PS`
                           ,"2017.RBSA.PS.n"              = item68.cast$`n_2017 RBSA PS`
)

item68.final.MF <- item68.table[which(item68.table$BuildingType == "Multifamily")
                                   ,-which(colnames(item68.table) %in% c("BuildingType"))]
exportTable(item68.final.MF, "MF", "Table 81", weighted = FALSE,OS = T, osIndicator = "PSE")







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
                                                                              ,"Clean.Room"
                                                                              ,"Category"))])
item69.data <- left_join(item69.data, item69.merge[which(colnames(item69.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"CK_SiteID"               
                                                                                       ,"Fixture.Qty"
                                                                                       ,"LIGHTING_BulbsPerFixture"
                                                                                       ,"Lamp.Category"
                                                                                       ,"count"
                                                                                       ,"Lamps"
                                                                                       ,"Clean.Room"
                                                                                       ,"Category"))])
item69.data$count <- 1
stopifnot(nrow(item69.data) == nrow(item69.merge))
#######################
# Weighted Analysis
#######################
item69.summary <- proportionRowsAndColumns1(CustomerLevelData = item69.data
                                          ,valueVariable    = 'Lamps'
                                          ,columnVariable   = 'Category'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "Region")

item69.cast <- dcast(setDT(item69.summary)
                     , formula = BuildingType + Lamp.Category ~ Category
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item69.table <- data.frame("BuildingType"    = item69.cast$BuildingType
                           ,"Lamp.Type"      = item69.cast$Lamp.Category
                           ,"PSE.Percent"                 = item69.cast$w.percent_PSE
                           ,"PSE.SE"                      = item69.cast$w.SE_PSE
                           ,"PSE.n"                       = item69.cast$n_PSE
                           ,"PSE.King.County.Percent"     = item69.cast$`w.percent_PSE KING COUNTY`
                           ,"PSE.King.County.SE"          = item69.cast$`w.SE_PSE KING COUNTY`
                           ,"PSE.King.County.n"           = item69.cast$`n_PSE KING COUNTY`
                           ,"PSE.Non.King.County.Percent" = item69.cast$`w.percent_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.SE"      = item69.cast$`w.SE_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.n"       = item69.cast$`n_PSE NON-KING COUNTY`
                           ,"2017.RBSA.PS.Percent"        = item69.cast$`w.percent_2017 RBSA PS`
                           ,"2017.RBSA.PS.SE"             = item69.cast$`w.SE_2017 RBSA PS`
                           ,"2017.RBSA.PS.n"              = item69.cast$`n_2017 RBSA PS`
                           ,"PSE_EB"                      = item69.cast$EB_PSE
                           ,"PSE.King.County_EB"          = item69.cast$`EB_PSE KING COUNTY`
                           ,"PSE.Non.King.County_EB"      = item69.cast$`EB_PSE NON-KING COUNTY`
                           ,"2017.RBSA.PS_EB"             = item69.cast$`EB_2017 RBSA PS`
)

item69.final.MF <- item69.table[which(item69.table$BuildingType == "Multifamily")
                                   ,-which(colnames(item69.table) %in% c("BuildingType"))]
exportTable(item69.final.MF, "MF", "Table 83", weighted = TRUE,OS = T, osIndicator = "PSE")



#######################
# Unweighted Analysis
#######################
item69.summary <- proportions_two_groups_unweighted(CustomerLevelData = item69.data
                                          ,valueVariable    = 'Lamps'
                                          ,columnVariable   = 'Category'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "Region")

item69.cast <- dcast(setDT(item69.summary)
                     , formula = BuildingType + Lamp.Category ~ Category
                     , value.var = c("Percent", "SE", "Count", "n"))


item69.table <- data.frame("BuildingType"    = item69.cast$BuildingType
                           ,"Lamp.Type"      = item69.cast$Lamp.Category
                           ,"PSE.Percent"                 = item69.cast$Percent_PSE
                           ,"PSE.SE"                      = item69.cast$SE_PSE
                           ,"PSE.n"                       = item69.cast$n_PSE
                           ,"PSE.King.County.Percent"     = item69.cast$`Percent_PSE KING COUNTY`
                           ,"PSE.King.County.SE"          = item69.cast$`SE_PSE KING COUNTY`
                           ,"PSE.King.County.n"           = item69.cast$`n_PSE KING COUNTY`
                           ,"PSE.Non.King.County.Percent" = item69.cast$`Percent_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.SE"      = item69.cast$`SE_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.n"       = item69.cast$`n_PSE NON-KING COUNTY`
                           ,"2017.RBSA.PS.Percent"        = item69.cast$`Percent_2017 RBSA PS`
                           ,"2017.RBSA.PS.SE"             = item69.cast$`SE_2017 RBSA PS`
                           ,"2017.RBSA.PS.n"              = item69.cast$`n_2017 RBSA PS`
)

item69.final.MF <- item69.table[which(item69.table$BuildingType == "Multifamily")
                                   ,-which(colnames(item69.table) %in% c("BuildingType"))]
exportTable(item69.final.MF, "MF", "Table 83", weighted = FALSE,OS = T, osIndicator = "PSE")






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

item70.merge <- item70.merge[which(item70.merge$Category == "PSE")]
################################################
# Adding pop and sample sizes for weights
################################################
item70.data <- weightedData(item70.merge[-which(colnames(item70.merge) %in% c("CK_SiteID"               
                                                                              ,"Fixture.Qty"
                                                                              ,"LIGHTING_BulbsPerFixture"
                                                                              ,"Lamp.Category"
                                                                              ,"count"
                                                                              ,"Lamps"
                                                                              ,"Clean.Room"
                                                                              ,"Category"))])
item70.data <- left_join(item70.data, item70.merge[which(colnames(item70.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"CK_SiteID"               
                                                                                       ,"Fixture.Qty"
                                                                                       ,"LIGHTING_BulbsPerFixture"
                                                                                       ,"Lamp.Category"
                                                                                       ,"count"
                                                                                       ,"Lamps"
                                                                                       ,"Clean.Room"
                                                                                       ,"Category"))])

stopifnot(nrow(item70.data) == nrow(item70.merge))
#######################
# Weighted Analysis
#######################
item70.summary <- proportionRowsAndColumns1(CustomerLevelData = item70.data
                                          ,valueVariable    = 'Lamps'
                                          ,columnVariable   = 'Clean.Room'
                                          ,rowVariable      = 'Lamp.Category'
                                          ,aggregateColumnName = "All Room Types")
item70.summary <- item70.summary[which(item70.summary$Clean.Room != "All Room Types"),]

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

item70.final.MF <- item70.table[which(item70.table$BuildingType == "Multifamily")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]

exportTable(item70.final.MF, "MF", "Table 84", weighted = TRUE,OS = T, osIndicator = "PSE")


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

item70.final.MF <- item70.table[which(item70.table$BuildingType == "Multifamily")
                                ,-which(colnames(item70.table) %in% c("BuildingType"))]

exportTable(item70.final.MF, "MF", "Table 84", weighted = FALSE,OS = T, osIndicator = "PSE")








# #############################################################################################
# # Table AE: DISTRIBUTION OF LAMPS BY TYPE AND ROOM - DIFF BINS
# #############################################################################################
# tableAE.merge <- item70.merge
# tableAE.merge$Clean.Room[which(tableAE.merge$Clean.Room %in% c("Dining Room","Living Room","Family Room"))] <- "Living Room"
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableAE.data <- weightedData(tableAE.merge[-which(colnames(tableAE.merge) %in% c("CK_SiteID"
#                                                                               ,"Fixture.Qty"
#                                                                               ,"LIGHTING_BulbsPerFixture"
#                                                                               ,"Lamp.Category"
#                                                                               ,"count"
#                                                                               ,"Lamps"
#                                                                               ,"Clean.Room"
#                                                                               ,"Category"))])
# tableAE.data <- left_join(tableAE.data, tableAE.merge[which(colnames(tableAE.merge) %in% c("CK_Cadmus_ID"
#                                                                                        ,"CK_SiteID"
#                                                                                        ,"Fixture.Qty"
#                                                                                        ,"LIGHTING_BulbsPerFixture"
#                                                                                        ,"Lamp.Category"
#                                                                                        ,"count"
#                                                                                        ,"Lamps"
#                                                                                        ,"Clean.Room"
#                                                                                        ,"Category"))])
# 
# 
# #######################
# # Weighted Analysis
# #######################
# tableAE.summary <- proportionRowsAndColumns1(CustomerLevelData = tableAE.data
#                                             ,valueVariable    = 'Lamps'
#                                             ,columnVariable   = 'Clean.Room'
#                                             ,rowVariable      = 'Lamp.Category'
#                                             ,aggregateColumnName = "All Room Types")
# tableAE.summary <- tableAE.summary[which(tableAE.summary$Clean.Room != "All Room Types"),]
# 
# tableAE.all.room.types <- proportions_one_group(CustomerLevelData = tableAE.data
#                                                ,valueVariable = 'Lamps'
#                                                ,groupingVariable = "Lamp.Category"
#                                                ,total.name = "All Room Types"
#                                                ,columnName = "Clean.Room"
#                                                ,weighted = TRUE
#                                                ,two.prop.total = TRUE)
# tableAE.all.room.types$Lamp.Category[which(tableAE.all.room.types$Lamp.Category == "Total")] <- "All Categories"
# 
# tableAE.samplesize <- proportions_one_group(CustomerLevelData = tableAE.data
#                                            ,valueVariable = 'Lamps'
#                                            ,groupingVariable = 'Clean.Room'
#                                            ,total.name = 'All Categories'
#                                            ,columnName = 'Lamp.Category'
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE)
# tableAE.samplesize <- tableAE.samplesize[which(tableAE.samplesize$Clean.Room != "Total"),]
# 
# tableAE.final <- rbind.data.frame(tableAE.summary, tableAE.all.room.types, tableAE.samplesize, stringsAsFactors = F)
# 
# tableAE.cast <- dcast(setDT(tableAE.final)
#                      , formula = BuildingType + Clean.Room ~ Lamp.Category
#                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# tableAE.table <- data.frame("BuildingType"                  = tableAE.cast$BuildingType
#                            ,"Room.Type"                    = tableAE.cast$Clean.Room
#                            ,"Percent_CFL"                  = tableAE.cast$`w.percent_Compact Fluorescent`
#                            ,"SE_CFL"                       = tableAE.cast$`w.SE_Compact Fluorescent`
#                            ,"Percent_Halogen"              = tableAE.cast$w.percent_Halogen
#                            ,"SE_Halogen"                   = tableAE.cast$w.SE_Halogen
#                            ,"Percent_Incandescent"         = tableAE.cast$w.percent_Incandescent
#                            ,"SE_Incandescent"              = tableAE.cast$w.SE_Incandescent
#                            ,"Percent_Incandescent.Halogen" = tableAE.cast$`w.percent_Incandescent / Halogen`
#                            ,"SE_Incandescent.Halogen"      = tableAE.cast$`w.SE_Incandescent / Halogen`
#                            ,"Percent_LED"                  = tableAE.cast$`w.percent_Light Emitting Diode`
#                            ,"SE_LED"                       = tableAE.cast$`w.SE_Light Emitting Diode`
#                            ,"Percent_LF"                   = tableAE.cast$`w.percent_Linear Fluorescent`
#                            ,"SE_LF"                        = tableAE.cast$`w.SE_Linear Fluorescent`
#                            ,"Percent_Other"                = tableAE.cast$w.percent_Other
#                            ,"SE_Other"                     = tableAE.cast$w.SE_Other
#                            ,"n"                            = tableAE.cast$`n_All Categories`
#                            ,"EB_CFL"                       = tableAE.cast$`EB_Compact Fluorescent`
#                            ,"EB_Halogen"                   = tableAE.cast$EB_Halogen
#                            ,"EB_Incandescent"              = tableAE.cast$EB_Incandescent
#                            ,"EB_Incandescent.Halogen"      = tableAE.cast$`EB_Incandescent / Halogen`
#                            ,"EB_LED"                       = tableAE.cast$`EB_Light Emitting Diode`
#                            ,"EB_LF"                        = tableAE.cast$`EB_Linear Fluorescent`
#                            ,"EB_Other"                     = tableAE.cast$EB_Other
# )
# 
# # row ordering example code
# levels(tableAE.table$Room.Type)
# rowOrder <- c("Bathroom"
#               ,"Bedroom"
#               ,"Closet"
#               ,"Dining Room"
#               ,"Family Room"
#               ,"Garage"
#               ,"Hall"
#               ,"Kitchen"
#               ,"Laundry"
#               ,"Living Room"
#               ,"Office"
#               ,"Other"
#               ,"Outside"
#               ,"All Room Types")
# tableAE.table <- tableAE.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)
# tableAE.table <- data.frame(tableAE.table)
# 
# tableAE.final.MF <- tableAE.table[which(tableAE.table$BuildingType == "Multifamily")
#                                 ,-which(colnames(tableAE.table) %in% c("BuildingType"))]
# 
# exportTable(tableAE.final.MF, "MF", "Table AE", weighted = TRUE,OS = T, osIndicator = "PSE")
# 
# 
# #######################
# # # Unweighted Analysis
# # #######################
# tableAE.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableAE.data
#                                                     ,valueVariable    = 'count'
#                                                     ,columnVariable   = 'Clean.Room'
#                                                     ,rowVariable      = 'Lamp.Category'
#                                                     ,aggregateColumnName = "All Room Types")
# tableAE.summary <- tableAE.summary[which(tableAE.summary$Clean.Room != "All Room Types"),]
# 
# 
# tableAE.all.room.types <- proportions_one_group(CustomerLevelData = tableAE.data
#                                                ,valueVariable = 'Lamps'
#                                                ,groupingVariable = "Lamp.Category"
#                                                ,total.name = "All Room Types"
#                                                ,columnName = "Clean.Room"
#                                                ,weighted = FALSE
#                                                ,two.prop.total = TRUE)
# tableAE.all.room.types$Lamp.Category[which(tableAE.all.room.types$Lamp.Category == "Total")] <- "All Categories"
# 
# tableAE.samplesize <- proportions_one_group(CustomerLevelData = tableAE.data
#                                            ,valueVariable = 'Lamps'
#                                            ,groupingVariable = 'Clean.Room'
#                                            ,total.name = 'All Categories'
#                                            ,columnName = 'Lamp.Category'
#                                            ,weighted = FALSE
#                                            ,two.prop.total = TRUE)
# tableAE.samplesize <- tableAE.samplesize[which(tableAE.samplesize$Clean.Room != "Total"),]
# 
# tableAE.final <- rbind.data.frame(tableAE.summary, tableAE.all.room.types, tableAE.samplesize, stringsAsFactors = F)
# 
# tableAE.cast <- dcast(setDT(tableAE.final)
#                      , formula = BuildingType + Clean.Room ~ Lamp.Category
#                      , value.var = c("Percent", "SE", "Count", "n"))
# 
# tableAE.table <- data.frame("BuildingType"                  = tableAE.cast$BuildingType
#                            ,"Room.Type"                    = tableAE.cast$Clean.Room
#                            ,"Percent_CFL"                  = tableAE.cast$`Percent_Compact Fluorescent`
#                            ,"SE_CFL"                       = tableAE.cast$`SE_Compact Fluorescent`
#                            ,"Percent_Halogen"              = tableAE.cast$Percent_Halogen
#                            ,"SE_Halogen"                   = tableAE.cast$SE_Halogen
#                            ,"Percent_Incandescent"         = tableAE.cast$Percent_Incandescent
#                            ,"SE_Incandescent"              = tableAE.cast$SE_Incandescent
#                            ,"Percent_Incandescent.Halogen" = tableAE.cast$`Percent_Incandescent / Halogen`
#                            ,"SE_Incandescent.Halogen"      = tableAE.cast$`SE_Incandescent / Halogen`
#                            ,"Percent_LED"                  = tableAE.cast$`Percent_Light Emitting Diode`
#                            ,"SE_LED"                       = tableAE.cast$`SE_Light Emitting Diode`
#                            ,"Percent_LF"                   = tableAE.cast$`Percent_Linear Fluorescent`
#                            ,"SE_LF"                        = tableAE.cast$`SE_Linear Fluorescent`
#                            ,"Percent_Other"                = tableAE.cast$Percent_Other
#                            ,"SE_Other"                     = tableAE.cast$SE_Other
#                            ,"n"                            = tableAE.cast$`n_All Categories`
# )
# 
# 
# # row ordering example code
# levels(tableAE.table$Room.Type)
# rowOrder <- c("Bathroom"
#               ,"Bedroom"
#               ,"Closet"
#               ,"Dining Room"
#               ,"Family Room"
#               ,"Garage"
#               ,"Hall"
#               ,"Kitchen"
#               ,"Laundry"
#               ,"Living Room"
#               ,"Office"
#               ,"Other"
#               ,"Outside"
#               ,"All Room Types")
# tableAE.table <- tableAE.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)
# tableAE.table <- data.frame(tableAE.table)
# 
# tableAE.final.MF <- tableAE.table[which(tableAE.table$BuildingType == "Multifamily")
#                                 ,-which(colnames(tableAE.table) %in% c("BuildingType"))]
# 
# exportTable(tableAE.final.MF, "MF", "Table AE", weighted = FALSE,OS = T, osIndicator = "PSE")

