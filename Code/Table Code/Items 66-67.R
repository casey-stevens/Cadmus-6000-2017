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
rbsa.dat <- rbsa.dat[-grep("bldg", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))


#############################################################################################
#Item 66: AVERAGE NUMBER OF LAMPS PER HOME BY STATE (SF table 73, MH table 52)
#############################################################################################
#subset to columns needed for analysis
item66.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Clean.Room"))]
item66.dat$count <- 1

item66.dat1 <- left_join(rbsa.dat, item66.dat, by = "CK_Cadmus_ID")

item66.dat2 <- item66.dat1[grep("SITE", item66.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item66.dat2$Fixture.Qty <- as.numeric(as.character(item66.dat2$Fixture.Qty))
item66.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item66.dat2$LIGHTING_BulbsPerFixture))

item66.dat2$Lamps <- item66.dat2$Fixture.Qty * item66.dat2$LIGHTING_BulbsPerFixture
unique(item66.dat2$Lamps)

item66.dat3 <- item66.dat2[which(!(is.na(item66.dat2$Lamps))),]
item66.dat4 <- item66.dat3[which(item66.dat3$Clean.Room != "Storage"),]

item66.customer <- summarise(group_by(item66.dat4, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item66.merge <- left_join(rbsa.dat, item66.customer)
unique(item66.merge$Lamps)


item66.merge <- item66.merge[which(!is.na(item66.merge$Lamps)),]


################################################
# Adding pop and sample sizes for weights
################################################
item66.data <- weightedData(item66.merge[-which(colnames(item66.merge) %in% c("Lamps"))])
item66.data <- left_join(item66.data, item66.merge[which(colnames(item66.merge) %in% c("CK_Cadmus_ID"
                                                                                   ,"Lamps"))])
item66.data$count <- 1

#######################
# Weighted Analysis
#######################
item66.final <- mean_one_group(CustomerLevelData = item66.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

item66.final.SF <- item66.final[which(item66.final$BuildingType == "Single Family")
                                ,-which(colnames(item66.final) %in% c("BuildingType"))]
item66.final.MH <- item66.final[which(item66.final$BuildingType == "Manufactured")
                                ,-which(colnames(item66.final) %in% c("BuildingType"))]

# exportTable(item66.final.SF, "SF", "Table 73", weighted = TRUE)
exportTable(item66.final.MH, "MH", "Table 52", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item66.final <- mean_one_group_unweighted(CustomerLevelData = item66.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

item66.final.SF <- item66.final[which(item66.final$BuildingType == "Single Family")
                                ,-which(colnames(item66.final) %in% c("BuildingType"))]
item66.final.MH <- item66.final[which(item66.final$BuildingType == "Manufactured")
                                ,-which(colnames(item66.final) %in% c("BuildingType"))]

# exportTable(item66.final.SF, "SF", "Table 73", weighted = FALSE)
exportTable(item66.final.MH, "MH", "Table 52", weighted = FALSE)





#############################################################################################
#Item 67: AVERAGE NUMBER OF FIXTURES PER HOME BY STATE (SF table 74, MH table 53)
#############################################################################################
#subset to columns needed for analysis
item67.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"CK_LightingDetail_ID"
                                                               ,"Clean.Room"))]
item67.dat$count <- 1

item67.dat1 <- left_join(rbsa.dat, item67.dat, by = "CK_Cadmus_ID")

item67.dat2 <- item67.dat1[grep("SITE", item67.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item67.dat2$Fixture.Qty <- as.numeric(as.character(item67.dat2$Fixture.Qty))
item67.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item67.dat2$LIGHTING_BulbsPerFixture))

item67.dat2$Lamps <- item67.dat2$Fixture.Qty * item67.dat2$LIGHTING_BulbsPerFixture
unique(item67.dat2$Lamps)

item67.dat3 <- item67.dat2[which(!(is.na(item67.dat2$Fixture.Qty))),]
item67.dat4 <- item67.dat3[which(item67.dat3$Clean.Room != "Storage"),]

##total fixtures per home
item67.fixtures <- summarise(group_by(item67.dat4, CK_Cadmus_ID)
                          ,Fixtures = sum(Fixture.Qty))


item67.merge <- left_join(rbsa.dat, item67.fixtures)
unique(item67.merge$Fixtures)


item67.merge <- item67.merge[which(!is.na(item67.merge$Fixtures)),]


################################################
# Adding pop and sample sizes for weights
################################################
item67.data <- weightedData(item67.merge[-which(colnames(item67.merge) %in% c("Fixtures"))])
item67.data <- left_join(item67.data, item67.merge[which(colnames(item67.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Fixtures"))])
item67.data$count <- 1

#######################
# Weighted Analysis
#######################
item67.final <- mean_one_group(CustomerLevelData = item67.data
                               ,valueVariable    = 'Fixtures'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

item67.final.SF <- item67.final[which(item67.final$BuildingType == "Single Family")
                                ,-which(colnames(item67.final) %in% c("BuildingType"))]
item67.final.MH <- item67.final[which(item67.final$BuildingType == "Manufactured")
                                ,-which(colnames(item67.final) %in% c("BuildingType"))]

# exportTable(item67.final.SF, "SF", "Table 74", weighted = TRUE)
exportTable(item67.final.MH, "MH", "Table 53", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item67.final <- mean_one_group_unweighted(CustomerLevelData = item67.data
                                          ,valueVariable    = 'Fixtures'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

item67.final.SF <- item67.final[which(item67.final$BuildingType == "Single Family")
                                ,-which(colnames(item67.final) %in% c("BuildingType"))]
item67.final.MH <- item67.final[which(item67.final$BuildingType == "Manufactured")
                                ,-which(colnames(item67.final) %in% c("BuildingType"))]

# exportTable(item67.final.SF, "SF", "Table 74", weighted = FALSE)
exportTable(item67.final.MH, "MH", "Table 53", weighted = FALSE)






















############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#############################################################################################
#Item 66: AVERAGE NUMBER OF LAMPS PER HOME BY CK_Building_ID (SF table 73, MH table 52)
#############################################################################################
#subset to columns needed for analysis
item66.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Clean.Room"))]
item66.os.dat$count <- 1

item66.os.dat2 <- left_join(scl.dat, item66.os.dat, by = "CK_Cadmus_ID")

#clean fixture and bulbs per fixture
item66.os.dat2$Fixture.Qty <- as.numeric(as.character(item66.os.dat2$Fixture.Qty))
item66.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item66.os.dat2$LIGHTING_BulbsPerFixture))

item66.os.dat2$Lamps <- item66.os.dat2$Fixture.Qty * item66.os.dat2$LIGHTING_BulbsPerFixture
unique(item66.os.dat2$Lamps)

item66.os.dat3 <- item66.os.dat2[which(!(is.na(item66.os.dat2$Lamps))),]
item66.os.dat4 <- item66.os.dat3[which(item66.os.dat3$Clean.Room != "Storage"),]

item66.os.customer <- summarise(group_by(item66.os.dat4, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item66.os.merge <- left_join(scl.dat, item66.os.customer)
unique(item66.os.merge$Lamps)


item66.os.merge <- item66.os.merge[which(!is.na(item66.os.merge$Lamps)),]


################################################
# Adding pop and sample sizes for weights
################################################
item66.os.data <- weightedData(item66.os.merge[-which(colnames(item66.os.merge) %in% c("Lamps"))])
item66.os.data <- left_join(item66.os.data, unique(item66.os.merge[which(colnames(item66.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"Lamps"))]))
item66.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item66.os.final <- mean_one_group(CustomerLevelData = item66.os.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'CK_Building_ID'
                               ,aggregateRow     = 'Remove')
item66.os.final <- item66.os.final[which(item66.os.final$CK_Building_ID != "Remove"),]

item66.os.final.SF <- item66.os.final[which(item66.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item66.os.final) %in% c("BuildingType"))]

exportTable(item66.os.final.SF, "SF", "Table 73", weighted = TRUE, osIndicator = "SCL", OS = T)


#######################
# Unweighted Analysis
#######################
item66.os.final <- mean_one_group_unweighted(CustomerLevelData = item66.os.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'CK_Building_ID'
                                          ,aggregateRow     = 'Remove')
item66.os.final <- item66.os.final[which(item66.os.final$CK_Building_ID != "Remove"),]

item66.os.final.SF <- item66.os.final[which(item66.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item66.os.final) %in% c("BuildingType"))]

exportTable(item66.os.final.SF, "SF", "Table 73", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 67: AVERAGE NUMBER OF FIXTURES PER HOME BY CK_Building_ID (SF table 74, MH table 53)
#############################################################################################
#subset to columns needed for analysis
item67.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"CK_LightingDetail_ID"
                                                               ,"Clean.Room"))]
item67.os.dat$count <- 1

item67.os.dat2 <- left_join(scl.dat, item67.os.dat, by = "CK_Cadmus_ID")

#clean fixture and bulbs per fixture
item67.os.dat2$Fixture.Qty <- as.numeric(as.character(item67.os.dat2$Fixture.Qty))
item67.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item67.os.dat2$LIGHTING_BulbsPerFixture))

item67.os.dat2$Lamps <- item67.os.dat2$Fixture.Qty * item67.os.dat2$LIGHTING_BulbsPerFixture
unique(item67.os.dat2$Lamps)

item67.os.dat3 <- item67.os.dat2[which(!(is.na(item67.os.dat2$Fixture.Qty))),]
item67.os.dat4 <- item67.os.dat3[which(item67.os.dat3$Clean.Room != "Storage"),]

##total fixtures per home
item67.os.fixtures <- summarise(group_by(item67.os.dat4, CK_Cadmus_ID)
                             ,Fixtures = sum(Fixture.Qty))


item67.os.merge <- left_join(scl.dat, item67.os.fixtures)
unique(item67.os.merge$Fixtures)


item67.os.merge <- item67.os.merge[which(!is.na(item67.os.merge$Fixtures)),]


################################################
# Adding pop and sample sizes for weights
################################################
item67.os.data <- weightedData(item67.os.merge[-which(colnames(item67.os.merge) %in% c("Fixtures"))])
item67.os.data <- left_join(item67.os.data, unique(item67.os.merge[which(colnames(item67.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                          ,"Fixtures"))]))
item67.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item67.os.final <- mean_one_group(CustomerLevelData = item67.os.data
                               ,valueVariable    = 'Fixtures'
                               ,byVariable       = 'CK_Building_ID'
                               ,aggregateRow     = 'Remove')
item67.os.final <- item67.os.final[which(item67.os.final$CK_Building_ID != "Remove"),]

item67.os.final.SF <- item67.os.final[which(item67.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item67.os.final) %in% c("BuildingType"))]

exportTable(item67.os.final.SF, "SF", "Table 74", weighted = TRUE, osIndicator = "SCL", OS = T)


#######################
# Unweighted Analysis
#######################
item67.os.final <- mean_one_group_unweighted(CustomerLevelData = item67.os.data
                                          ,valueVariable    = 'Fixtures'
                                          ,byVariable       = 'CK_Building_ID'
                                          ,aggregateRow     = 'Remove')
item67.os.final <- item67.os.final[which(item67.os.final$CK_Building_ID != "Remove"),]

item67.os.final.SF <- item67.os.final[which(item67.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item67.os.final) %in% c("BuildingType"))]

exportTable(item67.os.final.SF, "SF", "Table 74", weighted = FALSE, osIndicator = "SCL", OS = T)
