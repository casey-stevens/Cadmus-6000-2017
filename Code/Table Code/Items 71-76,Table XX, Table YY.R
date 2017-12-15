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
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))

#connected lighting: Column called "Switch.Type" -> select only "App Control"


#############################################################################################
#Item 71: AVERAGE NUMBER OF CFLS INSTALLED PER HOME BY STATE (SF table 78, MH table 57)
#############################################################################################
#subset to columns needed for analysis
item71.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item71.dat$count <- 1

item71.dat0 <- item71.dat[which(item71.dat$Lamp.Category == "Compact Fluorescent"),]
item71.dat0.1 <- item71.dat0[which(!(item71.dat0$Clean.Room %in% c("Storage"))),]

item71.dat1 <- left_join(item71.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item71.dat2 <- item71.dat1[grep("SITE", item71.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item71.dat2$Fixture.Qty <- as.numeric(as.character(item71.dat2$Fixture.Qty))
item71.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item71.dat2$LIGHTING_BulbsPerFixture))

item71.dat2$Lamps <- item71.dat2$Fixture.Qty * item71.dat2$LIGHTING_BulbsPerFixture
unique(item71.dat2$Lamps)

item71.dat3 <- item71.dat2[which(!(is.na(item71.dat2$Lamps))),]

item71.customer <- summarise(group_by(item71.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item71.merge <- left_join(rbsa.dat, item71.customer)
item71.merge$Lamps[which(is.na(item71.merge$Lamps))] <- 0


################################################
# Adding pop and sample sizes for weights
################################################
item71.data <- weightedData(item71.merge[-which(colnames(item71.merge) %in% c("Lamps"))])
item71.data <- left_join(item71.data, item71.merge[which(colnames(item71.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))])
item71.data$count <- 1

#######################
# Weighted Analysis
#######################
item71.final <- mean_one_group(CustomerLevelData = item71.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

item71.final.SF <- item71.final[which(item71.final$BuildingType == "Single Family")
                                ,-which(colnames(item71.final) %in% c("BuildingType"))]
item71.final.MH <- item71.final[which(item71.final$BuildingType == "Manufactured")
                                ,-which(colnames(item71.final) %in% c("BuildingType"))]

exportTable(item71.final.SF, "SF", "Table 78", weighted = TRUE)
exportTable(item71.final.MH, "MH", "Table 57", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item71.final <- mean_one_group_unweighted(CustomerLevelData = item71.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

item71.final.SF <- item71.final[which(item71.final$BuildingType == "Single Family")
                                ,-which(colnames(item71.final) %in% c("BuildingType"))]
item71.final.MH <- item71.final[which(item71.final$BuildingType == "Manufactured")
                                ,-which(colnames(item71.final) %in% c("BuildingType"))]

exportTable(item71.final.SF, "SF", "Table 78", weighted = FALSE)
exportTable(item71.final.MH, "MH", "Table 57", weighted = FALSE)





#############################################################################################
#Table XX: AVERAGE NUMBER OF LEDS INSTALLED PER HOME BY STATE (SF table 78, MH table 57)
#############################################################################################
#subset to columns needed for analysis
tableXX.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
tableXX.dat$count <- 1

tableXX.dat0 <- tableXX.dat[which(tableXX.dat$Lamp.Category == "Light Emitting Diode"),]
tableXX.dat0.1 <- tableXX.dat0[which(!(tableXX.dat0$Clean.Room %in% c("Storage"))),]

tableXX.dat1 <- left_join(tableXX.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

tableXX.dat2 <- tableXX.dat1[grep("SITE", tableXX.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
tableXX.dat2$Fixture.Qty <- as.numeric(as.character(tableXX.dat2$Fixture.Qty))
tableXX.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(tableXX.dat2$LIGHTING_BulbsPerFixture))

tableXX.dat2$Lamps <- tableXX.dat2$Fixture.Qty * tableXX.dat2$LIGHTING_BulbsPerFixture
unique(tableXX.dat2$Lamps)

tableXX.dat3 <- tableXX.dat2[which(!(is.na(tableXX.dat2$Lamps))),]

tableXX.customer <- summarise(group_by(tableXX.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

tableXX.merge <- left_join(rbsa.dat, tableXX.customer)
tableXX.merge$Lamps[which(is.na(tableXX.merge$Lamps))] <- 0


################################################
# Adding pop and sample sizes for weights
################################################
tableXX.data <- weightedData(tableXX.merge[-which(colnames(tableXX.merge) %in% c("Lamps"))])
tableXX.data <- left_join(tableXX.data, tableXX.merge[which(colnames(tableXX.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))])
tableXX.data$count <- 1

#######################
# Weighted Analysis
#######################
tableXX.final <- mean_one_group(CustomerLevelData = tableXX.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

tableXX.final.SF <- tableXX.final[which(tableXX.final$BuildingType == "Single Family")
                                ,-which(colnames(tableXX.final) %in% c("BuildingType"))]
tableXX.final.MH <- tableXX.final[which(tableXX.final$BuildingType == "Manufactured")
                                ,-which(colnames(tableXX.final) %in% c("BuildingType"))]

exportTable(tableXX.final.SF, "SF", "Table XX", weighted = TRUE)
exportTable(tableXX.final.MH, "MH", "Table XX", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableXX.final <- mean_one_group_unweighted(CustomerLevelData = tableXX.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

tableXX.final.SF <- tableXX.final[which(tableXX.final$BuildingType == "Single Family")
                                ,-which(colnames(tableXX.final) %in% c("BuildingType"))]
tableXX.final.MH <- tableXX.final[which(tableXX.final$BuildingType == "Manufactured")
                                ,-which(colnames(tableXX.final) %in% c("BuildingType"))]

exportTable(tableXX.final.SF, "SF", "Table XX", weighted = FALSE)
exportTable(tableXX.final.MH, "MH", "Table XX", weighted = FALSE)






#############################################################################################
#Item 72: AVERAGE NUMBER OF HALOGEN LAMPS INSTALLED PER HOME BY STATE (SF table 79, MH table 58)
#############################################################################################
#subset to columns needed for analysis
item72.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item72.dat$count <- 1

item72.dat0 <- item72.dat[which(item72.dat$Lamp.Category == "Halogen"),]
item72.dat0.1 <- item72.dat0[which(!(item72.dat0$Clean.Room %in% c("Storage"))),]

item72.dat1 <- left_join(item72.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item72.dat2 <- item72.dat1[-grep("BLDG", item72.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item72.dat2$Fixture.Qty <- as.numeric(as.character(item72.dat2$Fixture.Qty))
item72.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item72.dat2$LIGHTING_BulbsPerFixture))

item72.dat2$Lamps <- item72.dat2$Fixture.Qty * item72.dat2$LIGHTING_BulbsPerFixture
unique(item72.dat2$Lamps)

item72.dat3 <- item72.dat2[which(!(is.na(item72.dat2$Lamps))),]


item72.customer <- summarise(group_by(item72.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item72.merge <- left_join(rbsa.dat, item72.customer)
item72.merge$Lamps[which(is.na(item72.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item72.data <- weightedData(item72.merge[-which(colnames(item72.merge) %in% c("Lamps"))])
item72.data <- left_join(item72.data, item72.merge[which(colnames(item72.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))])
item72.data$count <- 1

#######################
# Weighted Analysis
#######################
item72.final <- mean_one_group(CustomerLevelData = item72.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

item72.final.SF <- item72.final[which(item72.final$BuildingType == "Single Family")
                                ,-which(colnames(item72.final) %in% c("BuildingType"))]
item72.final.MH <- item72.final[which(item72.final$BuildingType == "Manufactured")
                                ,-which(colnames(item72.final) %in% c("BuildingType"))]

exportTable(item72.final.SF, "SF", "Table 79", weighted = TRUE)
exportTable(item72.final.MH, "MH", "Table 58", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item72.final <- mean_one_group_unweighted(CustomerLevelData = item72.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

item72.final.SF <- item72.final[which(item72.final$BuildingType == "Single Family")
                                ,-which(colnames(item72.final) %in% c("BuildingType"))]
item72.final.MH <- item72.final[which(item72.final$BuildingType == "Manufactured")
                                ,-which(colnames(item72.final) %in% c("BuildingType"))]

exportTable(item72.final.SF, "SF", "Table 79", weighted = FALSE)
exportTable(item72.final.MH, "MH", "Table 58", weighted = FALSE)










#############################################################################################
#Item 73: AVERAGE NUMBER OF INCANDESCENT LAMPS INSTALLED PER HOME BY STATE (SF table 80, MH table 59)
#############################################################################################
#subset to columns needed for analysis
item73.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item73.dat$count <- 1

item73.dat0 <- item73.dat[which(item73.dat$Lamp.Category == "Incandescent"),]
item73.dat0.1 <- item73.dat0[which(!(item73.dat0$Clean.Room %in% c("Storage"))),]

item73.dat1 <- left_join(item73.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item73.dat2 <- item73.dat1[-grep("BLDG", item73.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item73.dat2$Fixture.Qty <- as.numeric(as.character(item73.dat2$Fixture.Qty))
item73.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item73.dat2$LIGHTING_BulbsPerFixture))

item73.dat2$Lamps <- item73.dat2$Fixture.Qty * item73.dat2$LIGHTING_BulbsPerFixture
unique(item73.dat2$Lamps)

item73.dat3 <- item73.dat2[which(!(is.na(item73.dat2$Lamps))),]



item73.customer <- summarise(group_by(item73.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item73.merge <- left_join(rbsa.dat, item73.customer)
item73.merge$Lamps[which(is.na(item73.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item73.data <- weightedData(item73.merge[-which(colnames(item73.merge) %in% c("Lamps"))])
item73.data <- left_join(item73.data, item73.merge[which(colnames(item73.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))])
item73.data$count <- 1

#######################
# Weighted Analysis
#######################
item73.final <- mean_one_group(CustomerLevelData = item73.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

item73.final.SF <- item73.final[which(item73.final$BuildingType == "Single Family")
                                ,-which(colnames(item73.final) %in% c("BuildingType"))]
item73.final.MH <- item73.final[which(item73.final$BuildingType == "Manufactured")
                                ,-which(colnames(item73.final) %in% c("BuildingType"))]

exportTable(item73.final.SF, "SF", "Table 80", weighted = TRUE)
exportTable(item73.final.MH, "MH", "Table 59", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item73.final <- mean_one_group_unweighted(CustomerLevelData = item73.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

item73.final.SF <- item73.final[which(item73.final$BuildingType == "Single Family")
                                ,-which(colnames(item73.final) %in% c("BuildingType"))]
item73.final.MH <- item73.final[which(item73.final$BuildingType == "Manufactured")
                                ,-which(colnames(item73.final) %in% c("BuildingType"))]

exportTable(item73.final.SF, "SF", "Table 80", weighted = FALSE)
exportTable(item73.final.MH, "MH", "Table 59", weighted = FALSE)







#############################################################################################
#Item 74: AVERAGE NUMBER OF LINEAR FLUORESCENT LAMPS INSTALLED PER HOME BY STATE (SF table 81, MH table 60)
#############################################################################################
#subset to columns needed for analysis
item74.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item74.dat$count <- 1

item74.dat0 <- item74.dat[which(item74.dat$Lamp.Category == "Linear Fluorescent"),]
item74.dat0.1 <- item74.dat0[which(!(item74.dat0$Clean.Room %in% c("Storage"))),]

item74.dat1 <- left_join(item74.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item74.dat2 <- item74.dat1[-grep("BLDG", item74.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item74.dat2$Fixture.Qty <- as.numeric(as.character(item74.dat2$Fixture.Qty))
item74.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item74.dat2$LIGHTING_BulbsPerFixture))

item74.dat2$Lamps <- item74.dat2$Fixture.Qty * item74.dat2$LIGHTING_BulbsPerFixture
unique(item74.dat2$Lamps)

item74.dat3 <- item74.dat2[which(!(is.na(item74.dat2$Lamps))),]



item74.customer <- summarise(group_by(item74.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item74.merge <- left_join(rbsa.dat, item74.customer)
item74.merge$Lamps[which(is.na(item74.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item74.data <- weightedData(item74.merge[-which(colnames(item74.merge) %in% c("Lamps"))])
item74.data <- left_join(item74.data, item74.merge[which(colnames(item74.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))])
item74.data$count <- 1

#######################
# Weighted Analysis
#######################
item74.final <- mean_one_group(CustomerLevelData = item74.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

item74.final.SF <- item74.final[which(item74.final$BuildingType == "Single Family")
                                ,-which(colnames(item74.final) %in% c("BuildingType"))]
item74.final.MH <- item74.final[which(item74.final$BuildingType == "Manufactured")
                                ,-which(colnames(item74.final) %in% c("BuildingType"))]

exportTable(item74.final.SF, "SF", "Table 81", weighted = TRUE)
exportTable(item74.final.MH, "MH", "Table 60", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item74.final <- mean_one_group_unweighted(CustomerLevelData = item74.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

item74.final.SF <- item74.final[which(item74.final$BuildingType == "Single Family")
                                ,-which(colnames(item74.final) %in% c("BuildingType"))]
item74.final.MH <- item74.final[which(item74.final$BuildingType == "Manufactured")
                                ,-which(colnames(item74.final) %in% c("BuildingType"))]

exportTable(item74.final.SF, "SF", "Table 81", weighted = FALSE)
exportTable(item74.final.MH, "MH", "Table 60", weighted = FALSE)











#############################################################################################
#Item 75: AVERAGE NUMBER OF oTHER LAMPS INSTALLED PER HOME BY STATE (SF table 82, MH table 61)
#############################################################################################
#subset to columns needed for analysis
item75.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item75.dat$count <- 1

item75.dat0 <- item75.dat[which(item75.dat$Lamp.Category == "Other"),]
item75.dat0.1 <- item75.dat0[which(!(item75.dat0$Clean.Room %in% c("Storage"))),]

item75.dat1 <- left_join(item75.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item75.dat2 <- item75.dat1[-grep("BLDG", item75.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item75.dat2$Fixture.Qty <- as.numeric(as.character(item75.dat2$Fixture.Qty))
item75.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item75.dat2$LIGHTING_BulbsPerFixture))

item75.dat2$Lamps <- item75.dat2$Fixture.Qty * item75.dat2$LIGHTING_BulbsPerFixture
unique(item75.dat2$Lamps)

item75.dat3 <- item75.dat2[which(!(is.na(item75.dat2$Lamps))),]



item75.customer <- summarise(group_by(item75.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item75.merge <- left_join(rbsa.dat, item75.customer)
item75.merge$Lamps[which(is.na(item75.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item75.data <- weightedData(item75.merge[-which(colnames(item75.merge) %in% c("Lamps"))])
item75.data <- left_join(item75.data, item75.merge[which(colnames(item75.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))])
item75.data$count <- 1

#######################
# Weighted Analysis
#######################
item75.final <- mean_one_group(CustomerLevelData = item75.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

item75.final.SF <- item75.final[which(item75.final$BuildingType == "Single Family")
                                ,-which(colnames(item75.final) %in% c("BuildingType"))]
item75.final.MH <- item75.final[which(item75.final$BuildingType == "Manufactured")
                                ,-which(colnames(item75.final) %in% c("BuildingType"))]

exportTable(item75.final.SF, "SF", "Table 82", weighted = TRUE)
exportTable(item75.final.MH, "MH", "Table 61", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item75.final <- mean_one_group_unweighted(CustomerLevelData = item75.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

item75.final.SF <- item75.final[which(item75.final$BuildingType == "Single Family")
                                ,-which(colnames(item75.final) %in% c("BuildingType"))]
item75.final.MH <- item75.final[which(item75.final$BuildingType == "Manufactured")
                                ,-which(colnames(item75.final) %in% c("BuildingType"))]

exportTable(item75.final.SF, "SF", "Table 82", weighted = FALSE)
exportTable(item75.final.MH, "MH", "Table 61", weighted = FALSE)








#############################################################################################
#Item 76: AVERAGE NUMBER OF STORED COMPACT FLUORESCENT LAMPS BY STATE (SF table 83, MH table 62)
#############################################################################################
#subset to columns needed for analysis
item76.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item76.dat$count <- 1

item76.dat0 <- item76.dat[which(item76.dat$Clean.Room == "Storage"),]
item76.dat0.1 <- item76.dat0[which(item76.dat$Lamp.Category == "Compact Fluorescent"),]

item76.dat1 <- left_join(item76.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

# item76.dat2 <- item76.dat1[grep("SITE", item76.dat1$CK_SiteID),]
item76.dat2 <- item76.dat1

#clean fixture and bulbs per fixture
item76.dat2$Fixture.Qty <- as.numeric(as.character(item76.dat2$Fixture.Qty))
item76.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item76.dat2$LIGHTING_BulbsPerFixture))

item76.dat2$Lamps <- item76.dat2$Fixture.Qty * item76.dat2$LIGHTING_BulbsPerFixture
unique(item76.dat2$Lamps)

item76.dat3 <- item76.dat2[which(!(is.na(item76.dat2$Lamps))),]


item76.customer <- summarise(group_by(item76.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item76.merge <- left_join(rbsa.dat, item76.customer)
item76.merge$Lamps[which(is.na(item76.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item76.data <- weightedData(item76.merge[-which(colnames(item76.merge) %in% c("Lamps"))])
item76.data <- left_join(item76.data, item76.merge[which(colnames(item76.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))])
item76.data$count <- 1

#######################
# Weighted Analysis
#######################
item76.final <- mean_one_group(CustomerLevelData = item76.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

item76.final.SF <- item76.final[which(item76.final$BuildingType == "Single Family")
                                ,-which(colnames(item76.final) %in% c("BuildingType"))]
item76.final.MH <- item76.final[which(item76.final$BuildingType == "Manufactured")
                                ,-which(colnames(item76.final) %in% c("BuildingType"))]

exportTable(item76.final.SF, "SF", "Table 83", weighted = TRUE)
exportTable(item76.final.MH, "MH", "Table 62", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item76.final <- mean_one_group_unweighted(CustomerLevelData = item76.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

item76.final.SF <- item76.final[which(item76.final$BuildingType == "Single Family")
                                ,-which(colnames(item76.final) %in% c("BuildingType"))]
item76.final.MH <- item76.final[which(item76.final$BuildingType == "Manufactured")
                                ,-which(colnames(item76.final) %in% c("BuildingType"))]

exportTable(item76.final.SF, "SF", "Table 83", weighted = FALSE)
exportTable(item76.final.MH, "MH", "Table 62", weighted = FALSE)







#############################################################################################
#Table YY: Average number of Stored LED lamps by state
#############################################################################################
#subset to columns needed for analysis
tableYY.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
tableYY.dat$count <- 1

tableYY.dat0 <- tableYY.dat[which(tableYY.dat$Clean.Room == "Storage"),]
tableYY.dat0.1 <- tableYY.dat0[which(tableYY.dat$Lamp.Category == "Light Emitting Diode"),]

tableYY.dat1 <- left_join(tableYY.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

# tableYY.dat2 <- tableYY.dat1[grep("SITE", tableYY.dat1$CK_SiteID),]
tableYY.dat2 <- tableYY.dat1

#clean fixture and bulbs per fixture
tableYY.dat2$Fixture.Qty <- as.numeric(as.character(tableYY.dat2$Fixture.Qty))
tableYY.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(tableYY.dat2$LIGHTING_BulbsPerFixture))

tableYY.dat2$Lamps <- tableYY.dat2$Fixture.Qty * tableYY.dat2$LIGHTING_BulbsPerFixture
unique(tableYY.dat2$Lamps)

tableYY.dat3 <- tableYY.dat2[which(!(is.na(tableYY.dat2$Lamps))),]


tableYY.customer <- summarise(group_by(tableYY.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

tableYY.merge <- left_join(rbsa.dat, tableYY.customer)
tableYY.merge$Lamps[which(is.na(tableYY.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
tableYY.data <- weightedData(tableYY.merge[-which(colnames(tableYY.merge) %in% c("Lamps"))])
tableYY.data <- left_join(tableYY.data, tableYY.merge[which(colnames(tableYY.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))])
tableYY.data$count <- 1

#######################
# Weighted Analysis
#######################
tableYY.final <- mean_one_group(CustomerLevelData = tableYY.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

tableYY.final.SF <- tableYY.final[which(tableYY.final$BuildingType == "Single Family")
                                ,-which(colnames(tableYY.final) %in% c("BuildingType"))]
tableYY.final.MH <- tableYY.final[which(tableYY.final$BuildingType == "Manufactured")
                                ,-which(colnames(tableYY.final) %in% c("BuildingType"))]

exportTable(tableYY.final.SF, "SF", "Table YY", weighted = TRUE)
exportTable(tableYY.final.MH, "MH", "Table YY", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableYY.final <- mean_one_group_unweighted(CustomerLevelData = tableYY.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

tableYY.final.SF <- tableYY.final[which(tableYY.final$BuildingType == "Single Family")
                                ,-which(colnames(tableYY.final) %in% c("BuildingType"))]
tableYY.final.MH <- tableYY.final[which(tableYY.final$BuildingType == "Manufactured")
                                ,-which(colnames(tableYY.final) %in% c("BuildingType"))]

exportTable(tableYY.final.SF, "SF", "Table YY", weighted = FALSE)
exportTable(tableYY.final.MH, "MH", "Table YY", weighted = FALSE)

