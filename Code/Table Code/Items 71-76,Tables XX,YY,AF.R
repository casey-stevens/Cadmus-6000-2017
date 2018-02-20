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

# exportTable(item71.final.SF, "SF", "Table 78", weighted = TRUE)
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

# exportTable(item71.final.SF, "SF", "Table 78", weighted = FALSE)
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

# exportTable(tableXX.final.SF, "SF", "Table XX", weighted = TRUE)
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

# exportTable(tableXX.final.SF, "SF", "Table XX", weighted = FALSE)
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

# exportTable(item72.final.SF, "SF", "Table 79", weighted = TRUE)
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

# exportTable(item72.final.SF, "SF", "Table 79", weighted = FALSE)
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

# exportTable(item73.final.SF, "SF", "Table 80", weighted = TRUE)
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

# exportTable(item73.final.SF, "SF", "Table 80", weighted = FALSE)
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

# exportTable(item74.final.SF, "SF", "Table 81", weighted = TRUE)
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

# exportTable(item74.final.SF, "SF", "Table 81", weighted = FALSE)
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

# exportTable(item75.final.SF, "SF", "Table 82", weighted = TRUE)
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

# exportTable(item75.final.SF, "SF", "Table 82", weighted = FALSE)
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

# exportTable(item76.final.SF, "SF", "Table 83", weighted = TRUE)
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

# exportTable(item76.final.SF, "SF", "Table 83", weighted = FALSE)
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

# exportTable(tableYY.final.SF, "SF", "Table YY", weighted = TRUE)
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

# exportTable(tableYY.final.SF, "SF", "Table YY", weighted = FALSE)
exportTable(tableYY.final.MH, "MH", "Table YY", weighted = FALSE)












#############################################################################################
#Table AF: Average number of Stored lamps by lamp type and state
#############################################################################################
#subset to columns needed for analysis
tableAF.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"CK_SiteID"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]
tableAF.dat$count <- 1

tableAF.dat0 <- left_join(rbsa.dat, tableAF.dat, by = "CK_Cadmus_ID")

tableAF.dat1 <- tableAF.dat0[which(!is.na(tableAF.dat0$Lamp.Category)),]

tableAF.dat2 <- tableAF.dat1[grep("SITE", tableAF.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
tableAF.dat2$Fixture.Qty <- as.numeric(as.character(tableAF.dat2$Fixture.Qty))
tableAF.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(tableAF.dat2$LIGHTING_BulbsPerFixture))

tableAF.dat2$Lamps <- tableAF.dat2$Fixture.Qty * tableAF.dat2$LIGHTING_BulbsPerFixture
unique(tableAF.dat2$Lamps)

tableAF.dat3 <- tableAF.dat2[which(!(is.na(tableAF.dat2$Lamps))),]

tableAF.led.sum <- summarise(group_by(tableAF.dat3, CK_Cadmus_ID, Lamp.Category)
                             ,TotalBulbs = sum(Lamps))

## subset to only storage bulbs
tableAF.storage <- tableAF.dat3[which(tableAF.dat3$Clean.Room == "Storage"),]
#summarise within site
tableAF.storage.sum <- summarise(group_by(tableAF.storage, CK_Cadmus_ID, Lamp.Category)
                                 ,StorageBulbs = sum(Lamps))
length(unique(tableAF.storage.sum$CK_Cadmus_ID))


tableAF.merge1 <- left_join(tableAF.led.sum, tableAF.storage.sum)

tableAF.cast <- dcast(setDT(tableAF.merge1)
                     ,formula = CK_Cadmus_ID ~ Lamp.Category
                     ,value.var = c("StorageBulbs"))
tableAF.cast[is.na(tableAF.cast),] <- 0

tableAF.melt <- melt(tableAF.cast, id.vars = "CK_Cadmus_ID")
names(tableAF.melt) <- c("CK_Cadmus_ID", "Lamp.Category", "StorageBulbs")


tableAF.merge  <- left_join(rbsa.dat, tableAF.melt)
tableAF.merge$StorageBulbs[which(is.na(tableAF.merge$StorageBulbs))] <- 0


################################################
# Adding pop and sample sizes for weights
################################################
tableAF.data <- weightedData(tableAF.merge[-which(colnames(tableAF.merge) %in% c("StorageBulbs"
                                                                                 ,"Lamp.Category"
                                                                                 # ,"TotalBulbs"
                                                                                 ))])
tableAF.data <- left_join(tableAF.data, tableAF.merge[which(colnames(tableAF.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"StorageBulbs"
                                                                                           ,"Lamp.Category"
                                                                                           # ,"TotalBulbs"
                                                                                           ))])
tableAF.data$count <- 1

#######################
# Weighted Analysis
#######################
tableAF.summary <- mean_two_groups(CustomerLevelData = tableAF.data
                                 ,valueVariable = "StorageBulbs"
                                 ,byVariableRow = "Lamp.Category"
                                 ,byVariableColumn = "State"
                                 ,columnAggregate = "Region"
                                 ,rowAggregate = "All Categories")
tableAF.summary <- tableAF.summary[which(tableAF.summary$Lamp.Category != "All Categories"),]


# tableAF.total <- mean_one_group(CustomerLevelData = tableAF.data
#                                   ,valueVariable = "StorageBulbs"
#                                   ,byVariable = "State"
#                                   ,aggregateRow = "Region")

tableAF.cast <- data.frame(tableAF.summary, stringsAsFactors = F)

tableAF.table <- data.frame("BuildingType"    = tableAF.cast$BuildingType
                            ,"Lamp.Category"  = tableAF.cast$Lamp.Category
                            ,"ID"             = tableAF.cast$Mean_ID
                            ,"ID.SE"          = tableAF.cast$SE_ID
                            ,"ID.n"           = tableAF.cast$n_ID
                            ,"MT"             = tableAF.cast$Mean_MT
                            ,"MT.SE"          = tableAF.cast$SE_MT
                            ,"MT.n"           = tableAF.cast$n_MT
                            ,"OR"             = tableAF.cast$Mean_OR
                            ,"OR.SE"          = tableAF.cast$SE_OR
                            ,"OR.n"           = tableAF.cast$n_OR
                            ,"WA"             = tableAF.cast$Mean_WA
                            ,"WA.SE"          = tableAF.cast$SE_WA
                            ,"WA.n"           = tableAF.cast$n_WA
                            ,"Region"         = tableAF.cast$Mean_Region
                            ,"Region.SE"      = tableAF.cast$SE_Region
                            ,"Region.n"       = tableAF.cast$n_Region
                            ,"ID.EB"          = tableAF.cast$EB_ID
                            ,"MT.EB"          = tableAF.cast$EB_MT
                            ,"OR.EB"          = tableAF.cast$EB_OR
                            ,"WA.EB"          = tableAF.cast$EB_WA
                            ,"Region.EB"      = tableAF.cast$EB_Region
)

levels(tableAF.table$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"Unknown"
              ,"All Categories")
tableAF.table <- tableAF.table %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAF.table <- data.frame(tableAF.table)

tableAF.final.SF <- tableAF.table[which(tableAF.table$BuildingType == "Single Family")
                                  ,which(colnames(tableAF.table) %notin% c("BuildingType"))]
# tableAF.final.SF <- tableAF.final.SF[which(tableAF.final.SF$Lamp.Category != "All Categories"),]
tableAF.final.MH <- tableAF.table[which(tableAF.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAF.table) %in% c("BuildingType"))]
# tableAF.final.MH <- tableAF.final.MH[which(tableAF.final.MH$Lamp.Category != "All Categories"),]

# exportTable(tableAF.final.SF, "SF", "Table AF", weighted = TRUE)
exportTable(tableAF.final.MH, "MH", "Table AF", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAF.final <- mean_two_groups_unweighted(CustomerLevelData = tableAF.data
                                 ,valueVariable = "StorageBulbs"
                                 ,byVariableRow = "Lamp.Category"
                                 ,byVariableColumn = "State"
                                 ,columnAggregate = "Region"
                                 ,rowAggregate = "All Categories")
tableAF.cast <- data.frame(tableAF.final, stringsAsFactors = F)

tableAF.table <- data.frame("BuildingType"    = tableAF.cast$BuildingType
                            ,"Lamp.Category"  = tableAF.cast$Lamp.Category
                            ,"ID"             = tableAF.cast$Mean_ID
                            ,"ID.SE"          = tableAF.cast$SE_ID
                            ,"ID.n"           = tableAF.cast$n_ID
                            ,"MT"             = tableAF.cast$Mean_MT
                            ,"MT.SE"          = tableAF.cast$SE_MT
                            ,"MT.n"           = tableAF.cast$n_MT
                            ,"OR"             = tableAF.cast$Mean_OR
                            ,"OR.SE"          = tableAF.cast$SE_OR
                            ,"OR.n"           = tableAF.cast$n_OR
                            ,"WA"             = tableAF.cast$Mean_WA
                            ,"WA.SE"          = tableAF.cast$SE_WA
                            ,"WA.n"           = tableAF.cast$n_WA
                            ,"Region"         = tableAF.cast$Mean_Region
                            ,"Region.SE"      = tableAF.cast$SE_Region
                            ,"Region.n"       = tableAF.cast$n_Region
)

levels(tableAF.table$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"Unknown"
              ,"All Categories")
tableAF.table <- tableAF.table %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAF.table <- data.frame(tableAF.table)

tableAF.final.SF <- tableAF.table[which(tableAF.table$BuildingType == "Single Family")
                                  ,which(colnames(tableAF.table) %notin% c("BuildingType"))]
# tableAF.final.SF <- tableAF.final.SF[which(tableAF.final.SF$Lamp.Category != "All Categories"),]
tableAF.final.MH <- tableAF.table[which(tableAF.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAF.table) %in% c("BuildingType"))]
# tableAF.final.MH <- tableAF.final.MH[which(tableAF.final.MH$Lamp.Category != "All Categories"),]

# exportTable(tableAF.final.SF, "SF", "Table AF", weighted = FALSE)
exportTable(tableAF.final.MH, "MH", "Table AF", weighted = FALSE)





















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
#Item 71: AVERAGE NUMBER OF CFLS INSTALLED PER HOME BY CK_Building_ID (SF table 78, MH table 57)
#############################################################################################
#subset to columns needed for analysis
item71.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item71.os.dat$count <- 1

item71.os.dat0 <- item71.os.dat[which(item71.os.dat$Lamp.Category == "Compact Fluorescent"),]
item71.os.dat0.1 <- item71.os.dat0[which(!(item71.os.dat0$Clean.Room %in% c("Storage"))),]

item71.os.dat1 <- left_join(item71.os.dat0.1, scl.dat, by = "CK_Cadmus_ID")

item71.os.dat2 <- item71.os.dat1

#clean fixture and bulbs per fixture
item71.os.dat2$Fixture.Qty <- as.numeric(as.character(item71.os.dat2$Fixture.Qty))
item71.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item71.os.dat2$LIGHTING_BulbsPerFixture))

item71.os.dat2$Lamps <- item71.os.dat2$Fixture.Qty * item71.os.dat2$LIGHTING_BulbsPerFixture
unique(item71.os.dat2$Lamps)

item71.os.dat3 <- item71.os.dat2[which(!(is.na(item71.os.dat2$Lamps))),]

item71.os.customer <- summarise(group_by(item71.os.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item71.os.merge <- left_join(scl.dat, item71.os.customer)
item71.os.merge$Lamps[which(is.na(item71.os.merge$Lamps))] <- 0


################################################
# Adding pop and sample sizes for weights
################################################
item71.os.data <- weightedData(item71.os.merge[-which(colnames(item71.os.merge) %in% c("Lamps"))])
item71.os.data <- left_join(item71.os.data, unique(item71.os.merge[which(colnames(item71.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                          ,"Lamps"))]))
item71.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item71.os.final <- mean_one_group(CustomerLevelData = item71.os.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'CK_Building_ID'
                               ,aggregateRow     = 'Remove')
item71.os.final <- item71.os.final[which(item71.os.final$CK_Building_ID != "Remove"),]

item71.os.final.SF <- item71.os.final[which(item71.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item71.os.final) %in% c("BuildingType"))]

exportTable(item71.os.final.SF, "SF", "Table 78", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item71.os.final <- mean_one_group_unweighted(CustomerLevelData = item71.os.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'CK_Building_ID'
                                          ,aggregateRow     = 'Remove')
item71.os.final <- item71.os.final[which(item71.os.final$CK_Building_ID != "Remove"),]

item71.os.final.SF <- item71.os.final[which(item71.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item71.os.final) %in% c("BuildingType"))]

exportTable(item71.os.final.SF, "SF", "Table 78", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Table XX: AVERAGE NUMBER OF LEDS INSTALLED PER HOME BY CK_Building_ID (SF table 78, MH table 57)
#############################################################################################
#subset to columns needed for analysis
tableXX.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"CK_SiteID"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]
tableXX.os.dat$count <- 1

tableXX.os.dat0 <- tableXX.os.dat[which(tableXX.os.dat$Lamp.Category == "Light Emitting Diode"),]
tableXX.os.dat0.1 <- tableXX.os.dat0[which(!(tableXX.os.dat0$Clean.Room %in% c("Storage"))),]

tableXX.os.dat1 <- left_join(tableXX.os.dat0.1, scl.dat, by = "CK_Cadmus_ID")

tableXX.os.dat2 <- tableXX.os.dat1

#clean fixture and bulbs per fixture
tableXX.os.dat2$Fixture.Qty <- as.numeric(as.character(tableXX.os.dat2$Fixture.Qty))
tableXX.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(tableXX.os.dat2$LIGHTING_BulbsPerFixture))

tableXX.os.dat2$Lamps <- tableXX.os.dat2$Fixture.Qty * tableXX.os.dat2$LIGHTING_BulbsPerFixture
unique(tableXX.os.dat2$Lamps)

tableXX.os.dat3 <- tableXX.os.dat2[which(!(is.na(tableXX.os.dat2$Lamps))),]

tableXX.os.customer <- summarise(group_by(tableXX.os.dat3, CK_Cadmus_ID)
                              ,Lamps = sum(Lamps))

tableXX.os.merge <- left_join(scl.dat, tableXX.os.customer)
tableXX.os.merge$Lamps[which(is.na(tableXX.os.merge$Lamps))] <- 0


################################################
# Adding pop and sample sizes for weights
################################################
tableXX.os.data <- weightedData(tableXX.os.merge[-which(colnames(tableXX.os.merge) %in% c("Lamps"))])
tableXX.os.data <- left_join(tableXX.os.data, unique(tableXX.os.merge[which(colnames(tableXX.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Lamps"))]))
tableXX.os.data$count <- 1

#######################
# Weighted Analysis
#######################
tableXX.os.final <- mean_one_group(CustomerLevelData = tableXX.os.data
                                ,valueVariable    = 'Lamps'
                                ,byVariable       = 'CK_Building_ID'
                                ,aggregateRow     = 'Remove')
tableXX.os.final <- tableXX.os.final[which(tableXX.os.final$CK_Building_ID != "Remove"),]

tableXX.os.final.SF <- tableXX.os.final[which(tableXX.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableXX.os.final) %in% c("BuildingType"))]

exportTable(tableXX.os.final.SF, "SF", "Table XX", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableXX.os.final <- mean_one_group_unweighted(CustomerLevelData = tableXX.os.data
                                           ,valueVariable    = 'Lamps'
                                           ,byVariable       = 'CK_Building_ID'
                                           ,aggregateRow     = 'Remove')
tableXX.os.final <- tableXX.os.final[which(tableXX.os.final$CK_Building_ID != "Remove"),]

tableXX.os.final.SF <- tableXX.os.final[which(tableXX.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableXX.os.final) %in% c("BuildingType"))]

exportTable(tableXX.os.final.SF, "SF", "Table XX", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 72: AVERAGE NUMBER OF HALOGEN LAMPS INSTALLED PER HOME BY CK_Building_ID (SF table 79, MH table 58)
#############################################################################################
#subset to columns needed for analysis
item72.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item72.os.dat$count <- 1

item72.os.dat0 <- item72.os.dat[which(item72.os.dat$Lamp.Category == "Halogen"),]
item72.os.dat0.1 <- item72.os.dat0[which(!(item72.os.dat0$Clean.Room %in% c("Storage"))),]

item72.os.dat1 <- left_join(item72.os.dat0.1, scl.dat, by = "CK_Cadmus_ID")

item72.os.dat2 <- item72.os.dat1

#clean fixture and bulbs per fixture
item72.os.dat2$Fixture.Qty <- as.numeric(as.character(item72.os.dat2$Fixture.Qty))
item72.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item72.os.dat2$LIGHTING_BulbsPerFixture))

item72.os.dat2$Lamps <- item72.os.dat2$Fixture.Qty * item72.os.dat2$LIGHTING_BulbsPerFixture
unique(item72.os.dat2$Lamps)

item72.os.dat3 <- item72.os.dat2[which(!(is.na(item72.os.dat2$Lamps))),]


item72.os.customer <- summarise(group_by(item72.os.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item72.os.merge <- left_join(scl.dat, item72.os.customer)
item72.os.merge$Lamps[which(is.na(item72.os.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item72.os.data <- weightedData(item72.os.merge[-which(colnames(item72.os.merge) %in% c("Lamps"))])
item72.os.data <- left_join(item72.os.data, unique(item72.os.merge[which(colnames(item72.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))]))
item72.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item72.os.final <- mean_one_group(CustomerLevelData = item72.os.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'CK_Building_ID'
                               ,aggregateRow     = 'Remove')
item72.os.final <- item72.os.final[which(item72.os.final$CK_Building_ID != "Remove"),]

item72.os.final.SF <- item72.os.final[which(item72.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item72.os.final) %in% c("BuildingType"))]

exportTable(item72.os.final.SF, "SF", "Table 79", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item72.os.final <- mean_one_group_unweighted(CustomerLevelData = item72.os.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'CK_Building_ID'
                                          ,aggregateRow     = 'Remove')
item72.os.final <- item72.os.final[which(item72.os.final$CK_Building_ID != "Remove"),]

item72.os.final.SF <- item72.os.final[which(item72.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item72.os.final) %in% c("BuildingType"))]

exportTable(item72.os.final.SF, "SF", "Table 79", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 73: AVERAGE NUMBER OF INCANDESCENT LAMPS INSTALLED PER HOME BY CK_Building_ID (SF table 80, MH table 59)
#############################################################################################
#subset to columns needed for analysis
item73.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item73.os.dat$count <- 1

item73.os.dat0 <- item73.os.dat[which(item73.os.dat$Lamp.Category == "Incandescent"),]
item73.os.dat0.1 <- item73.os.dat0[which(!(item73.os.dat0$Clean.Room %in% c("Storage"))),]

item73.os.dat1 <- left_join(item73.os.dat0.1, scl.dat, by = "CK_Cadmus_ID")

item73.os.dat2 <- item73.os.dat1

#clean fixture and bulbs per fixture
item73.os.dat2$Fixture.Qty <- as.numeric(as.character(item73.os.dat2$Fixture.Qty))
item73.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item73.os.dat2$LIGHTING_BulbsPerFixture))

item73.os.dat2$Lamps <- item73.os.dat2$Fixture.Qty * item73.os.dat2$LIGHTING_BulbsPerFixture
unique(item73.os.dat2$Lamps)

item73.os.dat3 <- item73.os.dat2[which(!(is.na(item73.os.dat2$Lamps))),]



item73.os.customer <- summarise(group_by(item73.os.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item73.os.merge <- left_join(scl.dat, item73.os.customer)
item73.os.merge$Lamps[which(is.na(item73.os.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item73.os.data <- weightedData(item73.os.merge[-which(colnames(item73.os.merge) %in% c("Lamps"))])
item73.os.data <- left_join(item73.os.data, unique(item73.os.merge[which(colnames(item73.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))]))
item73.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item73.os.final <- mean_one_group(CustomerLevelData = item73.os.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'CK_Building_ID'
                               ,aggregateRow     = 'Remove')
item73.os.final <- item73.os.final[which(item73.os.final$CK_Building_ID != "Remove"),]

item73.os.final.SF <- item73.os.final[which(item73.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item73.os.final) %in% c("BuildingType"))]

exportTable(item73.os.final.SF, "SF", "Table 80", weighted = TRUE, osIndicator = "SCL", OS = T)


#######################
# Unweighted Analysis
#######################
item73.os.final <- mean_one_group_unweighted(CustomerLevelData = item73.os.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'CK_Building_ID'
                                          ,aggregateRow     = 'Remove')
item73.os.final <- item73.os.final[which(item73.os.final$CK_Building_ID != "Remove"),]

item73.os.final.SF <- item73.os.final[which(item73.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item73.os.final) %in% c("BuildingType"))]

exportTable(item73.os.final.SF, "SF", "Table 80", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 74: AVERAGE NUMBER OF LINEAR FLUORESCENT LAMPS INSTALLED PER HOME BY CK_Building_ID (SF table 81, MH table 60)
#############################################################################################
#subset to columns needed for analysis
item74.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item74.os.dat$count <- 1

item74.os.dat0 <- item74.os.dat[which(item74.os.dat$Lamp.Category == "Linear Fluorescent"),]
item74.os.dat0.1 <- item74.os.dat0[which(!(item74.os.dat0$Clean.Room %in% c("Storage"))),]

item74.os.dat2 <- left_join(item74.os.dat0.1, scl.dat, by = "CK_Cadmus_ID")

#clean fixture and bulbs per fixture
item74.os.dat2$Fixture.Qty <- as.numeric(as.character(item74.os.dat2$Fixture.Qty))
item74.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item74.os.dat2$LIGHTING_BulbsPerFixture))

item74.os.dat2$Lamps <- item74.os.dat2$Fixture.Qty * item74.os.dat2$LIGHTING_BulbsPerFixture
unique(item74.os.dat2$Lamps)

item74.os.dat3 <- item74.os.dat2[which(!(is.na(item74.os.dat2$Lamps))),]



item74.os.customer <- summarise(group_by(item74.os.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item74.os.merge <- left_join(scl.dat, item74.os.customer)
item74.os.merge$Lamps[which(is.na(item74.os.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item74.os.data <- weightedData(item74.os.merge[-which(colnames(item74.os.merge) %in% c("Lamps"))])
item74.os.data <- left_join(item74.os.data, unique(item74.os.merge[which(colnames(item74.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))]))
item74.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item74.os.final <- mean_one_group(CustomerLevelData = item74.os.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'CK_Building_ID'
                               ,aggregateRow     = 'Remove')
item74.os.final <- item74.os.final[which(item74.os.final$CK_Building_ID != "Remove"),]

item74.os.final.SF <- item74.os.final[which(item74.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item74.os.final) %in% c("BuildingType"))]

exportTable(item74.os.final.SF, "SF", "Table 81", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item74.os.final <- mean_one_group_unweighted(CustomerLevelData = item74.os.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'CK_Building_ID'
                                          ,aggregateRow     = 'Remove')
item74.os.final <- item74.os.final[which(item74.os.final$CK_Building_ID != "Remove"),]

item74.os.final.SF <- item74.os.final[which(item74.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item74.os.final) %in% c("BuildingType"))]

exportTable(item74.os.final.SF, "SF", "Table 81", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 75: AVERAGE NUMBER OF oTHER LAMPS INSTALLED PER HOME BY CK_Building_ID (SF table 82, MH table 61)
#############################################################################################
#subset to columns needed for analysis
item75.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item75.os.dat$count <- 1

item75.os.dat0 <- item75.os.dat[which(item75.os.dat$Lamp.Category == "Other"),]
item75.os.dat0.1 <- item75.os.dat0[which(!(item75.os.dat0$Clean.Room %in% c("Storage"))),]

item75.os.dat2 <- left_join(item75.os.dat0.1, scl.dat, by = "CK_Cadmus_ID")

#clean fixture and bulbs per fixture
item75.os.dat2$Fixture.Qty <- as.numeric(as.character(item75.os.dat2$Fixture.Qty))
item75.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item75.os.dat2$LIGHTING_BulbsPerFixture))

item75.os.dat2$Lamps <- item75.os.dat2$Fixture.Qty * item75.os.dat2$LIGHTING_BulbsPerFixture
unique(item75.os.dat2$Lamps)

item75.os.dat3 <- item75.os.dat2[which(!(is.na(item75.os.dat2$Lamps))),]



item75.os.customer <- summarise(group_by(item75.os.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item75.os.merge <- left_join(scl.dat, item75.os.customer)
item75.os.merge$Lamps[which(is.na(item75.os.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item75.os.data <- weightedData(item75.os.merge[-which(colnames(item75.os.merge) %in% c("Lamps"))])
item75.os.data <- left_join(item75.os.data, unique(item75.os.merge[which(colnames(item75.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))]))
item75.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item75.os.final <- mean_one_group(CustomerLevelData = item75.os.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'CK_Building_ID'
                               ,aggregateRow     = 'Remove')
item75.os.final <- item75.os.final[which(item75.os.final$CK_Building_ID != "Remove"),]

item75.os.final.SF <- item75.os.final[which(item75.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item75.os.final) %in% c("BuildingType"))]

exportTable(item75.os.final.SF, "SF", "Table 82", weighted = TRUE, osIndicator = "SCL", OS = T)


#######################
# Unweighted Analysis
#######################
item75.os.final <- mean_one_group_unweighted(CustomerLevelData = item75.os.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'CK_Building_ID'
                                          ,aggregateRow     = 'Remove')
item75.os.final <- item75.os.final[which(item75.os.final$CK_Building_ID != "Remove"),]

item75.os.final.SF <- item75.os.final[which(item75.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item75.os.final) %in% c("BuildingType"))]

exportTable(item75.os.final.SF, "SF", "Table 82", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 76: AVERAGE NUMBER OF STORED COMPACT FLUORESCENT LAMPS BY CK_Building_ID (SF table 83, MH table 62)
#############################################################################################
#subset to columns needed for analysis
item76.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item76.os.dat$count <- 1

item76.os.dat0 <- item76.os.dat[which(item76.os.dat$Clean.Room == "Storage"),]
item76.os.dat0.1 <- item76.os.dat0[which(item76.os.dat$Lamp.Category == "Compact Fluorescent"),]

item76.os.dat1 <- left_join(item76.os.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

# item76.os.dat2 <- item76.os.dat1[grep("SITE", item76.os.dat1$CK_SiteID),]
item76.os.dat2 <- item76.os.dat1

#clean fixture and bulbs per fixture
item76.os.dat2$Fixture.Qty <- as.numeric(as.character(item76.os.dat2$Fixture.Qty))
item76.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item76.os.dat2$LIGHTING_BulbsPerFixture))

item76.os.dat2$Lamps <- item76.os.dat2$Fixture.Qty * item76.os.dat2$LIGHTING_BulbsPerFixture
unique(item76.os.dat2$Lamps)

item76.os.dat3 <- item76.os.dat2[which(!(is.na(item76.os.dat2$Lamps))),]


item76.os.customer <- summarise(group_by(item76.os.dat3, CK_Cadmus_ID)
                             ,Lamps = sum(Lamps))

item76.os.merge <- left_join(rbsa.dat, item76.os.customer)
item76.os.merge$Lamps[which(is.na(item76.os.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
item76.os.data <- weightedData(item76.os.merge[-which(colnames(item76.os.merge) %in% c("Lamps"))])
item76.os.data <- left_join(item76.os.data, unique(item76.os.merge[which(colnames(item76.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Lamps"))]))
item76.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item76.os.final <- mean_one_group(CustomerLevelData = item76.os.data
                               ,valueVariable    = 'Lamps'
                               ,byVariable       = 'CK_Building_ID'
                               ,aggregateRow     = 'Remove')
item76.os.final <- item76.os.final[which(item76.os.final$CK_Building_ID != "Remove"),]

item76.os.final.SF <- item76.os.final[which(item76.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item76.os.final) %in% c("BuildingType"))]

exportTable(item76.os.final.SF, "SF", "Table 83", weighted = TRUE, osIndicator = "SCL", OS = T)


#######################
# Unweighted Analysis
#######################
item76.os.final <- mean_one_group_unweighted(CustomerLevelData = item76.os.data
                                          ,valueVariable    = 'Lamps'
                                          ,byVariable       = 'CK_Building_ID'
                                          ,aggregateRow     = 'Remove')
item76.os.final <- item76.os.final[which(item76.os.final$CK_Building_ID != "Remove"),]

item76.os.final.SF <- item76.os.final[which(item76.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item76.os.final) %in% c("BuildingType"))]

exportTable(item76.os.final.SF, "SF", "Table 83", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Table YY: Average number of Stored LED lamps by CK_Building_ID
#############################################################################################
#subset to columns needed for analysis
tableYY.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"CK_SiteID"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]
tableYY.os.dat$count <- 1

tableYY.os.dat0 <- tableYY.os.dat[which(tableYY.os.dat$Clean.Room == "Storage"),]
tableYY.os.dat0.1 <- tableYY.os.dat0[which(tableYY.os.dat$Lamp.Category == "Light Emitting Diode"),]

tableYY.os.dat1 <- left_join(tableYY.os.dat0.1, scl.dat, by = "CK_Cadmus_ID")

tableYY.os.dat2 <- tableYY.os.dat1

#clean fixture and bulbs per fixture
tableYY.os.dat2$Fixture.Qty <- as.numeric(as.character(tableYY.os.dat2$Fixture.Qty))
tableYY.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(tableYY.os.dat2$LIGHTING_BulbsPerFixture))

tableYY.os.dat2$Lamps <- tableYY.os.dat2$Fixture.Qty * tableYY.os.dat2$LIGHTING_BulbsPerFixture
unique(tableYY.os.dat2$Lamps)

tableYY.os.dat3 <- tableYY.os.dat2[which(!(is.na(tableYY.os.dat2$Lamps))),]


tableYY.os.customer <- summarise(group_by(tableYY.os.dat3, CK_Cadmus_ID)
                              ,Lamps = sum(Lamps))

tableYY.os.merge <- left_join(scl.dat, tableYY.os.customer)
tableYY.os.merge$Lamps[which(is.na(tableYY.os.merge$Lamps))] <- 0 


################################################
# Adding pop and sample sizes for weights
################################################
tableYY.os.data <- weightedData(tableYY.os.merge[-which(colnames(tableYY.os.merge) %in% c("Lamps"))])
tableYY.os.data <- left_join(tableYY.os.data, unique(tableYY.os.merge[which(colnames(tableYY.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Lamps"))]))
tableYY.os.data$count <- 1

#######################
# Weighted Analysis
#######################
tableYY.os.final <- mean_one_group(CustomerLevelData = tableYY.os.data
                                ,valueVariable    = 'Lamps'
                                ,byVariable       = 'CK_Building_ID'
                                ,aggregateRow     = 'Remove')
tableYY.os.final <- tableYY.os.final[which(tableYY.os.final$CK_Building_ID != "Remove"),]

tableYY.os.final.SF <- tableYY.os.final[which(tableYY.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableYY.os.final) %in% c("BuildingType"))]

exportTable(tableYY.os.final.SF, "SF", "Table YY", weighted = TRUE, osIndicator = "SCL", OS = T)


#######################
# Unweighted Analysis
#######################
tableYY.os.final <- mean_one_group_unweighted(CustomerLevelData = tableYY.os.data
                                           ,valueVariable    = 'Lamps'
                                           ,byVariable       = 'CK_Building_ID'
                                           ,aggregateRow     = 'Remove')
tableYY.os.final <- tableYY.os.final[which(tableYY.os.final$CK_Building_ID != "Remove"),]

tableYY.os.final.SF <- tableYY.os.final[which(tableYY.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableYY.os.final) %in% c("BuildingType"))]

exportTable(tableYY.os.final.SF, "SF", "Table YY", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Table AF: Average number of Stored lamps by lamp type and CK_Building_ID
#############################################################################################
#subset to columns needed for analysis
tableAF.os.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"CK_SiteID"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]
tableAF.os.dat$count <- 1

tableAF.os.dat0 <- left_join(scl.dat, tableAF.os.dat, by = "CK_Cadmus_ID")

tableAF.os.dat1 <- tableAF.os.dat0[which(!is.na(tableAF.os.dat0$Lamp.Category)),]

tableAF.os.dat2 <- tableAF.os.dat1[grep("SITE", tableAF.os.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
tableAF.os.dat2$Fixture.Qty <- as.numeric(as.character(tableAF.os.dat2$Fixture.Qty))
tableAF.os.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(tableAF.os.dat2$LIGHTING_BulbsPerFixture))

tableAF.os.dat2$Lamps <- tableAF.os.dat2$Fixture.Qty * tableAF.os.dat2$LIGHTING_BulbsPerFixture
unique(tableAF.os.dat2$Lamps)

tableAF.os.dat3 <- tableAF.os.dat2[which(!(is.na(tableAF.os.dat2$Lamps))),]

tableAF.os.led.sum <- summarise(group_by(tableAF.os.dat3, CK_Cadmus_ID, Lamp.Category)
                             ,TotalBulbs = sum(Lamps))

## subset to only storage bulbs
tableAF.os.storage <- tableAF.os.dat3[which(tableAF.os.dat3$Clean.Room == "Storage"),]
#summarise within site
tableAF.os.storage.sum <- summarise(group_by(tableAF.os.storage, CK_Cadmus_ID, Lamp.Category)
                                 ,StorageBulbs = sum(Lamps))
length(unique(tableAF.os.storage.sum$CK_Cadmus_ID))


tableAF.os.merge1 <- left_join(tableAF.os.led.sum, tableAF.os.storage.sum)

tableAF.os.cast <- dcast(setDT(tableAF.os.merge1)
                      ,formula = CK_Cadmus_ID ~ Lamp.Category
                      ,value.var = c("StorageBulbs"))
tableAF.os.cast[is.na(tableAF.os.cast),] <- 0

tableAF.os.melt <- melt(tableAF.os.cast, id.vars = "CK_Cadmus_ID")
names(tableAF.os.melt) <- c("CK_Cadmus_ID", "Lamp.Category", "StorageBulbs")


tableAF.os.merge  <- left_join(scl.dat, tableAF.os.melt)
tableAF.os.merge$StorageBulbs[which(is.na(tableAF.os.merge$StorageBulbs))] <- 0


################################################
# Adding pop and sample sizes for weights
################################################
tableAF.os.data <- weightedData(tableAF.os.merge[-which(colnames(tableAF.os.merge) %in% c("StorageBulbs"
                                                                                          ,"Lamp.Category"
                                                                                          # ,"TotalBulbs"
))])
tableAF.os.data <- left_join(tableAF.os.data, tableAF.os.merge[which(colnames(tableAF.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                       ,"StorageBulbs"
                                                                                                       ,"Lamp.Category"
                                                                                                       # ,"TotalBulbs"
))])
tableAF.os.data$count <- 1

#######################
# Weighted Analysis
#######################
tableAF.os.final <- mean_two_groups(CustomerLevelData = tableAF.os.data
                                    ,valueVariable = "StorageBulbs"
                                    ,byVariableRow = "Lamp.Category"
                                    ,byVariableColumn = "CK_Building_ID"
                                    ,columnAggregate = "Remove"
                                    ,rowAggregate = "All Categories")
tableAF.os.cast <- data.frame(tableAF.os.final, stringsAsFactors = F)

tableAF.os.table <- data.frame("BuildingType"    = tableAF.os.cast$BuildingType
                            ,"Lamp.Category"  = tableAF.os.cast$Lamp.Category
                            ,"Mean_SCL.GenPop"      = tableAF.os.cast$Mean_SCL.GenPop
                            ,"SE_SCL.GenPop"        = tableAF.os.cast$SE_SCL.GenPop
                            ,"n_SCL.GenPop"         = tableAF.os.cast$n_SCL.GenPop
                            ,"Mean_SCL.LI"          = tableAF.os.cast$Mean_SCL.LI
                            ,"SE_SCL.LI"            = tableAF.os.cast$SE_SCL.LI
                            ,"n_SCL.LI"             = tableAF.os.cast$n_SCL.LI
                            ,"Mean_SCL.EH"          = tableAF.os.cast$Mean_SCL.EH
                            ,"SE_SCL.EH"            = tableAF.os.cast$SE_SCL.EH
                            ,"n_SCL.EH"             = tableAF.os.cast$n_SCL.EH
                            ,"Mean_2017.RBSA.PS"    = tableAF.os.cast$Mean_2017.RBSA.PS
                            ,"SE_2017.RBSA.PS"      = tableAF.os.cast$SE_2017.RBSA.PS
                            ,"n_2017.RBSA.PS"       = tableAF.os.cast$n_2017.RBSA.PS
                            ,"EB_SCL.GenPop"        = tableAF.os.cast$EB_SCL.GenPop
                            ,"EB_SCL.LI"            = tableAF.os.cast$EB_SCL.LI
                            ,"EB_SCL.EH"            = tableAF.os.cast$EB_SCL.EH
                            ,"EB_2017.RBSA.PS"      = tableAF.os.cast$EB_2017.RBSA.PS
)

levels(tableAF.os.table$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"All Categories")
tableAF.os.table <- tableAF.os.table %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAF.os.table <- data.frame(tableAF.os.table)

tableAF.os.final.SF <- tableAF.os.table[which(tableAF.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableAF.os.table) %notin% c("BuildingType"))]

exportTable(tableAF.os.final.SF, "SF", "Table AF", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableAF.os.final <- mean_two_groups_unweighted(CustomerLevelData = tableAF.os.data
                                            ,valueVariable = "StorageBulbs"
                                            ,byVariableRow = "Lamp.Category"
                                            ,byVariableColumn = "CK_Building_ID"
                                            ,columnAggregate = "Remove"
                                            ,rowAggregate = "All Categories")
tableAF.os.cast <- data.frame(tableAF.os.final, stringsAsFactors = F)

tableAF.os.table <- data.frame("BuildingType"    = tableAF.os.cast$BuildingType
                            ,"Lamp.Category"  = tableAF.os.cast$Lamp.Category
                            ,"Mean_SCL.GenPop"      = tableAF.os.cast$Mean_SCL.GenPop
                            ,"SE_SCL.GenPop"        = tableAF.os.cast$SE_SCL.GenPop
                            ,"n_SCL.GenPop"         = tableAF.os.cast$n_SCL.GenPop
                            ,"Mean_SCL.LI"          = tableAF.os.cast$Mean_SCL.LI
                            ,"SE_SCL.LI"            = tableAF.os.cast$SE_SCL.LI
                            ,"n_SCL.LI"             = tableAF.os.cast$n_SCL.LI
                            ,"Mean_SCL.EH"          = tableAF.os.cast$Mean_SCL.EH
                            ,"SE_SCL.EH"            = tableAF.os.cast$SE_SCL.EH
                            ,"n_SCL.EH"             = tableAF.os.cast$n_SCL.EH
                            ,"Mean_2017.RBSA.PS"    = tableAF.os.cast$Mean_2017.RBSA.PS
                            ,"SE_2017.RBSA.PS"      = tableAF.os.cast$SE_2017.RBSA.PS
                            ,"n_2017.RBSA.PS"       = tableAF.os.cast$n_2017.RBSA.PS
)

levels(tableAF.os.table$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"All Categories")
tableAF.os.table <- tableAF.os.table %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAF.os.table <- data.frame(tableAF.os.table)

tableAF.os.final.SF <- tableAF.os.table[which(tableAF.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableAF.os.table) %notin% c("BuildingType"))]
exportTable(tableAF.os.final.SF, "SF", "Table AF", weighted = FALSE, osIndicator = "SCL", OS = T)
