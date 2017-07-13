#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export))
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
item71.dat0.1 <- item71.dat0[which(!(item71.dat0$Clean.Room %in% c("Basement", "Storage"))),]

item71.dat1 <- left_join(item71.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item71.dat2 <- item71.dat1[-grep("BLDG", item71.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item71.dat2$Fixture.Qty <- as.numeric(as.character(item71.dat2$Fixture.Qty))
item71.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item71.dat2$LIGHTING_BulbsPerFixture))

item71.dat2$Lamps <- item71.dat2$Fixture.Qty * item71.dat2$LIGHTING_BulbsPerFixture
unique(item71.dat2$Lamps)

item71.dat3 <- item71.dat2[which(!(is.na(item71.dat2$Lamps))),]


#by total lamps within each household
item71.totLamps <- summarise(group_by(item71.dat3, CK_Cadmus_ID, BuildingType, State)
                           ,SiteLamps = sum(Lamps))
#summarize by state
item71.state <- summarise(group_by(item71.totLamps, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(SiteLamps)
                          ,SE         = sd(SiteLamps) / sqrt(SampleSize))
#summarize across states
item71.region <- summarise(group_by(item71.totLamps, BuildingType)
                          ,State      = "Region"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(SiteLamps)
                          ,SE         = sd(SiteLamps) / sqrt(SampleSize))
item71.final <- rbind.data.frame(item71.state, item71.region, stringsAsFactors = F)



item71.table <- data.frame("BuildingType" = item71.final$BuildingType
                           ,"State" = item71.final$State
                           ,"Mean" = item71.final$Mean
                           ,"SE" = item71.final$SE
                           ,"SampleSize" = item71.final$SampleSize)


item71.table1 <- item71.table[which(item71.table$BuildingType %in% c("Single Family","Manufactured")),]
View(item71.table1)









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
item72.dat0.1 <- item72.dat0[which(!(item72.dat0$Clean.Room %in% c("Basement", "Storage"))),]

item72.dat1 <- left_join(item72.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item72.dat2 <- item72.dat1[-grep("BLDG", item72.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item72.dat2$Fixture.Qty <- as.numeric(as.character(item72.dat2$Fixture.Qty))
item72.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item72.dat2$LIGHTING_BulbsPerFixture))

item72.dat2$Lamps <- item72.dat2$Fixture.Qty * item72.dat2$LIGHTING_BulbsPerFixture
unique(item72.dat2$Lamps)

item72.dat3 <- item72.dat2[which(!(is.na(item72.dat2$Lamps))),]


#by total lamps within each household
item72.totLamps <- summarise(group_by(item72.dat3, CK_Cadmus_ID, BuildingType, State)
                             ,SiteLamps = sum(Lamps))
#summarize by state
item72.state <- summarise(group_by(item72.totLamps, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(SiteLamps)
                          ,SE         = sd(SiteLamps) / sqrt(SampleSize))
#summarize across states
item72.region <- summarise(group_by(item72.totLamps, BuildingType)
                           ,State      = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean       = mean(SiteLamps)
                           ,SE         = sd(SiteLamps) / sqrt(SampleSize))
item72.final <- rbind.data.frame(item72.state, item72.region, stringsAsFactors = F)

item72.table <- data.frame("BuildingType" = item72.final$BuildingType
                           ,"State" = item72.final$State
                           ,"Mean" = item72.final$Mean
                           ,"SE" = item72.final$SE
                           ,"SampleSize" = item72.final$SampleSize)


item72.table1 <- item72.table[which(item72.table$BuildingType %in% c("Single Family","Manufactured")),]
View(item72.table1)









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
item73.dat0.1 <- item73.dat0[which(!(item73.dat0$Clean.Room %in% c("Basement", "Storage"))),]

item73.dat1 <- left_join(item73.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item73.dat2 <- item73.dat1[-grep("BLDG", item73.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item73.dat2$Fixture.Qty <- as.numeric(as.character(item73.dat2$Fixture.Qty))
item73.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item73.dat2$LIGHTING_BulbsPerFixture))

item73.dat2$Lamps <- item73.dat2$Fixture.Qty * item73.dat2$LIGHTING_BulbsPerFixture
unique(item73.dat2$Lamps)

item73.dat3 <- item73.dat2[which(!(is.na(item73.dat2$Lamps))),]


#by total lamps within each household
item73.totLamps <- summarise(group_by(item73.dat3, CK_Cadmus_ID, BuildingType, State)
                             ,SiteLamps = sum(Lamps))
#summarize by state
item73.state <- summarise(group_by(item73.totLamps, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(SiteLamps)
                          ,SE         = sd(SiteLamps) / sqrt(SampleSize))
#summarize across states
item73.region <- summarise(group_by(item73.totLamps, BuildingType)
                           ,State      = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean       = mean(SiteLamps)
                           ,SE         = sd(SiteLamps) / sqrt(SampleSize))
item73.final <- rbind.data.frame(item73.state, item73.region, stringsAsFactors = F)

item73.table <- data.frame("BuildingType" = item73.final$BuildingType
                           ,"State" = item73.final$State
                           ,"Mean" = item73.final$Mean
                           ,"SE" = item73.final$SE
                           ,"SampleSize" = item73.final$SampleSize)


item73.table1 <- item73.table[which(item73.table$BuildingType %in% c("Single Family","Manufactured")),]
View(item73.table1)











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
item74.dat0.1 <- item74.dat0[which(!(item74.dat0$Clean.Room %in% c("Basement", "Storage"))),]

item74.dat1 <- left_join(item74.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item74.dat2 <- item74.dat1[-grep("BLDG", item74.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item74.dat2$Fixture.Qty <- as.numeric(as.character(item74.dat2$Fixture.Qty))
item74.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item74.dat2$LIGHTING_BulbsPerFixture))

item74.dat2$Lamps <- item74.dat2$Fixture.Qty * item74.dat2$LIGHTING_BulbsPerFixture
unique(item74.dat2$Lamps)

item74.dat3 <- item74.dat2[which(!(is.na(item74.dat2$Lamps))),]


#by total lamps within each household
item74.totLamps <- summarise(group_by(item74.dat3, CK_Cadmus_ID, BuildingType, State)
                             ,SiteLamps = sum(Lamps))
#summarize by state
item74.state <- summarise(group_by(item74.totLamps, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(SiteLamps)
                          ,SE         = sd(SiteLamps) / sqrt(SampleSize))
#summarize across states
item74.region <- summarise(group_by(item74.totLamps, BuildingType)
                           ,State      = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean       = mean(SiteLamps)
                           ,SE         = sd(SiteLamps) / sqrt(SampleSize))
item74.final <- rbind.data.frame(item74.state, item74.region, stringsAsFactors = F)

item74.table <- data.frame("BuildingType" = item74.final$BuildingType
                           ,"State" = item74.final$State
                           ,"Mean" = item74.final$Mean
                           ,"SE" = item74.final$SE
                           ,"SampleSize" = item74.final$SampleSize)


item74.table1 <- item74.table[which(item74.table$BuildingType %in% c("Single Family","Manufactured")),]
View(item74.table1)












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
item75.dat0.1 <- item75.dat0[which(!(item75.dat0$Clean.Room %in% c("Basement", "Storage"))),]

item75.dat1 <- left_join(item75.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")

item75.dat2 <- item75.dat1[-grep("BLDG", item75.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item75.dat2$Fixture.Qty <- as.numeric(as.character(item75.dat2$Fixture.Qty))
item75.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item75.dat2$LIGHTING_BulbsPerFixture))

item75.dat2$Lamps <- item75.dat2$Fixture.Qty * item75.dat2$LIGHTING_BulbsPerFixture
unique(item75.dat2$Lamps)

item75.dat3 <- item75.dat2[which(!(is.na(item75.dat2$Lamps))),]


#by total lamps within each household
item75.totLamps <- summarise(group_by(item75.dat3, CK_Cadmus_ID, BuildingType, State)
                             ,SiteLamps = sum(Lamps))
#summarize by state
item75.state <- summarise(group_by(item75.totLamps, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(SiteLamps)
                          ,SE         = sd(SiteLamps) / sqrt(SampleSize))
#summarize across states
item75.region <- summarise(group_by(item75.totLamps, BuildingType)
                           ,State      = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean       = mean(SiteLamps)
                           ,SE         = sd(SiteLamps) / sqrt(SampleSize))
item75.final <- rbind.data.frame(item75.state, item75.region, stringsAsFactors = F)

item75.table <- data.frame("BuildingType" = item75.final$BuildingType
                           ,"State" = item75.final$State
                           ,"Mean" = item75.final$Mean
                           ,"SE" = item75.final$SE
                           ,"SampleSize" = item75.final$SampleSize)


item75.table1 <- item75.table[which(item75.table$BuildingType %in% c("Single Family","Manufactured")),]
View(item75.table1)









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

# item76.dat2 <- item76.dat1[-grep("BLDG", item76.dat1$CK_SiteID),]
item76.dat2 <- item76.dat1

#clean fixture and bulbs per fixture
item76.dat2$Fixture.Qty <- as.numeric(as.character(item76.dat2$Fixture.Qty))
item76.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item76.dat2$LIGHTING_BulbsPerFixture))

item76.dat2$Lamps <- item76.dat2$Fixture.Qty * item76.dat2$LIGHTING_BulbsPerFixture
unique(item76.dat2$Lamps)

item76.dat3 <- item76.dat2[which(!(is.na(item76.dat2$Lamps))),]


#by total lamps within each household
item76.totLamps <- summarise(group_by(item76.dat3, CK_Cadmus_ID, BuildingType, State)
                             ,SiteLamps = sum(Lamps))
#summarize by state
item76.state <- summarise(group_by(item76.totLamps, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(SiteLamps)
                          ,SE         = sd(SiteLamps) / sqrt(SampleSize))
#summarize across states
item76.region <- summarise(group_by(item76.totLamps, BuildingType)
                           ,State      = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean       = mean(SiteLamps)
                           ,SE         = sd(SiteLamps) / sqrt(SampleSize))
item76.final <- rbind.data.frame(item76.state, item76.region, stringsAsFactors = F)

item76.table <- data.frame("BuildingType" = item76.final$BuildingType
                           ,"State" = item76.final$State
                           ,"Mean" = item76.final$Mean
                           ,"SE" = item76.final$SE
                           ,"SampleSize" = item76.final$SampleSize)


item76.table1 <- item76.table[which(item76.table$BuildingType %in% c("Single Family","Manufactured")),]
View(item76.table1)
