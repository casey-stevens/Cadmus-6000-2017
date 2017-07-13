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
#Item 66: AVERAGE NUMBER OF LAMPS PER HOME BY STATE (SF table 73, MH table 52)
#############################################################################################
#subset to columns needed for analysis
item66.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"))]
item66.dat$count <- 1

item66.dat1 <- left_join(item66.dat, rbsa.dat, by = "CK_Cadmus_ID")

item66.dat2 <- item66.dat1[-grep("BLDG", item66.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item66.dat2$Fixture.Qty <- as.numeric(as.character(item66.dat2$Fixture.Qty))
item66.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item66.dat2$LIGHTING_BulbsPerFixture))

item66.dat2$Lamps <- item66.dat2$Fixture.Qty * item66.dat2$LIGHTING_BulbsPerFixture
unique(item66.dat2$Lamps)

item66.dat3 <- item66.dat2[which(!(is.na(item66.dat2$Lamps))),]


##total lamps per home
item66.lamps <- summarise(group_by(item66.dat3, BuildingType, State, CK_Cadmus_ID)
                          ,LampsPerSite = sum(Lamps))
##average lamps across homes per state
item66.state <- summarise(group_by(item66.lamps, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean = mean(LampsPerSite)
                          ,SE   = sd(LampsPerSite) / SampleSize)
##average lamps across homes per state
item66.region <- summarise(group_by(item66.lamps, BuildingType)
                           ,State = "Region"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean = mean(LampsPerSite)
                          ,SE   = sd(LampsPerSite) / SampleSize)
#combine state and region information
item66.final <- rbind.data.frame(item66.state, item66.region, stringsAsFactors = F)


#table format
item66.table <- data.frame("BuildingType" = item66.final$BuildingType
                           ,"State" = item66.final$State
                           ,"Mean" = item66.final$Mean
                           ,"SE" = item66.final$SE
                           ,"SampleSize" = item66.final$SampleSize)

#subset to only relevant building types
item66.table1 <- item66.table[which(item66.table$BuildingType %in% c("Single Family", "Manufactured")),]


# ##write out to correct tab
# library(XLConnect)
# # writeWorksheetToFile(file = paste(outputFolder, "Prelim_Unweighted_Results.xlsx", sep = "/"), data = item66.final1, sheet = "Item 66")
# # wb <- loadWorkbook(paste(outputFolder, "Prelim_Unweighted_Results.xlsx", sep = "/"), create=FALSE)
# # writeWorksheet(wb, item66.final1, "Item 66", startRow = 20, startCol = 1, header = TRUE)
# 
# 
# wb <- createWorkbook()
# ws.name.site <- "Item 66"
# addWorksheet(wb, ws.name.site)
# writeData(wb,sheet = ws.name.site,x = item66.final1, startRow = 20)
# 
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/www/js/workers")
# saveWorkbook(wb, file.path(outputFolder,"Prelim_Unweighted_Results.xlsx"))




#############################################################################################
#Item 67: AVERAGE NUMBER OF FIXTURES PER HOME BY STATE (SF table 74, MH table 53)
#############################################################################################
#subset to columns needed for analysis
item67.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"CK_LightingDetail_ID"))]
item67.dat$count <- 1

item67.dat1 <- left_join(item67.dat, rbsa.dat, by = "CK_Cadmus_ID")

item67.dat2 <- item67.dat1[-grep("BLDG", item67.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item67.dat2$Fixture.Qty <- as.numeric(as.character(item67.dat2$Fixture.Qty))
item67.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item67.dat2$LIGHTING_BulbsPerFixture))

item67.dat2$Lamps <- item67.dat2$Fixture.Qty * item67.dat2$LIGHTING_BulbsPerFixture
unique(item67.dat2$Lamps)

item67.dat3 <- item67.dat2[which(!(is.na(item67.dat2$Fixture.Qty))),]

##total fixtures per home
item67.fixtures <- summarise(group_by(item67.dat3, BuildingType, State, CK_Cadmus_ID)
                          ,FixturesPerSite = sum(Fixture.Qty))
##average Fixtures across homes per state
item67.state <- summarise(group_by(item67.fixtures, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean = mean(FixturesPerSite)
                          ,SE   = sd(FixturesPerSite) / SampleSize)
##average Fixtures across homes per state
item67.region <- summarise(group_by(item67.fixtures, BuildingType)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(FixturesPerSite)
                           ,SE   = sd(FixturesPerSite) / SampleSize)
#combine state and region information
item67.final <- rbind.data.frame(item67.state, item67.region, stringsAsFactors = F)



#table format
item67.table <- data.frame("BuildingType" = item67.final$BuildingType
                           ,"State" = item67.final$State
                           ,"Mean" = item67.final$Mean
                           ,"SE" = item67.final$SE
                           ,"SampleSize" = item67.final$SampleSize)

#subset to only relevant building types
item67.table1 <- item67.table[which(item67.table$BuildingType %in% c("Single Family", "Manufactured")),]

