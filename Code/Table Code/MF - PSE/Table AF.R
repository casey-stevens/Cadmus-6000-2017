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
# lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))



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
                                                                                 ,"Category"
                                                                                 ))])
tableAF.data <- left_join(tableAF.data, tableAF.merge[which(colnames(tableAF.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"StorageBulbs"
                                                                                           ,"Lamp.Category"
                                                                                           # ,"TotalBulbs"
                                                                                           ,"Category"
                                                                                           ))])
tableAF.data$count <- 1
stopifnot(nrow(tableAF.data) == nrow(tableAF.merge))


#######################################################
# AF Multifamily
#######################################################
#######################
# Weighted Analysis
#######################
tableAF.data.MF <- tableAF.data[which(tableAF.data$BuildingType == "Multifamily"),]
tableAF.data.MF <- tableAF.data.MF[grep("site",tableAF.data.MF$CK_Building_ID,ignore.case = T),]

tableAF.MF.summary <- mean_two_groups(CustomerLevelData = tableAF.data.MF
                                   ,valueVariable = "StorageBulbs"
                                   ,byVariableRow = "Lamp.Category"
                                   ,byVariableColumn = "Category"
                                   ,columnAggregate = "Remove"
                                   ,rowAggregate = "All Categories")


tableAF.MF.cast <- tableAF.MF.summary
names(tableAF.MF.cast)
tableAF.MF.table <- data.frame("BuildingType"    = tableAF.MF.cast$BuildingType
                               ,"Lamp.Category"  = tableAF.MF.cast$Lamp.Category
                               ,"PSE.Mean"                 = tableAF.MF.cast$Mean_PSE
                               ,"PSE.SE"                   = tableAF.MF.cast$SE_PSE
                               ,"PSE.n"                    = tableAF.MF.cast$n_PSE
                               ,"PSE.King.County.Mean"     = tableAF.MF.cast$`Mean_PSE KING COUNTY`
                               ,"PSE.King.County.SE"       = tableAF.MF.cast$`SE_PSE KING COUNTY`
                               ,"PSE.King.County.n"        = tableAF.MF.cast$`n_PSE KING COUNTY`
                               ,"PSE.Non.King.County.Mean" = tableAF.MF.cast$`Mean_PSE NON-KING COUNTY`
                               ,"PSE.Non.King.County.SE"   = tableAF.MF.cast$`SE_PSE NON-KING COUNTY`
                               ,"PSE.Non.King.County.n"    = tableAF.MF.cast$`n_PSE NON-KING COUNTY`
                               ,"2017.RBSA.PS.Mean"        = tableAF.MF.cast$`Mean_2017 RBSA PS`
                               ,"2017.RBSA.PS.SE"          = tableAF.MF.cast$`SE_2017 RBSA PS`
                               ,"2017.RBSA.PS.n"           = tableAF.MF.cast$`n_2017 RBSA PS`
                               ,"PSE_EB"                   = tableAF.MF.cast$EB_PSE
                               ,"PSE.King.County_EB"       = tableAF.MF.cast$`EB_PSE KING COUNTY`
                               ,"PSE.Non.King.County_EB"   = tableAF.MF.cast$`EB_PSE NON-KING COUNTY`
                               ,"2017.RBSA.PS_EB"          = tableAF.MF.cast$`EB_2017 RBSA PS`
)

levels(tableAF.MF.table$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"Unknown"
              ,"All Categories")
tableAF.MF.table <- tableAF.MF.table %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAF.MF.table <- data.frame(tableAF.MF.table)

tableAF.MF.final.MF <- tableAF.MF.table[which(tableAF.MF.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableAF.MF.table) %notin% c("BuildingType"))]

exportTable(tableAF.MF.final.MF, "MF", "Table AF", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
tableAF.MF.final <- mean_two_groups_unweighted(CustomerLevelData = tableAF.data.MF
                                            ,valueVariable = "StorageBulbs"
                                            ,byVariableRow = "Lamp.Category"
                                            ,byVariableColumn = "Category"
                                            ,columnAggregate = "Remove"
                                            ,rowAggregate = "All Categories")
tableAF.MF.cast <- data.frame(tableAF.MF.final, stringsAMFactors = F)

tableAF.MF.table <- data.frame("BuildingType"    = tableAF.MF.cast$BuildingType
                               ,"Lamp.Category"  = tableAF.MF.cast$Lamp.Category
                               ,"PSE.Mean"                 = tableAF.MF.cast$Mean_PSE
                               ,"PSE.SE"                   = tableAF.MF.cast$SE_PSE
                               ,"PSE.n"                    = tableAF.MF.cast$n_PSE
                               ,"PSE.King.County.Mean"     = tableAF.MF.cast$`Mean_PSE KING COUNTY`
                               ,"PSE.King.County.SE"       = tableAF.MF.cast$`SE_PSE KING COUNTY`
                               ,"PSE.King.County.n"        = tableAF.MF.cast$`n_PSE KING COUNTY`
                               ,"PSE.Non.King.County.Mean" = tableAF.MF.cast$`Mean_PSE NON-KING COUNTY`
                               ,"PSE.Non.King.County.SE"   = tableAF.MF.cast$`SE_PSE NON-KING COUNTY`
                               ,"PSE.Non.King.County.n"    = tableAF.MF.cast$`n_PSE NON-KING COUNTY`
                               ,"2017.RBSA.PS.Mean"        = tableAF.MF.cast$`Mean_2017 RBSA PS`
                               ,"2017.RBSA.PS.SE"          = tableAF.MF.cast$`SE_2017 RBSA PS`
                               ,"2017.RBSA.PS.n"           = tableAF.MF.cast$`n_2017 RBSA PS`
                               ,"PSE_EB"                   = tableAF.MF.cast$EB_PSE
                               ,"PSE.King.County_EB"       = tableAF.MF.cast$`EB_PSE KING COUNTY`
                               ,"PSE.Non.King.County_EB"   = tableAF.MF.cast$`EB_PSE NON-KING COUNTY`
                               ,"2017.RBSA.PS_EB"          = tableAF.MF.cast$`EB_2017 RBSA PS`
)

levels(tableAF.MF.table$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"Unknown"
              ,"All Categories")
tableAF.MF.table <- tableAF.MF.table %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAF.MF.table <- data.frame(tableAF.MF.table)

tableAF.MF.final.MF <- tableAF.MF.table[which(tableAF.MF.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableAF.MF.table) %notin% c("BuildingType"))]

exportTable(tableAF.MF.final.MF, "MF", "Table AF", weighted = FALSE,OS = T, osIndicator = "PSE")
