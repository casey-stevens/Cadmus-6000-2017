#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 
rbsa.dat.site <- rbsa.dat[grep("SITE", rbsa.dat$CK_Building_ID),]
rbsa.dat.bldg <- rbsa.dat[grep("BLDG", rbsa.dat$CK_Building_ID),]
rbsa.dat.MF <- rbsa.dat[grep("Multifamily", rbsa.dat$BuildingType),]

#read in Envelope data for MF table
# envelope.dat <- read.xlsx(envelope.export)
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_SiteID"
                                                                  ,"Wall.Area"
                                                                  ,"Wall.Type"
                                                                  ,"Wall.Framing.Material"))]

envelope.dat2 <- left_join(rbsa.dat, envelope.dat1, by = c("CK_Building_ID" = "CK_SiteID"))
length(unique(envelope.dat2$CK_Cadmus_ID))
envelope.dat.MF <- envelope.dat2[grep("Multifamily", envelope.dat2$BuildingType),]

#############################################################################################
#Item 234: Table 26
#############################################################################################
envelope.dat.MF$Wall.Area <- as.numeric(as.character(envelope.dat.MF$Wall.Area))
item234.dat <- envelope.dat.MF[which(!is.na(envelope.dat.MF$Wall.Area)),]
item234.dat <- item234.dat[which(item234.dat$Wall.Type %notin% c("Knee Wall")),]
item234.dat <- item234.dat[grep("BLDG", item234.dat$CK_Building_ID),]
item234.dat <- item234.dat[which(item234.dat$Wall.Type %notin% c("N/A")),]
unique(item234.dat$Wall.Type)

item234.dat$WallType <- ""
item234.dat$WallType[grep("masonry", item234.dat$Wall.Type, ignore.case = T)] <- "Masonry"
item234.dat$WallType[grep("framed",  item234.dat$Wall.Type, ignore.case = T)] <- "Framed"
item234.dat$WallType[which(!(item234.dat$WallType %in% c("Masonry", "Framed")))] <- "Other"
item234.dat$WallType[which(item234.dat$WallType == "Framed")] <- paste(item234.dat$Wall.Framing.Material[which(item234.dat$WallType == "Framed")], "Frame", sep = " ")
unique(item234.dat$WallType)

item234.dat$WallType[which(item234.dat$WallType == "Unknown Frame")] <- "Other"

item234.merge <- left_join(rbsa.dat.MF, item234.dat)
item234.merge <- item234.merge[which(!is.na(item234.merge$WallType)),]

################################################
# Adding pop and sample sizes for weights
################################################
item234.data <- weightedData(item234.merge[-which(colnames(item234.merge) %in% c("Wall.Type"
                                                                                 ,"Wall.Area"
                                                                                 ,"Wall.Framing.Material"
                                                                                 ,"WallType"
                                                                                 ,"Category"))])
item234.data <- left_join(item234.data, item234.merge[which(colnames(item234.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"WallType"
                                                                                           ,"Wall.Area"
                                                                                           ,"Category"))])

item234.data$count <- 1
item234.data$Count <- 1

#######################
# Weighted Analysis
#######################
item234.final <- proportionRowsAndColumns1(CustomerLevelData = item234.data
                                           ,valueVariable    = 'Wall.Area'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'WallType'
                                           ,aggregateColumnName = "Remove")
item234.final <- item234.final[which(item234.final$Category != "Remove"),]

item234.cast <- dcast(setDT(item234.final)
                      ,formula = WallType ~ Category
                      ,value.var = c("w.percent","w.SE", "count","n","N","EB"))
names(item234.cast)
item234.final <- data.frame( "WallType"                = item234.cast$WallType
                             ,"PSE.Percent"                 = item234.cast$w.percent_PSE
                             ,"PSE.SE"                      = item234.cast$w.SE_PSE
                             ,"PSE.n"                       = item234.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item234.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item234.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item234.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item234.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item234.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item234.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item234.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item234.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"              = item234.cast$`n_2017 RBSA PS`
                             ,"PSE.EB"                      = item234.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item234.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item234.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item234.cast$`EB_2017 RBSA PS`
                             )
unique(item234.final$WallType)
rowOrder <- c("Wood Frame"
              ,"Masonry"
              ,"Other"
              ,"Total")
item234.final <- item234.final %>% mutate(WallType = factor(WallType, levels = rowOrder)) %>% arrange(WallType)  
item234.final <- data.frame(item234.final[which(names(item234.final) != "BuildingType")])


exportTable(item234.final, "MF", "Table 26", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# unweighted Analysis
#######################
item234.final <- proportions_two_groups_unweighted(CustomerLevelData = item234.data
                                           ,valueVariable    = 'Wall.Area'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'WallType'
                                           ,aggregateColumnName = "Remove")
item234.final <- item234.final[which(item234.final$Category != "Remove"),]

item234.cast <- dcast(setDT(item234.final)
                      ,formula = WallType ~ Category
                      ,value.var = c("Percent","SE", "Count","n"))

item234.final <- data.frame( "WallType"     = item234.cast$WallType
                             ,"PSE.Percent"                 = item234.cast$Percent_PSE
                             ,"PSE.SE"                      = item234.cast$SE_PSE
                             ,"PSE.n"                       = item234.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item234.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item234.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item234.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item234.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item234.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item234.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item234.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item234.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"              = item234.cast$`n_2017 RBSA PS`)
unique(item234.final$WallType)
rowOrder <- c("Wood Frame"
              ,"Masonry"
              ,"Other"
              ,"Total")
item234.final <- item234.final %>% mutate(WallType = factor(WallType, levels = rowOrder)) %>% arrange(WallType)  
item234.final <- data.frame(item234.final[which(names(item234.final) != "BuildingType")])

exportTable(item234.final, "MF", "Table 26", weighted = FALSE,OS = T, osIndicator = "PSE")
