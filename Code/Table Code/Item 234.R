#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

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
rbsa.dat.MF <- rbsa.dat[grep("Multifamily", rbsa.dat$BuildingType),]

#read in Envelope data for MF table
envelope.dat <- read.xlsx(envelope.export)
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

item234.merge <- left_join(rbsa.dat, item234.dat)
item234.merge <- item234.merge[which(!is.na(item234.merge$WallType)),]

################################################
# Adding pop and sample sizes for weights
################################################
item234.data <- weightedData(item234.merge[-which(colnames(item234.merge) %in% c("Wall.Type"
                                                                                 ,"Wall.Area"
                                                                                 ,"Wall.Framing.Material"
                                                                                 ,"WallType"))])
item234.data <- left_join(item234.data, item234.merge[which(colnames(item234.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"WallType"
                                                                                           ,"Wall.Area"))])

item234.data$count <- 1
item234.data$Count <- 1

#######################
# Weighted Analysis
#######################
item234.final <- proportionRowsAndColumns1(CustomerLevelData = item234.data
                                           ,valueVariable    = 'Wall.Area'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'WallType'
                                           ,aggregateColumnName = "Remove")
item234.final <- item234.final[which(item234.final$HomeType != "Remove"),]
# item234.final <- item234.final[which(item234.final$WallType != "Total"),]


item234.all.vintages <- proportions_one_group(CustomerLevelData = item234.data
                                                 ,valueVariable = 'Wall.Area'
                                                 ,groupingVariable = 'WallType'
                                                 ,total.name = "All Sizes"
                                                 ,columnName = "HomeType"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)

item234.final <- rbind.data.frame(item234.final, item234.all.vintages, stringsAsFactors = F)

item234.cast <- dcast(setDT(item234.final)
                      ,formula = HomeType ~ WallType
                      ,value.var = c("w.percent","w.SE", "count","n","N","EB"))
names(item234.cast)
item234.final <- data.frame( "Building.Size"     = item234.cast$HomeType
                             ,"In-fill.Steel"    = NA
                             ,"In-fill.Steel.SE" = NA
                             ,"Masonry"          = item234.cast$w.percent_Masonry
                             ,"Masonry.SE"       = item234.cast$w.SE_Masonry
                             ,"Steel.Frame"      = NA#item234.cast$w.percent_Steel.Frame
                             ,"Steel.Frame.SE"   = NA#item234.cast$w.SE_Steel.Frame
                             ,"Wood"             = item234.cast$`w.percent_Wood Frame`
                             ,"Wood.SE"          = item234.cast$`w.SE_Wood Frame`
                             ,"Other"            = item234.cast$w.percent_Other
                             ,"Other.SE"         = item234.cast$w.SE_Other
                             ,"n"                = item234.cast$n_Total
                             ,"In-fill.Steel.EB" = NA
                             ,"Masonry.EB"       = item234.cast$EB_Masonry
                             ,"Steel.Frame.EB"   = NA#item234.cast$EB_Steel.Frame
                             ,"Wood.EB"          = item234.cast$`EB_Wood Frame`
                             ,"Other.EB"         = item234.cast$EB_Other)

exportTable(item234.final, "MF", "Table 26", weighted = TRUE)


#######################
# unweighted Analysis
#######################
item234.final <- proportions_two_groups_unweighted(CustomerLevelData = item234.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'WallType'
                                           ,aggregateColumnName = "Remove")
item234.final <- item234.final[which(item234.final$HomeType != "Remove"),]
# item234.final <- item234.final[which(item234.final$WallType != "Total"),]


item234.all.vintages <- proportions_one_group(CustomerLevelData = item234.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = 'WallType'
                                                 ,total.name = "All Sizes"
                                                 ,columnName = "HomeType"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE)

item234.final <- rbind.data.frame(item234.final, item234.all.vintages, stringsAsFactors = F)

item234.cast <- dcast(setDT(item234.final)
                      ,formula = HomeType ~ WallType
                      ,value.var = c("Percent","SE", "Count","n"))

item234.final <- data.frame( "Building.Size"     = item234.cast$HomeType
                             ,"In-fill.Steel"    = NA
                             ,"In-fill.Steel.SE" = NA
                             ,"Masonry"          = item234.cast$Percent_Masonry
                             ,"Masonry.SE"       = item234.cast$SE_Masonry
                             ,"Steel.Frame"      = NA#item234.cast$Percent_Steel.Frame
                             ,"Steel.Frame.SE"   = NA#item234.cast$SE_Steel.Frame
                             ,"Wood"             = item234.cast$`Percent_Wood Frame`
                             ,"Wood.SE"          = item234.cast$`SE_Wood Frame`
                             ,"Other"            = item234.cast$Percent_Other
                             ,"Other.SE"         = item234.cast$SE_Other
                             ,"n"                = item234.cast$n_Total)

exportTable(item234.final, "MF", "Table 26", weighted = FALSE)
