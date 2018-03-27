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
rbsa.dat.site <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID,ignore.case = T),]
rbsa.dat.site.MF   <- rbsa.dat.site[grep("multifamily",rbsa.dat.site$BuildingType, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID,ignore.case = T),]
rbsa.dat.MF   <- rbsa.dat.bldg[grep("multifamily",rbsa.dat.bldg$BuildingType, ignore.case = T),]

#Read in data for analysis
# one.line.bldg.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), startRow = 2)
#clean cadmus IDs
one.line.bldg.dat$CK_Building_ID <- trimws(toupper(one.line.bldg.dat$PK_BuildingID))

#Read in data for analysis
# buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))



#############################################################################################
#Item 214A: MF table 6
#############################################################################################
item214A.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Qty.Buildings.in.Complex"))]
item214A.dat$Qty.Buildings.in.Complex <- as.numeric(as.character(item214A.dat$Qty.Buildings.in.Complex))

item214A.dat1 <- item214A.dat[which(!is.na(item214A.dat$Qty.Buildings.in.Complex)),]

item214A.dat1$Ind <- 0
item214A.dat1$Ind[which(item214A.dat1$Qty.Buildings.in.Complex > 1)] <- 1

item214A.merge <- left_join(rbsa.dat.MF, item214A.dat1)
item214A.merge <- item214A.merge[which(!is.na(item214A.merge$Ind)),]

item214A.merge <- item214A.merge[which(item214A.merge$Category == "PSE"),]
######################################
#Pop and Sample Sizes for weights
######################################
item214A.data <- weightedData(item214A.merge[which(colnames(item214A.merge) %notin% c("Qty.Buildings.in.Complex"
                                                                                   ,"Ind"
                                                                                   ,"Category"))])

item214A.data <- left_join(item214A.data, unique(item214A.merge[which(colnames(item214A.merge) %in% c("CK_Cadmus_ID"
                                                                                                  ,"CK_Building_ID"
                                                                                           ,"Qty.Buildings.in.Complex"
                                                                                           ,"Ind"
                                                                                           ,"Category"))]))
item214A.data$count <- 1
item214A.data$Count <- 1

#########################
# weighted analysis
#########################
item214A.final <- proportions_one_group(CustomerLevelData = item214A.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes")
item214A.final.MF <- item214A.final[which(names(item214A.final) != "BuildingType")]

exportTable(item214A.final.MF, "MF","Table 6A",weighted = TRUE, OS = T, osIndicator = "PSE")

#########################
# unweighted analysis
#########################
item214A.final <- proportions_one_group(CustomerLevelData = item214A.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes"
                                       ,weighted = FALSE)
item214A.final.MF <- item214A.final[which(names(item214A.final) != "BuildingType")]

exportTable(item214A.final.MF, "MF","Table 6A",weighted = FALSE, OS = T, osIndicator = "PSE")
#############################################################################################
#Item 214B: MF table 6
#############################################################################################
item214B.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Qty.Buildings.in.Complex"))]
item214B.dat$Qty.Buildings.in.Complex <- as.numeric(as.character(item214B.dat$Qty.Buildings.in.Complex))

item214B.dat1 <- item214B.dat[which(!is.na(item214B.dat$Qty.Buildings.in.Complex)),]

item214B.dat1$Ind <- 0
item214B.dat1$Ind[which(item214B.dat1$Qty.Buildings.in.Complex > 1)] <- 1

item214B.merge <- left_join(rbsa.dat.MF, item214B.dat1)
item214B.merge <- item214B.merge[which(!is.na(item214B.merge$Ind)),]


######################################
#Pop and Sample Sizes for weights
######################################
item214B.data <- weightedData(item214B.merge[which(colnames(item214B.merge) %notin% c("Qty.Buildings.in.Complex"
                                                                                   ,"Ind"
                                                                                   ,"Category"))])

item214B.data <- left_join(item214B.data, unique(item214B.merge[which(colnames(item214B.merge) %in% c("CK_Cadmus_ID"
                                                                                                  ,"CK_Building_ID"
                                                                                                  ,"Qty.Buildings.in.Complex"
                                                                                                  ,"Ind"
                                                                                                  ,"Category"))]))
item214B.data$count <- 1
item214B.data$Count <- 1

#########################
# weighted analysis
#########################
item214B.data$State <- item214B.data$Category
item214B.final <- proportions_one_group(CustomerLevelData = item214B.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "State"
                                       ,total.name = "Remove")
item214B.final.MF <- item214B.final[which(item214B.final$State != "Total"), which(names(item214B.final) != "BuildingType")]

exportTable(item214B.final.MF, "MF","Table 6B",weighted = TRUE, OS = T, osIndicator = "PSE")

#########################
# unweighted analysis
#########################
item214B.final <- proportions_one_group(CustomerLevelData = item214B.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "State"
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
item214B.final.MF <- item214B.final[which(item214B.final$State != "Total"), which(names(item214B.final) != "BuildingType")]

exportTable(item214B.final.MF, "MF","Table 6B",weighted = FALSE, OS = T, osIndicator = "PSE")



#############################################################################################
#Item 215: MF table 7
#############################################################################################
item215.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Qty.Buildings.in.Complex"
                                                                          ,"Total.Units.in.Building"))]
item215.dat$Qty.Buildings.in.Complex <- as.numeric(as.character(item215.dat$Qty.Buildings.in.Complex))
item215.dat$Total.Units.in.Building <- as.numeric(as.character(item215.dat$Total.Units.in.Building))

item215.dat1 <- item215.dat[which(!is.na(item215.dat$Qty.Buildings.in.Complex)),]
item215.dat1 <- item215.dat1[which(!is.na(item215.dat1$Total.Units.in.Building)),]

item215.dat1$Ind <- 0
item215.dat1$Ind[which(item215.dat1$Qty.Buildings.in.Complex > 1)] <- item215.dat1$Total.Units.in.Building[which(item215.dat1$Qty.Buildings.in.Complex > 1)]

item215.merge <- left_join(rbsa.dat.MF, item215.dat1)
item215.merge <- item215.merge[which(!is.na(item215.merge$Ind)),]


######################################
#Pop and Sample Sizes for weights
######################################
item215.data <- weightedData(item215.merge[which(colnames(item215.merge) %notin% c("Qty.Buildings.in.Complex"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"Ind"
                                                                                   ,"Category"))])

item215.data <- left_join(item215.data, unique(item215.merge[which(colnames(item215.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Qty.Buildings.in.Complex"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"Ind"
                                                                                           ,"Category"))]))
item215.data$count <- 1
item215.data$Count <- item215.data$Total.Units.in.Building

#########################
# weighted analysis
#########################
item215.data$State <- item215.data$Category
item215.final <- proportions_one_group(CustomerLevelData = item215.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "State"
                                       ,total.name = "All Sizes")
item215.final.MF <- item215.final[which(item215.final$State != "Total"), which(names(item215.final) != "BuildingType")]

exportTable(item215.final.MF, "MF","Table 7",weighted = TRUE, OS = T, osIndicator = "PSE")

#########################
# unweighted analysis
#########################
item215.final <- proportions_one_group(CustomerLevelData = item215.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "State"
                                       ,total.name = "All Sizes"
                                       ,weighted = FALSE)
item215.final.MF <- item215.final[which(item215.final$State != "Total"), which(names(item215.final) != "BuildingType")]

exportTable(item215.final.MF, "MF","Table 7",weighted = FALSE, OS = T, osIndicator = "PSE")




#############################################################################################
#Item 222A: MF table 14
#############################################################################################
item222A.one.line.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                                   ,"Total.Units.in.Building"))]
item222A.building.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                           ,"SITES_MFB_cfg_MFB_PARKING_ParkingStalls_Number"))]
names(item222A.building.dat) <- c("Number.of.Stalls"
                                 ,"CK_Building_ID")

item222A.dat <- left_join(item222A.one.line.dat, item222A.building.dat)
item222A.dat$Number.of.Stalls <- as.numeric(as.character(item222A.dat$Number.of.Stalls))
item222A.dat$Total.Units.in.Building <- as.numeric(as.character(item222A.dat$Total.Units.in.Building))

item222A.dat1 <- item222A.dat[which(!is.na(item222A.dat$Number.of.Stalls)),]
item222A.dat1 <- item222A.dat1[which(!is.na(item222A.dat1$Total.Units.in.Building)),]
item222A.dat1 <- item222A.dat1[which(item222A.dat1$Total.Units.in.Building > 0),]

item222A.dat1$Stalls.per.unit <- item222A.dat1$Number.of.Stalls / item222A.dat1$Total.Units.in.Building

item222A.merge <- left_join(rbsa.dat.MF, item222A.dat1)
item222A.merge <- item222A.merge[which(!is.na(item222A.merge$Stalls.per.unit)),]

item222A.merge <- item222A.merge[which(item222A.merge$Category == "PSE"),]
######################################
#Pop and Sample Sizes for weights
######################################
item222A.data <- weightedData(item222A.merge[which(colnames(item222A.merge) %notin% c("Number.of.Stalls"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"Stalls.per.unit"
                                                                                   ,"Category"))])

item222A.data <- left_join(item222A.data, item222A.merge[which(colnames(item222A.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Number.of.Stalls"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"Stalls.per.unit"
                                                                                           ,"Category"))])
item222A.data$count <- 1

#########################
# weighted analysis
#########################
item222A.final <- mean_one_group(CustomerLevelData = item222A.data
                                ,valueVariable = "Stalls.per.unit"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item222A.final.MF <- item222A.final[which(names(item222A.final) != "BuildingType")]

exportTable(item222A.final.MF, "MF", "Table 14A", weighted = TRUE, OS = T, osIndicator = "PSE")

#########################
# unweighted analysis
#########################
item222A.final <- mean_one_group_unweighted(CustomerLevelData = item222A.data
                                ,valueVariable = "Stalls.per.unit"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item222A.final.MF <- item222A.final[which(names(item222A.final) != "BuildingType")]

exportTable(item222A.final.MF, "MF", "Table 14A", weighted = FALSE, OS = T, osIndicator = "PSE")
#############################################################################################
#Item 222B: MF table 14
#############################################################################################
item222B.one.line.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                                   ,"Total.Units.in.Building"))]
item222B.building.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                           ,"SITES_MFB_cfg_MFB_PARKING_ParkingStalls_Number"))]
names(item222B.building.dat) <- c("Number.of.Stalls"
                                 ,"CK_Building_ID")

item222B.dat <- left_join(item222B.one.line.dat, item222B.building.dat)
item222B.dat$Number.of.Stalls <- as.numeric(as.character(item222B.dat$Number.of.Stalls))
item222B.dat$Total.Units.in.Building <- as.numeric(as.character(item222B.dat$Total.Units.in.Building))

item222B.dat1 <- item222B.dat[which(!is.na(item222B.dat$Number.of.Stalls)),]
item222B.dat1 <- item222B.dat1[which(!is.na(item222B.dat1$Total.Units.in.Building)),]
item222B.dat1 <- item222B.dat1[which(item222B.dat1$Total.Units.in.Building > 0),]

item222B.dat1$Stalls.per.unit <- item222B.dat1$Number.of.Stalls / item222B.dat1$Total.Units.in.Building

item222B.merge <- left_join(rbsa.dat.MF, item222B.dat1)
item222B.merge <- item222B.merge[which(!is.na(item222B.merge$Stalls.per.unit)),]


######################################
#Pop and Sample Sizes for weights
######################################
item222B.data <- weightedData(item222B.merge[which(colnames(item222B.merge) %notin% c("Number.of.Stalls"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"Stalls.per.unit"
                                                                                   ,"Category"))])

item222B.data <- left_join(item222B.data, item222B.merge[which(colnames(item222B.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Number.of.Stalls"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"Stalls.per.unit"
                                                                                           ,"Category"))])
item222B.data$count <- 1

#########################
# weighted analysis
#########################
item222B.final <- mean_one_group(CustomerLevelData = item222B.data
                                ,valueVariable = "Stalls.per.unit"
                                ,byVariable = "Category"
                                ,aggregateRow = "All Sizes")
item222B.final.MF <- item222B.final[which(item222B.final$Category != "All Sizes"), which(names(item222B.final) != "BuildingType")]

exportTable(item222B.final.MF, "MF", "Table 14B", weighted = TRUE, OS = T, osIndicator = "PSE")

#########################
# unweighted analysis
#########################
item222B.final <- mean_one_group_unweighted(CustomerLevelData = item222B.data
                                           ,valueVariable = "Stalls.per.unit"
                                           ,byVariable = "Category"
                                           ,aggregateRow = "All Sizes")
item222B.final.MF <- item222B.final[which(item222B.final$Category != "All Sizes"), which(names(item222B.final) != "BuildingType")]

exportTable(item222B.final.MF, "MF", "Table 14B", weighted = FALSE, OS = T, osIndicator = "PSE")




#############################################################################################
#Item 231: MF Table 23
#############################################################################################
item231.dat <- one.line.bldg.dat[c(grep("%",names(one.line.bldg.dat)),
                                   which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID","Total.Window.Area")))]
for (i in 1:13){
  item231.dat[,i] <- as.numeric(as.character(item231.dat[,i]))
}
item231.dat[is.na(item231.dat)] <- 0

i = 2
for (i in 1:12){
  item231.dat[,i] <- item231.dat[,i] * item231.dat[,13]
}

item231.dat <- item231.dat[which(names(item231.dat) != "Total.Window.Area")]

item231.melt <- melt(item231.dat, id.vars = "CK_Building_ID")
item231.melt <- item231.melt[which(item231.melt$value > 0),]
names(item231.melt) <- c("CK_Building_ID","Window.Type","Window.Area")

item231.merge <- left_join(rbsa.dat.MF, item231.melt)
item231.merge <- item231.merge[which(!is.na(item231.merge$Window.Area)),]
item231.merge <- item231.merge[which(!is.na(item231.merge$HomeYearBuilt_MF)),]
item231.merge <- item231.merge[which(item231.merge$Category == "PSE"),]
######################################
#Pop and Sample Sizes for weights
######################################
item231.data <- weightedData(item231.merge[which(colnames(item231.merge) %notin% c("Window.Type"
                                                                                   ,"Window.Area"
                                                                                   ,"Category"))])

item231.data <- left_join(item231.data, item231.merge[which(colnames(item231.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Window.Type"
                                                                                           ,"Window.Area"
                                                                                           ,"Category"))])
item231.data$count <- 1
#########################
# weighted analysis
#########################
item231.summary <- proportionRowsAndColumns1(CustomerLevelData = item231.data
                                             ,valueVariable = 'Window.Area'
                                             ,columnVariable = 'HomeYearBuilt_bins_MF'
                                             ,rowVariable = 'Window.Type'
                                             ,aggregateColumnName = 'Remove')
item231.summary <- item231.summary[which(item231.summary$HomeYearBuilt_bins_MF != "Remove"),]

item231.all.vintages <- proportions_one_group(CustomerLevelData = item231.data
                                              ,valueVariable = 'Window.Area'
                                              ,groupingVariable = 'Window.Type'
                                              ,total.name = 'All Vintages'
                                              ,columnName = "HomeYearBuilt_bins_MF"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)

item231.final <- rbind.data.frame(item231.summary, item231.all.vintages)

item231.cast <- dcast(setDT(item231.final)
                      ,formula = BuildingType + HomeYearBuilt_bins_MF ~ Window.Type
                      ,value.var = c("w.percent","w.SE","count","N","n","EB"))
names(item231.cast)
item231.table <- data.frame("BuildingType"                          = item231.cast$BuildingType
                            ,"Housing.Vintage"                      = item231.cast$HomeYearBuilt_bins_MF
                            ,"Percent.Metal.Single"                 = item231.cast$`w.percent_%.Metal.+.Single`
                            ,"SE.Metal.Single"                      = item231.cast$`w.SE_%.Metal.+.Single`
                            ,"Percent.Metal.Double"                 = item231.cast$`w.percent_%.Metal.+.Double`
                            ,"SE.Metal.Double"                      = item231.cast$`w.SE_%.Metal.+.Double`
                            # ,"Percent.Metal.Other"                  = item231.cast$`w.percent_%.Metal.+.Other`
                            # ,"SE.Metal.Other"                       = item231.cast$`w.SE_%.Metal.+.Other`
                            ,"Percent.Wood.Vinyl.Fiberglass.Single" = item231.cast$`w.percent_%.Wood,.Vinyl,.or.Fiberglass.+.Single`
                            ,"SE.Wood.Vinyl.Fiberglass.Single"      = item231.cast$`w.SE_%.Wood,.Vinyl,.or.Fiberglass.+.Single`
                            ,"Percent.Wood.Vinyl.Fiberglass.Double" = item231.cast$`w.percent_%.Wood,.Vinyl,.or.Fiberglass.+.Double`
                            ,"SE.Wood.Vinyl.Fiberglass.Double"      = item231.cast$`w.SE_%.Wood,.Vinyl,.or.Fiberglass.+.Double`
                            ,"Percent.Wood.Vinyl.Fiberglass.Other"  = item231.cast$`w.percent_%.Wood,.Vinyl,.or.Fiberglass.+.Other`
                            ,"SE.Wood.Vinyl.Fiberglass.Other"       = item231.cast$`w.SE_%.Wood,.Vinyl,.or.Fiberglass.+.Other`
                            # ,"Percent.Other.Double"                 = item231.cast$`w.percent_%.Other.+.Double`
                            # ,"SE.Other.Double"                      = item231.cast$`w.SE_%.Other.+.Double`
                            # ,"Percent.Other.Other"                  = item231.cast$`w.percent_%.Other.+.Other`
                            # ,"SE.Other.Other"                       = item231.cast$`w.SE_%.Other.+.Other`
                            ,"n"                                    = item231.cast$n_Total
                            ,"EB.Metal.Single"                      = item231.cast$`EB_%.Metal.+.Single`
                            ,"EB.Metal.Double"                      = item231.cast$`EB_%.Metal.+.Double`
                            # ,"EB.Metal.Other"                      = item231.cast$`EB_%.Metal.+.Other`
                            ,"EB.Wood.Vinyl.Fiberglass.Single"      = item231.cast$`EB_%.Wood,.Vinyl,.or.Fiberglass.+.Single`
                            ,"EB.Wood.Vinyl.Fiberglass.Double"      = item231.cast$`EB_%.Wood,.Vinyl,.or.Fiberglass.+.Double`
                            ,"EB.Wood.Vinyl.Fiberglass.Other"       = item231.cast$`EB_%.Wood,.Vinyl,.or.Fiberglass.+.Other`
                            # ,"EB.Other.Double"                      = item231.cast$`EB_%.Other.+.Double`
                            # ,"EB.Other.Other"                       = item231.cast$`EB_%.Other.+.Other`
                            )
levels(item231.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item231.table <- item231.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item231.table <- data.frame(item231.table)

item231.table.MF <- item231.table[which(names(item231.table) != "BuildingType")]
exportTable(item231.table.MF,"MF","Table 23",weighted = TRUE, OS = T, osIndicator = "PSE")


#########################
# unweighted analysis
#########################
item231.summary <- proportions_two_groups_unweighted(CustomerLevelData = item231.data
                                             ,valueVariable = 'Window.Area'
                                             ,columnVariable = 'HomeYearBuilt_bins_MF'
                                             ,rowVariable = 'Window.Type'
                                             ,aggregateColumnName = 'Remove')
item231.summary <- item231.summary[which(item231.summary$HomeYearBuilt_bins_MF != "Remove"),]

item231.all.vintages <- proportions_one_group(CustomerLevelData = item231.data
                                              ,valueVariable = 'Window.Area'
                                              ,groupingVariable = 'Window.Type'
                                              ,total.name = 'All Vintages'
                                              ,columnName = "HomeYearBuilt_bins_MF"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)

item231.final <- rbind.data.frame(item231.summary, item231.all.vintages)

item231.cast <- dcast(setDT(item231.final)
                      ,formula = BuildingType + HomeYearBuilt_bins_MF ~ Window.Type
                      ,value.var = c("Percent","SE","Count","n"))

item231.table <- data.frame("BuildingType"                          = item231.cast$BuildingType
                            ,"Housing.Vintage"                      = item231.cast$HomeYearBuilt_bins_MF
                            ,"Percent.Metal.Single"                 = item231.cast$`Percent_%.Metal.+.Single`
                            ,"SE.Metal.Single"                      = item231.cast$`SE_%.Metal.+.Single`
                            ,"Percent.Metal.Double"                 = item231.cast$`Percent_%.Metal.+.Double`
                            ,"SE.Metal.Double"                      = item231.cast$`SE_%.Metal.+.Double`
                            # ,"Percent.Metal.Other"                 = item231.cast$`Percent_%.Metal.+.Other`
                            # ,"SE.Metal.Other"                      = item231.cast$`SE_%.Metal.+.Other`
                            ,"Percent.Wood.Vinyl.Fiberglass.Single" = item231.cast$`Percent_%.Wood,.Vinyl,.or.Fiberglass.+.Single`
                            ,"SE.Wood.Vinyl.Fiberglass.Single"      = item231.cast$`SE_%.Wood,.Vinyl,.or.Fiberglass.+.Single`
                            ,"Percent.Wood.Vinyl.Fiberglass.Double" = item231.cast$`Percent_%.Wood,.Vinyl,.or.Fiberglass.+.Double`
                            ,"SE.Wood.Vinyl.Fiberglass.Double"      = item231.cast$`SE_%.Wood,.Vinyl,.or.Fiberglass.+.Double`
                            ,"Percent.Wood.Vinyl.Fiberglass.Other"  = item231.cast$`Percent_%.Wood,.Vinyl,.or.Fiberglass.+.Other`
                            ,"SE.Wood.Vinyl.Fiberglass.Other"       = item231.cast$`SE_%.Wood,.Vinyl,.or.Fiberglass.+.Other`
                            # ,"Percent.Other.Double"                 = item231.cast$`Percent_%.Other.+.Double`
                            # ,"SE.Other.Double"                      = item231.cast$`SE_%.Other.+.Double`
                            # ,"Percent.Other.Other"                  = item231.cast$`Percent_%.Other.+.Other`
                            # ,"SE.Other.Other"                       = item231.cast$`SE_%.Other.+.Other`
                            ,"n"                                    = item231.cast$n_Total)
levels(item231.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item231.table <- item231.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item231.table <- data.frame(item231.table)

item231.table.MF <- item231.table[which(names(item231.table) != "BuildingType")]
exportTable(item231.table.MF,"MF","Table 23",weighted = FALSE, OS = T, osIndicator = "PSE")







# #############################################################################################
# #Item 240: MF table 32
# #############################################################################################
# item240.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
#                                                                           ,"Whole.House.UA"
#                                                                           ,"Total.Units.in.Building"))]
# 
# item240.dat$Whole.House.UA <- as.numeric(as.character(item240.dat$Whole.House.UA))
# item240.dat$Total.Units.in.Building <- as.numeric(as.character(item240.dat$Total.Units.in.Building))
# 
# item240.dat1 <- item240.dat[which(!is.na(item240.dat$Whole.House.UA)),]
# item240.dat1 <- item240.dat1[which(!is.na(item240.dat1$Total.Units.in.Building)),]
# item240.dat1 <- item240.dat1[which(item240.dat1$Total.Units.in.Building > 0),]
# 
# item240.dat1$UA.per.unit <- item240.dat1$Whole.House.UA / item240.dat1$Total.Units.in.Building
# item240.dat1 <- item240.dat1[which(item240.dat1$UA.per.unit > 0),]
# 
# item240.merge <- left_join(rbsa.dat.MF, item240.dat1)
# item240.merge <- item240.merge[which(!is.na(item240.merge$UA.per.unit)),]
# 
# 
# ######################################
# #Pop and Sample Sizes for weights
# ######################################
# item240.data <- weightedData(item240.merge[which(colnames(item240.merge) %notin% c("Whole.House.UA"
#                                                                                    ,"Total.Units.in.Building"
#                                                                                    ,"UA.per.unit"
#                                                                                    ,"Category"))])
# 
# item240.data <- left_join(item240.data, item240.merge[which(colnames(item240.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Whole.House.UA"
#                                                                                            ,"Total.Units.in.Building"
#                                                                                            ,"UA.per.unit"
#                                                                                            ,"Category"))])
# item240.data$count <- 1
# 
# #########################
# # weighted analysis
# #########################
# item240.final <- mean_one_group(CustomerLevelData = item240.data
#                                 ,valueVariable = "UA.per.unit"
#                                 ,byVariable = "HomeType"
#                                 ,aggregateRow = "All Sizes")
# item240.final.MF <- item240.final[which(names(item240.final) != "BuildingType")]
# 
# exportTable(item240.final.MF, "MF", "Table 32", weighted = TRUE, OS = T, osIndicator = "PSE")
# 
# #########################
# # unweighted analysis
# #########################
# item240.final <- mean_one_group_unweighted(CustomerLevelData = item240.data
#                                            ,valueVariable = "UA.per.unit"
#                                            ,byVariable = "HomeType"
#                                            ,aggregateRow = "All Sizes")
# item240.final.MF <- item240.final[which(names(item240.final) != "BuildingType")]
# 
# exportTable(item240.final.MF, "MF", "Table 32", weighted = FALSE, OS = T, osIndicator = "PSE")



#############################################################################################
#Item 241: MF table 33
#############################################################################################
item241.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Whole.House.UA"
                                                                          ,"Total.Units.in.Building"))]

item241.dat$Whole.House.UA <- as.numeric(as.character(item241.dat$Whole.House.UA))
item241.dat$Total.Units.in.Building <- as.numeric(as.character(item241.dat$Total.Units.in.Building))

item241.dat1 <- item241.dat[which(!is.na(item241.dat$Whole.House.UA)),]
item241.dat1 <- item241.dat1[which(!is.na(item241.dat1$Total.Units.in.Building)),]
item241.dat1 <- item241.dat1[which(item241.dat1$Total.Units.in.Building > 0),]

item241.dat1$UA.per.unit <- item241.dat1$Whole.House.UA / item241.dat1$Total.Units.in.Building
item241.dat1 <- item241.dat1[which(item241.dat1$UA.per.unit > 0),]

item241.merge <- left_join(rbsa.dat.MF, item241.dat1)
item241.merge <- item241.merge[which(!is.na(item241.merge$UA.per.unit)),]
item241.merge <- item241.merge[which(!is.na(item241.merge$HomeYearBuilt)),]

######################################
#Pop and Sample Sizes for weights
######################################
item241.data <- weightedData(item241.merge[which(colnames(item241.merge) %notin% c("Whole.House.UA"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"UA.per.unit"
                                                                                   ,"Category"))])

item241.data <- left_join(item241.data, item241.merge[which(colnames(item241.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Whole.House.UA"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"UA.per.unit"
                                                                                           ,"Category"))])
item241.data$count <- 1

#########################
# weighted analysis
#########################
item241.summary <- mean_two_groups(CustomerLevelData = item241.data
                                   ,valueVariable = "UA.per.unit"
                                   ,byVariableRow = "HomeYearBuilt_bins_MF"
                                   ,byVariableColumn = "Category"
                                   ,columnAggregate = "Remove"
                                   ,rowAggregate = "All Vintages")

item241.final <- data.frame("BuildingType" = item241.summary$BuildingType
                            ,"Housing.Vintage" = item241.summary$HomeYearBuilt_bins_MF
                            ,"Mean.PSE" = item241.summary$Mean_PSE
                            ,"SE.PSE" = item241.summary$SE_PSE
                            ,"n.PSE" = item241.summary$n_PSE
                            ,"Mean.PSE.King.County" = item241.summary$`Mean_PSE KING COUNTY`
                            ,"SE.PSE.King.County" = item241.summary$`SE_PSE KING COUNTY`
                            ,"n.PSE.King.County" = item241.summary$`n_PSE KING COUNTY`
                            ,"Mean.PSE.Non.King.County" = item241.summary$`Mean_PSE NON-KING COUNTY`
                            ,"SE.PSE.Non.King.County" = item241.summary$`SE_PSE NON-KING COUNTY`
                            ,"n.PSE.Non.King.County" = item241.summary$`n_PSE NON-KING COUNTY`
                            ,"Mean.2017.RBSA.PS" = item241.summary$`Mean_2017 RBSA PS`
                            ,"SE.2017.RBSA.PS" = item241.summary$`SE_2017 RBSA PS`
                            ,"n.2017.RBSA.PS" = item241.summary$`n_2017 RBSA PS`
                            ,"EB.PSE" = item241.summary$EB_PSE
                            ,"EB.PSE.King.County" = item241.summary$`EB_PSE KING COUNTY`
                            ,"EB.PSE.Non.King.County" = item241.summary$`EB_PSE NON-KING COUNTY`
                            ,"EB.2017.RBSA.PS" = item241.summary$`EB_2017 RBSA PS`
                            )

levels(item241.final$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item241.final <- item241.final %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item241.final <- data.frame(item241.final)

item241.final.MF <- item241.final[which(names(item241.final) != "BuildingType")]

exportTable(item241.final.MF, "MF", "Table 33", weighted = TRUE, OS = T, osIndicator = "PSE")

#########################
# unweighted analysis
#########################
item241.summary <- mean_two_groups_unweighted(CustomerLevelData = item241.data
                                   ,valueVariable = "UA.per.unit"
                                   ,byVariableRow = "HomeYearBuilt_bins_MF"
                                   ,byVariableColumn = "Category"
                                   ,columnAggregate = "Remove"
                                   ,rowAggregate = "All Vintages")

item241.final <- data.frame("BuildingType" = item241.summary$BuildingType
                            ,"Housing.Vintage" = item241.summary$HomeYearBuilt_bins_MF
                            ,"Mean.PSE" = item241.summary$Mean_PSE
                            ,"SE.PSE" = item241.summary$SE_PSE
                            ,"n.PSE" = item241.summary$n_PSE
                            ,"Mean.PSE.King.County" = item241.summary$`Mean_PSE KING COUNTY`
                            ,"SE.PSE.King.County" = item241.summary$`SE_PSE KING COUNTY`
                            ,"n.PSE.King.County" = item241.summary$`n_PSE KING COUNTY`
                            ,"Mean.PSE.Non.King.County" = item241.summary$`Mean_PSE NON-KING COUNTY`
                            ,"SE.PSE.Non.King.County" = item241.summary$`SE_PSE NON-KING COUNTY`
                            ,"n.PSE.Non.King.County" = item241.summary$`n_PSE NON-KING COUNTY`
                            ,"Mean.2017.RBSA.PS" = item241.summary$`Mean_2017 RBSA PS`
                            ,"SE.2017.RBSA.PS" = item241.summary$`SE_2017 RBSA PS`
                            ,"n.2017.RBSA.PS" = item241.summary$`n_2017 RBSA PS`
)

levels(item241.final$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item241.final <- item241.final %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item241.final <- data.frame(item241.final)

item241.final.MF <- item241.final[which(names(item241.final) != "BuildingType")]

exportTable(item241.final.MF, "MF", "Table 33", weighted = FALSE, OS = T, osIndicator = "PSE")



#############################################################################################
#Item 242: MF table 34
#############################################################################################
item242.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Whole.House.UA"
                                                                          ,"Total.Conditioned.Area"))]

item242.dat$Whole.House.UA <- as.numeric(as.character(item242.dat$Whole.House.UA))

item242.dat0 <- item242.dat[which(item242.dat$Total.Conditioned.Area > 0),]

item242.dat1 <- item242.dat0[which(!is.na(item242.dat0$Whole.House.UA)),]
item242.dat1$UA.per.area <- item242.dat1$Whole.House.UA / as.numeric(item242.dat1$Total.Conditioned.Area)

item242.merge <- left_join(rbsa.dat.MF, item242.dat1, by = "CK_Building_ID")
item242.merge <- item242.merge[which(!is.na(item242.merge$UA.per.area)),]


######################################
#Pop and Sample Sizes for weights
######################################
item242.data <- weightedData(item242.merge[which(colnames(item242.merge) %notin% c("Whole.House.UA"
                                                                                   ,"UA.per.area"
                                                                                   ,"Total.Conditioned.Area"
                                                                                   ,"Category"))])

item242.data <- left_join(item242.data, item242.merge[which(colnames(item242.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Whole.House.UA"
                                                                                           ,"UA.per.area"
                                                                                           ,"Total.Conditioned.Area"
                                                                                           ,"Category"))])
item242.data$count <- 1

#########################
# weighted analysis
#########################
item242.final <- mean_one_group(CustomerLevelData = item242.data
                                ,valueVariable = "UA.per.area"
                                ,byVariable = "Category"
                                ,aggregateRow = "Remove")
item242.final.MF <- item242.final[which(item242.final$Category != "Remove"),which(names(item242.final) != "BuildingType")]

exportTable(item242.final.MF, "MF", "Table 34", weighted = TRUE, OS = T, osIndicator = "PSE")

#########################
# unweighted analysis
#########################
item242.final <- mean_one_group_unweighted(CustomerLevelData = item242.data
                                           ,valueVariable = "UA.per.area"
                                           ,byVariable = "Category"
                                           ,aggregateRow = "Remove")
item242.final.MF <- item242.final[which(item242.final$Category != "Remove"),which(names(item242.final) != "BuildingType")]

exportTable(item242.final.MF, "MF", "Table 34", weighted = FALSE, OS = T, osIndicator = "PSE")
