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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
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
#Item 214: MF table 6
#############################################################################################
item214.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Qty.Buildings.in.Complex"))]
item214.dat$Qty.Buildings.in.Complex <- as.numeric(as.character(item214.dat$Qty.Buildings.in.Complex))

item214.dat1 <- item214.dat[which(!is.na(item214.dat$Qty.Buildings.in.Complex)),]

item214.dat1$Ind <- 0
item214.dat1$Ind[which(item214.dat1$Qty.Buildings.in.Complex > 1)] <- 1

item214.merge <- left_join(rbsa.dat.MF, item214.dat1)
item214.merge <- item214.merge[which(!is.na(item214.merge$Ind)),]


######################################
#Pop and Sample Sizes for weights
######################################
item214.data <- weightedData(item214.merge[which(colnames(item214.merge) %notin% c("Qty.Buildings.in.Complex"
                                                                                   ,"Ind"))])

item214.data <- left_join(item214.data, unique(item214.merge[which(colnames(item214.merge) %in% c("CK_Cadmus_ID"
                                                                                                  ,"CK_Building_ID"
                                                                                           ,"Qty.Buildings.in.Complex"
                                                                                           ,"Ind"))]))
item214.data$count <- 1
item214.data$Count <- 1

#########################
# weighted analysis
#########################
item214.final <- proportions_one_group(CustomerLevelData = item214.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes")
item214.final.MF <- item214.final[which(names(item214.final) != "BuildingType")]

exportTable(item214.final.MF, "MF","Table 6",weighted = TRUE)

#########################
# unweighted analysis
#########################
item214.final <- proportions_one_group(CustomerLevelData = item214.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes"
                                       ,weighted = FALSE)
item214.final.MF <- item214.final[which(names(item214.final) != "BuildingType")]

exportTable(item214.final.MF, "MF","Table 6",weighted = FALSE)



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
                                                                                   ,"Ind"))])

item215.data <- left_join(item215.data, unique(item215.merge[which(colnames(item215.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Qty.Buildings.in.Complex"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"Ind"))]))
item215.data$count <- 1
item215.data$Count <- item215.data$Total.Units.in.Building

#########################
# weighted analysis
#########################
item215.final <- proportions_one_group(CustomerLevelData = item215.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes")
item215.final.MF <- item215.final[which(names(item215.final) != "BuildingType")]

exportTable(item215.final.MF, "MF","Table 7",weighted = TRUE)

#########################
# unweighted analysis
#########################
item215.final <- proportions_one_group(CustomerLevelData = item215.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes"
                                       ,weighted = FALSE)
item215.final.MF <- item215.final[which(names(item215.final) != "BuildingType")]

exportTable(item215.final.MF, "MF","Table 7",weighted = FALSE)




#############################################################################################
#Item 222: MF table 14
#############################################################################################
item222.one.line.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                                   ,"Total.Units.in.Building"))]
item222.building.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                           ,"SITES_MFB_cfg_MFB_PARKING_ParkingStalls_Number"))]
names(item222.building.dat) <- c("Number.of.Stalls"
                                 ,"CK_Building_ID")

item222.dat <- left_join(item222.one.line.dat, item222.building.dat)
item222.dat$Number.of.Stalls <- as.numeric(as.character(item222.dat$Number.of.Stalls))
item222.dat$Total.Units.in.Building <- as.numeric(as.character(item222.dat$Total.Units.in.Building))

item222.dat1 <- item222.dat[which(!is.na(item222.dat$Number.of.Stalls)),]
item222.dat1 <- item222.dat1[which(!is.na(item222.dat1$Total.Units.in.Building)),]
item222.dat1 <- item222.dat1[which(item222.dat1$Total.Units.in.Building > 0),]

item222.dat1$Stalls.per.unit <- item222.dat1$Number.of.Stalls / item222.dat1$Total.Units.in.Building

item222.merge <- left_join(rbsa.dat.MF, item222.dat1)
item222.merge <- item222.merge[which(!is.na(item222.merge$Stalls.per.unit)),]


######################################
#Pop and Sample Sizes for weights
######################################
item222.data <- weightedData(item222.merge[which(colnames(item222.merge) %notin% c("Number.of.Stalls"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"Stalls.per.unit"))])

item222.data <- left_join(item222.data, item222.merge[which(colnames(item222.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Number.of.Stalls"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"Stalls.per.unit"))])
item222.data$count <- 1

#########################
# weighted analysis
#########################
item222.final <- mean_one_group(CustomerLevelData = item222.data
                                ,valueVariable = "Stalls.per.unit"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item222.final.MF <- item222.final[which(names(item222.final) != "BuildingType")]

exportTable(item222.final.MF, "MF", "Table 14", weighted = TRUE)

#########################
# unweighted analysis
#########################
item222.final <- mean_one_group_unweighted(CustomerLevelData = item222.data
                                ,valueVariable = "Stalls.per.unit"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item222.final.MF <- item222.final[which(names(item222.final) != "BuildingType")]

exportTable(item222.final.MF, "MF", "Table 14", weighted = FALSE)




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

######################################
#Pop and Sample Sizes for weights
######################################
item231.data <- weightedData(item231.merge[which(colnames(item231.merge) %notin% c("Window.Type"
                                                                                   ,"Window.Area"))])

item231.data <- left_join(item231.data, item231.merge[which(colnames(item231.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Window.Type"
                                                                                           ,"Window.Area"))])
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
                            ,"Percent.Metal.Other"                  = item231.cast$`w.percent_%.Metal.+.Other`
                            ,"SE.Metal.Other"                       = item231.cast$`w.SE_%.Metal.+.Other`
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
                            ,"EB.Metal.Other"                      = item231.cast$`EB_%.Metal.+.Other`
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
exportTable(item231.table.MF,"MF","Table 23",weighted = TRUE)


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
                            ,"Percent.Metal.Other"                 = item231.cast$`Percent_%.Metal.+.Other`
                            ,"SE.Metal.Other"                      = item231.cast$`SE_%.Metal.+.Other`
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
exportTable(item231.table.MF,"MF","Table 23",weighted = FALSE)







#############################################################################################
#Item 240: MF table 32
#############################################################################################
item240.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Whole.House.UA"
                                                                          ,"Total.Units.in.Building"))]

item240.dat$Whole.House.UA <- as.numeric(as.character(item240.dat$Whole.House.UA))
item240.dat$Total.Units.in.Building <- as.numeric(as.character(item240.dat$Total.Units.in.Building))

item240.dat1 <- item240.dat[which(!is.na(item240.dat$Whole.House.UA)),]
item240.dat1 <- item240.dat1[which(!is.na(item240.dat1$Total.Units.in.Building)),]
item240.dat1 <- item240.dat1[which(item240.dat1$Total.Units.in.Building > 0),]

item240.dat1$UA.per.unit <- item240.dat1$Whole.House.UA / item240.dat1$Total.Units.in.Building
item240.dat1 <- item240.dat1[which(item240.dat1$UA.per.unit > 0),]

item240.merge <- left_join(rbsa.dat.MF, item240.dat1)
item240.merge <- item240.merge[which(!is.na(item240.merge$UA.per.unit)),]


######################################
#Pop and Sample Sizes for weights
######################################
item240.data <- weightedData(item240.merge[which(colnames(item240.merge) %notin% c("Whole.House.UA"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"UA.per.unit"))])

item240.data <- left_join(item240.data, item240.merge[which(colnames(item240.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Whole.House.UA"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"UA.per.unit"))])
item240.data$count <- 1

#########################
# weighted analysis
#########################
item240.final <- mean_one_group(CustomerLevelData = item240.data
                                ,valueVariable = "UA.per.unit"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item240.final.MF <- item240.final[which(names(item240.final) != "BuildingType")]

exportTable(item240.final.MF, "MF", "Table 32", weighted = TRUE)

#########################
# unweighted analysis
#########################
item240.final <- mean_one_group_unweighted(CustomerLevelData = item240.data
                                           ,valueVariable = "UA.per.unit"
                                           ,byVariable = "HomeType"
                                           ,aggregateRow = "All Sizes")
item240.final.MF <- item240.final[which(names(item240.final) != "BuildingType")]

exportTable(item240.final.MF, "MF", "Table 32", weighted = FALSE)



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
                                                                                   ,"UA.per.unit"))])

item241.data <- left_join(item241.data, item241.merge[which(colnames(item241.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Whole.House.UA"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"UA.per.unit"))])
item241.data$count <- 1

#########################
# weighted analysis
#########################
item241.final <- mean_one_group(CustomerLevelData = item241.data
                                ,valueVariable = "UA.per.unit"
                                ,byVariable = "HomeYearBuilt_bins_MF"
                                ,aggregateRow = "All Vintages")

levels(item241.final$HomeYearBuilt_bins_MF)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item241.final <- item241.final %>% mutate(HomeYearBuilt_bins_MF = factor(HomeYearBuilt_bins_MF, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins_MF)  
item241.final <- data.frame(item241.final)

item241.final.MF <- item241.final[which(names(item241.final) != "BuildingType")]

exportTable(item241.final.MF, "MF", "Table 33", weighted = TRUE)

#########################
# unweighted analysis
#########################
item241.final <- mean_one_group_unweighted(CustomerLevelData = item241.data
                                           ,valueVariable = "UA.per.unit"
                                           ,byVariable = "HomeYearBuilt_bins_MF"
                                           ,aggregateRow = "All Vintages")

levels(item241.final$HomeYearBuilt_bins_MF)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item241.final <- item241.final %>% mutate(HomeYearBuilt_bins_MF = factor(HomeYearBuilt_bins_MF, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins_MF)  
item241.final <- data.frame(item241.final)

item241.final.MF <- item241.final[which(names(item241.final) != "BuildingType")]

exportTable(item241.final.MF, "MF", "Table 33", weighted = FALSE)



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
                                                                                   ,"Total.Conditioned.Area"))])

item242.data <- left_join(item242.data, item242.merge[which(colnames(item242.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Whole.House.UA"
                                                                                           ,"UA.per.area"
                                                                                           ,"Total.Conditioned.Area"))])
item242.data$count <- 1

#########################
# weighted analysis
#########################
item242.final <- mean_one_group(CustomerLevelData = item242.data
                                ,valueVariable = "UA.per.area"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item242.final.MF <- item242.final[which(names(item242.final) != "BuildingType")]

exportTable(item242.final.MF, "MF", "Table 34", weighted = TRUE)

#########################
# unweighted analysis
#########################
item242.final <- mean_one_group_unweighted(CustomerLevelData = item242.data
                                           ,valueVariable = "UA.per.area"
                                           ,byVariable = "HomeType"
                                           ,aggregateRow = "All Sizes")
item242.final.MF <- item242.final[which(names(item242.final) != "BuildingType")]

exportTable(item242.final.MF, "MF", "Table 34", weighted = FALSE)
