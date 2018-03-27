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
rbsa.dat.site <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
# buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))
length(unique(buildings.dat$CK_Building_ID))
buildings.dat.clean <- buildings.dat[which(!duplicated(buildings.dat$CK_Building_ID)),]

#Read in data for analysis
# buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Building_ID <- trimws(toupper(buildings.interview.dat$CK_BuildingID))



#############################################################################################
# Item 225: DISTRIBUTION OF OWNERSHIP TYPE BY BUILDING SIZE (MF table 17)
#############################################################################################
item225.dat <- buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Building_ID"
                                                                  ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership"))]

colnames(item225.dat) <- c("Ownership", "CK_Building_ID")

item225.dat0 <- item225.dat[which(item225.dat$Ownership %notin% c("N/A",NA)),]
item225.dat1 <- item225.dat0[which(item225.dat0$Ownership != "Unknown"),]
item225.dat1$Ownership[which(item225.dat1$Ownership %in% c("COOP", "HOA"))] <- "Cooperative"
item225.dat1$Ownership[which(item225.dat1$Ownership %in% c("LLC"))] <- "Corporation/REIT"

item225.dat2 <- left_join(rbsa.dat.bldg,item225.dat1)

#subset to only MF sites
item225.dat3 <- item225.dat2[which(item225.dat2$BuildingType %in% "Multifamily"),]

item225.merge <- item225.dat3[which(!is.na(item225.dat3$Ownership)),]
item225.merge <- item225.merge[which(item225.merge$Ownership != "N/A"),]

################################################
# Adding pop and sample sizes for weights
################################################
item225.data <- weightedData(item225.merge[-which(colnames(item225.merge) %in% c("Ownership"
                                                                                 ,"Category"))])
item225.data <- left_join(item225.data, item225.merge[which(colnames(item225.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ownership"
                                                                                           ,"Category"))])

item225.data$count <- 1
#######################
# Weighted Analysis
#######################
item225.final <- proportionRowsAndColumns1(CustomerLevelData = item225.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'Ownership'
                                           ,aggregateColumnName = "Remove")
item225.final <- item225.final[which(item225.final$Category != "Remove"),]

item225.cast <- dcast(setDT(item225.final)
                      ,formula = Ownership ~ Category
                      ,value.var = c("w.percent","w.SE", "count","n","N", "EB"))
names(item225.cast)
item225.table <- data.frame("Ownership"                = item225.cast$Ownership
                            ,"PSE.Percent"                 = item225.cast$w.percent_PSE
                            ,"PSE.SE"                      = item225.cast$w.SE_PSE
                            ,"PSE.n"                       = item225.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item225.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item225.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item225.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item225.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item225.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item225.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item225.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item225.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS_n"              = item225.cast$`n_2017 RBSA PS`
                            ,"PSE.EB"                      = item225.cast$EB_PSE
                            ,"PSE.King.County_EB"          = item225.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = item225.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = item225.cast$`EB_2017 RBSA PS`)

exportTable(item225.table, "MF", "Table 17", weighted = TRUE,OS = T, osIndicator = "PSE")
#######################
# unweighted Analysis
#######################
item225.final <- proportions_two_groups_unweighted(CustomerLevelData = item225.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'Ownership'
                                           ,aggregateColumnName = "Remove")
item225.final <- item225.final[which(item225.final$Category != "Remove"),]

item225.cast <- dcast(setDT(item225.final)
                      ,formula = Ownership ~ Category
                      ,value.var = c("Percent","SE", "Count","n"))

item225.table <- data.frame("Ownership"                = item225.cast$Ownership
                            ,"PSE.Percent"                 = item225.cast$Percent_PSE
                            ,"PSE.SE"                      = item225.cast$SE_PSE
                            ,"PSE.n"                       = item225.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item225.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item225.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item225.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item225.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item225.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item225.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item225.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item225.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS_n"              = item225.cast$`n_2017 RBSA PS`)


exportTable(item225.table, "MF", "Table 17", weighted = FALSE,OS = T, osIndicator = "PSE")
