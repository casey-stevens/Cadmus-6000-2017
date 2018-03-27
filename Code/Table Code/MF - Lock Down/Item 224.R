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


#############################################################################################
# Item 224: DISTRIBUTION OF NON-RESIDENTIAL FLOOR AREA (IN BUILDINGS WITH NON-RESIDENTIAL) BY USE TYPE AND BUILDING SIZE (MF table 16)
#############################################################################################
item224.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                  # ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"
                                                                  ,""))]

colnames(item224.dat) <- c("Grocery"
                           ,"Office"
                           ,"Other"
                           ,"Retail"
                           ,"Vacant"
                           ,"CK_Building_ID")


for (i in 1:5){
  item224.dat[,i] <- as.numeric(as.character(item224.dat[,i]))
}
item224.dat[is.na(item224.dat)] <- 0

item224.dat$Total.Area <- item224.dat$Grocery + item224.dat$Office + item224.dat$Other + item224.dat$Retail + item224.dat$Vacant

item224.melt <- melt(item224.dat, id = c("CK_Building_ID","Total.Area"))
colnames(item224.melt) <- c("CK_Building_ID", "Total.Area", "Nonres.Type", "Area")
item224.melt$CK_Building_ID <- as.character(item224.melt$CK_Building_ID)

item224.merge <- left_join(rbsa.dat.bldg, item224.melt)

#subset to only MF sites
item224.dat1 <- item224.merge[which(item224.merge$BuildingTypeXX %in% c("Apartment Building (3 or fewer floors)"
                                                                        ,"Apartment Building (4 to 6 floors)"
                                                                        ,"Apartment Building (More than 6 floors)")),]

#remove any zeros 
item224.dat2 <- data.frame(item224.dat1[which(item224.dat1$Total.Area != 0),], stringsAsFactors = F)
item224.dat2$Ind <- 0
item224.dat2$Ind[which(item224.dat2$Area > 0)] <- 1

item224.data <- weightedData(item224.dat2[which(colnames(item224.dat2) %notin% c("Total.Area"
                                                                                 ,"Nonres.Type"
                                                                                 ,"Area"
                                                                                 ,"Ind"
                                                                                 ,"Category"))])
item224.data <- left_join(item224.data, item224.dat2[which(colnames(item224.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Total.Area"
                                                                                         ,"Nonres.Type"
                                                                                         ,"Area"
                                                                                         ,"Ind"
                                                                                         ,"Category"))])
item224.data$count <- 1
stopifnot(nrow(item224.data) == nrow(item224.dat2))
####################
#Weighted analysis
####################
item224.summary <- proportionRowsAndColumns1(CustomerLevelData = item224.data
                                             ,valueVariable = 'Area'
                                             ,columnVariable = 'Category'
                                             ,rowVariable = 'Nonres.Type'
                                             ,aggregateColumnName = 'Remove')
item224.summary <- item224.summary[which(item224.summary$Category != "Remove"),]

item224.summary$n[which(item224.summary$Category == "PSE" & item224.summary$Nonres.Type == "Total")] <- min(item224.summary$n[which(item224.summary$Category == "PSE")])
item224.summary$n[which(item224.summary$Category == "PSE KING COUNTY" & item224.summary$Nonres.Type == "Total")] <- min(item224.summary$n[which(item224.summary$Category == "PSE KING COUNTY")])
item224.summary$n[which(item224.summary$Category == "PSE NON-KING COUNTY" & item224.summary$Nonres.Type == "Total")] <- min(item224.summary$n[which(item224.summary$Category == "PSE NON-KING COUNTY")])
item224.summary$n[which(item224.summary$Category == "2017 RBSA PS" & item224.summary$Nonres.Type == "Total")] <- min(item224.summary$n[which(item224.summary$Category == "2017 RBSA PS")])


item224.final <- item224.summary

item224.cast <- dcast(setDT(item224.final)
                      ,formula = Nonres.Type ~ Category
                      ,value.var = c("w.percent","w.SE", "count", "n", "N","EB"))

item224.table <- data.frame("Nonresidential_Use_Type"  = item224.cast$Nonres.Type
                            ,"PSE.Percent"                 = item224.cast$w.percent_PSE
                            ,"PSE.SE"                      = item224.cast$w.SE_PSE
                            ,"PSE.n"                       = item224.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item224.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item224.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item224.cast$n_PSE
                            ,"PSE.Non.King.County.Percent" = item224.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item224.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item224.cast$n_PSE
                            ,"2017.RBSA.PS.Percent"        = item224.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item224.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS_n"              = item224.cast$`n_2017 RBSA PS`
                            ,"PSE.EB"                      = item224.cast$EB_PSE
                            ,"PSE.King.County_EB"          = item224.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = item224.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = item224.cast$`EB_2017 RBSA PS`
                              )
exportTable(item224.table, "MF", "Table 16", weighted = TRUE)

####################
#Weighted analysis
####################
item224.summary <- proportions_two_groups_unweighted(CustomerLevelData = item224.data
                                                     ,valueVariable = 'Area'
                                                     ,columnVariable = 'Category'
                                                     ,rowVariable = 'Nonres.Type'
                                                     ,aggregateColumnName = 'Remove')
item224.summary <- item224.summary[which(item224.summary$Category != "Remove"),]

item224.final <- item224.summary

item224.cast <- dcast(setDT(item224.final)
                      ,formula = Nonres.Type ~ Category
                      ,value.var = c("Percent","SE", "Count", "n"))

item224.table <- data.frame("Nonresidential_Use_Type"  = item224.cast$Nonres.Type
                            ,"PSE.Percent"                 = item224.cast$Percent_PSE
                            ,"PSE.SE"                      = item224.cast$SE_PSE
                            ,"PSE.n"                       = item224.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item224.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item224.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item224.cast$n_PSE
                            ,"PSE.Non.King.County.Percent" = item224.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item224.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item224.cast$n_PSE
                            ,"2017.RBSA.PS.Percent"        = item224.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item224.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS_n"              = item224.cast$`n_2017 RBSA PS`
)
exportTable(item224.table, "MF", "Table 16", weighted = FALSE)
