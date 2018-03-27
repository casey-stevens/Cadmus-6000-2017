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
rbsa.dat.MF <- rbsa.dat[grep("Multifamily", rbsa.dat$BuildingType),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg",rbsa.dat.MF$CK_Building_ID),]

#read in Envelope data for MF table
# envelope.dat <- read.xlsx(envelope.export)
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_SiteID"
                                                                  ,"Ceiling.Type"
                                                                  ,"Ceiling.Area"
                                                                  ,"Floor.Type"
                                                                  ,"Floor.Area"
                                                                  ,"Floor.Sub-Type",
                                                                  "Type.of.Area.Below"
                                                                  ,"Area.Below.Heated?"))]

envelope.dat2 <- left_join(rbsa.dat, envelope.dat1, by = c("CK_Building_ID" = "CK_SiteID"))
length(unique(envelope.dat2$CK_Cadmus_ID))
envelope.dat.MF <- envelope.dat2[grep("Multifamily", envelope.dat2$BuildingType),]

unique(envelope.dat.MF$Ceiling.Type)
envelope.dat.MF$CeilingType <- ""
envelope.dat.MF$CeilingType[grep("Attic",envelope.dat.MF$Ceiling.Type,ignore.case = T)] <- "Attic"
envelope.dat.MF$CeilingType[grep("Vaulted",envelope.dat.MF$Ceiling.Type,ignore.case = T)] <- "Vaulted"
envelope.dat.MF$CeilingType[grep("Roof Deck",envelope.dat.MF$Ceiling.Type,ignore.case = T)] <- "Roof Deck"
envelope.dat.MF$CeilingType[grep("Sloped",envelope.dat.MF$Ceiling.Type,ignore.case = T)] <- "Vaulted"
envelope.dat.MF$CeilingType[which(!(envelope.dat.MF$CeilingType %in% c("Attic", "Vaulted","Roof Deck")))] <- "Other"
unique(envelope.dat.MF$CeilingType)

unique(envelope.dat.MF$Floor.Type)
unique(envelope.dat.MF$`Floor.Sub-Type`)
unique(envelope.dat.MF$Type.of.Area.Below)
unique(envelope.dat.MF$`Area.Below.Heated?`)

# Bring in clean ground contact types
FloorMappingTypes <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "MF Floor Type Lookup.xlsx"), sheet = 1)


# envelope.dat.MF$FloorType <- ""
# envelope.dat.MF$FloorType[grep("Slab",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Slab"
# envelope.dat.MF$FloorType[grep("Crawlspace",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Crawlspace"
# envelope.dat.MF$FloorType[grep("Basement",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Basement"
# envelope.dat.MF$FloorType[grep("Floor over other area",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Framed"
# envelope.dat.MF$FloorType[grep("Cantilever",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Cantilever"
# unique(envelope.dat.MF$FloorType)
# 
# envelope.dat.MF$SubFloorType <- "Unknown"
# unique(envelope.dat.MF$SubFloorType)
# envelope.dat.MF$SubFloorType[grep("framed",envelope.dat.MF$FloorType,ignore.case = T)] <- 
#   envelope.dat.MF$Type.of.Area.Below[grep("framed",envelope.dat.MF$FloorType,ignore.case = T)]
# envelope.dat.MF$SubFloorType[grep("conditioned",envelope.dat.MF$`Floor.Sub-Type`,ignore.case = T)] <- "Conditioned"
# envelope.dat.MF$SubFloorType[grep("unconditioned",envelope.dat.MF$`Floor.Sub-Type`,ignore.case = T)] <- "Unconditioned"
# envelope.dat.MF$SubFloorType[grep("slab",envelope.dat.MF$`Floor.Sub-Type`,ignore.case = T)] <- "Slab"
# 
# envelope.dat.MF$Floor.Sub.Type <- paste(envelope.dat.MF$FloorType, envelope.dat.MF$SubFloorType)
# unique(envelope.dat.MF$Floor.Sub.Type)
# 
# envelope.sub <- envelope.dat.MF[-grep("Unknown", envelope.dat.MF$Floor.Sub.Type),]
# ii=1
# for(ii in 1:nrow(envelope.sub)){
#   if(envelope.sub$Floor.Type[ii] %in% c("Floor over other area")){
#     envelope.sub$Floor.Sub.Type[ii] <- paste(envelope.sub$Floor.Sub.Type[ii], "Over", envelope.sub$Type.of.Area.Below[ii], sep = " ")
#   }else{
#     envelope.sub$Floor.Sub.Type[ii] <- envelope.sub$Floor.Sub.Type[ii]
#   }
# }
# # envelope.sub$Floor.Sub.Type <- gsub("Framed Parking", "Framed Floor", envelope.sub$Floor.Sub.Type)
# # envelope.sub$Floor.Sub.Type <- gsub("Framed Garage", "Framed Floor", envelope.sub$Floor.Sub.Type)
# # envelope.sub$Floor.Sub.Type <- gsub("Framed Storage", "Framed Floor", envelope.sub$Floor.Sub.Type)
# # envelope.sub$Floor.Sub.Type <- gsub("Framed Conditioned", "Framed Floor", envelope.sub$Floor.Sub.Type)
# # envelope.sub$Floor.Sub.Type <- gsub("Framed Crawlspace", "Framed Floor", envelope.sub$Floor.Sub.Type)
# unique(envelope.sub$Floor.Sub.Type)
# 
# envelope.sub <- envelope.sub[-grep("Other|Residential|Outside",envelope.sub$Floor.Sub.Type),]
# unique(envelope.sub$Floor.Sub.Type)

#############################################################################################
#Item 236: Table 28
#############################################################################################

item236.dat <- unique(envelope.dat.MF[which(colnames(envelope.dat.MF) %in% c("CK_Cadmus_ID",
                                                                             "CK_Building_ID",
                                                                             "Category",
                                                                             "Ceiling.Type",
                                                                             "Ceiling.Area",
                                                                             "BuildingTypeXX",
                                                                             "CeilingType"))])
item236.dat1 <- item236.dat[which(item236.dat$Ceiling.Area > 0),] 
item236.dat1$Ceiling.Area <- as.numeric(as.character(item236.dat1$Ceiling.Area))

item236.dat2 <- item236.dat1[which(item236.dat1$Ceiling.Type %notin% c("N/A")),]

item236.merge <- left_join(rbsa.dat, item236.dat2)
item236.merge <- item236.merge[which(!is.na(item236.merge$Ceiling.Area)),]
item236.merge <- item236.merge[grep("bldg", item236.merge$CK_Building_ID, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
item236.data <- weightedData(item236.merge[-which(colnames(item236.merge) %in% c("Ceiling.Type"
                                                                                 ,"Ceiling.Area"
                                                                                 ,"CeilingType"
                                                                                 ,"Category"))])
item236.data <- left_join(item236.data, unique(item236.merge[which(colnames(item236.merge) %in% c("CK_Cadmus_ID"
                                                                                                  ,"CK_Building_ID"
                                                                                                  ,"Ceiling.Type"
                                                                                                  ,"Ceiling.Area"
                                                                                                  ,"CeilingType"
                                                                                                  ,"Category"))]))

item236.data$count <- 1
item236.data$Ceiling.Area <- as.numeric(as.character(item236.data$Ceiling.Area))

length(unique(item236.data$CK_Building_ID))
#######################
# Weighted Analysis
#######################
item236.final <- proportionRowsAndColumns1(CustomerLevelData = item236.data
                                           ,valueVariable    = 'Ceiling.Area'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'CeilingType'
                                           ,aggregateColumnName = "Remove")
item236.final <- item236.final[which(item236.final$Category != "Remove"),]
# item236.final <- item236.final[which(item236.final$CeilingType != "Total"),]

item236.cast <- dcast(setDT(item236.final)
                      ,formula = CeilingType ~ Category
                      ,value.var = c("w.percent","w.SE", "count","n","N","EB"))
names(item236.cast)
item236.table <- data.frame( "Ceiling.Type" = item236.cast$CeilingType
                             ,"PSE.Percent"                 = item236.cast$w.percent_PSE
                             ,"PSE.SE"                      = item236.cast$w.SE_PSE
                             ,"PSE.n"                       = item236.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item236.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item236.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item236.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item236.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item236.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item236.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item236.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item236.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"              = item236.cast$`n_2017 RBSA PS`
                             ,"PSE.EB"                      = item236.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item236.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item236.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item236.cast$`EB_2017 RBSA PS`)
unique(item236.table$Ceiling.Type)
rowOrder <- c("Attic"
              ,"Roof Deck"
              ,"Vaulted"
              ,"Other"
              ,"Total")
item236.table <- item236.table %>% mutate(Ceiling.Type = factor(Ceiling.Type, levels = rowOrder)) %>% arrange(Ceiling.Type)  
item236.table <- data.frame(item236.table[which(names(item236.table) != "BuildingType")])


exportTable(item236.table, "MF", "Table 28", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# Weighted Analysis
#######################
item236.final <- proportions_two_groups_unweighted(CustomerLevelData = item236.data
                                           ,valueVariable    = 'Ceiling.Area'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'CeilingType'
                                           ,aggregateColumnName = "Remove")
item236.final <- item236.final[which(item236.final$Category != "Remove"),]

item236.cast <- dcast(setDT(item236.final)
                      ,formula = CeilingType ~ Category
                      ,value.var = c("Percent","SE", "Count","n"))

item236.table <- data.frame( "Ceiling.Type" = item236.cast$CeilingType
                             ,"PSE.Percent"                 = item236.cast$Percent_PSE
                             ,"PSE.SE"                      = item236.cast$SE_PSE
                             ,"PSE.n"                       = item236.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item236.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item236.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item236.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item236.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item236.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item236.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item236.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item236.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"              = item236.cast$`n_2017 RBSA PS`)
unique(item236.table$Ceiling.Type)
rowOrder <- c("Attic"
              ,"Roof Deck"
              ,"Vaulted"
              ,"Other"
              ,"Total")
item236.table <- item236.table %>% mutate(Ceiling.Type = factor(Ceiling.Type, levels = rowOrder)) %>% arrange(Ceiling.Type)  
item236.table <- data.frame(item236.table[which(names(item236.table) != "BuildingType")])


exportTable(item236.table, "MF", "Table 28", weighted = FALSE,OS = T, osIndicator = "PSE")













#############################################################################################
#Item 238: Table 30
#############################################################################################

item238.dat <- unique(envelope.dat.MF[which(colnames(envelope.dat.MF) %in% c("CK_Cadmus_ID",
                                                                             "CK_Building_ID",
                                                                             "BuildingTypeXX",
                                                                             "Floor.Type",
                                                                             "Floor.Sub-Type",
                                                                             "Floor.Area"
                                                                             ,"Type.of.Area.Below"
                                                                             ,"Area.Below.Heated?"))])
item238.dat$Floor.Area <- as.numeric(as.character(item238.dat$Floor.Area))

item238.dat1 <- item238.dat[which(item238.dat$Floor.Area > 0),] 
item238.dat1 <- item238.dat[which(item238.dat$Floor.Area %notin% c("N/A",NA)),] 

#floor type mapping
item238.dat1$FloorType <- NA

i=1
for (i in 1:length(FloorMappingTypes$Final.Floor.Type)){
  item238.dat1$FloorType[which(item238.dat1[,4] == FloorMappingTypes[i,1] & 
                                 item238.dat1[,5] == FloorMappingTypes[i,2] &
                                 item238.dat1[,6] == FloorMappingTypes[i,3] &
                                 item238.dat1[,7] == FloorMappingTypes[i,4])] <- FloorMappingTypes$Final.Floor.Type[i]
}
item238.dat1$FloorType <- trimws(item238.dat1$FloorType)
unique(item238.dat1$FloorType)

item238.dat2 <- item238.dat1[which(!is.na(item238.dat1$FloorType)),]

item238.merge <- left_join(rbsa.dat, item238.dat2)
item238.merge <- item238.merge[which(!is.na(item238.merge$FloorType)),]

################################################
# Adding pop and sample sizes for weights
################################################
item238.data <- weightedData(item238.merge[-which(colnames(item238.merge) %in% c("FloorType",
                                                                                 "Floor.Sub-Type",
                                                                                 "Floor.Area"
                                                                                 ,"Floor.Type"
                                                                                 ,"Type.of.Area.Below"
                                                                                 ,"Area.Below.Heated?"
                                                                                 ,"Category"))])
item238.data <- left_join(item238.data, unique(item238.merge[which(colnames(item238.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"FloorType",
                                                                                           "Floor.Sub-Type",
                                                                                           "Floor.Area"
                                                                                           ,"Floor.Type"
                                                                                           ,"Type.of.Area.Below"
                                                                                           ,"Area.Below.Heated?"
                                                                                           ,"Category"))]))

item238.data$count <- 1
item238.data$Floor.Area <- as.numeric(as.character(item238.data$Floor.Area))

#######################
# Weighted Analysis
#######################
item238.final <- proportionRowsAndColumns1(CustomerLevelData = item238.data
                                           ,valueVariable    = 'Floor.Area'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'FloorType'
                                           ,aggregateColumnName = "Remove")
item238.final <- item238.final[which(item238.final$Category != "Remove"),]

item238.cast <- dcast(setDT(item238.final)
                      ,formula = FloorType ~ Category
                      ,value.var = c("w.percent","w.SE", "count","n","N","EB"))

item238.table <- data.frame( "Floor.Type"    = item238.cast$FloorType
                             ,"PSE.Percent"                 = item238.cast$w.percent_PSE
                             ,"PSE.SE"                      = item238.cast$w.SE_PSE
                             ,"PSE.n"                       = item238.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item238.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item238.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item238.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item238.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item238.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item238.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item238.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item238.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"              = item238.cast$`n_2017 RBSA PS`
                             ,"PSE.EB"                      = item238.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item238.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item238.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item238.cast$`EB_2017 RBSA PS`
                             )

exportTable(item238.table, "MF", "Table 30", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
item238.final <- proportions_two_groups_unweighted(CustomerLevelData = item238.data
                                           ,valueVariable    = 'Floor.Area'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'FloorType'
                                           ,aggregateColumnName = "Remove")
item238.final <- item238.final[which(item238.final$Category != "Remove"),]

item238.cast <- dcast(setDT(item238.final)
                      ,formula = FloorType ~ Category
                      ,value.var = c("Percent","SE", "Count","n"))

item238.table <- data.frame( "Floor.Type"    = item238.cast$FloorType
                             ,"PSE.Percent"                 = item238.cast$Percent_PSE
                             ,"PSE.SE"                      = item238.cast$SE_PSE
                             ,"PSE.n"                       = item238.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item238.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item238.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item238.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item238.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item238.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item238.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item238.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item238.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"              = item238.cast$`n_2017 RBSA PS`)

exportTable(item238.table, "MF", "Table 30", weighted = FALSE,OS = T, osIndicator = "PSE")
