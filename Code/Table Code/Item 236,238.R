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
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_SiteID"
                                                                  ,"Ceiling.Type"
                                                                  ,"Ceiling.Area"
                                                                  ,"Floor.Type"
                                                                  ,"Floor.Area"
                                                                  ,"Floor.Sub-Type",
                                                                  "Type.of.Area.Below"))]

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
envelope.dat.MF$FloorType <- ""
envelope.dat.MF$FloorType[grep("Slab",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Slab"
envelope.dat.MF$FloorType[grep("Crawlspace",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Crawlspace"
envelope.dat.MF$FloorType[grep("Basement",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Basement"
envelope.dat.MF$FloorType[grep("Floor over other area",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Framed"
envelope.dat.MF$FloorType[grep("Cantilever",envelope.dat.MF$Floor.Type,ignore.case = T)] <- "Cantilever"
unique(envelope.dat.MF$FloorType)

envelope.dat.MF$SubFloorType <- "Unknown"
unique(envelope.dat.MF$SubFloorType)
envelope.dat.MF$SubFloorType[grep("framed",envelope.dat.MF$FloorType,ignore.case = T)] <- 
  envelope.dat.MF$Type.of.Area.Below[grep("framed",envelope.dat.MF$FloorType,ignore.case = T)]
envelope.dat.MF$SubFloorType[grep("conditioned",envelope.dat.MF$`Floor.Sub-Type`,ignore.case = T)] <- "Conditioned"
envelope.dat.MF$SubFloorType[grep("unconditioned",envelope.dat.MF$`Floor.Sub-Type`,ignore.case = T)] <- "Unconditioned"
envelope.dat.MF$SubFloorType[grep("slab",envelope.dat.MF$`Floor.Sub-Type`,ignore.case = T)] <- "Slab"

envelope.dat.MF$Floor.Sub.Type <- paste(envelope.dat.MF$FloorType, envelope.dat.MF$SubFloorType)
unique(envelope.dat.MF$Floor.Sub.Type)

envelope.sub <- envelope.dat.MF[-grep("Unknown", envelope.dat.MF$Floor.Sub.Type),]
ii=1
for(ii in 1:nrow(envelope.sub)){
  if(envelope.sub$Type.of.Area.Below[ii] != "-- Datapoint not asked for --"){
    envelope.sub$Floor.Sub.Type[ii] <- paste(envelope.sub$Floor.Sub.Type[ii], "Over", envelope.sub$Type.of.Area.Below[ii], sep = " ")
  }else{
    envelope.sub$Floor.Sub.Type[ii] <- envelope.sub$Floor.Sub.Type[ii]
  }
}
envelope.sub$Floor.Sub.Type <- gsub("Framed Parking", "Framed Floor", envelope.sub$Floor.Sub.Type)
envelope.sub$Floor.Sub.Type <- gsub("Framed Garage", "Framed Floor", envelope.sub$Floor.Sub.Type)
envelope.sub$Floor.Sub.Type <- gsub("Framed Storage", "Framed Floor", envelope.sub$Floor.Sub.Type)
envelope.sub$Floor.Sub.Type <- gsub("Framed Conditioned", "Framed Floor", envelope.sub$Floor.Sub.Type)
envelope.sub$Floor.Sub.Type <- gsub("Framed Crawlspace", "Framed Floor", envelope.sub$Floor.Sub.Type)
unique(envelope.sub$Floor.Sub.Type)

envelope.sub <- envelope.sub[-grep("Other",envelope.sub$Floor.Sub.Type),]

#############################################################################################
#Item 236: Table 28
#############################################################################################

item236.dat <- unique(envelope.dat.MF[which(colnames(envelope.dat.MF) %in% c("CK_Cadmus_ID",
                                                                             "CK_SiteID",
                                                                             "Ceiling.Type",
                                                                             "Ceiling.Area",
                                                                             "BuildingTypeXX",
                                                                             "CeilingType"))])
item236.dat1 <- item236.dat[which(item236.dat$Ceiling.Area > 0),] 
item236.dat1$Ceiling.Area <- as.numeric(as.character(item236.dat1$Ceiling.Area))

item236.merge <- left_join(rbsa.dat, item236.dat1)
item236.merge <- item236.merge[which(!is.na(item236.merge$Ceiling.Area)),]


################################################
# Adding pop and sample sizes for weights
################################################
item236.data <- weightedData(item236.merge[-which(colnames(item236.merge) %in% c("Ceiling.Type"
                                                                                 ,"Ceiling.Area"
                                                                                 ,"CeilingType"))])
item236.data <- left_join(item236.data, item236.merge[which(colnames(item236.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ceiling.Type"
                                                                                           ,"Ceiling.Area"
                                                                                           ,"CeilingType"))])

item236.data$count <- 1
item236.data$Ceiling.Area <- as.numeric(as.character(item236.data$Ceiling.Area))


#######################
# Weighted Analysis
#######################
item236.final <- proportionRowsAndColumns1(CustomerLevelData = item236.data
                                           ,valueVariable    = 'Ceiling.Area'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'CeilingType'
                                           ,aggregateColumnName = "Remove")
item236.final <- item236.final[which(item236.final$HomeType != "Remove"),]
item236.final <- item236.final[which(item236.final$CeilingType != "Total"),]


item236.all.vintages <- proportions_one_group_MF(CustomerLevelData = item236.data
                                                 ,valueVariable = 'Ceiling.Area'
                                                 ,groupingVariable = 'CeilingType'
                                                 ,total.name = "All Sizes"
                                                 ,columnName = "HomeType"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)

item236.final <- rbind.data.frame(item236.final, item236.all.vintages, stringsAsFactors = F)

item236.cast <- dcast(setDT(item236.final)
                      ,formula = HomeType ~ CeilingType
                      ,value.var = c("w.percent","w.SE", "count","n","N"))

item236.final <- data.frame( "Building.Size" = item236.cast$HomeType
                             ,"Attic"        = item236.cast$w.percent_Attic
                             ,"Attic.SE"     = item236.cast$w.SE_Attic
                             ,"Attic.n"      = item236.cast$n_Attic
                             ,"Roof.Deck"    = item236.cast$`w.percent_Roof Deck`
                             ,"Roof.Deck.SE" = item236.cast$`w.SE_Roof Deck`
                             ,"Roof.Deck.n"  = item236.cast$`n_Roof Deck`
                             ,"Vaulted"      = item236.cast$w.percent_Vaulted
                             ,"Vaulted.SE"   = item236.cast$w.SE_Vaulted
                             ,"Vaulted.n"    = item236.cast$n_Vaulted
                             ,"Other"        = item236.cast$w.percent_Other
                             ,"Other.SE"     = item236.cast$w.SE_Other
                             ,"Other.n"      = item236.cast$n_Other)

exportTable(item236.final, "MF", "Table 28", weighted = TRUE)


#######################
# Weighted Analysis
#######################
item236.final <- proportions_two_groups_unweighted(CustomerLevelData = item236.data
                                           ,valueVariable    = 'Ceiling.Area'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'CeilingType'
                                           ,aggregateColumnName = "Remove")
item236.final <- item236.final[which(item236.final$HomeType != "Remove"),]
item236.final <- item236.final[which(item236.final$CeilingType != "Total"),]


item236.all.vintages <- proportions_one_group_MF(CustomerLevelData = item236.data
                                                 ,valueVariable = 'Ceiling.Area'
                                                 ,groupingVariable = 'CeilingType'
                                                 ,total.name = "All Sizes"
                                                 ,columnName = "HomeType"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE)

item236.final <- rbind.data.frame(item236.final, item236.all.vintages, stringsAsFactors = F)

item236.cast <- dcast(setDT(item236.final)
                      ,formula = HomeType ~ CeilingType
                      ,value.var = c("Percent","SE", "Count","SampleSize"))

item236.final <- data.frame( "Building.Size" = item236.cast$HomeType
                             ,"Attic"        = item236.cast$Percent_Attic
                             ,"Attic.SE"     = item236.cast$SE_Attic
                             ,"Attic.n"      = item236.cast$SampleSize_Attic
                             ,"Roof.Deck"    = item236.cast$`Percent_Roof Deck`
                             ,"Roof.Deck.SE" = item236.cast$`SE_Roof Deck`
                             ,"Roof.Deck.n"  = item236.cast$`SampleSize_Roof Deck`
                             ,"Vaulted"      = item236.cast$Percent_Vaulted
                             ,"Vaulted.SE"   = item236.cast$SE_Vaulted
                             ,"Vaulted.n"    = item236.cast$SampleSize_Vaulted
                             ,"Other"        = item236.cast$Percent_Other
                             ,"Other.SE"     = item236.cast$SE_Other
                             ,"Other.n"      = item236.cast$SampleSize_Other)

exportTable(item236.final, "MF", "Table 28", weighted = FALSE)




#############################################################################################
#Item 238: Table 30
#############################################################################################

item238.dat <- unique(envelope.sub[which(colnames(envelope.sub) %in% c("CK_Cadmus_ID",
                                                                          "CK_SiteID",
                                                                          "BuildingTypeXX",
                                                                          "FloorType",
                                                                          "Floor.Sub.Type",
                                                                          "Floor.Area"))])
item238.dat1 <- item238.dat[which(item238.dat$Floor.Area > 0),] 

item238.dat1$Floor.Area <- as.numeric(as.character(item238.dat1$Floor.Area))

item238.merge <- left_join(rbsa.dat, item238.dat1)
item238.merge <- item238.merge[which(!is.na(item238.merge$Floor.Area)),]

################################################
# Adding pop and sample sizes for weights
################################################
item238.data <- weightedData(item238.merge[-which(colnames(item238.merge) %in% c("FloorType",
                                                                                 "Floor.Sub.Type",
                                                                                 "Floor.Area"))])
item238.data <- left_join(item238.data, item238.merge[which(colnames(item238.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"FloorType",
                                                                                           "Floor.Sub.Type",
                                                                                           "Floor.Area"))])

item238.data$count <- 1
item238.data$Floor.Area <- as.numeric(as.character(item238.data$Floor.Area))

#######################
# Weighted Analysis
#######################
item238.final <- proportionRowsAndColumns1(CustomerLevelData = item238.data
                                           ,valueVariable    = 'Floor.Area'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Floor.Sub.Type'
                                           ,aggregateColumnName = "Remove")
item238.final <- item238.final[which(item238.final$HomeType != "Remove"),]
item238.final <- item238.final[which(item238.final$Floor.Sub.Type != "Total"),]


item238.all.sizes <- proportions_one_group_MF(CustomerLevelData = item238.data
                                                 ,valueVariable = 'Floor.Area'
                                                 ,groupingVariable = 'Floor.Sub.Type'
                                                 ,total.name = "All Sizes"
                                                 ,columnName = "HomeType"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)

item238.final <- rbind.data.frame(item238.final, item238.all.sizes, stringsAsFactors = F)

item238.cast <- dcast(setDT(item238.final)
                      ,formula = Floor.Sub.Type ~ HomeType
                      ,value.var = c("w.percent","w.SE", "count","n","N"))



item238.final <- data.frame( "Floor.Type"    = item238.cast$Floor.Sub.Type
                             ,"Low.Rise.1.3"    = item238.cast$`w.percent_Apartment Building (3 or fewer floors)`
                             ,"Low.Rise.1.3.SE" = item238.cast$`w.SE_Apartment Building (3 or fewer floors)`
                             ,"Low.Rise.1.3.n"  = item238.cast$`n_Apartment Building (3 or fewer floors)`
                             ,"Mid.Rise.4.6"    = item238.cast$`w.percent_Apartment Building (4 to 6 floors)`
                             ,"Mid.Rise.4.6.SE" = item238.cast$`w.SE_Apartment Building (4 to 6 floors)`
                             ,"Mid.Rise.4.6.n"  = item238.cast$`n_Apartment Building (4 to 6 floors)`
                             ,"High.Rise.GT7"   = item238.cast$`w.percent_Apartment Building (More than 6 floors)`
                             ,"High.Rise.GT7.SE"= item238.cast$`w.SE_Apartment Building (More than 6 floors)`
                             ,"High.Rise.GT7.n" = item238.cast$`n_Apartment Building (More than 6 floors)`
                             ,"All.Sizes"       = item238.cast$`w.percent_All Sizes`
                             ,"All.Sizes.SE"    = item238.cast$`w.SE_All Sizes`
                             ,"All.Sizes.n"     = item238.cast$`n_All Sizes`)

exportTable(item238.final, "MF", "Table 30", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item238.final <- proportions_two_groups_unweighted(CustomerLevelData = item238.data
                                           ,valueVariable    = 'Floor.Area'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Floor.Sub.Type'
                                           ,aggregateColumnName = "Remove")
item238.final <- item238.final[which(item238.final$HomeType != "Remove"),]
item238.final <- item238.final[which(item238.final$Floor.Sub.Type != "Total"),]


item238.all.sizes <- proportions_one_group_MF(CustomerLevelData = item238.data
                                              ,valueVariable = 'Floor.Area'
                                              ,groupingVariable = 'Floor.Sub.Type'
                                              ,total.name = "All Sizes"
                                              ,columnName = "HomeType"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)

item238.final <- rbind.data.frame(item238.final, item238.all.sizes, stringsAsFactors = F)

item238.cast <- dcast(setDT(item238.final)
                      ,formula = Floor.Sub.Type ~ HomeType
                      ,value.var = c("Percent","SE", "Count","SampleSize"))

item238.final <- data.frame( "Floor.Type"    = item238.cast$Floor.Sub.Type
                             ,"Low.Rise.1.3"    = item238.cast$`Percent_Apartment Building (3 or fewer floors)`
                             ,"Low.Rise.1.3.SE" = item238.cast$`SE_Apartment Building (3 or fewer floors)`
                             ,"Low.Rise.1.3.n"  = item238.cast$`SampleSize_Apartment Building (3 or fewer floors)`
                             ,"Mid.Rise.4.6"    = item238.cast$`Percent_Apartment Building (4 to 6 floors)`
                             ,"Mid.Rise.4.6.SE" = item238.cast$`SE_Apartment Building (4 to 6 floors)`
                             ,"Mid.Rise.4.6.n"  = item238.cast$`SampleSize_Apartment Building (4 to 6 floors)`
                             ,"High.Rise.GT7"   = item238.cast$`Percent_Apartment Building (More than 6 floors)`
                             ,"High.Rise.GT7.SE"= item238.cast$`SE_Apartment Building (More than 6 floors)`
                             ,"High.Rise.GT7.n" = item238.cast$`SampleSize_Apartment Building (More than 6 floors)`
                             ,"All.Sizes"       = item238.cast$`Percent_All Sizes`
                             ,"All.Sizes.SE"    = item238.cast$`SE_All Sizes`
                             ,"All.Sizes.n"     = item238.cast$`SampleSize_All Sizes`)

exportTable(item238.final, "MF", "Table 30", weighted = FALSE)
