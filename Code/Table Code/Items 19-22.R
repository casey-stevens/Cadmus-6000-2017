#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


##  Create "Not In" operator
"%notin%" <- Negate("%in%")


# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


if(os.ind == "rbsa"){
  rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),] 
}else{
  rbsa.dat$CK_Building_ID <- rbsa.dat$Category
  rbsa.dat <- rbsa.dat[which(names(rbsa.dat) != "Category")]
}

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))




#############################################################################################
# Item 19: PERCENTAGE OF HOMES WITH BASEMENTS BY STATE (SF table 26)
#############################################################################################
item19.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Floor.Type"
                                                               ,"Floor.Sub-Type"))]

item19.dat1 <- left_join(rbsa.dat, item19.dat, by = "CK_Cadmus_ID")

#subset to only single family homes
item19.dat2 <- item19.dat1[which(item19.dat1$BuildingType == "Single Family"),]
item19.dat2$count <- 1

item19.dat2$Ind <- 0
item19.dat2$Ind[which(item19.dat2$Floor.Type == "Basement")] <- 1

item19.dat3 <- unique(item19.dat2[which(item19.dat2$Ind == 1),])

item19.dat3 <- item19.dat3[-which(duplicated(item19.dat3$CK_Cadmus_ID)),]

item19.merge <- left_join(rbsa.dat, item19.dat3)

item19.merge1 <- item19.merge[-which(item19.merge$`Floor.Sub-Type` == "Unknown"),]
item19.merge1 <- item19.merge1[which(item19.merge1$BuildingType == "Single Family"),]
# apply weights to the subset of the data
item19.data <- weightedData(item19.merge1[which(colnames(item19.merge1) %notin% c("count"
                                                                                ,"Floor.Type"
                                                                                ,"Floor.Sub-Type"
                                                                                ,"Ind"
                                                                                ,"cond.ind"))])
#merge back on measured variable
item19.data <- left_join(item19.data, item19.merge1[which(colnames(item19.merge1) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Floor.Type"
                                                                                       ,"Floor.Sub-Type"
                                                                                       ,"Ind"
                                                                                       ,"cond.ind"))])

item19.data$Ind[which(is.na(item19.data$Ind))] <- 0
item19.data$Ind <- as.numeric(as.character(item19.data$Ind))
item19.data$Count <- 1
item19.data$count <- 1

which(duplicated(item19.data$CK_Cadmus_ID))
#####################################
# Weighted Analysis
#####################################
item19.final <- proportions_one_group(CustomerLevelData  = item19.data
                                     , valueVariable    = 'Ind'
                                     , groupingVariable = 'State'
                                     , total.name       = "Region"
                                     , weighted = TRUE)

#subset by home type
item19.final.SF <- item19.final[which(item19.final$BuildingType == "Single Family"),-which(colnames(item19.final) %in% c("BuildingType"))]


#export data
exportTable(item19.final.SF, "SF", "Table 26", weighted = TRUE)


#####################################
# Unweighted Analysis
#####################################
item19.final <- proportions_one_group(CustomerLevelData  = item19.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "Homes with Basements"
                                      , weighted = FALSE)
item19.final$State[which(item19.final$State == "Total")] <- "Region"

#subset by home type
item19.final.SF <- item19.final[which(item19.final$BuildingType == "Single Family"),-which(colnames(item19.final) %in% c("BuildingType"))]


#export data
exportTable(item19.final.SF, "SF", "Table 26"
            , weighted = FALSE)






#############################################################################################
# Item 20: PERCENTAGE OF BASEMENTS THAT ARE CONDITIONED BY STATE (SF table 27)
#############################################################################################
item20.dat <- item19.merge1[which(item19.merge1$Floor.Type == "Basement"),]

# apply weights to the subset of the data
item20.data <- weightedData(item20.dat[which(colnames(item20.dat) %notin% c("count"
                                                                            ,"Floor.Type"
                                                                            ,"Floor.Sub-Type"
                                                                            ,"Ind"
                                                                            ,"cond.ind"))])
#merge back on measured variable
item20.data <- left_join(item20.data, item20.dat[which(colnames(item20.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"count"
                                                                                   ,"Floor.Type"
                                                                                   ,"Floor.Sub-Type"
                                                                                   ,"Ind"
                                                                                   ,"cond.ind"))])

item20.data$Ind[which(is.na(item20.data$Ind))] <- 0
item20.data$Ind <- as.numeric(as.character(item20.data$Ind))
item20.data$cond.ind <- 0
item20.data$cond.ind[which(item20.data$`Floor.Sub-Type` == "Conditioned")] <- 1
item20.data$count        <- 1


#####################################
# Weighted Analysis
#####################################
item20.final <- proportions_one_group(CustomerLevelData  = item20.data
                                      , valueVariable    = 'cond.ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , weighted = TRUE)

#subset by home type
item20.final.SF <- item20.final[which(item20.final$BuildingType == "Single Family"),-1]


#export data
exportTable(item20.final.SF, "SF", "Table 27"
            , weighted = TRUE)



#####################################
# Unweighted Analysis
#####################################
item20.final <- proportions_one_group(CustomerLevelData  = item20.data
                                      , valueVariable    = 'cond.ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , weighted = FALSE)

#subset by home type
item20.final.SF <- item20.final[which(item20.final$BuildingType == "Single Family"),
                                -which(colnames(item20.final) %in% c("Remove", "BuildingType"))]


#export data
exportTable(item20.final.SF, "SF", "Table 27"
            , weighted = FALSE)




#############################################################################################
# Item 21: DISTRIBUTION OF BASEMENT SLAB INSULATION BY INSULATION LEVEL (SF table 28)
#############################################################################################
item21.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"ENV_Foundation_BSMT_SlabInsulated_Y_N"
                                                               ,"ENV_Foundation_BSMT_SlabInsulationThickness"))]
colnames(item21.dat)
colnames(item21.dat) <- c("CK_Cadmus_ID", "BSMT_Slab_Insulated", "BSMT_Slab_Thickness")
unique(item21.dat$BSMT_Slab_Insulated)
unique(item21.dat$BSMT_Slab_Thickness)


item21.dat1 <- item21.dat[which(item21.dat$BSMT_Slab_Insulated %in% c("Yes", "No")),]
item21.dat1$BSMT_Slab_Thickness[which(item21.dat1$BSMT_Slab_Insulated == "No")] <- "None"
unique(item21.dat1$BSMT_Slab_Thickness)


item21.merge <- left_join(rbsa.dat, item21.dat1)
item21.merge <- item21.merge[which(!is.na(item21.merge$BSMT_Slab_Thickness)),]
item21.merge <- unique(item21.merge[which(item21.merge$BSMT_Slab_Thickness != "Unknown"),])


# apply weights to the subset of the data
item21.data <- weightedData(item21.merge[which(colnames(item21.merge) %notin% c("BSMT_Slab_Insulated"
                                                                                ,"BSMT_Slab_Thickness"))])
#merge back on measured variable
item21.data <- left_join(item21.data, item21.merge[which(colnames(item21.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"BSMT_Slab_Insulated"
                                                                                       ,"BSMT_Slab_Thickness"))])
item21.data$count <- 1
item21.data$Count <- 1


#####################################
# Weighted Analysis
#####################################
item21.final <- proportions_one_group(CustomerLevelData  = item21.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'BSMT_Slab_Thickness'
                                      , total.name       = "Total"
                                      , weighted = TRUE)

#subset by home type
item21.final.SF <- item21.final[which(item21.final$BuildingType == "Single Family"),
                                -which(colnames(item21.final) %in% c("Remove", "BuildingType"))]


#export data
exportTable(item21.final.SF, "SF", "Table 28"
            , weighted = TRUE)




#####################################
# Unweighted Analysis
#####################################
item21.final <- proportions_one_group(CustomerLevelData  = item21.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'BSMT_Slab_Thickness'
                                      , total.name       = "Total"
                                      , columnName       = "Remove"
                                      , weighted = FALSE)

#subset by home type
item21.final.SF <- item21.final[which(item21.final$BuildingType == "Single Family")
                                ,-which(colnames(item21.final) %in% c("Remove", "BuildingType"))]


#export data
exportTable(item21.final.SF, "SF", "Table 28"
            , weighted = FALSE)







#############################################################################################
# Item 22: PERCENTAGE OF HOMES WITH FLOOR AREA OVER CRAWLSPACE BY STATE (SF table 29)
#############################################################################################
#subset envelope data to necessary columns
item22.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Foundation"
                                                               , "Conditioned.Living.Area"))]

item22.dat1 <- unique(item22.dat[grep("crawl|Crawl", item22.dat$Foundation),])

item22.merge <- left_join(rbsa.dat, item22.dat1, by = "CK_Cadmus_ID")
item22.merge <- item22.merge[which(item22.merge$BuildingType == "Single Family"),]
item22.merge$count <- 1
item22.merge$FloorOverCrawl <- 0
item22.merge$FloorOverCrawl[which(item22.merge$Foundation == "Crawlspace")] <- 1


# apply weights to the subset of the data
item22.data <- weightedData(item22.merge[which(colnames(item22.merge) %notin% c("count"
                                                                                ,"Foundation"
                                                                                ,"Conditioned.Living.Area"
                                                                                ,"FloorOverCrawl"))])
#merge back on measured variable
item22.data <- left_join(item22.data, item22.merge[which(colnames(item22.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Foundation"
                                                                                       ,"Conditioned.Living.Area"
                                                                                       ,"FloorOverCrawl"))])

unique(item22.data$FloorOverCrawl)
item22.data$Ind <- item22.data$FloorOverCrawl
item22.data$Count <- 1
#####################################
# Weighted Analysis
#####################################
item22.final <- proportions_one_group(CustomerLevelData  = item22.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , weighted = TRUE)

#subset by home type
item22.final.SF <- item22.final[which(item22.final$BuildingType == "Single Family")
                                ,-which(colnames(item22.final) %in% c("BuildingType"))]


#export data
exportTable(item22.final.SF, "SF", "Table 29"
            , weighted = TRUE)



#####################################
# Unweighted Analysis
#####################################
item22.final <- proportions_one_group(CustomerLevelData  = item22.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , weighted = FALSE)

#subset by home type
item22.final.SF <- item22.final[which(item22.final$BuildingType == "Single Family")
                                ,which(colnames(item22.final) %notin% c("BuildingType"))]


#export data
exportTable(item22.final.SF, "SF", "Table 29"
            , weighted = FALSE)



































#############################################################################################
#
#
# OVERSAMPLE ANALYSIS - MUST CHANGE SOURCECODE SCRIPT 
#   TO IDENTIFY WHICH OVERSAMPLE YOU ARE USING
#
#
#############################################################################################

#############################################################################################
# Item 19: PERCENTAGE OF HOMES WITH BASEMENTS BY CK_Building_ID (SF table 26)
#############################################################################################
item19.os.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Floor.Type"
                                                               ,"Floor.Sub-Type"))]

item19.os.dat1 <- left_join(rbsa.dat, item19.os.dat, by = "CK_Cadmus_ID")

#subset to only single family homes
item19.os.dat2 <- item19.os.dat1[which(item19.os.dat1$BuildingType == "Single Family"),]
item19.os.dat2$count <- 1

item19.os.dat2$Ind <- 0
item19.os.dat2$Ind[which(item19.os.dat2$Floor.Type == "Basement")] <- 1

item19.os.dat3 <- unique(item19.os.dat2[which(item19.os.dat2$Ind == 1),])

item19.os.dat3 <- item19.os.dat3[-which(duplicated(item19.os.dat3$CK_Cadmus_ID)),]

item19.os.merge <- left_join(rbsa.dat, item19.os.dat3)

item19.os.merge1 <- item19.os.merge[-which(item19.os.merge$`Floor.Sub-Type` == "Unknown"),]
item19.os.merge1 <- item19.os.merge1[which(item19.os.merge1$BuildingType == "Single Family"),]
# apply weights to the subset of the data
item19.os.data <- weightedData(item19.os.merge1[which(colnames(item19.os.merge1) %notin% c("count"
                                                                                  ,"Floor.Type"
                                                                                  ,"Floor.Sub-Type"
                                                                                  ,"Ind"
                                                                                  ,"cond.ind"))])
#merge back on measured variable
item19.os.data <- left_join(item19.os.data, item19.os.merge1[which(colnames(item19.os.merge1) %in% c("CK_Cadmus_ID"
                                                                                         ,"count"
                                                                                         ,"Floor.Type"
                                                                                         ,"Floor.Sub-Type"
                                                                                         ,"Ind"
                                                                                         ,"cond.ind"))])

item19.os.data$Ind[which(is.na(item19.os.data$Ind))] <- 0
item19.os.data$Ind <- as.numeric(as.character(item19.os.data$Ind))
item19.os.data$Count <- 1
item19.os.data$count <- 1

which(duplicated(item19.os.data$CK_Cadmus_ID))
#####################################
# Weighted Analysis
#####################################
item19.os.final <- proportions_one_group(CustomerLevelData  = item19.os.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'CK_Building_ID'
                                      , total.name       = "Remove"
                                      , weighted = TRUE, osIndicator = "SCL", OS = T)
item19.os.final <- item19.os.final[which(item19.os.final$CK_Building_ID != "Total"),]

#subset by home type
item19.os.final.SF <- item19.os.final[which(item19.os.final$BuildingType == "Single Family"),-which(colnames(item19.os.final) %in% c("BuildingType"))]


#export data
exportTable(item19.os.final.SF, "SF", "Table 26", weighted = TRUE, osIndicator = "SCL", OS = T)


#####################################
# Unweighted Analysis
#####################################
item19.os.final <- proportions_one_group(CustomerLevelData  = item19.os.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'CK_Building_ID'
                                      , total.name       = "Remove"
                                      , weighted = FALSE, osIndicator = "SCL", OS = T)
item19.os.final <- item19.os.final[which(item19.os.final$CK_Building_ID != "Total"),]

#subset by home type
item19.os.final.SF <- item19.os.final[which(item19.os.final$BuildingType == "Single Family"),-which(colnames(item19.os.final) %in% c("BuildingType"))]


#export data
exportTable(item19.os.final.SF, "SF", "Table 26"
            , weighted = FALSE, osIndicator = "SCL", OS = T)






#############################################################################################
# Item 20: PERCENTAGE OF BASEMENTS THAT ARE CONDITIONED BY CK_Building_ID (SF table 27)
#############################################################################################
item20.os.dat <- item20.os.merge1[which(item20.os.merge1$Floor.Type == "Basement"),]

# apply weights to the subset of the data
item20.os.data <- weightedData(item20.os.dat[which(colnames(item20.os.dat) %notin% c("count"
                                                                            ,"Floor.Type"
                                                                            ,"Floor.Sub-Type"
                                                                            ,"Ind"
                                                                            ,"cond.ind"))])
#merge back on measured variable
item20.os.data <- left_join(item20.os.data, item20.os.dat[which(colnames(item20.os.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"count"
                                                                                   ,"Floor.Type"
                                                                                   ,"Floor.Sub-Type"
                                                                                   ,"Ind"
                                                                                   ,"cond.ind"))])

item20.os.data$Ind[which(is.na(item20.os.data$Ind))] <- 0
item20.os.data$Ind <- as.numeric(as.character(item20.os.data$Ind))
item20.os.data$cond.ind <- 0
item20.os.data$cond.ind[which(item20.os.data$`Floor.Sub-Type` == "Conditioned")] <- 1
item20.os.data$count        <- 1


#####################################
# Weighted Analysis
#####################################
item20.os.final <- proportions_one_group(CustomerLevelData  = item20.os.data
                                      , valueVariable    = 'cond.ind'
                                      , groupingVariable = 'CK_Building_ID'
                                      , total.name       = "Region"
                                      , weighted = TRUE, osIndicator = "SCL", OS = T)
item20.os.final <- item20.os.final[which(item20.os.final$CK_Building_ID != "Total"),]

#subset by home type
item20.os.final.SF <- item20.os.final[which(item20.os.final$BuildingType == "Single Family"),-1]


#export data
exportTable(item20.os.final.SF, "SF", "Table 27", weighted = TRUE, osIndicator = "SCL", OS = T)



#####################################
# Unweighted Analysis
#####################################
item20.os.final <- proportions_one_group(CustomerLevelData  = item20.os.data
                                      , valueVariable    = 'cond.ind'
                                      , groupingVariable = 'CK_Building_ID'
                                      , total.name       = "Region"
                                      , weighted = FALSE, osIndicator = "SCL", OS = T)
item20.os.final <- item20.os.final[which(item20.os.final$CK_Building_ID != "Total"),]

#subset by home type
item20.os.final.SF <- item20.os.final[which(item20.os.final$BuildingType == "Single Family"),
                                -which(colnames(item20.os.final) %in% c("Remove", "BuildingType"))]


#export data
exportTable(item20.os.final.SF, "SF", "Table 27", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
# Item 21: DISTRIBUTION OF BASEMENT SLAB INSULATION BY INSULATION LEVEL (SF table 28)
#############################################################################################
item21.os.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"ENV_Foundation_BSMT_SlabInsulated_Y_N"
                                                               ,"ENV_Foundation_BSMT_SlabInsulationThickness"))]
colnames(item21.os.dat)
colnames(item21.os.dat) <- c("CK_Cadmus_ID", "BSMT_Slab_Insulated", "BSMT_Slab_Thickness")
unique(item21.os.dat$BSMT_Slab_Insulated)
unique(item21.os.dat$BSMT_Slab_Thickness)


item21.os.dat1 <- item21.os.dat[which(item21.os.dat$BSMT_Slab_Insulated %in% c("Yes", "No")),]
item21.os.dat1$BSMT_Slab_Thickness[which(item21.os.dat1$BSMT_Slab_Insulated == "No")] <- "None"
unique(item21.os.dat1$BSMT_Slab_Thickness)


item21.os.merge <- left_join(rbsa.dat, item21.os.dat1)
item21.os.merge <- item21.os.merge[which(!is.na(item21.os.merge$BSMT_Slab_Thickness)),]
item21.os.merge <- unique(item21.os.merge[which(item21.os.merge$BSMT_Slab_Thickness != "Unknown"),])


# apply weights to the subset of the data
item21.os.data <- weightedData(item21.os.merge[which(colnames(item21.os.merge) %notin% c("BSMT_Slab_Insulated"
                                                                                ,"BSMT_Slab_Thickness"))])
#merge back on measured variable
item21.os.data <- left_join(item21.os.data, item21.os.merge[which(colnames(item21.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"BSMT_Slab_Insulated"
                                                                                       ,"BSMT_Slab_Thickness"))])
item21.os.data$count <- 1
item21.os.data$Count <- 1

#####################################
# Weighted Analysis
#####################################
item21.os.summary <- proportionRowsAndColumns1(CustomerLevelData = item21.os.data
                                               ,valueVariable = "count"
                                               ,columnVariable = "CK_Building_ID"
                                               ,rowVariable = "BSMT_Slab_Thickness"
                                               ,aggregateColumnName = "Remove")
item21.os.summary <- item21.os.summary[which(item21.os.summary$CK_Building_ID != "Remove"),]

if(os.ind == "rbsa"){
  rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),] 
}else{
  rbsa.dat$CK_Building_ID <- rbsa.dat$Category
  rbsa.dat <- rbsa.dat[which(names(rbsa.dat) != "Category")]
}

item21.cast <- dcast(setDT(item21.os.summary)
                     ,formula = CK_Building_ID ~ BSMT_Slab_Thickness
                     ,value.var = c("w.percent","w.SE","count","n","N","EB"))

#export data
exportTable(item21.cast, "SF", "Table 28", weighted = TRUE, osIndicator = "SCL", OS = T)

#####################################
# Unweighted Analysis
#####################################
item21.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = item21.os.data
                                               ,valueVariable = "count"
                                               ,columnVariable = "CK_Building_ID"
                                               ,rowVariable = "BSMT_Slab_Thickness"
                                               ,aggregateColumnName = "Remove")
item21.os.summary <- item21.os.summary[which(item21.os.summary$CK_Building_ID != "Remove"),]

item21.cast <- dcast(setDT(item21.os.summary)
                     ,formula = CK_Building_ID ~ BSMT_Slab_Thickness
                     ,value.var = c("Percent","SE","Count","n"))

#export data
exportTable(item21.cast, "SF", "Table 28", weighted = FALSE, osIndicator = "SCL", OS = T)







#############################################################################################
# Item 22: PERCENTAGE OF HOMES WITH FLOOR AREA OVER CRAWLSPACE BY CK_Building_ID (SF table 29)
#############################################################################################
#subset envelope data to necessary columns
item22.os.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Foundation"
                                                               , "Conditioned.Living.Area"))]

item22.os.dat1 <- unique(item22.os.dat[grep("crawl|Crawl", item22.os.dat$Foundation),])

if(os.ind == "rbsa"){
  rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),] 
}else{
  rbsa.dat$CK_Building_ID <- rbsa.dat$Category
  rbsa.dat <- rbsa.dat[which(names(rbsa.dat) != "Category")]
}

item22.os.merge <- left_join(rbsa.dat, item22.os.dat1, by = "CK_Cadmus_ID")
item22.os.merge <- item22.os.merge[which(item22.os.merge$BuildingType == "Single Family"),]
item22.os.merge$count <- 1
item22.os.merge$FloorOverCrawl <- 0
item22.os.merge$FloorOverCrawl[which(item22.os.merge$Foundation == "Crawlspace")] <- 1


# apply weights to the subset of the data
item22.os.data <- weightedData(item22.os.merge[which(colnames(item22.os.merge) %notin% c("count"
                                                                                ,"Foundation"
                                                                                ,"Conditioned.Living.Area"
                                                                                ,"FloorOverCrawl"))])
#merge back on measured variable
item22.os.data <- left_join(item22.os.data, unique(item22.os.merge[which(colnames(item22.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Foundation"
                                                                                       ,"Conditioned.Living.Area"
                                                                                       ,"FloorOverCrawl"))]))

unique(item22.os.data$FloorOverCrawl)
item22.os.data$Ind <- item22.os.data$FloorOverCrawl
item22.os.data$Count <- 1
#####################################
# Weighted Analysis
#####################################
item22.os.final <- proportions_one_group(CustomerLevelData  = item22.os.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'CK_Building_ID'
                                      , total.name       = "Region"
                                      , weighted = TRUE, osIndicator = "SCL", OS = T)

#subset by home type
item22.os.final.SF <- item22.os.final[which(item22.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item22.os.final) %in% c("BuildingType"))]

#export data
exportTable(item22.os.final.SF, "SF", "Table 29", weighted = TRUE, osIndicator = "SCL", OS = T)

#####################################
# Unweighted Analysis
#####################################
item22.os.final <- proportions_one_group(CustomerLevelData  = item22.os.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'CK_Building_ID'
                                      , total.name       = "Region"
                                      , weighted = FALSE, osIndicator = "SCL", OS = T)

#subset by home type
item22.os.final.SF <- item22.os.final[which(item22.os.final$BuildingType == "Single Family")
                                ,which(colnames(item22.os.final) %notin% c("BuildingType"))]

#export data
exportTable(item22.os.final.SF, "SF", "Table 29", weighted = FALSE, osIndicator = "SCL", OS = T)
