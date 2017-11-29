#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:          10/31/2017                                   
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

# Bring in clean ground contact types
GroundContactTypes <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "Ground Contact Types.xlsx"), sheet = 1)
GroundContactTypes <- GroundContactTypes[which(colnames(GroundContactTypes) != "Notes")]



#############################################################################################
# Item 24: PERCENTAGE OF CRAWLSPACES WITH INSULATED WALLS BY STATE (SF table 31)
#############################################################################################
item24.dat <- envelope.dat[grep("CK_Cadmus_ID|Crawlspace", colnames(envelope.dat))]

item24.dat1 <- left_join(rbsa.dat, item24.dat, by = "CK_Cadmus_ID")
length(unique(item24.dat1$CK_Cadmus_ID))

item24.dat2 <- item24.dat1[which(item24.dat1$`Crawlspace.Walls.Insulated?` %in% c("Yes", "No")),]
length(unique(item24.dat2$CK_Cadmus_ID))

item24.dat2$count <- 1
item24.dat2$crawl.ins.ind <- 0
item24.dat2$crawl.ins.ind[which(item24.dat2$`Crawlspace.Walls.Insulated?` == "Yes")] <- 1

item24.data <- weightedData(item24.dat2[-which(colnames(item24.dat2) %in% c("Crawlspace.Vents.Present"
                                                                            ,"Crawlspace.Vents.Blocked"
                                                                            ,"Crawlspace.Walls.Insulated?"
                                                                            ,"Crawlspace.Wall.Insulation.Type.1"
                                                                            ,"Crawlspace.Wall.Insulation.Thickness.1"
                                                                            ,"Crawlspace.Wall.Insulation.Condition.1" 
                                                                            ,"Crawlspace.Wall.Insulation.Type.2"
                                                                            ,"Crawlspace.Wall.Insulation.Thickness.2"
                                                                            ,"Crawlspace.Wall.Insulation.Condition.2"
                                                                            ,"Crawlspace.Wall.Exteriors.Insulated?"
                                                                            ,"Crawlspace.Wall.Exterior.Insulation.Type.1"
                                                                            ,"Crawlspace.Wall.Exterior.Insulation.Thickness.1"
                                                                            ,"Crawlspace.Wall.Exterior.Insulation.Condition.1"
                                                                            ,"Crawlspace.Wall.Exterior.Insulation.Type.2"
                                                                            ,"Crawlspace.Wall.Exterior.Insulation.Thickness.2"
                                                                            ,"Crawlspace.Wall.Exterior.Insulation.Condition.2"
                                                                            ,"count"
                                                                            ,"crawl.ins.ind"))])

# Should see 'Joining, by = "CK_Cadmus_ID"'
item24.data <- left_join(item24.data, item24.dat2[which(colnames(item24.dat2) %in% c("CK_Cadmus_ID"
                                                                                      ,"Crawlspace.Vents.Present"
                                                                                      ,"Crawlspace.Vents.Blocked"
                                                                                      ,"Crawlspace.Walls.Insulated?"
                                                                                      ,"Crawlspace.Wall.Insulation.Type.1"
                                                                                      ,"Crawlspace.Wall.Insulation.Thickness.1"
                                                                                      ,"Crawlspace.Wall.Insulation.Condition.1" 
                                                                                      ,"Crawlspace.Wall.Insulation.Type.2"
                                                                                      ,"Crawlspace.Wall.Insulation.Thickness.2"
                                                                                      ,"Crawlspace.Wall.Insulation.Condition.2"
                                                                                      ,"Crawlspace.Wall.Exteriors.Insulated?"
                                                                                      ,"Crawlspace.Wall.Exterior.Insulation.Type.1"
                                                                                      ,"Crawlspace.Wall.Exterior.Insulation.Thickness.1"
                                                                                      ,"Crawlspace.Wall.Exterior.Insulation.Condition.1"
                                                                                      ,"Crawlspace.Wall.Exterior.Insulation.Type.2"
                                                                                      ,"Crawlspace.Wall.Exterior.Insulation.Thickness.2"
                                                                                      ,"Crawlspace.Wall.Exterior.Insulation.Condition.2"
                                                                                      ,"count"
                                                                                      ,"crawl.ins.ind"))])

##################################
# Weighted - Single Family
##################################
item24.final <- proportions_one_group(CustomerLevelData  = item24.data
                                      , valueVariable    = 'crawl.ins.ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "Insulated Crawlspace Walls"
                                      , weighted = TRUE)

item24.final.SF <- item24.final[which(item24.final$BuildingType == "Single Family"),-1]

exportTable(item24.final.SF, "SF", "Table 31", weighted = TRUE)


##################################
# Unweighted - Single Family
##################################
item24.final <- proportions_one_group(CustomerLevelData  = item24.data
                                      , valueVariable    = 'crawl.ins.ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Total"
                                      , columnName       = "Insulated Crawlspace Walls"
                                      , weighted = FALSE)

item24.final.SF <- item24.final[which(item24.final$BuildingType == "Single Family"),-1]

exportTable(item24.final.SF, "SF", "Table 31", weighted = FALSE)






#############################################################################################
# Item 25: PERCENTAGE OF HOMES WITH ATTICS BY STATE (SF table 32)
#############################################################################################
item25.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.Type"))]

item25.dat0 <- left_join(rbsa.dat, item25.dat, by = "CK_Cadmus_ID")

item25.dat1 <- item25.dat0[which(item25.dat0$BuildingType == "Single Family"),]  # used to be item25.dat00

item25.dat1$Ceiling.Type.Indicator <- 0
item25.dat1$Ceiling.Type.Indicator[which(item25.dat1$Ceiling.Type == 'Attic')] <- 1

item25.summary <- summarise(group_by(item25.dat1, CK_Cadmus_ID)
                            ,Ceiling.Ind = sum(unique(Ceiling.Type.Indicator)))

item25.merge <- left_join(rbsa.dat, item25.summary)
item25.merge <- item25.merge[which(!is.na(item25.merge$Ceiling.Ind)),]


item25.data <- weightedData(item25.merge[-which(colnames(item25.merge) %in% c("Ceiling.Ind"))])

# Should see 'Joining, by = "CK_Cadmus_ID"'
item25.data <- left_join(item25.data, item25.merge[which(colnames(item25.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"Ceiling.Ind"))])

##################################
# Weighted
##################################
item25.final <- proportions_one_group(CustomerLevelData  = item25.data
                                      , valueVariable    = 'Ceiling.Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "Homes with Attics"
                                      , weighted = TRUE)

item25.final.SF <- item25.final[which(item25.final$BuildingType == "Single Family"),-1]

exportTable(item25.final.SF, "SF", "Table 32", weighted = TRUE)

##################################
# Unweighted
##################################
item25.final <- proportions_one_group(CustomerLevelData  = item25.data
                                      , valueVariable    = 'Ceiling.Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "Homes with Attics"
                                      , weighted = FALSE)

item25.final.SF <- item25.final[which(item25.final$BuildingType == "Single Family")
                                ,-which(colnames(item25.final) %in% c("BuildingType", "Homes.with.Attics", "Total.Count"))]

exportTable(item25.final.SF, "SF", "Table 32", weighted = FALSE)




#############################################################################################
# Item 28: PERCENTAGE OF HOMES WITH VAULT CEILINGS BY STATE (SF table 35)
#############################################################################################
item28.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.Type"))]

item28.dat0 <- left_join(rbsa.dat, item28.dat, by = "CK_Cadmus_ID")

item28.dat1 <- item28.dat0[which(item28.dat0$BuildingType == "Single Family"),] # used to be item28.dat00

item28.dat1$Ceiling.Type.Indicator <- 0
item28.dat1$Ceiling.Type.Indicator[which(item28.dat1$Ceiling.Type == 'Sloped / Vaulted (no attic)')] <- 1

item28.summary <- summarise(group_by(item28.dat1, CK_Cadmus_ID)
                            ,Ceiling.Ind = sum(unique(Ceiling.Type.Indicator)))

item28.merge <- left_join(rbsa.dat, item28.summary)
item28.merge <- item28.merge[which(!is.na(item28.merge$Ceiling.Ind)),]


item28.data <- weightedData(item28.merge[-which(colnames(item28.merge) %in% c("Ceiling.Ind"))])

# Should see 'Joining, by = "CK_Cadmus_ID"'
item28.data <- left_join(item28.data, item28.merge[which(colnames(item28.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"Ceiling.Ind"))])

##################################
# Weighted
##################################
item28.final <- proportions_one_group(CustomerLevelData  = item28.data
                                      , valueVariable    = 'Ceiling.Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "Remove"
                                      , weighted = TRUE)

item28.final.SF <- item28.final[which(item28.final$BuildingType == "Single Family"),-1]

exportTable(item28.final.SF, "SF", "Table 35", weighted = TRUE)

##################################
# Unweighted
##################################
item28.final <- proportions_one_group(CustomerLevelData  = item28.data
                                      , valueVariable    = 'Ceiling.Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "Remove"
                                      , weighted = FALSE)

item28.final.SF <- item28.final[which(item28.final$BuildingType == "Single Family")
                                ,-which(colnames(item25.final) %in% c("BuildingType", "Homes.with.Attics", "Total.Count"))]

exportTable(item28.final.SF, "SF", "Table 35", weighted = FALSE)





#############################################################################################
# Item 29: PERCENTAGE OF HOMES WITH ROOF DECK CEILINGS BY STATE (SF table 36)
#############################################################################################
item29.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.Type"))]

item29.dat0 <- left_join(rbsa.dat, item29.dat, by = "CK_Cadmus_ID")

item29.dat1 <- item29.dat0[which(item29.dat0$BuildingType == "Single Family"),]  # used to be item28.dat00

item29.dat1$Ceiling.Type.Indicator <- 0
item29.dat1$Ceiling.Type.Indicator[which(item29.dat1$Ceiling.Type == 'Roof Deck')] <- 1

item29.summary <- summarise(group_by(item29.dat1, CK_Cadmus_ID)
                            ,Ceiling.Ind = sum(unique(Ceiling.Type.Indicator)))

item29.merge <- left_join(rbsa.dat, item29.summary)
item29.merge <- item29.merge[which(!is.na(item29.merge$Ceiling.Ind)),]

item29.data <- weightedData(item29.merge[-which(colnames(item29.merge) %in% c("Ceiling.Ind"))])

# Should see 'Joining, by = "CK_Cadmus_ID"'
item29.data <- left_join(item29.data, item29.merge[which(colnames(item29.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"Ceiling.Ind"))])


##################################
# Weighted
##################################
item29.final <- proportions_one_group(CustomerLevelData  = item29.data
                                      , valueVariable    = 'Ceiling.Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Total"
                                      , columnName       = "Remove"
                                      , weighted = TRUE)

item29.final.SF <- item29.final[which(item29.final$BuildingType == "Single Family"),-1]

exportTable(item29.final.SF, "SF", "Table 36", weighted = TRUE)


##################################
# Unweighted
##################################
item29.final <- proportions_one_group(CustomerLevelData  = item29.data
                                      , valueVariable    = 'Ceiling.Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Total"
                                      , columnName       = "Remove"
                                      , weighted = FALSE)

item29.final.SF <- item29.final[which(item29.final$BuildingType == "Single Family")
                                ,-which(colnames(item25.final) %in% c("BuildingType", "Homes.with.Attics", "Total.Count"))]


exportTable(item29.final.SF, "SF", "Table 36", weighted = FALSE)
