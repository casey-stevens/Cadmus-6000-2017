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
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

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

item24.data$Crawlspace.Vents.Present.Indicator <- 0
item24.data$Crawlspace.Vents.Present.Indicator[which(item24.data$Crawlspace.Vents.Present == 'Yes')] <- 1

item24.final <- proportions_one_group(CustomerLevelData  = item24.data
                                      , valueVariable    = 'Crawlspace.Vents.Present.Indicator'
                                      , groupingVariable = 'State'
                                      , total.name       = "Total"
                                      , columnName       = "Insulated Crawlspace Walls"
                                      , weighted = TRUE)

item24.final.SF <- item24.final[which(item24.final$BuildingType == "Single Family"),-1]

exportTable(item24.final.SF, "SF", "Table 31")

# OLD CODE #
# 
# item24.final <- summarise(group_by(item24.dat2, BuildingType, State)
#                           ,InsulatedCount = sum(crawl.ins.ind)
#                           ,SampleSize     = sum(count)
#                           ,Percent        = InsulatedCount / SampleSize
#                           ,SE             = sqrt(Percent * (1 - Percent) / SampleSize)
# )
# 
# 
# item24.table <- data.frame("BuildingType" = item24.final$BuildingType
#                            ,"State" = item24.final$State
#                            ,"Percent" = item24.final$Percent
#                            ,"SE" = item24.final$SE
#                            ,"SampleSize" = item24.final$SampleSize)
# 







#############################################################################################
# Item 25: PERCENTAGE OF HOMES WITH ATTICS BY STATE (SF table 32)
#############################################################################################
item25.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.Type"))]

item25.dat0 <- left_join(rbsa.dat, item25.dat, by = "CK_Cadmus_ID")

item25.dat1 <- item25.dat0[which(item25.dat0$BuildingType == "Single Family"),]  # used to be item25.dat00

#item25.dat1 <- item25.dat00[which(item25.dat00$Ceiling.Type == "Attic"),]



item25.data <- weightedData(item25.dat1[-which(colnames(item25.dat1) %in% c("Ceiling.Type"))])

# Should see 'Joining, by = "CK_Cadmus_ID"'
item25.data <- left_join(item25.data, item25.dat1[which(colnames(item25.dat1) %in% c("CK_Cadmus_ID"
                                                                                     ,"Ceiling.Type"))])

item25.data$Ceiling.Type.Indicator <- 0
item25.data$Ceiling.Type.Indicator[which(item25.data$Ceiling.Type == 'Attic')] <- 1

item25.final <- proportions_one_group(CustomerLevelData  = item25.data
                                      , valueVariable    = 'Ceiling.Type.Indicator'
                                      , groupingVariable = 'State'
                                      , total.name       = "Total"
                                      , columnName       = "Homes with Attics"
                                      , weighted = TRUE)

item25.final.SF <- item25.final[which(item25.final$BuildingType == "Single Family"),-1]

exportTable(item25.final.SF, "SF", "Table 32")


# OLD CODE #
# 
# item25.cnt <- summarise(group_by(item25.dat1, BuildingType, State)
#                         , InsulatedCount = length(unique(CK_Cadmus_ID))
# )
# 
# item25.SS <- summarise(group_by(item25.dat00, BuildingType, State)
#                        , SampleSize = length(unique(CK_Cadmus_ID))
# )
# 
# item25.final <- left_join(item25.cnt, item25.SS, by = c("BuildingType", "State"))
# item25.final$Percent <- item25.final$InsulatedCount / item25.final$SampleSize
# item25.final$SE      <- sqrt(item25.final$Percent * (1 - item25.final$Percent) / item25.final$SampleSize)
# 
# item25.table <- data.frame("BuildingType" = item25.final$BuildingType
#                            ,"State" = item25.final$State
#                            ,"Percent" = item25.final$Percent
#                            ,"SE" = item25.final$SE
#                            ,"SampleSize" = item25.final$SampleSize)






#############################################################################################
# Item 28: PERCENTAGE OF HOMES WITH VAULT CEILINGS BY STATE (SF table 35)
#############################################################################################
item28.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.Type"))]

item28.dat0 <- left_join(rbsa.dat, item28.dat, by = "CK_Cadmus_ID")

item28.dat1 <- item28.dat0[which(item28.dat0$BuildingType == "Single Family"),] # used to be item28.dat00

#item28.dat1 <- item28.dat00[which(item28.dat00$Ceiling.Type == "Sloped / Vaulted (no attic)"),]

item28.data <- weightedData(item28.dat1[-which(colnames(item28.dat1) %in% c("Ceiling.Type"))])

# Should see 'Joining, by = "CK_Cadmus_ID"'
item28.data <- left_join(item28.data, item28.dat1[which(colnames(item28.dat1) %in% c("CK_Cadmus_ID"
                                                                                     ,"Ceiling.Type"))])

item28.data$Ceiling.Type.Indicator <- 0
item28.data$Ceiling.Type.Indicator[which(item28.data$Ceiling.Type == 'Sloped / Vaulted (no attic)')] <- 1

item28.final <- proportions_one_group(CustomerLevelData  = item28.data
                                      , valueVariable    = 'Ceiling.Type.Indicator'
                                      , groupingVariable = 'State'
                                      , total.name       = "Total"
                                      , columnName       = "Homes with Vault Ceilings"
                                      , weighted = TRUE)

item28.final.SF <- item28.final[which(item28.final$BuildingType == "Single Family"),-1]

exportTable(item28.final.SF, "SF", "Table 35")


# OLD CODE #
# 
# item28.cnt <- summarise(group_by(item28.dat1, BuildingType, State)
#                         , InsulatedCount = length(unique(CK_Cadmus_ID))
# )
# 
# item28.SS <- summarise(group_by(item28.dat00, BuildingType, State)
#                        , SampleSize = length(unique(CK_Cadmus_ID))
# )
# 
# item28.final <- left_join(item28.cnt, item28.SS, by = c("BuildingType", "State"))
# item28.final$Percent <- item28.final$InsulatedCount / item28.final$SampleSize
# item28.final$SE      <- sqrt(item28.final$Percent * (1 - item28.final$Percent) / item28.final$SampleSize)
# 
# item28.table <- data.frame("BuildingType" = item28.final$BuildingType
#                            ,"State" = item28.final$State
#                            ,"Percent" = item28.final$Percent
#                            ,"SE" = item28.final$SE
#                            ,"SampleSize" = item28.final$SampleSize)






#############################################################################################
# Item 29: PERCENTAGE OF HOMES WITH ROOF DECK CEILINGS BY STATE (SF table 36)
#############################################################################################
item29.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.Type"))]

item29.dat0 <- left_join(rbsa.dat, item29.dat, by = "CK_Cadmus_ID")

item29.dat1 <- item29.dat0[which(item29.dat0$BuildingType == "Single Family"),]  # used to be item28.dat00

#item29.dat1 <- item29.dat00[which(item29.dat00$Ceiling.Type == "Roof Deck"),]

item29.data <- weightedData(item29.dat1[-which(colnames(item29.dat1) %in% c("Ceiling.Type"))])

# Should see 'Joining, by = "CK_Cadmus_ID"'
item29.data <- left_join(item29.data, item29.dat1[which(colnames(item29.dat1) %in% c("CK_Cadmus_ID"
                                                                                     ,"Ceiling.Type"))])

item29.data$Ceiling.Type.Indicator <- 0
item29.data$Ceiling.Type.Indicator[which(item29.data$Ceiling.Type == 'Roof Deck')] <- 1

item29.final <- proportions_one_group(CustomerLevelData  = item29.data
                                      , valueVariable    = 'Ceiling.Type.Indicator'
                                      , groupingVariable = 'State'
                                      , total.name       = "Total"
                                      , columnName       = "Homes with Roof Deck Ceilings"
                                      , weighted = TRUE)

item29.final.SF <- item29.final[which(item29.final$BuildingType == "Single Family"),-1]

exportTable(item29.final.SF, "SF", "Table 36")


# OLD CODE #
# 
# item29.cnt <- summarise(group_by(item29.dat1, BuildingType, State)
#                         , InsulatedCount = length(unique(CK_Cadmus_ID))
# )
# 
# item29.SS <- summarise(group_by(item29.dat00, BuildingType, State)
#                        , SampleSize = length(unique(CK_Cadmus_ID))
# )
# 
# item29.final <- left_join(item29.cnt, item29.SS, by = c("BuildingType", "State"))
# item29.final$Percent <- item29.final$InsulatedCount / item29.final$SampleSize
# item29.final$SE      <- sqrt(item29.final$Percent * (1 - item29.final$Percent) / item29.final$SampleSize)
# 
# 
# item29.table <- data.frame("BuildingType" = item29.final$BuildingType
#                            ,"State" = item29.final$State
#                            ,"Percent" = item29.final$Percent
#                            ,"SE" = item29.final$SE
#                            ,"SampleSize" = item29.final$SampleSize)
