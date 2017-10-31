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


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

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
length(unique(item19.dat1$CK_Cadmus_ID))#601

#subset to only single family homes
item19.dat2 <- item19.dat1[which(item19.dat1$BuildingType == "Single Family"),]
item19.dat2$count <- 1

item19.dat2$Basement.Ind <- 0
item19.dat2$Basement.Ind[which(item19.dat2$Floor.Type == "Basement")] <- 1

item19.dat3 <- unique(item19.dat2[which(item19.dat2$Basement.Ind == 1),])

item19.merge <- left_join(rbsa.dat, item19.dat3)

item19.merge1 <- item19.merge[-which(item19.merge$`Floor.Sub-Type` == "Unknown"),]

# apply weights to the subset of the data
item19.data <- weightedData(item19.merge1[which(colnames(item19.merge1) %notin% c("count"
                                                                                ,"Floor.Type"
                                                                                ,"Floor.Sub-Type"
                                                                                ,"Basement.Ind"
                                                                                ,"cond.ind"))])
#merge back on measured variable
item19.data <- left_join(item19.data, item19.merge1[which(colnames(item19.merge1) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Floor.Type"
                                                                                       ,"Floor.Sub-Type"
                                                                                       ,"Basement.Ind"
                                                                                       ,"cond.ind"))])

item19.data$Basement.Ind[which(is.na(item19.data$Basement.Ind))] <- 0
item19.data$Basement.Ind <- as.numeric(as.character(item19.data$Basement.Ind))
item19.data$count <- 1


item19.final <- proportions_one_group(CustomerLevelData  = item19.data
                                     , valueVariable    = 'Basement.Ind'
                                     , groupingVariable = 'State'
                                     , total.name       = "Region"
                                     , weighted = TRUE)

#subset by home type
item19.final.SF <- item19.final[which(item19.final$BuildingType == "Single Family"),]


#export data
exportTable(item19.final.SF, "SF", "Table 26"
            , weighted = TRUE)




# #summarise -- State JUST Basement
# item19.sum1 <- summarise(group_by(item19.dat3, BuildingType, State)
#                          , BSMTCount = sum(count))
# #summarise -- State All Sites Basement
# item19.sum2 <- summarise(group_by(item19.dat2, BuildingType, State)
#                          , SampleSize = length(unique(CK_Cadmus_ID)))
# 
# #summarise -- Region JUST Basement
# item19.sum3 <- summarise(group_by(item19.dat3, BuildingType)
#                          , State = "Region"
#                          , BSMTCount = sum(count))
# #summarise -- Region All Sites Basement
# item19.sum4 <- summarise(group_by(item19.dat2, BuildingType)
#                          , State = "Region"
#                          , SampleSize = length(unique(CK_Cadmus_ID)))
# 
# ## rbind state and region sample sizes
# item19.tmp1 <- left_join(item19.sum1, item19.sum2, by = c("BuildingType", "State"))
# item19.tmp2 <- left_join(item19.sum3, item19.sum4, by = c("BuildingType", "State"))
# 
# item19.final <- rbind.data.frame(item19.tmp1, item19.tmp2, stringsAsFactors = F)
# 
# item19.final$Percent <- item19.final$BSMTCount / item19.final$SampleSize
# item19.final$SE <- sqrt(item19.final$Percent * (1 - item19.final$Percent) / item19.final$SampleSize)
# 
# 
# item19.table <- data.frame("BuildingType" = item19.final$BuildingType
#                            ,"State" = item19.final$State
#                            ,"Percent" = item19.final$Percent
#                            ,"SE" = item19.final$SE
#                            ,"SampleSize" = item19.final$SampleSize)








#############################################################################################
# Item 20: PERCENTAGE OF BASEMENTS THAT ARE CONDITIONED BY STATE (SF table 27)
#############################################################################################
item20.dat <- item19.merge1[which(item19.merge1$Floor.Type == "Basement"),]

# apply weights to the subset of the data
item20.data <- weightedData(item20.dat[which(colnames(item20.dat) %notin% c("count"
                                                                            ,"Floor.Type"
                                                                            ,"Floor.Sub-Type"
                                                                            ,"Basement.Ind"
                                                                            ,"cond.ind"))])
#merge back on measured variable
item20.data <- left_join(item20.data, item20.dat[which(colnames(item20.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"count"
                                                                                   ,"Floor.Type"
                                                                                   ,"Floor.Sub-Type"
                                                                                   ,"Basement.Ind"
                                                                                   ,"cond.ind"))])

item20.data$Basement.Ind[which(is.na(item20.data$Basement.Ind))] <- 0
item20.data$Basement.Ind <- as.numeric(as.character(item20.data$Basement.Ind))
item20.data$cond.ind <- 0
item20.data$cond.ind[which(item20.data$`Floor.Sub-Type` == "Conditioned")] <- 1
item20.data$count        <- 1


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











# item20.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
#                                                                ,"Floor.Type"
#                                                                ,"Floor.Sub-Type"))]
# 
# item20.dat1 <- left_join(rbsa.dat, item20.dat, by = "CK_Cadmus_ID")
# length(unique(item20.dat1$CK_Cadmus_ID))#565
# 
# #subset to only single family homes
# item20.dat2 <- item20.dat1[which(item20.dat1$BuildingType == "Single Family"),]
# item20.dat2$count <- 1
# 
# item20.dat3 <- item20.dat2[which(item20.dat2$Floor.Type == "Basement"),]
# item20.dat3$cond.ind <- 0
# item20.dat3$cond.ind[which(item20.dat3$`Floor.Sub-Type` == "Conditioned")] <- 1
# 
# #summarise -- State
# item20.sum1 <- summarise(group_by(item20.dat3, BuildingType, State)
#                          , ConditionedCount = sum(cond.ind) 
#                          , SampleSize = sum(count)
#                          , Percent = ConditionedCount / SampleSize
#                          , SE = sqrt(Percent * (1 - Percent) / SampleSize))
# ##Region
# item20.sum2 <- summarise(group_by(item20.dat3, BuildingType)
#                          , State = "Region"
#                          , ConditionedCount = sum(cond.ind) 
#                          , SampleSize = sum(count)
#                          , Percent = ConditionedCount / SampleSize
#                          , SE = sqrt(Percent * (1 - Percent) / SampleSize))
# ## rbind state and region sample sizes
# item20.final <- rbind.data.frame(item20.sum1, item20.sum2, stringsAsFactors = F)
# 
# item20.table <- data.frame("BuildingType" = item20.final$BuildingType
#                            ,"State" = item20.final$State
#                            ,"Percent" = item20.final$Percent
#                            ,"SE" = item20.final$SE
#                            ,"SampleSize" = item20.final$SampleSize)


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

# apply weights to the subset of the data
item21.data <- weightedData(item21.merge[which(colnames(item21.merge) %notin% c("BSMT_Slab_Insulated"
                                                                            ,"BSMT_Slab_Thickness"))])
#merge back on measured variable
item21.data <- left_join(item21.data, item21.merge[which(colnames(item21.merge) %in% c("CK_Cadmus_ID"
                                                                                        ,"BSMT_Slab_Insulated"
                                                                                      ,"BSMT_Slab_Thickness"))])
item21.data$count        <- 1

item21.final <- proportions_one_group(CustomerLevelData  = item21.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'BSMT_Slab_Thickness'
                                      , total.name       = "Total"
                                      , weighted = TRUE)

#subset by home type
item21.final.SF <- item21.final[which(item21.final$BuildingType == "Single Family"),-1]


#export data
exportTable(item21.final.SF, "SF", "Table 28"
            , weighted = TRUE)






#############################################################################################
# Item 22: PERCENTAGE OF HOMES WITH FLOOR AREA OVER CRAWLSPACE BY STATE (SF table 29)
#############################################################################################
#subset envelope data to necessary columns
item22.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Foundation"
                                                               , ""))]

item22.dat1 <- unique(item22.dat[grep("crawl|Crawl", item22.dat$Foundation),])

item22.merge <- left_join(rbsa.dat, item22.dat1, by = "CK_Cadmus_ID")

item22.merge$count <- 1
item22.merge$FloorOverCrawl <- 0
item22.merge$FloorOverCrawl[which(item22.merge$Foundation == "Crawlspace")] <- 1


# apply weights to the subset of the data
item22.data <- weightedData(item22.merge[which(colnames(item22.merge) %notin% c("count"
                                                                                ,"Foundation"
                                                                                ,"FloorOverCrawl"))])
#merge back on measured variable
item22.data <- left_join(item22.data, item22.merge[which(colnames(item22.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"count"
                                                                                         ,"Foundation"
                                                                                         ,"FloorOverCrawl"))])

unique(item22.data$FloorOverCrawl)

item22.final <- proportions_one_group(CustomerLevelData  = item22.data
                                      , valueVariable    = 'FloorOverCrawl'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , weighted = TRUE)

#subset by home type
item22.final.SF <- item22.final[which(item22.final$BuildingType == "Single Family"),]


#export data
exportTable(item22.final.SF, "SF", "Table 29"
            , weighted = TRUE)

 #subset envelope data to necessary columns
# item22.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
#                                                                , "Foundation"
#                                                                , ""))]
# 
# item22.dat1 <- unique(item22.dat[grep("crawl|Crawl", item22.dat$Foundation),])
# 
# item22.dat2 <- left_join(rbsa.dat, item22.dat1, by = "CK_Cadmus_ID")
# 
# item22.dat2$count <- 1
# item22.dat2$FloorOverCrawl <- 0
# item22.dat2$FloorOverCrawl[which(item22.dat2$Foundation == "Crawlspace")] <- 1
# 
# #SUBSET TO ONLY SINGLE FAMILY
# item22.dat3 <- item22.dat2[which(item22.dat2$BuildingType == "Single Family"),]
# 
# item22.state <- summarise(group_by(item22.dat3, State)
#                           ,Percent = sum(FloorOverCrawl) / sum(count)
#                           ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
#                           ,SampleSize = length(unique(CK_Cadmus_ID)))
# 
# item22.region <- summarise(group_by(item22.dat3)
#                            ,State = "Region"
#                            ,Percent = sum(FloorOverCrawl) / sum(count)
#                            ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
#                            ,SampleSize = length(unique(CK_Cadmus_ID)))
# 
# item22.final <- rbind.data.frame(item22.state, item22.region, stringsAsFactors = F)

