#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())
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
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID))

#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))



#############################################################################################
# Halogen queries
#############################################################################################
#subset to columns needed for analysis
halogen.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
halogen.dat$count <- 1

halogen.dat0 <- halogen.dat[which(halogen.dat$Lamp.Category == "Halogen"),]

halogen.dat1 <- left_join(rbsa.dat, halogen.dat0, by = "CK_Cadmus_ID")

halogen.dat2 <- halogen.dat1[grep("SITE", halogen.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
halogen.dat2$Fixture.Qty <- as.numeric(as.character(halogen.dat2$Fixture.Qty))
halogen.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(halogen.dat2$LIGHTING_BulbsPerFixture))

halogen.dat2$Lamps <- halogen.dat2$Fixture.Qty * halogen.dat2$LIGHTING_BulbsPerFixture
unique(halogen.dat2$Lamps)

halogen.dat3 <- halogen.dat2[which(!(is.na(halogen.dat2$Lamps))),]

halogen.cfl.sum <- summarise(group_by(halogen.dat3, CK_Cadmus_ID)
                            ,TotalBulbs = sum(Lamps))

halogen.merge1 <- left_join(rbsa.dat, halogen.cfl.sum)

## subset to only storage bulbs
halogen.storage <- halogen.dat3[which(halogen.dat3$Clean.Room == "Storage"),]
#summarise within site
halogen.storage.sum <- summarise(group_by(halogen.storage, CK_Cadmus_ID)
                                ,StorageBulbs = sum(Lamps))

halogen.merge2 <- left_join(halogen.merge1, halogen.storage.sum)
halogen.merge <- halogen.merge2[which(!is.na(halogen.merge2$TotalBulbs)),]
halogen.merge$StorageBulbs[which(is.na(halogen.merge$StorageBulbs))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
halogen.data <- weightedData(halogen.merge[-which(colnames(halogen.merge) %in% c("StorageBulbs"
                                                                              ,"TotalBulbs"))])
halogen.data <- left_join(halogen.data, halogen.merge[which(colnames(halogen.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"StorageBulbs"
                                                                                       ,"TotalBulbs"))])
halogen.data$count <- 1


# test
CustomerLevelData <- halogen.data
valueVariable     <- "TotalBulbs"
byVariable        <- "State"
aggregateRow      <- "Region"

# mean_one_group <- function(CustomerLevelData, valueVariable,
#                            byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  Popandns <- data.frame(ddply(CustomerLevelData
                               , c("State", "Region", "Territory"), summarise
                               ,n_h        = sum(unique(n.h))
                               ,N_h        = sum(unique(N.h))), stringsAsFactors = F)
  
  
  item.strata <- data.frame(ddply(CustomerLevelData
                                  , c("State", "Region", "Territory"), summarise
                                  ,strataMean = mean(get(valueVariable), na.rm = T)
                                  # ,strataSD   = sd(get(valueVariable), na.rm = T)
                                  ,n_hj       = length(unique(CK_Cadmus_ID))
                                  ), stringsAsFactors = F)
  #QAQC
  stopifnot(item.strata$n_hj == item.strata$n_h)
  item.strata <- left_join(item.strata, Popandns)
  
  # item.strata$strataSD[which(item.strata$strataSD %in% c("N/A","NaN",NA))] <- 0
  # item.strata$strataMean[which(item.strata$strataMean %in% c("N/A","NaN",NA))] <- 0
  
  ######################################################
  # weighted means and SEs by grouping variables
  ######################################################
  item.group <- data.frame(ddply(item.strata, c(byVariable), summarise
                                 ,Total = sum(N_h * strataMean) #/ sum(N_h)
                                 # ,SE   = sqrt(sum(N_h^2 * (1 / n_h) * (1 - n_h / N_h) * strataSD^2)) / sum(N_h)
                                 ,n    = sum(n_hj)
                                 ,n_h  = sum(n_h)
                                 ,N_h  = sum(N_h)
                                 # ,EB   = SE * qt(1-(1-0.9)/2, n-length(strataMean))
                                 # ,Precision = EB / Mean
                                 ), stringsAsFactors = F)
  
  item.region <- data.frame(summarise(item.strata
                                  ,byRow  = aggregateRow
                                  ,Total   = sum(N_h * strataMean) #/ sum(N_h)
                                  # ,SE     = sqrt(sum(N_h^2 * (1 / n_h) * (1 - n_h / N_h) * strataSD^2)) / sum(N_h)
                                  ,n      = sum(n_h)
                                  ,n_h    = sum(n_h)
                                  ,N_h    = sum(N_h)
                                  # ,EB     = SE * qt(1-(1-0.9)/2, n-length(strataMean))
                                  # ,Precision = EB / Mean
                                  ), stringsAsFactors = F)
  #rename columns
  colnames(item.region)[which(colnames(item.region) == 'byRow')] <- byVariable
  
  item.final <- rbind.data.frame(item.group, item.region, stringsAsFactors = F)
  View(item.final)
#   return(item.final)
# }


