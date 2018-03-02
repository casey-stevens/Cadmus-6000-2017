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
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
# Item 162: PERCENTAGE OF ELECTRICALLY HEATED HOMES WITH FLOOR AREA OVER CRAWLSPACE BY STATE (SF table B-7)
#############################################################################################
#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item162.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item162.dat.11 <- item162.dat.1[which(item162.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item162.dat.12 <- item162.dat.11[which(item162.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item162.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item162.dat.12$Primary.Heating.System)
item162.dat.12$count <- 1

item162.dat.13 <- unique(item162.dat.12[which(item162.dat.12$Heating.Fuel == "Electric"),])

item162.sum <- summarise(group_by(item162.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item162.sum$Count <- 1
which(duplicated(item162.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item162.sum$Heating.Fuel)

item162.merge <- left_join(rbsa.dat, item162.sum)
item162.merge <- item162.merge[which(!is.na(item162.merge$Heating.Fuel)),]

item162.mechanical <- item162.merge






#############################################################################################
# Similar to Item 22 -- Not completed yet
#############################################################################################
#
#For Envelope information
#
#############################################################################################
item162.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                ,"Foundation"
                                                                ,""))]

item162.dat1 <- unique(item162.dat[grep("crawl|Crawl", item162.dat$Foundation),])

item162.dat2 <- left_join(rbsa.dat, item162.dat1, by = "CK_Cadmus_ID")

item162.dat2$FloorOverCrawl <- 0
item162.dat2$FloorOverCrawl[which(item162.dat2$Foundation == "Crawlspace")] <- 1

item162.sum <- summarise(group_by(item162.dat2, CK_Cadmus_ID)
                         ,Ind = sum(unique(FloorOverCrawl)))

item162.merge <- left_join(rbsa.dat, item162.sum)
item162.merge <- left_join(item162.merge, item162.mechanical)

#subset to only electrically heated homes
item162.merge1 <- item162.merge[which(item162.merge$Heating.Fuel == "Electric"),]


################################################
# Adding pop and sample sizes for weights
################################################
item162.data <- weightedData(item162.merge1[-which(colnames(item162.merge1) %in% c("Heating.Fuel"
                                                                                 ,"Count"
                                                                                 ,"Ind"))])
item162.data <- left_join(item162.data, item162.merge1[which(colnames(item162.merge1) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating.Fuel"
                                                                                           ,"Count"
                                                                                           ,"Ind"))])
#######################
# Weighted Analysis
#######################
item162.final <- proportions_one_group(CustomerLevelData = item162.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE
                                       ,two.prop.total = NA)

item162.final.SF <- item162.final[which(item162.final$BuildingType == "Single Family")
                                  ,-which(colnames(item162.final) %in% c("BuildingType"))]

exportTable(item162.final.SF, "SF", "Table B-7", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item162.final <- proportions_one_group(CustomerLevelData = item162.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE
                                       ,two.prop.total = NA)

item162.final.SF <- item162.final[which(item162.final$BuildingType == "Single Family")
                                  ,-which(colnames(item162.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item162.final.SF, "SF", "Table B-7", weighted = FALSE)

