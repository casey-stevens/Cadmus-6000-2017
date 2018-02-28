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

#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))






#############################################################################################
# Item 296: DISTRIBUTION OF IN-UNIT CLOTHES WASHERS BY TYPE AND VINTAGE (MF Table 90)
#############################################################################################
#subset to columns needed for analysis
item296.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"Age"
                                                                    ,"Type"
                                                                    ,"Washer.Type"
                                                                    ,""))]
#join clean rbsa data onto appliances analysis data
item296.dat1 <- left_join(rbsa.dat, item296.dat, by = "CK_Cadmus_ID")

#subset to only MF
item296.dat2 <- item296.dat1[grep("Multifamily", item296.dat1$BuildingType),]
length(unique(item296.dat2$CK_Cadmus_ID))

#subset to only washers
item296.dat3 <- item296.dat2[which(item296.dat2$Type %in% c("Washer")),]

#subset to only common area washers
item296.dat4 <- item296.dat3[grep("SITE", item296.dat3$CK_SiteID),]

#subset to only common area washers that have observed age info
item296.dat5 <- item296.dat4[which(item296.dat4$Age > 0),]


####################
# Clean AGE
####################
item296.dat5$Age <- as.numeric(as.character(item296.dat5$Age))
item296.dat5$Washer.Age <- as.numeric(as.character(item296.dat5$Age))
item296.dat5$Washer.Age[which(item296.dat5$Age < 1980)] <- "Pre 1980"
item296.dat5$Washer.Age[which(item296.dat5$Age >= 1980 & item296.dat5$Age < 1990)] <- "1980-1989"
item296.dat5$Washer.Age[which(item296.dat5$Age >= 1990 & item296.dat5$Age < 1995)] <- "1990-1994"
item296.dat5$Washer.Age[which(item296.dat5$Age >= 1995 & item296.dat5$Age < 2000)] <- "1995-1999"
item296.dat5$Washer.Age[which(item296.dat5$Age >= 2000 & item296.dat5$Age < 2005)] <- "2000-2004"
item296.dat5$Washer.Age[which(item296.dat5$Age >= 2005 & item296.dat5$Age < 2009)] <- "2005-2009"
item296.dat5$Washer.Age[which(item296.dat5$Age >= 2009)] <- "Post 2009"
unique(item296.dat5$Washer.Age)

item296.dat5$Washer.Type[grep("Stacked|stacked|top", item296.dat5$Washer.Type)] <- "Stacked Washer/Dryer"
item296.dat5$Washer.Type[grep("Wager|combo", item296.dat5$Washer.Type)] <- "Combined Washer/Dryer"
unique(item296.dat5$Washer.Type)
####################
# end cleaning
####################

item296.dat6 <- item296.dat5[which(item296.dat5$Washer.Type != "Unknown"),]
item296.dat7 <- item296.dat6[which(!is.na(item296.dat6$Washer.Age)),]

######################################
#Pop and Sample Sizes for weights
######################################
item296.data <- weightedData(item296.dat7[which(colnames(item296.dat7) %notin% c("CK_SiteID"
                                                                                 ,"Type"
                                                                                   ,"Age"
                                                                                   ,"Washer.Type"
                                                                                   ,"Washer.Age"))])

item296.data <- left_join(item296.data, item296.dat7[which(colnames(item296.dat7) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Type"
                                                                                         ,"Age"
                                                                                         ,"Washer.Type"
                                                                                         ,"Washer.Age"))])
item296.data$count <- 1


######################
# weighted analysis
######################
item296.summary <- proportionRowsAndColumns1(CustomerLevelData = item296.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Washer.Age'
                                             ,rowVariable = 'Washer.Type'
                                             ,aggregateColumnName = "All Vintages")
item296.summary$Washer.Type[which(item296.summary$Washer.Type == "Total")] <- "All Types"


item296.cast <- dcast(setDT(item296.summary)
                      ,formula = Washer.Age ~ Washer.Type
                      ,value.var = c("w.percent", "w.SE","count","n", "N","EB"))
names(item296.cast)
item296.table <- data.frame("Clothes.Washer.Age"                 = item296.cast$Washer.Age
                            ,"Combined.Washer.Dryer"             = NA#item296.cast$`w.percent_Combined Washer/Dryer`
                            ,"Combined.Washer.Dryer.SE"          = NA#item296.cast$`w.SE_Combined Washer/Dryer`
                            ,"Horizontal.Axis"                   = item296.cast$`w.percent_Horizontal Axis`
                            ,"Horizontal.Axis.SE"                = item296.cast$`w.SE_Horizontal Axis`
                            ,"Stacked.Washer.Dryer"              = item296.cast$`w.percent_Stacked Washer/Dryer`
                            ,"Stacked.Washer.Dryer.SE"           = item296.cast$`w.SE_Stacked Washer/Dryer`
                            ,"Vertical.Axis.with.Agitator"       = item296.cast$`w.percent_Vertical Axis (with agitator)`
                            ,"Vertical.Axis.with.Agitator.SE"    = item296.cast$`w.SE_Vertical Axis (with agitator)`
                            ,"Vertical.Axis.without.Agitator"    = item296.cast$`w.percent_Vertical Axis (without agitator)`
                            ,"Vertical.Axis.without.Agitator.SE" = item296.cast$`w.SE_Vertical Axis (without agitator)`
                            ,"All.Types"                         = item296.cast$`w.percent_All Types`
                            ,"All.Types.SE"                      = item296.cast$`w.SE_All Types`
                            ,"n"                                 = item296.cast$`n_All Types`
                            ,"Combined.Washer.Dryer.EB"          = NA#item296.cast$`w.EB_Combined Washer/Dryer`
                            ,"Horizontal.Axis.EB"                = item296.cast$`EB_Horizontal Axis`
                            ,"Stacked.Washer.Dryer.EB"           = item296.cast$`EB_Stacked Washer/Dryer`
                            ,"Vertical.Axis.with.Agitator.EB"    = item296.cast$`EB_Vertical Axis (with agitator)`
                            ,"Vertical.Axis.without.Agitator.EB" = item296.cast$`EB_Vertical Axis (without agitator)`
                            ,"All.Types.EB"                      = item296.cast$`EB_All Types`)

levels(item296.table$Clothes.Washer.Age)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"Post 2009"
              ,"All Vintages")
item296.table <- item296.table %>% mutate(Clothes.Washer.Age = factor(Clothes.Washer.Age, levels = rowOrder)) %>% arrange(Clothes.Washer.Age)  
item296.table <- data.frame(item296.table)

exportTable(item296.table, "MF", "Table 90", weighted = TRUE)

######################
# unweighted analysis
######################
item296.summary <- proportions_two_groups_unweighted(CustomerLevelData = item296.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Washer.Age'
                                             ,rowVariable = 'Washer.Type'
                                             ,aggregateColumnName = "All Vintages")
item296.summary$Washer.Type[which(item296.summary$Washer.Type == "Total")] <- "All Types"


item296.cast <- dcast(setDT(item296.summary)
                      ,formula = Washer.Age ~ Washer.Type
                      ,value.var = c("Percent", "SE","Count","n"))

item296.table <- data.frame("Clothes.Washer.Age"                 = item296.cast$Washer.Age
                            ,"Combined.Washer.Dryer"             = NA#item296.cast$`Percent_Combined Washer/Dryer`
                            ,"Combined.Washer.Dryer.SE"          = NA#item296.cast$`SE_Combined Washer/Dryer`
                            ,"Horizontal.Axis"                   = item296.cast$`Percent_Horizontal Axis`
                            ,"Horizontal.Axis.SE"                = item296.cast$`SE_Horizontal Axis`
                            ,"Stacked.Washer.Dryer"              = item296.cast$`Percent_Stacked Washer/Dryer`
                            ,"Stacked.Washer.Dryer.SE"           = item296.cast$`SE_Stacked Washer/Dryer`
                            ,"Vertical.Axis.with.Agitator"       = item296.cast$`Percent_Vertical Axis (with agitator)`
                            ,"Vertical.Axis.with.Agitator.SE"    = item296.cast$`SE_Vertical Axis (with agitator)`
                            ,"Vertical.Axis.without.Agitator"    = item296.cast$`Percent_Vertical Axis (without agitator)`
                            ,"Vertical.Axis.without.Agitator.SE" = item296.cast$`SE_Vertical Axis (without agitator)`
                            ,"All.Types"                         = item296.cast$`Percent_All Types`
                            ,"All.Types.SE"                      = item296.cast$`SE_All Types`
                            ,"n"                                 = item296.cast$`n_All Types`)

levels(item296.table$Clothes.Washer.Age)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"Post 2009"
              ,"All Vintages")
item296.table <- item296.table %>% mutate(Clothes.Washer.Age = factor(Clothes.Washer.Age, levels = rowOrder)) %>% arrange(Clothes.Washer.Age)  
item296.table <- data.frame(item296.table)

exportTable(item296.table, "MF", "Table 90", weighted = FALSE)







#############################################################################################
#Item 297: DISTRIBUTION OF IN-UNIT CLOTHES DRYER BY DRYER VINTAGE (MF Table 91)
#############################################################################################
#subset to columns needed for analysis
item297.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"Age"
                                                                    ,"Type"
                                                                    ,""))]

#join clean rbsa data onto appliances analysis data
item297.dat1 <- left_join(rbsa.dat, item297.dat, by = "CK_Cadmus_ID")

#subset to only MF
item297.dat2 <- item297.dat1[grep("Multifamily", item297.dat1$BuildingType),]

#subset to only Dryers
item297.dat3 <- item297.dat2[which(item297.dat2$Type %in% c("Dryer")),]

#subset to only common area Dryers
item297.dat4 <- item297.dat3[grep("SITE", item297.dat3$CK_SiteID),]

#subset to only common area Dryers that have observed age info
item297.dat5 <- item297.dat4[which(item297.dat4$Age > 0),]

####################
# Clean AGE
####################
item297.dat5$Age       <- as.numeric(as.character(item297.dat5$Age))
item297.dat5$Dryer.Age <- as.numeric(as.character(item297.dat5$Age))
item297.dat5$Dryer.Age[which(item297.dat5$Age < 1980)] <- "Pre 1980"
item297.dat5$Dryer.Age[which(item297.dat5$Age >= 1980 & item297.dat5$Age < 1990)] <- "1980-1989"
item297.dat5$Dryer.Age[which(item297.dat5$Age >= 1990 & item297.dat5$Age < 1995)] <- "1990-1994"
item297.dat5$Dryer.Age[which(item297.dat5$Age >= 1995 & item297.dat5$Age < 2000)] <- "1995-1999"
item297.dat5$Dryer.Age[which(item297.dat5$Age >= 2000 & item297.dat5$Age < 2005)] <- "2000-2004"
item297.dat5$Dryer.Age[which(item297.dat5$Age >= 2005 & item297.dat5$Age < 2009)] <- "2005-2009"
item297.dat5$Dryer.Age[which(item297.dat5$Age >= 2009)] <- "Post 2009"
unique(item297.dat5$Dryer.Age)
####################
# end cleaning
####################
item297.dat5 <- item297.dat5[which(!is.na(item297.dat5$Dryer.Age)),]
# item297.dat5$Dryer.Age[which(is.na(item297.dat5$Dryer.Age))] <- "Unknown"


######################################
#Pop and Sample Sizes for weights
######################################
item297.data <- weightedData(item297.dat5[which(colnames(item297.dat5) %notin% c("CK_SiteID"
                                                                                 ,"Type"
                                                                                 ,"Age"
                                                                                 ,"Dryer.Age"
                                                                                 ,""))])

item297.data <- left_join(item297.data, item297.dat5[which(colnames(item297.dat5) %in% c("CK_Cadmus_ID"
                                                                                         ,"Type"
                                                                                         ,"Dryer.Age"))])
item297.data <- unique(item297.data)
item297.data$count <- 1


######################
# weighted analysis
######################
item297.final <- proportions_one_group(CustomerLevelData = item297.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'Dryer.Age'
                                          ,total.name = 'Remove')
# item297.final <- item297.final[which(item297.final$Dryer.Age != "Total"),]

exportTable(item297.final, "MF", "Table 91", weighted = TRUE)

######################
# unweighted analysis
######################
item297.final <- proportions_one_group_MF(CustomerLevelData = item297.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'Dryer.Age'
                                          ,total.name = 'Remove'
                                          ,weighted = FALSE)
# item297.final <- item297.final[which(item297.final$Dryer.Age != "Total"),]

exportTable(item297.final, "MF", "Table 91", weighted = FALSE)