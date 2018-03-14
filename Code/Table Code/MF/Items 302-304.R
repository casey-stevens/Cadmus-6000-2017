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
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]
#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- as.character(trimws(toupper(appliances.dat$CK_Cadmus_ID)))
#subset to columns provided by Rietz -- this includes columns for all three items (302-304)
appliances.dat1 <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"CK_SiteID"
                                                                        ,"Type"
                                                                        ,"TV.Wattage"
                                                                        ,"Age"
                                                                        ,"TV.Screen.Type"
                                                                        ,"Room"
                                                                        ,"Clean.Room"
                                                                        ,""))]

#merge appliances data onto RBSA cleaned data
rbsa.appliances <- left_join(rbsa.dat, appliances.dat1, by = "CK_Cadmus_ID")

rbsa.appliances1 <- rbsa.appliances[grep("SITE", rbsa.appliances$CK_SiteID),]

#subset to only MF
rbsa.app.MF <- rbsa.appliances1[grep("Multifamily",rbsa.appliances1$BuildingType),]

###################################################################################################################
# ITEM 302: AVERAGE IN-UNIT TELEVISION POWER BY VINTAGE (MF Table 96)
###################################################################################################################
#subset to only televisions
item302.dat <- rbsa.app.MF[which(rbsa.app.MF$Type == "Television"),]

#remove missing TV.Wattage values
item302.dat1 <- item302.dat[which(!(item302.dat$TV.Wattage %in% c("unknown",NA))),]

#subset to only known TV vintages
item302.dat2 <- item302.dat1[which(item302.dat1$Age != "-- Datapoint not asked for --"),]
item302.dat2$Age <- as.numeric(as.character(item302.dat2$Age))
item302.dat2$TV.Wattage <- as.numeric(as.character(item302.dat2$TV.Wattage))

####Create vintage bins according to previous RBSA table
item302.dat2$Age.Cat <- "Unknown Vintage"
item302.dat2$Age.Cat[which(item302.dat2$Age < 1990)] <- "Pre 1990"
item302.dat2$Age.Cat[which(item302.dat2$Age >= 1990 & item302.dat2$Age < 2000)] <- "1990-1999"
item302.dat2$Age.Cat[which(item302.dat2$Age >= 2000 & item302.dat2$Age < 2005)] <- "2000-2004"
item302.dat2$Age.Cat[which(item302.dat2$Age >= 2005 & item302.dat2$Age < 2010)] <- "2005-2009"
item302.dat2$Age.Cat[which(item302.dat2$Age >= 2010 & item302.dat2$Age < 2015)] <- "2010-2014"
item302.dat2$Age.Cat[which(item302.dat2$Age >= 2015)] <- "Post 2014"
#check age category mapping
unique(item302.dat2$Age.Cat)

item302.dat4 <- item302.dat2[which(!is.na(item302.dat2$TV.Wattage)),]

######################################
#Pop and Sample Sizes for weights
######################################
item302.data <- weightedData(item302.dat4[which(colnames(item302.dat4) %notin% c("CK_SiteID"
                                                                                 ,"Type"
                                                                                 ,"Age"                
                                                                                 ,"TV.Screen.Type"
                                                                                 ,"TV.Wattage"
                                                                                 ,"Room"
                                                                                 ,"Clean.Room"
                                                                                 ,"Age.Cat"))])

item302.data <- left_join(item302.data, item302.dat4[which(colnames(item302.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Type"
                                                                                         ,"Age"                
                                                                                         ,"TV.Screen.Type"
                                                                                         ,"TV.Wattage"
                                                                                         ,"Room"
                                                                                         ,"Clean.Room"
                                                                                         ,"Age.Cat"))])
item302.data$count <- 1


######################
# weighted analysis
######################
item302.final <- mean_one_group(CustomerLevelData = item302.data
                                ,valueVariable = 'TV.Wattage'
                                ,byVariable = 'Age.Cat'
                                ,aggregateRow = "All Vintages")
item302.final <- item302.final[which(colnames(item302.final) %notin% c("BuildingType"))]

unique(item302.final$Age.Cat)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Unknown Vintage"
              ,"All Vintages")
item302.final <- item302.final %>% mutate(Age.Cat = factor(Age.Cat, levels = rowOrder)) %>% arrange(Age.Cat)  
item302.final <- data.frame(item302.final)


exportTable(item302.final, "MF", "Table 96", weighted = TRUE)
######################
# unweighted analysis
######################
item302.final <- mean_one_group_unweighted(CustomerLevelData = item302.data
                                ,valueVariable = 'TV.Wattage'
                                ,byVariable = 'Age.Cat'
                                ,aggregateRow = "All Vintages")
item302.final <- item302.final[which(colnames(item302.final) %notin% c("BuildingType"))]

unique(item302.final$Age.Cat)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Unknown Vintage"
              ,"All Vintages")
item302.final <- item302.final %>% mutate(Age.Cat = factor(Age.Cat, levels = rowOrder)) %>% arrange(Age.Cat)  
item302.final <- data.frame(item302.final)

exportTable(item302.final, "MF", "Table 96", weighted = FALSE)






###################################################################################################################
# ITEM 303: DISTRIBUTION OF IN-UNIT TELEVISION SCREENS BY TYPE AND VINTAGE (MF Table 97)
###################################################################################################################
#subset to only televisions
item303.dat1 <- rbsa.app.MF[which(rbsa.app.MF$Type == "Television"),]

#subset to only known TV vintages
item303.dat2 <- item303.dat1[which(item303.dat1$Age != "-- Datapoint not asked for --"),]
item303.dat2$Age <- as.numeric(as.character(item303.dat2$Age))
item303.dat2$TV.Wattage <- as.numeric(as.character(item303.dat2$TV.Wattage))

####Create vintage bins according to previous RBSA table
item303.dat2$Age.Cat <- item303.dat2$Age
item303.dat2$Age.Cat[which(item303.dat2$Age < 1990)] <- "Pre 1990"
item303.dat2$Age.Cat[which(item303.dat2$Age >= 1990 & item303.dat2$Age < 2000)] <- "1990-1999"
item303.dat2$Age.Cat[which(item303.dat2$Age >= 2000 & item303.dat2$Age < 2005)] <- "2000-2004"
item303.dat2$Age.Cat[which(item303.dat2$Age >= 2005 & item303.dat2$Age < 2010)] <- "2005-2009"
item303.dat2$Age.Cat[which(item303.dat2$Age >= 2010 & item303.dat2$Age < 2015)] <- "2010-2014"
item303.dat2$Age.Cat[which(item303.dat2$Age >= 2015)] <- "Post 2014"
#check age category mapping
unique(item303.dat2$Age.Cat)

###### create TV screen type categories accoring to previous RBSA table (CRT and other)
unique(item303.dat2$TV.Screen.Type)
item303.dat2$TV.Screen.Cat <- item303.dat2$TV.Screen.Type
item303.dat2$TV.Screen.Cat[grep("tube", item303.dat2$TV.Screen.Type, ignore.case = T)] <- "CRT"
item303.dat2$TV.Screen.Cat[grep("non-crt|unknown",item303.dat2$TV.Screen.Type, ignore.case = T)] <- "Other"
unique(item303.dat2$TV.Screen.Cat)
names(item303.dat2)

item303.dat3 <- item303.dat2[which(!is.na(item303.dat2$Age.Cat)),]

######################################
#Pop and Sample Sizes for weights
######################################
item303.data <- weightedData(item303.dat3[which(colnames(item303.dat3) %notin% c("CK_SiteID"
                                                                                 ,"Type"
                                                                                 ,"Age"                
                                                                                 ,"TV.Screen.Type"
                                                                                 ,"TV.Wattage"
                                                                                 ,"Room"
                                                                                 ,"Clean.Room"
                                                                                 ,"Age.Cat"
                                                                                 ,"TV.Screen.Cat"))])

item303.data <- left_join(item303.data, item303.dat3[which(colnames(item303.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Type"
                                                                                         ,"Age"                
                                                                                         ,"TV.Screen.Type"
                                                                                         ,"TV.Wattage"
                                                                                         ,"Room"
                                                                                         ,"Clean.Room"
                                                                                         ,"Age.Cat"
                                                                                         ,"TV.Screen.Cat"))])
item303.data$count <- 1


######################
# weighted analysis
######################
item303.summary <- proportionRowsAndColumns1(CustomerLevelData = item303.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Age.Cat'
                                             ,rowVariable = 'TV.Screen.Cat'
                                             ,aggregateColumnName = "All Vintages")
# item303.summary <- item303.summary[which(item303.summary$TV.Screen.Cat != "Total"),]
item303.summary <- item303.summary[which(item303.summary$Age.Cat != "All Vintages"),]

item303.all.vintages <- proportions_one_group(CustomerLevelData = item303.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = 'TV.Screen.Cat'
                                                 ,total.name = "All Vintages"
                                                 ,columnName = "Age.Cat"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)

item303.final <- rbind.data.frame(item303.summary, item303.all.vintages, stringsAsFactors = F)

item303.cast <- dcast(setDT(item303.final)
                      ,formula = Age.Cat ~ TV.Screen.Cat
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
names(item303.cast)
#put into correct table format
item303.table <- data.frame("Equipment_Vintage" = item303.cast$Age.Cat
                            ,"CRT_Percent"      = item303.cast$w.percent_CRT
                            ,"CRT_SE"           = item303.cast$w.SE_CRT
                            ,"LED_Percent"      = item303.cast$w.percent_LED
                            ,"LED_SE"           = item303.cast$w.SE_LED
                            ,"LCD_Percent"      = item303.cast$w.percent_LCD
                            ,"LCD_SE"           = item303.cast$w.SE_LCD
                            ,"LED_LCD_Percent"  = item303.cast$`w.percent_LED LCD`
                            ,"LED_LCD_SE"       = item303.cast$`w.SE_LED LCD`
                            ,"Plasma_Percent"   = item303.cast$w.percent_Plasma
                            ,"Plasma_SE"        = item303.cast$w.SE_Plasma
                            ,"Other_Percent"    = item303.cast$w.percent_Other
                            ,"Other_SE"         = item303.cast$w.SE_Other
                            ,"n"                = item303.cast$n_Total
                            ,"CRT_EB"           = item303.cast$EB_CRT
                            ,"LED_EB"           = item303.cast$EB_LED
                            ,"LCD_EB"           = item303.cast$EB_LCD
                            ,"LED_LCD_EB"       = item303.cast$`EB_LED LCD`
                            ,"Plasma_EB"        = item303.cast$EB_Plasma
                            ,"Other_EB"         = item303.cast$EB_Other
                            )
levels(item303.table$Equipment_Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"All Vintages")
item303.table <- item303.table %>% mutate(Equipment_Vintage = factor(Equipment_Vintage, levels = rowOrder)) %>% arrange(Equipment_Vintage)  
item303.table <- data.frame(item303.table)

exportTable(item303.table, "MF", "Table 97", weighted = TRUE)



######################
# unweighted analysis
######################
item303.summary <- proportions_two_groups_unweighted(CustomerLevelData = item303.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Age.Cat'
                                             ,rowVariable = 'TV.Screen.Cat'
                                             ,aggregateColumnName = "All Vintages")
# item303.summary <- item303.summary[which(item303.summary$TV.Screen.Cat != "Total"),]
item303.summary <- item303.summary[which(item303.summary$Age.Cat != "All Vintages"),]

item303.all.vintages <- proportions_one_group(CustomerLevelData = item303.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = 'TV.Screen.Cat'
                                                 ,total.name = "All Vintages"
                                                 ,columnName = "Age.Cat"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE)

item303.final <- rbind.data.frame(item303.summary, item303.all.vintages, stringsAsFactors = F)

item303.cast <- dcast(setDT(item303.final)
                      ,formula = Age.Cat ~ TV.Screen.Cat
                      ,value.var = c("Percent", "SE", "Count", "n"))

#put into correct table format
item303.table <- data.frame("Equipment_Vintage" = item303.cast$Age.Cat
                            ,"CRT_Percent"      = item303.cast$Percent_CRT
                            ,"CRT_SE"           = item303.cast$SE_CRT
                            ,"LED_Percent"      = item303.cast$Percent_LED
                            ,"LED_SE"           = item303.cast$SE_LED
                            ,"LCD_Percent"      = item303.cast$Percent_LCD
                            ,"LCD_SE"           = item303.cast$SE_LCD
                            ,"LED_LCD_Percent"  = item303.cast$`Percent_LED LCD`
                            ,"LED_LCD_SE"       = item303.cast$`SE_LED LCD`
                            ,"Plasma_Percent"   = item303.cast$Percent_Plasma
                            ,"Plasma_SE"        = item303.cast$SE_Plasma
                            ,"Other_Percent"    = item303.cast$Percent_Other
                            ,"Other_SE"         = item303.cast$SE_Other
                            ,"n"                = item303.cast$n_Total
                            )
levels(item303.table$Equipment_Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"All Vintages")
item303.table <- item303.table %>% mutate(Equipment_Vintage = factor(Equipment_Vintage, levels = rowOrder)) %>% arrange(Equipment_Vintage)  
item303.table <- data.frame(item303.table)

exportTable(item303.table, "MF", "Table 97", weighted = FALSE)






###################################################################################################################
# ITEM 304: DISTRIBUTION OF IN-UNIT TELEVISIONS BY ROOM TYPE (MF Table 98)
###################################################################################################################
#subset to only televisions
item304.dat <- rbsa.app.MF[which(rbsa.app.MF$Type == "Television"),]

#subset to remove any missing room types
item304.dat1 <- item304.dat[which(!(is.na(item304.dat$Clean.Room))),]

######################################
#Pop and Sample Sizes for weights
######################################
item304.data <- weightedData(item304.dat1[which(colnames(item304.dat1) %notin% c("CK_SiteID"
                                                                                 ,"Type"
                                                                                 ,"Age"                
                                                                                 ,"TV.Screen.Type"
                                                                                 ,"TV.Wattage"
                                                                                 ,"Room"
                                                                                 ,"Clean.Room"))])

item304.data <- left_join(item304.data, item304.dat1[which(colnames(item304.dat1) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Type"
                                                                                         ,"Age"                
                                                                                         ,"TV.Screen.Type"
                                                                                         ,"TV.Wattage"
                                                                                         ,"Room"
                                                                                         ,"Clean.Room"))])
item304.data$count <- 1


######################
# weighted analysis
######################
item304.final <- proportions_one_group(CustomerLevelData = item304.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'Clean.Room'
                                          ,total.name = "All Room Types"
                                          ,weighted = TRUE)
exportTable(item304.final, "MF", "Table 98", weighted = TRUE)

######################
# unweighted analysis
######################
item304.final <- proportions_one_group(CustomerLevelData = item304.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'Clean.Room'
                                          ,total.name = "All Room Types"
                                          ,weighted = FALSE)
exportTable(item304.final, "MF", "Table 98", weighted = FALSE)
