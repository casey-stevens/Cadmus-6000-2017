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
rbsa.dat <- rbsa.dat[grep("single", rbsa.dat$BuildingType, ignore.case = T),]

length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Primary.Heating.System"
                                                                        ,"Heating.Fuel"
                                                                        ,"System.Sub-Type"
                                                                        ,"Component.1.Year.of.Manufacture"))]
mechanical.dat2 <- mechanical.dat1[which(mechanical.dat1$Primary.Heating.System == "Yes"),]
mechanical.dat2 <- mechanical.dat2[which(mechanical.dat2$Heating.Fuel %in% c("Natural Gas", "Gas")),]

#Read in data for analysis
survey.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated"),stringsAsFactors = F)
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))
survey.dat <- survey.dat[which(names(survey.dat) %in% c("CK_Cadmus_ID"
                                                        ,"What.is.the.highest.level.of.school.that.someone.in.your.home.has.completed."
                                                        ,"In.2015..was.your.annual.household.income.before.taxes.above.or.below..50.000."
                                                        ,"Was.your.annual.household.income.before.taxes.above.or.below..25.000."
                                                        ,"Was.it....0.to.under..25.000"
                                                        ,"Was.it....25.000.to.under..50.000"
                                                        ,"Was.it....50.000.or.more"))]
names(survey.dat) <- c("Education.Level","Income.Above.or.Below.50000","Income.Above.or.Below.25000","Income.0.to.25000","Income.25000.to.50000","Income.More.Than.50000","CK_Cadmus_ID")
survey.dat$Income.Level <- survey.dat$Income.More.Than.50000

ii = 2
for (ii in 1:nrow(survey.dat)){
  if(survey.dat$Income.More.Than.50000[ii] != "N/A"){
    survey.dat$Income.Level[ii] <- survey.dat$Income.More.Than.50000[ii]
  }
  if(survey.dat$Income.25000.to.50000[ii] != "N/A"){
    survey.dat$Income.Level[ii] <- survey.dat$Income.25000.to.50000[ii]
  }
  if(survey.dat$Income.0.to.25000[ii] != "N/A"){
    survey.dat$Income.Level[ii] <- survey.dat$Income.0.to.25000[ii]
  }
  if(survey.dat$Income.Level[ii] %in% c("N/A","Unknown", "Prefer not to say")){
    survey.dat$Income.Level[ii] <- survey.dat$Income.Above.or.Below.50000[ii]
  }
  if(survey.dat$Income.Level[ii] %in% c("N/A","Unknown", "Prefer not to say")){
    survey.dat$Income.Level[ii] <- survey.dat$Income.Above.or.Below.25000[ii]
  }
}
unique(survey.dat$Income.Level)
survey.dat$Income.Level[which(survey.dat$Income.Level == "Exactly $50,000")] <- "$50,000 to under $55,000"
survey.dat <- survey.dat[which(names(survey.dat) %in% c("CK_Cadmus_ID", "Education.Level", "Income.Level"))]


#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))
appliances.dat <- appliances.dat[which(names(appliances.dat) %in% c("CK_Cadmus_ID","Type","Age","Stove.Fuel","Oven.Fuel"))]
#############################################################################################
#
# Homes with gas primary heating fuel broken down by equipment type/vintage
#
#############################################################################################
mechanical.dat3 <- left_join(mechanical.dat2, survey.dat)
mechanical.dat3 <- mechanical.dat3[which(mechanical.dat3$Component.1.Year.of.Manufacture %notin% c("N/A","Unknown",NA,0)),]
mechanical.dat3$Component.1.Year.of.Manufacture <- as.numeric(as.character(mechanical.dat3$Component.1.Year.of.Manufacture))
mechanical.dat3$Equipment_Vintage_Bins <- mechanical.dat3$Component.1.Year.of.Manufacture
mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture < 1976)] <- "Pre 1976"
# mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture >= 1970 & mechanical.dat3$Component.1.Year.of.Manufacture <= 1975)] <- "1970-1975"
mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture > 1975 & mechanical.dat3$Component.1.Year.of.Manufacture <= 1980)] <- "1976-1980"
mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture > 1980 & mechanical.dat3$Component.1.Year.of.Manufacture <= 1985)] <- "1981-1985"
mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture > 1985 & mechanical.dat3$Component.1.Year.of.Manufacture <= 1990)] <- "1986-1990"
mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture > 1990 & mechanical.dat3$Component.1.Year.of.Manufacture <= 1995)] <- "1991-1995"
mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture > 1995 & mechanical.dat3$Component.1.Year.of.Manufacture <= 2000)] <- "1996-2000"
mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture > 2000 & mechanical.dat3$Component.1.Year.of.Manufacture <= 2005)] <- "2001-2005"
mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture > 2005 & mechanical.dat3$Component.1.Year.of.Manufacture <= 2010)] <- "2006-2010"
mechanical.dat3$Equipment_Vintage_Bins[which(mechanical.dat3$Component.1.Year.of.Manufacture > 2010)] <- "Post 2010"
unique(mechanical.dat3$Equipment_Vintage_Bins)

mechanical.merge <- left_join(rbsa.dat, mechanical.dat3)
mechanical.merge <- mechanical.merge[which(!is.na(mechanical.merge$Equipment_Vintage_Bins)),]
################################################
# Adding pop and sample sizes for weights
################################################
mechanical.data <- weightedData(mechanical.merge[-which(colnames(mechanical.merge) %in% c("Generic"
                                                                                          ,"System.Sub-Type"
                                                                                          ,"Primary.Heating.System"
                                                                                          ,"Heating.Fuel"
                                                                                          ,"Component.1.Year.of.Manufacture"
                                                                                          ,"Education.Level"
                                                                                          ,"Income.Level"
                                                                                          ,"Equipment_Vintage_Bins"))])
mechanical.data <- left_join(mechanical.data, mechanical.merge[which(colnames(mechanical.merge) %in% c("CK_Cadmus_ID"
                                                                                                       ,"Generic"
                                                                                                       ,"System.Sub-Type"
                                                                                                       ,"Primary.Heating.System"
                                                                                                       ,"Heating.Fuel"
                                                                                                       ,"Component.1.Year.of.Manufacture"
                                                                                                       ,"Education.Level"
                                                                                                       ,"Income.Level"
                                                                                                       ,"Equipment_Vintage_Bins"))])
mechanical.data$count <- 1
mechanical.data$Count <- 1


#######################
# Weighted Analysis - 
#######################
MQ1.summary <- proportionRowsAndColumns1(CustomerLevelData = mechanical.data
                                         ,valueVariable = "count"
                                         ,columnVariable = "Generic"
                                         ,rowVariable = "Equipment_Vintage_Bins"
                                         ,aggregateColumnName = "All Systems")
MQ1.summary <- MQ1.summary[which(MQ1.summary$Generic != "All Systems"),]

MQ1.all.systems <- proportions_one_group(CustomerLevelData = mechanical.data
                                         ,valueVariable = "count"
                                         ,groupingVariable = "Equipment_Vintage_Bins"
                                         ,total.name = "All Systems"
                                         ,columnName = "Generic"
                                         ,weighted = TRUE
                                         ,two.prop.total = TRUE)
MQ1.final <- rbind.data.frame(MQ1.summary, MQ1.all.systems, stringsAsFactors = F)
MQ1.cast  <- dcast(setDT(MQ1.final)
                   ,formula = BuildingType + Equipment_Vintage_Bins ~ Generic
                   ,value.var = c("w.percent","w.SE","n","EB"))

MQ1.table <- data.frame("BuildingType" = MQ1.cast$BuildingType
                        ,"Equipment.Vintage" = MQ1.cast$Equipment_Vintage_Bins
                        ,"Percent.Boiler"  = MQ1.cast$w.percent_Boiler
                        ,"SE.Boiler" = MQ1.cast$w.SE_Boiler
                        ,"n.Boiler" = MQ1.cast$n_Boiler
                        ,"Percent.Furnace" = MQ1.cast$w.percent_Furnace
                        ,"SE.Furnace" = MQ1.cast$w.SE_Furnace
                        ,"n.Furnace" = MQ1.cast$n_Furnace
                        ,"Percent.All.Systems" = MQ1.cast$`w.percent_All Systems`
                        ,"SE.All.Systems" = MQ1.cast$`w.SE_All Systems`
                        ,"n.All.Systems" = MQ1.cast$`n_All Systems`)
#######################
# unweighted Analysis - 
#######################
MQ1.summary <- proportions_two_groups_unweighted(CustomerLevelData = mechanical.data
                                         ,valueVariable = "count"
                                         ,columnVariable = "Generic"
                                         ,rowVariable = "Equipment_Vintage_Bins"
                                         ,aggregateColumnName = "All Systems")
MQ1.summary <- MQ1.summary[which(MQ1.summary$Generic != "All Systems"),]

MQ1.all.systems <- proportions_one_group(CustomerLevelData = mechanical.data
                                         ,valueVariable = "count"
                                         ,groupingVariable = "Equipment_Vintage_Bins"
                                         ,total.name = "All Systems"
                                         ,columnName = "Generic"
                                         ,weighted = FALSE
                                         ,two.prop.total = TRUE)
MQ1.final <- rbind.data.frame(MQ1.summary, MQ1.all.systems, stringsAsFactors = F)
MQ1.cast  <- dcast(setDT(MQ1.final)
                   ,formula = BuildingType + Equipment_Vintage_Bins ~ Generic
                   ,value.var = c("Percent","SE","n"))

MQ1.table.unw <- data.frame("BuildingType" = MQ1.cast$BuildingType
                        ,"Equipment.Vintage" = MQ1.cast$Equipment_Vintage_Bins
                        ,"Percent.Boiler"  = MQ1.cast$Percent_Boiler
                        ,"SE.Boiler" = MQ1.cast$SE_Boiler
                        ,"n.Boiler" = MQ1.cast$n_Boiler
                        ,"Percent.Furnace" = MQ1.cast$Percent_Furnace
                        ,"SE.Furnace" = MQ1.cast$SE_Furnace
                        ,"n.Furnace" = MQ1.cast$n_Furnace
                        ,"Percent.All.Systems" = MQ1.cast$`Percent_All Systems`
                        ,"SE.All.Systems" = MQ1.cast$`SE_All Systems`
                        ,"n.All.Systems" = MQ1.cast$`n_All Systems`)








#######################
# Next Table - 
#######################
MQ2.dat <- left_join(mechanical.dat2, appliances.dat)
MQ2.dat$Age <- as.numeric(as.character(MQ2.dat$Age))
MQ2.dat <- left_join(rbsa.dat,MQ2.dat)
MQ2.dat <- MQ2.dat[which(!is.na(MQ2.dat$Age)),]
MQ2.dat <- MQ2.dat[which(MQ2.dat$Type == "Stove/Oven"),]
MQ2.sub <- MQ2.dat[which(names(MQ2.dat) %in% c("CK_Cadmus_ID","Age", "Oven.Fuel", "Stove.Fuel"))]
MQ2.melt <- melt(MQ2.sub, id = c("CK_Cadmus_ID","Age"))
names(MQ2.melt) <- c("CK_Cadmus_ID", "Age", "Type", "Fuel.Type")
MQ2.melt$Type <- as.character(MQ2.melt$Type)
MQ2.melt$Type[grep("stove", MQ2.melt$Type, ignore.case = T)] <- "Stove"
MQ2.melt$Type[grep("oven", MQ2.melt$Type, ignore.case = T)] <- "Oven"

MQ.merge <- left_join(rbsa.dat, MQ2.melt)
MQ.merge <- MQ.merge[which(!is.na(MQ.merge$Fuel.Type)),]

MQ.merge$Age <- as.numeric(as.character(MQ.merge$Age))
MQ.merge$Equipment_Vintage_Bins <- MQ.merge$Age
MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age < 1976)] <- "Pre 1976"
# MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age >= 1970 & MQ.merge$Age <= 1975)] <- "1970-1975"
MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age > 1975 & MQ.merge$Age <= 1980)] <- "1976-1980"
MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age > 1980 & MQ.merge$Age <= 1985)] <- "1981-1985"
MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age > 1985 & MQ.merge$Age <= 1990)] <- "1986-1990"
MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age > 1990 & MQ.merge$Age <= 1995)] <- "1991-1995"
MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age > 1995 & MQ.merge$Age <= 2000)] <- "1996-2000"
MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age > 2000 & MQ.merge$Age <= 2005)] <- "2001-2005"
MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age > 2005 & MQ.merge$Age <= 2010)] <- "2006-2010"
MQ.merge$Equipment_Vintage_Bins[which(MQ.merge$Age > 2010)] <- "Post 2010"
unique(MQ.merge$Equipment_Vintage_Bins)

################################################
# Adding pop and sample sizes for weights
################################################
mechanical.data1 <- weightedData(MQ.merge[-which(colnames(MQ.merge) %in% c("Generic"
                                                                         ,"System.Sub-Type"
                                                                         ,"Primary.Heating.System"
                                                                         ,"Heating.Fuel"
                                                                         ,"Component.1.Year.of.Manufacture"
                                                                         ,"Education.Level"
                                                                         ,"Income.Level"
                                                                         ,"Type"
                                                                         ,"Age"
                                                                         ,"Fuel.Type"
                                                                         ,"count"
                                                                         ,"Count"
                                                                         ,"Equipment_Vintage_Bins"))])
mechanical.data1 <- left_join(mechanical.data1, MQ.merge[which(colnames(MQ.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Generic"
                                                                                       ,"System.Sub-Type"
                                                                                       ,"Primary.Heating.System"
                                                                                       ,"Heating.Fuel"
                                                                                       ,"Component.1.Year.of.Manufacture"
                                                                                       ,"Education.Level"
                                                                                       ,"Income.Level"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"Fuel.Type"
                                                                                       ,"count"
                                                                                       ,"Count"
                                                                                       ,"Equipment_Vintage_Bins"))])
mechanical.data1$count <- 1
mechanical.data1$Count <- 1

#######################
# Weighted Analysis - 
#######################
MQ2.summary <- proportionRowsAndColumns1(CustomerLevelData = mechanical.data1
                                         ,valueVariable = "count"
                                         ,columnVariable = "Type"
                                         ,rowVariable = "Equipment_Vintage_Bins"
                                         ,aggregateColumnName = "All Types")
MQ2.summary <- MQ2.summary[which(MQ2.summary$Type != "All Types"),]

MQ2.all.systems <- proportions_one_group(CustomerLevelData = mechanical.data1
                                         ,valueVariable = "count"
                                         ,groupingVariable = "Equipment_Vintage_Bins"
                                         ,total.name = "All Types"
                                         ,columnName = "Type"
                                         ,weighted = TRUE
                                         ,two.prop.total = TRUE)
MQ2.final <- rbind.data.frame(MQ2.summary, MQ2.all.systems, stringsAsFactors = F)
MQ2.cast  <- dcast(setDT(MQ2.final)
                   ,formula = BuildingType + Equipment_Vintage_Bins ~ Type
                   ,value.var = c("w.percent","w.SE","n","EB"))

MQ2.table <- data.frame("BuildingType" = MQ2.cast$BuildingType
                        ,"Equipment.Vintage" = MQ2.cast$Equipment_Vintage_Bins
                        ,"Percent.Oven"  = MQ2.cast$w.percent_Oven
                        ,"SE.Oven" = MQ2.cast$w.SE_Oven
                        ,"n.Oven" = MQ2.cast$n_Oven
                        ,"Percent.Stove" = MQ2.cast$w.percent_Stove
                        ,"SE.Stove" = MQ2.cast$w.SE_Stove
                        ,"n.Stove" = MQ2.cast$n_Stove
                        ,"Percent.All.Types" = MQ2.cast$`w.percent_All Types`
                        ,"SE.All.Types" = MQ2.cast$`w.SE_All Types`
                        ,"n.All.Types" = MQ2.cast$`n_All Types`)
#######################
# unweighted Analysis - 
#######################
MQ2.summary <- proportions_two_groups_unweighted(CustomerLevelData = mechanical.data1
                                                 ,valueVariable = "count"
                                                 ,columnVariable = "Type"
                                                 ,rowVariable = "Equipment_Vintage_Bins"
                                                 ,aggregateColumnName = "All Types")
MQ2.summary <- MQ2.summary[which(MQ2.summary$Type != "All Types"),]

MQ2.all.systems <- proportions_one_group(CustomerLevelData = mechanical.data1
                                         ,valueVariable = "count"
                                         ,groupingVariable = "Equipment_Vintage_Bins"
                                         ,total.name = "All Types"
                                         ,columnName = "Type"
                                         ,weighted = FALSE
                                         ,two.prop.total = TRUE)
MQ2.final <- rbind.data.frame(MQ2.summary, MQ2.all.systems, stringsAsFactors = F)
MQ2.cast  <- dcast(setDT(MQ2.final)
                   ,formula = BuildingType + Equipment_Vintage_Bins ~ Type
                   ,value.var = c("Percent","SE","n"))

MQ2.table.unw <- data.frame("BuildingType" = MQ2.cast$BuildingType
                            ,"Equipment.Vintage" = MQ2.cast$Equipment_Vintage_Bins
                            ,"Percent.Oven"  = MQ2.cast$Percent_Oven
                            ,"SE.Oven" = MQ2.cast$SE_Oven
                            ,"n.Oven" = MQ2.cast$n_Oven
                            ,"Percent.Stove" = MQ2.cast$Percent_Stove
                            ,"SE.Stove" = MQ2.cast$SE_Stove
                            ,"n.Stove" = MQ2.cast$n_Stove
                            ,"Percent.All.Types" = MQ2.cast$`Percent_All Types`
                            ,"SE.All.Types" = MQ2.cast$`SE_All Types`
                            ,"n.All.Types" = MQ2.cast$`n_All Types`)
