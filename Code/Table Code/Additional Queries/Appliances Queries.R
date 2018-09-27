#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          
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
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))


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

unique(one.line.dat$Primary.Heating.System)


mechanical.dat <- read.xlsx(mechanical.export)
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
mechanical.dat <- mechanical.dat[which(names(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"Primary.Heating.System"
                                                                    ,"Primary.Cooling.System"
                                                                    ,"Control.Type"
                                                                    ,"Component.1.Year.of.Manufacture"))]
mechanical.dat$Ind <- 0
mechanical.dat$Ind[grep("wifi|wi-fi|smart", mechanical.dat$Control.Type, ignore.case = T)] <- 1
length(unique(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- unique(mechanical.dat[which(mechanical.dat$Primary.Heating.System %in% c("Yes")),])
mechanical.dat1 <- unique(mechanical.dat1[which(mechanical.dat1$Component.1.Year.of.Manufacture %notin% c("N/A","Unknown",NA)),])
mechanical.dat1$CK_Cadmus_ID[which(duplicated(mechanical.dat1$CK_Cadmus_ID))]
mechanical.dat1$Component.1.Year.of.Manufacture <- as.numeric(as.character(mechanical.dat1$Component.1.Year.of.Manufacture))
mechanical.dat1$Equipment_Vintage_Bins <- mechanical.dat1$Component.1.Year.of.Manufacture
mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture < 1976)] <- "Pre 1976"
# mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture >= 1970 & mechanical.dat1$Component.1.Year.of.Manufacture <= 1975)] <- "1970-1975"
mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture > 1975 & mechanical.dat1$Component.1.Year.of.Manufacture <= 1980)] <- "1976-1980"
mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture > 1980 & mechanical.dat1$Component.1.Year.of.Manufacture <= 1985)] <- "1981-1985"
mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture > 1985 & mechanical.dat1$Component.1.Year.of.Manufacture <= 1990)] <- "1986-1990"
mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture > 1990 & mechanical.dat1$Component.1.Year.of.Manufacture <= 1995)] <- "1991-1995"
mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture > 1995 & mechanical.dat1$Component.1.Year.of.Manufacture <= 2000)] <- "1996-2000"
mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture > 2000 & mechanical.dat1$Component.1.Year.of.Manufacture <= 2005)] <- "2001-2005"
mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture > 2005 & mechanical.dat1$Component.1.Year.of.Manufacture <= 2010)] <- "2006-2010"
mechanical.dat1$Equipment_Vintage_Bins[which(mechanical.dat1$Component.1.Year.of.Manufacture > 2010)] <- "Post 2010"
unique(mechanical.dat1$Equipment_Vintage_Bins)

mechanical.sub <- mechanical.dat1[which(names(mechanical.dat1) %in% c("CK_Cadmus_ID","Generic","Ind","Equipment_Vintage_Bins"))]


#############################################################################################
#
# Smart/Wifi Thermostats
#
#############################################################################################
column.ind <- colnames(appliances.dat)[grep("wifi|thermostat", colnames(appliances.dat), ignore.case = T)]
unique(appliances.dat$Thermostat.Type)

AQ1.dat <- appliances.dat[which(names(appliances.dat) %in% c("CK_Cadmus_ID","Thermostat.Type"))]
AQ1.dat1 <- AQ1.dat[grep("smart|wifi", AQ1.dat$Thermostat.Type, ignore.case = T),]
AQ1.dat1$Ind <- 1
AQ1.dat2 <- unique(AQ1.dat1[which(names(AQ1.dat1) != "Thermostat.Type")])

AQ1.merge <- left_join(rbsa.dat, AQ1.dat2)
AQ1.merge$Ind[which(is.na(AQ1.merge$Ind))] <- 0
AQ1.merge <- left_join(AQ1.merge, survey.dat)
AQ1.merge <- left_join(AQ1.merge, mechanical.sub, by = "CK_Cadmus_ID")
################################################
# Adding pop and sample sizes for weights
################################################
AQ1.data <- weightedData(AQ1.merge[-which(colnames(AQ1.merge) %in% c("Ind.x","Ind.y","Education.Level","Income.Level","Generic","Equipment_Vintage_Bins"))])
AQ1.data <- left_join(AQ1.data, AQ1.merge[which(colnames(AQ1.merge) %in% c("CK_Cadmus_ID","Ind.x","Ind.y","Education.Level","Income.Level","Generic","Equipment_Vintage_Bins"))])
AQ1.data$count <- 1
AQ1.data$Count <- 1
#######################
# Weighted Analysis - housing vintages
#######################
AQ1.data1 <- AQ1.data[which(!is.na(AQ1.data$HomeYearBuilt)),]
AQ1.data1$Ind <- AQ1.data1$Ind.x
AQ1.data1$HomeYearBuilt_bins_percentage <- AQ1.data1$HomeYearBuilt_bins2
AQ1.percent <- proportions_one_group(CustomerLevelData = AQ1.data1
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "HomeYearBuilt_bins2"
                                     ,weighted = TRUE)
#######################
# unweighted Analysis - housing vintages
#######################
AQ1.percent.unw <- proportions_one_group(CustomerLevelData = AQ1.data1
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "HomeYearBuilt_bins2"
                                     ,weighted = FALSE)


#######################
# Weighted Analysis - income level
#######################
AQ1.data2 <- AQ1.data[which(AQ1.data$Income.Level %notin% c("N/A","Prefer not to say", "Above $50,000","Below $50,000","Unknown")),]
AQ1.data2$Ind <- AQ1.data2$Ind.x
AQ2.percent <- proportions_one_group(CustomerLevelData = AQ1.data2
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "Income.Level"
                                     ,weighted = TRUE)
#######################
# unweighted Analysis - income level
#######################
AQ2.percent.unw <- proportions_one_group(CustomerLevelData = AQ1.data2
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Income.Level"
                                         ,weighted = FALSE)

#######################
# Weighted Analysis - education level
#######################
AQ1.data3 <- AQ1.data[which(AQ1.data$Education.Level %notin% c("N/A","Prefer not to say", "Above $50,000","Below $50,000","Unknown")),]
AQ1.data3$Ind <- AQ1.data3$Ind.x
AQ3.percent <- proportions_one_group(CustomerLevelData = AQ1.data3
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "Education.Level"
                                     ,weighted = TRUE)
#######################
# unweighted Analysis - education level
#######################
AQ3.percent.unw <- proportions_one_group(CustomerLevelData = AQ1.data3
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Education.Level"
                                         ,weighted = FALSE)

#######################
# Weighted Analysis - percent of heating systems that are controlled by wifi-enabled/smart thermostats by equipment vintage
#######################
AQ1.data4 <- AQ1.data[which(AQ1.data$Equipment_Vintage_Bins %notin% c(NA,"N/A","Prefer not to say", "Above $50,000","Below $50,000","Unknown")),]
AQ1.data4 <- AQ1.data4[which(AQ1.data4$Generic != "Mini-split HP"),]
AQ1.data4$Ind <- AQ1.data4$Ind.y
AQ1.data4$HomeYearBuilt_bins_percentage <- AQ1.data4$Equipment_Vintage_Bins
AQ4.percent <- proportions_one_group(CustomerLevelData = AQ1.data4
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "HomeYearBuilt_bins_percentage"
                                     ,weighted = TRUE)
names(AQ4.percent)[which(names(AQ4.percent) == "HomeYearBuilt_bins_percentage")] <- "Heating_System_Age"


#######################
# unweighted Analysis - percent of heating systems that are controlled by wifi-enabled/smart thermostats by equipment vintage
#######################
AQ4.percent.unw <- proportions_one_group(CustomerLevelData = AQ1.data4
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "HomeYearBuilt_bins_percentage"
                                         ,weighted = FALSE)
names(AQ4.percent.unw)[which(names(AQ4.percent.unw) == "HomeYearBuilt_bins_percentage")] <- "Heating_System_Age"



#######################
# Weighted Analysis - percent of heating systems that are controlled by wifi-enabled/smart thermostats by equipment vintage
#######################
AQ5.percent <- proportions_one_group(CustomerLevelData = AQ1.data4
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "Generic"
                                     ,weighted = TRUE)
names(AQ5.percent)[which(names(AQ5.percent) == "Generic")] <- "Heating_System_Type"


#######################
# unweighted Analysis - percent of heating systems that are controlled by wifi-enabled/smart thermostats by equipment vintage
#######################
AQ5.percent.unw <- proportions_one_group(CustomerLevelData = AQ1.data4
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Generic"
                                         ,weighted = FALSE)
names(AQ5.percent.unw)[which(names(AQ5.percent.unw) == "Generic")] <- "Heating_System_Type"





#############################################################################################
#
# Smart Power Strips
#
#############################################################################################
column.ind <- colnames(appliances.dat)[grep("wifi|power", colnames(appliances.dat), ignore.case = T)]
unique(appliances.dat$Power.Strip.Use)
unique(appliances.dat$Smart.Power.Strip)

BQ1.dat <- appliances.dat[which(names(appliances.dat) %in% c("CK_Cadmus_ID","Power.Strip.Use","Smart.Power.Strip"))]
BQ1.dat1 <- BQ1.dat[grep("home|entertain|other", BQ1.dat$Power.Strip.Use, ignore.case = T),]

BQ1.dat1$Ind <- 0
BQ1.dat1$Ind[which(BQ1.dat1$Smart.Power.Strip == "Yes")] <- 1


BQ1.dat2 <- summarise(group_by(BQ1.dat1, CK_Cadmus_ID, Power.Strip.Use)
                      ,Ind = sum((Ind), na.rm = T))

BQ1.merge <- left_join(rbsa.dat, BQ1.dat2)
BQ1.merge <- left_join(BQ1.merge, survey.dat)
BQ1.merge <- BQ1.merge[which(!is.na(BQ1.merge$Ind)),]
################################################
# Adding pop and sample sizes for weights
################################################
BQ1.data <- weightedData(BQ1.merge[-which(colnames(BQ1.merge) %in% c("Ind","Education.Level","Income.Level","Power.Strip.Use"))])
BQ1.data <- left_join(BQ1.data, BQ1.merge[which(colnames(BQ1.merge) %in% c("CK_Cadmus_ID","Ind","Education.Level","Income.Level","Power.Strip.Use"))])
BQ1.data$count <- 1
BQ1.data$Count <- 1
#######################
# Weighted Analysis - housing vintages
#######################
BQ1.data1 <- BQ1.data[which(!is.na(BQ1.data$HomeYearBuilt)),]
BQ1.data1$HomeYearBuilt_bins_percentage <- BQ1.data1$HomeYearBuilt_bins2
BQ1.percent <- proportions_one_group(CustomerLevelData = BQ1.data1
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "HomeYearBuilt_bins2"
                                     ,weighted = TRUE)
#######################
# unweighted Analysis - housing vintages
#######################
BQ1.percent.unw <- proportions_one_group(CustomerLevelData = BQ1.data1
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "HomeYearBuilt_bins2"
                                         ,weighted = FALSE)


#######################
# Weighted Analysis - income level
#######################
BQ1.data2 <- BQ1.data[which(BQ1.data$Income.Level %notin% c("N/A","Prefer not to say", "Above $50,000","Below $50,000","Unknown")),]
BQ2.percent <- proportions_one_group(CustomerLevelData = BQ1.data2
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "Income.Level"
                                     ,weighted = TRUE)
#######################
# unweighted Analysis - income level
#######################
BQ2.percent.unw <- proportions_one_group(CustomerLevelData = BQ1.data2
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Income.Level"
                                         ,weighted = FALSE)

#######################
# Weighted Analysis - education level
#######################
BQ1.data3 <- BQ1.data[which(BQ1.data$Education.Level %notin% c("N/A","Prefer not to say","Unknown")),]
BQ3.percent <- proportions_one_group(CustomerLevelData = BQ1.data3
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "Education.Level"
                                     ,weighted = TRUE)
#######################
# unweighted Analysis - education level
#######################
BQ3.percent.unw <- proportions_one_group(CustomerLevelData = BQ1.data3
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Education.Level"
                                         ,weighted = FALSE)

#######################
# Weighted Analysis - power strip use type
#######################
BQ1.data4 <- BQ1.data[which(BQ1.data$Power.Strip.Use %notin% c("N/A","Prefer not to say","Unknown")),]
BQ4.percent <- proportions_one_group(CustomerLevelData = BQ1.data4
                                     ,valueVariable = "Ind"
                                     ,groupingVariable = "Power.Strip.Use"
                                     ,weighted = TRUE)
#######################
# unweighted Analysis - power strip use type
#######################
BQ4.percent.unw <- proportions_one_group(CustomerLevelData = BQ1.data4
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Power.Strip.Use"
                                         ,weighted = FALSE)
