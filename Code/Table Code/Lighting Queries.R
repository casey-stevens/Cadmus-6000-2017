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
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))

#Read in data for analysis
sites.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, sites.export)),stringsAsFactors = FALSE)
sites.dat <- data.frame(sites.dat, stringsAsFactors = F)
#clean cadmus IDs
sites.dat$CK_Cadmus_ID <- trimws(toupper(sites.dat$CK_Cadmus_ID))


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

#############################################################################################
#
# connected lighting
#
#############################################################################################
LQ1.dat <- sites.dat[which(colnames(sites.dat) %in% c("CK_Cadmus_ID","SITE_GENL_INFO_SmartHome_DevicesList"))]
LQ1.dat1 <- LQ1.dat[grep("Lighting Controls",LQ1.dat$SITE_GENL_INFO_SmartHome_DevicesList,ignore.case = T),]
unique(LQ1.dat$SITE_GENL_INFO_SmartHome_DevicesList)

LQ1.dat2 <- left_join(rbsa.dat, LQ1.dat1)

LQ1.dat2$Ind <- 0
LQ1.dat2$Ind[grep("lighting controls",LQ1.dat2$SITE_GENL_INFO_SmartHome_DevicesList,ignore.case = T)] <- 1
unique(LQ1.dat2$Ind)

LQ1.merge <- LQ1.dat2
LQ1.mergeX <- left_join(LQ1.dat2, survey.dat)
LQ1.merge1 <- LQ1.merge[which(!is.na(LQ1.merge$HomeYearBuilt)),]
LQ1.merge2 <- LQ1.mergeX[which(LQ1.mergeX$Income.Level %notin% c("N/A","Unknown","Prefer not to say", NA, "Above $50,000","Below $50,000")),]
LQ1.merge3 <- LQ1.mergeX[which(LQ1.mergeX$Education.Level %notin% c("N/A","Unknown","Prefer not to say", NA)),]
################################################
# Adding pop and sample sizes for weights
################################################
LQ1.data1 <- weightedData(LQ1.merge1[-which(colnames(LQ1.merge1) %in% c("Ind"
                                                                       ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                       ,"Education.Level"
                                                                       ,"Income.Level"))])
LQ1.data1 <- left_join(LQ1.data1, LQ1.merge1[which(colnames(LQ1.merge1) %in% c("CK_Cadmus_ID"
                                                                             ,"Ind"
                                                                             ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                             ,"Education.Level"
                                                                             ,"Income.Level"))])
LQ1.data1$count <- 1
LQ1.data1$Count <- 1



LQ1.data2 <- weightedData(LQ1.merge2[-which(colnames(LQ1.merge2) %in% c("Ind"
                                                                         ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                         ,"Education.Level"
                                                                         ,"Income.Level"))])
LQ1.data2 <- left_join(LQ1.data2, LQ1.merge2[which(colnames(LQ1.merge2) %in% c("CK_Cadmus_ID"
                                                                               ,"Ind"
                                                                               ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                               ,"Education.Level"
                                                                               ,"Income.Level"))])
LQ1.data2$count <- 1
LQ1.data2$Count <- 1



LQ1.data3 <- weightedData(LQ1.merge3[-which(colnames(LQ1.merge3) %in% c("Ind"
                                                                         ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                         ,"Education.Level"
                                                                         ,"Income.Level"))])
LQ1.data3 <- left_join(LQ1.data3, LQ1.merge3[which(colnames(LQ1.merge3) %in% c("CK_Cadmus_ID"
                                                                               ,"Ind"
                                                                               ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                               ,"Education.Level"
                                                                               ,"Income.Level"))])
LQ1.data3$count <- 1
LQ1.data3$Count <- 1

#######################
# Weighted Analysis - housing vintages
#######################
# LQ1.data1$HomeYearBuilt_bins_percentage <- LQ1.data1$HomeYearBuilt_bins2
# LQ1.percent <- proportions_one_group(CustomerLevelData = LQ1.data1
#                                      ,valueVariable = "Ind"
#                                      ,groupingVariable = "HomeYearBuilt_bins2"
#                                      ,weighted = TRUE)
#######################
# unweighted Analysis - housing vintages
#######################
LQ1.percent.unw <- proportions_one_group(CustomerLevelData = LQ1.data1
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "HomeYearBuilt_bins2"
                                         ,weighted = FALSE)


#######################
# Weighted Analysis - income level
#######################
# LQ2.percent <- proportions_one_group(CustomerLevelData = LQ1.data2
#                                      ,valueVariable = "Ind"
#                                      ,groupingVariable = "Income.Level"
#                                      ,weighted = TRUE)
#######################
# unweighted Analysis - income level
#######################
LQ2.percent.unw <- proportions_one_group(CustomerLevelData = LQ1.data2
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Income.Level"
                                         ,weighted = FALSE)

#######################
# Weighted Analysis - education level
#######################
# LQ3.percent <- proportions_one_group(CustomerLevelData = LQ1.data3
#                                      ,valueVariable = "Ind"
#                                      ,groupingVariable = "Education.Level"
#                                      ,weighted = TRUE)
#######################
# unweighted Analysis - education level
#######################
LQ3.percent.unw <- proportions_one_group(CustomerLevelData = LQ1.data3
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Education.Level"
                                         ,weighted = FALSE)



#############################################################################################
#
# Homes without CFLs or LEDs
#
#############################################################################################
LQ4.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID","LIGHTING_BulbType"))]


LQ4.dat$Ind <- 0
LQ4.dat$Ind[grep("compact fluorescent|light emitting diode",LQ4.dat$LIGHTING_BulbType,ignore.case = T)] <- 1
unique(LQ4.dat$Ind)

LQ4.sum <- summarise(group_by(LQ4.dat, CK_Cadmus_ID)
                     ,Ind = sum(unique(Ind)))
unique(LQ4.sum$Ind)
LQ4.sum$Ind <- abs(1 - LQ4.sum$Ind)
LQ4.merge <- left_join(rbsa.dat, LQ4.sum)
LQ4.mergeX <- left_join(LQ4.merge, survey.dat)
LQ4.merge4 <- LQ4.merge[which(!is.na(LQ4.merge$HomeYearBuilt)),]
LQ4.merge5 <- LQ4.mergeX[which(LQ4.mergeX$Income.Level %notin% c("N/A","Unknown","Prefer not to say", NA, "Above $50,000","Below $50,000")),]
LQ4.merge6 <- LQ4.mergeX[which(LQ4.mergeX$Education.Level %notin% c("N/A","Unknown","Prefer not to say", NA)),]
################################################
# Adding pop and sample sizes for weights
################################################
LQ4.data4 <- weightedData(LQ4.merge4[-which(colnames(LQ4.merge4) %in% c("Ind"
                                                                        ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                        ,"Education.Level"
                                                                        ,"Income.Level"))])
LQ4.data4 <- left_join(LQ4.data4, LQ4.merge4[which(colnames(LQ4.merge4) %in% c("CK_Cadmus_ID"
                                                                               ,"Ind"
                                                                               ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                               ,"Education.Level"
                                                                               ,"Income.Level"))])
LQ4.data4$count <- 1
LQ4.data4$Count <- 1



LQ4.data5 <- weightedData(LQ4.merge5[-which(colnames(LQ4.merge5) %in% c("Ind"
                                                                        ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                        ,"Education.Level"
                                                                        ,"Income.Level"))])
LQ4.data5 <- left_join(LQ4.data5, LQ4.merge5[which(colnames(LQ4.merge5) %in% c("CK_Cadmus_ID"
                                                                               ,"Ind"
                                                                               ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                               ,"Education.Level"
                                                                               ,"Income.Level"))])
LQ4.data5$count <- 1
LQ4.data5$Count <- 1



LQ4.data6 <- weightedData(LQ4.merge6[-which(colnames(LQ4.merge6) %in% c("Ind"
                                                                        ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                        ,"Education.Level"
                                                                        ,"Income.Level"))])
LQ4.data6 <- left_join(LQ4.data6, LQ4.merge6[which(colnames(LQ4.merge6) %in% c("CK_Cadmus_ID"
                                                                               ,"Ind"
                                                                               ,"SITE_GENL_INFO_SmartHome_DevicesList"
                                                                               ,"Education.Level"
                                                                               ,"Income.Level"))])
LQ4.data6$count <- 1
LQ4.data6$Count <- 1

#######################
# Weighted Analysis - housing vintages
#######################
# LQ4.data4$HomeYearBuilt_bins_percentage <- LQ4.data4$HomeYearBuilt_bins2
# LQ4.percent <- proportions_one_group(CustomerLevelData = LQ4.data4
#                                      ,valueVariable = "Ind"
#                                      ,groupingVariable = "HomeYearBuilt_bins2"
#                                      ,weighted = TRUE)
#######################
# unweighted Analysis - housing vintages
#######################
LQ4.percent.unw <- proportions_one_group(CustomerLevelData = LQ4.data4
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "HomeYearBuilt_bins2"
                                         ,weighted = FALSE)


#######################
# Weighted Analysis - income level
#######################
# LQ5.percent <- proportions_one_group(CustomerLevelData = LQ4.data5
#                                      ,valueVariable = "Ind"
#                                      ,groupingVariable = "Income.Level"
#                                      ,weighted = TRUE)
#######################
# unweighted Analysis - income level
#######################
LQ5.percent.unw <- proportions_one_group(CustomerLevelData = LQ4.data5
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Income.Level"
                                         ,weighted = FALSE)

#######################
# Weighted Analysis - education level
#######################
# LQ6.percent <- proportions_one_group(CustomerLevelData = LQ4.data6
#                                      ,valueVariable = "Ind"
#                                      ,groupingVariable = "Education.Level"
#                                      ,weighted = TRUE)
#######################
# unweighted Analysis - education level
#######################
LQ6.percent.unw <- proportions_one_group(CustomerLevelData = LQ4.data6
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "Education.Level"
                                         ,weighted = FALSE)
