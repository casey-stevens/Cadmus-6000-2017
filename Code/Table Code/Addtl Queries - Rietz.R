#############################################################################################
##  Title:            additional queries for Andrew                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          07/05/2017
##  Updated:                                             
##  Billing Code(s):  
##  Description:      
#############################################################################################
##  Clear variables
# rm(list = ls())
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
length(unique(rbsa.dat$CK_Cadmus_ID)) 
rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID,ignore.case = T),]

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
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))
rooms.dat <- rooms.dat[which(names(rooms.dat) %in% c("CK_Cadmus_ID","Area"))]
rooms.dat$Area <- as.numeric(as.character(rooms.dat$Area))
rooms.sum <- ddply(rooms.dat, "CK_Cadmus_ID", summarise
                   ,MF.Home.Size = sum(Area, na.rm = T))

one.line.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.export), sheet = "Site One Line Summary", startRow = 2)
#clean cadmus IDs
one.line.dat$CK_Cadmus_ID <- trimws(toupper(one.line.dat$Cadmus.ID))
one.line.dat$CK_Building_ID <- trimws(toupper(one.line.dat$CK_BuildingID))
one.line.dat <- one.line.dat[which(names(one.line.dat) %in% c("CK_Cadmus_ID"
                                                              ,"Conditioned.Area"
                                                              ,"Year.Built"))]
names(one.line.dat) <- c("Year.Built","Home.Size","CK_Cadmus_ID")
one.line.dat$Home.Size <- as.numeric(as.character(one.line.dat$Home.Size))

one.line.final <- left_join(one.line.dat, rooms.sum)

for(ii in 1:nrow(one.line.final)){
  if(is.na(one.line.final$Home.Size[ii])){
    one.line.final$Home.Size[ii] <- one.line.final$MF.Home.Size[ii]
  }else{
    one.line.final$Home.Size[ii]
  }
}

one.line.final$Home.Size <- as.numeric(as.character(one.line.final$Home.Size))
one.line.final$Home.Size.Bins <- as.numeric(as.character(one.line.final$Home.Size))
one.line.final$Home.Size.Bins[which(one.line.final$Home.Size < 501)] <- "Less than 500 sq.ft."
one.line.final$Home.Size.Bins[which(one.line.final$Home.Size >= 501 & one.line.final$Home.Size < 1001)] <- "501-1000 sq.ft."
one.line.final$Home.Size.Bins[which(one.line.final$Home.Size >= 1001 & one.line.final$Home.Size < 1501)] <- "1001-1500 sq.ft."
one.line.final$Home.Size.Bins[which(one.line.final$Home.Size >= 1501 & one.line.final$Home.Size < 2001)] <- "1501-2000 sq.ft."
one.line.final$Home.Size.Bins[which(one.line.final$Home.Size >= 2001 & one.line.final$Home.Size < 2501)] <- "2001-2500 sq.ft."
one.line.final$Home.Size.Bins[which(one.line.final$Home.Size >= 2501 & one.line.final$Home.Size < 3001)] <- "2501-3000 sq.ft."
one.line.final$Home.Size.Bins[which(one.line.final$Home.Size >= 3001 & one.line.final$Home.Size < 3501)] <- "3001-3500 sq.ft."
one.line.final$Home.Size.Bins[which(one.line.final$Home.Size >= 3501 & one.line.final$Home.Size < 4001)] <- "3501-4000 sq.ft."
one.line.final$Home.Size.Bins[which(one.line.final$Home.Size > 4000)] <- "Greater than 4000 sq.ft."
unique(one.line.final$Home.Size.Bins)



#############################################################################################
# Query 17
#############################################################################################
query17.dat <- left_join(rbsa.dat, survey.dat)
query17.dat1 <- query17.dat[which(!is.na(query17.dat$Education.Level)),]
query17.dat2 <- query17.dat1[which(query17.dat1$Education.Level %notin% c("Unknown", "Prefer not to say","N/A")),]
unique(query17.dat2$Education.Level)

################################################
# Adding pop and sample sizes for weights
################################################
query17.data <- weightedData(query17.dat2[-which(colnames(query17.dat2) %in% c("Education.Level"
                                                                               ,"Income.Level"))])
query17.data <- left_join(query17.data, query17.dat2[which(colnames(query17.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Education.Level"
                                                                                         ,"Income.Level"))])
query17.data$count <- 1
#######################
# Weighted Analysis
#######################
query17.summary <- proportionRowsAndColumns1(CustomerLevelData = query17.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Education.Level"
                                             ,aggregateColumnName = "Region")

query17.cast <- dcast(setDT(query17.summary)
                      ,formula = BuildingType + Education.Level ~ State
                      ,value.var = c("w.percent","w.SE","n","EB"))

query17.table <- data.frame("BuildingType" = query17.cast$BuildingType
                            ,"Education.Level" = query17.cast$Education.Level
                            ,"Percent.ID" = query17.cast$w.percent_ID
                            ,"SE.ID" = query17.cast$w.SE_ID
                            ,"n.ID" = query17.cast$n_ID
                            ,"Percent.MT" = query17.cast$w.percent_MT
                            ,"SE.MT" = query17.cast$w.SE_MT
                            ,"n.MT" = query17.cast$n_MT
                            ,"Percent.OR" = query17.cast$w.percent_OR
                            ,"SE.OR" = query17.cast$w.SE_OR
                            ,"n.OR" = query17.cast$n_OR
                            ,"Percent.WA" = query17.cast$w.percent_WA
                            ,"SE.WA" = query17.cast$w.SE_WA
                            ,"n.WA" = query17.cast$n_WA
                            ,"Percent.Region" = query17.cast$w.percent_Region
                            ,"SE.Region" = query17.cast$w.SE_Region
                            ,"n.Region" = query17.cast$n_Region
                            ,"EB.ID" = query17.cast$EB_ID
                            ,"EB.MT" = query17.cast$EB_MT
                            ,"EB.OR" = query17.cast$EB_OR
                            ,"EB.WA" = query17.cast$EB_WA
                            ,"EB.Region" = query17.cast$EB_Region
                            )
query17.table.SF <- query17.table[which(query17.table$BuildingType == "Single Family")
                                  ,-which(colnames(query17.table) %in% c("BuildingType"))]
query17.table.MH <- query17.table[which(query17.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query17.table) %in% c("BuildingType"))]
query17.table.MF <- query17.table[which(query17.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query17.table) %in% c("BuildingType"))]
#######################
# Unweighted Analysis
#######################
query17.summary <- proportions_two_groups_unweighted(CustomerLevelData = query17.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "State"
                                                     ,rowVariable = "Education.Level"
                                                     ,aggregateColumnName = "Region")

query17.cast <- dcast(setDT(query17.summary)
                      ,formula = BuildingType + Education.Level ~ State
                      ,value.var = c("Percent","SE","n"))

query17.table <- data.frame("BuildingType" = query17.cast$BuildingType
                            ,"Education.Level" = query17.cast$Education.Level
                            ,"Percent.ID" = query17.cast$Percent_ID
                            ,"SE.ID" = query17.cast$SE_ID
                            ,"n.ID" = query17.cast$n_ID
                            ,"Percent.MT" = query17.cast$Percent_MT
                            ,"SE.MT" = query17.cast$SE_MT
                            ,"n.MT" = query17.cast$n_MT
                            ,"Percent.OR" = query17.cast$Percent_OR
                            ,"SE.OR" = query17.cast$SE_OR
                            ,"n.OR" = query17.cast$n_OR
                            ,"Percent.WA" = query17.cast$Percent_WA
                            ,"SE.WA" = query17.cast$SE_WA
                            ,"n.WA" = query17.cast$n_WA
                            ,"Percent.Region" = query17.cast$Percent_Region
                            ,"SE.Region" = query17.cast$SE_Region
                            ,"n.Region" = query17.cast$n_Region
)
query17.table.SF.unw <- query17.table[which(query17.table$BuildingType == "Single Family")
                                  ,-which(colnames(query17.table) %in% c("BuildingType"))]
query17.table.MH.unw <- query17.table[which(query17.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query17.table) %in% c("BuildingType"))]
query17.table.MF.unw <- query17.table[which(query17.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query17.table) %in% c("BuildingType"))]







#############################################################################################
# Query 17
#############################################################################################
query18.dat <- left_join(rbsa.dat, survey.dat)
query18.dat1 <- query18.dat[which(!is.na(query18.dat$Education.Level)),]
query18.dat2 <- query18.dat1[which(query18.dat1$Education.Level %notin% c("Unknown", "Prefer not to say","N/A")),]
unique(query18.dat2$Education.Level)
unique(query18.dat2$Detailed.Region)
query18.dat2$Region.Cat <- "NA"
query18.dat2$Region.Cat[which(query18.dat2$Detailed.Region == "Puget Sound")] <- "PS WA"
query18.dat2$Region.Cat[which(query18.dat2$Detailed.Region == "Western Washington")] <- "W WA"
query18.dat2$Region.Cat[which(query18.dat2$Detailed.Region == "Eastern Washington")] <- "E WA"
query18.dat2$Region.Cat[which(query18.dat2$Detailed.Region == "Idaho")] <- "ID"
query18.dat2$Region.Cat[which(query18.dat2$Detailed.Region == "Western Montana")] <- "W MT"
query18.dat2$Region.Cat[which(query18.dat2$Detailed.Region == "Western Oregon")] <- "W OR"
query18.dat2$Region.Cat[which(query18.dat2$Detailed.Region == "Eastern Oregon")] <- "E OR"
unique(query18.dat2$Region.Cat)
################################################
# Adding pop and sample sizes for weights
################################################
query18.data <- weightedData(query18.dat2[-which(colnames(query18.dat2) %in% c("Education.Level"
                                                                               ,"Income.Level"
                                                                               ,"Region.Cat"))])
query18.data <- left_join(query18.data, query18.dat2[which(colnames(query18.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Education.Level"
                                                                                         ,"Income.Level"
                                                                                         ,"Region.Cat"))])
query18.data$count <- 1
#######################
# Weighted Analysis
#######################
query18.summary <- proportionRowsAndColumns1(CustomerLevelData = query18.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "Region.Cat"
                                             ,rowVariable = "Education.Level"
                                             ,aggregateColumnName = "All Regions")

query18.cast <- dcast(setDT(query18.summary)
                      ,formula = BuildingType + Education.Level ~ Region.Cat
                      ,value.var = c("w.percent","w.SE","n","EB"))

query18.table <- data.frame("BuildingType" = query18.cast$BuildingType
                            ,"Education.Level" = query18.cast$Education.Level
                            ,"Percent.ID" = query18.cast$w.percent_ID
                            ,"SE.ID" = query18.cast$w.SE_ID
                            ,"n.ID" = query18.cast$n_ID
                            ,"Percent.W.MT" = query18.cast$`w.percent_W MT`
                            ,"SE.W.MT" = query18.cast$`w.SE_W MT`
                            ,"n.W.MT" = query18.cast$`n_W MT`
                            ,"Percent.E.OR" = query18.cast$`w.percent_E OR`
                            ,"SE.E.OR" = query18.cast$`w.SE_E OR`
                            ,"n.E.OR" = query18.cast$`n_E OR`
                            ,"Percent.W.OR" = query18.cast$`w.percent_W OR`
                            ,"SE.W.OR" = query18.cast$`w.SE_W OR`
                            ,"n.W.OR" = query18.cast$`n_W OR`
                            ,"Percent.E.WA" = query18.cast$`w.percent_E WA`
                            ,"SE.E.WA" = query18.cast$`w.SE_E WA`
                            ,"n.E.WA" = query18.cast$`n_E WA`
                            ,"Percent.W.WA" = query18.cast$`w.percent_W WA`
                            ,"SE.W.WA" = query18.cast$`w.SE_W WA`
                            ,"n.W.WA" = query18.cast$`n_W WA`
                            ,"Percent.PS.WA" = query18.cast$`w.percent_PS WA`
                            ,"SE.PS.WA" = query18.cast$`w.SE_PS WA`
                            ,"n.PS.WA" = query18.cast$`n_PS WA`
                            ,"Percent.All.Regions" = query18.cast$`w.percent_All Regions`
                            ,"SE.All.Regions" = query18.cast$`w.SE_All Regions`
                            ,"n.All.Regions" = query18.cast$`n_All Regions`
                            ,"EB.ID" = query18.cast$EB_ID
                            ,"EB.W.MT" = query18.cast$`EB_W MT`
                            ,"EB.E.OR" = query18.cast$`EB_E OR`
                            ,"EB.W.OR" = query18.cast$`EB_W OR`
                            ,"EB.E.WA" = query18.cast$`EB_E WA`
                            ,"EB.W.WA" = query18.cast$`EB_W WA`
                            ,"EB.PS.WA" = query18.cast$`EB_PS WA`
                            ,"EB.All.Regions" = query18.cast$`EB_All Regions`
)
query18.table.SF <- query18.table[which(query18.table$BuildingType == "Single Family")
                                  ,-which(colnames(query18.table) %in% c("BuildingType"))]
query18.table.MH <- query18.table[which(query18.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query18.table) %in% c("BuildingType"))]
query18.table.MF <- query18.table[which(query18.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query18.table) %in% c("BuildingType"))]
#######################
# Unweighted Analysis
#######################
query18.summary <- proportions_two_groups_unweighted(CustomerLevelData = query18.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "Region.Cat"
                                                     ,rowVariable = "Education.Level"
                                                     ,aggregateColumnName = "All Regions")

query18.cast <- dcast(setDT(query18.summary)
                      ,formula = BuildingType + Education.Level ~ Region.Cat
                      ,value.var = c("Percent","SE","n"))

query18.table <- data.frame("BuildingType" = query18.cast$BuildingType
                            ,"Education.Level" = query18.cast$Education.Level
                            ,"Percent.ID" = query18.cast$Percent_ID
                            ,"SE.ID" = query18.cast$SE_ID
                            ,"n.ID" = query18.cast$n_ID
                            ,"Percent.W.MT" = query18.cast$`Percent_W MT`
                            ,"SE.W.MT" = query18.cast$`SE_W MT`
                            ,"n.W.MT" = query18.cast$`n_W MT`
                            ,"Percent.E.OR" = query18.cast$`Percent_E OR`
                            ,"SE.E.OR" = query18.cast$`SE_E OR`
                            ,"n.E.OR" = query18.cast$`n_E OR`
                            ,"Percent.W.OR" = query18.cast$`Percent_W OR`
                            ,"SE.W.OR" = query18.cast$`SE_W OR`
                            ,"n.W.OR" = query18.cast$`n_W OR`
                            ,"Percent.E.WA" = query18.cast$`Percent_E WA`
                            ,"SE.E.WA" = query18.cast$`SE_E WA`
                            ,"n.E.WA" = query18.cast$`n_E WA`
                            ,"Percent.W.WA" = query18.cast$`Percent_W WA`
                            ,"SE.W.WA" = query18.cast$`SE_W WA`
                            ,"n.W.WA" = query18.cast$`n_W WA`
                            ,"Percent.PS.WA" = query18.cast$`Percent_PS WA`
                            ,"SE.PS.WA" = query18.cast$`SE_PS WA`
                            ,"n.PS.WA" = query18.cast$`n_PS WA`
                            ,"Percent.All.Regions" = query18.cast$`Percent_All Regions`
                            ,"SE.All.Regions" = query18.cast$`SE_All Regions`
                            ,"n.All.Regions" = query18.cast$`n_All Regions`
)
query18.table.SF.unw <- query18.table[which(query18.table$BuildingType == "Single Family")
                                  ,-which(colnames(query18.table) %in% c("BuildingType"))]
query18.table.MH.unw <- query18.table[which(query18.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query18.table) %in% c("BuildingType"))]
query18.table.MF.unw <- query18.table[which(query18.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query18.table) %in% c("BuildingType"))]










#############################################################################################
# Query 19
#############################################################################################
query19.dat <- left_join(rbsa.dat, survey.dat)
query19.dat1 <- query19.dat[which(!is.na(query19.dat$Income.Level)),]
query19.dat2 <- query19.dat1[which(query19.dat1$Income.Level %notin% c("Unknown", "Prefer not to say","N/A","Above $50,000","Below $50,000")),]
unique(query19.dat2$Income.Level)

################################################
# Adding pop and sample sizes for weights
################################################
query19.data <- weightedData(query19.dat2[-which(colnames(query19.dat2) %in% c("Education.Level"
                                                                               ,"Income.Level"))])
query19.data <- left_join(query19.data, query19.dat2[which(colnames(query19.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Education.Level"
                                                                                         ,"Income.Level"))])
query19.data$count <- 1
#######################
# Weighted Analysis
#######################
query19.summary <- proportionRowsAndColumns1(CustomerLevelData = query19.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Income.Level"
                                             ,aggregateColumnName = "Region")

query19.cast <- dcast(setDT(query19.summary)
                      ,formula = BuildingType + Income.Level ~ State
                      ,value.var = c("w.percent","w.SE","n","EB"))

query19.table <- data.frame("BuildingType" = query19.cast$BuildingType
                            ,"Income.Level" = query19.cast$Income.Level
                            ,"Percent.ID" = query19.cast$w.percent_ID
                            ,"SE.ID" = query19.cast$w.SE_ID
                            ,"n.ID" = query19.cast$n_ID
                            ,"Percent.MT" = query19.cast$w.percent_MT
                            ,"SE.MT" = query19.cast$w.SE_MT
                            ,"n.MT" = query19.cast$n_MT
                            ,"Percent.OR" = query19.cast$w.percent_OR
                            ,"SE.OR" = query19.cast$w.SE_OR
                            ,"n.OR" = query19.cast$n_OR
                            ,"Percent.WA" = query19.cast$w.percent_WA
                            ,"SE.WA" = query19.cast$w.SE_WA
                            ,"n.WA" = query19.cast$n_WA
                            ,"Percent.Region" = query19.cast$w.percent_Region
                            ,"SE.Region" = query19.cast$w.SE_Region
                            ,"n.Region" = query19.cast$n_Region
                            ,"EB.ID" = query19.cast$EB_ID
                            ,"EB.MT" = query19.cast$EB_MT
                            ,"EB.OR" = query19.cast$EB_OR
                            ,"EB.WA" = query19.cast$EB_WA
                            ,"EB.Region" = query19.cast$EB_Region
)

levels(query19.table$Income.Level)
rowOrder <- c("Under $5,000"
              ,"$5,000 to under $10,000"
              ,"$10,000 to under $15,000"
              ,"$15,000 to under $20,000"
              ,"$20,000 to under $25,000"
              ,"$25,000 to under $30,000"
              ,"$30,000 to under $35,000"
              ,"$35,000 to under $40,000"
              ,"$40,000 to under $45,000"
              ,"$45,000 to under $50,000"
              ,"$50,000 to under $55,000"
              ,"$55,000 to under $60,000"
              ,"$60,000 to under $65,000"
              ,"$65,000 to under $70,000"
              ,"$70,000 to under $75,000"
              ,"$75,000 to under $80,000"
              ,"$80,000 to under $85,000"
              ,"$85,000 to under $90,000"
              ,"$90,000 to under $95,000"
              ,"$95,000 to under $100,000"
              ,"$100,000 to under $150,000"
              ,"$150,000 to under $200,000"
              ,"$200,000 or more"
              ,"Total")
query19.table <- query19.table %>% mutate(Income.Level = factor(Income.Level, levels = rowOrder)) %>% arrange(Income.Level)  
query19.table <- data.frame(query19.table)

query19.table.SF <- query19.table[which(query19.table$BuildingType == "Single Family")
                                  ,-which(colnames(query19.table) %in% c("BuildingType"))]
query19.table.MH <- query19.table[which(query19.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query19.table) %in% c("BuildingType"))]
query19.table.MF <- query19.table[which(query19.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query19.table) %in% c("BuildingType"))]
#######################
# Unweighted Analysis
#######################
query19.summary <- proportions_two_groups_unweighted(CustomerLevelData = query19.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "State"
                                                     ,rowVariable = "Income.Level"
                                                     ,aggregateColumnName = "Region")

query19.cast <- dcast(setDT(query19.summary)
                      ,formula = BuildingType + Income.Level ~ State
                      ,value.var = c("Percent","SE","n"))

query19.table <- data.frame("BuildingType" = query19.cast$BuildingType
                            ,"Income.Level" = query19.cast$Income.Level
                            ,"Percent.ID" = query19.cast$Percent_ID
                            ,"SE.ID" = query19.cast$SE_ID
                            ,"n.ID" = query19.cast$n_ID
                            ,"Percent.MT" = query19.cast$Percent_MT
                            ,"SE.MT" = query19.cast$SE_MT
                            ,"n.MT" = query19.cast$n_MT
                            ,"Percent.OR" = query19.cast$Percent_OR
                            ,"SE.OR" = query19.cast$SE_OR
                            ,"n.OR" = query19.cast$n_OR
                            ,"Percent.WA" = query19.cast$Percent_WA
                            ,"SE.WA" = query19.cast$SE_WA
                            ,"n.WA" = query19.cast$n_WA
                            ,"Percent.Region" = query19.cast$Percent_Region
                            ,"SE.Region" = query19.cast$SE_Region
                            ,"n.Region" = query19.cast$n_Region
)
rowOrder <- c("Under $5,000"
              ,"$5,000 to under $10,000"
              ,"$10,000 to under $15,000"
              ,"$15,000 to under $20,000"
              ,"$20,000 to under $25,000"
              ,"$25,000 to under $30,000"
              ,"$30,000 to under $35,000"
              ,"$35,000 to under $40,000"
              ,"$40,000 to under $45,000"
              ,"$45,000 to under $50,000"
              ,"$50,000 to under $55,000"
              ,"$55,000 to under $60,000"
              ,"$60,000 to under $65,000"
              ,"$65,000 to under $70,000"
              ,"$70,000 to under $75,000"
              ,"$75,000 to under $80,000"
              ,"$80,000 to under $85,000"
              ,"$85,000 to under $90,000"
              ,"$90,000 to under $95,000"
              ,"$95,000 to under $100,000"
              ,"$100,000 to under $150,000"
              ,"$150,000 to under $200,000"
              ,"$200,000 or more"
              ,"Total")
query19.table.SF.unw <- query19.table[which(query19.table$BuildingType == "Single Family")
                                  ,-which(colnames(query19.table) %in% c("BuildingType"))]
query19.table.MH.unw <- query19.table[which(query19.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query19.table) %in% c("BuildingType"))]
query19.table.MF.unw <- query19.table[which(query19.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query19.table) %in% c("BuildingType"))]







#############################################################################################
# Query 20
#############################################################################################
query20.dat <- left_join(rbsa.dat, survey.dat)
query20.dat1 <- query20.dat[which(!is.na(query20.dat$Income.Level)),]
query20.dat2 <- query20.dat1[which(query20.dat1$Income.Level %notin% c("Unknown", "Prefer not to say","N/A","Above $50,000","Below $50,000")),]
unique(query20.dat2$Income.Level)
unique(query20.dat2$Detailed.Region)
query20.dat2$Region.Cat <- "NA"
query20.dat2$Region.Cat[which(query20.dat2$Detailed.Region == "Puget Sound")] <- "PS WA"
query20.dat2$Region.Cat[which(query20.dat2$Detailed.Region == "Western Washington")] <- "W WA"
query20.dat2$Region.Cat[which(query20.dat2$Detailed.Region == "Eastern Washington")] <- "E WA"
query20.dat2$Region.Cat[which(query20.dat2$Detailed.Region == "Idaho")] <- "ID"
query20.dat2$Region.Cat[which(query20.dat2$Detailed.Region == "Western Montana")] <- "W MT"
query20.dat2$Region.Cat[which(query20.dat2$Detailed.Region == "Western Oregon")] <- "W OR"
query20.dat2$Region.Cat[which(query20.dat2$Detailed.Region == "Eastern Oregon")] <- "E OR"
unique(query20.dat2$Region.Cat)
################################################
# Adding pop and sample sizes for weights
################################################
query20.data <- weightedData(query20.dat2[-which(colnames(query20.dat2) %in% c("Education.Level"
                                                                               ,"Income.Above.or.Below.50000"
                                                                               ,"Income.Above.or.Below.25000"
                                                                               ,"Income.0.to.25000"
                                                                               ,"Income.25000.to.50000"
                                                                               ,"Income.More.Than.50000"
                                                                               ,"Income.Level"
                                                                               ,"Region.Cat"))])
query20.data <- left_join(query20.data, query20.dat2[which(colnames(query20.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Education.Level"
                                                                                         ,"Income.Above.or.Below.50000"
                                                                                         ,"Income.Above.or.Below.25000"
                                                                                         ,"Income.0.to.25000"
                                                                                         ,"Income.25000.to.50000"
                                                                                         ,"Income.More.Than.50000"
                                                                                         ,"Income.Level"
                                                                                         ,"Region.Cat"))])
query20.data$count <- 1
#######################
# Weighted Analysis
#######################
query20.summary <- proportionRowsAndColumns1(CustomerLevelData = query20.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "Region.Cat"
                                             ,rowVariable = "Income.Level"
                                             ,aggregateColumnName = "All Regions")

query20.cast <- dcast(setDT(query20.summary)
                      ,formula = BuildingType + Income.Level ~ Region.Cat
                      ,value.var = c("w.percent","w.SE","n","EB"))

query20.table <- data.frame("BuildingType" = query20.cast$BuildingType
                            ,"Income.Level" = query20.cast$Income.Level
                            ,"Percent.ID" = query20.cast$w.percent_ID
                            ,"SE.ID" = query20.cast$w.SE_ID
                            ,"n.ID" = query20.cast$n_ID
                            ,"Percent.W.MT" = query20.cast$`w.percent_W MT`
                            ,"SE.W.MT" = query20.cast$`w.SE_W MT`
                            ,"n.W.MT" = query20.cast$`n_W MT`
                            ,"Percent.E.OR" = query20.cast$`w.percent_E OR`
                            ,"SE.E.OR" = query20.cast$`w.SE_E OR`
                            ,"n.E.OR" = query20.cast$`n_E OR`
                            ,"Percent.W.OR" = query20.cast$`w.percent_W OR`
                            ,"SE.W.OR" = query20.cast$`w.SE_W OR`
                            ,"n.W.OR" = query20.cast$`n_W OR`
                            ,"Percent.E.WA" = query20.cast$`w.percent_E WA`
                            ,"SE.E.WA" = query20.cast$`w.SE_E WA`
                            ,"n.E.WA" = query20.cast$`n_E WA`
                            ,"Percent.W.WA" = query20.cast$`w.percent_W WA`
                            ,"SE.W.WA" = query20.cast$`w.SE_W WA`
                            ,"n.W.WA" = query20.cast$`n_W WA`
                            ,"Percent.PS.WA" = query20.cast$`w.percent_PS WA`
                            ,"SE.PS.WA" = query20.cast$`w.SE_PS WA`
                            ,"n.PS.WA" = query20.cast$`n_PS WA`
                            ,"Percent.All.Regions" = query20.cast$`w.percent_All Regions`
                            ,"SE.All.Regions" = query20.cast$`w.SE_All Regions`
                            ,"n.All.Regions" = query20.cast$`n_All Regions`
                            ,"EB.ID" = query20.cast$EB_ID
                            ,"EB.W.MT" = query20.cast$`EB_W MT`
                            ,"EB.E.OR" = query20.cast$`EB_E OR`
                            ,"EB.W.OR" = query20.cast$`EB_W OR`
                            ,"EB.E.WA" = query20.cast$`EB_E WA`
                            ,"EB.W.WA" = query20.cast$`EB_W WA`
                            ,"EB.PS.WA" = query20.cast$`EB_PS WA`
                            ,"EB.All.Regions" = query20.cast$`EB_All Regions`
)
levels(query20.table$Income.Level)
rowOrder <- c("Under $5,000"
              ,"$5,000 to under $10,000"
              ,"$10,000 to under $15,000"
              ,"$15,000 to under $20,000"
              ,"$20,000 to under $25,000"
              ,"$25,000 to under $30,000"
              ,"$30,000 to under $35,000"
              ,"$35,000 to under $40,000"
              ,"$40,000 to under $45,000"
              ,"$45,000 to under $50,000"
              ,"$50,000 to under $55,000"
              ,"$55,000 to under $60,000"
              ,"$60,000 to under $65,000"
              ,"$65,000 to under $70,000"
              ,"$70,000 to under $75,000"
              ,"$75,000 to under $80,000"
              ,"$80,000 to under $85,000"
              ,"$85,000 to under $90,000"
              ,"$90,000 to under $95,000"
              ,"$95,000 to under $100,000"
              ,"$100,000 to under $150,000"
              ,"$150,000 to under $200,000"
              ,"$200,000 or more"
              ,"Total")
query20.table <- query20.table %>% mutate(Income.Level = factor(Income.Level, levels = rowOrder)) %>% arrange(Income.Level)  
query20.table <- data.frame(query20.table)

query20.table.SF <- query20.table[which(query20.table$BuildingType == "Single Family")
                                  ,-which(colnames(query20.table) %in% c("BuildingType"))]
query20.table.MH <- query20.table[which(query20.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query20.table) %in% c("BuildingType"))]
query20.table.MF <- query20.table[which(query20.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query20.table) %in% c("BuildingType"))]
#######################
# Unweighted Analysis
#######################
query20.summary <- proportions_two_groups_unweighted(CustomerLevelData = query20.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "Region.Cat"
                                                     ,rowVariable = "Income.Level"
                                                     ,aggregateColumnName = "All Regions")

query20.cast <- dcast(setDT(query20.summary)
                      ,formula = BuildingType + Income.Level ~ Region.Cat
                      ,value.var = c("Percent","SE","n"))

query20.table <- data.frame("BuildingType" = query20.cast$BuildingType
                            ,"Income.Level" = query20.cast$Income.Level
                            ,"Percent.ID" = query20.cast$Percent_ID
                            ,"SE.ID" = query20.cast$SE_ID
                            ,"n.ID" = query20.cast$n_ID
                            ,"Percent.W.MT" = query20.cast$`Percent_W MT`
                            ,"SE.W.MT" = query20.cast$`SE_W MT`
                            ,"n.W.MT" = query20.cast$`n_W MT`
                            ,"Percent.E.OR" = query20.cast$`Percent_E OR`
                            ,"SE.E.OR" = query20.cast$`SE_E OR`
                            ,"n.E.OR" = query20.cast$`n_E OR`
                            ,"Percent.W.OR" = query20.cast$`Percent_W OR`
                            ,"SE.W.OR" = query20.cast$`SE_W OR`
                            ,"n.W.OR" = query20.cast$`n_W OR`
                            ,"Percent.E.WA" = query20.cast$`Percent_E WA`
                            ,"SE.E.WA" = query20.cast$`SE_E WA`
                            ,"n.E.WA" = query20.cast$`n_E WA`
                            ,"Percent.W.WA" = query20.cast$`Percent_W WA`
                            ,"SE.W.WA" = query20.cast$`SE_W WA`
                            ,"n.W.WA" = query20.cast$`n_W WA`
                            ,"Percent.PS.WA" = query20.cast$`Percent_PS WA`
                            ,"SE.PS.WA" = query20.cast$`SE_PS WA`
                            ,"n.PS.WA" = query20.cast$`n_PS WA`
                            ,"Percent.All.Regions" = query20.cast$`Percent_All Regions`
                            ,"SE.All.Regions" = query20.cast$`SE_All Regions`
                            ,"n.All.Regions" = query20.cast$`n_All Regions`
)
levels(query20.table$Income.Level)
rowOrder <- c("Under $5,000"
              ,"$5,000 to under $10,000"
              ,"$10,000 to under $15,000"
              ,"$15,000 to under $20,000"
              ,"$20,000 to under $25,000"
              ,"$25,000 to under $30,000"
              ,"$30,000 to under $35,000"
              ,"$35,000 to under $40,000"
              ,"$40,000 to under $45,000"
              ,"$45,000 to under $50,000"
              ,"$50,000 to under $55,000"
              ,"$55,000 to under $60,000"
              ,"$60,000 to under $65,000"
              ,"$65,000 to under $70,000"
              ,"$70,000 to under $75,000"
              ,"$75,000 to under $80,000"
              ,"$80,000 to under $85,000"
              ,"$85,000 to under $90,000"
              ,"$90,000 to under $95,000"
              ,"$95,000 to under $100,000"
              ,"$100,000 to under $150,000"
              ,"$150,000 to under $200,000"
              ,"$200,000 or more"
              ,"Total")
query20.table <- query20.table %>% mutate(Income.Level = factor(Income.Level, levels = rowOrder)) %>% arrange(Income.Level)  
query20.table <- data.frame(query20.table)

query20.table.SF.unw <- query20.table[which(query20.table$BuildingType == "Single Family")
                                  ,-which(colnames(query20.table) %in% c("BuildingType"))]
query20.table.MH.unw <- query20.table[which(query20.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query20.table) %in% c("BuildingType"))]
query20.table.MF.unw <- query20.table[which(query20.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query20.table) %in% c("BuildingType"))]











#############################################################################################
# Query 21
#############################################################################################
query21.dat <- left_join(rbsa.dat, one.line.final)
query21.dat1 <- query21.dat[which(!is.na(query21.dat$Home.Size.Bins)),]
query21.dat2 <- query21.dat1[which(query21.dat1$Home.Size %notin% c("Unknown", "Prefer not to say","N/A")),]
unique(query21.dat2$Home.Size.Bins)

################################################
# Adding pop and sample sizes for weights
################################################
query21.data <- weightedData(query21.dat2[-which(colnames(query21.dat2) %in% c("Year.Built"
                                                                               ,"Home.Size"
                                                                               ,"Home.Size.Bins"
                                                                               ,"MF.Home.Size"))])
query21.data <- left_join(query21.data, query21.dat2[which(colnames(query21.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Year.Built"
                                                                                         ,"Home.Size"
                                                                                         ,"Home.Size.Bins"
                                                                                         ,"MF.Home.Size"))])
query21.data$count <- 1
#######################
# Weighted Analysis
#######################
query21.summary <- proportionRowsAndColumns1(CustomerLevelData = query21.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Home.Size.Bins"
                                             ,aggregateColumnName = "Region")

query21.cast <- dcast(setDT(query21.summary)
                      ,formula = BuildingType + Home.Size.Bins ~ State
                      ,value.var = c("w.percent","w.SE","n","EB"))

query21.table <- data.frame("BuildingType" = query21.cast$BuildingType
                            ,"Home.Size" = query21.cast$Home.Size.Bins
                            ,"Percent.ID" = query21.cast$w.percent_ID
                            ,"SE.ID" = query21.cast$w.SE_ID
                            ,"n.ID" = query21.cast$n_ID
                            ,"Percent.MT" = query21.cast$w.percent_MT
                            ,"SE.MT" = query21.cast$w.SE_MT
                            ,"n.MT" = query21.cast$n_MT
                            ,"Percent.OR" = query21.cast$w.percent_OR
                            ,"SE.OR" = query21.cast$w.SE_OR
                            ,"n.OR" = query21.cast$n_OR
                            ,"Percent.WA" = query21.cast$w.percent_WA
                            ,"SE.WA" = query21.cast$w.SE_WA
                            ,"n.WA" = query21.cast$n_WA
                            ,"Percent.Region" = query21.cast$w.percent_Region
                            ,"SE.Region" = query21.cast$w.SE_Region
                            ,"n.Region" = query21.cast$n_Region
                            ,"EB.ID" = query21.cast$EB_ID
                            ,"EB.MT" = query21.cast$EB_MT
                            ,"EB.OR" = query21.cast$EB_OR
                            ,"EB.WA" = query21.cast$EB_WA
                            ,"EB.Region" = query21.cast$EB_Region
)

levels(query21.table$Home.Size)
rowOrder <- c("Less than 500 sq.ft."
              ,"501-1000 sq.ft."
              ,"1001-1500 sq.ft."
              ,"1501-2000 sq.ft."
              ,"2001-2500 sq.ft."
              ,"2501-3000 sq.ft."
              ,"3001-3500 sq.ft."
              ,"3501-4000 sq.ft."
              ,"Greater than 4000 sq.ft."
              ,"Total")
query21.table <- query21.table %>% mutate(Home.Size = factor(Home.Size, levels = rowOrder)) %>% arrange(Home.Size)  
query21.table <- data.frame(query21.table)

query21.table.SF <- query21.table[which(query21.table$BuildingType == "Single Family")
                                  ,-which(colnames(query21.table) %in% c("BuildingType"))]
query21.table.MH <- query21.table[which(query21.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query21.table) %in% c("BuildingType"))]
query21.table.MF <- query21.table[which(query21.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query21.table) %in% c("BuildingType"))]
#######################
# Unweighted Analysis
#######################
query21.summary <- proportions_two_groups_unweighted(CustomerLevelData = query21.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "State"
                                                     ,rowVariable = "Home.Size.Bins"
                                                     ,aggregateColumnName = "Region")

query21.cast <- dcast(setDT(query21.summary)
                      ,formula = BuildingType + Home.Size.Bins ~ State
                      ,value.var = c("Percent","SE","n"))

query21.table <- data.frame("BuildingType" = query21.cast$BuildingType
                            ,"Home.Size" = query21.cast$Home.Size.Bins
                            ,"Percent.ID" = query21.cast$Percent_ID
                            ,"SE.ID" = query21.cast$SE_ID
                            ,"n.ID" = query21.cast$n_ID
                            ,"Percent.MT" = query21.cast$Percent_MT
                            ,"SE.MT" = query21.cast$SE_MT
                            ,"n.MT" = query21.cast$n_MT
                            ,"Percent.OR" = query21.cast$Percent_OR
                            ,"SE.OR" = query21.cast$SE_OR
                            ,"n.OR" = query21.cast$n_OR
                            ,"Percent.WA" = query21.cast$Percent_WA
                            ,"SE.WA" = query21.cast$SE_WA
                            ,"n.WA" = query21.cast$n_WA
                            ,"Percent.Region" = query21.cast$Percent_Region
                            ,"SE.Region" = query21.cast$SE_Region
                            ,"n.Region" = query21.cast$n_Region
)
levels(query21.table$Home.Size)
rowOrder <- c("Less than 500 sq.ft."
              ,"501-1000 sq.ft."
              ,"1001-1500 sq.ft."
              ,"1501-2000 sq.ft."
              ,"2001-2500 sq.ft."
              ,"2501-3000 sq.ft."
              ,"3001-3500 sq.ft."
              ,"3501-4000 sq.ft."
              ,"Greater than 4000 sq.ft."
              ,"Total")
query21.table <- query21.table %>% mutate(Home.Size = factor(Home.Size, levels = rowOrder)) %>% arrange(Home.Size)  
query21.table <- data.frame(query21.table)
query21.table.SF.unw <- query21.table[which(query21.table$BuildingType == "Single Family")
                                  ,-which(colnames(query21.table) %in% c("BuildingType"))]
query21.table.MH.unw <- query21.table[which(query21.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query21.table) %in% c("BuildingType"))]
query21.table.MF.unw <- query21.table[which(query21.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query21.table) %in% c("BuildingType"))]







#############################################################################################
# Query 20
#############################################################################################
query22.dat <- left_join(rbsa.dat, one.line.final)
query22.dat1 <- query22.dat[which(!is.na(query22.dat$Home.Size.Bins)),]
query22.dat2 <- query22.dat1[which(query22.dat1$Home.Size %notin% c("Unknown", "Prefer not to say","N/A")),]
unique(query22.dat2$Home.Size.Bins)
unique(query22.dat2$Detailed.Region)
query22.dat2$Region.Cat <- "NA"
query22.dat2$Region.Cat[which(query22.dat2$Detailed.Region == "Puget Sound")] <- "PS WA"
query22.dat2$Region.Cat[which(query22.dat2$Detailed.Region == "Western Washington")] <- "W WA"
query22.dat2$Region.Cat[which(query22.dat2$Detailed.Region == "Eastern Washington")] <- "E WA"
query22.dat2$Region.Cat[which(query22.dat2$Detailed.Region == "Idaho")] <- "ID"
query22.dat2$Region.Cat[which(query22.dat2$Detailed.Region == "Western Montana")] <- "W MT"
query22.dat2$Region.Cat[which(query22.dat2$Detailed.Region == "Western Oregon")] <- "W OR"
query22.dat2$Region.Cat[which(query22.dat2$Detailed.Region == "Eastern Oregon")] <- "E OR"
unique(query22.dat2$Region.Cat)
################################################
# Adding pop and sample sizes for weights
################################################
query22.data <- weightedData(query22.dat2[-which(colnames(query22.dat2) %in% c("Year.Built"
                                                                               ,"Home.Size"
                                                                               ,"MF.Home.Size"
                                                                               ,"Region.Cat"
                                                                               ,"Home.Size.Bins"))])
query22.data <- left_join(query22.data, query22.dat2[which(colnames(query22.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Year.Built"
                                                                                         ,"Home.Size"
                                                                                         ,"MF.Home.Size"
                                                                                         ,"Region.Cat"
                                                                                         ,"Home.Size.Bins"))])
query22.data$count <- 1
#######################
# Weighted Analysis
#######################
query22.summary <- proportionRowsAndColumns1(CustomerLevelData = query22.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "Region.Cat"
                                             ,rowVariable = "Home.Size.Bins"
                                             ,aggregateColumnName = "All Regions")

query22.cast <- dcast(setDT(query22.summary)
                      ,formula = BuildingType + Home.Size.Bins ~ Region.Cat
                      ,value.var = c("w.percent","w.SE","n","EB"))

query22.table <- data.frame("BuildingType" = query22.cast$BuildingType
                            ,"Home.Size" = query22.cast$Home.Size.Bins
                            ,"Percent.ID" = query22.cast$w.percent_ID
                            ,"SE.ID" = query22.cast$w.SE_ID
                            ,"n.ID" = query22.cast$n_ID
                            ,"Percent.W.MT" = query22.cast$`w.percent_W MT`
                            ,"SE.W.MT" = query22.cast$`w.SE_W MT`
                            ,"n.W.MT" = query22.cast$`n_W MT`
                            ,"Percent.E.OR" = query22.cast$`w.percent_E OR`
                            ,"SE.E.OR" = query22.cast$`w.SE_E OR`
                            ,"n.E.OR" = query22.cast$`n_E OR`
                            ,"Percent.W.OR" = query22.cast$`w.percent_W OR`
                            ,"SE.W.OR" = query22.cast$`w.SE_W OR`
                            ,"n.W.OR" = query22.cast$`n_W OR`
                            ,"Percent.E.WA" = query22.cast$`w.percent_E WA`
                            ,"SE.E.WA" = query22.cast$`w.SE_E WA`
                            ,"n.E.WA" = query22.cast$`n_E WA`
                            ,"Percent.W.WA" = query22.cast$`w.percent_W WA`
                            ,"SE.W.WA" = query22.cast$`w.SE_W WA`
                            ,"n.W.WA" = query22.cast$`n_W WA`
                            ,"Percent.PS.WA" = query22.cast$`w.percent_PS WA`
                            ,"SE.PS.WA" = query22.cast$`w.SE_PS WA`
                            ,"n.PS.WA" = query22.cast$`n_PS WA`
                            ,"Percent.All.Regions" = query22.cast$`w.percent_All Regions`
                            ,"SE.All.Regions" = query22.cast$`w.SE_All Regions`
                            ,"n.All.Regions" = query22.cast$`n_All Regions`
                            ,"EB.ID" = query22.cast$EB_ID
                            ,"EB.W.MT" = query22.cast$`EB_W MT`
                            ,"EB.E.OR" = query22.cast$`EB_E OR`
                            ,"EB.W.OR" = query22.cast$`EB_W OR`
                            ,"EB.E.WA" = query22.cast$`EB_E WA`
                            ,"EB.W.WA" = query22.cast$`EB_W WA`
                            ,"EB.PS.WA" = query22.cast$`EB_PS WA`
                            ,"EB.All.Regions" = query22.cast$`EB_All Regions`
)
levels(query22.table$Home.Size)
rowOrder <- c("Less than 500 sq.ft."
              ,"501-1000 sq.ft."
              ,"1001-1500 sq.ft."
              ,"1501-2000 sq.ft."
              ,"2001-2500 sq.ft."
              ,"2501-3000 sq.ft."
              ,"3001-3500 sq.ft."
              ,"3501-4000 sq.ft."
              ,"Greater than 4000 sq.ft."
              ,"Total")
query22.table <- query22.table %>% mutate(Home.Size = factor(Home.Size, levels = rowOrder)) %>% arrange(Home.Size)  
query22.table <- data.frame(query22.table)
query22.table.SF <- query22.table[which(query22.table$BuildingType == "Single Family")
                                  ,-which(colnames(query22.table) %in% c("BuildingType"))]
query22.table.MH <- query22.table[which(query22.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query22.table) %in% c("BuildingType"))]
query22.table.MF <- query22.table[which(query22.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query22.table) %in% c("BuildingType"))]
#######################
# Unweighted Analysis
#######################
query22.summary <- proportions_two_groups_unweighted(CustomerLevelData = query22.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "Region.Cat"
                                                     ,rowVariable = "Home.Size.Bins"
                                                     ,aggregateColumnName = "All Regions")

query22.cast <- dcast(setDT(query22.summary)
                      ,formula = BuildingType + Home.Size.Bins ~ Region.Cat
                      ,value.var = c("Percent","SE","n"))

query22.table <- data.frame("BuildingType" = query22.cast$BuildingType
                            ,"Home.Size" = query22.cast$Home.Size.Bins
                            ,"Percent.ID" = query22.cast$Percent_ID
                            ,"SE.ID" = query22.cast$SE_ID
                            ,"n.ID" = query22.cast$n_ID
                            ,"Percent.W.MT" = query22.cast$`Percent_W MT`
                            ,"SE.W.MT" = query22.cast$`SE_W MT`
                            ,"n.W.MT" = query22.cast$`n_W MT`
                            ,"Percent.E.OR" = query22.cast$`Percent_E OR`
                            ,"SE.E.OR" = query22.cast$`SE_E OR`
                            ,"n.E.OR" = query22.cast$`n_E OR`
                            ,"Percent.W.OR" = query22.cast$`Percent_W OR`
                            ,"SE.W.OR" = query22.cast$`SE_W OR`
                            ,"n.W.OR" = query22.cast$`n_W OR`
                            ,"Percent.E.WA" = query22.cast$`Percent_E WA`
                            ,"SE.E.WA" = query22.cast$`SE_E WA`
                            ,"n.E.WA" = query22.cast$`n_E WA`
                            ,"Percent.W.WA" = query22.cast$`Percent_W WA`
                            ,"SE.W.WA" = query22.cast$`SE_W WA`
                            ,"n.W.WA" = query22.cast$`n_W WA`
                            ,"Percent.PS.WA" = query22.cast$`Percent_PS WA`
                            ,"SE.PS.WA" = query22.cast$`SE_PS WA`
                            ,"n.PS.WA" = query22.cast$`n_PS WA`
                            ,"Percent.All.Regions" = query22.cast$`Percent_All Regions`
                            ,"SE.All.Regions" = query22.cast$`SE_All Regions`
                            ,"n.All.Regions" = query22.cast$`n_All Regions`
)
levels(query22.table$Home.Size)
rowOrder <- c("Less than 500 sq.ft."
              ,"501-1000 sq.ft."
              ,"1001-1500 sq.ft."
              ,"1501-2000 sq.ft."
              ,"2001-2500 sq.ft."
              ,"2501-3000 sq.ft."
              ,"3001-3500 sq.ft."
              ,"3501-4000 sq.ft."
              ,"Greater than 4000 sq.ft."
              ,"Total")
query22.table <- query22.table %>% mutate(Home.Size = factor(Home.Size, levels = rowOrder)) %>% arrange(Home.Size)  
query22.table <- data.frame(query22.table)
query22.table.SF.unw <- query22.table[which(query22.table$BuildingType == "Single Family")
                                  ,-which(colnames(query22.table) %in% c("BuildingType"))]
query22.table.MH.unw <- query22.table[which(query22.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query22.table) %in% c("BuildingType"))]
query22.table.MF.unw <- query22.table[which(query22.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query22.table) %in% c("BuildingType"))]












#############################################################################################
# Query 23
#############################################################################################
query23.dat <- left_join(rbsa.dat, one.line.dat)
query23.dat1 <- query23.dat[which(!is.na(query23.dat$HomeYearBuilt)),]
query23.dat2 <- query23.dat1
unique(query23.dat2$HomeYearBuilt)

# Convert home year built to specific bins
query23.dat2$Home.Vintage <- as.numeric(as.character(query23.dat2$HomeYearBuilt))
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt < 1951)] <- "Pre 1951"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1951 & query23.dat2$HomeYearBuilt < 1961)] <- "1951-1955"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1951 & query23.dat2$HomeYearBuilt < 1961)] <- "1956-1960"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1961 & query23.dat2$HomeYearBuilt < 1971)] <- "1961-1965"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1961 & query23.dat2$HomeYearBuilt < 1971)] <- "1966-1970"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1971 & query23.dat2$HomeYearBuilt < 1981)] <- "1971-1975"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1971 & query23.dat2$HomeYearBuilt < 1981)] <- "1976-1980"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1981 & query23.dat2$HomeYearBuilt < 1986)] <- "1981-1985"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1986 & query23.dat2$HomeYearBuilt < 1991)] <- "1986-1990"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1991 & query23.dat2$HomeYearBuilt < 1996)] <- "1991-1995"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 1996 & query23.dat2$HomeYearBuilt < 2001)] <- "1996-2000"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 2001 & query23.dat2$HomeYearBuilt < 2006)] <- "2001-2005"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 2006 & query23.dat2$HomeYearBuilt < 2011)] <- "2006-2010"
query23.dat2$Home.Vintage[which(query23.dat2$HomeYearBuilt >= 2011)] <- "Post 2010"
unique(query23.dat2$Home.Vintage)

################################################
# Adding pop and sample sizes for weights
################################################
query23.data <- weightedData(query23.dat2[-which(colnames(query23.dat2) %in% c("Year.Built"
                                                                               ,"Home.Size"
                                                                               ,"Home.Vintage"))])
query23.data <- left_join(query23.data, query23.dat2[which(colnames(query23.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Year.Built"
                                                                                         ,"Home.Size"
                                                                                         ,"Home.Vintage"))])
query23.data$count <- 1
#######################
# Weighted Analysis
#######################
query23.summary <- proportionRowsAndColumns1(CustomerLevelData = query23.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Home.Vintage"
                                             ,aggregateColumnName = "Region")

query23.cast <- dcast(setDT(query23.summary)
                      ,formula = BuildingType + Home.Vintage ~ State
                      ,value.var = c("w.percent","w.SE","n","EB"))

query23.table <- data.frame("BuildingType" = query23.cast$BuildingType
                            ,"Home.Vintage" = query23.cast$Home.Vintage
                            ,"Percent.ID" = query23.cast$w.percent_ID
                            ,"SE.ID" = query23.cast$w.SE_ID
                            ,"n.ID" = query23.cast$n_ID
                            ,"Percent.MT" = query23.cast$w.percent_MT
                            ,"SE.MT" = query23.cast$w.SE_MT
                            ,"n.MT" = query23.cast$n_MT
                            ,"Percent.OR" = query23.cast$w.percent_OR
                            ,"SE.OR" = query23.cast$w.SE_OR
                            ,"n.OR" = query23.cast$n_OR
                            ,"Percent.WA" = query23.cast$w.percent_WA
                            ,"SE.WA" = query23.cast$w.SE_WA
                            ,"n.WA" = query23.cast$n_WA
                            ,"Percent.Region" = query23.cast$w.percent_Region
                            ,"SE.Region" = query23.cast$w.SE_Region
                            ,"n.Region" = query23.cast$n_Region
                            ,"EB.ID" = query23.cast$EB_ID
                            ,"EB.MT" = query23.cast$EB_MT
                            ,"EB.OR" = query23.cast$EB_OR
                            ,"EB.WA" = query23.cast$EB_WA
                            ,"EB.Region" = query23.cast$EB_Region
)

levels(query23.table$Home.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1955"
              ,"1956-1960"
              ,"1961-1965"
              ,"1966-1970"
              ,"1971-1975"
              ,"1976-1980"
              ,"1981-1985"
              ,"1986-1990"
              ,"1991-1995"
              ,"1996-2000"
              ,"2001-2005"
              ,"2006-2010"
              ,"Post 2010"
              ,"Total")
query23.table <- query23.table %>% mutate(Home.Vintage = factor(Home.Vintage, levels = rowOrder)) %>% arrange(Home.Vintage)  
query23.table <- data.frame(query23.table)

query23.table.SF <- query23.table[which(query23.table$BuildingType == "Single Family")
                                  ,-which(colnames(query23.table) %in% c("BuildingType"))]
query23.table.MH <- query23.table[which(query23.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query23.table) %in% c("BuildingType"))]
query23.table.MF <- query23.table[which(query23.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query23.table) %in% c("BuildingType"))]
#######################
# Unweighted Analysis
#######################
query23.summary <- proportions_two_groups_unweighted(CustomerLevelData = query23.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "State"
                                                     ,rowVariable = "Home.Vintage"
                                                     ,aggregateColumnName = "Region")

query23.cast <- dcast(setDT(query23.summary)
                      ,formula = BuildingType + Home.Vintage ~ State
                      ,value.var = c("Percent","SE","n"))

query23.table <- data.frame("BuildingType" = query23.cast$BuildingType
                            ,"Home.Vintage" = query23.cast$Home.Vintage
                            ,"Percent.ID" = query23.cast$Percent_ID
                            ,"SE.ID" = query23.cast$SE_ID
                            ,"n.ID" = query23.cast$n_ID
                            ,"Percent.MT" = query23.cast$Percent_MT
                            ,"SE.MT" = query23.cast$SE_MT
                            ,"n.MT" = query23.cast$n_MT
                            ,"Percent.OR" = query23.cast$Percent_OR
                            ,"SE.OR" = query23.cast$SE_OR
                            ,"n.OR" = query23.cast$n_OR
                            ,"Percent.WA" = query23.cast$Percent_WA
                            ,"SE.WA" = query23.cast$SE_WA
                            ,"n.WA" = query23.cast$n_WA
                            ,"Percent.Region" = query23.cast$Percent_Region
                            ,"SE.Region" = query23.cast$SE_Region
                            ,"n.Region" = query23.cast$n_Region
)
levels(query23.table$Home.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1955"
              ,"1956-1960"
              ,"1961-1965"
              ,"1966-1970"
              ,"1971-1975"
              ,"1976-1980"
              ,"1981-1985"
              ,"1986-1990"
              ,"1991-1995"
              ,"1996-2000"
              ,"2001-2005"
              ,"2006-2010"
              ,"Post 2010"
              ,"Total")
query23.table <- query23.table %>% mutate(Home.Vintage = factor(Home.Vintage, levels = rowOrder)) %>% arrange(Home.Vintage)  
query23.table <- data.frame(query23.table)

query23.table.SF.unw <- query23.table[which(query23.table$BuildingType == "Single Family")
                                  ,-which(colnames(query23.table) %in% c("BuildingType"))]
query23.table.MH.unw <- query23.table[which(query23.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query23.table) %in% c("BuildingType"))]
query23.table.MF.unw <- query23.table[which(query23.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query23.table) %in% c("BuildingType"))]







#############################################################################################
# Query 20
#############################################################################################
query24.dat2 <- query23.dat2
unique(query24.dat2$Home.Vintage)
unique(query24.dat2$Detailed.Region)
query24.dat2$Region.Cat <- "NA"
query24.dat2$Region.Cat[which(query24.dat2$Detailed.Region == "Puget Sound")] <- "PS WA"
query24.dat2$Region.Cat[which(query24.dat2$Detailed.Region == "Western Washington")] <- "W WA"
query24.dat2$Region.Cat[which(query24.dat2$Detailed.Region == "Eastern Washington")] <- "E WA"
query24.dat2$Region.Cat[which(query24.dat2$Detailed.Region == "Idaho")] <- "ID"
query24.dat2$Region.Cat[which(query24.dat2$Detailed.Region == "Western Montana")] <- "W MT"
query24.dat2$Region.Cat[which(query24.dat2$Detailed.Region == "Western Oregon")] <- "W OR"
query24.dat2$Region.Cat[which(query24.dat2$Detailed.Region == "Eastern Oregon")] <- "E OR"
unique(query24.dat2$Region.Cat)
################################################
# Adding pop and sample sizes for weights
################################################
query24.data <- weightedData(query24.dat2[-which(colnames(query24.dat2) %in% c("Year.Built"
                                                                               ,"Home.Vintage"
                                                                               ,"Region.Cat"
                                                                               ,"Home.Vintage"
                                                                               ,"Home.Size"))])
query24.data <- left_join(query24.data, query24.dat2[which(colnames(query24.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Year.Built"
                                                                                         ,"Home.Vintage"
                                                                                         ,"Region.Cat"
                                                                                         ,"Home.Vintage"
                                                                                         ,"Home.Size"))])
query24.data$count <- 1
#######################
# Weighted Analysis
#######################
query24.summary <- proportionRowsAndColumns1(CustomerLevelData = query24.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "Region.Cat"
                                             ,rowVariable = "Home.Vintage"
                                             ,aggregateColumnName = "All Regions")

query24.cast <- dcast(setDT(query24.summary)
                      ,formula = BuildingType + Home.Vintage ~ Region.Cat
                      ,value.var = c("w.percent","w.SE","n","EB"))

query24.table <- data.frame("BuildingType" = query24.cast$BuildingType
                            ,"Home.Vintage" = query24.cast$Home.Vintage
                            ,"Percent.ID" = query24.cast$w.percent_ID
                            ,"SE.ID" = query24.cast$w.SE_ID
                            ,"n.ID" = query24.cast$n_ID
                            ,"Percent.W.MT" = query24.cast$`w.percent_W MT`
                            ,"SE.W.MT" = query24.cast$`w.SE_W MT`
                            ,"n.W.MT" = query24.cast$`n_W MT`
                            ,"Percent.E.OR" = query24.cast$`w.percent_E OR`
                            ,"SE.E.OR" = query24.cast$`w.SE_E OR`
                            ,"n.E.OR" = query24.cast$`n_E OR`
                            ,"Percent.W.OR" = query24.cast$`w.percent_W OR`
                            ,"SE.W.OR" = query24.cast$`w.SE_W OR`
                            ,"n.W.OR" = query24.cast$`n_W OR`
                            ,"Percent.E.WA" = query24.cast$`w.percent_E WA`
                            ,"SE.E.WA" = query24.cast$`w.SE_E WA`
                            ,"n.E.WA" = query24.cast$`n_E WA`
                            ,"Percent.W.WA" = query24.cast$`w.percent_W WA`
                            ,"SE.W.WA" = query24.cast$`w.SE_W WA`
                            ,"n.W.WA" = query24.cast$`n_W WA`
                            ,"Percent.PS.WA" = query24.cast$`w.percent_PS WA`
                            ,"SE.PS.WA" = query24.cast$`w.SE_PS WA`
                            ,"n.PS.WA" = query24.cast$`n_PS WA`
                            ,"Percent.All.Regions" = query24.cast$`w.percent_All Regions`
                            ,"SE.All.Regions" = query24.cast$`w.SE_All Regions`
                            ,"n.All.Regions" = query24.cast$`n_All Regions`
                            ,"EB.ID" = query24.cast$EB_ID
                            ,"EB.W.MT" = query24.cast$`EB_W MT`
                            ,"EB.E.OR" = query24.cast$`EB_E OR`
                            ,"EB.W.OR" = query24.cast$`EB_W OR`
                            ,"EB.E.WA" = query24.cast$`EB_E WA`
                            ,"EB.W.WA" = query24.cast$`EB_W WA`
                            ,"EB.PS.WA" = query24.cast$`EB_PS WA`
                            ,"EB.All.Regions" = query24.cast$`EB_All Regions`
)
levels(query24.table$Home.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1955"
              ,"1956-1960"
              ,"1961-1965"
              ,"1966-1970"
              ,"1971-1975"
              ,"1976-1980"
              ,"1981-1985"
              ,"1986-1990"
              ,"1991-1995"
              ,"1996-2000"
              ,"2001-2005"
              ,"2006-2010"
              ,"Post 2010"
              ,"Total")
query24.table <- query24.table %>% mutate(Home.Vintage = factor(Home.Vintage, levels = rowOrder)) %>% arrange(Home.Vintage)  
query24.table <- data.frame(query24.table)

query24.table.SF <- query24.table[which(query24.table$BuildingType == "Single Family")
                                  ,-which(colnames(query24.table) %in% c("BuildingType"))]
query24.table.MH <- query24.table[which(query24.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query24.table) %in% c("BuildingType"))]
query24.table.MF <- query24.table[which(query24.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query24.table) %in% c("BuildingType"))]
#######################
# Unweighted Analysis
#######################
query24.summary <- proportions_two_groups_unweighted(CustomerLevelData = query24.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "Region.Cat"
                                                     ,rowVariable = "Home.Vintage"
                                                     ,aggregateColumnName = "All Regions")

query24.cast <- dcast(setDT(query24.summary)
                      ,formula = BuildingType + Home.Vintage ~ Region.Cat
                      ,value.var = c("Percent","SE","n"))

query24.table <- data.frame("BuildingType" = query24.cast$BuildingType
                            ,"Home.Vintage" = query24.cast$Home.Vintage
                            ,"Percent.ID" = query24.cast$Percent_ID
                            ,"SE.ID" = query24.cast$SE_ID
                            ,"n.ID" = query24.cast$n_ID
                            ,"Percent.W.MT" = query24.cast$`Percent_W MT`
                            ,"SE.W.MT" = query24.cast$`SE_W MT`
                            ,"n.W.MT" = query24.cast$`n_W MT`
                            ,"Percent.E.OR" = query24.cast$`Percent_E OR`
                            ,"SE.E.OR" = query24.cast$`SE_E OR`
                            ,"n.E.OR" = query24.cast$`n_E OR`
                            ,"Percent.W.OR" = query24.cast$`Percent_W OR`
                            ,"SE.W.OR" = query24.cast$`SE_W OR`
                            ,"n.W.OR" = query24.cast$`n_W OR`
                            ,"Percent.E.WA" = query24.cast$`Percent_E WA`
                            ,"SE.E.WA" = query24.cast$`SE_E WA`
                            ,"n.E.WA" = query24.cast$`n_E WA`
                            ,"Percent.W.WA" = query24.cast$`Percent_W WA`
                            ,"SE.W.WA" = query24.cast$`SE_W WA`
                            ,"n.W.WA" = query24.cast$`n_W WA`
                            ,"Percent.PS.WA" = query24.cast$`Percent_PS WA`
                            ,"SE.PS.WA" = query24.cast$`SE_PS WA`
                            ,"n.PS.WA" = query24.cast$`n_PS WA`
                            ,"Percent.All.Regions" = query24.cast$`Percent_All Regions`
                            ,"SE.All.Regions" = query24.cast$`SE_All Regions`
                            ,"n.All.Regions" = query24.cast$`n_All Regions`
)
levels(query24.table$Home.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1955"
              ,"1956-1960"
              ,"1961-1965"
              ,"1966-1970"
              ,"1971-1975"
              ,"1976-1980"
              ,"1981-1985"
              ,"1986-1990"
              ,"1991-1995"
              ,"1996-2000"
              ,"2001-2005"
              ,"2006-2010"
              ,"Post 2010"
              ,"Total")
query24.table <- query24.table %>% mutate(Home.Vintage = factor(Home.Vintage, levels = rowOrder)) %>% arrange(Home.Vintage)  
query24.table <- data.frame(query24.table)

query24.table.SF.unw <- query24.table[which(query24.table$BuildingType == "Single Family")
                                  ,-which(colnames(query24.table) %in% c("BuildingType"))]
query24.table.MH.unw <- query24.table[which(query24.table$BuildingType == "Manufactured")
                                  ,-which(colnames(query24.table) %in% c("BuildingType"))]
query24.table.MF.unw <- query24.table[which(query24.table$BuildingType == "Multifamily")
                                  ,-which(colnames(query24.table) %in% c("BuildingType"))]

