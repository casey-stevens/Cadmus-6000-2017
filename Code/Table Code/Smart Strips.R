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



# Read in clean os data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#Read in data for analysis
appliances.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
                             ,stringsAsFactors = FALSE)

#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))




##############################################################
# For Andrew Grant
##############################################################
#For everything else
tableHH.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Smart.Power.Strip"))]

tableHH.dat0 <- tableHH.dat[grep("power",tableHH.dat$Type, ignore.case = T),]
tableHH.dat1 <- tableHH.dat0#[which(tableHH.dat0$Smart.Power.Strip %in% c("Yes", "No")),]

tableHH.dat2 <- left_join(os.dat, tableHH.dat1, by = "CK_Cadmus_ID")

tableHH.dat2$Ind <- 0
tableHH.dat2$Ind[which(!is.na(tableHH.dat2$Type))] <- 1

tableHH.sum <- summarise(group_by(tableHH.dat2, CK_Cadmus_ID, CK_Building_ID)
                         ,Power.Strips = sum(Ind))

tableHH.merge <- left_join(os.dat, tableHH.sum)
################################################
# Adding pop and sample sizes for weights
################################################
tableHH.data <- weightedData(tableHH.merge[-which(colnames(tableHH.merge) %in% c("Power.Strips"))])
tableHH.data <- left_join(tableHH.data, tableHH.merge[which(colnames(tableHH.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Power.Strips"))])
tableHH.data$Count <- 1
tableHH.data$count <- 1
#######################
# Weighted Analysis
#######################
tableHH.final <- mean_one_group(CustomerLevelData = tableHH.data
                                  ,valueVariable = 'Power.Strips'
                                  ,byVariable = "CK_Building_ID"
                                  ,aggregateRow = "Remove")
tableHH.final <- tableHH.final[which(tableHH.final$CK_Building_ID != "Remove"),]
tableHH.final.SF <- tableHH.final[which(tableHH.final$BuildingType == "Single Family")
                                  ,which(names(tableHH.final) %notin% c("BuildingType"))]

#######################
# Weighted Analysis
#######################
tableHH.final.unw <- mean_one_group_unweighted(CustomerLevelData = tableHH.data
                                ,valueVariable = 'Power.Strips'
                                ,byVariable = "CK_Building_ID"
                                ,aggregateRow = "Remove")
tableHH.final.unw <- tableHH.final.unw[which(tableHH.final.unw$CK_Building_ID != "Remove"),]
tableHH.final.unw.SF <- tableHH.final.unw[which(tableHH.final.unw$BuildingType == "Single Family")
                                  ,which(names(tableHH.final.unw) %notin% c("BuildingType"))]



















#For everything else
tableHH.new.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Smart.Power.Strip", "Power.Strip.Use"))]

tableHH.new.dat0 <- tableHH.new.dat[grep("power",tableHH.new.dat$Type, ignore.case = T),]
tableHH.new.dat1 <- tableHH.new.dat0#[which(tableHH.new.dat0$Smart.Power.Strip %in% c("Yes", "No")),]

tableHH.new.dat2 <- left_join(os.dat[which(os.dat$CK_Building_ID == "SCL GenPop"),], tableHH.new.dat1, by = "CK_Cadmus_ID")

tableHH.new.dat2$Ind <- 0
tableHH.new.dat2$Ind[which(!is.na(tableHH.new.dat2$Type))] <- 1

tableHH.new.sum <- summarise(group_by(tableHH.new.dat2, CK_Cadmus_ID, CK_Building_ID, Power.Strip.Use, Smart.Power.Strip)
                         ,Power.Strips = sum(Ind))

tableHH.new.merge <- left_join(os.dat, tableHH.new.sum)
tableHH.new.final <- tableHH.new.merge[which(tableHH.new.merge$CK_Building_ID == "SCL GenPop"),]
names(tableHH.new.final)[which(names(tableHH.new.final) == "CK_Building_ID")] <- "Sample"
#  Write out confidence/precision info
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
write.xlsx(tableHH.new.final, paste(filepathCleaningDocs, "Insulation Exports", paste("Power Strips by Use Type ", rundate, ".xlsx", sep = ""), sep="/"),
           append = T, row.names = F, showNA = F)

tableHH.2.new.sum <- summarise(group_by(tableHH.new.dat2, CK_Cadmus_ID, CK_Building_ID, Smart.Power.Strip)
                             ,Power.Strips = sum(Ind))

tableHH.2.new.merge <- left_join(os.dat, tableHH.2.new.sum)
tableHH.2.new.final <- tableHH.2.new.merge[which(tableHH.2.new.merge$CK_Building_ID == "SCL GenPop"),]
names(tableHH.2.new.final)[which(names(tableHH.2.new.final) == "CK_Building_ID")] <- "Sample"
#  Write out confidence/precision info
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
write.xlsx(tableHH.2.new.final, paste(filepathCleaningDocs, "Insulation Exports", paste("Power Strips ", rundate, ".xlsx", sep = ""), sep="/"),
           append = T, row.names = F, showNA = F)












##############################################################
# For Andrew Grant
##############################################################
#############################################################################################
# Table II: Distribution of power strips by use type and CK_Building_ID
#############################################################################################
#For everything else
colnames(appliances.dat)[grep("power",colnames(appliances.dat), ignore.case = T)]
tableII.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type", "Smart.Power.Strip","Power.Strip.Use"))]

tableII.os.dat0 <- tableII.os.dat[grep("power",tableII.os.dat$Type, ignore.case = T),]

tableII.os.merge <- left_join(os.dat, tableII.os.dat0, by = "CK_Cadmus_ID")

tableII.os.merge <- tableII.os.merge[which((tableII.os.merge$Power.Strip.Use %notin% c("Unknown",NA))),]
tableII.os.merge$Power.Strip.Use <- trimws(tableII.os.merge$Power.Strip.Use)
unique(tableII.os.merge$Power.Strip.Use)

tableII.os.merge$Power.Strip.Use[grep("aquarium|charger", tableII.os.merge$Power.Strip.Use, ignore.case = T)] <- "Other"

tableII.os.merge <- left_join(os.dat, tableII.os.merge)
tableII.os.merge <- tableII.os.merge[which(!is.na(tableII.os.merge$Power.Strip.Use)),]
length(unique(tableII.os.merge$CK_Cadmus_ID[which(tableII.os.merge$CK_Building_ID == "Single Family")]))
################################################
# Adding pop and sample sizes for weights
################################################
tableII.os.data <- weightedData(tableII.os.merge[-which(colnames(tableII.os.merge) %in% c("Type"
                                                                                          ,"Power.Strip.Use"
                                                                                          ,"Smart.Power.Strip"))])
tableII.os.data <- left_join(tableII.os.data, unique(tableII.os.merge[which(colnames(tableII.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                              ,"Type"
                                                                                                              ,"Power.Strip.Use"
                                                                                                              ,"Smart.Power.Strip"))]))
tableII.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableII.os.summary <- proportionRowsAndColumns1(CustomerLevelData = tableII.os.data
                                                ,valueVariable = "Count"
                                                ,columnVariable = "Smart.Power.Strip"
                                                ,rowVariable = "Power.Strip.Use"
                                                ,aggregateColumnName = "Remove")
tableII.os.summary <- tableII.os.summary[which(tableII.os.summary$Smart.Power.Strip != "Remove"),]
tableII.os.table <- data.frame(dcast(setDT(tableII.os.summary)
                         ,formula = BuildingType + Power.Strip.Use ~ Smart.Power.Strip
                         ,value.var = c("w.percent","w.SE","count","n","N","EB")))

tableII.os.table.SF <- tableII.os.table[which(tableII.os.table$BuildingType == "Single Family")
                                        ,which(colnames(tableII.os.table) %notin% c("BuildingType"))]


#######################
# Weighted Analysis
#######################
tableII.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableII.os.data
                                                        ,valueVariable = "Count"
                                                        ,columnVariable = "Smart.Power.Strip"
                                                        ,rowVariable = "Power.Strip.Use"
                                                        ,aggregateColumnName = "Remove")
tableII.os.summary <- tableII.os.summary[which(tableII.os.summary$Smart.Power.Strip != "Remove"),]
tableII.os.table <- data.frame(dcast(setDT(tableII.os.summary)
                                     ,formula = BuildingType + Power.Strip.Use ~ Smart.Power.Strip
                                     ,value.var = c("Percent","SE","n")))

tableII.os.table.SF <- tableII.os.table[which(tableII.os.table$BuildingType == "Single Family")
                                        ,which(colnames(tableII.os.table) %notin% c("BuildingType"))]

