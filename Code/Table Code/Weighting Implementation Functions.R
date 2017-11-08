###############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Andres Roma, Cadmus Group               
##  Created:          10/10/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

#################################################################################
#Function: mean_one_group
#Used For: Calculating the mean where means are calculated accross 
#          multiple rows for one grouping variable
#          For example, calculating the average of a measure in each state
#################################################################################
# Test
# CustomerLevelData = item58.data
# valueVariable    = 'Ind'
# byVariable       = 'State'
# aggregateRow     = 'Region'

#weighted function for means with one grouping variable
mean_one_group <- function(CustomerLevelData, valueVariable, 
                                    byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  
  if(byVariable == "State"){
  item.strata <- data.frame(ddply(CustomerLevelData
                                  , c("BuildingType", "State", "Region", "Territory"), summarise
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataMean = mean(get(valueVariable))
                            ,strataSD   = sd(get(valueVariable))
                            ,n          = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
  
  #this else if statement is strictly for appliances data (specifically when the mean is a count, like average number of televisions)
  }else {
    item.strata <- data.frame(ddply(CustomerLevelData
                                    , c("BuildingType", "State", "Region", "Territory", byVariable), summarise
                             ,n_h        = unique(n.h)
                             ,N_h        = unique(N.h)
                             ,fpc        = (1 - n_h / N_h)
                             ,w_h        = n_h / N_h
                             ,strataMean = mean(get(valueVariable))
                             ,strataSD   = sd(get(valueVariable))
                             ,n          = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
  }
  
  ######################################################
  # weighted means and SEs by grouping variables
  ######################################################
  item.group <- data.frame(ddply(item.strata, c("BuildingType", byVariable), summarise
                           ,Mean = sum(N_h * strataMean) / sum(N_h)
                           ,SE   = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,n    = sum(n)), stringsAsFactors = F)
  
  ######################################################
  # weighted means and SEs across grouping variables
  ######################################################
  item.region <- data.frame(ddply(item.strata, "BuildingType", summarise
                            ,byRow         = aggregateRow
                            ,Mean       = sum(N_h * strataMean) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,n          = sum(n)), stringsAsFactors = F)
      #rename columns
      colnames(item.region)[which(colnames(item.region) == 'byRow')] <- byVariable
  
  item.final <- rbind.data.frame(item.group, item.region, stringsAsFactors = F)
  
  return(item.final)
} 


# Test
# CustomerLevelData = item52.data
# valueVariable = 'HSPF' 
# byVariable    = 'EquipVintage_bins'
# aggregateRow  = "All Vintages"

mean_one_group_unweighted <- function(CustomerLevelData, valueVariable, 
                           byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }

    ######################################################
    # means and SEs by grouping variables
    ######################################################
    item.byGroup <- data.frame(ddply(CustomerLevelData, c("BuildingType", byVariable), summarise
                              ,n     = length(unique(CK_Cadmus_ID))
                              ,Mean  = mean(get(valueVariable), na.rm = T)
                              ,SE    = sd(get(valueVariable), na.rm = T) / sqrt(n)), stringsAsFactors = F)
    
    ######################################################
    # means and SEs across grouping variables
    ######################################################
    item.all <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                          ,All   = aggregateRow
                          ,n     = length(unique(CK_Cadmus_ID))
                          ,Mean  = mean(get(valueVariable), na.rm = T)
                          ,SE    = sd(get(valueVariable), na.rm = T) / sqrt(n)), stringsAsFactors = F)
    
    item.all <- data.frame(ConvertColName(item.all,'All',byVariable),stringsAsFactors = F)
    
    item.final <- rbind.data.frame(item.byGroup, item.all, stringsAsFactors = F)
    
    return(item.final)
}






#####################################################################################
# This function works for when there are means by row and by column,
# specify the row variable and the column variable and the variable that
# needs summarizing, then specify whether the row needs an aggregate summary
# and whether the column needs an aggregate summary
#####################################################################################
# Test
# CustomerLevelData = item123.data
# valueVariable    = 'Occupants'
# byVariableRow    = 'Age.Category'
# byVariableColumn = 'State'
# columnAggregate  = "Region"
# rowAggregate     = "All_Ages"


mean_two_groups <- function(CustomerLevelData
                            , valueVariable
                            , byVariableRow
                            , byVariableColumn
                            , columnAggregate = NA
                            , rowAggregate    = NA) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ######################################################
  # Using customer level data, Summarise data up to strata level
  # Treat summarise differently depending if either the column or row varaible is state
  ######################################################
  if (byVariableRow == 'State') {
    item.strata <- ddply(CustomerLevelData
                         , c("BuildingType", "State", "Region", "Territory", byVariableColumn), summarise
                             ,n_h        = unique(n.h)
                             ,N_h        = unique(N.h)
                             ,fpc        = (1 - n_h / N_h)
                             ,w_h        = n_h / N_h
                             ,strataMean = mean(get(valueVariable), na.rm = T)
                             ,strataSD   = sd(get(valueVariable), na.rm = T))
  item.strata$strataSD[which(item.strata$strataSD == "NaN")] <- 0
  } 
  
  if (byVariableColumn == 'State') {
    item.strata <- ddply(CustomerLevelData
                         , c("BuildingType", "State", "Region", "Territory", byVariableRow), summarise
                             ,n_h        = unique(n.h)
                             ,N_h        = unique(N.h)
                             ,fpc        = (1 - n_h / N_h)
                             ,w_h        = n_h / N_h
                             ,strataMean = mean(get(valueVariable), na.rm = T)
                             ,strataSD   = sd(get(valueVariable), na.rm = T)
                             ,n          = length(unique(CK_Cadmus_ID)))
    item.strata$strataSD[which(item.strata$strataSD == "NaN")] <- 0
  }
  
  if (byVariableRow != 'State' & byVariableColumn != 'State') {
    item.strata <- ddply(CustomerLevelData
                         , c("BuildingType", "State", "Region", "Territory", byVariableRow, byVariableColumn), summarise
                             ,n_h        = unique(n.h)
                             ,N_h        = unique(N.h)
                             ,fpc        = (1 - n_h / N_h)
                             ,w_h        = n_h / N_h
                             ,strataMean = mean(get(valueVariable), na.rm = T)
                             ,strataSD   = sd(get(valueVariable), na.rm = T))
    item.strata$strataSD[which(item.strata$strataSD == "NaN")] <- 0
    }

  ######################################################
  # Get sample sizes, means, and SEs by both grouping variables
  ######################################################
  item.group.sample.size <- ddply(CustomerLevelData, c("BuildingType", byVariableRow, byVariableColumn),summarise
                                  ,n = length(unique(CK_Cadmus_ID)))
  
  item.group.all <- ddply(item.strata, c("BuildingType", byVariableRow, byVariableColumn), summarise
                              ,Mean = sum(N_h * strataMean) / sum(N_h)
                              ,SE   = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2, na.rm = T)) / sum(unique(N_h)))
  
  #merge samples sizes onto mean and SE info
  item.group.all <- left_join(item.group.all, item.group.sample.size)

  ######################################################
  # Perform grouping variable level analysis aggregating the row by group
  ######################################################
  if (!is.na(rowAggregate)){
    item.group.rowAgg <- ddply(item.strata, c("BuildingType", byVariableColumn), summarise
                                   ,byRow      = rowAggregate
                                   ,Mean       = sum(N_h * strataMean) / sum(N_h)
                                   ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2, na.rm = T)) / sum(unique(N_h))
                                   ,n          = sum(unique(n)))
        #Rename column byrow
        item.group.rowAgg <- ConvertColName(item.group.rowAgg,
                                            "byRow",
                                            byVariableRow)
    item.group.rowFinal <- rbind.data.frame(item.group.all, item.group.rowAgg, stringsAsFactors = F)
  }
  
  if (!is.na(columnAggregate)) {
    item.group.colAgg1 <- ddply(item.strata, c("BuildingType", byVariableRow), summarise
                                   ,byCol = columnAggregate
                                   ,Mean  = sum(N_h * strataMean) / sum(N_h)
                                   ,SE    = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2, na.rm = T)) / sum(unique(N_h))
                                   ,n     = sum(unique(n)))
        #rename column byCol
        item.group.colAgg1 <- ConvertColName(item.group.colAgg1, "byCol",
                                             byVariableColumn)
        
    if (!is.na(rowAggregate)) {
      item.group.colAgg2 <- ddply(item.strata, "BuildingType", summarise
                                      ,byCol = columnAggregate
                                      ,byRow = rowAggregate
                                      ,Mean  = sum(N_h * strataMean) / sum(N_h)
                                      ,SE    = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                                      ,n     = sum(unique(n)))
          #rename columns byCol and by Row
          item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byRow",
                                               byVariableRow)
          item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byCol",
                                               byVariableColumn)
          
      item.group.colAggFinal <- rbind.data.frame(item.group.colAgg1,
                                                 item.group.colAgg2,
                                                 stringsAsFactors = F)
    } else {
      item.group.colAggFinal <- item.group.colAgg1
    }
  }
  
  ## row bind the data before casting
  #  If column variable is NA, row bind row variable final with aggregate final
  if (nrow(item.group.rowFinal) > 0) {
    dataToCast <- rbind.data.frame(item.group.rowFinal, item.group.colAggFinal,
                                   stringsAsFactors = F)
  } else { 
    #  If column variable is not missing, row bind column and row variables (group) final with aggregate final
    dataToCast <- rbind.data.frame(item.group.all, item.group.colAggFinal,
                                          stringsAsFactors = F)
    }
  
  #Cast data
  CastedData <- dcast(setDT(dataToCast)
                       ,formula = BuildingType + get(byVariableRow) ~ get(byVariableColumn)
                       ,value.var = c("Mean", "SE", "n"))
  CastedData <- ConvertColName(CastedData, 'byVariableRow',
                               byVariableRow)
  return(CastedData)
}



# Test
# CustomerLevelData = item123.data
# valueVariable    = 'Occupants'
# byVariableRow    = 'Age.Category'
# byVariableColumn = 'State'
# columnAggregate  = "Region"
# rowAggregate     = "All_Ages"

mean_two_groups_unweighted <- function(CustomerLevelData
                            , valueVariable
                            , byVariableRow
                            , byVariableColumn
                            , columnAggregate = NA
                            , rowAggregate    = NA) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ######################################################
  # Using customer level data, Summarise data up to strata level
  # Treat summarise differently depending if either the column or row varaible is state
  ######################################################
  if (byVariableRow == 'State') {
    item.strata <- data.frame(ddply(CustomerLevelData
                                    , c("BuildingType", "State", "Region", "Territory", byVariableColumn), summarise
                             ,Mean = mean(get(valueVariable),na.rm = T)
                             ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,n    = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
    item.strata$SE[which(item.strata$SE == "NaN")] <- 0
  } 
  if (byVariableColumn == 'State') {
    item.strata <- data.frame(ddply(CustomerLevelData
                                    , c("BuildingType", "State", "Region", "Territory", byVariableRow), summarise
                             ,Mean = mean(get(valueVariable),na.rm = T)
                             ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,n    = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
    item.strata$SE[which(item.strata$SE == "NaN")] <- 0
  }
  if (byVariableRow != 'State' & byVariableColumn != 'State') {
    item.strata <- data.frame(ddply(CustomerLevelData
                                      , c("BuildingType", "State", "Region", "Territory", byVariableRow, byVariableColumn), summarise
                             ,Mean = mean(get(valueVariable),na.rm = T)
                             ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,n    = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
    item.strata$SE[which(item.strata$SE == "NaN")] <- 0
  }
  
  ######################################################
  # Perform analysis using both by groups
  ######################################################
  item.group.all <- data.frame(ddply(CustomerLevelData, c("BuildingType", byVariableRow, byVariableColumn), summarise
                              ,Mean = mean(get(valueVariable),na.rm = T)
                              ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,n    = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)

  ######################################################
  # Perform analysis aggregating the row by group
  ######################################################
  if (!is.na(rowAggregate)){
    item.group.rowAgg <- data.frame(ddply(CustomerLevelData, c("BuildingType", byVariableColumn), summarise
                                   ,byRow= rowAggregate
                                   ,Mean = mean(get(valueVariable),na.rm = T)
                                   ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                                   ,n    = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
        #rename column
        item.group.rowAgg <- ConvertColName(item.group.rowAgg,"byRow",byVariableRow)
    
    item.group.rowFinal <- rbind.data.frame(item.group.all, item.group.rowAgg, stringsAsFactors = F)
  }
  
  if (!is.na(columnAggregate)) {
    item.group.colAgg1 <- data.frame(ddply(CustomerLevelData, c("BuildingType", byVariableRow), summarise
                                    ,byCol = columnAggregate
                                    ,Mean = mean(get(valueVariable),na.rm = T)
                                    ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                                    ,n    = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
        #rename column
        item.group.colAgg1 <- ConvertColName(item.group.colAgg1, "byCol",byVariableColumn)
    
    if (!is.na(rowAggregate)) {
      item.group.colAgg2 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                      ,byCol = columnAggregate
                                      ,byRow = rowAggregate
                                      ,Mean = mean(get(valueVariable),na.rm = T)
                                      ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                                      ,n    = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
          #rename columns
          item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byRow",byVariableRow)
          item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byCol",byVariableColumn)
      
      item.group.colAggFinal <- rbind.data.frame(item.group.colAgg1,item.group.colAgg2,stringsAsFactors = F)
    } else {
      item.group.colAggFinal <- item.group.colAgg1
      }
  }
  
 
  ## row bind the data before casting
  #  If column variable is NA, row bind row variable final with aggregate final
  if (nrow(item.group.rowFinal) > 0) {
    dataToCast <- rbind.data.frame(item.group.rowFinal, item.group.colAggFinal,
                                   stringsAsFactors = F)
  } else {
    #  If column variable is not missing, row bind column and row variables (group) final with aggregate final
    dataToCast <- rbind.data.frame(item.group.all,item.group.colAggFinal,
                                         stringsAsFactors = F)
    }
  
  #cast data
  CastedData <- dcast(setDT(dataToCast)
                      ,formula = BuildingType + get(byVariableRow) ~ get(byVariableColumn)
                      ,value.var = c("Mean", "SE", "n"))
  CastedData <- ConvertColName(CastedData, 'byVariableRow',
                               byVariableRow)
  return(CastedData)
}

  
  

#################################################################################
#Function: proportions_one_group
#Used For: Calculating the proportion where proportions are calculated accross 
#          multiple rows. 
#          For example, calculating the distribution of a measure for all homes in each state
#################################################################################
# Test
# CustomerLevelData = item33.data
# valueVariable    = 'Quantity'
# groupingVariable = 'State'
# total.name       = "Region"
# columnName       = "Framing.Categories"
# weighted         = TRUE

proportions_one_group <- function(CustomerLevelData
                                      , valueVariable
                                      , groupingVariable
                                      , total.name
                                      , columnName
                                      , weighted  = TRUE
                                      , two.prop.total = NA) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  

  if (weighted == TRUE){
    #sample and pop sizes within defined strata - this is to account for the fact that not all categories from each table will be observed in each strata
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory"), summarise
                                 ,N.h   = unique(N.h)
                                 ,n.h   = unique(n.h)), stringsAsFactors = F)
    
    if(groupingVariable == "State"){
      
      # this if statement is specifically to calculate the denominator for tables
      # where we want to know the percent of stored bulbs by bulb type
      if(valueVariable == "StorageBulbs"){
        # obtain count and proportion by strata and row grouping variable
        StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                     , c("BuildingType", "State", "Region", "Territory"), summarise
                                              ,count       = sum(get(valueVariable))
                                              ,total.count = sum(TotalBulbs)
                                              ,p.h = count / total.count), stringsAsFactors = F)
      }else{
        # obtain count and proportion by strata and row grouping variable
        StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                     , c("BuildingType", "State", "Region", "Territory"), summarise
                                              ,count = sum(get(valueVariable))
                                              ,total.count = length(unique(CK_Cadmus_ID))
                                              ,p.h = count / total.count), stringsAsFactors = F)
      }
      
      #join strata counts with summary of grouping variable within strata
      StrataData <- left_join(StrataPopCounts , StrataGroupedProportions, 
                              by = c("BuildingType", "State", "Region","Territory"))
      
      #obtain the total population size for the building type by state combination observed in the sample
      StrataData_n <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                          ,"State"
                                                                          ,"Region"
                                                                          ,"Territory"
                                                                          ,"N.h"
                                                                          ,"n.h"))])
      columnVarWeights <- data.frame(ddply(StrataData_n, c("BuildingType", groupingVariable),summarise
                                           ,columnVar.N.h = sum(N.h)
                                           ,columnVar.n.h = sum(n.h)), stringsAsFactors = F)
      
      columnTotalWeights <- data.frame(ddply(StrataData, "BuildingType", summarise
                                    ,columnTot.N.h = sum(unique(N.h))
                                    ,columnTot.n.h = sum(unique(n.h))), stringsAsFactors = F)
      
      #join strata data with weights by column grouping variable 
      StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType", "State"))
      StrataDataWeights <- left_join(StrataDataWeights, columnTotalWeights, by = "BuildingType")
      
      
      #summarise by column variable
      #summary of both grouping variables
      ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                                     , c("BuildingType", groupingVariable), summarise
                                            ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                            ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnVar.N.h)
                                            ,count     = sum(count)
                                            ,N         = unique(columnVar.N.h)
                                            ,n         = unique(columnVar.n.h)), stringsAsFactors = F)
      
      #summarise across home types (total level)
      ColumnTotals <- data.frame(ddply(StrataDataWeights, "BuildingType", summarise
                                ,rowTotal  = total.name
                                ,w.percent = sum(N.h * p.h) / unique(columnTot.N.h)
                                ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnTot.N.h)
                                ,count     = sum(count)
                                ,N         = unique(columnTot.N.h)
                                ,n         = unique(columnTot.n.h)), stringsAsFactors = F) 
          #rename column
          ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', groupingVariable)
      
      
      
      #join total information onto summary by grouping variables
      AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, ColumnTotals, stringsAsFactors = F)
      
      item.full <- data.frame(AllRowsFinal, stringsAsFactors = F)
      
    
### If grouping variable is NOT state
    }else {
      # obtain count and proportion by strata and row grouping variable
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                            ,count = sum(get(valueVariable))), stringsAsFactors = F)
      
      
      StrataProportion <- data.frame(ddply(StrataGroupedProportions
                                           , c("BuildingType", "State", "Region", "Territory"), summarise
                                      ,total.count = sum(count)), stringsAsFactors = F)
      
      StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
      StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
      
    
   
      #join strata counts with summary of grouping variable within strata
      StrataData <- left_join(StrataPopCounts , StrataGroupedProportions, 
                              by = c("BuildingType", "State", "Region","Territory"))
      
      #obtain the total population size for the building type by state combination observed in the sample
      StrataData_n <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                          ,"State"
                                                                          ,"Region"
                                                                          ,"Territory"
                                                                          ,"N.h"
                                                                          ,"n.h"))])
      columnVarWeights <- data.frame(ddply(StrataData_n, c("BuildingType"),summarise
                                           ,columnVar.N.h = sum(N.h)
                                           ,columnVar.n.h = sum(n.h)), stringsAsFactors = F)
  
    
      #join strata data with weights by column grouping variable 
      StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType"))
      
      
      #summarise by column variable
      #summary of both grouping variables
      ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                                     , c("BuildingType", groupingVariable), summarise
                                            ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                            ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnVar.N.h)
                                            ,count     = sum(count)
                                            ,N         = unique(columnVar.N.h)
                                            ,n         = unique(columnVar.n.h)), stringsAsFactors = F)
      
      #summarise across home types (total level)
      ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup, "BuildingType", summarise
                                ,rowTotal       = "Total"
                                ,w.percent      = sum(w.percent)
                                ,w.SE           = NA
                                ,count          = sum(count, na.rm = T)
                                ,n              = sum(unique(n), na.rm = T)
                                ,N              = sum(unique(N), na.rm = T)), stringsAsFactors = F) 
          #rename column
          ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', groupingVariable)
      
      
      
      #join total information onto summary by grouping variables
      AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, 
                                        ColumnTotals, stringsAsFactors = F)
      
      if(!is.na(two.prop.total)){
        AllRowsFinal$tmp.total <- total.name
        AllRowsFinal <- ConvertColName(AllRowsFinal, 'tmp.total', columnName)
      }
      
      item.full <- data.frame(AllRowsFinal, stringsAsFactors = F)
    
      return(item.full)
      }
  
###### For Unweighted ###########
  } else {
    item.tmp1 <- data.frame(ddply(CustomerLevelData, c("BuildingType", groupingVariable), summarise
                       ,SampleSize = length(unique(CK_Cadmus_ID))
                       ,Count       = sum(get(valueVariable))), stringsAsFactors = F)
    
    
    item.tmp2 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                           ,Total = "Total"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count   = sum(get(valueVariable))), stringsAsFactors = F)
  
        # Convert column name
        item.tmp2 <- ConvertColName(item.tmp2, 'Total', groupingVariable)
    
    item.combined <- rbind.data.frame(item.tmp1, item.tmp2, stringsAsFactors = F)
    
    
    if(valueVariable == "StorageBulbs"){
      item.tmpyy <- data.frame(ddply(CustomerLevelData, c("BuildingType", "State"), summarise
                             ,Total.Count   = sum(TotalBulbs)), stringsAsFactors = F)
      item.tmpxx <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                              ,State = "Total"
                              ,Total.Count = sum(TotalBulbs)), stringsAsFactors = F)
      
      item.tmp3 <- rbind.data.frame(item.tmpyy, item.tmpxx, stringsAsFactors = F)
    }else{
      item.tmp3 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                             ,Total.Count   = sum(get(valueVariable))), stringsAsFactors = F)
    }
    
    
    item.final <- left_join(item.combined, item.tmp3)
    
    if(!is.na(two.prop.total)){
      item.final$tmp.total <- total.name
      item.final <- ConvertColName(item.final, 'tmp.total', columnName)
    }
    
    item.final <- data.frame(item.final, stringsAsFactors = F)
    
    
    if(groupingVariable == "State"){
      
      # this if statement is specifically to calculate the denominator for tables
      # where we want to know the percent of stored bulbs by bulb type
      if(valueVariable == "StorageBulbs"){
        get.unweighted.proportion <- data.frame(ddply(CustomerLevelData
                                                       , c("BuildingType", "State"), summarise
                                              ,Percent = sum(get(valueVariable))/ sum(TotalBulbs)), stringsAsFactors = F)
        
        item.final <- left_join(item.final, get.unweighted.proportion)
        
        #calculate percent
        item.final$Percent[which(is.na(item.final$Percent))] <- item.final$Count[which(is.na(item.final$Percent))] / item.final$Total.Count[which(is.na(item.final$Percent))]
        
      }else{
        item.final$Percent <- item.final$Count / item.final$SampleSize
      }
    }else{
      item.final$Percent <- item.final$Count / item.final$Total.Count
    }
    
    item.final$SE      <- sqrt(item.final$Percent * (1 - item.final$Percent) / item.final$SampleSize)
    return(item.final)
  }
}


#################################################################################
#Function: proportionRowsAndColumns1
#Used For: Calculating the proportion where proportions are calculated accross 
#          multiple columns. For example Table 13. Each column should add up to
#          100%. There is a total row which is 100%. 
#          And there is an overall column, which combines all the columns data
#################################################################################
# TEST
# CustomerLevelData     = item10.data
# valueVariable       = 'count'
# columnVariable      = 'Wall.Type'
# rowVariable         = 'rvalue.bins'
# aggregateColumnName = "All Frame Types"
# # totalRow = TRUE
# weighted = FALSE

proportionRowsAndColumns1 <- function(CustomerLevelData
                                      , valueVariable
                                      , columnVariable
                                      , rowVariable
                                      , aggregateColumnName){
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ########################################################################
  # Obtain correct population and sample size information within strata
  ########################################################################
  # this if statement is because insulation tables
  ############# FIGURE OUT WHY
  
  if (rowVariable == "rvalue.bins"){
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                        , summarise
                                        ,N.h   = unique(N.h)
                                        ,n.h   = unique(n.h)), stringsAsFactors = F)
  }  else {
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory")
                                        , summarise
                                        ,N.h   = unique(N.h)
                                        ,n.h   = unique(n.h)), stringsAsFactors = F)
  }
  
  
  #####################################################################
  # Obtain count, total count, and proportions at the correct levels
  #####################################################################
  # If state is the column variable, we need to perform all analyses without it included as a group_by variable
  # Otherwise it will duplicate the State column
  if (columnVariable == "State") {
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable)
                                                 , summarise
                                                 , count = sum(get(valueVariable)) ), stringsAsFactors = F)
    
    StrataProportion         <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                                 , summarise
                                                 , total.count = sum(count)), stringsAsFactors = F)
    
    StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
    StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
    
    # Analysis for any column variable that is not state should include columnVariable as a grouping variable
  }else{
    #Summarise
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 ,summarise
                                                 ,count = sum(get(valueVariable))), stringsAsFactors = F)
    
    #Summarise
    StrataProportion         <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                                 ,summarise
                                                 ,total.count = sum(count)), stringsAsFactors = F)
    
    #Join Data
    StrataGroupedProportions <- left_join(StrataGroupedProportions, StrataProportion)
    
    #Summarise
    StrataGroupedProportions <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 ,summarise
                                                 ,count = count
                                                 ,p.h = count / total.count), stringsAsFactors = F)
    
  } # END if else statement to get count, total count, and proportion
  
  #join strata counts with summary of grouping variable within strata
  StrataData <- left_join(StrataPopCounts , StrataGroupedProportions)
  
  #################################################################################
  #obtain the total population size for the strata and columnVariable combination
  #################################################################################
  StrataData_n <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                      ,"State"
                                                                      ,"Region"
                                                                      ,"Territory"
                                                                      ,columnVariable
                                                                      ,"N.h"
                                                                      ,"n.h"))])
  columnVarWeights <- data.frame(ddply(StrataData_n, c("BuildingType", columnVariable)
                                       ,summarise
                                       ,columnVar.N.h = sum(N.h)
                                       ,columnVar.n.h = sum(n.h)), stringsAsFactors = F)
  
  #join strata data with weights by column grouping variable 
  StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType", columnVariable))
  
  
  ####################################################################################################
  #calculate weighted percent and weighted standard errors grouping by both column and row variables
  ####################################################################################################
  ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                               , c("BuildingType", columnVariable, rowVariable)
                                               , summarise
                                               ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                               ,w.SE      = sqrt(sum((1 - n.h / N.h) * 
                                                                       (N.h^2 / n.h) * 
                                                                       (p.h * (1 - p.h)))) / unique(columnVar.N.h)
                                               ,count     = sum(count)
                                               ,N         = unique(columnVar.N.h)
                                               ,n         = unique(columnVar.n.h) ), stringsAsFactors = F)
  # calculate column totals
  ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup
                                   , c("BuildingType", columnVariable)
                                   ,summarise
                                   ,rowTotal       = "Total"
                                   ,w.percent      = sum(w.percent)
                                   ,w.SE           = NA
                                   ,count          = sum(count, na.rm = T)
                                   ,n              = sum(unique(n), na.rm = T)
                                   ,N              = sum(unique(N), na.rm = T)), stringsAsFactors = F) 
  #rename column
  ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', rowVariable)
  
  
  #join total information onto summary by grouping variables
  AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, ColumnTotals, stringsAsFactors = F)
  
  
  ########################################################################
  #obtain the total population size for the strata observed in the sample
  ########################################################################
  StrataData_n_agg <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                          ,"State"
                                                                          ,"Region"
                                                                          ,"Territory"
                                                                          ,"N.h"
                                                                          ,"n.h"))])
  AggregateWeight <- ddply(StrataData_n_agg, "BuildingType", summarise
                           ,aggregate.N.h = sum(N.h, na.rm = T)
                           ,aggregate.n.h = sum(n.h, na.rm = T))
  
  #join strata data onto region weights
  item.agg.join <- left_join(StrataData, AggregateWeight, by = c("BuildingType"))
  
  
  #summarise by second grouping variable
  item.agg.weighted <- ddply(item.agg.join, c("BuildingType", rowVariable), summarise
                             ,aggregateName = aggregateColumnName
                             ,w.percent = sum(N.h * p.h) / unique(aggregate.N.h)
                             ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(aggregate.N.h)
                             ,count     = sum(count)
                             ,N         = unique(aggregate.N.h)
                             ,n         = unique(aggregate.n.h))
  #rename column
  colnames(item.agg.weighted)[which(colnames(item.agg.weighted) == 'aggregateName')] <- columnVariable
  
  #summarise at the total level
  item.agg.tot <- ddply(item.agg.weighted, c("BuildingType", columnVariable), summarise
                        ,rowTotal = "Total"
                        ,w.percent = sum(w.percent)
                        ,w.SE      = NA
                        ,count     = sum(count, na.rm = T)
                        ,n         = sum(unique(n), na.rm = T)
                        ,N         = sum(unique(N), na.rm = T))
  #rename column
  colnames(item.agg.tot)[which(colnames(item.agg.tot) == 'rowTotal')]   <- rowVariable
  
  #join table with total info
  item.agg.full <- rbind.data.frame(item.agg.weighted ,item.agg.tot, stringsAsFactors = F)
  
  #reassign
  item.agg.final <- item.agg.full
  
  
  #################################################################
  # Combine calculations made at the column variable level with 
  # calculations made across the column variables
  #################################################################
  item.full <- rbind.data.frame(AllRowsFinal, item.agg.final, stringsAsFactors = F)
  
  return(item.full)
}


# Test
# CustomerLevelData = item10.data
# valueVariable       = 'count'
# columnVariable      = 'Wall.Type'
# rowVariable         = 'rvalue.bins'
# aggregateColumnName = "All Frame Types"

proportions_two_groups_unweighted <- function(CustomerLevelData
                                              , valueVariable
                                              , columnVariable
                                              , rowVariable
                                              , aggregateColumnName){
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ########################################################################
  # Obtain correct population and sample size information within strata
  ########################################################################
  
  #count and sample size by building types and both grouping variables
  item.unweighted1 <- data.frame(ddply(CustomerLevelData, c("BuildingType", columnVariable, rowVariable), summarise
                                       ,Count      = sum(get(valueVariable))
                                       ,SampleSize = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
  
  #count and sample size by building type and column variable (total across rows)
  item.unweighted2 <- data.frame(ddply(CustomerLevelData, c("BuildingType", columnVariable), summarise
                                       ,rowTotal = "Total"
                                       ,Count = sum(get(valueVariable))
                                       ,SampleSize = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
  #rename column
  item.unweighted2 <- ConvertColName(item.unweighted2,'rowTotal',rowVariable)
  
  #count and sample size by building type and row variable
  item.unweighted3 <- data.frame(ddply(CustomerLevelData, c("BuildingType", rowVariable), summarise
                                       ,colTotal = aggregateColumnName
                                       ,Count = sum(get(valueVariable))
                                       ,SampleSize = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
  #rename column
  item.unweighted3 <- ConvertColName(item.unweighted3,'colTotal',columnVariable)
  
  #count and sample size by onlyl building types (total across rows and columns)
  item.unweighted4 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                       ,colTotal = aggregateColumnName
                                       ,rowTotal = "Total"
                                       ,Count = sum(get(valueVariable))
                                       ,SampleSize = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
  #rename columns
  item.unweighted4 <- ConvertColName(item.unweighted4,'rowTotal',rowVariable)
  item.unweighted4 <- ConvertColName(item.unweighted4,'colTotal',columnVariable)
  
  #row bind all calculations
  item.combined <- rbind.data.frame(item.unweighted1, item.unweighted2,
                                    item.unweighted3, item.unweighted4, stringsAsFactors = F)
  
  #get information for total rows only and rename then to get the total count (for the proportion)
  item.totals <- rbind.data.frame(item.unweighted2, item.unweighted4, stringsAsFactors = F)
  #subset to only column names needed (i.e. building type, count, and the column variable)
  item.totals <- item.totals[which(colnames(item.totals) %in% c("BuildingType",columnVariable, "Count"))]
  colnames(item.totals) <- c("BuildingType",columnVariable, "Total.Count")
  
  
  #join the total count onto the counts and sample sizes
  item.final         <- left_join(item.combined, item.totals, by = c("BuildingType",columnVariable))
  
  ## Calculate unweighted percent and standard error
  item.final$Percent <- item.final$Count / item.final$Total.Count
  item.final$SE      <- sqrt(item.final$Percent * (1 - item.final$Percent) / item.final$SampleSize)
  
  item.final <- data.frame("BuildingType" = item.final$BuildingType
                           ,"Percent"     = item.final$Percent
                           ,"Count"       = item.final$Count
                           ,"SampleSize"  = item.final$SampleSize, stringsAsFactors = F)
  
  return(item.final)
}
