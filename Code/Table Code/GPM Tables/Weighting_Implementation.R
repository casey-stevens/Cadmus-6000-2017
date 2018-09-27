#################################################################################
#Function: proportion_table_two_groups
#Used For: 
#
# 
#
#################################################################################
proportion_table_two_groups <- function(CustomerLevelData
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
  StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory")
                                      , summarise
                                      ,N.h   = sum(unique(N.h))
                                      ,n.h   = unique(n.h)), stringsAsFactors = F)
  
  #####################################################################
  # Obtain count, total count, and proportions at the correct levels
  #####################################################################
  StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                               , c("BuildingType", "State", "Region", "Territory", rowVariable)
                                               , summarise
                                               , count = sum(get(valueVariable))
                                               , n_hj = length(unique(CK_Cadmus_ID)) ), stringsAsFactors = F)
  
  StrataProportion         <- data.frame(ddply(StrataGroupedProportions
                                               , c("BuildingType", "State", "Region", "Territory")
                                               , summarise
                                               , total.count = sum(count)), stringsAsFactors = F)
  StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
  StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
  StrataGroupedProportions$p.h[which(StrataGroupedProportions$p.h == "NaN")] <- 0
  
  #join strata counts with summary of grouping variable within strata
  StrataData <- left_join(StrataPopCounts , StrataGroupedProportions)
  
  #################################################################################
  #obtain the total population size for the strata and columnVariable combination
  #################################################################################
  StrataData_n <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                      ,"State"
                                                                      ,"N.h"
                                                                      ,"n.h"))])
  columnVarWeights <- data.frame(ddply(StrataData_n, c("BuildingType", "State")
                                       ,summarise
                                       ,columnVar.N.h = sum(N.h)
                                       ,columnVar.n.h = sum(n.h)), stringsAsFactors = F)
  
  #join strata data with weights by column grouping variable
  StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType", "State"))
  
  ####################################################################################################
  #calculate weighted percent and weighted standard errors grouping by both column and row variables
  ####################################################################################################
  ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                               , c("BuildingType", columnVariable, rowVariable)
                                               , summarise
                                               ,w.percent = sum(N.h * p.h) /  unique(columnVar.N.h)
                                               ,w.SE      = sqrt(sum((1 - n.h / N.h) * 
                                                                       (N.h^2 / n.h) * 
                                                                       (p.h * (1 - p.h)), na.rm = T)) / unique(columnVar.N.h)
                                               ,count     = sum(count)
                                               ,N         = sum(N.h)
                                               ,n         = sum(n_hj)
                                               ,EB        = w.SE * qt(1-(1-0.9)/2, n)
  ), stringsAsFactors = F)
  
  # calculate column totals
  ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup
                                   , c("BuildingType", columnVariable)
                                   ,summarise
                                   ,rowTotal       = "Total"
                                   ,w.percent      = sum(w.percent)
                                   ,w.SE           = NA
                                   ,count          = sum(count, na.rm = T)
                                   ,n              = sum((n), na.rm = T)
                                   ,N              = sum(unique(N), na.rm = T)
                                   ,EB             = NA
  ), stringsAsFactors = F) 
  
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
  AggregateWeight <- ddply(StrataData_n_agg, c("BuildingType")
                           ,summarise
                           ,aggregate.N.h = sum(N.h, na.rm = T)
                           ,aggregate.n.h = sum(n.h, na.rm = T))
  
  #join strata data onto region weights
  item.agg.join <- left_join(StrataData, AggregateWeight, by = c("BuildingType"))
  
  item.agg.weighted <- ddply(item.agg.join, c("BuildingType", rowVariable), summarise
                             ,aggregateName = aggregateColumnName
                             ,w.percent = sum(N.h * p.h) / unique(aggregate.N.h)
                             ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(aggregate.N.h)
                             ,count     = sum(count)
                             ,N         = unique(aggregate.N.h)
                             ,n         = sum(n_hj)
                             ,EB   = w.SE * qt(1-(1-0.9)/2, n)
  )
  #rename column
  colnames(item.agg.weighted)[which(colnames(item.agg.weighted) == 'aggregateName')] <- columnVariable
  
  #summarise at the total level
  item.agg.tot <- ddply(item.agg.weighted, c("BuildingType", columnVariable), summarise
                        ,rowTotal = "Total"
                        ,w.percent = sum(w.percent)
                        ,w.SE      = NA
                        ,count     = sum(count, na.rm = T)
                        ,n         = sum((n), na.rm = T)
                        ,N         = sum(unique(N), na.rm = T)
                        ,EB        = NA)
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
  item.full <- item.full[which(colnames(item.full) != "Total.Count")]

  return(item.full)
  
}












#################################################################################
#Function: proportion_table_one_group
#Used For: Calculating the proportion where proportions are calculated accross 
#          multiple rows. 
#          For example, calculating the distribution of a measure for all homes in each state
#################################################################################
proportion_table_one_group <- function(CustomerLevelData
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
  
    #sample and pop sizes within defined strata - this is to account for the fact that not all categories from each table will be observed in each strata
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory"), summarise
                                        ,N.h   = sum(unique(N.h))
                                        ,n.h   = unique(n.h)), stringsAsFactors = F)
    
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                 ,n_hj = length(unique(CK_Cadmus_ID))
                                                 ,count = sum(get(valueVariable))), stringsAsFactors = F)
    
    
    StrataProportion <- data.frame(ddply(StrataGroupedProportions
                                         , c("BuildingType", "State", "Region", "Territory"), summarise
                                         ,total.count = sum(count)), stringsAsFactors = F)
    
    StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
    StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
    StrataGroupedProportions$p.h[which(StrataGroupedProportions$p.h == "NaN")] <- 0
    
    
    StrataData <- left_join(StrataPopCounts , StrataGroupedProportions,
                            by = c("BuildingType", "State", "Region","Territory"))

    
    #####################################################################################################x
    # For "Distribution" tables
    #####################################################################################################x
    #obtain the total population size for the building type by state combination observed in the sample
    StrataData_n <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                        ,"State"
                                                                        ,"Region"
                                                                        ,"Territory"
                                                                        ,"N.h"
                                                                        ,"n.h"))])
    
    columnVarWeights <- data.frame(ddply(StrataData, c("BuildingType"),summarise
                                         ,columnVar.N.h = sum(unique(N.h))
                                         ,columnVar.n.h = sum(n.h)), stringsAsFactors = F)
    
    #join strata data with weights by column grouping variable
    StrataDataWeights <- left_join(StrataData, columnVarWeights, by = "BuildingType")
    
    #####################################################################################################x
    # For "Distribution" tables
    #####################################################################################################x
    #summarise by column variable
    #summary of both grouping variables
    ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                                 , c("BuildingType", groupingVariable), summarise
                                                 ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                                 ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnVar.N.h)
                                                 ,count     = sum(count)
                                                 ,N         = unique(columnVar.N.h)
                                                 ,n         = sum(n_hj)
                                                 ,EB   = w.SE * qt(1-(1-0.9)/2, n-1)), stringsAsFactors = F)
    ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup, "BuildingType", summarise
                                       ,rowTotal  = "Total"
                                       ,w.percent = sum(w.percent)
                                       ,w.SE      = NA
                                       ,count     = sum(count, na.rm = T)
                                       ,N         = sum(unique(N), na.rm = T)
                                       ,EB        = NA), stringsAsFactors = F)
    SampleSizes <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                      ,n         = length(unique(CK_Cadmus_ID))))
    ColumnTotals <- left_join(ColumnTotals, SampleSizes)
    ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', groupingVariable)
    
    
    AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, 
                                      ColumnTotals, stringsAsFactors = F)
    
    item.full <- data.frame(AllRowsFinal, stringsAsFactors = F)
    item.full <- item.full[which(colnames(item.full) != "Total.Count")]
    return(item.full)
}
