###############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Andres Roma, Cadmus Group               
##  Created:          10/10/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

#################################################################################
#Function: proportionRowsAndColumns1
#Used For: Calculating the proportion where proportions are calculated accross 
#          multiple columns. For example Table 13. Each column should add up to
#          100%. There is a total row which is 100%. 
#          And there is an overall column, which combines all the columns data
#################################################################################

# TEST
# CustomerLevelData <-  item1.dat
# valueVariable <- 'count'
# columnVariable <- 'State'
# rowVariable <- 'HomeType'
# aggregateColumnName = "Region"
  # # totalRow = TRUE
  # weighted = FALSE

proportionRowsAndColumns1 <- function(CustomerLevelData
                                      , valueVariable
                                      , columnVariable
                                      , rowVariable
                                      , aggregateColumnName
                                      # , totalRow = TRUE
                                      # , weighted = TRUE
                                      ){
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }

  ########################
  # Step 1: State
  ########################

    # sample and pop sizes within defined strata - 
    # this is to account for the fact that not all categories from each table will be observed in each strata
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
 
    
    
    if (columnVariable == "State") {
      # obtain count and proportion by strata and row grouping variable
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
      
    } # END else statement
    
    #join strata counts with summary of grouping variable within strata
    StrataData <- left_join(StrataPopCounts , StrataGroupedProportions)
    
    #obtain the total population size for the building type by state combination observed in the sample
    columnVarWeights <- data.frame(ddply(StrataData, c("BuildingType", columnVariable)
                                         ,summarise
                                         ,columnVar.N.h = sum(unique(N.h))
                                         ,columnVar.n.h = sum(unique(n.h))), stringsAsFactors = F)
    
    #join strata data with weights by column grouping variable 
    StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType", columnVariable))
    
    
    #summarise by column variable
    #summary of both grouping variables
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
    
    #summarise across home types (total level)
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
    
    
    
  #################################
  # Step 2: Region (across states)
  #################################
    #obtain the total population size for the building type observed in the sample
    AggregateWeight <- summarise(group_by(StrataData, BuildingType)
                                 ,aggregate.N.h = sum(unique(N.h), na.rm = T)
                                 ,aggregate.n.h = sum(unique(n.h), na.rm = T))
    
    #join strata data onto region weights
    item.region.join <- left_join(StrataData, AggregateWeight, by = c("BuildingType"))
    
    
    #summarise by second grouping variable
    item.region.weighted <- summarise(group_by(item.region.join, BuildingType, 
                                               get(rowVariable))
                                      ,aggregateName = aggregateColumnName
                                      ,w.percent = sum(N.h * p.h) / unique(aggregate.N.h)
                                      ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(aggregate.N.h)
                                      ,count     = sum(count)
                                      ,N         = unique(aggregate.N.h)
                                      ,n         = unique(aggregate.n.h)
    )
    colnames(item.region.weighted)[which(colnames(item.region.weighted) == 'get(rowVariable)')] <- rowVariable
    colnames(item.region.weighted)[which(colnames(item.region.weighted) == 'aggregateName')] <- columnVariable
    
    #summarise at the total level
    item.region.tot <- summarise(group_by(item.region.weighted, BuildingType, get(columnVariable))
                                 ,rowTotal = "Total"
                                 ,w.percent = sum(w.percent)
                                 ,w.SE      = NA
                                 ,count     = sum(count, na.rm = T)
                                 ,n         = sum(unique(n), na.rm = T)
                                 ,N         = sum(unique(N), na.rm = T))
    
    colnames(item.region.tot)[which(colnames(item.region.tot) == "get(columnVariable)")] <- columnVariable
    colnames(item.region.tot)[which(colnames(item.region.tot) == 'rowTotal')]   <- rowVariable

    item.region.full <- rbind.data.frame(item.region.weighted ,item.region.tot, stringsAsFactors = F)
    
    item.region.final <- item.region.full[which(item.region.full$n != 0),]
    
    
  ###########
  # Combine
  ###########
    item.full <- rbind.data.frame(AllRowsFinal, item.region.final, stringsAsFactors = F)
    
    return(item.full)
}


# Test
# CustomerLevelData = item1.dat
# valueVariable     = 'count'
# columnVariable    = 'State'
# rowVariable       = 'HomeType'
# aggregateColumnName = "Region"

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
  

  item.unweighted1 <- summarise(group_by(CustomerLevelData, BuildingType, get(columnVariable), get(rowVariable))
                                ,Count = sum(get(valueVariable))
                                ,SampleSize = length(unique(CK_Cadmus_ID)))
  item.unweighted1 <- ConvertColName(item.unweighted1, 'get(columnVariable)', columnVariable)
  item.unweighted1 <- data.frame(ConvertColName(item.unweighted1, 'get(rowVariable)', rowVariable),stringsAsFactors = F)

  item.unweighted2 <- summarise(group_by(CustomerLevelData, BuildingType, get(columnVariable))
                                ,rowTotal = "Total"
                                ,Count = sum(get(valueVariable))
                                ,SampleSize = length(unique(CK_Cadmus_ID)))
  item.unweighted2 <- ConvertColName(item.unweighted2,'get(columnVariable)',columnVariable)
  item.unweighted2 <- data.frame(ConvertColName(item.unweighted2,'rowTotal',rowVariable),stringsAsFactors = F)

  item.unweighted3 <- summarise(group_by(CustomerLevelData, BuildingType, get(rowVariable))
                                   ,colTotal = aggregateColumnName
                                   ,Count = sum(get(valueVariable))
                                   ,SampleSize = length(unique(CK_Cadmus_ID)))
  item.unweighted3 <- ConvertColName(item.unweighted3,'get(rowVariable)',rowVariable)
  item.unweighted3 <- data.frame(ConvertColName(item.unweighted3,'colTotal',columnVariable),stringsAsFactors = F)

  item.unweighted4 <- summarise(group_by(CustomerLevelData, BuildingType)
                               ,colTotal = aggregateColumnName
                               ,rowTotal = "Total"
                               ,Count = sum(get(valueVariable))
                               ,SampleSize = length(unique(CK_Cadmus_ID)))
  item.unweighted4 <- ConvertColName(item.unweighted4,'rowTotal',rowVariable)
  item.unweighted4 <- data.frame(ConvertColName(item.unweighted4,'colTotal',columnVariable),stringsAsFactors = F)
  item.combined <- rbind.data.frame(item.unweighted1, item.unweighted2,
                                    item.unweighted3, item.unweighted4, stringsAsFactors = F)

  item.totals <- rbind.data.frame(item.unweighted2, item.unweighted4, stringsAsFactors = F)

  item.totals <- item.totals[which(colnames(item.totals) %in% c("BuildingType",columnVariable, "Count"))]
  colnames(item.totals) <- c("BuildingType",columnVariable, "Total.Count")

  item.final         <- left_join(item.combined, item.totals, by = c("BuildingType",columnVariable))
  item.final$Percent <- item.final$Count / item.final$Total.Count
  item.final$SE      <- sqrt(item.final$Percent * (1 - item.final$Percent) / item.final$Denom.SampleSize)
  return(item.final)
}








# Test
# CustomerLevelData = item9.data
# valueVariable = 'Site_Area'
# byVariable    = 'Clean.Type'
# aggregateRow  = 'All Room Types'

mean_one_group <- function(CustomerLevelData, valueVariable, 
                                    byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  
  if(byVariable == "State"){
  item.strata <- summarise(group_by(CustomerLevelData, BuildingType, State, Region, Territory, State)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataMean = mean(get(valueVariable))
                            ,strataSD   = sd(get(valueVariable))
                            ,n          = length(unique(CK_Cadmus_ID))
                            ,count      = sum(count)
  )
  }else{
  item.strata <- summarise(group_by(CustomerLevelData, BuildingType, State, Region, Territory, get(byVariable))
                           ,n_h        = unique(n.h)
                           ,N_h        = unique(N.h)
                           ,fpc        = (1 - n_h / N_h)
                           ,w_h        = n_h / N_h
                           ,strataMean = mean(get(valueVariable))
                           ,strataSD   = sd(get(valueVariable))
                           ,n          = length(unique(CK_Cadmus_ID))
                           ,count      = sum(count)
                           
  )
  
  colnames(item.strata)[which(colnames(item.strata) == 'get(byVariable)')] <- byVariable
  }
  item.strata$strataSD[which(item.strata$strataSD %in% c("NaN", NA))] <- 0
  
  ######################################################
  # Step 2: Using strata level data,
  #   Perform state level analysis
  ######################################################
  item.group <- summarise(group_by(item.strata, BuildingType, get(byVariable))
                           ,Mean       = sum(N_h * strataMean) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(n)
                           ,Count      = sum(count))
  colnames(item.group)[which(colnames(item.group) == 'get(byVariable)')] <- byVariable
  
  ######################################################
  # Step 3: Using strata level data,
  #   Perform region level analysis
  ######################################################
  item.region <- summarise(group_by(item.strata, BuildingType)
                            ,by         = aggregateRow
                            ,Mean       = sum(N_h * strataMean) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(n)
                            ,Count      = sum(count))
  colnames(item.region)[which(colnames(item.region) == 'by')] <- byVariable
  
  item.final <- rbind.data.frame(item.group, item.region, stringsAsFactors = F)
  
  return(item.final)
} 



mean_one_group_unweighted <- function(CustomerLevelData, valueVariable, 
                           byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
    #by state
    item.byGroup <- summarise(group_by(CustomerLevelData, BuildingType, get(byVariable))
                               ,n     = length(unique(CK_Cadmus_ID))
                               ,count = sum(count)
                               ,Mean  = mean(get(valueVariable), na.rm = T)
                               ,SE    = sd(get(valueVariable), na.rm = T) / sqrt(n))
    item.byGroup <- data.frame(ConvertColName(item.byGroup,'get(byVariable)',byVariable),stringsAsFactors = F)
    #by region
    item.all <- summarise(group_by(CustomerLevelData, BuildingType)
                          ,All   = aggregateRow
                          ,n     = length(unique(CK_Cadmus_ID))
                          ,count = sum(count)
                          ,Mean  = mean(get(valueVariable), na.rm = T)
                          ,SE    = sd(get(valueVariable), na.rm = T) / sqrt(n))
    
    item.all <- data.frame(ConvertColName(item.all,'All',byVariable),stringsAsFactors = F)
    
    item.final <- rbind.data.frame(item.byGroup, item.all, stringsAsFactors = F)
    
    return(item.final)
}






#####################################################################################
### This function works for when there are means by row and by column,
### specify the row variable and the column variable and the variable that
### needs summarizing. then specify whether the row needs an aggregate summary
### and whether the column needs an aggregate summary
#####################################################################################



# Test
# CustomerLevelData = item5.customer
# valueVariable     = 'siteAreaConditioned'
# byVariableRow     = 'HomeYearBuilt_bins2'
# byVariableColumn  = 'State'
# columnAggregate   = "Region"
# rowAggregate      = "All Vintages"

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
  # Step 1.1: Using customer level data,
  #   Summarise data up to strata level
  ######################################################
  if (byVariableRow == 'State') {
    item.strata <- summarise(group_by(CustomerLevelData, BuildingType, State, 
                                      Region, Territory, get(byVariableColumn))
                             ,n_h        = unique(n.h)
                             ,N_h        = unique(N.h)
                             ,fpc        = (1 - n_h / N_h)
                             ,w_h        = n_h / N_h
                             ,strataMean = mean(get(valueVariable), na.rm = T)
                             ,strataSD   = sd(get(valueVariable), na.rm = T)
                             ,n          = length(unique(CK_Cadmus_ID))
  )
  
  item.strata$strataSD[which(item.strata$strataSD == "NaN")] <- 0
  item.strata <- ConvertColName(item.strata, "get(byVariableColumn)", byVariableColumn)
  } 
  if (byVariableColumn == 'State') {
    item.strata <- summarise(group_by(CustomerLevelData, BuildingType, State, 
                                      Region, Territory, get(byVariableRow))
                             ,n_h        = unique(n.h)
                             ,N_h        = unique(N.h)
                             ,fpc        = (1 - n_h / N_h)
                             ,w_h        = n_h / N_h
                             ,strataMean = mean(get(valueVariable), na.rm = T)
                             ,strataSD   = sd(get(valueVariable), na.rm = T)
                             ,n          = length(unique(CK_Cadmus_ID))
    )
    
    item.strata$strataSD[which(item.strata$strataSD == "NaN")] <- 0
    item.strata <- ConvertColName(item.strata, "get(byVariableRow)", byVariableRow)
  }
  if (byVariableRow != 'State' & byVariableColumn != 'State') {
    item.strata <- summarise(group_by(CustomerLevelData
                                      , BuildingType, State,Region, Territory, get(byVariableRow),get(byVariableColumn))
                             ,n_h        = unique(n.h)
                             ,N_h        = unique(N.h)
                             ,fpc        = (1 - n_h / N_h)
                             ,w_h        = n_h / N_h
                             ,strataMean = mean(get(valueVariable), na.rm = T)
                             ,strataSD   = sd(get(valueVariable), na.rm = T)
                             ,n          = length(unique(CK_Cadmus_ID))
    )
    
    item.strata$strataSD[which(item.strata$strataSD == "NaN")] <- 0
    item.strata <- ConvertColName(item.strata, "get(byVariableRow)", byVariableRow)
    item.strata <- ConvertColName(item.strata, "get(byVariableColumn)", byVariableColumn)
    }

  ######################################################
  # Step 2: Using strata level data,
  #   Perform state level analysis using both by groups
  ######################################################
  item.group.all <- summarise(group_by(item.strata, BuildingType, get(byVariableRow), get(byVariableColumn))
                              ,Mean       = sum(N_h * strataMean) / sum(N_h)
                              ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2, na.rm = T)) / sum(unique(N_h))
                              ,SampleSize = sum(unique(n)))
  item.group.all <- ConvertColName(item.group.all, 'get(byVariableRow)', byVariableRow)
  item.group.all <- ConvertColName(item.group.all, 'get(byVariableColumn)', byVariableColumn)

  ######################################################
  # Step 3: Using strata level data,
  #   Perform state level analysis aggregating the row by group
  ######################################################
  if (!is.na(rowAggregate)){
    item.group.rowAgg <- summarise(group_by(item.strata, BuildingType, get(byVariableColumn))
                                   ,byRow      = rowAggregate
                                   ,Mean       = sum(N_h * strataMean) / sum(N_h)
                                   ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2, na.rm = T)) / sum(unique(N_h))
                                   ,SampleSize = sum(unique(n)))
    item.group.rowAgg <- ConvertColName(item.group.rowAgg, 
                                        'get(byVariableColumn)', 
                                        byVariableColumn)
    item.group.rowAgg <- ConvertColName(item.group.rowAgg,
                                        "byRow",
                                        byVariableRow)
    item.group.rowFinal <- rbind.data.frame(item.group.all, item.group.rowAgg, stringsAsFactors = F)
  }
  
  if (!is.na(columnAggregate)) {
    item.group.colAgg1 <- summarise(group_by(item.strata, BuildingType, 
                                            get(byVariableRow))
                                   ,byCol = columnAggregate
                                   ,Mean       = sum(N_h * strataMean) / sum(N_h)
                                   ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2, na.rm = T)) / sum(unique(N_h))
                                   ,SampleSize = sum(unique(n)))
    item.group.colAgg1 <- ConvertColName(item.group.colAgg1, "get(byVariableRow)",
                                         byVariableRow)
    item.group.colAgg1 <- ConvertColName(item.group.colAgg1, "byCol",
                                         byVariableColumn)
    if (!is.na(rowAggregate)) {
      item.group.colAgg2 <- summarise(group_by(item.strata, BuildingType)
                                      ,byCol = columnAggregate
                                      ,byRow = rowAggregate
                                      ,Mean       = sum(N_h * strataMean) / sum(N_h)
                                      ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                                      ,SampleSize = sum(unique(n)))
      item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byRow",
                                           byVariableRow)
      item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byCol",
                                           byVariableColumn)
      item.group.colAggFinal <- rbind.data.frame(item.group.colAgg1,
                                                 item.group.colAgg2,
                                                 stringsAsFactors = F)
    } else {item.group.colAggFinal <- item.group.colAgg1}
  }
  if (nrow(item.group.rowFinal) > 0) {
    dataToCast <- rbind.data.frame(item.group.rowFinal, item.group.colAggFinal,
                                   stringsAsFactors = F)
  } else {dataToCast <- rbind.data.frame(item.group.all,item.group.colAggFinal,
                                          stringsAsFactors = F)}
  
  CastedData <- dcast(setDT(dataToCast)
                       ,formula = BuildingType + get(byVariableRow) ~ get(byVariableColumn)
                       ,value.var = c("Mean", "SE", "SampleSize"))
  CastedData <- ConvertColName(CastedData, 'byVariableRow',
                               byVariableRow)
  return(CastedData)
}




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
  # Step 1.1: Using customer level data,
  #   Summarise data up to strata level
  ######################################################
  if (byVariableRow == 'State') {
    item.strata <- summarise(group_by(CustomerLevelData, BuildingType, State, 
                                      Region, Territory, get(byVariableColumn))
                             ,Mean = mean(get(valueVariable),na.rm = T)
                             ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,n    = length(unique(CK_Cadmus_ID))
    )
    
    item.strata$SE[which(item.strata$SE == "NaN")] <- 0
    item.strata <- ConvertColName(item.strata, "get(byVariableColumn)", byVariableColumn)
  } 
  if (byVariableColumn == 'State') {
    item.strata <- summarise(group_by(CustomerLevelData, BuildingType, State, 
                                      Region, Territory, get(byVariableRow))
                             ,Mean = mean(get(valueVariable),na.rm = T)
                             ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,n    = length(unique(CK_Cadmus_ID))
    )
    
    item.strata$SE[which(item.strata$SE == "NaN")] <- 0
    item.strata <- ConvertColName(item.strata, "get(byVariableRow)", byVariableRow)
  }
  if (byVariableRow != 'State' & byVariableColumn != 'State') {
    item.strata <- summarise(group_by(CustomerLevelData
                                      , BuildingType, State,Region, Territory, get(byVariableRow),get(byVariableColumn))
                             ,Mean = mean(get(valueVariable),na.rm = T)
                             ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,n    = length(unique(CK_Cadmus_ID))
    )
    
    item.strata$SE[which(item.strata$SE == "NaN")] <- 0
    item.strata <- ConvertColName(item.strata, "get(byVariableRow)", byVariableRow)
    item.strata <- ConvertColName(item.strata, "get(byVariableColumn)", byVariableColumn)
  }
  
  ######################################################
  # Step 2: Using strata level data,
  #   Perform state level analysis using both by groups
  ######################################################
  item.group.all <- summarise(group_by(CustomerLevelData, BuildingType, get(byVariableRow), get(byVariableColumn))
                              ,Mean       = mean(get(valueVariable),na.rm = T)
                              ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,n    = length(unique(CK_Cadmus_ID)))
  item.group.all <- ConvertColName(item.group.all, 'get(byVariableRow)', byVariableRow)
  item.group.all <- ConvertColName(item.group.all, 'get(byVariableColumn)', byVariableColumn)

  ######################################################
  # Step 3: Using strata level data,
  #   Perform state level analysis aggregating the row by group
  ######################################################
  if (!is.na(rowAggregate)){
    item.group.rowAgg <- summarise(group_by(CustomerLevelData, BuildingType, get(byVariableColumn))
                                   ,byRow      = rowAggregate
                                   ,Mean = mean(get(valueVariable),na.rm = T)
                                   ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                                   ,n    = length(unique(CK_Cadmus_ID)))
    item.group.rowAgg <- ConvertColName(item.group.rowAgg, 
                                        'get(byVariableColumn)', 
                                        byVariableColumn)
    item.group.rowAgg <- ConvertColName(item.group.rowAgg,
                                        "byRow",
                                        byVariableRow)
    item.group.rowFinal <- rbind.data.frame(item.group.all, item.group.rowAgg, stringsAsFactors = F)
  }
  
  if (!is.na(columnAggregate)) {
    item.group.colAgg1 <- summarise(group_by(CustomerLevelData, BuildingType, 
                                             get(byVariableRow))
                                    ,byCol = columnAggregate
                                    ,Mean = mean(get(valueVariable),na.rm = T)
                                    ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                                    ,n    = length(unique(CK_Cadmus_ID)))
    item.group.colAgg1 <- ConvertColName(item.group.colAgg1, "get(byVariableRow)",
                                         byVariableRow)
    item.group.colAgg1 <- ConvertColName(item.group.colAgg1, "byCol",
                                         byVariableColumn)
    if (!is.na(rowAggregate)) {
      item.group.colAgg2 <- summarise(group_by(CustomerLevelData, BuildingType)
                                      ,byCol = columnAggregate
                                      ,byRow = rowAggregate
                                      ,Mean = mean(get(valueVariable),na.rm = T)
                                      ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                                      ,n    = length(unique(CK_Cadmus_ID)))
      item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byRow",
                                           byVariableRow)
      item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byCol",
                                           byVariableColumn)
      item.group.colAggFinal <- rbind.data.frame(item.group.colAgg1,
                                                 item.group.colAgg2,
                                                 stringsAsFactors = F)
    } else {item.group.colAggFinal <- item.group.colAgg1}
  }
  if (nrow(item.group.rowFinal) > 0) {
    dataToCast <- rbind.data.frame(item.group.rowFinal, item.group.colAggFinal,
                                   stringsAsFactors = F)
  } else {dataToCast <- rbind.data.frame(item.group.all,item.group.colAggFinal,
                                         stringsAsFactors = F)}
  
  CastedData <- dcast(setDT(dataToCast)
                      ,formula = BuildingType + get(byVariableRow) ~ get(byVariableColumn)
                      ,value.var = c("Mean", "SE", "n"))
  CastedData <- ConvertColName(CastedData, 'byVariableRow',
                               byVariableRow)
  return(CastedData)
}

  
  


  # #Test
  # CustomerLevelData <- item20.data
  # valueVariable = 'cond.ind'
  # groupingVariable = 'State'
  # total.name = "Region"
  # columnName = ""
  # weighted = TRUE
  
  
proportions_one_group <- function(CustomerLevelData
                                      , valueVariable
                                      , groupingVariable
                                      , total.name
                                      , columnName
                                      , weighted  = TRUE) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  

  ########################
  # Step 1: State
  ########################
  
  if (weighted == TRUE){
  #sample and pop sizes within defined strata - this is to account for the fact that not all categories from each table will be observed in each strata
  StrataPopCounts <- summarise(group_by(CustomerLevelData, BuildingType, State, Region, Territory)
                               ,N.h   = unique(N.h)
                               ,n.h   = unique(n.h))
  if(groupingVariable == "State"){
    # obtain count and proportion by strata and row grouping variable
    StrataGroupedProportions <- summarise(group_by(CustomerLevelData
                                                   , BuildingType
                                                   , State
                                                   , Region
                                                   , Territory)
                                          ,count = sum(get(valueVariable))
                                          ,total.count = length(unique(CK_Cadmus_ID))
                                          ,p.h = count / total.count)
    
    #join strata counts with summary of grouping variable within strata
    StrataData <- left_join(StrataPopCounts , StrataGroupedProportions, 
                            by = c("BuildingType", "State", "Region","Territory"))
    
    #obtain the total population size for the building type by state combination observed in the sample
    columnVarWeights <- summarise(group_by(StrataData
                                           , BuildingType
                                           , get(groupingVariable))
                                  ,columnVar.N.h = sum(unique(N.h))
                                  ,columnVar.n.h = sum(unique(n.h)))
    columnVarWeights <- ConvertColName(columnVarWeights, 'get(groupingVariable)',
                                         groupingVariable)
    columnTotalWeights <- summarise(group_by(StrataData
                                           , BuildingType)
                                  ,columnTot.N.h = sum(unique(N.h))
                                  ,columnTot.n.h = sum(unique(n.h)))
    
    #join strata data with weights by column grouping variable 
    StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType", "State"))
    StrataDataWeights <- left_join(StrataDataWeights, columnTotalWeights, by = "BuildingType")
    
    
    #summarise by column variable
    #summary of both grouping variables
    ColumnProportionsByGroup <- summarise(group_by(StrataDataWeights
                                                   , BuildingType
                                                   , get(groupingVariable))
                                          ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                          ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnVar.N.h)
                                          ,count     = sum(count)
                                          ,N         = unique(columnVar.N.h)
                                          ,n         = unique(columnVar.n.h)
    )  
    
    ColumnProportionsByGroup <- ConvertColName(ColumnProportionsByGroup,'get(groupingVariable)',groupingVariable)
    ColumnProportionsByGroup <- data.frame(ColumnProportionsByGroup, stringsAsFactors = F)
    
    #summarise across home types (total level)
    ColumnTotals <- summarise(group_by(StrataDataWeights
                                       , BuildingType)
                              ,rowTotal  = total.name
                              ,w.percent = sum(N.h * p.h) / unique(columnTot.N.h)
                              ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnTot.N.h)
                              ,count     = sum(count)
                              ,N         = unique(columnTot.N.h)
                              ,n         = unique(columnTot.n.h)
                              ) 
    #rename column
    ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', groupingVariable)
    
    
    
    #join total information onto summary by grouping variables
    AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, 
                                      ColumnTotals, stringsAsFactors = F)
    
    item.full <- data.frame(AllRowsFinal, stringsAsFactors = F)
    
    
  }else {#if(groupingVariable %in% c("rvalue.bins", "rvalue.bins.SF", "rvalue.bins.MH", "Framing.Categories","Heating_Type","Ceiling.Insulation.Thickness.1"))
    # obtain count and proportion by strata and row grouping variable
    StrataGroupedProportions <- summarise(group_by(CustomerLevelData
                                                   , BuildingType
                                                   , State
                                                   , Region
                                                   , Territory
                                                   , get(groupingVariable))
                                          ,count = sum(get(valueVariable))
    )
    StrataGroupedProportions <- ConvertColName(StrataGroupedProportions
                                               ,"get(groupingVariable)"
                                               ,groupingVariable)
    StrataProportion <- summarise(group_by(StrataGroupedProportions
                                           , BuildingType
                                           , State
                                           , Region
                                           , Territory)
                                  ,total.count = sum(count))
    StrataProportion <- data.frame(StrataProportion, stringsAsFactors = F)
    
    StrataGroupedProportions <- left_join(StrataGroupedProportions, StrataProportion)
    StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
    
    
    #fix column name
    StrataGroupedProportions <- ConvertColName(StrataGroupedProportions
                                               ,"get(groupingVariable)"
                                               ,groupingVariable)
  
 
    #join strata counts with summary of grouping variable within strata
    StrataData <- left_join(StrataPopCounts , StrataGroupedProportions, 
                            by = c("BuildingType", "State", "Region","Territory"))
    
    #obtain the total population size for the building type by state combination observed in the sample
    columnVarWeights <- summarise(group_by(StrataData
                                           , BuildingType)
                                  ,columnVar.N.h = sum(unique(N.h))
                                  ,columnVar.n.h = sum(unique(n.h)))
  
    #join strata data with weights by column grouping variable 
    StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType"))
    
    
    #summarise by column variable
    #summary of both grouping variables
    ColumnProportionsByGroup <- summarise(group_by(StrataDataWeights
                                                   , BuildingType
                                                   , get(groupingVariable))
                                          ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                          ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnVar.N.h)
                                          ,count     = sum(count)
                                          ,N         = unique(columnVar.N.h)
                                          ,n         = unique(columnVar.n.h)
    )  
    
    ColumnProportionsByGroup <- ConvertColName(ColumnProportionsByGroup,'get(groupingVariable)',groupingVariable)
    ColumnProportionsByGroup <- data.frame(ColumnProportionsByGroup, stringsAsFactors = F)
    
    #summarise across home types (total level)
    ColumnTotals <- summarise(group_by(ColumnProportionsByGroup, BuildingType)
                              ,rowTotal       = "Total"
                              ,w.percent      = sum(w.percent)
                              ,w.SE           = NA
                              ,count          = sum(count, na.rm = T)
                              ,n              = sum(unique(n), na.rm = T)
                              ,N              = sum(unique(N), na.rm = T)) 
    #rename column
    ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', groupingVariable)
    
    
    
    #join total information onto summary by grouping variables
    AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, 
                                      ColumnTotals, stringsAsFactors = F)
    
    AllRowsFinal$tmp.total <- total.name
    AllRowsFinal <- ConvertColName(AllRowsFinal, 'tmp.total', columnName)
    
    
    item.full <- data.frame(AllRowsFinal, stringsAsFactors = F)
  
    return(item.full)
    }
  
  } else {
    item.tmp1 <- ddply(CustomerLevelData, c("BuildingType", groupingVariable), summarise
                       ,SampleSize = length(unique(CK_Cadmus_ID))
                       ,Count       = sum(get(valueVariable)))
    
    item.tmp2 <- summarise(group_by(CustomerLevelData, BuildingType)
                             ,Total = "Total"
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Count   = sum(get(valueVariable)))
      # Convert column name
      item.tmp2 <- data.frame(ConvertColName(item.tmp2
                                             , 'Total'
                                             , groupingVariable)
                              ,stringsAsFactors = F)
    
    item.combined <- rbind.data.frame(item.tmp1, item.tmp2, stringsAsFactors = F)
    
    item.tmp3 <- summarise(group_by(CustomerLevelData, BuildingType)
                             ,Total.Count   = sum(get(valueVariable)))
    
    item.final <- left_join(item.combined, item.tmp3, by = "BuildingType")
    item.final$tmp.total <- total.name
    item.final <- ConvertColName(item.final, 'tmp.total', columnName)
    
    
    item.final <- data.frame(item.final, stringsAsFactors = F)
    
    if(groupingVariable == "State"){
      item.final$Percent <- item.final$Count / item.final$SampleSize
    }else{
      item.final$Percent <- item.final$Count / item.final$Total.Count
    }
    
    item.final$SE      <- sqrt(item.final$Percent * (1 - item.final$Percent) / item.final$SampleSize)
    return(item.final)
  }
}
