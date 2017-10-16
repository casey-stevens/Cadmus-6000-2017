###############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Andres Roma, Cadmus Group               
##  Created:          02/27/2017
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

proportionRowsAndColumns1 <- function(CustomerLevelData
                                      , valueVariable
                                      , columnVariable
                                      , rowVariable
                                      , aggregateColumnName = NA
                                      , totalRow = TRUE) {
  
  #Test
  # CustomerLevelData <- item2.dat
  # valueVariable  <- 'count'
  # columnVariable <- 'State'
  # rowVariable    <- 'HomeYearBuilt_bins2'
  # aggregateColumnName = NA
  # totalRow = TRUE
  
  ########################
  # Step 1: State
  ########################
  
  #sample and pop sizes within defined strata - this is to account for the fact that not all categories from each table will be observed in each strata
  StrataPopCounts <- summarise(group_by(CustomerLevelData, BuildingType, State, Region, Territory)
                               ,N.h   = unique(N.h)
                               ,n.h   = unique(n.h))
  
  # obtain count and proportion by strata and row grouping variable
  StrataGroupedProportions <- summarise(group_by(CustomerLevelData
                                                 , BuildingType
                                                 , State
                                                 , Region
                                                 , Territory
                                                 , get(rowVariable))
                                        ,count = sum(count)
                                        ,p.h   = count / unique(n.h))
  
  StrataGroupedProportions <- ConvertColName(StrataGroupedProportions
                                             ,"get(rowVariable)"
                                             ,rowVariable)
  StrataData <- left_join(StrataPopCounts , StrataGroupedProportions, 
                          by = c("BuildingType", "State", "Region","Territory"))
  
  #obtain the total population size for the building type by state combination observed in the sample
  StateWeights <- summarise(group_by(StrataData
                                     , BuildingType
                                     , get(columnVariable))
                            ,State.N.h = sum(unique(N.h), na.rm = T)
                            ,State.n.h = sum(unique(n.h)), na.rm = T)
  StateWeights <- ConvertColName(StateWeights
                                 ,"get(columnVariable)"
                                 ,columnVariable)
  StrataDataWeights <- left_join(StrataData, StateWeights, by = c("BuildingType",
                                                                columnVariable))
  
  
  #summarise by column variable
  ColumnProportionsByGroup <- summarise(group_by(StrataDataWeights, BuildingType, 
                                                get(columnVariable),
                                                get(rowVariable))
                                      ,w.percent = sum(N.h * p.h) / unique(State.N.h)
                                      ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(State.N.h)
                                      ,count     = sum(count)
                                      ,N         = unique(State.N.h)
                                      ,n         = unique(State.n.h))
  
  ColumnProportionsByGroup <- ConvertColName(ColumnProportionsByGroup,
                                            "get(columnVariable)",
                                            columnVariable)
  ColumnProportionsByGroup <- ConvertColName(ColumnProportionsByGroup,
                                            "get(rowVariable)",
                                            rowVariable)
  
  #summarise across home types (total level)
  if (totalRow) {
  ColumnTotals <- summarise(group_by(StrataDataWeights, BuildingType, State)
                            ,num = "Total"
                            ,w.percent      = sum(w.percent)
                            ,w.SE           = NA
                            ,count          = sum(count, na.rm = T)
                            ,n              = sum(unique(n), na.rm = T)
                            ,N              = sum(unique(N), na.rm = T)) 
  ColumnTotals <- ConvertColName(ColumnTotals, 'num', rowVariable)
  
  AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, 
                                    ColumnTotals, stringsAsFactors = F)
  AllRowsFinal <- AllRowsFinal[which(AllRowsFinal$n != 0),]
  }
  #################################
  # Step 2: Region (across states)
  #################################
  if (length(aggregateColumns) > 0) {
    #obtain the total population size for the building type by state combination observed in the sample
    AggregateWeight <- summarise(group_by(item.state, BuildingType)
                                 ,Region.N.h = sum(unique(N.h), na.rm = T)
                                 ,Region.n.h = sum(unique(n.h), na.rm = T))
    
    item.region.join <- left_join(item.state, AggregateWeight, by = c("BuildingType"))
    
    #summarise by home type
    item.region.weighted <- summarise(group_by(item.region.join, BuildingType, 
                                               get(groupVariableNumerator))
                                       ,denom = "Region"
                                       ,w.percent = sum(N.h * p.h) / unique(Region.N.h)
                                       ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(Region.N.h)
                                       ,count     = sum(count)
                                       ,N         = unique(Region.N.h)
                                       ,n         = unique(Region.n.h)
    )
    colnames(item.region.weighted)[which(colnames(item.region.weighted) == 'get(groupVariableNumerator)')] <- groupVariableNumerator
    colnames(item.region.weighted)[which(colnames(item.region.weighted) == 'denom')] <- groupVariableDenominator
    #summarise across home types (total level)
    item.region.tot <- summarise(group_by(item.region.weighted, BuildingType)
                                  ,denom = "Region"
                                  ,num = "Total"
                                  ,w.percent = sum(w.percent)
                                  ,w.SE      = NA
                                  ,count     = sum(count, na.rm = T)
                                  ,n         = sum(unique(n), na.rm = T)
                                  ,N         = sum(unique(N), na.rm = T))
    
    colnames(item.region.tot)[which(colnames(item.region.tot) == 'denom')] <- groupVariableDenominator
    colnames(item.region.tot)[which(colnames(item.region.tot) == 'num')] <- groupVariableNumerator
    
    item.region.full <- rbind.data.frame(item.region.weighted, 
                                         item.region.tot, stringsAsFactors = F)
    
    # item1.region.final <- item1.region.full[which(!is.na(item1.region.full$BuildingTypeXX)),]
    item.region.final <- item.region.full[which(item.region.full$n != 0),]
  }
  item.full <- rbind.data.frame(item.state.final, 
                                 item.region.final, stringsAsFactors = F)
  return(item.full)
}

mean_one_group <- function(CustomerLevelData, valueVariable, 
                                    byVariable) {
  
  ######################################################
  # Step 1.1: Using customer level data,
  #   Summarise data up to strata level
  ######################################################
  item.strata <- summarise(group_by(CustomerLevelData, BuildingType, State, Region, Territory)
                            ,n_h        = unique(n.h)
                            ,N_h        = unique(N.h)
                            ,fpc        = (1 - n_h / N_h)
                            ,w_h        = n_h / N_h
                            ,strataMean = sum(get(valueVariable)) / n_h
                            ,strataSD   = sd(valueVariable)
                            ,n          = length(unique(CK_Cadmus_ID))
  )
  
  item.strata$strataSD[which(item.strata$strataSD == "NaN")] <- 0
  
  ######################################################
  # Step 2: Using strata level data,
  #   Perform state level analysis
  ######################################################
  item.group <- summarise(group_by(item.strata, BuildingType, get(byVariable))
                           ,Mean       = sum(N_h * strataMean) / sum(N_h)
                           ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                           ,SampleSize = sum(unique(n))
  )
  colnames(item.group)[which(colnames(item.group) == 'get(byVariable)')] <- byVariable
  
  ######################################################
  # Step 3: Using strata level data,
  #   Perform region level analysis
  ######################################################
  item.region <- summarise(group_by(item.strata, BuildingType)
                            ,by      = "Region"
                            ,Mean       = sum(N_h * strataMean) / sum(N_h)
                            ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                            ,SampleSize = sum(unique(n)))
  colnames(item.region)[which(colnames(item.region) == 'by')] <- byVariable
  
  item.final <- rbind.data.frame(item.group, item.region, stringsAsFactors = F)
  
  return(item.final)
}

# test <- mean_one_group(CustomerLevelData = item4.customer,
#                        valueVariable = 'siteAreaConditioned',
#                        byVariable = 'State')








### This function works for when there are means by row and by column,
### specify the row variable and the column variable and the variable that
### needs summarizing. then specify whether the row needs an aggregate summary
### and whether the column needs an aggregate summary

ConvertColName <- function(dataset, currentColName, newColName) {
  data <- dataset
  colnames(data)[which(colnames(data) == currentColName)] <- newColName
  return(data)
}

mean_two_groups <- function(CustomerLevelData, valueVariable, 
                            byVariableRow, byVariableColumn,
                            columnAggregate = NA, rowAggregate = NA) {
  
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
                             ,strataMean = sum(get(valueVariable)) / n_h
                             ,strataSD   = sd(get(valueVariable))
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
                             ,strataMean = sum(get(valueVariable)) / n_h
                             ,strataSD   = sd(get(valueVariable))
                             ,n          = length(unique(CK_Cadmus_ID))
    )
    
    item.strata$strataSD[which(item.strata$strataSD == "NaN")] <- 0
    item.strata <- ConvertColName(item.strata, "get(byVariableRow)", byVariableRow)
  }
  if (byVariableRow != 'State' & byVariableColumn != 'State') {
    item.strata <- summarise(group_by(CustomerLevelData, BuildingType, State, 
                                      Region, Territory, get(byVariableRow),
                                      get(byVariableColumn))
                             ,n_h        = unique(n.h)
                             ,N_h        = unique(N.h)
                             ,fpc        = (1 - n_h / N_h)
                             ,w_h        = n_h / N_h
                             ,strataMean = sum(get(valueVariable)) / n_h
                             ,strataSD   = sd(get(valueVariable))
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
  item.group.all <- summarise(group_by(item.strata, BuildingType, 
                                   get(byVariableRow), get(byVariableColumn))
                          ,Mean       = sum(N_h * strataMean) / sum(N_h)
                          ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                          ,SampleSize = sum(unique(n)))
  item.group.all <- ConvertColName(item.group.all, 'get(byVariableRow)', byVariableRow)
  item.group.all <- ConvertColName(item.group.all, 'get(byVariableColumn)', byVariableColumn)

  ######################################################
  # Step 3: Using strata level data,
  #   Perform state level analysis aggregating the row by group
  ######################################################
  if (!is.na(rowAggregate)){
    item.group.rowAgg <- summarise(group_by(item.strata, BuildingType, 
                                   get(byVariableColumn))
                                   ,byRow = rowAggregate
                                   ,Mean       = sum(N_h * strataMean) / sum(N_h)
                                   ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                                   ,SampleSize = sum(unique(n)))
    colnames(item.group.rowAgg)[which(colnames(item.group.rowAgg) == 'byRow')] <- byVariableRow
    item.group.rowAgg <- ConvertColName(item.group.rowAgg, 
                                        'get(byVariableColumn)', 
                                        byVariableColumn)
    item.group.rowAgg <- ConvertColName(item.group.rowAgg,
                                        "byRow",
                                        byVariableRow)
    item.group.rowFinal <- rbind.data.frame(item.group.all, item.group.rowAgg,
                                               stringsAsFactors = F)
  }
  
  if (!is.na(columnAggregate)) {
    item.group.colAgg1 <- summarise(group_by(item.strata, BuildingType, 
                                            get(byVariableRow))
                                   ,byCol = columnAggregate
                                   ,Mean       = sum(N_h * strataMean) / sum(N_h)
                                   ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
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
  return(CastedData)
}

  
  