###############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Andres Roma, Cadmus Group               
##  Created:          02/27/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

#### This code establishes functions which will simplify the process of implementing
#### the weights


proportion_two_groups <- function(CustomerLevelData, valueVariable, 
                                  groupVariableDenominator, groupVariableNumerator) {
  
  ########################
  # Step 1: State
  ########################
  
  #sample and pop sizes within defined strata - this is to account for the fact that not all categories from each table will be observed in each strata
  item.state1 <- summarise(group_by(data, BuildingType, State, Region, Territory)
                            ,N.h   = unique(N.h)
                            ,n.h   = unique(n.h)
  )
  
  # obtain count and proportion by strata and home type
  item.state2 <- summarise(group_by(data, BuildingType, State, Region, Territory, 
                                    groupVariable2)
                            ,count = sum(valueVariable)
                            ,p.h   = count / unique(n.h)
  )
  
  item.state <- left_join(item.state2 , item.state1, by = c("BuildingType", 
                                                            "State", "Region", 
                                                            "Territory"))
  
  #obtain the total population size for the building type by state combination observed in the sample
  weights.state <- summarise(group_by(item.state, BuildingType, as.name(groupVariableDenominator))
                             ,State.N.h = sum(unique(N.h), na.rm = T)
                             ,State.n.h = sum(unique(n.h)), na.rm = T)
  
  item.state.join <- left_join(item.state, weights.state, by = c("BuildingType",
                                                                   groupVariableDenominator))
  
  
  #summarise by numerator grouping variable
  item.state.weighted <- summarise(group_by(item.state.join, BuildingType, 
                                             as.name(groupVariableDenominator),
                                             as.name(groupVariableNumerator))
                                    ,w.percent = sum(N.h * p.h) / unique(State.N.h)
                                    ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(State.N.h)
                                    ,count     = sum(count)
                                    ,N         = unique(State.N.h)
                                    ,n         = unique(State.n.h))
  
  
  
}