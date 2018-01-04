#################################################################################
#Function: mean_one_group
#Used For: Calculating the mean where means are calculated accross 
#          multiple rows for one grouping variable
#          For example, calculating the average of a measure in each state
#################################################################################
# Test
# CustomerLevelData = item9.data
# valueVariable = 'Site_Area'
# byVariable    = 'Clean.Type'
# aggregateRow  = "All Room Types"


#weighted function for means with one grouping variable
mean_one_group <- function(CustomerLevelData, valueVariable, 
                           byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  strata_domain_level <- data.frame(ddply(CustomerLevelData
                                , c("BuildingType", "State", "Region", "Territory"), summarise
                                ,n_l        = unique(n.h)
                                ,N_l        = unique(N.h)), stringsAsFactors = F)
  strata_domain_summary    <- data.frame(ddply(CustomerLevelData
                                      , c("BuildingType", "State", "Region", "Territory", byVariable), summarise
                                      ,y_lk     = sum(get(valueVariable), na.rm = T)
                                      ,y_bar_lk = sum(y_ilk, na.rm = T) / sum(m_ilk)
                                      ,n_lk     = length(unique(CK_Cadmus_ID))
                                      ,m_lk     = sum(m_ilk)
                                      ), stringsAsFactors = F)
  strata_domain_merge <- left_join(strata_domain_summary, strata_domain_level)
  site_strata_domain_merge <- left_join(CustomerLevelData, strata_domain_merge)
  
  domain_summary <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType",byVariable), summarise
                                     ,M_hat_k = sum(unique(N_l) / unique(n_l) * sum(m_ilk))
                                     ,y_bar_hat_k  = (1 / M_hat_k) * sum(unique(N_l) / unique(n_l) * sum(y_ilk))
  ), stringsAsFactors = F)
  
  site_strata_domain_merge <- left_join(site_strata_domain_merge, domain_summary)
  
  strata_domain_estimation <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType","State","Region","Territory",byVariable), summarise
                                     ,inner_sum = sum(((y_bar_ilk - y_bar_lk)^2 / (n_lk )) + (n_lk * (1 - n_lk / n_l) * (y_bar_lk - y_bar_hat_k)^2))
  ), stringsAsFactors = F)
  
  site_strata_domain_merge <- left_join(site_strata_domain_merge, strata_domain_estimation)
  
  domain_estimation <- data.frame(ddply(site_strata_domain_merge
                                               , c("BuildingType",byVariable), summarise
                                               # ,inner_sum = sum(((y_bar_ilk - y_bar_lk)^2 / (n_lk )) + (n_lk * (1 - n_lk / n_l) * (y_bar_lk - y_bar_hat_k)^2))
                                               ,outer_sum = sum((N_l^2 / (n_l * (n_l ))) * (1 - n_l / N_l) * ((1 / n_lk) * (1 - n_l / N_l) * sum(inner_sum)))
                                               ,var_hat_y_bar_hat_k = (1 / unique(M_hat_k)^2) * outer_sum
                                               ,EB_y_bat_hat_k = sqrt(var_hat_y_bar_hat_k) * 1.645
  ), stringsAsFactors = F)
  
  
  across_domain_summary <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType"), summarise
                                     ,M_hat_k = sum(N_l / n_l * sum(m_ilk))
                                     ,y_bar_hat_k  = (1 / M_hat_k) * sum(N_l / n_l * sum(y_ilk))
                                     ,var_hat_y_bar_hat_k = (1 / M_hat_k^2) * sum( (N_l^2 / (n_l * (n_l - 1))) * (1 - n_l / N_l) * ((1 / n_lk) * (1 - n_l / N_l) * sum( ((y_bar_ilk - y_bar_lk)^2 / (n_lk - 1)) + (n_lk * (1 - n_lk / n_l) * (y_bar_lk - y_bar_hat_k)^2), na.rm = T) ), na.rm = T)
                                     ,EB_y_bat_hat_k = sqrt(var_hat_y_bar_hat_k) * 1.645
  ), stringsAsFactors = F)

  # ##  Write out confidence/precision info
  # Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
  # write.xlsx(site_strata_domain_merge, paste(filepathCleaningDocs, "Insulation Exports", paste("Domain_Estimation_Mean", rundate, ".xlsx", sep = ""), sep="/"),
  #            append = T, row.names = F, showNA = F)
  
  
  item.final <- rbind.data.frame(domain_summary, across_domain_summary, stringsAsFactors = F)
  item.final <- item.final[which(colnames(item.final) %in% c("BuildingType","Mean","SE","n"))]
  return(item.final)
}

