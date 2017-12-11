###############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          10/17/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

#################################################################################
# Function: exportTable
# Used For: Exporting the specific tables to the previous RBSA Tables in excel
#           Allows for exporting to the correct workbooks / tabs
#################################################################################


exportTable <- function(buildingTypeData, buildingTypeIndicator, tableName, weighted = TRUE, weights = NA, final = NA) {
  if (weighted == TRUE){
    if(!is.na(final)){
      Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
      workbook.export <- loadWorkbook(file = paste(outputFolder, paste("Tables in Excel - ",buildingTypeIndicator," - Report Copy.xlsx", sep = ""), sep="/"))
      
      writeData(workbook.export, sheet = tableName, x = buildingTypeData, startRow = 4)
      
      saveWorkbook(workbook.export, file = paste(outputFolder, paste("Tables in Excel - ",buildingTypeIndicator," - Report Copy.xlsx", sep = ""), sep="/"), overwrite = T)
    }else{
      library(openxlsx)
      Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
      workbook.export <- loadWorkbook(file = paste(outputFolder, paste("Tables in Excel - ",buildingTypeIndicator," - COPY_v2.xlsx", sep = ""), sep="/"))
      
      writeData(workbook.export, sheet = tableName, x = buildingTypeData, startRow = 40)
      
      saveWorkbook(workbook.export, file = paste(outputFolder, paste("Tables in Excel - ",buildingTypeIndicator," - COPY_v2.xlsx", sep = ""), sep="/"), overwrite = T)
    }
  }else{
    library(openxlsx)
    Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
    workbook.export <- loadWorkbook(file = paste(outputFolder, paste("Tables in Excel - ",buildingTypeIndicator," - COPY_v2.xlsx", sep = ""), sep="/"))
    
    writeData(workbook.export, sheet = tableName, x = buildingTypeData, startRow = 20)
    
    saveWorkbook(workbook.export, file = paste(outputFolder, paste("Tables in Excel - ",buildingTypeIndicator," - COPY_v2.xlsx", sep = ""), sep="/"), overwrite = T)
  }
  
  
  # if (!is.na(weights)){
  #   Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
  #   
  #   workbook.export <- loadWorkbook(file = paste(outputFolder, paste("Weights for Tables - ",buildingTypeIndicator,".xlsx", sep = ""), sep="/"))
  #   writeData(workbook.export, sheet = tableName, x = buildingTypeData, startRow = 20)
  #   
  # }
}
