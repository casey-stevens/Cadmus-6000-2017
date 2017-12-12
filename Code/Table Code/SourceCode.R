#############################################################################################
##  Title:            RBSA Analysis - Source File Paths                     
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################




##  Include packages
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(openxlsx)
library(stringr)
library(data.table)


################################################################################
# SET FILEPATHS for folders and file names of:
# - raw input data
# - clean input data
# - output data
################################################################################
rootpath              <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data"
analysisFolder        <- rootpath
# filepathRawData       <- file.path(analysisFolder,"Data for PSE")
filepathRawData       <- file.path(analysisFolder,"$Clean Data", "2017.10.30")
filepathCleanData     <- file.path(analysisFolder, "Analysis Documents", "Clean Data")
filepathCleaningDocs  <- file.path(analysisFolder, "Analysis Documents")
filepathWeightingDocs <- file.path(analysisFolder, "Analysis Documents", "Weight Source")
outputFolder          <- file.path(analysisFolder, "Tables from Previous RBSA Report")

stopifnot(all(file.exists(rootpath, analysisFolder, filepathRawData, filepathCleanData, filepathCleaningDocs, outputFolder)))

#File Names
appliances.export          <- "Appliances.xlsx"
buildings.interview.export <- "Buildings Interview.xlsx"
buildings.export           <- "Buildings.xlsx"
envelope.export            <- "Envelope_OLD.xlsx"
lighting.export            <- "Lighting.xlsx"
mechanical.export          <- "Mechanical.xlsx"
rooms.export               <- "Rooms.xlsx"
sites.interview.export     <- "Sites Interview.xlsx"
sites.export               <- "Sites.xlsx"
water.export               <- "Water.xlsx"
windows.export             <- "Windows.xlsx"
survey.export              <- "Participant Survey.xlsx"
one.line.export            <- "One Line Summary.xlsm"
one.line.bldg.export       <- "One Line Summary - BLDG.xlsm"
