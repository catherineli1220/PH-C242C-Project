################################################################################################
# NAME: Longitudinal Data Analysis Final Project
# AUTHORS: Kevin Chen, Matt Lee, Catherine Li
# DATE STARTED: 11/25/2018  
# PURPOSE: Analysis of the Framingham Teaching Data Set for PH C242C Final Project.
# UPDATES: 11/25/2018: CL added code to import data set
#          
################################################################################################

####### PROGRAM START

# Clear workspace
rm(list=ls())

# Load Packages. Be sure your local computer has these installed before running. No need to re-install each time you run. 
library(ggplot2)
library(geepack) # for modified poisson

# Set Working Directory
#setwd("") # Kevin's directory
#setwd("/Users/matthewlee/Box/School/MS Y1/Fall 2018/PH C242C/PH-C242C-Project/") # Matt's directory
setwd("C:/Users/Catherine/Desktop/PB HLTH C242C/Final Project") # Catherine's directory

########################
# IMPORT & CLEAN FRAMINGHAM DATA
########################

  # Import Framingham teaching dataset
  data <- read.csv("frmgham2.csv")
  

#END