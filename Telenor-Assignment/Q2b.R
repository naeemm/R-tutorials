################################################################################
# b. Highest number of crashes by operator and Type of aircrafts.
################################################################################

# load the data
rm(list=ls()) # clear all gloabl and local environment variables
setwd("/home/mnaeem/Downloads/tel")  # change it according to your local folder
# dev.off(dev.list()["RStudioGD"]) # clears all graph, requires momentary pause in code execution for refreshing otherwise plotting error
cat("\014") # clear the console
df <- read.csv("3-Airplane_Crashes_Since_1908.txt", header = TRUE, sep = ",") # df hold all dataset

################################################################################
#**************  Muhammad.Naeem@univ-lyon2.fr  *******************
#*********************  Question 2 (b) ***************************
################################################################################

source("funcQ2b.R")

# crashes by Operator
funcQ2b(df$Operator, "OpertorCloud.png", "Operator", 30) 

# crashes by Type of aircraft
funcQ2b(df$Type, "TypeCloud.png", "Type", 30)

#========================  End  =============================

