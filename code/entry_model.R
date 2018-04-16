#--------------------------------------------#
# R CODE FOR THE ENTRY MODEL (BERRY, 1992)
# Mai-Anh Dang, Hoang Tran, Xiahua Wang
# M2 EEE - Toulouse School of Economics
# April, 2018
#--------------------------------------------#
#
# This file include:
#
# Simple Homogeneous Firm Model 
# Berry (1992) Entry model with 3 identification strategies
#
# Data
# Provided by Prof. Bontemps, with the socioeconomics from Census
# and the airlines data from DB1B, Department of Transportation
#---------------------------------------------#


## (1) Load Data ####

dat <-read.csv("./data/markets_final.csv",header=T)

#--------------------------------------------#
# Model 1: Simple Number of Firms Model ####
# Bresnahan and Reiss 
#--------------------------------------------#
