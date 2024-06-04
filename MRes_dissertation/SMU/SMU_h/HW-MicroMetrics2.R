#############################################################
# Homework: Applied MicroEconometrics B- Enreaching Dataset #
############################################################

#Libraries
library(tidyverse)
library(readr)
library(readxl)
library(data.table)

#Data Loading
Tcase <- read_csv("Desktop/Tcase.csv")
SanteDep <- read_excel("Desktop/SanteDep.xlsx")
MedRevenu <- read_excel("Desktop/MedRevenu.xlsx")

#Dataset Homogenization around metropolitan France

Tcase = Tcase[-c(95:99),-1] #Removing oversee Territories


Data=cbind(Tcase,SanteDep,MedRevenu)
Data=Data[,-c(3,5)]







