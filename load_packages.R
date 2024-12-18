pkg <- c('BIOMASS',"tidyverse","dplyr","corrplot",
         "vegan","stringr", "car","FactoMineR",
         "factoextra", "vioplot","lme4","bbmle")

pkg <- pkg[!pkg%in%installed.packages()]
pkg
install.packages (pkg)

library (BIOMASS)
library(tidyverse)
library(dplyr)
library(stringr)
library (corrplot)
library (vegan)
library(car)
library(FactoMineR)
library(factoextra)
library (vioplot)
library(lme4)
library(bbmle)
