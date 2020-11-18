## Cortical Thickness in Lang Regions / SES ##


rm(list=ls()) ## clear the workspace
dev.off() ## Clear the plots

#Set working directory
setwd("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/")

### CT Data ###
anat_data = read.csv('./Lang_CT_SES.csv')

library(plyr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(lavaan)
#library(psych)
library(MBESS)
library(magrittr)
library(dplyr)
library(ppcor)
library(data.table)
library(stats)
library(corrplot)
library(Hmisc)
library(ppcor)

## Recode some Vars
anat_data$dxCoded <- as.factor(anat_data$Dx) # 1 = ASD, 2 = TD
anat_data$dxCoded <-unclass(anat_data$dxCoded)

anat_data$sexCoded <- as.factor(anat_data$SEX) # 1 = Female, 2 = Male
anat_data$sexCoded <-unclass(anat_data$sexCoded)


SES_CT_Corr <- rcorr(as.matrix(anat_data[,c("dxCoded", "sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_thickness","rh_transversetemporal_thickness","lh_temporalpole_thickness", "rh_temporalpole_thickness","lh_superiortemporal_thickness", "rh_superiortemporal_thickness","lh_middletemporal_thickness", "rh_middletemporal_thickness","lh_fusiform_thickness", "rh_fusiform_thickness", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("./corrPlots", "SES_CT_varsCorr.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_CT_Corr$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_CT_Corr$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()


### Reg Models  

model_lhSupTemp_INR <- lm(lh_superiortemporal_thickness ~  INR + sexCoded, data=anat_data, na.action = na.exclude) # Null

model_lhfusiform_INR <- lm(lh_fusiform_thickness ~  INR + TBV, data=anat_data, na.action = na.exclude) # Null

model_lhSTG_INR <- lm(lh_superiortemporal_thickness ~  INR, data=anat_data, na.action = na.exclude) 

model_lhMTG_INR <- lm(lh_middletemporal_thickness ~  INR + SES1, data=anat_data, na.action = na.exclude) 

model_lhtemporalpole_INR <- lm(lh_temporalpole_thickness ~  INR + TBV, data=anat_data, na.action = na.exclude) 

model_TBV <- lm(TBV ~  zipIncome + sexCoded, data=anat_data, na.action = na.exclude) 

###### ASD ONly


anat_data_ASD <- subset(anat_data, anat_data$Dx == "ASD")

SES_CT_Corr_ASD <- rcorr(as.matrix(anat_data_ASD[,c("sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_thickness","rh_transversetemporal_thickness","lh_temporalpole_thickness", "rh_temporalpole_thickness","lh_superiortemporal_thickness", "rh_superiortemporal_thickness","lh_middletemporal_thickness", "rh_middletemporal_thickness","lh_fusiform_thickness", "rh_fusiform_thickness", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("./corrPlots", "SES_CT_varsCorr_ASD.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_CT_Corr_ASD$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_CT_Corr_ASD$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()
