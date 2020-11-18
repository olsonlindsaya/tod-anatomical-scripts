## Surface Area in Lang Regions / SES ##


rm(list=ls()) ## clear the workspace
dev.off() ## Clear the plots

#Set working direSAory
setwd("/Users/lolson/Documents/001_JDP/001_BDIL/006_ProjeSAs/11_ToddlerSES/todanatomicaldata/")

### SA Data ###
anat_data = read.csv('./SA_SES.csv')

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


SES_SA_Corr <- rcorr(as.matrix(anat_data[,c("dxCoded", "sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_area","rh_transversetemporal_area","lh_temporalpole_area", "rh_temporalpole_area","lh_superiortemporal_area", "rh_superiortemporal_area","lh_middletemporal_area", "rh_middletemporal_area","lh_fusiform_area", "rh_fusiform_area", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("./corrPlots", "SES_SA_varsCorr.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_SA_Corr$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_SA_Corr$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()


### Reg Models  

model_lhSupTemp <- lm(lh_superiortemporal_area ~  SES1 + TBV, data=anat_data, na.action= na.exclude) #Nothing sig. when controlling for TBV


mypath <- file.path("./scatterPlots", "lh_STG_SA_ASDvTD.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=SES1, y=lh_superiortemporal_area, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('Left STG Surface Area')+
  xlab('Neighborhood Advantage') +
  labs(title="Neighborhood Advantage by Left STG SA") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


model_lhfusiform_INR <- lm(lh_fusiform_area ~  zipIncome + TBV + sexCoded, data=anat_data, na.action= na.exclude)

mypath <- file.path("./scatterPlots", "lh_fusiform_SA_ASDvTD.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=zipIncome, y=lh_fusiform_area, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('Left Fusiform Surface Area')+
  xlab('Median Income (Postal Code)') +
  labs(title="Median Income (Postal Code) by Left STG SA") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


model_lhMTG_INR <- lm(lh_middletemporal_area ~  dxCoded + sexCoded +TBV, data=anat_data, na.action= na.exclude) #null 

model_lhtemporalpole_INR <- lm(lh_temporalpole_area ~  zipIncome + sexCoded + TBV, data=anat_data, na.action= na.exclude)  #null

model_TBV <- lm(TBV ~  zipIncome + sexCoded, data=anat_data, na.action= na.exclude) 

###### ASD ONly


anat_data_ASD <- subset(anat_data, anat_data$Dx == "ASD")

SES_SA_Corr_ASD <- rcorr(as.matrix(anat_data_ASD[,c("sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_area","rh_transversetemporal_area","lh_temporalpole_area", "rh_temporalpole_area","lh_superiortemporal_area", "rh_superiortemporal_area","lh_middletemporal_area", "rh_middletemporal_area","lh_fusiform_area", "rh_fusiform_area", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("./corrPlots", "SES_SA_varsCorr_ASD.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_SA_Corr_ASD$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_SA_Corr_ASD$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()
