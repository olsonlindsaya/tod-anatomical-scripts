## SES Anat Analyses ##

rm(list=ls()) ## clear the workspace
dev.off() ## Clear the plots

### Making a comment for git ###

#Set working directory
setwd("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/tod-anatomical-scripts/")

### LGI Data ###
#anat_data = read.csv('/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/SES_Anat_v2.csv')

### Excluding Q3s ###
anat_data = read.csv('/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/SES_Anat_v2_q45.csv')

#addressesForGeocoding <- read.csv("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/geocodingData/addressesForGeocoding_lo.csv")

library(plyr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(lavaan)
library(MBESS)
library(magrittr)
library(dplyr)
library(ppcor)
library(data.table)
library(stats)
library(corrplot)
library(Hmisc)
library(rmarkdown)
library(knitr)


#PCA SES
comcon <- addressesForGeocoding[, c("inc",  "povpct", "mdgrent", "mdhval", "pubasspct", "unemp", "bapct")]
comcon.PCA = prcomp(comcon, scale. = TRUE)
#comcon.PCA$eig
#comcon.PCA$var$coord
comcon.loadings = as.data.frame(comcon.PCA$rotation)
comcon.scores = as.data.frame(comcon.PCA$x)
comcon.eig = as.data.frame(comcon.PCA$sdev)
addressesForGeocoding$SES1 <- comcon.scores$PC1
addressesForGeocoding$SES2 <- comcon.scores$PC2

write.csv(addressesForGeocoding, "/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/geocodingData/addressesForGeocoding_lo2.csv", row.names = FALSE)

## Recode some Vars
anat_data$dxCoded <- as.factor(anat_data$Dx) # 1 = ASD, 2 = TD
anat_data$dxCoded <-unclass(anat_data$dxCoded)
 
anat_data$sexCoded <- as.factor(anat_data$SEX) # 1 = Female, 2 = Male
anat_data$sexCoded <-unclass(anat_data$sexCoded)

#### Correlation Plot with SES Vars and LGI in Lang Regions


SES_Lang_LGI <- rcorr(as.matrix(anat_data[,c("dxCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_lgi", "rh_transversetemporal_lgi", "lh_superiortemporal_lgi", "rh_superiortemporal_lgi", "lh_middletemporal_lgi", "rh_middletemporal_lgi", "lh_parsopercularis_lgi", "rh_parsopercularis_lgi", "lh_parsorbitalis_lgi", "rh_parsorbitalis_lgi", "lh_parstriangularis_lgi", "rh_parstriangularis_lgi", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/corrPlots/LGI", "SES_LGI_varsCorr_q45.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_Lang_LGI$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_Lang_LGI$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()


### Make partial correlation plot, partialling out TBV 
# partial correlation between "hl" and "disp" given "deg" and "BC"
# pcor.test(y.data$hl,y.data$disp,y.data[,c("deg","BC")])

DxCoded <-as.factor(anat_data$Dx)
DxCoded <- as.numeric(DxCoded)




##### ASD Only #######

anat_data_ASD <- subset(anat_data, anat_data$Dx == "ASD")

options(digits = 2)
SES_corr_ASD <- rcorr(as.matrix(anat_data_ASD[,c("MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_lgi", "rh_transversetemporal_lgi", "lh_superiortemporal_lgi", "rh_superiortemporal_lgi", "lh_middletemporal_lgi", "rh_middletemporal_lgi", "lh_parsopercularis_lgi", "rh_parsopercularis_lgi", "lh_parsorbitalis_lgi", "rh_parsorbitalis_lgi", "lh_parstriangularis_lgi", "rh_parstriangularis_lgi", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/corrPlots/LGI", "SES_varsCorr_ASD_q45.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_corr_ASD$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_corr_ASD$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()


##### TD Only #######

anat_data_TD <- subset(anat_data, anat_data$Dx == "TD")

SES_corr_TD <- rcorr(as.matrix(anat_data_TD[,c("MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_lgi", "rh_transversetemporal_lgi", "lh_superiortemporal_lgi", "rh_superiortemporal_lgi", "lh_middletemporal_lgi", "rh_middletemporal_lgi", "lh_parsopercularis_lgi", "rh_parsopercularis_lgi", "lh_parsorbitalis_lgi", "rh_parsorbitalis_lgi", "lh_parstriangularis_lgi", "rh_parstriangularis_lgi", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/corrPlots/LGI", "SES_varsCorr_TD_q45.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_corr_TD$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_corr_TD$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()

## LGI / SES Regression Models ##

## Predict total brain volume with SES measures ##

model_TBV <- lm(TBV ~ V1.Age + sexCoded + dxCoded, data=anat_data, na.action = na.exclude)
####### Make Partial Reg Plot ##########
modelTBV_residAgeSex <- lm(TBV ~  V1.Age + sexCoded, data=anat_data, na.action = na.exclude)
TBV_residAgeSex <- resid(modelTBV_residAgeSex)

# INR, given Dx, ZipIncome, MEL
modelSES1_residAgeSex <- lm(SES1 ~  V1.Age + sexCoded, data=anat_data, na.action = na.exclude)
SES1_resid_ageSex <- resid(modelSES1_residAgeSex)

## Partial Reg Plot SES1 by TBV
mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/TBV", "TBV_SES1_partialAgeSex.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=SES1_resid_ageSex, y=TBV_residAgeSex, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('TBV')+
  xlab('Neighborhood Advantage') +
  labs(title="Neighborhood Advantage by Total Brain Volume") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

##  Reg Plot age by TBV
mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/TBV", "TBVbyAge.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=V1.Age, y=TBV, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('TBV')+
  xlab('Age (months)') +
  labs(title="Total Brain Volume by Age (months)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

## Boxplot TBV by Sex

#Total
mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/TBV", "TBVbySex.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=SEX, y=TBV, color=SEX)) + 
  geom_boxplot() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(values=c("purple","blue")) +
  scale_fill_manual(values=c("purple","blue"))
dev.off()


#ASD
mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/TBV", "TBVbySex_ASD.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data_ASD, aes(x=SEX, y=TBV, color=SEX)) + 
  geom_boxplot() +
  theme(text = element_text(size = 20)) +
  coord_cartesian(ylim = c(750000,1800000)) +
  scale_color_manual(values=c("purple","blue")) +
  scale_fill_manual(values=c("purple","blue"))
dev.off()


#TD
mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/TBV", "TBVbySex_TD.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data_TD, aes(x=SEX, y=TBV, color=SEX)) + 
  geom_boxplot() +
  theme(text = element_text(size = 20)) +
  coord_cartesian(ylim = c(750000,1800000)) +
  scale_color_manual(values=c("purple","blue")) +
  scale_fill_manual(values=c("purple","blue")) 
dev.off()


## Boxplot TBV by Dx

#Total
mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/TBV", "TBVbyDx.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=Dx, y=TBV, color=Dx)) + 
  geom_boxplot() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue"))
dev.off()



####################################
## ASD and TD lh Superior Temporal Gyrus LGI
model_lhSupTemp_INR <- lm(lh_superiortemporal_lgi ~  INR  + zipIncome + TBV, data=anat_data, na.action = na.exclude)

model_lhSupTemp_INR <- lm(lh_superiortemporal_lgi ~  INR + SES1 + TBV, data=anat_data, na.action = na.exclude)


####### Make Partial Reg Plot ##########
modelLH_STG_residTBV <- lm(lh_superiortemporal_lgi ~  TBV + zipIncome, data=anat_data, na.action = na.exclude)
LH_STG_residTBV_zipIncome <- resid(modelLH_STG_residTBV)

# INR, given Dx, ZipIncome, MEL
modelINR_residTBV <- lm(INR ~  TBV + zipIncome, data=anat_data, na.action = na.exclude)
INR_resid_TBV_zipIncome <- resid(modelINR_residTBV)

## Partial correlation coefficient

df <- anat_data_ASD
df1 <- df[!is.na(as.numeric(df$INR))&!is.na(df$TBV)&!is.na(df$lh_superiortemporal_lgi),]
pcor.test(df1$INR, df1$lh_superiortemporal_lgi, df1$TBV, method = c("pearson"))


df1 <- df[!is.na(df$SES1)&!is.na(df$TBV)&!is.na(df$lh_superiortemporal_lgi),]
pcor.test(df1$SES1, df1$lh_superiortemporal_lgi, df1$TBV, method = c("pearson"))

cor.test(df$SES1, df$lh_superiortemporal_lgi)

cor.test(df$zipIncome, df$lh_transversetemporal_lgi)
## Partial Reg Plot

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/LGI", "lh_STG_LGI_ASDvTD_partialReg_q45.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=INR_resid_TBV_zipIncome, y=LH_STG_residTBV_zipIncome, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('Left STG LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="Income:Needs by Left STG LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

### ASD Only Partial Reg Plot 

anat_data_ASD <- subset(anat_data, anat_data$Dx == "ASD")

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_STG_LGI_ASD_partialReg.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data_ASD, aes(x=resid2, y=resid1)) +
  geom_point() +
  geom_smooth(method=lm) +
  ylab('Left STG LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="Income:Needs by Left STG LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

ggplot(anat_data, aes(x=INR, y=lh_superiortemporal_lgi, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("blue","red")) +
  scale_fill_manual(values=c("blue","red")) +
  ylab('Left STG LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="Income:Needs by Left STG LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

## ASD Only lh superior temporal LGI

model_lhSupTemp_INR_ASD <- lm(lh_superiortemporal_lgi ~ INR +TBV + V1.Age, data=anat_data_ASD, na.action = na.exclude)

ggplot(anat_data, aes(x=INR, y=lh_superiortemporal_lgi, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("blue","red")) +
  scale_fill_manual(values=c("blue","red")) +
  ylab('Left STG LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="Income:Needs by Left STG LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_STG_LGI.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data_ASD, aes(x=SES1, y=lh_superiortemporal_lgi)) +
  geom_point() +
  geom_smooth(method=lm) +
  ylab('LH STG LGI')+
  xlab('Neighborhood Advantage') +
  labs(title="LH Superior Temporal LGI by Neighborhood Advantage") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

## lh superior temporal LGI
model_lhSupTemp_SES1_ASD <- lm(lh_superiortemporal_lgi ~ SES1 +TBV, data=anat_data_ASD, na.action = na.exclude)


mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_STG_LGI.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data_ASD, aes(x=SES1, y=lh_superiortemporal_lgi)) +
  geom_point() +
  geom_smooth(method=lm) +
  ylab('LH STG LGI')+
  xlab('Neighborhood Advantage') +
  labs(title="LH Superior Temporal LGI by Neighborhood Advantage") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


### TD Only STG LGI


model_lhSupTemp_INR_TD <- lm(lh_superiortemporal_lgi ~ INR +TBV, data=anat_data_TD, na.action = na.exclude)

## Partial Reg Plot ## 
model_INR_resid_TBV_TD <- lm(INR ~ TBV, data=anat_data_TD, na.action = na.exclude)
INR_resid_TBV_TD <- resid(model_INR_resid_TBV)

model_STG_resid_TBV_TD <- lm(lh_superiortemporal_lgi ~ TBV, data=anat_data_TD, na.action = na.exclude)
STG_resid_TBV_TD <- resid(model_STG_resid_TBV_TD)

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_STG_LGI_TD_resid.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data_TD, aes(x=INR_resid_TBV_TD, y=STG_resid_TBV_TD)) +
  geom_point() +
  geom_smooth(method=lm) +
  ylab('Left STG LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="Income:Needs by Left STG LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


####### Make Partial Reg Plot ##########
modelLH_tranTemp_residTBV <- lm(lh_transversetemporal_lgi ~  TBV, data=anat_data, na.action = na.exclude)
resid.3 <- resid(modelLH_tranTemp_residTBV)

anat_data$resid3 <- resid.3
# INR, given Dx, ZipIncome, MEL


model_lhTransversTemp<- lm(lh_transversetemporal_lgi ~ +  INR + TBV + V1.Age + Dx, data=anat_data, na.action = na.exclude)


mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_transverseTemp_LGI_partialReg.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=resid.2, y=resid3, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('LH Transverse Temporal LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="LH Transverse Temporal LGI by Income:Needs Ratio") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


## RH fusiform LGI

model_rh_fusiform <- lm(rh_fusiform_lgi ~ +  INR + TBV, data=anat_data, na.action = na.exclude)
model_lh_fusiform <- lm(lh_fusiform_lgi ~ +  INR + TBV, data=anat_data, na.action = na.exclude)

modelRH_fusiform_residTBV <- lm(rh_fusiform_lgi ~  TBV, data=anat_data, na.action = na.exclude)
resid.4 <- resid(modelRH_fusiform_residTBV)

anat_data$resid4 <- resid.4

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "rh_fusiform_LGI_residTBV.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=resid2, y=resid4, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('RH Fusiform LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="RH Fusiform LGI by Income:Needs Ratio") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

model_lh_temporalpole <- lm(lh_temporalpole_lgi ~ +  MEL_cat + SES2, data=anat_data, na.action = na.exclude) # Null


model_rh_middletemporal <- lm(rh_middletemporal_lgi ~ SES2 + TBV, data=anat_data, na.action = na.exclude)


model_rh_middletemporal_ASD <- lm(rh_middletemporal_lgi ~ SES2 + TBV, data=anat_data_ASD, na.action = na.exclude)


model_rh_middletemporal_TD <- lm(rh_middletemporal_lgi ~ SES2 + TBV, data=anat_data_TD, na.action = na.exclude)

### Pars Obiculars
##  lh superior temporal LGI

model_lhParsOperc <- lm(lh_parsopercularis_lgi ~ dxCoded + zipIncome +TBV, data=anat_data, na.action = na.exclude)

model_lhParsOperc_ASD <- lm(lh_parsopercularis_lgi ~ zipIncome +TBV, data=anat_data_ASD, na.action = na.exclude)

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_parsOperc_LGI.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=INR, y=lh_parsopercularis_lgi, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('Left Pars Opercularis LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="Income:Needs by Left Pars Opercularis LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

### Pars Obiculars
##  lh superior temporal LGI

model_rhParsOperc <- lm(rh_parsopercularis_lgi ~ SES1 + V1.Age +TBV, data=anat_data, na.action = na.exclude)

model_lhParsOperc_ASD <- lm(lh_parsopercularis_lgi ~ zipIncome +TBV, data=anat_data_ASD, na.action = na.exclude)

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_parsOperc_LGI.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=INR, y=lh_parsopercularis_lgi, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('Left Pars Opercularis LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="Income:Needs by Left Pars Opercularis LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


model_lhParsTri <- lm(lh_parstriangularis_lgi ~ zipIncome +TBV, data=anat_data, na.action = na.exclude)


mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_parsTri_LGI.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=INR, y=lh_parstriangularis_lgi, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('Left Pars Triangularis LGI')+
  xlab('Income:Needs Ratio') +
  labs(title="Income:Needs by Left Pars Triangularis LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()



model_lhParsOrbit <- lm(lh_parsorbitalis_lgi ~ zipIncome +TBV, data=anat_data, na.action = na.exclude)


mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_parsOrbit_LGI.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data, aes(x=zipIncome, y=lh_parsorbitalis_lgi, color=Dx, shape=Dx)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Dx)) +
  scale_color_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  ylab('Left Pars Orbitalis LGI')+
  xlab('Median Income (Postal Code)') +
  labs(title="Zip-Income by Left Pars Orbitalis LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

model_lhParsOrbit_ASD <- lm(lh_parsorbitalis_lgi ~ zipIncome +V1.Age, data=anat_data_ASD, na.action = na.exclude)

### Pars Orbit ASD Only
mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_parsOrbit_LGI_ASD.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data_ASD, aes(x=zipIncome, y=lh_parsorbitalis_lgi)) +
  geom_point() +
  geom_smooth(method=lm) +
  ylab('Left Pars Orbitalis LGI')+
  xlab('Median Income (Postal Code)') +
  labs(title="Zip-Income by Left Pars Orbitalis LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()



model_lhParsOrbit_TD <- lm(lh_parsorbitalis_lgi ~ zipIncome + V1.Age, data=anat_data_TD, na.action = na.exclude)

### Pars Orbit TD Only
mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots", "lh_parsOrbit_LGI_TD.tiff")
tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
ggplot(anat_data_TD, aes(x=zipIncome, y=lh_parsorbitalis_lgi)) +
  geom_point() +
  geom_smooth(method=lm) +
  ylab('Left Pars Orbitalis LGI')+
  xlab('Median Income (Postal Code)') +
  labs(title="Zip-Income by Left Pars Orbitalis LGI") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

### Iterate Reg Models 

set.seed(1)

# number of columns in the Lung and Blood data.frames. 22,000 for you?
n <- 16
outcomes <- anat_data[,c(17:32)]
outcomeVars = names(outcomes)

coVars <- anat_data[,c(4:6)]
sesVars <- anat_data[,c(43, 44, 41, 46)]


# run n regressions
my_lms <- lapply(1:n, function(x) lm(outcomes[,x] ~ anat_data$SES2 + anat_data$TBV + anat_data$V1.Age))

# if you need more info, get full summary call. now you can get whatever, like:
summaries <- lapply(my_lms, summary)

# extract just coefficients
sapply(my_lms, coef)

# ...coefficents with p values:
lapply(summaries, function(x) x$coefficients[, c(1,4)])
# ...or r-squared values
sapply(summaries, function(x) c(r_sq = x$r.squared, 
                                adj_r_sq = x$adj.r.squared))
