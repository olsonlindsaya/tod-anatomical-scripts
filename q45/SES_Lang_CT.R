## Cortical Thickness in Lang Regions / SES ##


rm(list=ls()) ## clear the workspace
dev.off() ## Clear the plots

#Set working directory
setwd("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/")

### CT Data ###
anat_data = read.csv('/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/Lang_CT_SES.csv')

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

### Histograms of SES Variables

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "SES1_ASD_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_ASD$SES1,
      geom="histogram",
      binwidth =  1,  
      main = "Histogram for Neighborhood Advantage: ASD", 
      xlab = "Neighborhood Advantage",  
      fill=I("red"), 
      col=I("black"),
      ylim = c(0,9),
      xlim=c(-5,5))
dev.off()

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "SES1_TD_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_TD$SES1,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Neighborhood Advantage: TD", 
      xlab = "Neighborhood Advantage",  
      fill=I("blue"), 
      col=I("black"),
      ylim = c(0,9),
      xlim=c(-5,5))
dev.off()

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "SES1_INR_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_ASD$INR,
      geom="histogram",
      binwidth =  1,  
      main = "Histogram for Income-to-Needs Ratio: ASD", 
      xlab = "Income:Needs Ratio",  
      fill=I("red"), 
      col=I("black"),
      ylim = c(0,9),
      xlim=c(-1,11))
dev.off()

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "INR_TD_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_TD$INR,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Income-to-Needs Ratio: TD", 
      xlab = "Income:Needs Ratio",  
      fill=I("blue"), 
      col=I("black"),
      ylim = c(0,9),
      xlim=c(-1,11))
dev.off()

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "MEL_ASD_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_ASD$MEL_cat,
      geom="histogram",
      binwidth =  1,  
      main = "Histogram for MEL: ASD", 
      xlab = "Maternal Educational Level",  
      fill=I("red"), 
      col=I("black"),
      ylim = c(0,13),
      xlim=c(0,8))
dev.off()

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "MEL_TD_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_TD$MEL_cat,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for MEL: TD", 
      xlab = "Maternal Educational Level",  
      fill=I("blue"), 
      col=I("black"),
      ylim = c(0,13),
      xlim=c(0,8))
dev.off()


mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "zipIncome_ASD_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_ASD$zipIncome,
      geom="histogram",
      binwidth =  5000,  
      main = "Histogram for Zip-Income: ASD", 
      xlab = "Median Income (Postal Code)",  
      fill=I("red"), 
      col=I("black"),
      #ylim = c(0,13),
      xlim=c(20000,100000))
dev.off()

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "ZipIncome_TD_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_TD$zipIncome,
      geom="histogram",
     binwidth = 5000,  
      main = "Histogram for Zip-Income: TD", 
      xlab = "Median Income (Postal Code)", 
      fill=I("blue"), 
      col=I("black"),
     # ylim = c(0,13),
     xlim=c(20000,100000))
dev.off()

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "SES1_INR_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_ASD$INR,
      geom="histogram",
      binwidth =  1,  
      main = "Histogram for Income-to-Needs Ratio: ASD", 
      xlab = "Income:Needs Ratio",  
      fill=I("red"), 
      col=I("black"),
      ylim = c(0,9),
      xlim=c(-1,11))
dev.off()

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "INR_TD_hist.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
qplot(anat_data_TD$INR,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Income-to-Needs Ratio: TD", 
      xlab = "Income:Needs Ratio",  
      fill=I("blue"), 
      col=I("black"),
      ylim = c(0,9),
      xlim=c(-1,11))
dev.off()


## Recode some Vars
anat_data$dxCoded <- as.factor(anat_data$Dx) # 1 = ASD, 2 = TD
anat_data$dxCoded <-unclass(anat_data$dxCoded)

anat_data$sexCoded <- as.factor(anat_data$SEX) # 1 = Female, 2 = Male
anat_data$sexCoded <-unclass(anat_data$sexCoded)


SES_CT_Corr <- rcorr(as.matrix(anat_data[,c("dxCoded", "sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_thickness", "rh_transversetemporal_thickness", "lh_superiortemporal_thickness", "rh_superiortemporal_thickness", "lh_middletemporal_thickness", "rh_middletemporal_thickness", "lh_parsopercularis_thickness", "rh_parsopercularis_thickness", "lh_parsorbitalis_thickness", "rh_parsorbitalis_thickness", "lh_parstriangularis_thickness", "rh_parstriangularis_thickness", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/corrPlots/CT", "SES_CT_varsCorr.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_CT_Corr$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_CT_Corr$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()


###### ASD ONly


anat_data_ASD <- subset(anat_data, anat_data$Dx == "ASD")

SES_CT_Corr_ASD <- rcorr(as.matrix(anat_data_ASD[,c("sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_thickness", "rh_transversetemporal_thickness", "lh_superiortemporal_thickness", "rh_superiortemporal_thickness", "lh_middletemporal_thickness", "rh_middletemporal_thickness", "lh_parsopercularis_thickness", "rh_parsopercularis_thickness", "lh_parsorbitalis_thickness", "rh_parsorbitalis_thickness", "lh_parstriangularis_thickness", "rh_parstriangularis_thickness", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/corrPlots/CT", "SES_CT_varsCorr_ASD.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_CT_Corr_ASD$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_CT_Corr_ASD$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()


###### TD ONly


anat_data_TD <- subset(anat_data, anat_data$Dx == "TD")

SES_CT_Corr_TD <- rcorr(as.matrix(anat_data_TD[,c("sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_thickness", "rh_transversetemporal_thickness", "lh_superiortemporal_thickness", "rh_superiortemporal_thickness", "lh_middletemporal_thickness", "rh_middletemporal_thickness", "lh_parsopercularis_thickness", "rh_parsopercularis_thickness", "lh_parsorbitalis_thickness", "rh_parsorbitalis_thickness", "lh_parstriangularis_thickness", "rh_parstriangularis_thickness", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/corrPlots/CT", "SES_CT_varsCorr_TD.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_CT_Corr_TD$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_CT_Corr_TD$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()

### Run Reg Models 

n <- 16
outcomes <- anat_data[,c(17:32)]
outcomeVars = names(outcomes)

#coVars <- anat_data[,c(4:6)]
#sesVars <- anat_data[,c(43, 44, 41, 46)]


# run n regressions
my_lms <- lapply(1:n, function(x) lm(outcomes[,x] ~ anat_data$V1.Age + anat_data$zipIncome))
# if you need more info, get full summary call. now you can get whatever, like:
summaries <- lapply(my_lms, summary)

# extract just coefficients
sapply(my_lms, coef)

# ...coefficents with p values:
lapply(summaries, function(x) x$coefficients[, c(1,4)])
# ...or r-squared values
sapply(summaries, function(x) c(r_sq = x$r.squared, 
                                adj_r_sq = x$adj.r.squared))

### SES1 Figure Loop
for(i in 1:16){
  
  
  thisOutcome = outcomeVars[i]
  
  mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/CT",paste("SES1by", outcomeVars[i], ".tiff", sep = ""))
  
  tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
  ggplot(anat_data, aes(x=SES1, y=as.matrix(outcomes[i]), color=Dx, shape=Dx)) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Neighborhood Advantage') +
    theme_classic()
  dev.off()
}



### INR Figure Loop
for(i in 1:16){
  
  
  thisOutcome = outcomeVars[i]
  
  mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/CT",paste("INRby", outcomeVars[i], ".tiff", sep = ""))
  
  tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
  ggplot(anat_data, aes(x=INR, y=as.matrix(outcomes[i]), color=Dx, shape=Dx)) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Income:Needs Ratio') +
    theme_classic()
  dev.off()
}

