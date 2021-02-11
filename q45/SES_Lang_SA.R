## Surface Area in Lang Regions / SES ##


rm(list=ls()) ## clear the workspace
dev.off() ## Clear the plots

#Set working direSAory
setwd("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/")

### SA Data ###
anat_data = read.csv('/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/SA_SES.csv')

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


SES_area_Corr <- rcorr(as.matrix(anat_data[,c("dxCoded", "sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_area", "rh_transversetemporal_area", "lh_superiortemporal_area", "rh_superiortemporal_area", "lh_middletemporal_area", "rh_middletemporal_area", "lh_parsopercularis_area", "rh_parsopercularis_area", "lh_parsorbitalis_area", "rh_parsorbitalis_area", "lh_parstriangularis_area", "rh_parstriangularis_area", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/corrPlots/SA", "SES_area_varsCorr.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_area_Corr$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_area_Corr$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()


### Reg Models 

### Option 1 ####

set.seed(1)

# number of columns in the Lung and Blood data.frames. 22,000 for you?
n <- 16
outcomes <- anat_data[,c(17:32)]
outcomeVars = names(outcomes)

coVars <- anat_data[,c(4:6)]
sesVars <- anat_data[,c(43, 44, 41, 46)]


# run n regressions
my_lms <- lapply(1:n, function(x) lm(outcomes[,x] ~ anat_data$zipIncome+ anat_data$TBV + anat_data$V1.Age))

# if you need more info, get full summary call. now you can get whatever, like:
summaries <- lapply(my_lms, summary)

# extract just coefficients
sapply(my_lms, coef)

# ...coefficents with p values:
lapply(summaries, function(x) x$coefficients[, c(1,4)])
# ...or r-squared values
sapply(summaries, function(x) c(r_sq = x$r.squared, 
                                adj_r_sq = x$adj.r.squared))


### SES 1 Loop ####
for(i in 1:16){
  
  
  thisOutcome = outcomeVars[i]
  
  mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/SA",paste("SES1by", outcomeVars[i], ".tiff", sep = ""))
  
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


### SES 2 Loop ####
for(i in 1:16){
  
  
  thisOutcome = outcomeVars[i]
  
  mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/SA",paste("SES2by", outcomeVars[i], ".tiff", sep = ""))
  
  tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
  ggplot(anat_data, aes(x=SES2, y=as.matrix(outcomes[i]), color=Dx, shape=Dx)) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Neighborhood Disadvantage') +
    theme_classic()
  dev.off()
}

### zipIncome Loop ####
for(i in 1:16){
  
  
  thisOutcome = outcomeVars[i]
  
  mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/SA",paste("zipIncomeby", outcomeVars[i], ".tiff", sep = ""))
  
  tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
  ggplot(anat_data, aes(x=zipIncome, y=as.matrix(outcomes[i]), color=Dx, shape=Dx)) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Median Income (Postal Code)') +
    theme_classic()
  dev.off()
}


### INR Loop
for(i in 1:16){
  
  
  thisOutcome = outcomeVars[i]
  
  mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/SA",paste("INRby", outcomeVars[i], ".tiff", sep = ""))
  
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

### MEL_Cat Loop
for(i in 1:16){
  
  
  thisOutcome = outcomeVars[i]
  
  mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/SA",paste("MELby", outcomeVars[i], ".tiff", sep = ""))
  
  tiff(file=mypath,width = 6, height = 6, units = 'in', res = 300)
  ggplot(anat_data, aes(x=MEL_cat, y=as.matrix(outcomes[i]), color=Dx, shape=Dx)) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Maternal Educational Level') +
    theme_classic()
  dev.off()
}

  

###### ASD ONly


anat_data_ASD <- subset(anat_data, anat_data$Dx == "ASD")

SES_area_Corr_ASD <- rcorr(as.matrix(anat_data_ASD[,c("sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_area", "rh_transversetemporal_area", "lh_superiortemporal_area", "rh_superiortemporal_area", "lh_middletemporal_area", "rh_middletemporal_area", "lh_parsopercularis_area", "rh_parsopercularis_area", "lh_parsorbitalis_area", "rh_parsorbitalis_area", "lh_parstriangularis_area", "rh_parstriangularis_area", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/corrPlots/SA", "SES_area_varsCorr_ASD.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_area_Corr_ASD$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_area_Corr_ASD$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()


###### TD ONlY


anat_data_TD <- subset(anat_data, anat_data$Dx == "TD")

SES_area_Corr_TD <- rcorr(as.matrix(anat_data_TD[,c("sexCoded", "MEL_cat", "INR", "SES1", "SES2", "zipIncome","lh_transversetemporal_area", "rh_transversetemporal_area", "lh_superiortemporal_area", "rh_superiortemporal_area", "lh_middletemporal_area", "rh_middletemporal_area", "lh_parsopercularis_area", "rh_parsopercularis_area", "lh_parsorbitalis_area", "rh_parsorbitalis_area", "lh_parstriangularis_area", "rh_parstriangularis_area", "TBV")], use="pairwise.complete.obs"))

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/corrPlots/SA", "SES_area_varsCorr_TD.tiff")
tiff(file=mypath,width = 10, height = 10, units = 'in', res = 300)
corrplot(SES_area_Corr_TD$r, method = "color",  type = "lower", outline = T, addgrid.col = "darkgray", order="original", 
         cl.pos = "b", tl.col = "indianred4", tl.srt = 45,
         p.mat = SES_area_Corr_TD$P, sig.level = 0.05, insig = "blank",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("midnightblue","white","darkred"))(100))
dev.off()
