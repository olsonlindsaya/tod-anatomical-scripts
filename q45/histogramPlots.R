mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "SES1_ASD_hist.tiff")
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_ASD$SES1,
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
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_TD$SES1,
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
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_ASD$INR,
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
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_TD$INR,
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
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_ASD$MEL_cat,
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
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_TD$MEL_cat,
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
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_ASD$zipIncome,
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
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_TD$zipIncome,
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
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_ASD$INR,
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
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q3_TD$INR,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Income-to-Needs Ratio: TD", 
      xlab = "Income:Needs Ratio",  
      fill=I("blue"), 
      col=I("black"),
      ylim = c(0,9),
      xlim=c(-1,11))
dev.off()


mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "AgeASD_hist.tiff")
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q4_ASD$V1.Age,
      geom="histogram",
      binwidth =  4,  
      main = "Histogram for Age: ASD", 
      xlab = "Age (Months)",  
      fill=I("red"), 
      col=I("black"),
      ylim = c(0,5),
      xlim=c(15,75))
dev.off()

mypath <- file.path("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/histograms", "AgeTD_hist.tiff")
tiff(file=mypath,width = 5, height = 5, units = 'in', res = 300)
qplot(LGI_data_q4_TD$V1.Age,
      geom="histogram",
      binwidth =  4,  
      main = "Histogram for Age: TD", 
      xlab = "Age (Months)",  
      fill=I("blue"), 
      col=I("black"),
      ylim = c(0,5),
      xlim=c(15,75))
dev.off()