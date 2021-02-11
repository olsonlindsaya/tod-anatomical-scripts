# Iterate Scatter Plots in R, for RMarkdown

### Scatter Plots of Bivariate Associations, ASD and TD together 


```{r, scatterPlots, include=TRUE, echo=FALSE, warning=FALSE}
outcomes <- anat_data[,c(7:21)]
outcomeVars = names(outcomes)


### SES 1 Loop ####
for (i in seq_along(outcomeVars)) {
  
  thisOutcome = outcomeVars[i]
  #plots <-print(ggplot(anat_data, aes(x=SES1, y=as.matrix(outcomes[i]), color=Dx, shape=Dx))) +
  print(ggplot(anat_data, aes(x=SES1, y=as.matrix(outcomes[i]), color=Dx, shape=Dx))) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Neighborhood Advantage') +
    theme_classic()
  #ggsave(plots,filename=paste("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/LGI/SES1 _by_",outcomeVars[i],".tiff",sep=""))
}




### SES 2 Loop ####
for (i in seq_along(outcomeVars)) {
  
  thisOutcome = outcomeVars[i]
  plots <-print(ggplot(anat_data, aes(x=SES2, y=as.matrix(outcomes[i]), color=Dx, shape=Dx))) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Neighborhood Disadvantage') +
    theme_classic()
  #ggsave(plots,filename=paste("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/LGI/SES2 _by_",outcomeVars[i],".tiff",sep=""))
}



### zipIncome Loop ####
for (i in seq_along(outcomeVars)) {
  
  thisOutcome = outcomeVars[i]
  plots <-print(ggplot(anat_data, aes(x=zipIncome, y=as.matrix(outcomes[i]), color=Dx, shape=Dx))) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Zip-Income') +
    theme_classic()
  #ggsave(plots,filename=paste("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/LGI/zipIncome _by_",outcomeVars[i],".tiff",sep=""))
}


### INR Loop
for (i in seq_along(outcomeVars)) {
  
  thisOutcome = outcomeVars[i]
  plots <-print(ggplot(anat_data, aes(x=INR, y=as.matrix(outcomes[i]), color=Dx, shape=Dx))) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Income:Needs Ratio') +
    theme_classic()
  #ggsave(plots,filename=paste("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/LGI/INR_by_",outcomeVars[i],".tiff",sep=""))
}


### MEL_Cat Loop
for (i in seq_along(outcomeVars)) {
  
  thisOutcome = outcomeVars[i]
  plots <-print(ggplot(anat_data, aes(x=MEL_cat, y=as.matrix(outcomes[i]), color=Dx, shape=Dx))) +
    geom_point() +
    geom_smooth(method=lm, aes(fill=Dx)) +
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    ylab(thisOutcome) +
    xlab('Income:Needs Ratio') +
    theme_classic()
  #ggsave(plots,filename=paste("/Users/lolson/Documents/001_JDP/001_BDIL/006_Projects/11_ToddlerSES/todanatomicaldata/scatterPlots/LGI/MEL_by_",outcomeVars[i],".tiff",sep=""))
}



```