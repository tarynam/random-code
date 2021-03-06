#geom_boxplot does not inherently use the 95% confidence interval as it's whiskers
#we would like to force it to by using a stat_summary modification
#to do this we writ a function to feed into stat_summary and specifying the output as a boxplot
#ToothGrowth is a built in data set

quantiles_95 <- function(x) {
    r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
    #quantile is a stats function that takes a vector "x" and returns the value at a specified quantile
    # 0.5 is the median or the middle line of our new boxplot
    # 0.25 and 0.75 will give use the IQR so the box of our boxplot
    # 0.05 and 0.95 are our 95% CI which gives us our whiskers
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}

#just to see the output of the function
quantiles_95(ToothGrowth$len) #we input ToothGrowth$len as x because that's what we are plotting on the y
#ggplot is smart enough to split this by our x value but we could do it ourselves
quantiles_95(ToothGrowth$len[ToothGrowth$supp=="OJ"])
quantiles_95(ToothGrowth$len[ToothGrowth$supp=="VC"])

#now compare the two plots one generated by geom_boxplot and one generated by our function
g<-ggplot(ToothGrowth, aes(x=supp, y=len))+
    geom_boxplot()+
    ggtitle("built in")+ 
    coord_cartesian(ylim=c(0,35)) #to make sure the two plots are on the same scale
c<-ggplot(ToothGrowth, aes(x=supp, y=len))+
    stat_summary(fun.data = quantiles_95, geom="boxplot")+
    ggtitle("our function")+
    coord_cartesian(ylim=c(0,35)) #to make sure the two plots are on the same scale

grid.arrange(g,c, ncol=2)
