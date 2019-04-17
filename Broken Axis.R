library(ggplot2)
data(diamonds)
summary(diamonds)
data<-diamonds

#set a new variable "mask" that we will facet the data on to force a broken axis and give every line 0
data$mask = 0
#decide a y value (1000) that the axis will be broken at and assign all rows with value > that value a 1
data$mask[data$price > 1000] = 1
#relevel so orient the facet correctly
data$mask = factor(data$mask, levels = c('1', '0'))

#classic ggplot
p <- ggplot(data, aes(x=carat, y=price, col=cut))
        p <- p + theme_classic() #classic theme is best for broken axis --> least obvious
        p <- p + theme(legend.position="bottom")
        p <- p + geom_jitter(width=.1,height=0, shape=1,size=2)
        p <- p + xlab("") + ylab("")

#facet grid this way facets vertically so that mask=1 is plotted above and separately from mask=0
p<- p + facet_grid(mask~., scales="free")
p + theme(strip.text.y = element_blank()) # remove the facet labels