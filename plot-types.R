library(ggplot2)
data(diamonds)
data<-diamonds

###PIE CHART
#need aggregated proportions so use an aggregate function with length
#if your data already has frequencies then use and aggregate function to find the mean frequencies by group
df<-aggregate(x~cut, diamonds, length)
pie(df$x, labels=df$cut) #x is the column the summary frequencies, cut is the thing you want to separate the pie by

###STACKED BARS
df<-melt(diamonds, id.vars=c("cut", "color", "clarity"), measure.vars=c("price"))
melt(df1,id.vars=c("TB","SM"))
df3<-as.data.frame(
    ddply(df2, c("TB", "SM", "variable"), summarise,
          median = median(value), sd = sd(value),
          error = qnorm(0.975)*sd(value)/sqrt(length(value))))
df3$upper<-(df3$median+df3$error)
df3$lower<-(df3$median-df3$error)

df3$variable<-factor(df3$variable, levels=c('TH2.Freq','TH1.2.Freq','TH1.Freq',ordered=TRUE))
df3$Label <- paste(df3$TB, df3$SM, sep='_')


g<-ggplot(df3, aes(SM, median))

g+ geom_bar(aes(fill = factor(variable)), stat="identity", width = 0.7) +
    scale_colour_brewer(palette = "Helper")+
    facet_wrap(~TB,nrow = 1)+
    geom_errorbar(aes(ymax=upper , ymin= lower ),width=.5) +
    theme(panel.background = element_rect(fill='white', colour='white'), 
          panel.grid = element_line(color = NA),
          panel.grid.minor = element_line(color = NA),
          panel.border = element_rect(fill = NA, color = "black"),
          axis.text.x  = element_text(size=10, colour="black", face = "bold"), 
          axis.title.x = element_text(vjust=0.1, face = "bold"),
          axis.text.y = element_text(size=12, colour="black"),
          axis.title.y = element_text(vjust=0.2, size = 12, face = "bold"))
