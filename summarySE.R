#I did not write this function but it will take data and return summary statistics

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm)
                       )
                   },
                   measurevar
    )
    
    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
}

###EXAMPLE
library(ggplot2)
data(diamonds)
data<-diamonds
DF<- summarySE(diamonds, "price", "cut")

#then graph it with a point and error
ggplot(DF, aes(x=cut, y=price)) +
    geom_errorbar(aes(ymin=price-se, ymax=price+se), colour="black", width=.1, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3) +
    theme_classic()

#graph with column and error
ggplot(DF, aes(x=cut, y=price)) +
    geom_col()+
    geom_errorbar(aes(ymin=price-se, ymax=price+se), colour="black", width=.1, position=position_dodge(0.1)) +
    geom_line(position=position_dodge(0.1)) +
    theme_classic()

#graph with 95% confidence interval instead of SEM
ggplot(DF, aes(x=cut, y=price)) + 
    geom_errorbar(aes(ymin=price-ci, ymax=price+ci), width=.1, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1))+
    theme_classic()
