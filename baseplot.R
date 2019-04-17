baseplot <- function(DF, group1, group2, value) {
    p <- ggplot(DF, aes(x=DF[,group1], y=DF[,value], col=group1))
    p <- p + theme_bw()
    p <- p + theme(legend.position="bottom")
    p <- p + geom_jitter(width=.1,height=0, shape=1,size=2)
    p <- p + scale_color_brewer(palette="Set1", guide=guide_legend(ncol=6, title=NULL))
    p <- p + xlab("") + ylab("")
    return(p)
}