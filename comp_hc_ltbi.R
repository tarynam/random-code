mutate_hc<-function(df){
    df$QFT<-NA
    for(i in 1:length(df$QFT)){
        if(df$TB[i]=="HC") df$QFT[i]<-"QFT-"
        else if(df$TB[i]=="LTBI") df$QFT[i]<-"QFT+"
        else df$QFT[i]<-"TB"
    }
    df$TB<-gsub("LTBI", "HC", df$TB)
    df<-filter(df, SM!="N", TB!="TB") 
    df$SM<-factor(df$SM, levels=c("X","SM"))
    df$QFT<-factor(df$QFT, levels=c("QFT-","QFT+"))
    df
}

pvals_hc<-function(data, column){
    library(dplyr)
    library(knitr)
    #This whole first chunk is specific to my flow data so you may want to take it out
    a<-wilcox.test(data[,column]~data[,"QFT"], exact=FALSE)
    x<-split(data,data$SM) 
    y<- lapply(x, function(g) wilcox.test(g[,column]~g[,"QFT"], exact=FALSE))
    
    #extract the p value itself from each wilcox test
    pvals<-c(a$p.value, y$SM$p.value, y$X$p.value)
    
    #create a set of labels so you know which p value belongs to which comparison
    labels<-c("QFT+ to QFT-", "QFT+ SM+ to QFT- SM+","QFT+ SM- to QFT- SM-")
    
    #bind them together into a table
    table<-data.frame(cbind(round(pvals, 4), row.names=labels))
    table$V1<-as.numeric(as.character(table$V1))
    colnames(table)<-c("p-value")
    table
}

plot_hc<-function(data, xval, yval){
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_fill_manual(values="#1a9850")+
        scale_alpha_manual(values=c(0.5,1))+
        scale_x_discrete(labels=c("SM-", "SM+"))+
        theme_classic()+ 
        facet_grid(~TB)+
        theme(legend.position="none")+
        theme(plot.subtitle = element_text(hjust = 0.5))+
        theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=0.6)) +   
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                           method="wilcox.test", paired = FALSE, label.y= (max(data[,yval])*1.1),
                           label.x.npc = "center")+
        labs(x="",
             y=paste("Freq of ", yval, "+ CD4+ (%)"))
}